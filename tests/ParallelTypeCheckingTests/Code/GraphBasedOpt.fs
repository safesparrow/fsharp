module internal ParallelTypeCheckingTests.Code.GraphBasedOpt

#nowarn "1182"

open System.IO
open FSharp.Compiler.Optimizer
open FSharp.Compiler.Service.Driver.OptimizeTypes
open FSharp.Compiler.TypedTreeOps
open ParallelTypeCheckingTests.Utils
open ParallelTypeCheckingTests
open FSharp.Compiler.TypedTree
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

let collectResults (inputs: CollectorInputs) : CollectorOutputs =
    let files =
        inputs
        |> Array.map (fun {Phase1 = phase1; Phase2 = _phase2; Phase3 = phase3} ->
            let (_, _, implFileOptData, _), optimizeDuringCodeGen = phase1
            let _, implFile = phase3
            let implFile =
                {
                    ImplFile = implFile
                    OptimizeDuringCodeGen = optimizeDuringCodeGen
                }
            implFile, implFileOptData
        )
        
    let lastFilePhase1Env =
        inputs
        |> Array.last
        |> fun {Phase1 = phase1} ->
            let (optEnvPhase1, _, _, _), _ = phase1
            optEnvPhase1
            
    files, lastFilePhase1Env

type FilePhaseFuncs = Phase1Fun * Phase2Fun * Phase3Fun   
type FileResults =
    {
        mutable Phase1: Phase1Res option
        mutable Phase2: Phase2Res option
        mutable Phase3: Phase3Res option
    }
    with
        member this.HasResult (phase: Phase) =
            match phase with
            | Phase.Phase1 -> this.Phase1 |> Option.isSome
            | Phase.Phase2 -> this.Phase2 |> Option.isSome
            | Phase.Phase3 -> this.Phase3 |> Option.isSome
        member x.Get1 () = x.Phase1 |> Option.get
        member x.Get2 () = x.Phase2 |> Option.get
        member x.Get3 () = x.Phase3 |> Option.get
        
        static member Empty =
            {
                Phase1 = None
                Phase2 = None
                Phase3 = None
            }

module FileResults =
    let complete (results: FileResults) =
        let {FileResults.Phase1 = phase1; Phase2 = phase2; Phase3 = phase3} = results
        match phase1, phase2, phase3 with
        | Some phase1, Some phase2, Some phase3 -> {FileResultsComplete.Phase1 = phase1; Phase2 = phase2; Phase3 = phase3}
        | _ -> failwith $"Unexpected lack of results"

type WorkItem =
    | Phase1 of Phase1Inputs
    | Phase2 of Phase2Inputs
    | Phase3 of Phase3Inputs

type Node =
    {
        Phase: Phase
        Idx: FileIdx
    }
    with override this.ToString() = $"[{this.Idx}-{this.Phase}]"
module Node =
    let make phase idx = { Idx = idx; Phase = phase }

let getPhase1Res (p: FileResults) =
    p.Phase1
    |> Option.get
    |> fun ((env, file, _, hidden), _) -> env, file, hidden

let getPhase2Res (p: FileResults) =
    p.Phase2
    |> Option.get

let getPhase3Res (p: FileResults) =
    p.Phase3
    |> Option.get
    |> fun (env, _) -> env

type IdxGraph = Graph<FileIdx>
    
type _Result = OptimizeRes

let mergeHidingInfos (empty: SignatureHidingInfo) (infos: SignatureHidingInfo[]): SignatureHidingInfo =
    infos
    |> Array.fold SignatureHidingInfo.Union empty

type Goer = IdxGraph -> IncrementalOptimizationEnv -> FilePhaseFuncs -> CheckedImplFile[] -> CollectorOutputs

let goGraph (idxGraph: IdxGraph) (env0: IncrementalOptimizationEnv) ((phase1, phase2, phase3): FilePhaseFuncs) (files: CheckedImplFile[]) : CollectorOutputs =
    // Create a 3x graph by cloning each file with its deps for each phase. Add links from phase3 -> phase2 -> phase1
    let graph =
        idxGraph
        |> Seq.collect (fun (KeyValue(file, deps)) ->
            // Create a node per each phase
            Phase.all
            |> Array.map (fun phase ->
                let cur = Node.make phase file
                let deps =
                    deps
                    |> Array.map (Node.make phase)
                let prevNode =
                    Phase.prev phase
                    |> Option.map (fun prev -> Node.make prev file)
                let deps =
                    match prevNode with
                    | Some prev -> Array.append deps [|prev|]
                    | None -> deps
                cur, deps
            )
        )
        |> readOnlyDict
        
    let mergeEnvs envs =
        mergeEnvs env0 envs
        
    let transitiveIdxGraph =
        idxGraph
        |> Graph.transitiveOpt

    let results =
        Array.init files.Length (fun _ -> FileResults.Empty)
    let getRes (FileIdx idx) = results[idx]
    let hidingInfo0 = SignatureHidingInfo.Empty
    
    let work (x: Node) : unit =
        let {Idx=idx; Phase=phase} = x
        let file = files[idx.Idx]
        let res = getRes idx
        let depResults =
            transitiveIdxGraph[idx]
            |> Array.map getRes
        
        match phase with
        | Phase.Phase1 ->
            // take env and hidingInfo from dependencies
            let env =
                depResults
                |> Array.map (fun r ->
                    let (a,_b,_c,_d), _e = r.Get1()
                    a
                )
                |> mergeEnvs
            let hidingInfo =
                depResults
                |> Array.map (fun r ->
                    let (_a,_b,_c,d), _e = r.Get1()
                    d
                )
                |> mergeHidingInfos hidingInfo0
            let inputs = env, hidingInfo, file
            let phase1Res = phase1 inputs
            res.Phase1 <- Some phase1Res
        | Phase.Phase2 ->
            // take env from dependencies
            let env =
                depResults
                |> Array.map (fun r ->
                    let a,_b = r.Get2()
                    a
                )
                |> mergeEnvs
            // Get file and hidingInfo from phase1 of the current file
            let (_optEnv, file, _, hidingInfo), _ = res.Get1()
            let inputs = env, hidingInfo, file
            let phase2Res = phase2 inputs
            res.Phase2 <- Some phase2Res
        | Phase.Phase3 ->
            // take env from dependencies
            let env =
                depResults
                |> Array.map (fun r ->
                    let a,_b = r.Get3()
                    a
                )
                |> mergeEnvs
            // Get file and hidingInfo from phase1 of the current file
            let (_optEnv, _, _, hidingInfo), _ = res.Get1()
            // Get impl file from phase2
            let _, file = res.Get2()
            let inputs = env, hidingInfo, file
            let phase3Res = phase3 inputs
            res.Phase3 <- Some phase3Res
    
    GraphProcessing.processGraphSimpler<Node>
        graph
        work
        1
    
    let completeResults = results |> Array.map FileResults.complete
    let collected = collectResults completeResults
    collected
