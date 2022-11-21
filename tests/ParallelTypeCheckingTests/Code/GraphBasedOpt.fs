﻿module internal ParallelTypeCheckingTests.Code.GraphBasedOpt

#nowarn "1182"

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.CheckBasics
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.NameResolution
open FSharp.Compiler.OptimizeInputs
open FSharp.Compiler.Optimizer
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.TypedTreeOps
open ParallelTypeCheckingTests.FileInfoGathering
open ParallelTypeCheckingTests.Types
open ParallelTypeCheckingTests.Utils
open ParallelTypeCheckingTests
open ParallelTypeCheckingTests.DepResolving
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras


type OptimizeDuringCodeGen = bool -> Expr -> Expr
type OptimizeRes =
    (IncrementalOptimizationEnv * CheckedImplFile * ImplFileOptimizationInfo * SignatureHidingInfo) * OptimizeDuringCodeGen

type Optimize =
    OptimizationSettings *
    CcuThunk *
    TcGlobals *
    ConstraintSolver.TcValF *
    Import.ImportMap *
    IncrementalOptimizationEnv *
    bool *
    bool *
    bool *
    SignatureHidingInfo *
    CheckedImplFile ->
        OptimizeRes

type PhaseInputs = IncrementalOptimizationEnv * SignatureHidingInfo * CheckedImplFile

type Phase1Inputs = PhaseInputs
type Phase1Res = OptimizeRes
type Phase1Fun = Phase1Inputs -> Phase1Res

type Phase2Inputs = PhaseInputs
type Phase2Res = IncrementalOptimizationEnv * CheckedImplFile
type Phase2Fun = Phase2Inputs -> Phase2Res

type Phase3Inputs = PhaseInputs
type Phase3Res = IncrementalOptimizationEnv * CheckedImplFile
type Phase3Fun = Phase3Inputs -> Phase3Res

type PhaseRes =
    | Phase1 of Phase1Res
    | Phase2 of Phase2Res
    | Phase3 of Phase3Res

type FileResultsComplete =
    {
        Phase1: Phase1Res
        Phase2: Phase2Res
        Phase3: Phase3Res
    }
type CollectorInputs = FileResultsComplete[]
type CollectorOutputs = (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv

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

type Phase =
    | Phase1
    | Phase2
    | Phase3
module Phase =
    let all = [|Phase1; Phase2; Phase3|]
    let prev (phase: Phase) =
        match phase with
        | Phase1 -> None
        | Phase2 -> Some Phase1
        | Phase3 -> Some Phase2
    let next (phase: Phase) =
        match phase with
        | Phase1 -> Some Phase2
        | Phase2 -> Some Phase3
        | Phase3 -> None

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
        static member Empty =
            {
                Phase1 = None
                Phase2 = None
                Phase3 = None
            }

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
    |> fun ((env, _, _, hidden), _) -> env, hidden

let getPhase2Res (p: FileResults) =
    p.Phase2
    |> Option.get

let getPhase3Res (p: FileResults) =
    p.Phase3
    |> Option.get
    |> fun (env, _) -> env

type IdxGraph = Graph<FileIdx>
    
type _Result = OptimizeRes

let processNode ({Idx = idx; Phase = phase} as node : Node) (res: FileResults) : OptimizeRes =
    failwith ""
    
let goGraph (graph: IdxGraph) (env0: IncrementalOptimizationEnv) ((phase1, phase2, phase3): FilePhaseFuncs) (files: CheckedImplFile[]) : CollectorOutputs =
    // Create a 3x graph by cloning each file with its deps for each phase. Add links from phase3 -> phase2 -> phase1
    let graph =
        graph
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

    let results =
        GraphProcessing.processGraphSimple
            graph
            (failwith "")
            1
    
    
    failwith ""
