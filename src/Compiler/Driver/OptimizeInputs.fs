// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.OptimizeInputs

open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Threading
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.Diagnostics
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.CompilerOptions
open FSharp.Compiler.IlxGen
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.IO
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeOps

let mutable showTermFileCount = 0

let PrintWholeAssemblyImplementation (tcConfig: TcConfig) outfile header expr =
    if tcConfig.showTerms then
        if tcConfig.writeTermsToFiles then
            let fileName = outfile + ".terms"

            use f =
                FileSystem
                    .OpenFileForWriteShim(fileName + "-" + string showTermFileCount + "-" + header, FileMode.Create)
                    .GetWriter()

            showTermFileCount <- showTermFileCount + 1
            LayoutRender.outL f (Display.squashTo 192 (DebugPrint.implFilesL expr))
        else
            dprintf "\n------------------\nshowTerm: %s:\n" header
            LayoutRender.outL stderr (Display.squashTo 192 (DebugPrint.implFilesL expr))
            dprintf "\n------------------\n"

let AddExternalCcuToOptimizationEnv tcGlobals optEnv (ccuinfo: ImportedAssembly) =
    match ccuinfo.FSharpOptimizationData.Force() with
    | None -> optEnv
    | Some data -> Optimizer.BindCcu ccuinfo.FSharpViewOfMetadata data optEnv tcGlobals

let GetInitialOptimizationEnv (tcImports: TcImports, tcGlobals: TcGlobals) =
    let ccuinfos = tcImports.GetImportedAssemblies()
    let optEnv = Optimizer.IncrementalOptimizationEnv.Empty
    let optEnv = List.fold (AddExternalCcuToOptimizationEnv tcGlobals) optEnv ccuinfos
    optEnv

[<RequireQualifiedAccess>]
module private ParallelOptimization =
    open Optimizer
    
    /// <summary>
    /// Each file's optimization can be split into three different phases, executed one after another.
    /// Each phase calls 'Optimizer.OptimizeImplFile' and performs some other tasks.
    /// Each phase uses outputs of the previous phase and outputs of previous file's optimization for the same phase.
    /// </summary>
    [<RequireQualifiedAccess>]
    type private OptimizationPhase =
        | Phase1
        | Phase21
        | Phase22
        | Phase31
        | Phase32
        | Phase33
        | Phase34
    
    module private OptimizationPhase =
        let prev (phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> None
            | OptimizationPhase.Phase21 -> Some OptimizationPhase.Phase1
            | OptimizationPhase.Phase22 -> Some OptimizationPhase.Phase21
            | OptimizationPhase.Phase31 -> Some OptimizationPhase.Phase22
            | OptimizationPhase.Phase32 -> Some OptimizationPhase.Phase31
            | OptimizationPhase.Phase33 -> Some OptimizationPhase.Phase32
            | OptimizationPhase.Phase34 -> Some OptimizationPhase.Phase33
        let next (phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> Some OptimizationPhase.Phase21
            | OptimizationPhase.Phase21 -> Some OptimizationPhase.Phase22
            | OptimizationPhase.Phase22 -> Some OptimizationPhase.Phase31
            | OptimizationPhase.Phase31 -> Some OptimizationPhase.Phase32
            | OptimizationPhase.Phase32 -> Some OptimizationPhase.Phase33
            | OptimizationPhase.Phase33 -> Some OptimizationPhase.Phase34
            | OptimizationPhase.Phase34 -> None

    type private OptimizeDuringCodeGen = bool -> Expr -> Expr

    type private OptimizeImplFileRes =
        (IncrementalOptimizationEnv * CheckedImplFile * ImplFileOptimizationInfo * SignatureHidingInfo) * OptimizeDuringCodeGen

    // Inputs and outputs of each phase
    
    type private SimpleInputs = CheckedImplFile
    type private SimpleRes = CheckedImplFile
    type private SimpleFun = SimpleInputs -> SimpleRes
    type private PhaseInputs = IncrementalOptimizationEnv * SignatureHidingInfo * CheckedImplFile
    type private Phase1Inputs = PhaseInputs
    type private Phase1Res = OptimizeImplFileRes
    type private Phase1Fun = Phase1Inputs -> Phase1Res
    type private Phase2Inputs = IncrementalOptimizationEnv * SignatureHidingInfo * ImplFileOptimizationInfo * CheckedImplFile
    type private Phase2Res = IncrementalOptimizationEnv * CheckedImplFile
    type private Phase2Fun = Phase2Inputs -> Phase2Res
    type private Phase3Inputs = PhaseInputs
    type private Phase3Res = IncrementalOptimizationEnv * CheckedImplFile
    type private Phase3Fun = Phase3Inputs -> Phase3Res
    type private Phase21Inputs = SimpleInputs
    type private Phase21Res = SimpleRes
    type private Phase21Fun = SimpleFun
    type private Phase22Inputs = Phase2Inputs
    type private Phase22Res = Phase2Res
    type private Phase22Fun = Phase2Fun
    type private Phase31Inputs = SimpleInputs
    type private Phase31Res = SimpleRes
    type private Phase31Fun = SimpleFun
    type private Phase32Inputs = SimpleInputs
    type private Phase32Res = SimpleRes
    type private Phase32Fun = SimpleFun
    type private Phase33Inputs = SimpleInputs
    type private Phase33Res = SimpleRes
    type private Phase33Fun = SimpleFun
    type private Phase34Inputs = Phase3Inputs
    type private Phase34Res = Phase3Res
    type private Phase34Fun = Phase3Fun    

    /// Complete optimization results for a single file
    type private FileResultsComplete =
        {
            Phase1: Phase1Res
            Phase21: Phase21Res
            Phase22: Phase22Res
            Phase31: Phase31Res
            Phase32: Phase32Res
            Phase33: Phase33Res
            Phase34: Phase34Res
        }

    type private OptimizationFuncs = Phase1Fun * Phase21Fun * Phase22Fun * Phase31Fun * Phase32Fun * Phase33Fun * Phase34Fun
        
    /// Partial optimization results for a single file - mutated as optimization progresses.
    type private FileResults =
        {
            mutable Phase1: Phase1Res option
            mutable Phase21: Phase21Res option
            mutable Phase22: Phase22Res option
            mutable Phase31: Phase31Res option
            mutable Phase32: Phase32Res option
            mutable Phase33: Phase33Res option
            mutable Phase34: Phase34Res option
        }

        member this.HasResult(phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> this.Phase1 |> Option.isSome
            | OptimizationPhase.Phase21 -> this.Phase21 |> Option.isSome
            | OptimizationPhase.Phase22 -> this.Phase22 |> Option.isSome
            | OptimizationPhase.Phase31 -> this.Phase31 |> Option.isSome
            | OptimizationPhase.Phase32 -> this.Phase32 |> Option.isSome
            | OptimizationPhase.Phase33 -> this.Phase33 |> Option.isSome
            | OptimizationPhase.Phase34 -> this.Phase34 |> Option.isSome

        static member Empty =
            {
                Phase1 = None
                Phase21 = None
                Phase22 = None
                Phase31 = None
                Phase32 = None
                Phase33 = None
                Phase34 = None
            }

    /// Identifies a work item scheduled independently of others - consists of a (file) index and OptimizationPhase.
    /// There are N*7 nodes in the whole optimization process. 
    type private Node =
        {
            Idx: int
            Phase: OptimizationPhase
        }

        override this.ToString() = $"[{this.Idx}-{this.Phase}]"

    /// Final processing of file results to produce output needed for further compilation steps.
    let private collectFinalResults
        (fileResults: FileResultsComplete[])
        : (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv =
        let finalFileResults =
            fileResults
            |> Array.map
                (fun res ->
                    let (_, _, implFileOptData, _), optimizeDuringCodeGen = res.Phase1
                    let _, implFile = res.Phase34
                    let implFile =
                        {
                            ImplFile = implFile
                            OptimizeDuringCodeGen = optimizeDuringCodeGen
                        }

                    implFile, implFileOptData)

        let lastFilePhase1Env =
            fileResults
            |> Array.last
            |> fun { Phase1 = phase1Res } ->
                let (optEnvPhase1, _, _, _), _ = phase1Res
                optEnvPhase1

        finalFileResults, lastFilePhase1Env

    let private raiseNoResultsExn (node: Node) =
        raise (exn $"Unexpected lack of results for {node}")
    
    let private getPhase1Res (idx: int) (fileResults: FileResults) =
        match fileResults.Phase1 with
        | Some res -> res |> fst
        | None -> raiseNoResultsExn { Idx = idx; Phase = OptimizationPhase.Phase1 }

    let optimizeFilesInParallel
        (env0: IncrementalOptimizationEnv)
        ((phase1, phase21, phase22, phase31, phase32, phase33, phase34): OptimizationFuncs)
        (files: CheckedImplFile list)
        (ct: CancellationToken)
        : (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv =
        let files = files |> List.toArray
        let firstNodeToProcess =
            {
                Idx = 0
                Phase = OptimizationPhase.Phase1
            }

        let results = files |> Array.map (fun _ -> FileResults.Empty)
        let isNodeUnblocked { Idx = idx; Phase = phase } : bool =
            let previousPhase = phase |> OptimizationPhase.prev
            let previousFileReady = if idx = 0 then true else results[ idx - 1 ].HasResult phase
            let previousPhaseReady =
                match previousPhase with
                | Some previousPhase -> results[ idx ].HasResult previousPhase
                | None -> true
            previousFileReady && previousPhaseReady

        let visitedNodes = ConcurrentDictionary<Node, unit>()

        let dependentNodes (node: Node) =
            seq {
                match node.Phase |> OptimizationPhase.next with
                | Some nextPhase -> yield { node with Phase = nextPhase }
                | None -> ()
                
                if node.Idx < files.Length - 1 then
                    yield { node with Idx = node.Idx + 1 }
            }
            |> Seq.toArray
        
        let worker ({ Idx = idx; Phase = phase } as node: Node) : Node[] =
            let notPreviouslyVisited = visitedNodes.TryAdd(node, ())
            if notPreviouslyVisited = false then
                [||]
            else
                let prevIdx = idx - 1
                let res = results[idx]
                let previousFileResults = if idx > 0 then Some results[prevIdx] else None

                match phase with
                | OptimizationPhase.Phase1 ->
                    // Take env from previous file if it exists
                    let env, hidingInfo =
                        previousFileResults
                        |> Option.map (getPhase1Res idx)
                        |> Option.map (fun (env, _file, _optInfo, hidingInfo) -> env, hidingInfo)
                        |> Option.defaultValue (env0, SignatureHidingInfo.Empty)
                    let inputs = env, hidingInfo, files[idx]
                    let phase1Res = phase1 inputs
                    res.Phase1 <- Some phase1Res

                | OptimizationPhase.Phase21 ->
                    // Take other inputs from Phase1
                    let file =
                        res
                        |> getPhase1Res idx
                        |> (fun (_, file, _optimizationInfo, _hidingInfo) -> file)
                    let phaseRes = phase21 file
                    res.Phase21 <- Some phaseRes

                | OptimizationPhase.Phase22 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase22 = Some (env, _file) } -> env
                        | Some { Phase22 = None } ->
                            raiseNoResultsExn { node with Idx = prevIdx }

                    // Take other inputs from Phase1
                    let info, hidingInfo =
                        res
                        |> getPhase1Res idx
                        |> (fun (_, _file, optimizationInfo, hidingInfo) -> optimizationInfo, hidingInfo)
                    
                    let file =
                        res.Phase21
                        |> Option.get

                    let inputs = env, hidingInfo, info, file
                    let phase22Res = phase22 inputs
                    res.Phase22 <- Some phase22Res

                | OptimizationPhase.Phase31 ->
                    // Take file from Phase22
                    let _, file =
                        res.Phase22
                        |> Option.get
                    let phaseRes = phase31 file
                    res.Phase31 <- Some phaseRes

                | OptimizationPhase.Phase32 ->
                    // Take file from Phase31
                    let file =
                        res.Phase31
                        |> Option.get
                    let phaseRes = phase32 file
                    res.Phase32 <- Some phaseRes
                    
                | OptimizationPhase.Phase33 ->
                    // Take file from Phase32
                    let file =
                        match res.Phase32 with
                        | Some file -> file
                        | None -> raiseNoResultsExn { node with Phase = OptimizationPhase.Phase32 }
                    let phaseRes = phase33 file
                    res.Phase33 <- Some phaseRes
                
                | OptimizationPhase.Phase34 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase34 = Some (env, _) } -> env
                        | Some { Phase34 = None } -> raiseNoResultsExn { node with Idx = prevIdx }

                    // Take file from Phase33
                    let file =
                        match res.Phase33 with
                        | Some file -> file
                        | None -> raiseNoResultsExn { node with Phase = OptimizationPhase.Phase33 }
                    
                    // Take hidingInfo from Phase1
                    let hidingInfo = res |> getPhase1Res idx |> (fun (_, _, _, hidingInfo) -> hidingInfo)
                    
                    let inputs = env, hidingInfo, file
                    let phase34Res = phase34 inputs
                    res.Phase34 <- Some phase34Res

                dependentNodes node
                |> Array.filter isNodeUnblocked

        let numberOfPhases = 1 + 2 + 4
        
        // TODO Do we need to pass in DiagnosticsLogger, or does optimization not use it?
        FSharp.Compiler.Service.Utilities.ParallelProcessing.processInParallel
            "OptimizeInputs"
            [| firstNodeToProcess |]
            worker
            // Only up to 3 work items can be processed at the same time due to the shape of the dependency graph between them.
            numberOfPhases
            (fun () -> visitedNodes.Count >= files.Length * numberOfPhases)
            ct
            (fun node -> node.ToString())

        Debug.Assert(
            visitedNodes.Count = files.Length * numberOfPhases,
            $"Expected to have visited exactly {files.Length} * {numberOfPhases} = {files.Length * numberOfPhases} optimization nodes, but visited {visitedNodes.Count}."
        )

        let completeFileResults =
            results
            |> Array.mapi
                (fun i res ->
                    match res with
                    | { Phase1 = Some phase1; Phase21 = Some phase21; Phase22 = Some phase22; Phase31 = Some phase31; Phase32 = Some phase32; Phase33 = Some phase33; Phase34 = Some phase34 } ->
                        {
                            FileResultsComplete.Phase1 = phase1
                            FileResultsComplete.Phase21 = phase21
                            FileResultsComplete.Phase22 = phase22
                            FileResultsComplete.Phase31 = phase31
                            FileResultsComplete.Phase32 = phase32
                            FileResultsComplete.Phase33 = phase33
                            FileResultsComplete.Phase34 = phase34
                        }
                    | _ -> failwith $"Unexpected lack of optimization results for file [{i}] after processing all files.")

        let finalResults = completeFileResults |> collectFinalResults
        finalResults

let optimizeFilesSequentially optEnv (phase1, phase21, phase22, phase31, phase32, phase33, phase34) implFiles =
    let results, (optEnvFirstLoop, _, _, _) =
        ((optEnv, optEnv, optEnv, SignatureHidingInfo.Empty), implFiles)

        ||> List.mapFold (fun (optEnvFirstLoop: Optimizer.IncrementalOptimizationEnv, optEnvExtraLoop, optEnvFinalSimplify, hidden) implFile ->
            let (optEnvFirstLoop, implFile, implFileOptData, hidden), optimizeDuringCodeGen =
                phase1 (optEnvFirstLoop, hidden, implFile)

            let implFile =
                phase21 implFile
            let optEnvExtraLoop, implFile =
                phase22 (optEnvExtraLoop, hidden, implFileOptData, implFile)

            let implFile = phase31 implFile
            let implFile = phase32 implFile
            let implFile = phase33 implFile
            let optEnvFinalSimplify, implFile =
                phase34 (optEnvFinalSimplify, hidden, implFile)

            let implFile =
                {
                    ImplFile = implFile
                    OptimizeDuringCodeGen = optimizeDuringCodeGen
                }

            (implFile, implFileOptData), (optEnvFirstLoop, optEnvExtraLoop, optEnvFinalSimplify, hidden))

    results, optEnvFirstLoop

let ApplyAllOptimizations
    (
        tcConfig: TcConfig,
        tcGlobals,
        tcVal,
        outfile,
        importMap,
        isIncrementalFragment,
        optEnv,
        ccu: CcuThunk,
        implFiles
    ) =
    // NOTE: optEnv - threads through
    //
    // Always optimize once - the results of this step give the x-module optimization
    // info.  Subsequent optimization steps choose representations etc. which we don't
    // want to save in the x-module info (i.e. x-module info is currently "high level").
    PrintWholeAssemblyImplementation tcConfig outfile "pass-start" implFiles
#if DEBUG
    if tcConfig.showOptimizationData then
        dprintf "Expression prior to optimization:\n%s\n" (LayoutRender.showL (Display.squashTo 192 (DebugPrint.implFilesL implFiles)))

    if tcConfig.showOptimizationData then
        dprintf "CCU prior to optimization:\n%s\n" (LayoutRender.showL (Display.squashTo 192 (DebugPrint.entityL ccu.Contents)))
#endif

    ReportTime tcConfig "Optimizations"

    let phase1Settings =
        { tcConfig.optSettings with
            // Only do abstractBigTargets in the first phase, and only when TLR is on.
            abstractBigTargets = tcConfig.doTLR
            reportingPhase = true
        }

    // Only do these two steps in the first phase.
    let phase2And3Settings =
        { phase1Settings with
            abstractBigTargets = false
            reportingPhase = false
        }

    let measurements = ConcurrentDictionary<string, int64>()
    let measure (name: string) =
        let sw = Stopwatch.StartNew()
        {
            new System.IDisposable with
                member this.Dispose() =
                    lock measurements (fun () ->
                        if measurements.ContainsKey name = false then measurements[name] <- 0L
                        measurements[name] <- measurements[name] + sw.ElapsedMilliseconds
                    )
        }
    
    let phase1 (env: Optimizer.IncrementalOptimizationEnv, hidden: SignatureHidingInfo, implFile: CheckedImplFile) =
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]

        use _ = measure "1"
        Optimizer.OptimizeImplFile(
            phase1Settings,
            ccu,
            tcGlobals,
            tcVal,
            importMap,
            env,
            isIncrementalFragment,
            tcConfig.fsiMultiAssemblyEmit,
            tcConfig.emitTailcalls,
            hidden,
            implFile
        )

    let phase21
        (implFile: CheckedImplFile)
        =
        use _ = measure "2-1"
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase2-1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
        LowerLocalMutables.TransformImplFile tcGlobals importMap implFile
        
    
    let phase22
        (
            env: Optimizer.IncrementalOptimizationEnv,
            hidden: SignatureHidingInfo,
            _implFileOptData: Optimizer.ImplFileOptimizationInfo,
            implFile: CheckedImplFile
        ) =
        if tcConfig.extraOptimizationIterations > 0 then
            use _ = measure "2-2"
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase2-2" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            let (optEnvExtraLoop, implFile, _, _), _ =
                Optimizer.OptimizeImplFile(
                    phase2And3Settings,
                    ccu,
                    tcGlobals,
                    tcVal,
                    importMap,
                    env,
                    isIncrementalFragment,
                    tcConfig.fsiMultiAssemblyEmit,
                    tcConfig.emitTailcalls,
                    hidden,
                    implFile
                )

            optEnvExtraLoop, implFile
        else
            env, implFile

    let phase31
        (implFile: CheckedImplFile)
        =
        if tcConfig.doDetuple then
            use _ = measure "3-1"   
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase3-1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            let implFile = implFile |> Detuple.DetupleImplFile ccu tcGlobals
            implFile
        else
            implFile
    
    let phase32
        (implFile: CheckedImplFile)
        =
        if tcConfig.doTLR then
            use _ = measure "3-2"
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase3-2" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            implFile
            |> InnerLambdasToTopLevelFuncs.MakeTopLevelRepresentationDecisions ccu tcGlobals
        else
            implFile
        
    let phase33
        (implFile: CheckedImplFile)
        =
        use _ = measure "3-3"
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase3-3" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
        LowerCalls.LowerImplFile tcGlobals implFile
    
    let phase34 (env: Optimizer.IncrementalOptimizationEnv, hidden: SignatureHidingInfo, implFile: CheckedImplFile) =
        if tcConfig.doFinalSimplify then
            use _ = measure "3-4"
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase3-4" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            let (optEnvFinalSimplify, implFile, _, _), _ =
                Optimizer.OptimizeImplFile(
                    phase2And3Settings,
                    ccu,
                    tcGlobals,
                    tcVal,
                    importMap,
                    env,
                    isIncrementalFragment,
                    tcConfig.fsiMultiAssemblyEmit,
                    tcConfig.emitTailcalls,
                    hidden,
                    implFile
                )

            optEnvFinalSimplify, implFile
        else
            env, implFile
    
    let funcs = phase1, phase21, phase22, phase31, phase32, phase33, phase34
    
    let results, optEnvFirstLoop =
        match tcConfig.optSettings.processingMode with
        | Optimizer.OptimizationProcessingMode.PartiallyParallel ->
            let ct = CancellationToken.None
            let results, optEnvFirstPhase =
                ParallelOptimization.optimizeFilesInParallel optEnv funcs implFiles ct
            results |> Array.toList, optEnvFirstPhase
        | Optimizer.OptimizationProcessingMode.Sequential ->
            optimizeFilesSequentially optEnv funcs implFiles
    
    for kvp in measurements do
        printfn $"[{kvp.Key}] = {kvp.Value}"

#if DEBUG
    if tcConfig.showOptimizationData then
        results
        |> List.map snd
        |> List.iter (fun implFileOptData ->
            let str =
                (LayoutRender.showL (Display.squashTo 192 (Optimizer.moduleInfoL tcGlobals implFileOptData)))

            dprintf $"Optimization implFileOptData:\n{str}\n")
#endif

    let implFiles, implFileOptDatas = List.unzip results
    let assemblyOptData = Optimizer.UnionOptimizationInfos implFileOptDatas
    let tassembly = CheckedAssemblyAfterOptimization implFiles
    PrintWholeAssemblyImplementation tcConfig outfile "pass-end" (implFiles |> List.map (fun implFile -> implFile.ImplFile))
    ReportTime tcConfig "Ending Optimizations"
    tassembly, assemblyOptData, optEnvFirstLoop

//----------------------------------------------------------------------------
// ILX generation
//----------------------------------------------------------------------------

let CreateIlxAssemblyGenerator (_tcConfig: TcConfig, tcImports: TcImports, tcGlobals, tcVal, generatedCcu) =
    let ilxGenerator =
        IlxAssemblyGenerator(tcImports.GetImportMap(), tcGlobals, tcVal, generatedCcu)

    let ccus = tcImports.GetCcusInDeclOrder()
    ilxGenerator.AddExternalCcus ccus
    ilxGenerator

let GenerateIlxCode
    (
        ilxBackend,
        isInteractiveItExpr,
        tcConfig: TcConfig,
        topAttrs: TopAttribs,
        optimizedImpls,
        fragName,
        ilxGenerator: IlxAssemblyGenerator
    ) =

    let mainMethodInfo =
        if
            (tcConfig.target = CompilerTarget.Dll)
            || (tcConfig.target = CompilerTarget.Module)
        then
            None
        else
            Some topAttrs.mainMethodAttrs

    let ilxGenOpts: IlxGenOptions =
        {
            generateFilterBlocks = tcConfig.generateFilterBlocks
            emitConstantArraysUsingStaticDataBlobs = true
            workAroundReflectionEmitBugs = tcConfig.isInteractive
            generateDebugSymbols = tcConfig.debuginfo // REVIEW: is this still required?
            fragName = fragName
            localOptimizationsEnabled = tcConfig.optSettings.LocalOptimizationsEnabled
            testFlagEmitFeeFeeAs100001 = tcConfig.testFlagEmitFeeFeeAs100001
            mainMethodInfo = mainMethodInfo
            ilxBackend = ilxBackend
            fsiMultiAssemblyEmit = tcConfig.fsiMultiAssemblyEmit
            useReflectionFreeCodeGen = tcConfig.useReflectionFreeCodeGen
            isInteractive = tcConfig.isInteractive
            isInteractiveItExpr = isInteractiveItExpr
            alwaysCallVirt = tcConfig.alwaysCallVirt
        }

    ilxGenerator.GenerateCode(ilxGenOpts, optimizedImpls, topAttrs.assemblyAttrs, topAttrs.netModuleAttrs)

//----------------------------------------------------------------------------
// Assembly ref normalization: make sure all assemblies are referred to
// by the same references. Only used for static linking.
//----------------------------------------------------------------------------

let NormalizeAssemblyRefs (ctok, ilGlobals: ILGlobals, tcImports: TcImports) scoref =
    let normalizeAssemblyRefByName nm =
        match tcImports.TryFindDllInfo(ctok, Range.rangeStartup, nm, lookupOnly = false) with
        | Some dllInfo -> dllInfo.ILScopeRef
        | None -> scoref

    match scoref with
    | ILScopeRef.Local
    | ILScopeRef.Module _ -> scoref
    | ILScopeRef.PrimaryAssembly -> normalizeAssemblyRefByName ilGlobals.primaryAssemblyName
    | ILScopeRef.Assembly aref -> normalizeAssemblyRefByName aref.Name

let GetGeneratedILModuleName (t: CompilerTarget) (s: string) =
    // return the name of the file as a module name
    let ext =
        match t with
        | CompilerTarget.Dll -> "dll"
        | CompilerTarget.Module -> "netmodule"
        | _ -> "exe"

    s + "." + ext
