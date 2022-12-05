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
    /// <remarks>
    /// <a href="file:///parallel_optimization.drawio.svg">Diagram</a>
    /// </remarks>
    [<RequireQualifiedAccess>]
    type private OptimizationPhase =
        | Phase1
        | Phase2
        | Phase3
    
    module private OptimizationPhase =
        let prev (phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> None
            | OptimizationPhase.Phase2 -> Some OptimizationPhase.Phase1
            | OptimizationPhase.Phase3 -> Some OptimizationPhase.Phase2
        let next (phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> Some OptimizationPhase.Phase2
            | OptimizationPhase.Phase2 -> Some OptimizationPhase.Phase3
            | OptimizationPhase.Phase3 -> None

    type private OptimizeDuringCodeGen = bool -> Expr -> Expr

    type private OptimizeImplFileRes =
        (IncrementalOptimizationEnv * CheckedImplFile * ImplFileOptimizationInfo * SignatureHidingInfo) * OptimizeDuringCodeGen

    // Inputs and outputs of each phase
    
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

    /// Complete optimization results for a single file
    type private FileResultsComplete =
        {
            Phase1: Phase1Res
            Phase2: Phase2Res
            Phase3: Phase3Res
        }

    type private OptimizationFuncs = Phase1Fun * Phase2Fun * Phase3Fun
        
    /// Partial optimization results for a single file - mutated as optimization progresses.
    type private FileResults =
        {
            mutable Phase1: Phase1Res option
            mutable Phase2: Phase2Res option
            mutable Phase3: Phase3Res option
        }

        member this.HasResult(phase: OptimizationPhase) =
            match phase with
            | OptimizationPhase.Phase1 -> this.Phase1 |> Option.isSome
            | OptimizationPhase.Phase2 -> this.Phase2 |> Option.isSome
            | OptimizationPhase.Phase3 -> this.Phase3 |> Option.isSome

        static member Empty =
            {
                Phase1 = None
                Phase2 = None
                Phase3 = None
            }

    /// Identifies a work item scheduled independently of others - consists of a (file) index and OptimizationPhase.
    /// There are N*3 nodes in the whole optimization process. 
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
                    let _, implFile = res.Phase3
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
        ((phase1, phase2, phase3): OptimizationFuncs)
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

                | OptimizationPhase.Phase2 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase2 = Some (env, _file) } -> env
                        | Some { Phase2 = None } ->
                            raiseNoResultsExn { node with Idx = prevIdx }

                    // Take other inputs from Phase1
                    let file, info, hidingInfo =
                        res
                        |> getPhase1Res idx
                        |> (fun (_, file, optimizationInfo, hidingInfo) -> file, optimizationInfo, hidingInfo)

                    let inputs = env, hidingInfo, info, file
                    let phase2Res = phase2 inputs
                    res.Phase2 <- Some phase2Res

                | OptimizationPhase.Phase3 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase3 = Some (env, _) } -> env
                        | Some { Phase3 = None } -> raiseNoResultsExn { node with Idx = prevIdx }

                    // Take impl file from Phase2
                    let file =
                        match res.Phase2 with
                        | Some (_, file) -> file
                        | None -> raiseNoResultsExn { node with Phase = OptimizationPhase.Phase2 }
                    
                    // Take hidingInfo from Phase1
                    let hidingInfo = res |> getPhase1Res idx |> (fun (_, _, _, hidingInfo) -> hidingInfo)
                    
                    let inputs = env, hidingInfo, file
                    let phase3Res = phase3 inputs
                    res.Phase3 <- Some phase3Res

                dependentNodes node
                |> Array.filter isNodeUnblocked

        // TODO Do we need to pass in DiagnosticsLogger, or does optimization not use it?
        FSharp.Compiler.Service.Utilities.ParallelProcessing.processInParallel
            "OptimizeInputs"
            [| firstNodeToProcess |]
            worker
            // Only up to 3 work items can be processed at the same time due to the shape of the dependency graph between them.
            3
            (fun () -> visitedNodes.Count >= files.Length * 3)
            ct
            (fun node -> node.ToString())

        Debug.Assert(
            visitedNodes.Count = files.Length * 3,
            $"Expected to have visited exactly {files.Length} * 3 = {files.Length * 3} optimization nodes, but visited {visitedNodes.Count}."
        )

        let completeFileResults =
            results
            |> Array.mapi
                (fun i res ->
                    match res with
                    | { Phase1 = Some phase1; Phase2 = Some phase2; Phase3 = Some phase3 } ->
                        {
                            FileResultsComplete.Phase1 = phase1
                            Phase2 = phase2
                            Phase3 = phase3
                        }
                    | _ -> failwith $"Unexpected lack of optimization results for file [{i}] after processing all files.")

        let finalResults = completeFileResults |> collectFinalResults
        finalResults

let optimizeFilesSequentially optEnv (phase1, phase2, phase3) implFiles =
    let results, (optEnvFirstLoop, _, _, _) =
        ((optEnv, optEnv, optEnv, SignatureHidingInfo.Empty), implFiles)

        ||> List.mapFold (fun (optEnvFirstLoop: Optimizer.IncrementalOptimizationEnv, optEnvExtraLoop, optEnvFinalSimplify, hidden) implFile ->
            let (optEnvFirstLoop, implFile, implFileOptData, hidden), optimizeDuringCodeGen =
                phase1 (optEnvFirstLoop, hidden, implFile)

            let optEnvExtraLoop, implFile =
                phase2 (optEnvExtraLoop, hidden, implFileOptData, implFile)

            let optEnvFinalSimplify, implFile =
                phase3 (optEnvFinalSimplify, hidden, implFile)

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

    let phase1 (env: Optimizer.IncrementalOptimizationEnv, hidden: SignatureHidingInfo, implFile: CheckedImplFile) =
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]

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

    let phase2
        (
            env: Optimizer.IncrementalOptimizationEnv,
            hidden: SignatureHidingInfo,
            _implFileOptData: Optimizer.ImplFileOptimizationInfo,
            implFile: CheckedImplFile
        ) =
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase2" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]

        let implFile =
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase2-1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            LowerLocalMutables.TransformImplFile tcGlobals importMap implFile

        if tcConfig.extraOptimizationIterations > 0 then
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

    let phase3 (env: Optimizer.IncrementalOptimizationEnv, hidden: SignatureHidingInfo, implFile: CheckedImplFile) =
        use _ =
            FSharp.Compiler.Diagnostics.Activity.start "phase3" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]

        let implFile =
            if tcConfig.doDetuple then
                use _ =
                    FSharp.Compiler.Diagnostics.Activity.start "phase3-1" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
                let implFile = implFile |> Detuple.DetupleImplFile ccu tcGlobals
                implFile
            else
                implFile

        let implFile =
            if tcConfig.doTLR then
                use _ =
                    FSharp.Compiler.Diagnostics.Activity.start "phase3-2" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
                implFile
                |> InnerLambdasToTopLevelFuncs.MakeTopLevelRepresentationDecisions ccu tcGlobals
            else
                implFile

        let implFile =
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start "phase3-3" [| "QualifiedNameOfFile", implFile.QualifiedNameOfFile.Text |]
            LowerCalls.LowerImplFile tcGlobals implFile

        if tcConfig.doFinalSimplify then
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
    
    let results, optEnvFirstLoop =
        match tcConfig.optSettings.processingMode with
        | Optimizer.OptimizationProcessingMode.PartiallyParallel ->
            let ct = CancellationToken.None
            let results, optEnvFirstPhase =
                ParallelOptimization.optimizeFilesInParallel optEnv (phase1, phase2, phase3) implFiles ct
            results |> Array.toList, optEnvFirstPhase
        | Optimizer.OptimizationProcessingMode.Sequential ->
            optimizeFilesSequentially optEnv (phase1, phase2, phase3) implFiles

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
