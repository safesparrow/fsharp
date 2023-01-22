// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.OptimizeInputs

open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
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

type private OptimizeDuringCodeGen = bool -> Expr -> Expr
type PhaseRes =
    {
        File : CheckedImplFile
        OptEnvFirstLoop : Optimizer.IncrementalOptimizationEnv
        OptInfo : Optimizer.ImplFileOptimizationInfo
        OptEnvExtraLoop : Optimizer.IncrementalOptimizationEnv
        OptEnvFinalSimplify : Optimizer.IncrementalOptimizationEnv
        HidingInfo : SignatureHidingInfo
        OptDuringCodeGen : OptimizeDuringCodeGen
    }

type PhaseIdx = int
type F<'In,'Out> = 'In -> 'Out
type PhaseInputs =
    {
        PrevPhase : PhaseRes
        PrevFile : PhaseRes
    }
type PhaseFunc = PhaseInputs -> PhaseRes

/// <summary>
/// Each file's optimization can be split into three different phases, executed one after another.
/// Each phase calls 'Optimizer.OptimizeImplFile' and performs some other tasks.
/// Each phase uses outputs of the previous phase and outputs of previous file's optimization for the same phase.
/// </summary>
type Phase =
    {
        Idx : PhaseIdx
        Name : string
    }
    override this.ToString() = $"{this.Idx}-{this.Name}"

type PhaseInfo =
    {
        Phase : Phase
        Func : PhaseFunc
    }

type PhaseInfos = PhaseInfo[]

[<RequireQualifiedAccess>]
module private ParallelOptimization =
    open Optimizer
    
    /// Identifies a work item scheduled independently of others - consists of a (file) index and OptimizationPhase.
    /// There are N*7 nodes in the whole optimization process. 
    type private Node =
        {
            FileIdx: int
            Phase: PhaseIdx
        }

        override this.ToString() = $"[{this.FileIdx}-{this.Phase}]"

    /// Final processing of file results to produce output needed for further compilation steps.
    let private collectFinalResults
        (fileResults: PhaseRes[])
        : (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv =
        let finalFileResults =
            fileResults
            |> Array.map
                (fun res ->
                    let implFile =
                        {
                            ImplFile = res.File
                            OptimizeDuringCodeGen = res.OptDuringCodeGen
                        }

                    implFile, res.OptInfo)

        let lastFilePhase1Env =
            fileResults
            |> Array.last
            |> fun res -> res.OptEnvFirstLoop

        finalFileResults, lastFilePhase1Env

    let private raiseNoResultsExn (node: Node) =
        raise (exn $"Unexpected lack of results for {node}")
    
    let optimizeFilesInParallel2
        (env0: IncrementalOptimizationEnv)
        (phases : PhaseInfos)
        (files: CheckedImplFile list)
        (ct: CancellationToken)
        : (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv =
        
        
                
        // Create one worker for each phase, that will process a single phases for all files in order.
        let phaseWorker (phase : PhaseInfo) =
            ()
            
        
            
        failwith ""
    
    let optimizeFilesInParallel
        (env0: IncrementalOptimizationEnv)
        (phaseFuncs : OptimizationFuncs)
        (files: CheckedImplFile list)
        (ct: CancellationToken)
        : (CheckedImplFileAfterOptimization * ImplFileOptimizationInfo)[] * IncrementalOptimizationEnv =
        let files = files |> List.toArray
        let firstNodeToProcess =
            {
                FileIdx = 0
                Phase = PhaseExplicit.Phase1
            }

        let results = files |> Array.map (fun _ -> FileResults.Empty)
        let isNodeUnblocked { FileIdx = idx; Phase = phase } : bool =
            let previousPhase = phase |> PhaseExplicit.prev
            let previousFileReady = if idx = 0 then true else results[ idx - 1 ].HasResult phase
            let previousPhaseReady =
                match previousPhase with
                | Some previousPhase -> results[ idx ].HasResult previousPhase
                | None -> true
            previousFileReady && previousPhaseReady

        let visitedNodes = ConcurrentDictionary<Node, unit>()

        let dependentNodes (node: Node) =
            seq {
                match node.Phase |> PhaseExplicit.next with
                | Some nextPhase -> yield { node with Phase = nextPhase }
                | None -> ()
                
                if node.FileIdx < files.Length - 1 then
                    yield { node with FileIdx = node.FileIdx + 1 }
            }
            |> Seq.toArray
        
        let worker ({ FileIdx = idx; Phase = phase } as node: Node) : Node[] =
            let notPreviouslyVisited = visitedNodes.TryAdd(node, ())
            if notPreviouslyVisited = false then
                [||]
            else
                let prevIdx = idx - 1
                let res = results[idx]
                let previousFileResults =
                    if idx > 0 then Some results[prevIdx]
                    else None

                let previousFileResults =
                    match previousFileResults with
                    | Some res -> res
                    | None ->
                        
                
                match phase with
                | PhaseExplicit.Phase1 ->
                    // Take env from previous file if it exists
                    let env, hidingInfo =
                        previousFileResults
                        |> Option.defaultWith (fun () -> raiseNoResultsExn { FileIdx = idx; Phase = PhaseExplicit.Phase1 })
                        |> Option.map (getPhase1Res idx)
                        |> Option.map (fun (env, _file, _optInfo, hidingInfo) -> env, hidingInfo)
                        |> Option.defaultValue (env0, SignatureHidingInfo.Empty)
                    let inputs = env, files[idx], hidingInfo, hidingInfo
                    let phase1Res = (phaseFuncs PhaseExplicit.Phase1) inputs
                    res.Phase1 <- Some phase1Res

                | PhaseExplicit.Phase21 ->
                    // Take other inputs from Phase1
                    let file =
                        res
                        |> getPhase1Res idx
                        |> (fun (_, file, _optimizationInfo, _hidingInfo) -> file)
                    let phaseRes = phase21 file
                    res.Phase21 <- Some phaseRes

                | PhaseExplicit.Phase22 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase22 = Some (env, _file) } -> env
                        | Some { Phase22 = None } ->
                            raiseNoResultsExn { node with FileIdx = prevIdx }

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

                | PhaseExplicit.Phase31 ->
                    // Take file from Phase22
                    let _, file =
                        res.Phase22
                        |> Option.get
                    let phaseRes = phase31 file
                    res.Phase31 <- Some phaseRes

                | PhaseExplicit.Phase32 ->
                    // Take file from Phase31
                    let file =
                        res.Phase31
                        |> Option.get
                    let phaseRes = phase32 file
                    res.Phase32 <- Some phaseRes
                    
                | PhaseExplicit.Phase33 ->
                    // Take file from Phase32
                    let file =
                        match res.Phase32 with
                        | Some file -> file
                        | None -> raiseNoResultsExn { node with Phase = PhaseExplicit.Phase32 }
                    let phaseRes = phase33 file
                    res.Phase33 <- Some phaseRes
                
                | PhaseExplicit.Phase34 ->
                    // Take env from previous file if it exists
                    let env =
                        match previousFileResults with
                        | None -> env0
                        | Some { Phase34 = Some (env, _) } -> env
                        | Some { Phase34 = None } -> raiseNoResultsExn { node with FileIdx = prevIdx }

                    // Take file from Phase33
                    let file =
                        match res.Phase33 with
                        | Some file -> file
                        | None -> raiseNoResultsExn { node with Phase = PhaseExplicit.Phase33 }
                    
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

let optimizeFilesSequentially optEnv (phases : PhaseInfos) implFiles =
    let results, (optEnvFirstLoop, _, _, _) =
        ((optEnv, optEnv, optEnv, SignatureHidingInfo.Empty), implFiles)

        ||> List.mapFold (fun (optEnvFirstLoop: Optimizer.IncrementalOptimizationEnv, optEnvExtraLoop, optEnvFinalSimplify, hidden) implFile ->
            //
            // let state =
            //     {
            //         File = implFile
            //         OptEnvFirstLoop = optEnvFirstLoop
            //         OptInfo = Optimizer.ImplFileOptimizationInfo
            //         OptEnvExtraLoop : Optimizer.IncrementalOptimizationEnv
            //         OptEnvFinalSimplify : Optimizer.IncrementalOptimizationEnv
            //         HidingInfo : SignatureHidingInfo
            //         OptDuringCodeGen : OptimizeDuringCodeGen
            //     }
            
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
   
    let wrap (f : PhaseFunc) (name : string) =
        fun (inputs : PhaseInputs) ->
            use _ = measure name
            use _ =
                FSharp.Compiler.Diagnostics.Activity.start $"phase{name}" [| "QualifiedNameOfFile", inputs.PrevPhase.File.QualifiedNameOfFile.Text |]
            f inputs
    
    let phases = List<PhaseInfo>()
    
    let add (name : string) (f : PhaseFunc) =
        let phase =
            {
                Phase = 3
                Func = f
            }
        phases.Add(phase)
    
    let firstLoop ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let (env, file, optInfo, hidingInfo), optDuringCodeGen = Optimizer.OptimizeImplFile(
            phase1Settings,
            ccu,
            tcGlobals,
            tcVal,
            importMap,
            prevFile.OptEnvFirstLoop,
            isIncrementalFragment,
            tcConfig.fsiMultiAssemblyEmit,
            tcConfig.emitTailcalls,
            prevFile.HidingInfo,
            prevPhase.File
        )
        {
            prevPhase with
                PhaseRes.File = file
                OptEnvFirstLoop = env
                OptInfo = optInfo
                HidingInfo = hidingInfo
                OptDuringCodeGen = optDuringCodeGen
        }
    add "firstLoop" firstLoop
            
    let lowerLocalMutables ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let file = LowerLocalMutables.TransformImplFile tcGlobals importMap prevPhase.File
        {
            prevPhase with
                PhaseRes.File = file
        }
    add "lowerLocalMutables" lowerLocalMutables
        
    let phase22 ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let (optEnvExtraLoop, implFile, _, _), _ =
            Optimizer.OptimizeImplFile(
                phase2And3Settings,
                ccu,
                tcGlobals,
                tcVal,
                importMap,
                prevFile.OptEnvExtraLoop,
                isIncrementalFragment,
                tcConfig.fsiMultiAssemblyEmit,
                tcConfig.emitTailcalls,
                prevPhase.HidingInfo,
                prevPhase.File
            )

        {
            prevPhase with
                OptEnvExtraLoop = optEnvExtraLoop
                File = implFile
        }
    
    if tcConfig.extraOptimizationIterations > 0 then
        add "ExtraLoop" phase22
    
    let phase31 ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let implFile = prevPhase.File |> Detuple.DetupleImplFile ccu tcGlobals
        {
            prevPhase with
                File = implFile
        }
    if tcConfig.doDetuple then
        add "Detuple" phase31
    
    let phase32 ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let file =
            prevPhase.File
            |> InnerLambdasToTopLevelFuncs.MakeTopLevelRepresentationDecisions ccu tcGlobals
        {
            prevPhase with
                File = file
        }
    if tcConfig.doTLR then
        add "InnerLambdasToToplevelFuncs" phase32
    
    
    let phase33 ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let file = LowerCalls.LowerImplFile tcGlobals prevPhase.File
        {
            prevPhase with
                File = file
        }
    add "LowerCalls" phase33
    
    let phase34 ({PrevPhase = prevPhase; PrevFile = prevFile} : PhaseInputs) : PhaseRes =
        let (optEnvFinalSimplify, implFile, _, _), _ =
            Optimizer.OptimizeImplFile(
                phase2And3Settings,
                ccu,
                tcGlobals,
                tcVal,
                importMap,
                prevFile.OptEnvFinalSimplify,
                isIncrementalFragment,
                tcConfig.fsiMultiAssemblyEmit,
                tcConfig.emitTailcalls,
                prevPhase.HidingInfo,
                prevPhase.File
            )

        {
            prevPhase with
                OptEnvFinalSimplify = optEnvFinalSimplify
                File = implFile
        }
    if tcConfig.doFinalSimplify then
        add "FinalSimplify" phase34    
    
    let results, optEnvFirstLoop =
        match tcConfig.optSettings.processingMode with
        | Optimizer.OptimizationProcessingMode.PartiallyParallel ->
            let ct = CancellationToken.None
            let results, optEnvFirstPhase =
                ParallelOptimization.optimizeFilesInParallel optEnv funcs implFiles ct
            results |> Array.toList, optEnvFirstPhase
        | Optimizer.OptimizationProcessingMode.Sequential ->
            optimizeFilesSequentially optEnv phases implFiles
    
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
