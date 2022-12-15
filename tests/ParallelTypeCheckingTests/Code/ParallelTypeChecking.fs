﻿module internal ParallelTypeCheckingTests.ParallelTypeChecking

#nowarn "1182"

open System.IO
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.CheckBasics
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.NameResolution
open FSharp.Compiler.ParseAndCheckInputs
open ParallelTypeCheckingTests
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.TypedTree
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

type FileGraph = Graph<File>

// Within a file, equip loggers to locally filter w.r.t. scope pragmas in each input
let DiagnosticsLoggerForInput (tcConfig: TcConfig, input: ParsedInput, oldLogger) =
    CompilerDiagnostics.GetDiagnosticsLoggerFilteringByScopedPragmas(false, input.ScopedPragmas, tcConfig.diagnosticsOptions, oldLogger)

type State = TcState * bool
type FinalFileResult = TcEnv * TopAttribs * CheckedImplFile option * ModuleOrNamespaceType
type SingleResult = State -> FinalFileResult * State
type Item = File

type PartialResult = TcEnv * TopAttribs * CheckedImplFile option * ModuleOrNamespaceType

let folder (state: State) (result: SingleResult) : FinalFileResult * State = result state

[<RequireQualifiedAccess>]
type Dependency =
    | PhysicalFile of fileIndex: int
    | Pair of signatureFileIndex: int

/// Use parallel checking of implementation files that have signature files
let CheckMultipleInputsInParallel
    ((ctok, checkForErrors, tcConfig: TcConfig, tcImports: TcImports, tcGlobals, prefixPathOpt, tcState, eagerFormat, inputs): 'a * (unit -> bool) * TcConfig * TcImports * TcGlobals * LongIdent option * TcState * (PhasedDiagnostic -> PhasedDiagnostic) * ParsedInput list)
    : FinalFileResult list * TcState =

    use cts = new CancellationTokenSource()

    let sourceFiles: FileWithAST array =
        inputs
        |> List.toArray
        |> Array.mapi (fun idx (input: ParsedInput) ->
            {
                Idx = idx
                File = input.FileName
                AST = input
            })

    let filePairs = FilePairMap(sourceFiles)
    let graph = DependencyResolution.mkGraph filePairs sourceFiles

    // TcState has two typing environments: TcEnvFromSignatures && TcEnvFromImpls
    // When type checking a file, depending on the type (implementation or signature), it will use one of these typing environments (TcEnv).
    // Checking a file will populate the respective TcEnv.
    //
    // When a file has a dependencies, the information of the signature file in case a pair (1) will suffice to type-check the file.
    // Example: if `B.fs` has a dependency on `A`, the information of `A.fsi` is enough for `B.fs` to type-check,
    // on condition that it is available in the TcEnvFromImpls.
    // We introduce a special Pair dependency node in the graph to satisfy this. `B.fs -> [ A.fsi ]` becomes `B.fs -> [ Pair<A> ].
    // The `Pair<A>` node will duplicate the signature information which A.fsi provided.
    // Processing a Pair will add the information from the TcEnvFromSignatures to the TcEnvFromImpls.
    // This means `A` will be known in both TcEnvs and therefor `B.fs` can be type-checked.
    // By doing this, we can speed up the graph processing as type checking a signature file is less expensive than its implementation counterpart.
    //
    // When we need to actually type-check the implementation file of a Pair, we cannot have the duplicate information of the signature file present in TcEnvFromImpls.
    // Example `A.fs -> [ A.fsi ]`, because an implementation file always depends on its signature.
    // Type-checking `A.fs` will add the actual information to TcEnvFromImpls and we do not depend on the Pair<A> for `A.fs` itself.
    //
    // In order to deal correctly with the Pair logic, we need to transform the resolved graph to contain the additional pair nodes.
    // After we have type-checked the graph, we exclude the Pair nodes as they are not actual physical files in our project.
    //
    // (1) : A pair is consider the combination of a implementation and signature file. (For example `A.fsi` and matching `A.fs` is Pair<A>)
    let dependencyGraph =
        let mkPair n = Dependency.Pair n
        let mkPhysicalFile n = Dependency.PhysicalFile n

        // Map any signature dependencies to the Pair counterparts.
        // Unless, the signature dependency is the backing file of the current (implementation) file.
        let mapDependencies idx deps =
            Array.map
                (fun dep ->
                    if filePairs.IsSignature dep then
                        let implIdx = filePairs.GetImplementationIndex dep

                        if implIdx = idx then
                            // This is the matching signature for the implementation
                            // Keep using the physical file
                            mkPhysicalFile dep
                        else
                            mkPair dep
                    else
                        mkPhysicalFile dep)
                deps

        // Transform the graph to include Pair nodes when necessary.
        graph
        |> Seq.collect (fun (KeyValue (fileIdx, deps)) ->
            if filePairs.IsSignature fileIdx then
                // Add an additional Pair node for the signature file.
                [|
                    // Mark the current file as physical and map the dependencies.
                    mkPhysicalFile fileIdx, mapDependencies fileIdx deps
                    // Introduce a new node that depends on the signature.
                    mkPair fileIdx, [| mkPhysicalFile fileIdx |]
                |]
            else
                [| mkPhysicalFile fileIdx, mapDependencies fileIdx deps |])
        |> Graph.make

    // graph |> Graph.map (fun idx -> sourceFiles.[idx].File) |> Graph.print

    // let graphDumpPath =
    //     let graphDumpName =
    //         tcConfig.outputFile
    //         |> Option.map Path.GetFileName
    //         |> Option.defaultValue "project"
    //
    //     $"{graphDumpName}.deps.json"
    //
    // graph
    // |> Graph.map (fun idx -> sourceFiles.[idx].File)
    // |> Graph.serialiseToJson graphDumpPath

    let _ = ctok // TODO Use it
    let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger

    // In the first linear part of parallel checking, we use a 'checkForErrors' that checks either for errors
    // somewhere in the files processed prior to each one, or in the processing of this particular file.
    let priorErrors = checkForErrors ()

    let mutable cnt = 1

    let processPair (input: ParsedInput) ((currentTcState, _currentPriorErrors): State) : State -> PartialResult * State =
        fun (state: State) ->
            let tcState, currentPriorErrors = state

            let f =
                // Retrieve the type-checked signature information and add it to the TcEnvFromImpls.
                AddSignatureResultToTcImplEnv(tcImports, tcGlobals, prefixPathOpt, TcResultsSink.NoSink, currentTcState, input)

            // The `partialResult` will be excluded at the end of `GraphProcessing.processGraph`.
            // The important thing is that `nextTcState` will populated the necessary information to TcEnvFromImpls.
            let partialResult, nextTcState = f tcState
            partialResult, (nextTcState, currentPriorErrors)

    let processFile
        ((input, logger): ParsedInput * DiagnosticsLogger)
        ((currentTcState, _currentPriorErrors): State)
        : State -> PartialResult * State =
        cancellable {
            use _ = UseDiagnosticsLogger logger
            // TODO Is it OK that we don't update 'priorErrors' after processing batches?
            let checkForErrors2 () = priorErrors || (logger.ErrorCount > 0)

            let tcSink = TcResultsSink.NoSink
            cnt <- cnt + 1

            // printfn $"#{c} [thread {Thread.CurrentThread.ManagedThreadId}] Type-checking {input.FileName}"

            let! f = CheckOneInput'(checkForErrors2, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcSink, currentTcState, input, false)

            // printfn $"Finished Processing AST {file.ToString()}"
            return
                (fun (state: State) ->

                    // printfn $"Applying {file.ToString()}"
                    let tcState, priorErrors = state
                    let (partialResult: PartialResult, tcState) = f tcState

                    let hasErrors = logger.ErrorCount > 0
                    // TODO Should we use local _priorErrors or global priorErrors?
                    let priorOrCurrentErrors = priorErrors || hasErrors
                    let state: State = tcState, priorOrCurrentErrors
                    // printfn $"Finished applying {file.ToString()}"
                    partialResult, state)
        }
        |> Cancellable.runWithoutCancellation

    UseMultipleDiagnosticLoggers (inputs, diagnosticsLogger, Some eagerFormat) (fun inputsWithLoggers ->
        // Equip loggers to locally filter w.r.t. scope pragmas in each input
        let inputsWithLoggers =
            inputsWithLoggers
            |> List.toArray
            |> Array.map (fun (input, oldLogger) ->
                let logger = DiagnosticsLoggerForInput(tcConfig, input, oldLogger)
                input, logger)

        let processFile (dependency: Dependency) (state: State) : State -> PartialResult * State =
            match dependency with
            | Dependency.Pair idx ->
                let parsedInput, _ = inputsWithLoggers.[idx]
                processPair parsedInput state
            | Dependency.PhysicalFile idx ->
                let parsedInput, logger = inputsWithLoggers.[idx]
                processFile (parsedInput, logger) state

        let state: State = tcState, priorErrors

        let partialResults, (tcState, _) =
            TypeCheckingGraphProcessing.processFileGraph<Dependency, State, SingleResult, FinalFileResult>
                dependencyGraph
                processFile
                folder
                (function
                | Dependency.Pair _ -> false
                | Dependency.PhysicalFile _ -> true)
                state
                cts.Token

        let partialResults =
            partialResults |> Array.sortBy fst |> Array.map snd |> Array.toList

        partialResults, tcState)
