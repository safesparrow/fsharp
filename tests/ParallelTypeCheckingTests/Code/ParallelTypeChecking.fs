module internal ParallelTypeCheckingTests.ParallelTypeChecking

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
open FSharp.Compiler.ParseAndCheckInputs
open ParallelTypeCheckingTests
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras

type FileGraph = Graph<File>

// Within a file, equip loggers to locally filter w.r.t. scope pragmas in each input
let DiagnosticsLoggerForInput (tcConfig: TcConfig, input: ParsedInput, oldLogger) =
    CompilerDiagnostics.GetDiagnosticsLoggerFilteringByScopedPragmas(false, input.ScopedPragmas, tcConfig.diagnosticsOptions, oldLogger)

type State = TcState * bool
type FinalFileResult = TcEnv * TopAttribs * CheckedImplFile option * ModuleOrNamespaceType
type SingleResult = bool -> State -> FinalFileResult * State
type Item = File

type PartialResult = TcEnv * TopAttribs * CheckedImplFile option * ModuleOrNamespaceType

let folder (isFinalFold: bool) (state: State) (result: SingleResult) : FinalFileResult * State = result isFinalFold state

/// Use parallel checking of implementation files that have signature files
let CheckMultipleInputsInParallel
    ((ctok, checkForErrors, tcConfig: TcConfig, tcImports: TcImports, tcGlobals, prefixPathOpt, tcState, eagerFormat, inputs): 'a * (unit -> bool) * TcConfig * TcImports * TcGlobals * LongIdent option * TcState * (PhasedDiagnostic -> PhasedDiagnostic) * ParsedInput list)
    : FinalFileResult list * TcState =

    let sourceFiles: FileWithAST array =
        inputs
        |> List.toArray
        |> Array.mapi (fun idx (input: ParsedInput) ->
            {
                Idx = idx
                File = input.FileName
                AST = input
            })

    let graph = DependencyResolution.mkGraph sourceFiles
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

    let _ = ctok // TODO Use
    let diagnosticsLogger = DiagnosticsThreadStatics.DiagnosticsLogger

    // In the first linear part of parallel checking, we use a 'checkForErrors' that checks either for errors
    // somewhere in the files processed prior to each one, or in the processing of this particular file.
    let priorErrors = checkForErrors ()

    let mutable cnt = 1

    let processFile
        ((input, logger): ParsedInput * DiagnosticsLogger)
        ((currentTcState, _currentPriorErrors): State)
        : bool -> State -> PartialResult * State =
        cancellable {
            use _ = UseDiagnosticsLogger logger
            // printfn $"Processing AST {file.ToString()}"
            // Is it OK that we don't update 'priorErrors' after processing batches?
            let checkForErrors2 () = priorErrors || (logger.ErrorCount > 0)

            let tcSink = TcResultsSink.NoSink
            // let c = cnt
            cnt <- cnt + 1

            // printfn $"#{c} [thread {Thread.CurrentThread.ManagedThreadId}] Type-checking {input.FileName}"

            let! f =
                CheckOneInput'(
                    checkForErrors2,
                    tcConfig,
                    tcImports,
                    tcGlobals,
                    prefixPathOpt,
                    tcSink,
                    currentTcState,
                    input,
                    false // skipImpFiles...
                )

            // printfn $"Finished Processing AST {file.ToString()}"
            return
                (fun (isFinalFold: bool) (state: State) ->

                    // printfn $"Applying {file.ToString()}"
                    let tcState, priorErrors = state
                    let (partialResult: PartialResult, tcState) = f isFinalFold tcState

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

        let processFile (fileIdx: int) (state: State) : bool -> State -> PartialResult * State =
            let parsedInput, logger = inputsWithLoggers.[fileIdx]
            processFile (parsedInput, logger) state

        let folder: bool -> State -> SingleResult -> FinalFileResult * State = folder
        let _qnof = QualifiedNameOfFile.QualifiedNameOfFile(Ident("", Range.Zero))
        let state: State = tcState, priorErrors

        let partialResults, (tcState, _) =
            GraphProcessing.processGraph<int, State, SingleResult, FinalFileResult>
                graph
                processFile
                folder
                // When combining results, order them by index
                id
                state
                (fun _ -> true)
                10

        partialResults |> Array.toList, tcState)
