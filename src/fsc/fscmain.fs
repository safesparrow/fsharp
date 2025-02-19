// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module FSharp.Compiler.CommandLineMain

open System
open System.Reflection
open System.Runtime
open System.Runtime.CompilerServices
open System.Threading

open Internal.Utilities.Library
open Internal.Utilities.Library.Extras
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.Driver
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

[<Dependency("FSharp.Compiler.Service", LoadHint.Always)>]
do ()

type Timer(name: string) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    do printfn $"{name} - start"

    member this.Dispose() =
        printfn $"{name} - end - {sw.Elapsed.TotalSeconds}s"

    interface IDisposable with
        member this.Dispose() = this.Dispose()

let internal mainAux (argv: string[], onlyTypeCheck: bool, exiter: Exiter option) : int =
    use _ = FSharp.Compiler.Diagnostics.Activity.startNoTags "fscmain"

    use _ = new Timer("main")

    let compilerName =
        // the 64 bit desktop version of the compiler is name fscAnyCpu.exe, all others are fsc.exe
        if
            Environment.Is64BitProcess
            && typeof<obj>.Assembly.GetName().Name <> "System.Private.CoreLib"
        then
            "fscAnyCpu.exe"
        else
            "fsc.exe"

    Thread.CurrentThread.Name <- "F# Main Thread"

    // Set the initial phase to garbage collector to batch mode, which improves overall performance.
    use _ = UseBuildPhase BuildPhase.Parameter

    // An SDL recommendation
    UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption()

    try

        // We are on a compilation thread
        let ctok = AssumeCompilationThreadWithoutEvidence()

        // The F# compiler expects 'argv' to include the executable name, though it makes no use of it.
        let argv = Array.append [| compilerName |] argv

        // Check for --pause as the very first step so that a debugger can be attached here.
        let pauseFlag = argv |> Array.exists (fun x -> x = "/pause" || x = "--pause")

        if pauseFlag then
            System.Console.WriteLine("Press return to continue...")
            System.Console.ReadLine() |> ignore

        // Set up things for the --times testing flag
        let timesFlag = argv |> Array.exists (fun x -> x = "/times" || x = "--times")

        if timesFlag then
            let stats = ILBinaryReader.GetStatistics()

            AppDomain.CurrentDomain.ProcessExit.Add(fun _ ->
                printfn
                    "STATS: #ByteArrayFile = %d, #MemoryMappedFileOpen = %d, #MemoryMappedFileClosed = %d, #RawMemoryFile = %d, #WeakByteArrayFile = %d"
                    stats.byteFileCount
                    stats.memoryMapFileOpenedCount
                    stats.memoryMapFileClosedCount
                    stats.rawMemoryFileCount
                    stats.weakByteFileCount)

        // Get the handler for legacy resolution of references via MSBuild.
        let legacyReferenceResolver = LegacyMSBuildReferenceResolver.getResolver ()

        let exiter = exiter |> Option.defaultValue QuitProcessExiter

        // Perform the main compilation.
        //
        // This is the only place where ReduceMemoryFlag.No is set. This is because fsc.exe is not a long-running process and
        // thus we can use file-locking memory mapped files.
        //
        // This is also one of only two places where CopyFSharpCoreFlag.Yes is set.  The other is in LegacyHostedCompilerForTesting.
        CompileFromCommandLineArguments(
            ctok,
            argv,
            legacyReferenceResolver,
            false,
            ReduceMemoryFlag.No,
            CopyFSharpCoreFlag.Yes,
            exiter,
            ConsoleLoggerProvider(),
            None,
            None,
            not onlyTypeCheck
        )

        0

    with e ->
        // Last-chance error recovery (note, with a poor error range)
        errorRecovery e Range.range0
        1

[<EntryPoint>]
let main (argv: string[]) : int = mainAux (argv, false, None)
