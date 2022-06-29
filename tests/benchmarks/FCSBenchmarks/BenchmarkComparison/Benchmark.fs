namespace Benchmark

open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<AbstractClass>]
type SingleFileCompilerBenchmarkBase(compiler : SingleFileCompiler) =
    [<GlobalSetup>]
    member _.Setup() =
        compiler.Setup()

    [<Benchmark>]
    member _.Run() =
        compiler.Run()

    [<IterationCleanup>]
    member _.Cleanup() =
        compiler.Cleanup()

[<MemoryDiagnoser>]
type DecentlySizedStandAloneFileBenchmark() =
    inherit SingleFileCompilerBenchmarkBase(
        SingleFileCompiler(
            Path.Combine(__SOURCE_DIRECTORY__, "../decentlySizedStandAloneFile.fs"),
            OptionsCreationMethod.FromScript
        )
    )

module Benchmark =

    [<EntryPoint>]
    let main args =
        BenchmarkSwitcher.FromAssembly(typeof<DecentlySizedStandAloneFileBenchmark>.Assembly).Run(args) |> ignore
        0