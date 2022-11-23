namespace global

open NUnit.Framework
open OpenTelemetry.Trace


/// One-time setup for NUnit tests
[<SetUpFixture>]
type AssemblySetUp() =
    let mutable tracerProvider = None

    [<OneTimeSetUp>]
    member this.SetUp() =
        FSharp.Compiler.ParseAndCheckInputs.CheckMultipleInputsUsingGraphMode <-
            ParallelTypeCheckingTests.ParallelTypeChecking.CheckMultipleInputsInParallel
        FSharp.Compiler.OptimizeInputs.goer <- ParallelTypeCheckingTests.Code.GraphBasedOpt.goGraph |> Some
        tracerProvider <- ParallelTypeCheckingTests.TestUtils.setupOtel () |> Some

    [<OneTimeTearDown>]
    member this.TearDown() =
        tracerProvider
        |> Option.iter (fun x ->
            x.ForceFlush() |> ignore
            x.Dispose())

        tracerProvider <- None
