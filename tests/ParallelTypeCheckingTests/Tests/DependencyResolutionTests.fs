module ParallelTypeCheckingTests.Tests.DependencyResolutionTests

open NUnit.Framework
open ParallelTypeCheckingTests.DependencyResolution
open ParallelTypeCheckingTests
open Scenarios

let scenarios = codebases

[<TestCaseSource(nameof scenarios)>]
let ``Supported scenario`` (scenario: Scenario) =
    let graph = mkGraph (Array.map (fun f -> f.FileWithAST) scenario.Files)

    for file in scenario.Files do
        let expectedDeps = file.ExpectedDependencies
        let actualDeps = graph.[file.FileWithAST.Idx]
        Assert.AreEqual(expectedDeps, actualDeps, $"Dependencies don't match for {System.IO.Path.GetFileName file.FileWithAST.File}")
