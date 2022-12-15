module ParallelTypeCheckingTests.Tests.DependencyResolutionTests

open NUnit.Framework
open ParallelTypeCheckingTests.DependencyResolution
open ParallelTypeCheckingTests
open Scenarios

let scenarios = codebases

[<TestCaseSource(nameof scenarios)>]
let ``Supported scenario`` (scenario: Scenario) =
    let files = Array.map (fun f -> f.FileWithAST) scenario.Files
    let filePairs = FilePairMap(files)
    let graph = mkGraph filePairs files

    for file in scenario.Files do
        let expectedDeps = file.ExpectedDependencies
        let actualDeps = graph.[file.FileWithAST.Idx]
        Assert.AreEqual(expectedDeps, actualDeps, $"Dependencies don't match for {System.IO.Path.GetFileName file.FileWithAST.File}")
