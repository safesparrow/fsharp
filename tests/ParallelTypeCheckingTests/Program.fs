module internal ParallelTypeCheckingTests.Program

#nowarn "1182"

open System
open ParallelTypeCheckingTests.TestUtils

let parseMethod (method: string) =
    match method.ToLower() with
    | "sequential" -> Method.Sequential
    | "parallelfs" -> Method.ParallelCheckingOfBackedImplFiles
    | "graph" -> Method.Graph
    | _ -> failwith $"Unrecognised mode: {method}"

let parse (argv: string[]) : Args =
    match argv with
    | [| codebaseNr; method |] ->
        let codebaseNr = Int32.Parse codebaseNr
        let code = TestCompilationFromCmdlineArgs.codebases[codebaseNr]
        let method = parseMethod method
        TestCompilationFromCmdlineArgs.codebaseToConfig code method
    | _ -> failwith "Invalid args - use 'args_path [method [fs-parallel]]'"

[<EntryPoint>]
let main argv =
    FSharp.Compiler.ParseAndCheckInputs.CheckMultipleInputsUsingGraphMode <-
        ParallelTypeChecking.CheckMultipleInputsInParallel
    let args = parse argv
    TestCompilationFromCmdlineArgs.TestCompilerFromArgs args
    0