module internal ParallelTypeCheckingTests.Program

#nowarn "1182"

open FSharp.Compiler.CompilerConfig
open ParallelTypeCheckingTests.TestUtils

// let _parse (argv: string[]) : Args =
//     let parseMode (mode: string) =
//         match mode.ToLower() with
//         | "sequential" -> Method.Sequential
//         | "parallelfs" -> Method.ParallelCheckingOfBackedImplFiles
//         | "graph" -> Method.Graph
//         | _ -> failwith $"Unrecognised mode: {mode}"
//
//     let path, mode, workingDir =
//         match argv with
//         | [| path |] -> path, Method.Sequential, None
//         | [| path; method |] -> path, parseMode method, None
//         | [| path; method; workingDir |] -> path, parseMode method, Some workingDir
//         | _ -> failwith "Invalid args - use 'args_path [method [fs-parallel]]'"
//
//     {
//         Path = path
//         LineLimit = None
//         Method = mode
//         WorkingDir = workingDir
//     }
open ParallelTypeCheckingTests
open ParallelTypeCheckingTests.DependencyResolution

[<EntryPoint>]
let main _argv =
    let filesWithAST =
        fcsFiles
        |> Array.Parallel.mapi (fun idx file ->
            {
                Idx = idx
                AST = FSharp.Compiler.Service.Tests.Common.parseSourceCode (file, System.IO.File.ReadAllText(file))
                File = file
            })

    let _graph = mkGraph filesWithAST
    0
