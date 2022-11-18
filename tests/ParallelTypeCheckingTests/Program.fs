module internal ParallelTypeCheckingTests.Program

open ParallelTypeCheckingTests.TestUtils

let parseArgs (argv: string[]) : Args =
    let parseMode (method: string) =
        match method.ToLower() with
        | "sequential" -> Method.Sequential
        | "parallelfs" -> Method.ParallelCheckingOfBackedImplFiles
        | "graph" -> Method.Graph
        | _ -> failwith $"Unrecognised method: {method}"

    let method, path =
        match argv with
        | [| path |] -> Method.Graph, path
        | [| method; path |] -> parseMode method, path
        | _ -> failwith "Invalid args. Usage: '%method% %project_file%'"

    {
        Method = method
        ProjectFile = path
        Parallel = false
    }

[<EntryPoint>]
let main argv =
    let args = parseArgs argv
    TestCompilationFromCmdlineArgs.TestCompilerFromArgs args
    0
