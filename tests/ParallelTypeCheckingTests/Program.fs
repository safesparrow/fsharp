module ParallelTypeCheckingTests.Program
#nowarn "1182"
open ParallelTypeCheckingTests.Utils

let parse (argv: string[]): Args =
    let parseMode (mode : string) =
        match mode.ToLower() with
        | "sequential" -> Method.Sequential
        | "parallelfs" -> Method.ParallelFs
        | "nojaf" -> Method.Nojaf
        | "graph" -> Method.Graph
        | _ -> failwith $"Unrecognised method: {mode}"
    
    let path, mode, workingDir =
        match argv with
        | [|path|] ->
            path, Method.Sequential, None
        | [|path; mode|] ->
            path, parseMode mode, None
        | [|path; mode; workingDir|] ->
            path, parseMode mode, Some workingDir
        | _ -> failwith "Invalid args - use 'args_path [fs-parallel]"
    
    {
        Path = path
        LineLimit = None
        Mode = mode
        WorkingDir = workingDir
    }

[<EntryPoint>]
let main argv =
    let stuff = parse argv
    CompilationFromArgsTests.TestCompilerFromArgs stuff
    0