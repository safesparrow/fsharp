module ArgsGenerator

open System.IO
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

let optionsToArgs (opts: FSharpProjectOptions) : string[] =
    Array.append opts.OtherOptions opts.SourceFiles

let loadProjectArgs (projectFile: string) =
    let dir = Path.GetDirectoryName projectFile
    let _ = Init.init (DirectoryInfo dir) None

    let props =
        [
            "TargetFramework", "net7.0"
            "Configuration", "Release"
        ]
    let res =
        ProjectLoader.getProjectInfo projectFile props (BinaryLogGeneration.Within(DirectoryInfo("c:/projekty/fsharp/cracking_logs")))

    match res [] with
    | Result.Ok _res ->
        let fcs = FCS.mapToFSharpProjectOptions _res []
        let args = optionsToArgs fcs
        args
    | Result.Error err -> failwith $"Failed to crack project: {err}"

/// Given a project file to analyse, generates fsc commandline arguments for compiling it, then saves them to a file.
[<EntryPoint>]
let main argv =
    match argv with
    | [| projectFile; outputFile |] ->
        printfn $"Loading project args for project file: {projectFile}"
        let args = loadProjectArgs projectFile
        printfn $"Loaded {args.Length} args. Saving them to {outputFile}"
        File.WriteAllLines(outputFile, args)
        0
    | _ ->
        failwith "Invalid args. Usage: 'ArgsGenerator.exe %project_file% %output_file%'"
