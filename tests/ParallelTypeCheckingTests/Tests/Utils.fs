module ParallelTypeCheckingTests.TestUtils

open System
open System.IO
open FSharp.Compiler
open FSharp.Compiler.CompilerConfig
open Xunit
open FSharp.Test
open FSharp.Test.Compiler
open OpenTelemetry
open OpenTelemetry.Resources
open OpenTelemetry.Trace
open System.Diagnostics

let packages =
    // Here we assume that the NuGet packages are located in a certain user folder,
    // and that the projects being compiled use that global package cache
    let userprofile = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    let pathWithEnv = $@"{userprofile}\.nuget\packages"
    Environment.ExpandEnvironmentVariables(pathWithEnv)

let replacePaths (s: string) =
    s |> fun s -> s.Replace("$PACKAGES$", packages)

[<Struct>]
type Method =
    | Sequential
    | ParallelCheckingOfBackedImplFiles
    | Graph

let methods = [ Method.Sequential; Method.Graph ]

let setupOtel () =
    Sdk
        .CreateTracerProviderBuilder()
        .AddSource("fsc")
        .SetResourceBuilder(
            ResourceBuilder
                .CreateDefault()
                .AddService(serviceName = "program", serviceVersion = "42.42.42.44")
        )
        .AddJaegerExporter(fun c ->
            c.BatchExportProcessorOptions.MaxQueueSize <- 10000000
            c.BatchExportProcessorOptions.MaxExportBatchSize <- 10000000
            c.ExportProcessorType <- ExportProcessorType.Simple
            c.MaxPayloadSizeInBytes <- Nullable(1000000000))
        .Build()


type Args =
    {
        Method : Method
        ProjectFile : string
        Parallel : bool
    }
    
let makeCompilationUnit (files: (string * string) list) : CompilationUnit =
    let files =
        files |> List.map (fun (name, code) -> SourceCodeFileKind.Create(name, code))

    match files with
    | [] -> failwith "empty files"
    | first :: rest ->
        let f = fsFromString first |> FS
        f |> withAdditionalSourceFiles rest

let internal mapMethod (method: Method) =
    match method with
    | Method.Sequential -> TypeCheckingMode.Sequential
    | Method.ParallelCheckingOfBackedImplFiles -> TypeCheckingMode.ParallelCheckingOfBackedImplFiles
    | Method.Graph -> TypeCheckingMode.Graph

let runProcess
    (name : string)
    (args : string)
    workingDir
    (envVariables : (string * string) list)
    =
    let info = ProcessStartInfo ()
    info.WindowStyle <- ProcessWindowStyle.Hidden
    info.Arguments <- args
    info.FileName <- name
    info.UseShellExecute <- false
    info.WorkingDirectory <- workingDir
    info.CreateNoWindow <- true

    envVariables |> List.iter (fun (k, v) -> info.EnvironmentVariables[ k ] <- v)

    printfn $"Running '{name} {args}' in '{workingDir}'"
    let p = new Process (StartInfo = info)
    p.EnableRaisingEvents <- true
    p.Start () |> ignore
    p.WaitForExit ()

    if p.ExitCode <> 0 then
        failwith $"Running process '{name} {args}' failed."

let getProjectArgs (projectFile : string) : string[] =
    // TODO Make ArgsGenerator a dotnet tool
    //let argsGeneratorProjectPath = $"{__SOURCE_DIRECTORY__}/../../ArgsGenerator/ArgsGenerator.fsproj"
    let argsGeneratorProjectPath = $"{__SOURCE_DIRECTORY__}/../../ArgsGenerator/ArgsGenerator.fsproj"
    let workingDir = "."
    let name = "dotnet.exe" // TODO Handle non-Windows OS
    let argsFile = Path.GetTempFileName()
    let envVariables = []
    
    // We run the ArgsGenerator project using 'dotnet run', and specify two arguments: project file to analyse, output file.
    let args =
        [|
            "run"
            "--project"
            argsGeneratorProjectPath
            "--"
            $"\"{projectFile}\""
            argsFile
        |]
        |> fun args -> String.Join(" ", args)
    
    runProcess name args workingDir envVariables
    
    let output = File.ReadAllLines argsFile
    output

/// Given FSC arguments, extract those that represent source files
let extractSourceFilesFromArgs (args: string[]): string[] =
    args
    |> Array.filter (fun arg -> arg.StartsWith("-") |> not)