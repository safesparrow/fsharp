module Say

open System
open System.Diagnostics
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open MBrace.FsPickler


module Benchmarking =
    type BenchmarkAction =
        // member bc.ParseAndCheckFileInProject(fileName: string, fileVersion, sourceText: ISourceText, options: FSharpProjectOptions, userOpName) =
        | AnalyseFile of fileName : string * sourceText : string * options : FSharpProjectOptions
        
    type BenchmarkConfig =
        {
            ProjectCacheSize : int
        }
        with static member makeDefault () = {ProjectCacheSize = 200}

    type BenchmarkInputs =
        {
            Actions : BenchmarkAction list
            Config : BenchmarkConfig
        }


open Benchmarking

let private deserialize (data : byte[]) : BenchmarkInputs =
    let serializer = FsPickler.CreateBinarySerializer()
    serializer.DisableSubtypeResolution <- true
    serializer.UnPickle<BenchmarkInputs> data

type FCSBenchmark (config : BenchmarkConfig) =
    let checker = FSharpChecker.Create(projectCacheSize = config.ProjectCacheSize)
        
    let failOnErrors (results : FSharpCheckFileResults) =
        if results.Diagnostics.Length > 0 then failwithf $"had errors: %A{results.Diagnostics}"
    
    let performAction (action : BenchmarkAction) =
        let sw = Stopwatch.StartNew()
        let res =
            match action with
            | AnalyseFile (filePath, sourceText, options) ->
                let result, answer =
                    checker.ParseAndCheckFileInProject(filePath, 0, SourceText.ofString sourceText, options)
                    |> Async.RunSynchronously
                match answer with
                | FSharpCheckFileAnswer.Aborted -> failwith "checker aborted"
                | FSharpCheckFileAnswer.Succeeded results ->
                    failOnErrors results
                action, ((result, answer) :> Object)
        printfn $"Performed action {action.GetType()} in {sw.ElapsedMilliseconds}ms"
        res
            
    let cleanCaches () =
        checker.InvalidateAll()
        checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
        
    member this.Checker = checker
    member this.PerformAction action = performAction action
    member this.CleanCaches () = cleanCaches

let run (inputs : BenchmarkInputs) =
    let sw = Stopwatch.StartNew()
    let b = FCSBenchmark(inputs.Config)
    let outputs =
        inputs.Actions
        |> List.map b.PerformAction
    printfn $"Performed {outputs.Length} actions in {sw.ElapsedMilliseconds}ms"
    ()

[<EntryPoint>]
let main args =
    match args with
    | [|inputFile|] ->
        let rawData = File.ReadAllBytes(inputFile)
        let inputs = deserialize rawData
        run inputs
        0
    | _ ->
        printfn $"Invalid args: {args}"; 1
    