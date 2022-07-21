module BenchmarkRunner

open System
open System.Diagnostics
open System.IO
open CheckerBenchmark.Dto
open FSharp.Compiler.CodeAnalysis
open Newtonsoft.Json

// member bc.ParseAndCheckFileInProject(fileName: string, fileVersion, sourceText: ISourceText, options: FSharpProjectOptions, userOpName) =
type AnalyseFile =
    {
        FileName: string
        FileVersion: int
        SourceText: string
        Options: FSharpProjectOptions
    }

type BenchmarkAction =
    | AnalyseFile of AnalyseFile
    
type BenchmarkInputs =
    {
        Actions : BenchmarkAction list
        Config : BenchmarkConfig
    }
        
module Deserialization =

    let rec private optionsFromDto (o : FSharpProjectOptionsDto) : FSharpProjectOptions =       
        let fakeRP (rp : FSharpReferenceDto) : FSharpReferencedProject =
            let back = optionsFromDto rp.Options
            FSharpReferencedProject.CreateFSharp(rp.OutputFile, back)
        {
            ProjectFileName = o.ProjectFileName
            ProjectId = o.ProjectId
            SourceFiles = o.SourceFiles
            OtherOptions = o.OtherOptions
            ReferencedProjects =
                o.ReferencedProjects
                |> Array.map fakeRP
            IsIncompleteTypeCheckEnvironment = o.IsIncompleteTypeCheckEnvironment
            UseScriptResolutionRules = o.UseScriptResolutionRules
            LoadTime = o.LoadTime
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = o.Stamp
        }
    
    let private actionFromDto (dto : BenchmarkActionDto) =
        match dto with
        | BenchmarkActionDto.AnalyseFile x ->
            {
                AnalyseFile.FileName = x.FileName
                FileVersion = x.FileVersion
                SourceText = x.SourceText
                Options = x.Options |> optionsFromDto
            }
            |> BenchmarkAction.AnalyseFile
    
    let private inputsFromDto (dto : BenchmarkInputsDto) =
        {
            BenchmarkInputs.Actions = dto.Actions |> List.map actionFromDto
            Config = dto.Config
        }
            
    let deserializeInputsJson (json : string) : BenchmarkInputs =
        let settings = JsonSerializerSettings(PreserveReferencesHandling = PreserveReferencesHandling.All)
        let dto = JsonConvert.DeserializeObject<BenchmarkInputsDto>(json, settings)
        inputsFromDto dto

type FCSBenchmark (config : BenchmarkConfig) =
    let checker = FSharpChecker.Create(projectCacheSize = config.ProjectCacheSize)
        
    let failOnErrors (results : FSharpCheckFileResults) =
        () // TODO revert
        //if results.Diagnostics.Length > 0 then failwithf $"had errors: %A{results.Diagnostics}"
    
    let performAction (action : BenchmarkAction) =
        let sw = Stopwatch.StartNew()
        let res =
            match action with
            | AnalyseFile x ->
                let result, answer =
                    checker.ParseAndCheckFileInProject(x.FileName, x.FileVersion, FSharp.Compiler.Text.SourceText.ofString x.SourceText, x.Options)
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
        File.WriteAllText(inputFile, File.ReadAllText(inputFile).Replace("FSharpOptions.Benchmarking+BenchmarkInputs", "Say+Benchmarking+BenchmarkInputs"))
        let json = File.ReadAllText(inputFile)
        let inputs = Deserialization.deserializeInputsJson json
        run inputs
        0
    | _ ->
        printfn $"Invalid args: {args}"; 1
    