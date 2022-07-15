namespace FCSTest

open System
open System.Diagnostics
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Microsoft.Build.Evaluation
open Serializing

module AnalysisAdhocTests =
    
    let GenerateSampleSolution () =
        SolutionGenerator.generate "d:/projekty/fsharp/FCSTest/solution" 7
    
    let mutable checker : FSharpChecker option = None

    let Setup () =
        checker <- 
            FSharpChecker.Create(projectCacheSize = 200,
                                 keepAllBackgroundResolutions = false,
                                 keepAllBackgroundSymbolUses = false,
                                 enablePartialTypeChecking = false)
            |> Some
        
    let private parseAndCheckFileInProject (args : ParseArgs) =
        checker.Value.ParseAndCheckFileInProject(
            args.fileName,
            args.fileVersion,
            args.sourceText,
            args.options,
            ""
        )
    
    let private doRunAnalysis (dirs : TestDirs) (path : string) =
        let argsJson = File.ReadAllText(path)
        let args = deserialize dirs argsJson
        let sw = Stopwatch.StartNew()
        let result =
            parseAndCheckFileInProject args
            |> Async.RunSynchronously
        printfn $"Analysis of ${path} took {sw.ElapsedMilliseconds}ms"
        
    let private r = Random()
    let private modifyText (text : ISourceText) =
        SourceText.ofString (text.GetSubTextString(0, text.Length) + $"//{r.Next()}")
        
    let private doRunAnalysisMulti (dirs : TestDirs) (name : string) (n : int) =
        let argsJson = File.ReadAllText name
        let mutable args = deserialize dirs argsJson
        Console.WriteLine ($"start - {GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
        [|1..n|]
        |> Array.iter (fun name ->
            let go (modify : bool) =
                if modify then args <- {args with sourceText = modifyText args.sourceText}
                let sw = Stopwatch.StartNew()
                let result =
                    parseAndCheckFileInProject args
                    |> Async.RunSynchronously
                Console.WriteLine ($"{name} - {sw.Elapsed} - {GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
            go true
            go false
        )
        
    let dirs = {
        NugetPackages = $"C:\\Users\\{Environment.UserName}\\.nuget\\packages"
        CodeRoot = "D:\\projekty\\fantomas"
    }
    
    let examples = [
        "TwoProjects_Program.fs.json"
        "Fantomas_TopProject_DaemonTests.fs.json"
        "Fantomas_LeafProject_Parse.fs.json"
    ]
    
    [<EntryPoint>]
    let rec main args =
        match args with
        | [||] ->
            let x = Console.ReadLine()
            main [|x|]
        | [|x|] ->
             match Int32.TryParse x with
             | true, i when i>=0 && i<3 -> doRunAnalysis dirs $"dumps/{examples[i]}"
             | true, _ -> failwith "Invalid args"
             | _ -> doRunAnalysis dirs x
             0
        | _ -> failwith "invalid args"
