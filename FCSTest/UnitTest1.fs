namespace FCSTest

open System
open System.Diagnostics
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open NUnit.Framework
open Newtonsoft.Json
open Serializing


[<TestFixture>]
module X =
    let mutable checker : FSharpChecker option = None

    [<SetUp>]
    let Setup () =
        checker <- 
            FSharpChecker.Create(projectCacheSize = 200,
                                 keepAllBackgroundResolutions = false,
                                 keepAllBackgroundSymbolUses = false,
                                 enablePartialTypeChecking = false)
            |> Some
    let converter = Newtonsoft.Json.Converters.DiscriminatedUnionConverter()
        

    let parseAndCheckFileInProject (args : ParseArgs) =
        checker.Value.ParseAndCheckFileInProject(
            args.fileName,
            args.fileVersion,
            args.sourceText,
            args.options,
            ""
        )
        
    let r = new Random()
    let modifyText (text : ISourceText) =
        SourceText.ofString (text.GetSubTextString(0, text.Length) + $"//{r.Next()}")
        
    let dirs = {
        NugetPackages = "C:\\Users\\janus\\.nuget\\packages"
        CodeRoot = "D:\\projekty\\fantomas"
    }
    
    let convertJsonFile (path : string) =
        let json = File.ReadAllText(path)
        let fake = Newtonsoft.Json.JsonConvert.DeserializeObject<FakeParseArgs> json
        let replaced = replaceArgs (dirs |> Dirs.toPlaceholders) fake
        let json = Newtonsoft.Json.JsonConvert.SerializeObject(replaced, Formatting.Indented)
        File.WriteAllText(path, json)
        
    let go (name : string) =
        let sw = Stopwatch.StartNew()
        let path = "d:/projekty/fsharp/FCSTest/dumps/" + name
        convertJsonFile path
        let argsJson = File.ReadAllText(path)
        let args = deserialize dirs argsJson
        let args = {args with sourceText = modifyText args.sourceText}
        let x = parseAndCheckFileInProject args
        let y = x |> Async.RunSynchronously
        System.Console.WriteLine (sw.Elapsed)
        ()
        
    let goMulti (names : string[]) =
        System.Console.WriteLine ($"start - - {System.GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
        names
        |> Array.iter (fun name ->
            let argsJson = File.ReadAllText("d:/projekty/fsharp/FCSTest/dumps/" + name)
            let sw = Stopwatch.StartNew()
            let args = deserialize dirs argsJson
            let args = {args with sourceText = modifyText args.sourceText}
            let x = parseAndCheckFileInProject args
            let y = x |> Async.RunSynchronously
            System.Console.WriteLine ($"{name} - {sw.Elapsed} - {System.GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
        )
        
    [<Test>]
    [<Explicit>]
    let TwoProjects () =
        go "Program.fs.2022-06-04_003526.json"
        
    [<Test>]
    [<Explicit>]
    let FantomasEasy () =
        go "Parse.fs.2022-06-04_004240.json"

    
    [<Test>]
    [<Explicit>]
    let FantomasHard () =
        go "DaemonTests.fs.2022-06-04_004541.json"
    
    [<Test>]
    [<Explicit>]
    let FantomasHardMulti () =
        goMulti [|
            "1_Program.fs.2022-06-11_012033.json"
            "2_DaemonTests.fs.2022-06-11_012110.json"
        |]
        
    [<Test>]
    [<Explicit>]
    let Tester() =
        [|1; 2; 3; 4; 5; 6|]
        |> Array.map (fun x -> x*x)
        |> Array.filter (fun x -> x % 2 = 0)
        |> Array.collect (fun x -> [|x; x+1|])
        |> Array.sum
        |> ignore