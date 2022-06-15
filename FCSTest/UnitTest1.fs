namespace FCSTest

open System
open System.Diagnostics
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Microsoft.Build.Construction
open Microsoft.Build.Evaluation
open NUnit.Framework
open Newtonsoft.Json
open Serializing


[<TestFixture>]
module X =
    
    [<Test>]
    let Generate () =
        let dir = "d:/projekty/fsharp/FCSTest/solution"
        let makePath suffix = Path.Combine(dir, suffix)
        let sourceFilePath = makePath "library.fs"
        
        let makeEmptyProject () =
            let p = Project(NewProjectFileOptions.None)
            p.Xml.Sdk <- "Microsoft.NET.Sdk"
            p.SetProperty("TargetFramework", "net6.0") |> ignore
            p.SetProperty("DisableImplicitFSharpCoreReference", "true") |> ignore
            p
        
        let writeProject (name : string) =
            if Directory.Exists (makePath name) then Directory.Delete (makePath name, true)
            Directory.CreateDirectory (makePath name) |> ignore
            let dst = makePath (Path.Combine(name, "library.fs"))
            File.Copy(sourceFilePath, dst)
            File.WriteAllText(dst, File.ReadAllText(dst).Replace("module FCSTest.library", $"module {name}"))
            let p = makeEmptyProject()
            p.AddItem("Compile", "library.fs") |> ignore
            p.Save(makePath (Path.Combine(name, $"{name}.fsproj")))
        
        let name = "root"
        Directory.Delete(makePath name, true)
        let slnName = "solution"
        let slnPath = makePath $"{slnName}.sln"
        if File.Exists slnPath then File.Delete slnPath
        Directory.CreateDirectory (makePath name) |> ignore
        let dst = makePath (Path.Combine(name, "library.fs"))
        File.Copy(sourceFilePath, dst)
        let nrs = [1..7]
        let x =
            nrs
            |> List.map (fun i -> $"open leaf_{i}")
            |> fun l -> String.Join(Environment.NewLine, l)
        let y =
            nrs
            |> List.map (fun i -> $"let x{i} = leaf_{i}.Z1.Y1.X1.sum1 2 3")
            |> fun l -> String.Join(Environment.NewLine, l)
        let x = $"{x}{Environment.NewLine}{y}"
            
        File.WriteAllText(dst, File.ReadAllText(dst).Replace("module FCSTest.library", $"module FCSTest.library{Environment.NewLine}{x}"))
        let root = makeEmptyProject()
        root.AddItem("Compile", "library.fs") |> ignore
        
        let runX (args : string) = 
            let psi = ProcessStartInfo("dotnet", args)
            psi.WorkingDirectory <- dir
            let p = Process.Start(psi)
            p.WaitForExit()
            printfn $"Running 'dotnet {args}' in '{dir}' returned with exit code {p.ExitCode}"
        
        runX "new sln -n solution"
        
        for i in nrs do
            let name = $"leaf_{i}"
            writeProject name
            root.AddItem("ProjectReference", $"../{name}/{name}.fsproj") |> ignore
            runX $"sln add {name}"
        
        root.Save(makePath (Path.Combine(name, $"{name}.fsproj")))
        runX $"sln add {name}"
    
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
        
    let parallelise (o : )
        
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
        
        
    [<TestCase("TwoProjects", "Program.fs.2022-06-04_003526.json")>]
    [<TestCase("FantomasTopFile", "DaemonTests.fs.2022-06-04_004541.json")>]
    [<TestCase("FantomasFirstFile", "Parse.fs.2022-06-04_004240.json")>]
    [<TestCase("root_7leaves", "root_7leaves_library.fs.2022-06-15_222554.json")>]
    [<TestCase("root_20leaves", "root_20leaves_library.fs.2022-06-15_220456.json")>]
    [<Explicit>]
    let GoAny (name : string, file : string) =
        go file
        
    // [<Test>]
    // [<Explicit>]
    // let FantomasEasy () =
    //     go "Parse.fs.2022-06-04_004240.json"
    //     let calls = Microsoft.FSharp.Collections.CollectionTracing.calls
    //     let rows = 
    //         calls
    //         |> Seq.groupBy (fun e -> e.Function)
    //         |> Seq.map (fun (f, entries) -> f, entries |> Seq.toArray |> Array.length)
    //         |> Seq.toArray
    //     printfn $"+%A{rows}"
    //     ()

    
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