namespace FCSTest

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open FCSTest.Serializing
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
        
    let parallelise (o : ParseArgs) : Dictionary<int64, FSharpProjectOptions * int64[]> =
        let projects = Dictionary<int64, FSharpProjectOptions * int64[]>()
        let rec traverse (p : FSharpProjectOptions) =
            match p.Stamp with
            | None -> failwith "NO STAMP"
            | Some s ->
                match projects.TryGetValue s with
                | true, _ -> ()
                | false, _ ->
                    let deps = p.ReferencedProjects |> Array.map (fun r -> r.Options.Value.Stamp.Value)
                    projects[s] <- p, deps
                    p.ReferencedProjects
                    |> Array.iter (fun r -> traverse r.Options.Value)
        traverse o.options
        projects
    
    let goSmart (args : ParseArgs) =
        let map = parallelise args
        
        let q = Queue<int64>()
        
        let deps =
            map
            |> Seq.map (fun (KeyValue(s, (p,d))) -> s, d |> System.Linq.Enumerable.ToHashSet)
            |> dict
            
        deps
        |> Seq.filter (fun (KeyValue(s,deps)) -> deps.Count = 0)
        |> Seq.iter (fun (KeyValue(s, _)) -> q.Enqueue s)
        
        let createArgs (p : FSharpProjectOptions) =
            let sourcePath = p.SourceFiles |> Array.last
            let source = File.ReadAllText sourcePath
            {
                fileName = sourcePath
                fileVersion = Convert.ToInt32(p.Stamp.Value)
                sourceText = SourceText.ofString source
                ParseArgs.options = p
            }
        
        let sw = Stopwatch.StartNew()
        while q.Count > 0 do
            let s = q.Dequeue()
            let p, _ = map[s]
            let args = createArgs p
            parseAndCheckFileInProject args |> Async.RunSynchronously |> ignore
            printfn $"{sw.ElapsedMilliseconds}ms - Checked {p.ProjectFileName}"
            // remove s from all projects' deps
            deps
            |> Seq.iter (fun (KeyValue(s2, deps2)) ->
                let p2, _ = map[s2]
                if deps2.Remove s && deps2.Count = 0 then
                    q.Enqueue s2
                    printfn $"Enqueue {p2.ProjectFileName}"
            )
        
    let fetchArgs (name : string) =
        let path = "d:/projekty/fsharp/FCSTest/dumps/" + name
        convertJsonFile path
        let argsJson = File.ReadAllText(path)
        deserialize dirs argsJson
        
    let go (args : ParseArgs) =
        let sw = Stopwatch.StartNew()
        let x = parseAndCheckFileInProject args
        let y = x |> Async.RunSynchronously
        Console.WriteLine (sw.Elapsed)
        ()
        
    let goMulti (args : ParseArgs[]) =
        System.Console.WriteLine ($"start - - {System.GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
        args
        |> Array.iter (fun args ->
            let sw = Stopwatch.StartNew()
            let x = parseAndCheckFileInProject args |> Async.RunSynchronously
            Console.WriteLine ($"{args.fileName} - {sw.Elapsed} - {GC.GetTotalAllocatedBytes()/1024L/1024L}MB allocated so far")
        )
        
    [<TestCase("TwoProjects", "Program.fs.2022-06-04_003526.json")>]
    [<TestCase("FantomasTopFile", "DaemonTests.fs.2022-06-04_004541.json")>]
    [<TestCase("FantomasFirstFile", "Parse.fs.2022-06-04_004240.json")>]
    [<TestCase("root_7leaves", "root_7leaves_library.fs.2022-06-15_222554.json")>]
    [<TestCase("root_20leaves", "root_20leaves_library.fs.2022-06-15_220456.json")>]
    [<Explicit>]
    let GoSmart (name : string, file : string) =
        file
        |> fetchArgs
        |> goSmart
        ()
        
    [<TestCase("TwoProjects", "Program.fs.2022-06-04_003526.json")>]
    [<TestCase("FantomasTopFile", "DaemonTests.fs.2022-06-04_004541.json")>]
    [<TestCase("FantomasFirstFile", "Parse.fs.2022-06-04_004240.json")>]
    [<TestCase("root_7leaves", "root_7leaves_library.fs.2022-06-15_222554.json")>]
    [<TestCase("root_20leaves", "root_20leaves_library.fs.2022-06-15_220456.json")>]
    [<Explicit>]
    let GoAny (name : string, file : string) =
        file
        |> fetchArgs
        |> go
        ()
        
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
    let FantomasHardMulti () =
        [|
            "1_Program.fs.2022-06-11_012033.json"
            "2_DaemonTests.fs.2022-06-11_012110.json"
        |]
        |> Array.map fetchArgs
        |> goMulti