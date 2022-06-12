namespace FCSTest

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Serialization
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Microsoft.FSharp.Reflection
open NUnit.Framework
open Newtonsoft.Json
open Newtonsoft.Json.Linq

[<AutoOpen>]
module Serializing =
    
    [<CLIMutable>]
    type RP = {
        OutputFile: string
        Options : FakeOptions
    }
    and [<CLIMutable>] FakeOptions = {
        ProjectFileName : string
        ProjectId : string option
        SourceFiles : string[]
        OtherOptions: string[]
        ReferencedProjects: RP[]
        IsIncompleteTypeCheckEnvironment : bool
        UseScriptResolutionRules : bool
        LoadTime : DateTime
        UnresolvedReferences: FSharpUnresolvedReferencesSet option
        OriginalLoadReferences: (range * string * string) list
        Stamp: int64 option
    }
    
    [<CLIMutable>]
    type FakeParseArgs = {
            fileName: string
            fileVersion: int
            sourceText: string
            options: FakeOptions
    }
    
    type ParseArgs = {
            fileName: string
            fileVersion: int
            sourceText: ISourceText
            options: FSharpProjectOptions
    }
    
    type PrefixSwapPair = {
        From : string
        To : string
    }
        with member x.Reverse () =
                {
                    From = x.To
                    To = x.From
                }
    
    type Dirs = {
        NugetPackages : string
        CodeRoot : string
    }
    
    module Dirs =
        let toPlaceholders (d : Dirs) =
            [|
                {From = d.CodeRoot; To = "%CODE_ROOT%"}
                {From = d.NugetPackages; To = "%NUGET_PACKAGES%"}
            |]
        let fromPlaceholders (d : Dirs) =
            toPlaceholders d
            |> Array.map (fun x -> x.Reverse())
    
    let swapPrefix (s : string) (pair : PrefixSwapPair) =
        if s.StartsWith pair.From then
            pair.To + (s.Substring pair.From.Length)
        else s
        
    let swapPrefixes (pairs : PrefixSwapPair[]) (s : string) =
        Array.fold swapPrefix s pairs

    let rec replacePaths (pairs : PrefixSwapPair[]) (o : FakeOptions) =
        let replace (s : string) = swapPrefixes pairs s
        {
            o with
                ProjectFileName = o.ProjectFileName |> replace
                SourceFiles = o.SourceFiles |> Array.map replace
                ReferencedProjects =
                    o.ReferencedProjects
                    |> Array.map (fun rp ->
                        {
                            OutputFile = rp.OutputFile |> replace
                            Options = rp.Options |> replacePaths pairs
                        }
                    )
        }
        
    let rec replacePlaceholderPaths (d : Dirs) (o : FakeOptions) =
        let pairs = Dirs.fromPlaceholders d
        o |> replacePaths pairs
        
    let rec convertBack (o : FakeOptions) : FSharpProjectOptions =       
        let fakeRP (rp : RP) : FSharpReferencedProject =
            let back = convertBack rp.Options
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
            UnresolvedReferences = o.UnresolvedReferences
            OriginalLoadReferences = o.OriginalLoadReferences
            Stamp = o.Stamp
        }
        
    let convertBackAndMap (d : Dirs) (o : FakeOptions) : FSharpProjectOptions =
        let o = replacePlaceholderPaths d o
        convertBack o

    
    let rec convert (o : FSharpProjectOptions) : FakeOptions =
        let fakeRP (rp : FSharpReferencedProject) : RP =
            let c, fields = FSharpValue.GetUnionFields(rp, typeof<FSharpReferencedProject>, true)
            match c.Name with
            | "FSharpReference" ->
                let outputFile = fields.[0] :?> string
                let options = fields.[1] :?> FSharpProjectOptions
                let fakeOptions = convert options
                {
                    RP.OutputFile = outputFile
                    RP.Options = fakeOptions
                }
            | _ -> failwith $"Unknown {c.Name}"
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
            UnresolvedReferences = o.UnresolvedReferences
            OriginalLoadReferences = o.OriginalLoadReferences
            Stamp = o.Stamp
        }
        
    let convertAndMap (d : Dirs) (o : FSharpProjectOptions) : FakeOptions =
        let o = convert o
        let pairs = Dirs.toPlaceholders d
        replacePaths pairs o
    
    let replaceArgs (p : PrefixSwapPair[]) (a : FakeParseArgs) =
        {
            a with
                fileName = swapPrefixes p a.fileName
                options = replacePaths p a.options
        }
        
    let deserialize (d : Dirs) (json : string) : ParseArgs =
        let fake = Newtonsoft.Json.JsonConvert.DeserializeObject<FakeParseArgs> json
        let options : FSharpProjectOptions = convertBackAndMap d fake.options
        {
            fileName = fake.fileName
            fileVersion = fake.fileVersion
            sourceText = SourceText.ofString (fake.sourceText)
            options = options
        }
    
    let serialize (d : Dirs) (a : ParseArgs) : string =
        let options = convertAndMap d a.options
        let fake : FakeParseArgs = {
            FakeParseArgs.fileName = a.fileName
            fileVersion = a.fileVersion
            options = options
            sourceText = a.sourceText.GetSubTextString(0, a.sourceText.Length)
        }
        Newtonsoft.Json.JsonConvert.SerializeObject(fake, Newtonsoft.Json.Formatting.Indented)

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
        NugetPackages = "-r:C:\\Users\\janus\\.nuget\\packages"
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
        // let a = Audit.audit
        ()
        
    [<Test>]
    [<Explicit>]
    let FantomasEasy () =
        go "Parse.fs.2022-06-04_004240.json"
        // let a = Audit.audit
        ()

    
    [<Test>]
    [<Explicit>]
    let FantomasHard () =
        go "DaemonTests.fs.2022-06-04_004541.json"
        // let a = Audit.audit
        ()
    
    [<Test>]
    [<Explicit>]
    let FantomasHardMulti () =
        goMulti [|
            "1_Program.fs.2022-06-11_012033.json"
            "2_DaemonTests.fs.2022-06-11_012110.json"
        |]
        // let a = Audit.audit
        ()
        
            
    [<Test>]
    [<Explicit>]
    let Foo () =
        let b = {
            FSharpProjectOptions.ProjectFileName = "b"
            ProjectId = None
            SourceFiles = [||]
            OtherOptions = [||]
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }
        let a = {
            FSharpProjectOptions.ProjectFileName = "a"
            ProjectId = None
            SourceFiles = [||]
            OtherOptions = [||]
            ReferencedProjects = [|FSharpReferencedProject.CreateFSharp("b.dll", b)|]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }
        let args = { fileName = "file"
                     fileVersion = 3
                     sourceText = SourceText.ofString "source"
                     ParseArgs.options = a
        }
        let json = serialize dirs args
        File.WriteAllText("d:/projekty/fsharp/fcstest/serialized.json", json)
        let roundtrip = deserialize dirs json
        ()
        
    [<Test>]
    [<Explicit>]
    let Test1 () =
        let sw = Stopwatch.StartNew()
        
        let name = "Library.fs.2022-06-03_214345.json"
        let argsJson = File.ReadAllText("d:/projekty/fsharp/FCSTest/dumps/" + name)
        let args = deserialize dirs argsJson
        let x = parseAndCheckFileInProject args
        let y = x |> Async.RunSynchronously
        System.Console.WriteLine (sw.Elapsed)
        ()