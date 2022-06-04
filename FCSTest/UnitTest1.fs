namespace FCSTest

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Serialization
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Microsoft.FSharp.Reflection
open NUnit.Framework
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
        
    let deserialize (json : string) : ParseArgs =
        let fake = Newtonsoft.Json.JsonConvert.DeserializeObject<FakeParseArgs> json
        let options : FSharpProjectOptions = convertBack fake.options
        {
            fileName = fake.fileName
            fileVersion = fake.fileVersion
            sourceText = SourceText.ofString (fake.sourceText)
            options = options
        }
    
    let serialize (a : ParseArgs) : string =
        let options = convert a.options
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
        
    let go (name : string) =
        let sw = Stopwatch.StartNew()
        let argsJson = File.ReadAllText("d:/projekty/fsharp/FCSTest/dumps/" + name)
        let args = deserialize argsJson
        let args = {args with sourceText = modifyText args.sourceText}
        let x = parseAndCheckFileInProject args
        let y = x |> Async.RunSynchronously
        System.Console.WriteLine (sw.Elapsed)
        ()
        
    [<Test>]
    [<Repeat(10)>]
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
        let json = serialize args
        File.WriteAllText("d:/projekty/fsharp/fcstest/serialized.json", json)
        let roundtrip = deserialize json
        ()
        
    [<Test>]
    [<Explicit>]
    let Test1 () =
        let sw = Stopwatch.StartNew()
        
        let name = "Library.fs.2022-06-03_214345.json"
        let argsJson = File.ReadAllText("d:/projekty/fsharp/FCSTest/dumps/" + name)
        let args = deserialize argsJson
        let x = parseAndCheckFileInProject args
        let y = x |> Async.RunSynchronously
        System.Console.WriteLine (sw.Elapsed)
        ()