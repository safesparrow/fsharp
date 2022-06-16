namespace FCSTest

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Microsoft.FSharp.Reflection
open Newtonsoft.Json

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
    
    /// <summary>Directories used when generating the inputs, used to anonymize them</summary>
    type TestDirs = {
        NugetPackages : string
        CodeRoot : string
    }
    
    module TestDirs =
        let toPlaceholders (d : TestDirs) =
            [|
                {From = d.CodeRoot; To = "%CODE_ROOT%"}
                {From = d.NugetPackages; To = "%NUGET_PACKAGES%"}
            |]
        let fromPlaceholders (d : TestDirs) =
            toPlaceholders d
            |> Array.map (fun x -> x.Reverse())
    
    let swapSubstrings (s : string) (pair : PrefixSwapPair) =
        s.Replace(pair.From, pair.To)
        
    let swapPrefixes (pairs : PrefixSwapPair[]) (s : string) =
        Array.fold swapSubstrings s pairs

    let rec replacePaths (pairs : PrefixSwapPair[]) (o : FakeOptions) =
        let replace (s : string) = swapPrefixes pairs s
        {
            o with
                ProjectFileName = o.ProjectFileName |> replace
                SourceFiles = o.SourceFiles |> Array.map replace
                OtherOptions = o.OtherOptions |> Array.map replace
                ReferencedProjects =
                    o.ReferencedProjects
                    |> Array.map (fun rp ->
                        {
                            OutputFile = rp.OutputFile |> replace
                            Options = rp.Options |> replacePaths pairs
                        }
                    )
        }
        
    let replacePlaceholderPaths (d : TestDirs) (a : FakeParseArgs) : FakeParseArgs =
        let pairs = TestDirs.fromPlaceholders d
        {
            a with
                options = a.options |> replacePaths pairs
                fileName = a.fileName |> swapPrefixes pairs
        }
        
    let rec convertBackOptions (o : FakeOptions) : FSharpProjectOptions =       
        let fakeRP (rp : RP) : FSharpReferencedProject =
            let back = convertBackOptions rp.Options
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
        
    let convertBackAndMap (d : TestDirs) (a : FakeParseArgs) : ParseArgs =
        let a = replacePlaceholderPaths d a
        {
            ParseArgs.options = convertBackOptions a.options
            ParseArgs.fileName = a.fileName
            ParseArgs.fileVersion = a.fileVersion
            ParseArgs.sourceText = SourceText.ofString a.sourceText
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
        
    let convertAndMap (d : TestDirs) (o : FSharpProjectOptions) : FakeOptions =
        let o = convert o
        let pairs = TestDirs.toPlaceholders d
        replacePaths pairs o
    
    let replaceArgs (p : PrefixSwapPair[]) (a : FakeParseArgs) =
        {
            a with
                fileName = swapPrefixes p a.fileName
                options = replacePaths p a.options
        }
        
    let deserialize (d : TestDirs) (json : string) : ParseArgs =
        let fake = JsonConvert.DeserializeObject<FakeParseArgs> json
        convertBackAndMap d fake
    
    let serialize (d : TestDirs) (a : ParseArgs) : string =
        let options = convertAndMap d a.options
        let fake : FakeParseArgs = {
            FakeParseArgs.fileName = a.fileName
            fileVersion = a.fileVersion
            options = options
            sourceText = a.sourceText.GetSubTextString(0, a.sourceText.Length)
        }
        JsonConvert.SerializeObject(fake, Formatting.Indented)

    
    let anonymizeArgsFile (dirs : TestDirs) (path : string) =
        let json = File.ReadAllText(path)
        let fake = JsonConvert.DeserializeObject<FakeParseArgs> json
        let replaced = replaceArgs (dirs |> TestDirs.toPlaceholders) fake
        let json = JsonConvert.SerializeObject(replaced, Formatting.Indented)
        File.WriteAllText(path, json)
        