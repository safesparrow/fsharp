namespace FSharpOptions

open System.IO
open System.Runtime.CompilerServices
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Ionide.ProjInfo
open Ionide.ProjInfo.Types
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
    
    type FCSBenchmark (config : BenchmarkConfig) =
        let checker = FSharpChecker.Create(projectCacheSize = config.ProjectCacheSize)
            
        let failOnErrors (results : FSharpCheckFileResults) =
            if results.Diagnostics.Length > 0 then failwithf $"had errors: %A{results.Diagnostics}"
        
        let performAction (action : BenchmarkAction) =
            match action with
            | AnalyseFile (filePath, sourceText, options) ->
                let result, answer =
                    checker.ParseAndCheckFileInProject(filePath, 0, SourceText.ofString sourceText, options)
                    |> Async.RunSynchronously
                match answer with
                | FSharpCheckFileAnswer.Aborted -> failwith "checker aborted"
                | FSharpCheckFileAnswer.Succeeded results ->
                    failOnErrors results
                
        let cleanCaches () =
            checker.InvalidateAll()
            checker.ClearLanguageServiceRootCachesAndCollectAndFinalizeAllTransients()
            
        member this.Checker = checker
        member this.PerformAction action = performAction action
        member this.CleanCaches () = cleanCaches
    
module Cracker =

    let slnPath = @"D:\projekty\parallel_test\top.sln"
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let init () =
        Init.init (DirectoryInfo(Path.GetDirectoryName slnPath)) None
    
    type BenchmarkActionPhase1 =
        | AnalyseFile of fileName : string * projectName : string
    
    type ConfigPhase1 =
        {
            CodeRoot : string
        }
    
    type BenchmarkSpec =
        {
            Actions : BenchmarkActionPhase1 list
            Config : ConfigPhase1
        }
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let rest (msbuild : ToolsPath) =
        let loader = WorkspaceLoader.Create(msbuild, [])
        let bl = BinaryLogGeneration.Within(DirectoryInfo(Path.GetDirectoryName slnPath))
        let sln = Ionide.ProjInfo.Sln.Construction.SolutionFile.Parse slnPath
        let projectPaths =
            sln.ProjectsInOrder
            |> Seq.map (fun p -> p.AbsolutePath)
            |> Seq.toList
            |> List.filter (fun p -> p.Contains "op.fsproj" = false)
            |> List.take 1
        let projects = loader.LoadProjects(projectPaths, [], bl) |> Seq.toList
        printfn "Projects loaded"
        
        // let project = projects |> Array.find (fun p -> p.ProjectFileName.Contains("leaf_0.fsproj"))
        let optionsDict =
            projects
            |> List.map (fun project -> project.ProjectFileName, FCS.mapToFSharpProjectOptions project projects)
            |> dict
        
        let serialize (options : FSharpProjectOptions[]) =
            // let registry = CustomPicklerRegistry()
            // registry.DeclareSerializable<NonSerializable>()
            // let cache = PicklerCache.FromCustomPicklerRegistry registry
            let serializer = FsPickler.CreateBinarySerializer()//picklerResolver = cache)
            serializer.DisableSubtypeResolution <- true
            let data = serializer.Pickle options
            let r = serializer.UnPickle<FSharpProjectOptions> data
            ()
            
        serialize()
        
    
    [<EntryPoint>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let main args =
        let msbuild = init ()
        rest msbuild
        0