namespace FSharpOptions


open System
open System.Diagnostics
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

[<RequireQualifiedAccess>]
module Utils =
    let runProcess name args workingDir (envVariables : (string * string) list) =
        let info = ProcessStartInfo()
        info.WindowStyle <- ProcessWindowStyle.Hidden
        info.Arguments <- args
        info.FileName <- name
        info.UseShellExecute <- false
        info.WorkingDirectory <- workingDir
        info.RedirectStandardError <- true
        info.RedirectStandardOutput <- true
        info.RedirectStandardInput <- true
        info.CreateNoWindow <- true
        envVariables
        |> List.iter (info.EnvironmentVariables.Add)
        printfn $"Running '{name} {args}' in '{workingDir}'"
        let p = Process.Start(info)
        let o = p.StandardOutput.ReadToEnd()
        let errors = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then
            let msg = $"Process {name} {args} failed: {errors}."
            printfn $"{msg}. Its full output: {o}"
            failwith msg

[<RequireQualifiedAccess>]
module Git =
    open LibGit2Sharp
    
    let clone (dir : string) (gitUrl : string) : Repository =
        if Directory.Exists dir then
            failwith $"{dir} already exists for code root"
        printfn $"Fetching '{gitUrl}' in '{dir}'..."
        Repository.Init(dir) |> ignore
        let repo = new Repository(dir)
        let remote = repo.Network.Remotes.Add("origin", gitUrl)
        repo.Network.Fetch(remote.Name, [])
        repo
        
    let checkout (repo : Repository) (revision : string) : unit =
        printfn $"Checkout {revision} in {repo.Info.Path}"
        Commands.Checkout(repo, revision) |> ignore

module RepoSetup =
    open LibGit2Sharp

    type RepoSpec =
        {
            Name : string
            GitUrl : string
            Revision : string
        }
            with override this.ToString() = $"{this.Name} - {this.GitUrl} at revision {this.Revision}"
        
    type Config =
        {
            BaseDir : string
        }
    
    let revisionDir (config : Config) (spec : RepoSpec) =
        Path.Combine(config.BaseDir, spec.Name, spec.Revision)
    
    let prepare (config : Config) (spec : RepoSpec) =
        printfn $"Checking out {spec}"
        let dir = revisionDir config spec
        if Repository.IsValid dir |> not then
            use repo = Git.clone dir spec.GitUrl
            Git.checkout repo spec.Revision
        else
            printfn $"{dir} already exists - will assume the correct repository is already checked out"
        dir
    
module Cracker =

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let init (slnPath : string) =
        Init.init (DirectoryInfo(Path.GetDirectoryName slnPath)) None
    
    type BenchmarkActionPhase1 =
        | AnalyseFile of fileName : string * projectName : string
    
    type TestCodebase =
        | Git of repo : RepoSetup.RepoSpec
        | Local of codeRoot : string
    
    /// Provided as JSON
    type FullCase =
        {
            Codebase : TestCodebase
            Sln : string
            Actions : BenchmarkActionPhase1 list
        }
    
    type ConfigPhase1 =
        {
            CheckoutBaseDir : string
        }
    
    let prepareCodebase (config : ConfigPhase1) (case : FullCase) =
        let codeRoot =
            match case.Codebase with
            | TestCodebase.Git repo ->
                RepoSetup.prepare {BaseDir = config.CheckoutBaseDir} repo
            | TestCodebase.Local codeRoot ->
                codeRoot
        
        Utils.runProcess "dotnet" $"restore {case.Sln}" codeRoot []
        codeRoot
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let doLoadOptions (toolsPath : ToolsPath) (sln : string) =
        let loader = WorkspaceLoader.Create(toolsPath, [])
        let bl = BinaryLogGeneration.Within(DirectoryInfo(Path.GetDirectoryName sln))
        let sln = Ionide.ProjInfo.Sln.Construction.SolutionFile.Parse sln
        let projectPaths =
            sln.ProjectsInOrder
            |> Seq.map (fun p -> p.AbsolutePath)
            |> Seq.toList
        let projects = loader.LoadProjects(projectPaths, [], bl) |> Seq.toList
        printfn $"{projects.Length} projects loaded"
        
        projects
        |> List.map (fun project -> Path.GetFileNameWithoutExtension(project.ProjectFileName), FCS.mapToFSharpProjectOptions project projects)
        |> dict
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let loadOptions (sln : string) =
        let toolsPath = init sln
        doLoadOptions toolsPath sln
    
    let private serialize (inputs : Benchmarking.BenchmarkInputs) : byte[] =
        // let registry = CustomPicklerRegistry()
        // registry.DeclareSerializable<NonSerializable>()
        // let cache = PicklerCache.FromCustomPicklerRegistry registry
        let serializer = FsPickler.CreateBinarySerializer()//picklerResolver = cache)
        serializer.DisableSubtypeResolution <- true
        let data = serializer.Pickle inputs
        let r = serializer.UnPickle<Benchmarking.BenchmarkInputs> data
        data
    
    let generateInputs (config : ConfigPhase1) (case : FullCase) (codeRoot : string) =
        let options = loadOptions case.Sln
        
        let actions =
            case.Actions
            |> List.map (fun (AnalyseFile(projectRelativeFileName, projectName)) ->
                let project = options[projectName]
                let filePath = Path.Combine(project.ProjectFileName, projectRelativeFileName)
                let fileText = File.ReadAllText(filePath)
                Benchmarking.BenchmarkAction.AnalyseFile(filePath, fileText, project)
            )
        
        let config : Benchmarking.BenchmarkConfig =
            {
                Benchmarking.BenchmarkConfig.ProjectCacheSize = 200
            }
            
        {
            Benchmarking.BenchmarkInputs.Actions = actions
            Benchmarking.BenchmarkInputs.Config = config
        }
    
    let makeInputsPath (config : ConfigPhase1) (codeRoot : string) =
        let artifactsDir = Path.Combine(codeRoot, ".artifacts")
        let dateStr = DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss")
        let guid = Guid.NewGuid()
        Path.Combine(artifactsDir, $"{dateStr}.{guid}.DO_NOT_SHARE.fcsinputs.bin") 
    
    let runBenchmark (config : ConfigPhase1) (case : FullCase) =
        let codeRoot = prepareCodebase config case
        let inputs = generateInputs config case codeRoot
        let serialized = serialize inputs
        let inputsPath = makeInputsPath config codeRoot
        File.WriteAllBytes(inputsPath, serialized)
        printfn $"Binary inputs saved to {inputsPath}"
        
        
        ()
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let rest (msbuild : ToolsPath) (slnPath : string) =
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
        
        let slnPath = @"D:\projekty\parallel_test\top.sln"
    
        
        let msbuild = init slnPath
        rest msbuild slnPath
        0