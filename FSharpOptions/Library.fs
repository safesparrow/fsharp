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
            SlnRelative : string
            Actions : BenchmarkActionPhase1 list
        }
    
    type Config =
        {
            CheckoutBaseDir : string
        }
    
    let prepareCodebase (config : Config) (case : FullCase) =
        let codeRoot =
            match case.Codebase with
            | TestCodebase.Git repo ->
                RepoSetup.prepare {BaseDir = config.CheckoutBaseDir} repo
            | TestCodebase.Local codeRoot ->
                codeRoot
        
        let sln = Path.Combine(codeRoot, case.SlnRelative)
        Utils.runProcess "dotnet" $"restore {sln}" codeRoot []
        codeRoot
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let doLoadOptions (toolsPath : ToolsPath) (sln : string) =
        let props =
            [
                "Configuration", "Debug"
                "TargetPlatform", "x64"
                "Platform", "x64"
            ]
        let loader = WorkspaceLoader.Create(toolsPath, props)
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
        let serializer = FsPickler.CreateBinarySerializer()
        serializer.DisableSubtypeResolution <- true
        let data = serializer.Pickle inputs
        // test deserialization works
        let r = serializer.UnPickle<Benchmarking.BenchmarkInputs> data
        data
        
    
    let private serializeXml (inputs : Benchmarking.BenchmarkInputs) =
        let serializer = FsPickler.CreateXmlSerializer()
        serializer.DisableSubtypeResolution <- true
        let data = serializer.Pickle inputs
        // test deserialization works
        let r = serializer.UnPickle<Benchmarking.BenchmarkInputs> data
        data
    
    let generateInputs (config : Config) (case : FullCase) (codeRoot : string) =
        let sln = Path.Combine(codeRoot, case.SlnRelative)
        let options = loadOptions sln
        
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
    
    let makeInputsPath (config : Config) (codeRoot : string) =
        let artifactsDir = Path.Combine(codeRoot, ".artifacts")
        let dateStr = DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss")
        let guid = Guid.NewGuid()
        Path.Combine(artifactsDir, $"{dateStr}.{guid}.DO_NOT_SHARE.fcsinputs.bin") 
    
    let runBenchmark (config : Config) (case : FullCase) =
        let codeRoot = prepareCodebase config case
        let inputs = generateInputs config case codeRoot
        let serialized = serializeXml inputs
        let inputsPath = makeInputsPath config codeRoot
        Directory.CreateDirectory(Path.GetDirectoryName(inputsPath)) |> ignore
        File.WriteAllBytes(inputsPath, serialized)
        printfn $"Binary inputs saved to {inputsPath}"
        
        let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "../tests/benchmarks/FCSBenchmarks/CheckerGenericBenchmark")
        let envVariables = []
        Utils.runProcess "dotnet" $"restore -c Release -v:d CheckerGenericBenchmark.fsproj" workingDir envVariables
        Utils.runProcess "dotnet" $"build -c Release -v:d CheckerGenericBenchmark.fsproj" workingDir envVariables
        Utils.runProcess "dotnet" $"run -c Release --project CheckerGenericBenchmark.fsproj {inputsPath}" workingDir envVariables
        ()        
    
    [<EntryPoint>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let main args =
        let config =
            {
                Config.CheckoutBaseDir = "d:/projekty/CheckerBenchmarks"
            }
        let case =
            {
                Codebase = TestCodebase.Local @"D:\projekty\parallel_test"
                SlnRelative = "top.sln"
                Actions = []
            }
        runBenchmark config case
        0