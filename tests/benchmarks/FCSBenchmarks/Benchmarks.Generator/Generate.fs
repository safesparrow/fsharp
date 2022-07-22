module BenchmarkGenerator.Generate

open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open BenchmarkGenerator.Dto
open CommandLine
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo
open Ionide.ProjInfo.Sln.Construction
open Ionide.ProjInfo.Types
open Microsoft.Extensions.Logging
open Newtonsoft.Json

[<AutoOpen>]
module Log =
    let private loggerFactory = LoggerFactory.Create(
        fun builder ->
            builder.AddSimpleConsole(fun options ->
                options.IncludeScopes <- false
                options.SingleLine <- true
                options.TimestampFormat <- "HH:mm:ss.fff"
            )
            |> ignore
    )
    
    type internal Marker = interface end
    
    let internal log = loggerFactory.CreateLogger<Marker>()

/// General utilities
[<RequireQualifiedAccess>]
module Utils =
    let runProcess name args workingDir (envVariables : (string * string) list) (printOutput : bool) =
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
        |> List.iter (fun (k, v) -> info.EnvironmentVariables[k] <- v)
        
        log.LogInformation $"Running '{name} {args}' in '{workingDir}'"
        let p = Process.Start(info)
        let o = p.StandardOutput.ReadToEnd()
        let errors = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then
            let msg = $"Process {name} {args} failed: {errors}."
            log.LogError $"{msg}. Its full output:"
            printfn $"{o}"
            failwith msg
        else if printOutput then
            log.LogInformation ("Full output of the process:" + Environment.NewLine + o)

/// Handling Git operations
[<RequireQualifiedAccess>]
module Git =
    open LibGit2Sharp
    
    let clone (dir : string) (gitUrl : string) : Repository =
        if Directory.Exists dir then
            failwith $"{dir} already exists for code root"
        log.LogInformation $"Fetching '{gitUrl}' in '{dir}'..."
        Repository.Init(dir) |> ignore
        let repo = new Repository(dir)
        let remote = repo.Network.Remotes.Add("origin", gitUrl)
        repo.Network.Fetch(remote.Name, [])
        repo
        
    let checkout (repo : Repository) (revision : string) : unit =
        log.LogInformation $"Checkout revision {revision} in {repo.Info.Path}"
        Commands.Checkout(repo, revision) |> ignore

/// Preparing a codebase based on a 'RepoSpec'
[<RequireQualifiedAccess>]
module RepoSetup =
    open LibGit2Sharp

    [<CLIMutable>]
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
        log.LogInformation $"Checking out {spec}"
        let dir = revisionDir config spec
        if Repository.IsValid dir |> not then
            use repo = Git.clone dir spec.GitUrl
            Git.checkout repo spec.Revision
            repo
        else
            log.LogInformation $"{dir} already exists - will assume the correct repository is already checked out"
            new Repository(dir)

[<RequireQualifiedAccess>]
module Generate =
        
    /// <summary>
    /// An action that calls the following FSharpChecker method:
    /// member bc.ParseAndCheckFileInProject(fileName: string, fileVersion, sourceText: ISourceText, options: FSharpProjectOptions, userOpName) =
    /// </summary>
    type AnalyseFile =
        {
            FileName: string
            FileVersion: int
            SourceText: string
            Options: FSharpProjectOptions
        }
    
    /// An action to be performed during a benchmark
    type BenchmarkAction =
        | AnalyseFile of AnalyseFile
        
    type BenchmarkInputs =
        {
            Actions : BenchmarkAction list
            Config : BenchmarkConfig
        }
    
    [<RequireQualifiedAccess>]
    module Serialization =
        
        open Microsoft.FSharp.Reflection

        let rec private referenceToDto (rp : FSharpReferencedProject) : FSharpReferenceDto =
            // Reflection is needed since DU cases are internal.
            // The alternative is to add an [<InternalsVisibleTo>] entry to the FCS project
            let c, fields = FSharpValue.GetUnionFields(rp, typeof<FSharpReferencedProject>, true)
            match c.Name with
            | "FSharpReference" ->
                let outputFile = fields[0] :?> string
                let options = fields[1] :?> FSharpProjectOptions
                let fakeOptions = optionsToDto options
                {
                    FSharpReferenceDto.OutputFile = outputFile
                    FSharpReferenceDto.Options = fakeOptions
                }
            | _ -> failwith $"Unsupported {nameof(FSharpReferencedProject)} DU case: {c.Name}. only 'FSharpReference' is supported by the serializer"
        
        and private optionsToDto (o : FSharpProjectOptions) : FSharpProjectOptionsDto =
            {
                ProjectFileName = o.ProjectFileName
                ProjectId = o.ProjectId
                SourceFiles = o.SourceFiles
                OtherOptions = o.OtherOptions
                ReferencedProjects =
                    o.ReferencedProjects
                    |> Array.map referenceToDto
                IsIncompleteTypeCheckEnvironment = o.IsIncompleteTypeCheckEnvironment
                UseScriptResolutionRules = o.UseScriptResolutionRules
                LoadTime = o.LoadTime
                Stamp = o.Stamp
            }
                
        type BenchmarkInputsDto =
            {
                Actions : BenchmarkActionDto list
                Config : BenchmarkConfig
            }
            
        let actionToJson (action : BenchmarkAction) =
            match action with
            | BenchmarkAction.AnalyseFile x ->
                {
                    AnalyseFileDto.FileName = x.FileName
                    FileVersion = x.FileVersion
                    SourceText = x.SourceText
                    Options = x.Options |> optionsToDto
                }
                |> BenchmarkActionDto.AnalyseFile
        
        let inputsToJson (inputs : BenchmarkInputs) =
            {
                BenchmarkInputsDto.Actions = inputs.Actions |> List.map actionToJson
                Config = inputs.Config
            }

    type CheckAction =
        {
            FileName : string
            ProjectName : string
        }

    type CodebaseSourceType = Local | Git

    [<CLIMutable>]
    type CodebasePrepStep =
        {
            Command : string
            Args : string
        }
    
    [<CLIMutable>]
    type BenchmarkCase =
        {
            Repo : RepoSetup.RepoSpec
            LocalCodeRoot : string
            CodebasePrep : CodebasePrepStep list
            SlnRelative : string
            CheckActions : CheckAction list
        }
        
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let init (slnPath : string) =
        let exe = FileInfo(@"c:/program files/dotnet/dotnet.exe")//"D:\projekty\fsharp\.dotnet\dotnet.exe")
        Init.init (DirectoryInfo(Path.GetDirectoryName slnPath)) None//(Some exe)
    
    type Config =
        {
            CheckoutBaseDir : string
            RunnerProjectPath : string
        }
    
    type Codebase =
        | Local of string
        | Git of LibGit2Sharp.Repository
        with member this.Path = match this with | Local codeRoot -> codeRoot | Git repo -> repo.Info.WorkingDirectory
    
    let prepareCodebase (config : Config) (case : BenchmarkCase) : Codebase =
        use _ = Log.log.BeginScope("PrepareCodebase")
        let codebase =
            match (case.Repo :> obj, case.LocalCodeRoot) with
            | null, null -> failwith "Either git repo or local code root details are required"
            | repo, null ->
                let repo = RepoSetup.prepare {BaseDir = config.CheckoutBaseDir} case.Repo
                Codebase.Git repo
            | null, codeRoot ->
                Codebase.Local codeRoot
            | repo, codeRoot -> failwith $"Both git repo and local code root were provided - that's not supported"
        let sln = Path.Combine(codebase.Path, case.SlnRelative)
        log.LogInformation($"Running {case.CodebasePrep.Length} codebase prep steps...")
        case.CodebasePrep
        |> List.iteri (fun i step ->
            log.LogInformation($"Running codebase prep step [{i+1}/{case.CodebasePrep.Length}]")
            Utils.runProcess step.Command step.Args codebase.Path [] true
        )
        
        Utils.runProcess "dotnet" $"restore {sln}" codebase.Path [] false
        codebase
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private doLoadOptions (toolsPath : ToolsPath) (sln : string) =
        // TODO allow customization of build properties
        let props =
            [
                // "Configuration", "Debug"
                // "TargetPlatform", "x64"
                // "Platform", "x64"
            ]
        let loader = WorkspaceLoader.Create(toolsPath, props)
        let bl = BinaryLogGeneration.Within(DirectoryInfo("d:/.artifacts/binlogs"))//Path.GetDirectoryName sln))
        
        let slnA = SolutionFile.Parse sln
        let projectsA =
            slnA.ProjectsInOrder
            |> Seq.filter (fun p -> p.ProjectType = SolutionProjectType.KnownToBeMSBuildFormat)
            |> Seq.map (fun p -> p.AbsolutePath)
            |> Seq.toList
        let vs = Microsoft.Build.Locator.MSBuildLocator.RegisterDefaults()        
        let projects = loader.LoadSln(sln, [], bl) |> Seq.toList
        log.LogInformation $"{projects.Length} projects loaded"
        if projects.Length = 0 then
            failwith $"No projects were loaded from {sln} - this indicates an error in cracking the projects"
        
        let fsOptions =
            projects
            |> List.map (fun project -> Path.GetFileNameWithoutExtension(project.ProjectFileName), FCS.mapToFSharpProjectOptions project projects)
        fsOptions
        |> dict
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private loadOptions (sln : string) =
        use _ = log.BeginScope("LoadOptions")
        log.LogInformation($"Constructing FSharpProjectOptions from {sln}")
        let toolsPath = init sln
        doLoadOptions toolsPath sln
    
    let private serializeInputs (inputs : BenchmarkInputs) : string =
        log.LogInformation("Serializing generated inputs")
        let dto = inputs |> Serialization.inputsToJson
        dto |> JsonConvert.SerializeObject
    
    let private generateInputs (config : Config) (case : BenchmarkCase) (codeRoot : string) =
        let sln = Path.Combine(codeRoot, case.SlnRelative)
        let options = loadOptions sln
        
        log.LogInformation("Generating actions")
        let actions =
            case.CheckActions
            |> List.mapi (fun i {FileName = projectRelativeFileName; ProjectName = projectName} ->
                let project = options[projectName]
                let filePath = Path.Combine(Path.GetDirectoryName(project.ProjectFileName), projectRelativeFileName)
                let fileText = File.ReadAllText(filePath)
                BenchmarkAction.AnalyseFile {FileName = filePath; FileVersion = i; SourceText = fileText; Options = project}
            )
        
        let config : BenchmarkConfig =
            {
                BenchmarkConfig.ProjectCacheSize = 200
            }
            
        {
            BenchmarkInputs.Actions = actions
            BenchmarkInputs.Config = config
        }
    
    let private makeInputsPath (codeRoot : string) =
        let artifactsDir = Path.Combine(codeRoot, ".artifacts")
        let dateStr = DateTime.UtcNow.ToString("yyyy-MM-dd_HH-mm-ss")
        Path.Combine(artifactsDir, $"{dateStr}.fcsinputs.json") 
    
    // These are the env variables that Ionide.ProjInfo seems to set (in-process).
    // We need to get rid of them so that the child 'dotnet run' process is using the right tools
    let private projInfoEnvVariables =
        [
            "MSBuildExtensionsPath"
            "DOTNET_ROOT"
            "MSBUILD_EXE_PATH"
            "DOTNET_HOST_PATH"
            "MSBuildSDKsPath"
        ]
    
    let private emptyProjInfoEnvironmentVariables () =
        projInfoEnvVariables
        |> List.map (fun var -> var, "")
    
    let private prepareAndRun (config : Config) (case : BenchmarkCase) (doRun : bool) (cleanup : bool) =
        let codebase = prepareCodebase config case
        let inputs = generateInputs config case codebase.Path
        let serialized = serializeInputs inputs
        let inputsPath = makeInputsPath codebase.Path
        log.LogInformation $"Saving inputs as {inputsPath}"        
        Directory.CreateDirectory(Path.GetDirectoryName(inputsPath)) |> ignore
        File.WriteAllText(inputsPath, serialized)
        
        if doRun then
            use _ = log.BeginScope $"Run"
            log.LogInformation $"Starting the benchmark..."
            let workingDir = Path.GetDirectoryName(config.RunnerProjectPath)
            let envVariables =
                emptyProjInfoEnvironmentVariables()
                @ [
                    "Platform", "x64"
                ]
            log.LogInformation($"Running {config} codebase preparation steps:")
            log.LogInformation("Building runner project...")
            Utils.runProcess "dotnet" $"build -c Release Benchmarks.Runner.fsproj" workingDir envVariables true
            log.LogInformation("**** Runner project built. Running benchmark...")
            let projFullPath = Path.Combine(workingDir, "Benchmarks.Runner.fsproj")
            Utils.runProcess "dotnet" $"run -c Release --no-restore --project {projFullPath} -- {inputsPath}" workingDir envVariables true
        else
            log.LogInformation $"Not running the benchmark as requested"
            
        match codebase, cleanup with
        | Local root, _ -> ()
        | Git repo, false -> ()
        | Git repo, true ->
            log.LogInformation $"Cleaning up checked out git repo {repo.Info.Path} as requested"
            Directory.Delete repo.Info.Path
    
    type Args =
        {
            [<CommandLine.Option('c', Default = "d:/.artifacts", HelpText = "Base directory for git checkouts")>]
            CheckoutsDir : string
            [<CommandLine.Option('b', Default = "../../../../Benchmarks.Runner/Benchmarks.Runner.fsproj", HelpText = "Path to the benchmark runner project - defaults to '../Benchmarks.Runner/Benchmarks.Runner.fsproj'")>]
            BenchmarkPath : string
            [<CommandLine.Option('i', Required = true, HelpText = "Path to the input file describing the benchmark")>]
            Input : string
            [<CommandLine.Option(Default = true, HelpText = "If set to false, prepares the benchmark and prints the commandline to run it, then exits")>]
            Run : bool
            [<CommandLine.Option(Default = false, HelpText = "If set, removes the checkout directory afterwards. Doesn't apply to local codebases")>]
            Cleanup : bool
        }
    
    [<EntryPoint>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let main args =
        let parseResult = Parser.Default.ParseArguments<Args> args
        match parseResult.Tag with
        | ParserResultType.Parsed ->
            let args = parseResult.Value
            let config =
                {
                    Config.CheckoutBaseDir = args.CheckoutsDir
                    Config.RunnerProjectPath = Path.Combine(Environment.CurrentDirectory, args.BenchmarkPath)
                }
            let case =
                use _ = Log.log.BeginScope("Read input")
                try
                    let path = args.Input
                    path
                    |> File.ReadAllText
                    |> JsonConvert.DeserializeObject<BenchmarkCase>
                with e ->
                    let msg = $"Failed to read inputs file: {e.Message}"
                    log.LogCritical(msg)
                    reraise()
            
            use _ = Log.log.BeginScope("PrepareAndRun")
            prepareAndRun config case args.Run args.Cleanup
            0
        | _ ->
            1