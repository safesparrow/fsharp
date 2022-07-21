namespace BenchmarkGenerator

open System
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open BenchmarkGenerator.Dto
open CommandLine
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo
open Ionide.ProjInfo.Types
open Newtonsoft.Json

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
        
        printfn $"Running '{name} {args}' in '{workingDir}'"
        let p = Process.Start(info)
        let o = p.StandardOutput.ReadToEnd()
        let errors = p.StandardError.ReadToEnd()
        p.WaitForExit()
        if p.ExitCode <> 0 then
            let msg = $"Process {name} {args} failed: {errors}."
            printfn $"{msg}. Its full output: {o}"
            failwith msg
        else if printOutput then
            printfn "Full output:"
            printfn $"{o}"


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
        printfn $"Checkout revision {revision} in {repo.Info.Path}"
        Commands.Checkout(repo, revision) |> ignore

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
        printfn $"Checking out {spec}"
        let dir = revisionDir config spec
        if Repository.IsValid dir |> not then
            use repo = Git.clone dir spec.GitUrl
            Git.checkout repo spec.Revision
        else
            printfn $"{dir} already exists - will assume the correct repository is already checked out"
        dir

module Generate =
        
    // member bc.ParseAndCheckFileInProject(fileName: string, fileVersion, sourceText: ISourceText, options: FSharpProjectOptions, userOpName) =
    type AnalyseFile =
        {
            FileName: string
            FileVersion: int
            SourceText: string
            Options: FSharpProjectOptions
        }
    
    type BenchmarkAction =
        | AnalyseFile of AnalyseFile
        
    type BenchmarkInputs =
        {
            Actions : BenchmarkAction list
            Config : BenchmarkConfig
        }
        
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
    type BenchmarkCase =
        {
            Repo : RepoSetup.RepoSpec
            LocalCodeRoot : string
            SlnRelative : string
            CheckActions : CheckAction list
        }
        
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let init (slnPath : string) =
        let exe = FileInfo(@"D:\projekty\fsharp\.dotnet\dotnet.exe")
        Init.init (DirectoryInfo(Path.GetDirectoryName slnPath)) (Some exe)
    
    type Config =
        {
            CheckoutBaseDir : string
            RunnerProjectPath : string
        }
    
    let prepareCodebase (config : Config) (case : BenchmarkCase) =
        let codeRoot =
            match (case.Repo :> obj, case.LocalCodeRoot) with
            | null, null -> failwith "Either git repo or local code root details are required"
            | repo, null ->
                RepoSetup.prepare {BaseDir = config.CheckoutBaseDir} case.Repo
            | null, codeRoot -> codeRoot
            | repo, codeRoot -> failwith $"Both git repo and local code root were provided - that's not supported"
        
        let sln = Path.Combine(codeRoot, case.SlnRelative)
        Utils.runProcess "dotnet" $"restore {sln}" codeRoot [] false
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
        let bl = BinaryLogGeneration.Within(DirectoryInfo("d:/projekty/binlogs"))//Path.GetDirectoryName sln))
        
        let projects = loader.LoadSln(sln, [], bl) |> Seq.toList
        printfn $"{projects.Length} projects loaded"
        
        let fsOptions =
            projects
            |> List.map (fun project -> Path.GetFileNameWithoutExtension(project.ProjectFileName), FCS.mapToFSharpProjectOptions project projects)
        fsOptions
        |> dict
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let loadOptions (sln : string) =
        let toolsPath = init sln
        doLoadOptions toolsPath sln
    
    let private serializeInputs (inputs : BenchmarkInputs) : string =
        let dto = inputs |> Serialization.inputsToJson
        dto |> JsonConvert.SerializeObject
    
    let generateInputs (config : Config) (case : BenchmarkCase) (codeRoot : string) =
        let sln = Path.Combine(codeRoot, case.SlnRelative)
        let options = loadOptions sln
        
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
    
    let makeInputsPath (codeRoot : string) =
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
    
    let emptyProjInfoEnvironmentVariables () =
        projInfoEnvVariables
        |> List.map (fun var -> var, "")
    
    let prepareAndRun (config : Config) (case : BenchmarkCase) (doRun : bool) =
        let codeRoot = prepareCodebase config case
        let inputs = generateInputs config case codeRoot
        let serialized = serializeInputs inputs
        let inputsPath = makeInputsPath codeRoot
        Directory.CreateDirectory(Path.GetDirectoryName(inputsPath)) |> ignore
        File.WriteAllText(inputsPath, serialized)
        printfn $"Inputs saved in {inputsPath}"
        
        if doRun then
            printfn $"Starting the benchmark..."
            let workingDir = Path.GetDirectoryName(config.RunnerProjectPath)
            let envVariables = emptyProjInfoEnvironmentVariables()
            Utils.runProcess "dotnet" $"run -c Release --project BenchmarkRunner.fsproj {inputsPath}" workingDir envVariables true
            // TODO optional cleanup of the coderoot
        else
            printfn $"Not running the benchmark as requested"
    
    type Args =
        {
            [<CommandLine.Option(Default = ".artifacts", HelpText = "Base directory for git checkouts")>]
            CheckoutBaseDir : string
            [<CommandLine.Option(Default = __SOURCE_DIRECTORY__ + "../Benchmarks.Runner/Benchmarks.Runner.fsproj", HelpText = "Path to the benchmark runner project")>]
            BenchmarkProjectPath : string
            [<CommandLine.Option(Required = true, HelpText = "Path to the input file describing the benchmark")>]
            Input : string
            [<CommandLine.Option(Default = true, HelpText = "If set to false, exits before running the benchmark")>]
            Run : bool
            [<CommandLine.Option(Default = false, HelpText = "If set, removes the checkout directory afterwards")>]
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
                    Config.CheckoutBaseDir = args.CheckoutBaseDir
                    Config.RunnerProjectPath = args.BenchmarkProjectPath
                }
            let case =
                let path = args.Input
                path
                |> File.ReadAllText
                |> JsonConvert.DeserializeObject<BenchmarkCase>
            
            prepareAndRun config case args.Run
            0
        | _ ->
            1