module FSharp.Compiler.Service.Tests.ParallelProjectAnalysisTests

open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Tracing
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open NUnit.Framework

let private (</>) a b = Path.Combine(a, b)
let private sdk = "7.0.0-preview.5.22301.12"
let private targetFramework = "net7.0"

let private refPath =
    __SOURCE_DIRECTORY__
    </> ".."
    </> ".."
    </> ".dotnet"
    </> "packs"
    </> "Microsoft.NETCore.App.Ref"
    </> sdk
    </> "ref"
    </> targetFramework

let private fsharpCore =
    __SOURCE_DIRECTORY__
    </> ".."
    </> ".."
    </> "artifacts"
    </> "bin"
    </> "FSharp.Core"
    </> "Debug"
    </> "netstandard2.0"
    </> "FSharp.Core.dll"

type FSharpProjectOptions with

    member this.Output =
        this.OtherOptions
        |> Array.find (fun otherOption -> otherOption.StartsWith("-o:"))
        |> fun otherOption -> otherOption.Replace("-o:", "")

let private solutionDir =
    __SOURCE_DIRECTORY__ </> "data" </> "ParallelProjectAnalysis"

let private mkProjectOptions (projectName: string) (referencedProjects: FSharpProjectOptions array) : FSharpProjectOptions =

    let projectFolder = solutionDir </> projectName
    let projectFileName = projectFolder </> $"{projectName}.fsproj"

    let output =
        __SOURCE_DIRECTORY__
        </> ".."
        </> ".."
        </> "artifacts"
        </> "bin"
        </> projectName
        </> "Debug"
        </> targetFramework
        </> $"{projectName}.dll"

    assert File.Exists(output)

    let projectReferences =
        let mapOutput (rp: FSharpProjectOptions) = $"-r:{rp.Output}"

        referencedProjects
        |> Array.collect (fun rp ->
            [|
                yield mapOutput rp
                yield!
                    (Array.choose
                        (function
                        | FSharpReferencedProject.FSharpReference (_, rp) -> Some(mapOutput rp)
                        | _ -> None)
                        rp.ReferencedProjects)
            |])
        |> Array.distinct

    {
        ProjectFileName = projectFileName
        ProjectId = Some(Guid.NewGuid().ToString())
        SourceFiles = [| projectFolder </> "Library.fs" |]
        OtherOptions =
            [|
                $"-o:{output}"
                "-g"
                "--debug:portable"
                "--noframework"
                "--define:TRACE"
                "--define:DEBUG"
                "--define:NETSTANDARD"
                "--define:FX_NO_APP_DOMAINS"
                "--define:FX_NO_CORHOST_SIGNER"
                "--define:FX_NO_SYSTEM_CONFIGURATION"
                "--define:FX_NO_WIN_REGISTRY"
                "--define:FX_NO_WINFORMS"
                "--define:FX_RESHAPED_REFEMIT"
                "--define:NET"
                "--define:NET7_0"
                "--define:NETCOREAPP"
                "--define:NET5_0_OR_GREATER"
                "--define:NET6_0_OR_GREATER"
                "--define:NET7_0_OR_GREATER"
                "--define:NETCOREAPP1_0_OR_GREATER"
                "--define:NETCOREAPP1_1_OR_GREATER"
                "--define:NETCOREAPP2_0_OR_GREATER"
                "--define:NETCOREAPP2_1_OR_GREATER"
                "--define:NETCOREAPP2_2_OR_GREATER"
                "--define:NETCOREAPP3_0_OR_GREATER"
                "--define:NETCOREAPP3_1_OR_GREATER"
                "--optimize-"
                "--tailcalls-"
                $"-r:{fsharpCore}"
                sprintf "-r:%s" (refPath </> "Microsoft.CSharp.dll")
                sprintf "-r:%s" (refPath </> "Microsoft.VisualBasic.Core.dll")
                sprintf "-r:%s" (refPath </> "Microsoft.VisualBasic.dll")
                sprintf "-r:%s" (refPath </> "Microsoft.Win32.Primitives.dll")
                sprintf "-r:%s" (refPath </> "Microsoft.Win32.Registry.dll")
                sprintf "-r:%s" (refPath </> "mscorlib.dll")
                sprintf "-r:%s" (refPath </> "netstandard.dll")
                sprintf "-r:%s" (refPath </> "System.AppContext.dll")
                sprintf "-r:%s" (refPath </> "System.Buffers.dll")
                sprintf "-r:%s" (refPath </> "System.Collections.Concurrent.dll")
                sprintf "-r:%s" (refPath </> "System.Collections.dll")
                sprintf "-r:%s" (refPath </> "System.Collections.Immutable.dll")
                sprintf "-r:%s" (refPath </> "System.Collections.NonGeneric.dll")
                sprintf "-r:%s" (refPath </> "System.Collections.Specialized.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.Annotations.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.DataAnnotations.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.EventBasedAsync.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.ComponentModel.TypeConverter.dll")
                sprintf "-r:%s" (refPath </> "System.Configuration.dll")
                sprintf "-r:%s" (refPath </> "System.Console.dll")
                sprintf "-r:%s" (refPath </> "System.Core.dll")
                sprintf "-r:%s" (refPath </> "System.Data.Common.dll")
                sprintf "-r:%s" (refPath </> "System.Data.DataSetExtensions.dll")
                sprintf "-r:%s" (refPath </> "System.Data.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.Contracts.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.Debug.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.DiagnosticSource.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.FileVersionInfo.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.Process.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.StackTrace.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.TextWriterTraceListener.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.Tools.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.TraceSource.dll")
                sprintf "-r:%s" (refPath </> "System.Diagnostics.Tracing.dll")
                sprintf "-r:%s" (refPath </> "System.dll")
                sprintf "-r:%s" (refPath </> "System.Drawing.dll")
                sprintf "-r:%s" (refPath </> "System.Drawing.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.Dynamic.Runtime.dll")
                sprintf "-r:%s" (refPath </> "System.Formats.Asn1.dll")
                sprintf "-r:%s" (refPath </> "System.Formats.Tar.dll")
                sprintf "-r:%s" (refPath </> "System.Globalization.Calendars.dll")
                sprintf "-r:%s" (refPath </> "System.Globalization.dll")
                sprintf "-r:%s" (refPath </> "System.Globalization.Extensions.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Compression.Brotli.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Compression.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Compression.FileSystem.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Compression.ZipFile.dll")
                sprintf "-r:%s" (refPath </> "System.IO.dll")
                sprintf "-r:%s" (refPath </> "System.IO.FileSystem.AccessControl.dll")
                sprintf "-r:%s" (refPath </> "System.IO.FileSystem.dll")
                sprintf "-r:%s" (refPath </> "System.IO.FileSystem.DriveInfo.dll")
                sprintf "-r:%s" (refPath </> "System.IO.FileSystem.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.IO.FileSystem.Watcher.dll")
                sprintf "-r:%s" (refPath </> "System.IO.IsolatedStorage.dll")
                sprintf "-r:%s" (refPath </> "System.IO.MemoryMappedFiles.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Pipes.AccessControl.dll")
                sprintf "-r:%s" (refPath </> "System.IO.Pipes.dll")
                sprintf "-r:%s" (refPath </> "System.IO.UnmanagedMemoryStream.dll")
                sprintf "-r:%s" (refPath </> "System.Linq.dll")
                sprintf "-r:%s" (refPath </> "System.Linq.Expressions.dll")
                sprintf "-r:%s" (refPath </> "System.Linq.Parallel.dll")
                sprintf "-r:%s" (refPath </> "System.Linq.Queryable.dll")
                sprintf "-r:%s" (refPath </> "System.Memory.dll")
                sprintf "-r:%s" (refPath </> "System.Net.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Http.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Http.Json.dll")
                sprintf "-r:%s" (refPath </> "System.Net.HttpListener.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Mail.dll")
                sprintf "-r:%s" (refPath </> "System.Net.NameResolution.dll")
                sprintf "-r:%s" (refPath </> "System.Net.NetworkInformation.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Ping.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Requests.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Security.dll")
                sprintf "-r:%s" (refPath </> "System.Net.ServicePoint.dll")
                sprintf "-r:%s" (refPath </> "System.Net.Sockets.dll")
                sprintf "-r:%s" (refPath </> "System.Net.WebClient.dll")
                sprintf "-r:%s" (refPath </> "System.Net.WebHeaderCollection.dll")
                sprintf "-r:%s" (refPath </> "System.Net.WebProxy.dll")
                sprintf "-r:%s" (refPath </> "System.Net.WebSockets.Client.dll")
                sprintf "-r:%s" (refPath </> "System.Net.WebSockets.dll")
                sprintf "-r:%s" (refPath </> "System.Numerics.dll")
                sprintf "-r:%s" (refPath </> "System.Numerics.Vectors.dll")
                sprintf "-r:%s" (refPath </> "System.ObjectModel.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.DispatchProxy.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Emit.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Emit.ILGeneration.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Emit.Lightweight.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Extensions.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Metadata.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.Reflection.TypeExtensions.dll")
                sprintf "-r:%s" (refPath </> "System.Resources.Reader.dll")
                sprintf "-r:%s" (refPath </> "System.Resources.ResourceManager.dll")
                sprintf "-r:%s" (refPath </> "System.Resources.Writer.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.CompilerServices.Unsafe.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.CompilerServices.VisualC.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Extensions.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Handles.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.InteropServices.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.InteropServices.RuntimeInformation.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Intrinsics.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Loader.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Numerics.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Serialization.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Serialization.Formatters.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Serialization.Json.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Serialization.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.Runtime.Serialization.Xml.dll")
                sprintf "-r:%s" (refPath </> "System.Security.AccessControl.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Claims.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.Algorithms.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.Cng.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.Csp.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.Encoding.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.OpenSsl.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.Primitives.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Cryptography.X509Certificates.dll")
                sprintf "-r:%s" (refPath </> "System.Security.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Principal.dll")
                sprintf "-r:%s" (refPath </> "System.Security.Principal.Windows.dll")
                sprintf "-r:%s" (refPath </> "System.Security.SecureString.dll")
                sprintf "-r:%s" (refPath </> "System.ServiceModel.Web.dll")
                sprintf "-r:%s" (refPath </> "System.ServiceProcess.dll")
                sprintf "-r:%s" (refPath </> "System.Text.Encoding.CodePages.dll")
                sprintf "-r:%s" (refPath </> "System.Text.Encoding.dll")
                sprintf "-r:%s" (refPath </> "System.Text.Encoding.Extensions.dll")
                sprintf "-r:%s" (refPath </> "System.Text.Encodings.Web.dll")
                sprintf "-r:%s" (refPath </> "System.Text.Json.dll")
                sprintf "-r:%s" (refPath </> "System.Text.RegularExpressions.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Channels.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Overlapped.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Tasks.Dataflow.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Tasks.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Tasks.Extensions.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Tasks.Parallel.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Thread.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.ThreadPool.dll")
                sprintf "-r:%s" (refPath </> "System.Threading.Timer.dll")
                sprintf "-r:%s" (refPath </> "System.Transactions.dll")
                sprintf "-r:%s" (refPath </> "System.Transactions.Local.dll")
                sprintf "-r:%s" (refPath </> "System.ValueTuple.dll")
                sprintf "-r:%s" (refPath </> "System.Web.dll")
                sprintf "-r:%s" (refPath </> "System.Web.HttpUtility.dll")
                sprintf "-r:%s" (refPath </> "System.Windows.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.Linq.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.ReaderWriter.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.Serialization.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.XDocument.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.XmlDocument.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.XmlSerializer.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.XPath.dll")
                sprintf "-r:%s" (refPath </> "System.Xml.XPath.XDocument.dll")
                sprintf "-r:%s" (refPath </> "WindowsBase.dll")
                yield! projectReferences
                "--target:library"
                "--nowarn:FS2003,NU5105"
                "--warn:3"
                "--warnaserror:3239,1182,0025"
                "--fullpaths"
                "--flaterrors"
                "--highentropyva+"
                "--targetprofile:netcore"
                "--nocopyfsharpcore"
                "--deterministic+"
                "--simpleresolution"
                "--nowarn:3384"
                "--simpleresolution"
            |]
        ReferencedProjects =
            Array.map (fun (rp: FSharpProjectOptions) -> FSharpReferencedProject.CreateFSharp(rp.Output, rp)) referencedProjects
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.MinValue
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

type Listener() =
    inherit EventListener()

    let messages = List<string>()
    let mutable source = null

    override __.OnEventSourceCreated newSource =
        if newSource.Name = "FSharpCompiler" then
            ``base``.EnableEvents(newSource, EventLevel.LogAlways, EventKeywords.All)
            source <- newSource

    override __.OnEventWritten eventArgs =
        let payload = eventArgs.Payload

        if payload.Count = 2 then
            match payload.[0], payload.[1] with
            | :? string as message, (:? int as logCompilerFunctionId) ->
                if logCompilerFunctionId = int LogCompilerFunctionId.Service_IncrementalBuildersCache_BuildingNewCache then
                    messages.Add(message)
            | _ -> ()
        elif payload.Count = 1 then
            match payload.[0] with
            | :? int as logCompilerFunctionId when
                (logCompilerFunctionId = int LogCompilerFunctionId.Service_IncrementalBuildersCache_GettingCache)
                ->
                messages.Add("Got something from cache")
            | _ -> ()

    interface IDisposable with
        member __.Dispose() =
            if isNull source then () else ``base``.DisableEvents(source)

    member this.Messages = messages

let mutable listener = Unchecked.defaultof<Listener>
let mutable checker = Unchecked.defaultof<FSharpChecker>

[<SetUp>]
let init () =
    listener <- new Listener()
    checker <- FSharpChecker.Create(keepAssemblyContents = true)

// The goal of this test is to verify that no duplicate GraphNodes were created due to FCS_PARALLEL_PROJECTS_ANALYSIS.
[<Test>]
let ``Load reference in parallel in background check`` () =
    Environment.SetEnvironmentVariable("FCS_PARALLEL_PROJECTS_ANALYSIS", "true")

    let dotnet =
        let dotDotnet = __SOURCE_DIRECTORY__ </> ".." </> ".." </> ".dotnet"
        let exe = dotDotnet </> "dotnet.exe"
        if File.Exists exe then exe else dotDotnet </> "dotnet"

    // Build the solution first to ensure the binaries are present.
    Process.Start(dotnet, $"build \"{solutionDir}\"").WaitForExit()

    let projectOptionsC = mkProjectOptions "C" Array.empty
    let projectOptionsB1 = mkProjectOptions "B1" [| projectOptionsC |]
    let projectOptionsB2 = mkProjectOptions "B2" [| projectOptionsC |]
    let projectOptionsA = mkProjectOptions "A" [| projectOptionsB1; projectOptionsB2 |]

    let _, checkResults =
        checker.GetBackgroundCheckResultsForFileInProject(projectOptionsA.SourceFiles[0], projectOptionsA)
        |> Async.RunSynchronously

    if checkResults.Diagnostics.Length > 0 then
        Assert.Fail(sprintf "%A" checkResults.Diagnostics)

    let messages =
        listener.Messages
        |> Seq.choose (fun message ->
            if message.Contains("createBuilderNode for") then
                Some(message.Split(',').[0])
            else
                None)
        |> Seq.groupBy id
        |> Seq.filter (fun (_, group) -> Seq.length group = 1)
        |> Seq.toArray

    // Four unique GraphNodes were created.
    Assert.AreEqual(4, messages.Length)
    // The cache got hit for project C.
    Assert.True(Seq.exists ((=) "Got something from cache") listener.Messages)
