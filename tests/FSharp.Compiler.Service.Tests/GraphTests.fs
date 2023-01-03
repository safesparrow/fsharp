module FSharp.Compiler.Service.Tests.GraphTests

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open NUnit.Framework

let versionFile = @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Version.fs"
let versionSourceText = File.ReadAllText versionFile |> SourceText.ofString

let projectOptions: FSharpProjectOptions =
    {
        ProjectFileName = "Fantomas.Core"
        ProjectId = None
        SourceFiles =
            [|
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\.NETStandard,Version=v2.0.AssemblyAttributes.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.AssemblyInfo.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\AssemblyInfo.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\ISourceTextExtensions.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\ISourceTextExtensions.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\RangeHelpers.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\RangeHelpers.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Utils.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Utils.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\FormatConfig.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\SyntaxOak.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\ASTTransformer.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\ASTTransformer.fs"
                versionFile
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Queue.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Trivia.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Trivia.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Defines.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Defines.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Context.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Context.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodePrinter.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodePrinter.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodeFormatterImpl.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodeFormatterImpl.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Validation.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Validation.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Selection.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Selection.fs"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodeFormatter.fsi"
                @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\CodeFormatter.fs"
            |]
        OtherOptions =
            [|
                @"-o:C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.dll"
                @"-g"
                @"--debug:embedded"
                @"--embed:C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.AssemblyInfo.fs"
                @"--sourcelink:C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.sourcelink.json"
                @"--noframework"
                @"--define:TRACE"
                @"--define:DEBUG"
                @"--define:NETSTANDARD"
                @"--define:NETSTANDARD2_0"
                @"--define:NETSTANDARD1_0_OR_GREATER"
                @"--define:NETSTANDARD1_1_OR_GREATER"
                @"--define:NETSTANDARD1_2_OR_GREATER"
                @"--define:NETSTANDARD1_3_OR_GREATER"
                @"--define:NETSTANDARD1_4_OR_GREATER"
                @"--define:NETSTANDARD1_5_OR_GREATER"
                @"--define:NETSTANDARD1_6_OR_GREATER"
                @"--define:NETSTANDARD2_0_OR_GREATER"
                @"--doc:C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.xml"
                @"--optimize-"
                @"-r:C:\Users\nojaf\Projects\fantomas\src\Fantomas.FCS\obj\Debug\netstandard2.0\ref\Fantomas.FCS.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\fsharp.core\6.0.1\lib\netstandard2.0\FSharp.Core.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\Microsoft.Win32.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\mscorlib.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.AppContext.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\system.buffers\4.5.1\ref\netstandard2.0\System.Buffers.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.Concurrent.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.NonGeneric.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Collections.Specialized.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.Composition.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.EventBasedAsync.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ComponentModel.TypeConverter.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Console.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Core.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Data.Common.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Data.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Contracts.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Debug.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.FileVersionInfo.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Process.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.StackTrace.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.TextWriterTraceListener.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Tools.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.TraceSource.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Diagnostics.Tracing.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Drawing.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Drawing.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Dynamic.Runtime.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.Calendars.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Globalization.Extensions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.FileSystem.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Compression.ZipFile.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.DriveInfo.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.FileSystem.Watcher.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.IsolatedStorage.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.MemoryMappedFiles.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.Pipes.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.IO.UnmanagedMemoryStream.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Expressions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Parallel.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Linq.Queryable.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\system.memory\4.5.4\lib\netstandard2.0\System.Memory.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Http.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.NameResolution.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.NetworkInformation.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Ping.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Requests.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Security.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.Sockets.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebHeaderCollection.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebSockets.Client.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Net.WebSockets.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Numerics.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\system.numerics.vectors\4.4.0\ref\netstandard2.0\System.Numerics.Vectors.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ObjectModel.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.Extensions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Reflection.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.Reader.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.ResourceManager.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Resources.Writer.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\system.runtime.compilerservices.unsafe\4.5.3\ref\netstandard2.0\System.Runtime.CompilerServices.Unsafe.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.CompilerServices.VisualC.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Extensions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Handles.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.InteropServices.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.InteropServices.RuntimeInformation.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Numerics.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Formatters.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Json.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Runtime.Serialization.Xml.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Claims.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Algorithms.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Csp.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Encoding.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.Primitives.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Cryptography.X509Certificates.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.Principal.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Security.SecureString.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ServiceModel.Web.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.Encoding.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.Encoding.Extensions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Text.RegularExpressions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Overlapped.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Tasks.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Tasks.Parallel.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Thread.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.ThreadPool.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Threading.Timer.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Transactions.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.ValueTuple.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Web.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Windows.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.Linq.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.ReaderWriter.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.Serialization.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XDocument.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XmlDocument.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XmlSerializer.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XPath.dll"
                @"-r:C:\Users\nojaf\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\System.Xml.XPath.XDocument.dll"
                @"--target:library"
                @"--nowarn:NU1603,IL2121"
                @"--warn:3"
                @"--warnon:1182,3390"
                @"--warnaserror:3239,FS0025,FS1182"
                @"--fullpaths"
                @"--flaterrors"
                @"--highentropyva+"
                @"--targetprofile:netstandard"
                @"--nocopyfsharpcore"
                @"--deterministic+"
                @"--simpleresolution"
                @"--test:ParallelCheckingWithSignatureFilesOn"
                @"--refout:C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\refint\Fantomas.Core.dll"
            |]
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

let checker = FSharpChecker.Create()

[<Test>]
let ``type check file`` () =
    let result =
        checker.ParseAndCheckFileInProject(versionFile, 1, versionSourceText, projectOptions)
        |> Async.RunSynchronously

    ()

[<Test>]
let ``type check file using graph`` () =
    let graphResult =
        checker.ParseAndCheckFileInProjectUsingGraph(versionFile, projectOptions)
        |> Async.RunSynchronously

    ()
