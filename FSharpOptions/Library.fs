namespace FSharpOptions

open System.IO
open System.Runtime.CompilerServices
open Ionide.ProjInfo
open Ionide.ProjInfo.Types

module Say =
    
    let projectPath = @"D:\projekty\parallel_test\top.fsproj"
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let init () =
        Init.init (DirectoryInfo(Path.GetDirectoryName projectPath)) None
    
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let rest (msbuild : ToolsPath) =
        let loader = WorkspaceLoader.Create(msbuild, [])
        let bl = BinaryLogGeneration.Within(DirectoryInfo(Path.GetDirectoryName projectPath))
        let projects = loader.LoadProjects([projectPath], [], bl) |> Seq.toArray
        printfn "Projects"
        let project = projects |> Array.find (fun p -> p.ProjectFileName.Contains("top.fsproj"))
        let options = FCS.mapToFSharpProjectOptions project projects
        ()
    
    [<EntryPoint>]
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let main args =
        let msbuild = init ()
        rest msbuild
        0