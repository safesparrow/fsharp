module FSharp.Compiler.Service.Tests.PFilesTests

open NUnit.Framework
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.IO
open FSharp.Compiler.Text
open TestFramework


let mkStandardProjectReferences () =
    FSharp.Test.Utilities.TargetFrameworkUtil.currentReferences

let mkProjectCommandLineArgsSilent (dllName, fileNames) =
  let args =
    [|  yield "--simpleresolution"
        yield "--noframework"
        yield "--debug:full"
        yield "--define:DEBUG"
#if NETCOREAPP
        yield "--targetprofile:netcore"
        yield "--langversion:preview"
#endif
        yield "--optimize-"
        yield "--out:" + dllName
        yield "--doc:test.xml"
        yield "--warn:3"
        yield "--fullpaths"
        yield "--flaterrors"
        yield "--target:library"
        for x in fileNames do
            yield x
        let references = mkStandardProjectReferences ()
        for r in references do
            yield "-r:" + r
     |]
  args


[<Test>]
let ``PFiles``() =

    let fileName1 = Path.ChangeExtension(tryCreateTemporaryFileName (), ".fs")
    let fileName2 = Path.ChangeExtension(tryCreateTemporaryFileName (), ".fs")
    let base2 = tryCreateTemporaryFileName ()
    let dllName = Path.ChangeExtension(base2, ".dll")
    let projFileName = Path.ChangeExtension(base2, ".fsproj")
    let fileSource1Text = """
namespace pfiles_test

module Foo =
    let x = 3

module Bar =
    open Foo
    let y = x
    let z = Foo.x
"""
    let fileSource1 = SourceText.ofString fileSource1Text
    FileSystem.OpenFileForWriteShim(fileName1).Write(fileSource1Text)
    
    let fileSource2Text = """
namespace pfiles_test

module Foo2 =
    let x = 3

module Bar2 =
    open Foo2
    let y = x
    let z = Foo.x
"""
    let fileSource2 = SourceText.ofString fileSource2Text
    FileSystem.OpenFileForWriteShim(fileName1).Write(fileSource2Text)

    let fileNames = [fileName2; fileName1]
    let args = mkProjectCommandLineArgs (dllName, fileNames)
    let keepAssemblyContentsChecker = FSharpChecker.Create(keepAssemblyContents=true)
    let options =  keepAssemblyContentsChecker.GetProjectOptionsFromCommandLineArgs (projFileName, args)

    let fileCheckResults =
        keepAssemblyContentsChecker.ParseAndCheckFileInProject(fileName1, 0, fileSource1, options)  |> Async.RunImmediate
        |> function
            | _, FSharpCheckFileAnswer.Succeeded(res) -> res
            | _ -> failwithf "Parsing aborted unexpectedly..."
    let lines = FileSystem.OpenFileForReadShim(fileName1).ReadAllLines()
    let unusedOpens = UnusedOpens.getUnusedOpens (fileCheckResults, (fun i -> lines[i-1])) |> Async.RunImmediate
    ()
