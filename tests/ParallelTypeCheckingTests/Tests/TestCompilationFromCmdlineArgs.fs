module ParallelTypeCheckingTests.TestCompilationFromCmdlineArgs

open System.IO
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.DiagnosticsLogger
open NUnit.Framework
open System
open FSharp.Compiler
open ParallelTypeCheckingTests
open ParallelTypeCheckingTests.TestUtils

type Codebase =
    {
        ProjectFile : string
    }

let codebases =
    [|
        {
            ProjectFile = $@"{__SOURCE_DIRECTORY__}\.fcs_test\src\compiler\FSharp.Compiler.Service.fsproj"
        }
        {
            ProjectFile = $@"{__SOURCE_DIRECTORY__}\.fcs_test\tests\FSharp.Compiler.ComponentTests\FSharp.Compiler.ComponentTests.fsproj"
        }
    |]

let internal setupParsed {Method = method; ProjectFile = path; Parallel = _parallelFlag} =
    let args = getProjectArgs path
    let args = Array.append args (TestCompilation.methodOptions method |> List.toArray)
    //let args = Array.append args (if parallelFlag then [|"--parallelreferenceresolution"|] else [||])
    args

let internal TestCompilerFromArgs (config: Args) : unit =
    use _ =
        FSharp.Compiler.Diagnostics.Activity.start "Compile codebase" [ "method", config.Method.ToString() ]

    let oldWorkDir = Environment.CurrentDirectory

    let exiter =
        { new Exiter with
            member _.Exit n =
                Assert.Fail($"Fail - {n} errors found")
                failwith ""
        }

    try
        printfn $"Type-checking method used: {config.Method}"
        let args = setupParsed config
        File.WriteAllLines("c:/projekty/fsharp/cracking_logs/fsharp_projinfo_args.txt", args)
        //let args = File.ReadAllLines(@"C:\projekty\fsharp\heuristic\tests\ParallelTypeCheckingTests\Tests\ComponentTests.args.txt")
        let exit: int = CommandLineMain.mainAux (args, true, Some exiter)
        Assert.That(exit, Is.Zero)
    finally
        Environment.CurrentDirectory <- oldWorkDir

let internal codebaseToConfig (code : Codebase) method parallelFlag =
    {
        ProjectFile = code.ProjectFile
        Method = method
        Parallel = parallelFlag
    }

[<TestCaseSource(nameof codebases)>]
[<Explicit("Slow, only useful as a sanity check that the test codebase is sound and type-checks using the old method")>]
let ``1. Test sequential type-checking`` (code: Codebase) =
    let config = codebaseToConfig code Method.Sequential false
    TestCompilerFromArgs config

[<TestCaseSource(nameof codebases)>]
[<Explicit("Slow, only useful as a sanity check that the test codebase is sound and type-checks using the parallel-fs method")>]
let ``2. Test parallelfs type-checking`` (code: Codebase) =
    let config = codebaseToConfig code Method.ParallelCheckingOfBackedImplFiles false
    TestCompilerFromArgs config
    
[<TestCaseSource(nameof codebases)>]
let ``3. Test graph-based type-checking`` (code: Codebase) =
    let config = codebaseToConfig code Method.Graph false
    printfn $"Args file generated: {config.ProjectFile}"
    TestCompilerFromArgs config

[<TestCaseSource(nameof codebases)>]
let ``4. Test graph-based type-checking with parallel flag`` (code: Codebase) =
    let config = codebaseToConfig code Method.Graph true
    printfn $"Args file generated: {config.ProjectFile}"
    TestCompilerFromArgs config