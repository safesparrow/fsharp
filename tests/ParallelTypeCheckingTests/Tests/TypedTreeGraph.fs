module ParallelTypeCheckingTests.Tests.TypedTreeGraph

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols
open NUnit.Framework
open ParallelTypeCheckingTests
open ParallelTypeCheckingTests.TestUtils

type Codebase =
    {
        WorkDir: string
        Path: string
    }

    override x.ToString() = x.Path

let codebases =
    [|
        {
            WorkDir = $@"{__SOURCE_DIRECTORY__}\.fcs_test\src\compiler"
            Path = $@"{__SOURCE_DIRECTORY__}\FCS.args.txt"
        }
        {
            WorkDir = $@"{__SOURCE_DIRECTORY__}\.fcs_test\tests\FSharp.Compiler.ComponentTests"
            Path = $@"{__SOURCE_DIRECTORY__}\ComponentTests.args.txt"
        }
    |]

let checker = FSharpChecker.Create(keepAssemblyContents = true)

type DepCollector(filesThatCameBeforeIt: Set<string>) =
    let deps = HashSet<string>()

    member this.Add(declarationLocation: range) : unit =
        let sourceLocation = Path.GetFullPath declarationLocation.FileName
        let ext = Path.GetExtension sourceLocation

        if
            (ext = ".fs" || ext = ".fsi")
            && Set.contains sourceLocation filesThatCameBeforeIt
        then
            deps.Add(sourceLocation) |> ignore

    member this.Deps = Seq.toArray deps

let rec collectFromSymbol (collector: DepCollector) (s: FSharpSymbol) =
    match s with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        if mfv.ImplementationLocation.IsSome || mfv.SignatureLocation.IsSome then
            collector.Add mfv.DeclarationLocation

        collectFromSymbol collector mfv.ReturnParameter

        for cpg in mfv.CurriedParameterGroups do
            for p in cpg do
                collectFromSymbol collector p

    | :? FSharpParameter as fp ->
        if fp.Type.HasTypeDefinition then
            collector.Add fp.Type.TypeDefinition.DeclarationLocation

    | :? FSharpEntity as e ->
        if
            not (e.IsFSharpModule || e.IsNamespace)
            && (e.ImplementationLocation.IsSome || e.SignatureLocation.IsSome)
        then
            collector.Add e.DeclarationLocation

    | :? FSharpActivePatternCase as apc -> collector.Add apc.DeclarationLocation
    | _ -> ()

// Fair warning: this isn't fast or optimized code
let graphFromTypedTree (checker: FSharpChecker) (projectOptions: FSharpProjectOptions) : Async<IDictionary<int, FileWithAST> * Graph<int>> =
    async {
        let files = ConcurrentDictionary<int, FileWithAST>()

        let! filesWithDeps =
            projectOptions.SourceFiles
            |> Array.mapi (fun idx fileName ->
                async {
                    let sourceText = (File.ReadAllText >> SourceText.ofString) fileName
                    let! parseResult, checkResult = checker.ParseAndCheckFileInProject(fileName, 1, sourceText, projectOptions)

                    match checkResult with
                    | FSharpCheckFileAnswer.Aborted -> return failwith "aborted"
                    | FSharpCheckFileAnswer.Succeeded fileResult ->
                        let allSymbols = fileResult.GetAllUsesOfAllSymbolsInFile() |> Seq.toArray
                        let filesItCanKnow = set projectOptions.SourceFiles.[0 .. (idx - 1)]
                        let collector = DepCollector(filesItCanKnow)

                        for s in allSymbols do
                            collectFromSymbol collector s.Symbol

                        let file: FileWithAST =
                            {
                                Idx = idx
                                AST = parseResult.ParseTree
                                File = fileName
                            }

                        files.TryAdd(idx, file) |> ignore

                        let depIndexes =
                            collector.Deps
                            |> Array.map (fun dep -> projectOptions.SourceFiles |> Array.findIndex (fun file -> file = dep))

                        return (idx, depIndexes)
                })
            |> Async.Parallel

        let graph = readOnlyDict filesWithDeps

        return files, graph
    }

[<TestCaseSource(nameof codebases)>]
[<Explicit("Slow! Only useful as a sanity check that the test codebase is sound.")>]
let ``Create Graph from typed tree`` (code: Codebase) =
    let previousDir = Environment.CurrentDirectory

    async {
        try
            Environment.CurrentDirectory <- code.WorkDir

            let args = File.ReadAllLines(code.Path) |> Array.map replacePaths
            let fileName = Path.GetFileNameWithoutExtension(args.[0].Replace("-o:", ""))

            let sourceFiles, otherOptions =
                args
                |> Array.partition (fun option ->
                    not (option.StartsWith("-"))
                    && (option.EndsWith(".fs") || option.EndsWith(".fsi")))

            let sourceFiles = sourceFiles |> Array.map Path.GetFullPath

            let otherOptions =
                otherOptions
                |> Array.map (fun otherOption ->
                    // The reference to fsharp code needs to be an absolute one
                    if otherOption.StartsWith("-r:..") then
                        let absoluteBit = otherOption.Split(':').[1]
                        $"-r:{Path.Combine(code.WorkDir, absoluteBit)}"
                    else
                        otherOption)

            let proj =
                {
                    ProjectFileName = fileName
                    ProjectId = None
                    SourceFiles = sourceFiles
                    OtherOptions = otherOptions
                    ReferencedProjects = [||]
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.Now
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None
                }

            let! files, graphFromTypedTree = graphFromTypedTree checker proj

            graphFromTypedTree
            |> Graph.map (fun n -> files.[n].File)
            |> Graph.serialiseToJson $"{fileName}.typed-tree.deps.json"

            let collectAllDeps (graph: Graph<int>) =
                (Map.empty, [ 0 .. (sourceFiles.Length - 1) ])
                ||> List.fold (fun acc idx ->
                    let deps = graph.[idx]

                    let allDeps =
                        set [| yield! deps; yield! (Seq.collect (fun dep -> Map.find dep acc) deps) |]

                    Map.add idx allDeps acc)

            let typedTreeMap = collectAllDeps graphFromTypedTree
            let graphFromHeuristic = files.Values |> Seq.toArray |> DependencyResolution.mkGraph

            graphFromHeuristic
            |> Graph.map (fun n -> files.[n].File)
            |> Graph.serialiseToJson $"{fileName}.heuristic-tree.deps.json"

            let heuristicMap = collectAllDeps graphFromHeuristic

            let relativePath file =
                Path.GetRelativePath(code.WorkDir, file)

            let depNames (deps: Set<int>) =
                deps |> Seq.map (fun idx -> relativePath files.[idx].File) |> String.concat " "

            /// Compare the found dependencies of a specified heuristic versus the dependencies found in the typed tree
            let compareDeps source fileName idx (depsFromHeuristic: Set<int>) =
                let depsFromTypedTree = Map.find idx typedTreeMap

                if Set.isEmpty depsFromTypedTree && not (Set.isEmpty depsFromHeuristic) then
                    printfn $"{source}:{relativePath fileName} has %A{(depNames depsFromHeuristic)} while the typed tree had none!"
                else
                    let isSuperSet = Set.isSuperset depsFromHeuristic depsFromTypedTree
                    let delta = Set.difference depsFromTypedTree depsFromHeuristic

                    Assert.IsTrue(
                        isSuperSet,
                        $"""{relativePath fileName} did not contain a superset of the typed tree dependencies:
{source} is missing dependencies: %A{depNames delta}."""
                    )

            [| 0 .. (sourceFiles.Length - 1) |]
            |> Array.iter (fun (fileIdx: int) ->
                let file = files.[fileIdx]
                compareDeps "Trie heuristic" file.File file.Idx (Map.find file.Idx heuristicMap))

            printfn "%A" heuristicMap
        finally
            Environment.CurrentDirectory <- previousDir
    }
