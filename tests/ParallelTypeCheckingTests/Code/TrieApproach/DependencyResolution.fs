module ParallelTypeCheckingTests.Code.TrieApproach.DependencyResolution

open System.Linq
open FSharp.Compiler.Syntax
open ParallelTypeCheckingTests

// This is pseudo code of how we could restructure the trie code
// My main benefit is that you can easily visually inspect if an identifier will match something in the trie

// This code just looks for a path in the trie
// It could be cached and is easy to reason about.
let queryTrie (trie: TrieNode) (path: ModuleSegment list) : QueryTrieNodeResult =
    let rec visit (currentNode: TrieNode) (path: ModuleSegment list) =
        match path with
        | [] -> failwith "path should not be empty"
        | [ lastNodeFromPath ] ->
            match currentNode.Children.TryGetValue(lastNodeFromPath) with
            | false, _ -> QueryTrieNodeResult.NodeDoesNotExist
            | true, childNode ->
                if Set.isEmpty childNode.Files then
                    QueryTrieNodeResult.NodeDoesNotExposeData
                else
                    QueryTrieNodeResult.NodeExposesData(childNode.Files)
        | currentPath :: restPath ->
            match currentNode.Children.TryGetValue(currentPath) with
            | false, _ -> QueryTrieNodeResult.NodeDoesNotExist
            | true, childNode -> visit childNode restPath

    visit trie path

let queryTrieMemoized (trie: TrieNode) : QueryTrie =
    Internal.Utilities.Library.Tables.memoize (queryTrie trie)

// Now how to detect the deps between files?
// Process the content of each file using some state

let processOwnNamespace (queryTrie: QueryTrie) (path: ModuleSegment list) (state: FileContentQueryState) : FileContentQueryState =
    let queryResult = queryTrie path

    match queryResult with
    | QueryTrieNodeResult.NodeDoesNotExist -> state
    | QueryTrieNodeResult.NodeDoesNotExposeData -> state.AddOwnNamespace path
    | QueryTrieNodeResult.NodeExposesData files -> state.AddOwnNamespace(path, files)

// Helper function to process a open statement
// The statement could link to files and/or should be tracked as an open namespace
let processOpenPath (queryTrie: QueryTrie) (path: ModuleSegment list) (state: FileContentQueryState) : FileContentQueryState =
    let queryResult = queryTrie path

    match queryResult with
    | QueryTrieNodeResult.NodeDoesNotExist -> state
    | QueryTrieNodeResult.NodeDoesNotExposeData -> state.AddOpenNamespace path
    | QueryTrieNodeResult.NodeExposesData files -> state.AddOpenNamespace(path, files)

// Helper function to process an identifier
let processIdentifier (queryTrie: QueryTrie) (path: ModuleSegment list) (state: FileContentQueryState) : FileContentQueryState =
    let queryResult = queryTrie path

    match queryResult with
    | QueryTrieNodeResult.NodeDoesNotExist -> state
    | QueryTrieNodeResult.NodeDoesNotExposeData ->
        // This can occur when you are have a file that uses a known namespace (for example namespace System).
        // When any other code uses that System namespace it won't find anything in the user code.
        state
    | QueryTrieNodeResult.NodeExposesData files -> state.AddDependencies files

// Typically used to folder FileContentEntry items over a FileContentQueryState
let rec processStateEntry (queryTrie: QueryTrie) (state: FileContentQueryState) (entry: FileContentEntry) : FileContentQueryState =
    match entry with
    | FileContentEntry.TopLevelNamespace (topLevelPath, content) ->
        let state =
            match topLevelPath with
            | [] -> state
            | _ -> processOwnNamespace queryTrie topLevelPath state

        List.fold (processStateEntry queryTrie) state content

    | FileContentEntry.OpenStatement path ->
        // An open statement can directly reference file or be a partial open statement
        // Both cases need to be processed.
        let stateAfterFullOpenPath = processOpenPath queryTrie path state

        // Any existing open statement could be extended with the current path (if that node where to exists in the trie)
        // The extended path could add a new link (in case of a module or namespace with types)
        // It might also not add anything at all (in case it the extended path is still a partial one)
        (stateAfterFullOpenPath, state.OpenNamespaces)
        ||> Set.fold (fun acc openNS -> processOpenPath queryTrie [ yield! openNS; yield! path ] acc)

    | FileContentEntry.PrefixedIdentifier path ->
        match path with
        | [] ->
            // should not be possible though
            state
        | _ ->
            // path could consist out of multiple segments
            (state, [| 1 .. path.Length |])
            ||> Array.fold (fun state takeParts ->
                let path = List.take takeParts path
                // process the name was if it were a FQN
                let stateAfterFullIdentifier = processIdentifier queryTrie path state

                // Process the name in combination with the existing open namespaces
                (stateAfterFullIdentifier, state.OpenNamespaces)
                ||> Set.fold (fun acc openNS -> processIdentifier queryTrie [ yield! openNS; yield! path ] acc))

    | FileContentEntry.NestedModule (nestedContent = nestedContent) ->
        // We don't want our current state to be affect by any open statements in the nested module
        let nestedState = List.fold (processStateEntry queryTrie) state nestedContent
        // Afterward we are only interested in the found dependencies in the nested module
        let foundDependencies =
            Set.union state.FoundDependencies nestedState.FoundDependencies

        { state with
            FoundDependencies = foundDependencies
        }

let getFileNameBefore (files: FileWithAST array) idx =
    files.[0 .. (idx - 1)] |> Array.map (fun f -> f.Idx) |> Set.ofArray

let time msg f a =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let result = f a
    sw.Stop()
    printfn $"{msg} took %A{sw.Elapsed.Milliseconds}ms"
    result

/// Returns a list of all the files that child nodes contain
let indexesUnderNode (node: TrieNode) : Set<int> =
    let rec collect (node: TrieNode) (continuation: int list -> int list) : int list =
        let continuations: ((int list -> int list) -> int list) list =
            [
                for node in node.Children.Values do
                    yield collect node
            ]

        let finalContinuation indexes =
            continuation [ yield! node.Files; yield! List.collect id indexes ]

        Continuation.sequence continuations finalContinuation

    Set.ofList (collect node id)

/// A "ghost" dependency is a link between files that actually should be avoided.
/// The user has a partial namespace or opens a namespace that does not produce anything.
/// In order to still be able to compile the current file, the given namespace should be known to the file.
/// We did not find it via the trie, because there are no files that contribute to this namespace.
let collectGhostDependencies (fileIndex: int) (trie: TrieNode) (queryTrie: QueryTrie) (result: FileContentQueryState) =
    // Go over all open namespaces, and assert all those links eventually went anywhere
    Set.toArray result.OpenedNamespaces
    |> Array.collect (fun path ->
        match queryTrie path with
        | QueryTrieNodeResult.NodeExposesData _
        | QueryTrieNodeResult.NodeDoesNotExist -> Array.empty
        | QueryTrieNodeResult.NodeDoesNotExposeData ->
            // At this point we are following up if an open namespace really lead nowhere.
            let node =
                let rec visit (node: TrieNode) (path: ModuleSegment list) =
                    match path with
                    | [] -> node
                    | head :: tail -> visit node.Children.[head] tail

                visit trie path

            let children = indexesUnderNode node |> Set.filter (fun idx -> idx < fileIndex)
            let intersection = Set.intersect result.FoundDependencies children

            if Set.isEmpty intersection then
                // The partial open did not lead to anything
                // In order for it to exist in the current file we need to link it
                // to some file that introduces the namespace in the trie.
                if Set.isEmpty children then
                    // In this case not a single file is contributing to the opened namespace.
                    // As a last resort we assume all files are dependent, in order to preserve valid code.
                    [| 0 .. (fileIndex - 1) |]
                else
                    [| Seq.head children |]
            else
                // The partial open did eventually lead to a link in a file
                Array.empty)

let mkGraph (files: FileWithAST array) : Graph<int> =
    // Implementation files backed by signatures should be excluded to construct the trie.
    let trieInput =
        Array.filter
            (fun f ->
                match f.AST with
                | ParsedInput.SigFile _ -> true
                | ParsedInput.ImplFile _ -> Array.forall (fun (sigFile: FileWithAST) -> sigFile.File <> $"{f.File}i") files)
            files

    let trie = TrieMapping.mkTrie trieInput
    let queryTrie: QueryTrie = queryTrieMemoized trie

    let fileContents = Array.Parallel.map FileContentMapping.mkFileContent files

    let filesWithAutoOpen =
        Array.choose
            (fun f ->
                if AlwaysLinkDetection.doesFileHasAutoOpenBehavior f.AST then
                    Some f.Idx
                else
                    None)
            trieInput

    let findDependencies (file: FileWithAST) : int * int array =
        let fileContent = fileContents.[file.Idx]
        let knownFiles = getFileNameBefore files file.Idx

        // Process all entries of a file and query the trie when required to find the dependent files.
        let result =
            // Seq is faster than List in this case.
            Seq.fold (processStateEntry queryTrie) (FileContentQueryState.Create file.Idx knownFiles) fileContent

        // after processing the file we should verify if any of the open statements are found in the trie but do not yield any file link.
        let ghostDependencies = collectGhostDependencies file.Idx trie queryTrie result

        // Automatically add all files that came before the current file that use the [<AutoOpen>] attribute.
        let topLevelAutoOpenFiles =
            if Array.isEmpty filesWithAutoOpen then
                Array.empty
            else
                [| 0 .. (file.Idx - 1) |].Intersect(filesWithAutoOpen).ToArray()

        let allDependencies =
            [|
                yield! result.FoundDependencies
                yield! ghostDependencies
                yield! topLevelAutoOpenFiles
            |]
            |> Array.distinct

        file.Idx, allDependencies

    Array.Parallel.map findDependencies files |> readOnlyDict

// =============================================================================================================
// =============================================================================================================

open NUnit.Framework
open FSharp.Compiler.Service.Tests.Common

let mkGraphAndReport files =
    let filesWithAST =
        files
        |> Array.Parallel.mapi (fun idx file ->
            {
                Idx = idx
                AST = parseSourceCode (file, System.IO.File.ReadAllText(file))
                File = file
            })

    let _graph = mkGraph filesWithAST
    ()

// for KeyValue (fileIdx, deps) in graph do
//     let depString =
//         deps |> Array.map (fun dep -> filesWithAST.[dep].File) |> String.concat "\n    "
//
//     let fileName = filesWithAST.[fileIdx]
//
//     if deps.Length = 0 then
//         printfn $"%s{fileName.File}: []"
//     else
//         printfn $"%s{fileName.File}:\n    {depString}"

[<Test>]
let ``Fantomas.Core for realzies`` () =
    [|
        @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\.NETStandard,Version=v2.0.AssemblyAttributes.fs"
        @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\obj\Debug\netstandard2.0\Fantomas.Core.AssemblyInfo.fs"
        @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\AssemblyInfo.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\ISourceTextExtensions.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\RangeHelpers.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\AstExtensions.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\AstExtensions.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\TriviaTypes.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Utils.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\SourceParser.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\AstTransformer.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\AstTransformer.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Version.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Queue.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\FormatConfig.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Defines.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Defines.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Trivia.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Trivia.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\SourceTransformer.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Context.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodePrinter.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodePrinter.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodeFormatterImpl.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodeFormatterImpl.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Validation.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Selection.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\Selection.fs"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodeFormatter.fsi"
    // @"C:\Users\nojaf\Projects\main-fantomas\src\Fantomas.Core\CodeFormatter.fs"
    |]
    |> mkGraphAndReport

let fcsFiles =
    [|
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\FSComp.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\FSIstrings.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\UtilsStrings.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\buildproperties.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\FSharp.Compiler.Service.InternalsVisibleTo.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\.NETStandard,Version=v2.0.AssemblyAttributes.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\FSharp.Compiler.Service.AssemblyInfo.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\sformat.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\sformat.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\sr.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\sr.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ResizeArray.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ResizeArray.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\HashMultiMap.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\HashMultiMap.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\EditDistance.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\EditDistance.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\TaggedCollections.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\TaggedCollections.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\illib.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\illib.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\FileSystem.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\FileSystem.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ildiag.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ildiag.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\zmap.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\zmap.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\zset.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\zset.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\XmlAdapters.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\XmlAdapters.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\InternalCollections.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\InternalCollections.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\QueueList.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\QueueList.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\lib.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\lib.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ImmutableArray.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\ImmutableArray.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\rational.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\rational.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\PathMap.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\PathMap.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\RidHelpers.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\range.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Utilities\range.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\Logger.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\Logger.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\LanguageFeatures.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\LanguageFeatures.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticOptions.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticOptions.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\TextLayoutRender.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\TextLayoutRender.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticsLogger.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticsLogger.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticResolutionHints.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\DiagnosticResolutionHints.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\prim-lexing.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\prim-lexing.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\prim-parsing.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\prim-parsing.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\ReferenceResolver.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\ReferenceResolver.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\SimulatedMSBuildReferenceResolver.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\SimulatedMSBuildReferenceResolver.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\CompilerLocation.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\CompilerLocation.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\BuildGraph.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Facilities\BuildGraph.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\il.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\il.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilx.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilx.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilascii.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilascii.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\ilpars.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\illex.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilprint.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilprint.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilmorph.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilmorph.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilsign.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilsign.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilnativeres.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilnativeres.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilsupp.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilsupp.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilbinary.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilbinary.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilread.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilread.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilwritepdb.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilwritepdb.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilwrite.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilwrite.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilreflect.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\AbstractIL\ilreflect.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\PrettyNaming.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\PrettyNaming.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\UnicodeLexing.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\UnicodeLexing.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\XmlDoc.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\XmlDoc.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTrivia.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTrivia.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTree.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTree.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTreeOps.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\SyntaxTreeOps.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\ParseHelpers.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\ParseHelpers.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\pppars.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\pars.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\LexHelpers.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\LexHelpers.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\pplex.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\artifacts\obj\FSharp.Compiler.Service\Debug\netstandard2.0\\lex.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\LexFilter.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\SyntaxTree\LexFilter.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\tainted.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\tainted.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypeProviders.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypeProviders.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\QuotationPickler.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\QuotationPickler.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\CompilerGlobalState.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\CompilerGlobalState.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTree.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTree.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreeBasics.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreeBasics.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TcGlobals.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreeOps.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreeOps.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreePickle.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\TypedTree\TypedTreePickle.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\import.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\import.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\TypeHierarchy.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\TypeHierarchy.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\infos.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\infos.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AccessibilityLogic.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AccessibilityLogic.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AttributeChecking.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AttributeChecking.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\TypeRelations.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\TypeRelations.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\InfoReader.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\InfoReader.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\NicePrint.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\NicePrint.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AugmentWithHashCompare.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\AugmentWithHashCompare.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\NameResolution.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\NameResolution.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\SignatureConformance.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\SignatureConformance.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\MethodOverrides.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\MethodOverrides.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\MethodCalls.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\MethodCalls.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\PatternMatchCompilation.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\PatternMatchCompilation.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\ConstraintSolver.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\ConstraintSolver.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckFormatStrings.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckFormatStrings.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\FindUnsolved.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\FindUnsolved.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\QuotationTranslator.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\QuotationTranslator.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\PostInferenceChecks.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\PostInferenceChecks.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckBasics.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckBasics.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckExpressions.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckExpressions.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckPatterns.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckPatterns.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckComputationExpressions.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckComputationExpressions.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckIncrementalClasses.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckIncrementalClasses.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckDeclarations.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Checking\CheckDeclarations.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\Optimizer.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\Optimizer.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\DetupleArgs.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\DetupleArgs.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\InnerLambdasToTopLevelFuncs.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\InnerLambdasToTopLevelFuncs.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerCalls.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerCalls.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerSequences.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerSequences.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerComputedCollections.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerComputedCollections.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerStateMachines.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerStateMachines.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerLocalMutables.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Optimize\LowerLocalMutables.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\EraseClosures.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\EraseClosures.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\EraseUnions.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\EraseUnions.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\IlxGen.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\CodeGen\IlxGen.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\FxResolver.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\FxResolver.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/AssemblyResolveHandler.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/AssemblyResolveHandler.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/NativeDllResolveHandler.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/NativeDllResolveHandler.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/DependencyProvider.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\DependencyManager/DependencyProvider.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerConfig.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerConfig.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerImports.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerImports.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerDiagnostics.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerDiagnostics.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\ParseAndCheckInputs.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\ParseAndCheckInputs.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\ScriptClosure.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\ScriptClosure.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerOptions.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CompilerOptions.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\OptimizeInputs.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\OptimizeInputs.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\XmlDocFileWriter.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\XmlDocFileWriter.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\BinaryResourceFormats.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\BinaryResourceFormats.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\StaticLinking.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\StaticLinking.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CreateILModule.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\CreateILModule.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\fsc.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Driver\fsc.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\FSharpDiagnostic.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\FSharpDiagnostic.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\SymbolHelpers.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\SymbolHelpers.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\Symbols.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\Symbols.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\Exprs.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\Exprs.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\SymbolPatterns.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Symbols\SymbolPatterns.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\SemanticClassification.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\SemanticClassification.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ItemKey.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ItemKey.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\SemanticClassificationKey.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\SemanticClassificationKey.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpSource.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpSource.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\IncrementalBuild.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\IncrementalBuild.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceCompilerDiagnostics.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceCompilerDiagnostics.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceConstants.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceDeclarationLists.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceDeclarationLists.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceLexing.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceLexing.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParseTreeWalk.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParseTreeWalk.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceNavigation.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceNavigation.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParamInfoLocations.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParamInfoLocations.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpParseFileResults.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpParseFileResults.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParsedInputOps.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceParsedInputOps.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceAssemblyContent.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceAssemblyContent.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceXmlDocParser.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceXmlDocParser.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ExternalSymbol.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ExternalSymbol.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\QuickParse.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\QuickParse.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpCheckerResults.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\FSharpCheckerResults.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\service.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\service.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceInterfaceStubGenerator.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceInterfaceStubGenerator.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceStructure.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceStructure.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceAnalysis.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Service\ServiceAnalysis.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Interactive\ControlledExecution.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Interactive\fsi.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Interactive\fsi.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Legacy\LegacyMSBuildReferenceResolver.fsi"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Legacy\LegacyMSBuildReferenceResolver.fs"
        @"C:\Users\nojaf\Projects\main-fsharp\src\Compiler\Legacy\LegacyHostedCompilerForTesting.fs"
    |]

[<Test>]
let ``FCS for realzies`` () = mkGraphAndReport fcsFiles

[<Test>]
[<Explicit "To be removed">]
let ``FCS for debugging`` () =
    let filesWithAST =
        fcsFiles
        |> Array.mapi (fun idx file ->
            {
                Idx = idx
                AST = parseSourceCode (file, System.IO.File.ReadAllText(file))
                File = file
            })

    let contents =
        Array.map (fun (file: FileWithAST) -> FileContentMapping.mkFileContent file) filesWithAST

    ignore contents

type Scenario =
    {
        Name: string
        Files: (FileWithAST * Set<int>) array
    }

    override x.ToString() = x.Name

let scenario name files =
    let files = files |> List.toArray |> Array.mapi (fun idx f -> f idx)
    { Name = name; Files = files }

let sourceFile fileName content (dependencies: Set<int>) =
    fun idx ->
        {
            Idx = idx
            AST = parseSourceCode (fileName, content)
            File = fileName
        },
        dependencies

let codebases =
    [
        scenario
            "Link via full open statement"
            [
                sourceFile
                    "A.fs"
                    """
module A
do ()
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module B
open A
"""
                    (set [| 0 |])
            ]
        scenario
            "Partial open statement"
            [
                sourceFile
                    "A.fs"
                    """
module Company.A
let a = ()
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module Other.B
open Company
open A
"""
                    (set [| 0 |])
            ]
        scenario
            "Link via fully qualified identifier"
            [
                sourceFile
                    "X.fs"
                    """
module X.Y.Z

let z = 9
"""
                    Set.empty
                sourceFile
                    "Y.fs"
                    """
module A.B

let a = 1 + X.Y.Z.z
"""
                    (set [| 0 |])
            ]
        scenario
            "Link via partial open and prefixed identifier"
            [
                sourceFile
                    "A.fs"
                    """
module A.B.C

let d = 1
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module X.Y.Z

open A.B

let e = C.d + 1
"""
                    (set [| 0 |])
            ]
        scenario
            "Modules sharing a namespace do not link them automatically"
            [
                sourceFile
                    "A.fs"
                    """
module Project.A

let a = 0
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module Project.B

let b = 0
"""
                    Set.empty
                sourceFile
                    "C.fs"
                    """
module Project.C

let c = 0
"""
                    Set.empty
                sourceFile
                    "D.fs"
                    """
module Project.D

let d = 0
"""
                    Set.empty
            ]
        scenario
            "Files which add types to a namespace are automatically linked to files that share said namespace"
            [
                sourceFile
                    "A.fs"
                    """
namespace Product

type X = { Y : string }
"""
                    Set.empty
                // There is no way to infer what `b` is in this example
                // It could be the type defined in A, so we cannot take any risks here.
                // We link A as dependency of B because A exposes a type in the shared namespace `Product`.
                sourceFile
                    "B.fs"
                    """
module Product.Feature

let a b = b.Y + "z"
"""
                    (set [| 0 |])
            ]
        scenario
            "Toplevel AutoOpen attribute will link to all the subsequent files"
            [
                sourceFile
                    "A.fs"
                    """
[<AutoOpen>]
module Utils

let a b c = b - c
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
namespace X

type Y = { Q: int }
"""
                    (set [| 0 |])
            ]
        // Notice how we link B.fs to A.fsi, this will always be the case for signature/implementation pairs.
        // When debugging, notice that the `Helpers` will be not a part of the trie.
        scenario
            "Signature files are being used to construct the Trie"
            [
                sourceFile
                    "A.fsi"
                    """
module A

val a: int -> int
"""
                    Set.empty
                sourceFile
                    "A.fs"
                    """
module A

module Helpers =
    let impl a = a + 1

let a b = Helpers.impl b
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module B

let b = A.a 42
"""
                    (set [| 0 |])
            ]
        scenario
            "A partial open statement still links to a file as a last resort"
            [
                sourceFile
                    "A.fs"
                    """
module X.A

let a = 0
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module X.B

let b = 0
"""
                    Set.empty
                sourceFile
                    "C.fs"
                    """
module Y.C

// This open statement does not do anything.
// It can safely be removed, but because of its presence we need to link it to something that exposes the namespace X.
// We try and pick the file with the lowest index 
open X

let c = 0
"""
                    (set [| 0 |])
            ]
        // This is a very last resort measure to link C to all files that came before it.
        // `open X` does exist but there is no file that is actively contributing to the X namespace
        // This is a trade-off scenario, if A.fs had a type or nested module we would consider it to contribute to the X namespace.
        // As it is empty, we don't include the file index in the trie.
        scenario
            "A open statement that leads nowhere should link to every file that came above it."
            [
                sourceFile
                    "A.fs"
                    """
namespace X
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
namespace Y
"""
                    Set.empty
                sourceFile
                    "C.fs"
                    """
namespace Z

open X
"""
                    (set [| 0; 1 |])
            ]
        // The nested module in this case adds content to the namespace
        // Similar if a namespace had a type.
        scenario
            "Nested module with auto open attribute"
            [
                sourceFile
                    "A.fs"
                    """
namespace Product

[<AutoOpen>]
module X =
    let x: int = 0
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module Product.Feature

let a b = x + b
"""
                    (set [| 0 |])
            ]
        // Similar to a top level auto open attribute, the global namespace also introduces a link to all the files that come after it.
        scenario
            "Global namespace always introduces a link"
            [
                sourceFile
                    "A.fs"
                    """
namespace global

type A = { B : int }
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module Product.Feature

let f a = a.B
"""
                    (set [| 0 |])
            ]
    ]

[<TestCaseSource(nameof codebases)>]
let ``Supported scenario`` (scenario: Scenario) =
    let graph = mkGraph (Array.map fst scenario.Files)

    for file, expectedDeps in scenario.Files do
        let actualDeps = graph.[file.Idx]
        Assert.AreEqual(expectedDeps, actualDeps, $"Dependencies don't match for {System.IO.Path.GetFileName file.File}")
