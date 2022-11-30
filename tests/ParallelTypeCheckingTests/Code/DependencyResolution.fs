﻿module ParallelTypeCheckingTests.DependencyResolution

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
    // Map to easily retrieve the signature file index
    let implToSig =
        Array.choose
            (fun f ->
                match f.AST with
                | ParsedInput.SigFile _ ->
                    files
                    |> Array.skip (f.Idx + 1)
                    |> Array.tryFind (fun (implFile: FileWithAST) -> $"{implFile.File}i" = f.File)
                    |> Option.map (fun (implFile: FileWithAST) -> (implFile.Idx, f.Idx))
                | ParsedInput.ImplFile _ -> None)
            files
        |> Map.ofArray

    // Implementation files backed by signatures should be excluded to construct the trie.
    // Signature files should link to the implementation index instead.
    let trieInput =
        Array.choose
            (fun f ->
                match f.AST with
                | ParsedInput.SigFile _ -> Some f
                | ParsedInput.ImplFile _ -> if Map.containsKey f.Idx implToSig then None else Some f)
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

        // Automatically add a link from an implementation to its signature file (if present)
        let signatureDependency =
            match Map.tryFind file.Idx implToSig with
            | None -> Array.empty
            | Some sigIdx -> Array.singleton sigIdx

        let allDependencies =
            [|
                yield! result.FoundDependencies
                yield! ghostDependencies
                yield! topLevelAutoOpenFiles
                yield! signatureDependency
            |]
            |> Array.distinct

        file.Idx, allDependencies

    Array.Parallel.map findDependencies files |> readOnlyDict
