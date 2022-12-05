module ParallelTypeCheckingTests.TrieMapping

open System.Collections.Generic
open FSharp.Compiler.Syntax
open Microsoft.FSharp.Collections

let hs f = HashSet(Seq.singleton f)
let emptyHS () = HashSet(0)

let private autoOpenShapes =
    set
        [|
            "FSharp.Core.AutoOpenAttribute"
            "Core.AutoOpenAttribute"
            "AutoOpenAttribute"
            "FSharp.Core.AutoOpen"
            "Core.AutoOpen"
            "AutoOpen"
        |]

/// This isn't bullet proof but I wonder who would really alias this very core attribute.
let private isAutoOpenAttribute (attribute: SynAttribute) =
    match attribute.ArgExpr with
    | SynExpr.Const(constant = SynConst.Unit)
    | SynExpr.Const(constant = SynConst.String _)
    | SynExpr.Paren(expr = SynExpr.Const(constant = SynConst.String _)) ->
        let attributeName =
            attribute.TypeName.LongIdent
            |> List.map (fun ident -> ident.idText)
            |> String.concat "."

        autoOpenShapes.Contains attributeName
    | _ -> false

let private isAnyAttributeAutoOpen (attributes: SynAttributes) =
    not attributes.IsEmpty
    && List.exists (fun (atl: SynAttributeList) -> List.exists isAutoOpenAttribute atl.Attributes) attributes

/// Checks to see if the top level ModuleOrNamespace exposes content that could be inferred by any of the subsequent files.
/// This can happen when a `namespace global` is used, or when a module (with a single ident name) has the `[<AutoOpen>]` attribute.
let private doesFileExposeContentToTheRoot (ast: ParsedInput) : bool =
    match ast with
    | ParsedInput.SigFile (ParsedSigFileInput (contents = contents)) ->
        List.exists
            (fun (SynModuleOrNamespaceSig (attribs = attribs; longId = longId; kind = kind)) ->
                (isAnyAttributeAutoOpen attribs && longId.Length < 2)
                || kind = SynModuleOrNamespaceKind.GlobalNamespace)
            contents
    | ParsedInput.ImplFile (ParsedImplFileInput (contents = contents)) ->
        List.exists
            (fun (SynModuleOrNamespace (attribs = attribs; longId = longId; kind = kind)) ->
                (isAnyAttributeAutoOpen attribs && longId.Length < 2)
                || kind = SynModuleOrNamespaceKind.GlobalNamespace)
            contents

let mergeTrieNodes (defaultChildSize: int) (tries: TrieNode array) =
    let rec mergeTrieNodesAux (root: TrieNode) (KeyValue (k, v)) =
        if root.Children.ContainsKey k then
            let node = root.Children.[k]

            match node.Current, v.Current with
            | TrieNodeInfo.Namespace (filesThatExposeTypes = currentFiles), TrieNodeInfo.Namespace (filesThatExposeTypes = otherFiles) ->
                for otherFile in otherFiles do
                    do ()
                    currentFiles.Add(otherFile) |> ignore
            | _ -> ()

            for kv in v.Children do
                mergeTrieNodesAux node kv

        else
            root.Children.Add(k, v)

    match Array.tryExactlyOne tries with
    | Some ({ Current = TrieNodeInfo.Root _ } as singleTrie) -> singleTrie
    | _ ->
        let rootFiles = emptyHS ()

        let root =
            {
                Current = TrieNodeInfo.Root rootFiles
                Children = Dictionary<_, _>(defaultChildSize)
            }

        for trie in tries do
            for rootIndex in trie.Files do
                rootFiles.Add rootIndex |> ignore

            match trie.Current with
            | TrieNodeInfo.Root _ -> ()
            | current -> System.Diagnostics.Debug.Assert(false, $"The top level node info of a trie should be Root, got {current}")

            for kv in trie.Children do
                mergeTrieNodesAux root kv

        root

let rec mkTrieNodeFor (file: FileWithAST) : TrieNode =
    let idx = file.Idx
    let fileExposesToRoot = doesFileExposeContentToTheRoot file.AST

    match file.AST with
    | ParsedInput.SigFile (ParsedSigFileInput (contents = contents)) ->
        contents
        |> List.choose
            (fun (SynModuleOrNamespaceSig (longId = longId; kind = kind; attribs = attribs; decls = decls; accessibility = _accessibility)) ->
                let hasTypesOrAutoOpenNestedModules =
                    List.exists
                        (function
                        | SynModuleSigDecl.Types _ -> true
                        | SynModuleSigDecl.NestedModule(moduleInfo = SynComponentInfo (attributes = attributes)) ->
                            isAnyAttributeAutoOpen attributes
                        | _ -> false)
                        decls

                let isNamespace =
                    match kind with
                    | SynModuleOrNamespaceKind.AnonModule
                    | SynModuleOrNamespaceKind.NamedModule -> false
                    | SynModuleOrNamespaceKind.DeclaredNamespace
                    | SynModuleOrNamespaceKind.GlobalNamespace -> true

                let rootFiles = if fileExposesToRoot then hs idx else emptyHS ()

                let children =
                    let rec visit continuation (xs: LongIdent) =
                        match xs with
                        | [] -> failwith "should even empty"
                        | [ finalPart ] ->
                            let name = finalPart.idText

                            let current =
                                if isNamespace then
                                    TrieNodeInfo.Namespace(
                                        name,
                                        (if hasTypesOrAutoOpenNestedModules then
                                             hs idx
                                         else
                                             emptyHS ())
                                    )
                                else
                                    TrieNodeInfo.Module(name, idx)

                            let children = List.choose (mkTrieForNestedSigModule idx) decls

                            continuation (
                                Dictionary<_, _>(
                                    Seq.singleton (
                                        KeyValuePair(
                                            name,
                                            {
                                                Current = current
                                                Children = Dictionary(children)
                                            }
                                        )
                                    )
                                )
                            )
                        | head :: tail ->
                            let name = head.idText

                            visit
                                (fun node ->
                                    let files =
                                        match tail with
                                        | [ _ ] ->
                                            let topLevelModuleOrNamespaceHasAutoOpen = isAnyAttributeAutoOpen attribs

                                            if topLevelModuleOrNamespaceHasAutoOpen && not isNamespace then
                                                hs idx
                                            else
                                                emptyHS ()
                                        | _ -> emptyHS ()

                                    let current = TrieNodeInfo.Namespace(name, files)

                                    Dictionary<_, _>(Seq.singleton (KeyValuePair(name, { Current = current; Children = node })))
                                    |> continuation)
                                tail

                    if List.isEmpty longId then
                        // This can happen for a namespace global.
                        // We collect the child nodes from the decls
                        List.choose (mkTrieForNestedSigModule idx) decls |> Dictionary
                    else
                        visit id longId

                Some
                    {
                        Current = Root rootFiles
                        Children = children
                    })
        |> List.toArray
        |> mergeTrieNodes contents.Length
    | ParsedInput.ImplFile (ParsedImplFileInput (contents = contents)) ->
        contents
        |> List.choose
            (fun (SynModuleOrNamespace (longId = longId; attribs = attribs; kind = kind; decls = decls; accessibility = _accessibility)) ->
                let hasTypesOrAutoOpenNestedModules =
                    List.exists
                        (function
                        | SynModuleDecl.Types _ -> true
                        | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo (attributes = attributes)) ->
                            isAnyAttributeAutoOpen attributes
                        | _ -> false)
                        decls

                let isNamespace =
                    match kind with
                    | SynModuleOrNamespaceKind.AnonModule
                    | SynModuleOrNamespaceKind.NamedModule -> false
                    | SynModuleOrNamespaceKind.DeclaredNamespace
                    | SynModuleOrNamespaceKind.GlobalNamespace -> true

                let rootFiles = if fileExposesToRoot then hs idx else emptyHS ()

                let children =
                    let rec visit
                        (continuation: Dictionary<ModuleSegment, TrieNode> -> Dictionary<ModuleSegment, TrieNode>)
                        (xs: LongIdent)
                        : Dictionary<ModuleSegment, TrieNode> =
                        match xs with
                        | [] -> failwith "should even empty"
                        | [ finalPart ] ->
                            let name = finalPart.idText

                            let current =
                                if isNamespace then
                                    TrieNodeInfo.Namespace(
                                        name,
                                        (if hasTypesOrAutoOpenNestedModules then
                                             hs idx
                                         else
                                             emptyHS ())
                                    )
                                else
                                    TrieNodeInfo.Module(name, idx)

                            let children = List.choose (mkTrieForSynModuleDecl idx) decls

                            continuation (
                                Dictionary<_, _>(
                                    Seq.singleton (
                                        KeyValuePair(
                                            name,
                                            {
                                                Current = current
                                                Children = Dictionary(children)
                                            }
                                        )
                                    )
                                )
                            )
                        | head :: tail ->
                            let name = head.idText

                            visit
                                (fun node ->
                                    let files =
                                        match tail with
                                        | [ _ ] ->
                                            let topLevelModuleOrNamespaceHasAutoOpen = isAnyAttributeAutoOpen attribs

                                            if topLevelModuleOrNamespaceHasAutoOpen && not isNamespace then
                                                hs idx
                                            else
                                                emptyHS ()
                                        | _ -> emptyHS ()

                                    let current = TrieNodeInfo.Namespace(name, files)

                                    Dictionary<_, _>(Seq.singleton (KeyValuePair(name, { Current = current; Children = node })))
                                    |> continuation)
                                tail

                    if List.isEmpty longId then
                        // This can happen for anonymous modules and namespace global.
                        // We collect the child nodes from the decls
                        List.choose (mkTrieForSynModuleDecl idx) decls |> Dictionary
                    else
                        visit id longId

                Some
                    {
                        Current = Root rootFiles
                        Children = children
                    })
        |> List.toArray
        |> mergeTrieNodes contents.Length

and mkTrieForSynModuleDecl (fileIndex: int) (decl: SynModuleDecl) : KeyValuePair<string, TrieNode> option =
    match decl with
    | SynModuleDecl.NestedModule (moduleInfo = SynComponentInfo(longId = [ nestedModuleIdent ]); decls = decls) ->
        let name = nestedModuleIdent.idText
        let children = List.choose (mkTrieForSynModuleDecl fileIndex) decls

        Some(
            KeyValuePair(
                name,
                {
                    Current = TrieNodeInfo.Module(name, fileIndex)
                    Children = Dictionary(children)
                }
            )
        )
    | _ -> None

and mkTrieForNestedSigModule (fileIndex: int) (decl: SynModuleSigDecl) : KeyValuePair<string, TrieNode> option =
    match decl with
    | SynModuleSigDecl.NestedModule (moduleInfo = SynComponentInfo(longId = [ nestedModuleIdent ]); moduleDecls = decls) ->
        let name = nestedModuleIdent.idText
        let children = List.choose (mkTrieForNestedSigModule fileIndex) decls

        Some(
            KeyValuePair(
                name,
                {
                    Current = TrieNodeInfo.Module(name, fileIndex)
                    Children = Dictionary(children)
                }
            )
        )

    | _ -> None

let mkTrie (files: FileWithAST array) : TrieNode =
    mergeTrieNodes 0 (Array.Parallel.map mkTrieNodeFor files)
