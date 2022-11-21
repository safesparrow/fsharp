module ParallelTypeCheckingTests.Code.TrieApproach.AutoOpenDetection

open FSharp.Compiler.Syntax

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
let isAutoOpenAttribute (attribute: SynAttribute) =
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

let isAnyAttributeAutoOpen (attributes: SynAttributes) =
    List.exists (fun (atl: SynAttributeList) -> List.exists isAutoOpenAttribute atl.Attributes) attributes

let hasAutoOpenAttributeInFile (ast: ParsedInput) : bool =
    match ast with
    | ParsedInput.SigFile (ParsedSigFileInput (contents = contents)) ->
        List.exists (fun (SynModuleOrNamespaceSig (attribs = attribs)) -> isAnyAttributeAutoOpen attribs) contents
    | ParsedInput.ImplFile (ParsedImplFileInput (contents = contents)) ->
        List.exists (fun (SynModuleOrNamespace (attribs = attribs)) -> isAnyAttributeAutoOpen attribs) contents

// ==============================================================================================================================
// ==============================================================================================================================

open System.IO
open NUnit.Framework
open FSharp.Compiler.Service.Tests.Common

[<Test>]
let ``detect auto open`` () =
    let file =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "..", "src", "Compiler", "Utilities", "ImmutableArray.fsi")

    let ast = parseSourceCode (file, File.ReadAllText(file))
    Assert.True(hasAutoOpenAttributeInFile ast)
