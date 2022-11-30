module ParallelTypeCheckingTests.Tests.TrieMappingTests

open NUnit.Framework
open FSharp.Compiler.Service.Tests.Common
open ParallelTypeCheckingTests

let sampleFiles =
    [|
        "A.fs",
        """
module X.Y.A

let a = []
"""
        "B.fs",
        """
module X.Y.B

let b = []
"""
        "C.fs",
        """
namespace X.Y

type C = { CX: int; CY: int }
"""
    |]

[<Test>]
let ``Basic trie`` () =
    let files =
        sampleFiles
        |> Array.mapi (fun idx (fileName, code) ->
            {
                Idx = idx
                File = fileName
                AST = parseSourceCode (fileName, code)
            })

    let trie = TrieMapping.mkTrie files

    match trie.Current with
    | TrieNodeInfo.Root -> ()
    | current -> Assert.Fail($"mkTrie should always return a TrieNodeInfo.Root, got {current}")

    let xNode = trie.Children.["X"]
    Assert.AreEqual(1, xNode.Children.Count)
    Assert.True(Seq.isEmpty xNode.Files)

    let yNode = xNode.Children["Y"]
    Assert.AreEqual(2, yNode.Children.Count)
    Assert.AreEqual(set [| 2 |], yNode.Files)

    let aNode = yNode.Children["A"]
    Assert.AreEqual(0, aNode.Children.Count)
    Assert.AreEqual(set [| 0 |], aNode.Files)

    let bNode = yNode.Children["B"]
    Assert.AreEqual(0, bNode.Children.Count)
    Assert.AreEqual(set [| 1 |], bNode.Files)

[<Test>]
let ``Toplevel AutoOpen module with prefixed namespace`` () =
    let trie =
        TrieMapping.mkTrie
            [|
                {
                    Idx = 0
                    File = "A.fs"
                    AST =
                        parseSourceCode (
                            "A.fs",
                            """
[<AutoOpen>]
module A.B

let a = 0
"""
                        )
                }
            |]

    // Assert that both A and B expose file index 0
    let aNode = trie.Children.["A"]
    Assert.AreEqual(set [| 0 |], aNode.Files)
    let bNode = aNode.Children.["B"]
    Assert.AreEqual(set [| 0 |], bNode.Files)

[<Test>]
let ``Toplevel AutoOpen module with multi prefixed namespace`` () =
    let trie =
        TrieMapping.mkTrie
            [|
                {
                    Idx = 0
                    File = "A.fsi"
                    AST =
                        parseSourceCode (
                            "A.fsi",
                            """
[<AutoOpen>]
module A.B.C

let a = 0
"""
                        )
                }
            |]

    // Assert that B and C expose file index 0, namespace A should not.
    let aNode = trie.Children.["A"]
    Assert.AreEqual(Set.empty, aNode.Files)
    let bNode = aNode.Children.["B"]
    Assert.AreEqual(set [| 0 |], bNode.Files)
    let cNode = bNode.Children.["C"]
    Assert.AreEqual(set [| 0 |], cNode.Files)
