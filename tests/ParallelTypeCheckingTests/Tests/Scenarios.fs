module ParallelTypeCheckingTests.Tests.Scenarios

open FSharp.Compiler.Service.Tests.Common
open ParallelTypeCheckingTests

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
        scenario
            "Reference to property of static member from nested module is detected"
            [
                sourceFile
                    "A.fs"
                    """
module A

module B =
    type Person = {Name : string}
    type C =
        static member D: Person = failwith ""
"""
                    Set.empty
                sourceFile
                    "B.fs"
                    """
module B
let person: string = A.B.C.D.Name
"""
                    (set [| 0 |])
            ]
    ]
