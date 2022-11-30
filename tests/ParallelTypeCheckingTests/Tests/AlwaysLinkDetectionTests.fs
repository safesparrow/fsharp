module ParallelTypeCheckingTests.Tests.AlwaysLinkDetectionTests

open NUnit.Framework
open FSharp.Compiler.Service.Tests.Common
open ParallelTypeCheckingTests.AlwaysLinkDetection

[<Test>]
let ``Detect top level auto open`` () =
    let fileContent =
        """
[<AutoOpen>]
module internal Block

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module ImmutableArrayBuilder =

    val create: size: int -> ImmutableArray<'T>.Builder

[<RequireQualifiedAccess>]
module ImmutableArray =

    [<GeneralizableValue>]
    val empty<'T> : ImmutableArray<'T>

    val init: n: int -> f: (int -> 'T) -> ImmutableArray<'T>
"""

    let ast = parseSourceCode ("ImmutableArray.fsi", fileContent)
    Assert.True(doesFileHasAutoOpenBehavior ast)

[<Test>]
let ``Detect global namespace`` () =
    let fileContent =
        """
namespace global

type X = { Y: int }
"""

    let ast = parseSourceCode ("Global.fsi", fileContent)
    Assert.True(doesFileHasAutoOpenBehavior ast)

[<Test>]
let ``Top level auto open module with prefixed namespace should not be consider as always linked`` () =
    let fileContent =
        """
[<AutoOpen>]
module A.B
let a = 0
"""

    let ast = parseSourceCode ("Global.fsi", fileContent)
    Assert.False(doesFileHasAutoOpenBehavior ast)
