module FSharp.Compiler.ComponentTests.TypeChecks.Graph.Graphs

open FSharp.Test
open FSharp.Test.Compiler
open FSharp.Compiler.GraphChecking
open FSharp.Compiler.GraphChecking.Graph
open NUnit.Framework
open Scenarios

let normalize<'Node> (graph : Graph<'Node>) =
    graph

let assertGraphEqual<'Node> (expected : ('Node * 'Node[]) seq) (actual : Graph<'Node>) =
    CollectionAssert.AreEquivalent(actual, expected)

let getEdges (graph : Graph<'Node>) =
    graph
    |> Seq.collect (fun (KeyValue(node, deps)) ->
        deps
        |> Array.map (fun dep -> node, dep))
    |> Seq.toArray

[<Test>]
let ``Test`` () =
    
    let sum (graph : Graph<int>) =
        graph
        |> getEdges
        |> Seq.sumBy (fun (a, b) -> a + b)
    
    let original =
        [
            1, [|2; 3|]
            2, [|3|]
        ]
        |> Graph.make
    let mapped =
        original
        |> Graph.map (fun x -> x*2)
    
    Assert.That(mapped |> sum, Is.EqualTo (original |> sum |> (*) 2))
