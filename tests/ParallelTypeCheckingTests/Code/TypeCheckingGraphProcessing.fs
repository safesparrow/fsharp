/// Parallel processing of graph of work items with dependencies
module ParallelTypeCheckingTests.TypeCheckingGraphProcessing

open ParallelTypeCheckingTests.GraphProcessing
open System.Collections.Generic
open System.Threading

// TODO Do we need to suppress some error logging if we
// TODO apply the same partial results multiple times?
// TODO Maybe we can enable logging only for the final fold
/// <summary>
/// Combine results of dependencies needed to type-check a 'higher' node in the graph
/// </summary>
/// <param name="deps">Direct dependencies of a node</param>
/// <param name="transitiveDeps">Transitive dependencies of a node</param>
/// <param name="folder">A way to fold a single result into existing state</param>
/// <remarks>
/// Similar to 'processFileGraph', this function is generic yet specific to the type-checking process.
/// </remarks>
let private combineResults
    (emptyState: 'State)
    (deps: ProcessedNode<'Item, 'State * 'Result>[])
    (transitiveDeps: ProcessedNode<'Item, 'State * 'Result>[])
    (folder: 'State -> 'Result -> 'State)
    : 'State =
    match deps with
    | [||] -> emptyState
    | _ ->
        let biggestDep =
            let sizeMetric (node: ProcessedNode<_, _>) = node.Info.TransitiveDeps.Length
            deps |> Array.maxBy sizeMetric

        let firstState = biggestDep.Result |> fst

        // Add single-file results of remaining transitive deps one-by-one using folder
        // Note: Ordering is not preserved due to reusing results of the biggest child
        // rather than starting with empty state
        let included =
            let set = HashSet(biggestDep.Info.TransitiveDeps)
            set.Add biggestDep.Info.Item |> ignore
            set

        let resultsToAdd =
            transitiveDeps
            |> Array.filter (fun dep -> included.Contains dep.Info.Item = false)
            |> Array.distinctBy (fun dep -> dep.Info.Item)
            |> Array.map (fun dep -> dep.Result |> snd)

        let state = Array.fold folder firstState resultsToAdd
        state

// TODO This function and its parameters are quite specific to type-checking despite using generic types.
// Perhaps we should make it either more specific and remove type parameters, or more generic.
/// <summary>
/// Process a graph of items.
/// A version of 'GraphProcessing.processGraph' specific to type-checking.
/// </summary>
let processFileGraph<'Item, 'State, 'Result, 'FinalFileResult when 'Item: equality and 'Item: comparison>
    (graph: Graph<'Item>)
    (work: 'Item -> 'State -> 'Result)
    (folder: 'State -> 'Result -> 'FinalFileResult * 'State)
    (includeInFinalState: 'Item -> bool)
    (emptyState: 'State)
    (ct: CancellationToken)
    : ('Item * 'FinalFileResult)[] * 'State =

    let workWrapper (getProcessedNode: 'Item -> ProcessedNode<'Item, 'State * 'Result>) (node: NodeInfo<'Item>) : 'State * 'Result =
        let folder x y = folder x y |> snd
        let deps = node.Deps |> Array.except [| node.Item |] |> Array.map getProcessedNode

        let transitiveDeps =
            node.TransitiveDeps
            |> Array.except [| node.Item |]
            |> Array.map getProcessedNode

        let inputState = combineResults emptyState deps transitiveDeps folder
        let singleRes = work node.Item inputState
        let state = folder inputState singleRes
        state, singleRes

    let results = processGraph graph workWrapper includeInFinalState ct

    let finals, state: ('Item * 'FinalFileResult)[] * 'State =
        results
        |> Array.fold
            (fun (fileResults, state) (item, (_, itemRes)) ->
                let fileResult, state = folder state itemRes
                Array.append fileResults [| item, fileResult |], state)
            ([||], emptyState)

    finals, state
