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
let private combineResults
    (emptyState: 'State)
    (deps: Node<'Item, 'State * 'Result>[])
    (transitiveDeps: Node<'Item, 'State * 'Result>[])
    (folder: 'State -> 'Result -> 'State)
    : 'State =
    match deps with
    | [||] -> emptyState
    | _ ->
        let biggestDep =
            let sizeMetric (node: Node<_,_>) =
                node.Info.TransitiveDeps.Length
            deps
            |> Array.maxBy sizeMetric

        let orFail value =
            value |> Option.defaultWith (fun () -> failwith "Unexpected lack of result")

        let firstState = biggestDep.Result |> orFail |> fst

        // TODO Potential perf optimisation: Keep transDeps in a HashSet from the start,
        // avoiding reconstructing the HashSet here

        // Add single-file results of remaining transitive deps one-by-one using folder
        // Note: Good to preserve order here so that folding happens in file order
        let included =
            let set = HashSet(biggestDep.Info.TransitiveDeps)
            set.Add biggestDep.Info.Item |> ignore
            set

        let resultsToAdd =
            transitiveDeps
            |> Array.filter (fun dep -> included.Contains dep.Info.Item = false)
            |> Array.distinctBy (fun dep -> dep.Info.Item)
            |> Array.map (fun dep -> dep.Result |> orFail |> snd)

        let state = Array.fold folder firstState resultsToAdd
        state

let processGraph<'Item, 'State, 'Result, 'FinalFileResult when 'Item: equality and 'Item: comparison>
    (graph: Graph<'Item>)
    (doWork: 'Item -> 'State -> 'Result)
    (folder: bool -> 'State -> 'Result -> 'FinalFileResult * 'State)
    (emptyState: 'State)
    (parallelism: int)
    : 'FinalFileResult[] * 'State =

    let work
        (dict: IReadOnlyDictionary<'Item, Node<'Item, 'State * 'Result>>)
        (node: Node<'Item, 'State * 'Result>)
        : 'State * 'Result =
        let folder x y = folder false x y |> snd
        let deps = node.Info.Deps |> Array.map (fun node -> dict[node])
        let transitiveDeps = node.Info.TransitiveDeps |> Array.map (fun node -> dict[node])
        let inputState = combineResults emptyState deps transitiveDeps folder
        let singleRes = doWork node.Info.Item inputState
        let state = folder inputState singleRes
        state, singleRes

    use cts = new CancellationTokenSource()

    let results =
        processGraphSimple
            graph
            work
            parallelism

    let finals, state: 'FinalFileResult[] * 'State =
        results
        |> Array.fold
            (fun (fileResults, state) (_, itemRes) ->
                let fileResult, state = folder true state itemRes
                Array.append fileResults [| fileResult |], state)
            ([||], emptyState)

    finals, state
