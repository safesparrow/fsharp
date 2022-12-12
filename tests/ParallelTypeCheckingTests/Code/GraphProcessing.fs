/// Parallel processing of graph of work items with dependencies
module ParallelTypeCheckingTests.GraphProcessing

open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open ParallelTypeCheckingTests.Parallel

/// Used for processing
type NodeInfo<'Item> =
    {
        Item: 'Item
        Deps: 'Item[]
        TransitiveDeps: 'Item[]
        Dependants: 'Item[]
    }

// TODO Do not expose this type to other files
type Node<'Item, 'Result> =
    {
        Info: NodeInfo<'Item>
        mutable ProcessedDepsCount: int
        mutable Result: 'Result option
    }

/// Basic concurrent set implemented using ConcurrentDictionary
type private ConcurrentSet<'a>() =
    let dict = ConcurrentDictionary<'a, unit>()
    
    member this.Add(item: 'a): bool =
        dict.TryAdd(item, ())
    
/// <summary>
/// A generic method to generate results for a graph of work items in parallel.
/// Processes leaves first, and after each node has been processed, schedules any now unblocked dependants.
/// Returns a list of results, per item.
/// </summary>
/// <param name="graph">Graph of work items</param>
/// <param name="doWork">A function to generate results for a single item</param>
let processGraphSimple<'Item, 'Result when 'Item: equality and 'Item: comparison>
    (graph: Graph<'Item>)
    // TODO Avoid exposing mutable nodes to the caller
    (doWork: IReadOnlyDictionary<'Item, Node<'Item, 'Result>> -> Node<'Item, 'Result> -> 'Result)
    : 'Result[] // Results in order defined in 'graph'
    =
    let transitiveDeps = graph |> Graph.transitiveOpt
    let dependants = graph |> Graph.reverse

    let makeNode (item: 'Item) : Node<'Item, 'Result> =
        let info =
            let exists = graph.ContainsKey item

            if
                not exists
                || not (transitiveDeps.ContainsKey item)
                || not (dependants.ContainsKey item)
            then
                printfn $"Unexpected inconsistent state of the graph for item '{item}'"

            {
                Item = item
                Deps = graph[item]
                TransitiveDeps = transitiveDeps[item]
                Dependants = dependants[item]
            }

        {
            Info = info
            Result = None
            ProcessedDepsCount = 0
        }

    let nodes =
        graph.Keys
        |> Seq.map (fun item -> item, makeNode item)
        |> readOnlyDict
    let lookupMany items = items |> Array.map (fun item -> nodes[item])
    let leaves =
        nodes.Values
        |> Seq.filter (fun n -> n.Info.Deps.Length = 0)
        |> Seq.toArray

    printfn $"Node count: {nodes.Count}"
    use cts = new CancellationTokenSource()

    let mutable processedCount = 0
    let waitHandle = new AutoResetEvent(false)
    let rec post node =
        Async.Start(async {work node}, cts.Token)
    and work
        (node: Node<'Item, 'Result>)
        : unit =
        let singleRes = doWork nodes node
        node.Result <- Some singleRes
        let unblockedDependants =
            node.Info.Dependants
            |> lookupMany
            // For every dependant, increment its number of processed dependencies,
            // and filter ones which now have all dependencies processed.
            |> Array.filter (fun dependant ->
                // This counter can be incremented by multiple workers on different threads.
                let pdc = Interlocked.Increment(&dependant.ProcessedDepsCount)
                // Note: We cannot read 'dependant.ProcessedDepsCount' again to avoid returning the same item multiple times.
                pdc = dependant.Info.Deps.Length)
        unblockedDependants |> Array.iter post
        if Interlocked.Increment(&processedCount) = nodes.Count then
            waitHandle.Set() |> ignore
    
    leaves |> Array.iter post
    // TODO Handle async exceptions 
    // q.Error += ...
    waitHandle.WaitOne() |> ignore
    
    nodes.Values
    |> Seq.map (fun node ->
        node.Result
        |> Option.defaultWith (fun () -> failwith $"Unexpected lack of result for item '{node.Info.Item}'")
    )
    |> Seq.toArray
