/// Parallel processing of graph of work items with dependencies
module ParallelTypeCheckingTests.GraphProcessing

open System.Collections.Generic
open System.Threading

/// Used for processing
type NodeInfo<'Item> =
    {
        Item: 'Item
        Deps: 'Item[]
        TransitiveDeps: 'Item[]
        Dependants: 'Item[]
    }

type Node<'Item, 'Result> =
    {
        Info: NodeInfo<'Item>
        mutable ProcessedDepsCount: int
        mutable Result: 'Result option
    }

let processGraphSimple<'Item, 'Result when 'Item: equality and 'Item: comparison>
    (graph: Graph<'Item>)
    (doWork: IReadOnlyDictionary<'Item, Node<'Item, 'Result>> -> Node<'Item, 'Result> -> 'Result)
    (parallelism: int)
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
    let lookup item = nodes[item]
    let lookupMany items = items |> Array.map lookup
    let leaves =
        nodes.Values
        |> Seq.filter (fun n -> n.Info.Deps.Length = 0)
        |> Seq.toArray

    printfn $"Node count: {nodes.Count}"

    let work
        (node: Node<'Item, 'Result>)
        : Node<'Item, 'Result>[] =
        let singleRes = doWork nodes node
        node.Result <- Some singleRes
        // Need to double-check that only one dependency schedules this dependant
        let unblocked =
            node.Info.Dependants
            |> lookupMany
            |> Array.filter (fun x ->
                let pdc =
                    // TODO Not ideal, better ways most likely exist
                    lock x (fun () ->
                        x.ProcessedDepsCount <- x.ProcessedDepsCount + 1
                        x.ProcessedDepsCount)
                pdc = x.Info.Deps.Length)
        unblocked

    use cts = new CancellationTokenSource()

    Parallel.processInParallel
        leaves
        work
        parallelism
        (fun processedCount -> processedCount = nodes.Count)
        cts.Token
        (fun x -> x.Info.Item.ToString())

    nodes.Values
    |> Seq.map (fun node ->
        node.Result
        |> Option.defaultWith (fun () -> failwith $"Unexpected lack of result for item '{node.Info.Item}'")
    )
    |> Seq.toArray
