module Graph

type Graph<'k, 'd when 'k : comparison> =
    | Graph of data: Map<'k, 'd> * incoming: Map<'k, Set<'k>> * outgoing: Map<'k, Set<'k>>

let display (Graph (data, ins, outs)) =
    printfn "Graph\n-----"
    printfn "Node data"
    for (key, data) in (Map.toList data) do printfn "  %O : %O" key data
    printfn "\nIncoming connections per node"
    for (key, data) in (Map.toList ins) do printfn "  %O : %O" key data
    printfn "\nOutgoing connections per node"
    for (key, data) in (Map.toList outs) do printfn "  %O : %O" key data

let empty<'k, 'd when 'k : comparison> =
    Graph (Map<'k, 'd> [], Map<'k, Set<'k>> [], Map<'k, Set<'k>> [])


let checkCycle graph =
    false

let private log = false

let addNode key d outs (Graph (data, incoming, outgoing)) =
    let data' = Map.add key d data
    let outsSet = Set outs
    let outgoing' =
        let set =
            Map.tryFind key outgoing
            |> Option.defaultWith (fun () -> Set [])
            |> Set.union outsSet
        Map.add key set outgoing

    let ins =
        match Map.tryFind key incoming with
        | None -> Map.add key (Set []) incoming
        | _ -> incoming

    let incoming' =
        List.fold (fun iMap k ->
            if log then printfn "adding %O to ins of %O" key k 
            let set =
                Map.tryFind k iMap
                |> Option.defaultWith (fun () -> Set [])
                |> Set.add key
            Map.add k set iMap) ins outs
    if log then printfn "incoming : %O" incoming
    if log then printfn "with empty : %O" ins
    if log then printfn "incoming' : %O" incoming'
    Graph (data', incoming', outgoing')
        

let sortUpdate outgoing map (a, _) =
    match Map.tryFind a outgoing with
    | Some outSet ->
        if log then printfn "%s : %d outs" a (List.length (Set.toList outSet))
        Set.toList outSet
        |> List.fold (fun m k ->
            if log then printf "will remove %s from %s\n" a k
            let s =
                Map.tryFind k m
                |> Option.map (Set.remove a)
                |> Option.defaultWith (fun () -> Set [])
            Map.add k s m) map
    | None ->
        map
    

// This is a functional adaptation of Kahn's Algorithm
let sort (Graph (data, incoming, outgoing)) =
    let rec sortLevel map sorted =
        let (queueMap, map') = Map.partition (fun _ v -> List.isEmpty (Set.toList v)) map
        if log then printfn "map' : %O" map'
        let queue =
            queueMap
            |> Map.toList
        if log then printfn "queue : %O" queue

        match queue with
        | [] ->
            List.concat [sorted; queue]
        | _ ->
            let map'' = List.fold (sortUpdate outgoing) map' queue
            sortLevel map'' (List.concat [sorted ; queue ])

    let sorted = sortLevel incoming []
    if List.length sorted <> List.length (Map.toList data) then
        printfn "Sorted: %O" (List.map fst sorted)
        display (Graph (data, incoming, outgoing))
        None
    else
        List.map (fun (k, _) -> (k, Map.find k data)) sorted
        |> Some

let withNodes<'k, 'd when 'k : comparison> nodes =
    List.fold (fun g (nk, nd, nos) -> addNode nk nd nos g) empty<'k, 'd> nodes
    |> (fun g ->
        if log then display g
        g)
