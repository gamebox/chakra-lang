module Env
type Frame<'a> =
    { Bindings: Map<string, 'a> }

/// <summary>
/// A stack of maps that can be searched for symbols.
/// </summary>
///
type Env<'a> =
    { Current: Frame<'a>
      Rest: Frame<'a> list }

let emptyWith bs =
    let bindings =
        new Map<string, 'a>(List.map (fun (s, t) -> (s, t)) bs)

    { Current = { Bindings = bindings }
      Rest = [] }

let empty () = emptyWith []

let pushScope (bs: (string * 'a) list) { Current = c; Rest = r } : Env<'a> =
    { Current = { Bindings = Map(List.toArray bs) }
      Rest = (c :: r) }

let popScope =
    function
    | { Current = _; Rest = [] } as e -> e
    | { Current = _; Rest = top :: r } ->
        { Current = top
          Rest = r }

let add (string, item) env : Env<'a> =
    { env with
          Current =
              { env.Current with
                    Bindings = Map.add string item env.Current.Bindings } }

let addAll bindings env =
    List.fold (fun acc b -> add b acc) env bindings

let addToRoot (string, item) env : Env<'a> =
    if List.isEmpty env.Rest then
        add (string, item) env
    else
        let rest =
            List.mapi
                (fun i f ->
                    if i = env.Rest.Length - 1 then
                        { f with Bindings = Map.add string item f.Bindings }
                    else f)
                env.Rest

        { env with Rest = rest }

let update (string, item) env : Env<'a> =
    if env.Current.Bindings.ContainsKey(string) then
        { env with
              Current =
                  { env.Current with
                        Bindings = Map.add string item env.Current.Bindings } }
    else
        let r = env.Rest

        match List.tryFindIndex (fun frame -> Map.containsKey string frame.Bindings) r with
        | Some i ->
            let newF =
                { Bindings = Map.add string item (List.item i r).Bindings }

            { env with
                  Rest =
                      List.concat [ List.take i r
                                    [ newF ]
                                    List.skip (i + 1) r ] }
        | None -> env

let find (string: string) (env: Env<'a>) : 'a option =
    let getFromRest () : 'a option =
        let r = env.Rest

        match List.tryFindIndex (fun frame -> Map.containsKey string frame.Bindings) r with
        | Some i ->
            Map.tryFind string (List.item i r).Bindings
        | None ->
            None

    Map.tryFind string env.Current.Bindings
    |> Option.orElseWith getFromRest