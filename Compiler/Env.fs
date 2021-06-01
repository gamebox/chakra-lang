module Env

open AST
open TypeSystem
// Placeholder

type TypeError =
    | FatalTypeError of string
    | UntypedError of varName: string
    | FeatureNotSupported
    | NonFunctionApplication of bindingName: string * typ: Type
    | UndefinedBinding of string
    | ArgumentMismatch of argT: Type * paramT: Type
    | IllegalFieldAccess of binding: string * typ: Type
    | UnifyError of types: Type list
    | ModuleNotFound of moduleName: string
    | ExportsMissing of missingExports: string list
    | PatternMismatch of pattern: ChakraPattern * typ: Type

    member x.IsUntyped =
        match x with
        | UntypedError _ -> true
        | _ -> false

type BindingType =
    | Typed of Type
    | Untyped
    | Errored of TypeError

type Frame = { Bindings: Map<string, BindingType> }

/// <summary>
/// A stack of maps that can be searched for symbols.
/// </summary>
///
type Env =
    { Current: Frame
      Rest: Frame list
      Filename: string }

let stdlibModule fields = strct (fields, false, None)
let stdlibMath =
    stdlibModule [
              ("pow", fn [ "a", num; "b", num ] num)
              ("floor", fn [ "a", num ] num)
              ("ceil", fn [ "a", num ] num)
              ("round", fn [ "a", num ] num) 
            ]

let stdlibString =
    stdlibModule [
                ("starts-with?", fn [ "query", str; "string", str ] bool)
                ("ends-with?", fn [ "query", str; "string", str ] bool)
                ("contains?", fn [ "query", str; "string", str ] bool)
                ("substring",
                    fn
                       [ "start", num
                         "end", num
                         "string", str ]
                       str)
                ("join",
                    fn
                       [ "separator", str
                         "strings", list str ]
                       str)
            ]

let stdlibMap =
    stdlibModule [
      ("has?", fn [ "key", genA; "map", map genA genB ] bool)
      ("get", fn [ "key", genA; "map", map genA genB ] (opt genB))
      ("set",
       fn
           [ "key", genA
             "value", genB
             "map", map genA genB ]
           (map genA genB))
      ("keys", fn [ "map", map genA genB ] (list genA))
      ("values", fn [ "map", map genA genB ] (list genB))
      ("pairs", fn [ "map", map genA genB ] (list (tup [ genA; genB ])))
    ]

let stdlibList =
    stdlibModule [
      ("map",
       fn
           [ "fn", fn [ "item", genA ] genB
             "list", list genA ]
           genB)
      ("append", fn [ "item", genA; "list", list genA ] (list genA))
      ("head", fn [ "list", list genA ] (opt genA))
      ("tail", fn [ "list", list genA ] (list genA))
      ("concat", fn [ "lists", list (list genA) ] (list genA))
      ("fold",
       fn
           [ "fn", fn [ "state", genB; "item", genA ] genB
             "list", list (genA) ]
           genB)
    ]

let stdlibIo =
    stdlibModule [
      ("print",
       fn
           [ "cap", cap StdioCapability
             "str", str ]
           (cmd))
    ]

let stdlibExports =
    [ ("add", fn [ "a", num; "b", num ] num)
      ("sub", fn [ "a", num; "b", num ] num)
      ("div", fn [ "a", num; "b", num ] num)
      ("mul", fn [ "a", num; "b", num ] num)
      ("eq?", fn [ "a", genA; "b", genA ] bool)
      ("neq?", fn [ "a", genA; "b", genA ] bool)
      ("gt?", fn [ "a", num; "b", num ] bool)
      ("lt?", fn [ "a", num; "b", num ] bool)
      ("math", stdlibMath)
      ("string", stdlibString)
      ("map", stdlibMap)
      ("list", stdlibList)
      ("io", stdlibIo)
    ]

let stdlib =
    stdlibExports
    |> List.map (fun (s, t) -> (s, Typed t))
    |> Map<string, BindingType>

let emptyWith bs =
    let bindings =
        new Map<string, BindingType>(List.map (fun (s, t) -> (s, Typed t)) bs)

    { Current = { Bindings = bindings }
      Rest = []
      Filename = "" }

let defaultEnv =
    { Current = { Bindings = stdlib }
      Rest = []
      Filename = "" }

let typedBinding (str: string) ty = (str, Typed ty)

let untypedBinding (str: string) = (str, Untyped)

let newScope (bs: (string * BindingType) list) { Current = c; Rest = r } : Env =
    { Current = { Bindings = Map(List.toArray bs) }
      Rest = (c :: r)
      Filename = "" }

let popScope =
    function
    | { Current = _; Rest = [] } as e -> e
    | { Current = _; Rest = top :: r } ->
        { Current = top
          Rest = r
          Filename = "" }

let getTypeForBinding string { Current = c; Rest = r } =
    let rec innerFind =
        function
        | [] -> None
        | m :: tail ->
            match Map.tryFind string m with
            | Some (b) -> Some(b)
            | None -> innerFind tail

    innerFind (c.Bindings :: (List.map (fun f -> f.Bindings) r))

let hasTypedBinding env string =
    match getTypeForBinding string env with
    | Some (Typed _) -> true
    | _ -> false

let addBinding (string, typ) env : Env =
    { env with
          Current =
              { env.Current with
                    Bindings = Map.add string typ env.Current.Bindings } }

let addBindings bindings env =
    List.fold (fun acc b -> addBinding b acc) env bindings

let mergeAs (prefix) env =
    match env with
    | { Current = _; Rest = [] } -> env
    | { Current = c; Rest = (top :: r) } ->
        let newC =
            Map.toList c.Bindings
            |> List.map (fun (k, v) -> (prefix + k, v))
            |> List.append (Map.toList top.Bindings)
            |> Map.ofList

        { env with
              Current = { Bindings = newC }
              Rest = r }

let updateBinding (string, typ) env : Env =
    if env.Current.Bindings.ContainsKey(string) then
        { env with
              Current =
                  { env.Current with
                        Bindings = Map.add string typ env.Current.Bindings } }
    else
        let r = env.Rest

        match List.tryFindIndex (fun frame -> Map.containsKey string frame.Bindings) r with
        | Some i ->
            let newF =
                { Bindings = Map.add string typ (List.item i r).Bindings }

            { env with
                  Rest =
                      List.concat [ List.take i r
                                    [ newF ]
                                    List.skip (i + 1) r ] }
        | None -> env

let collectTypedBindings (name, b) =
    match b with
    | Typed t -> (name, t)
    | _ -> failwith "This should never happen"

let lowerIntoStruct { Current = c } =

    strct ((List.map collectTypedBindings (Map.toList c.Bindings)), false, None)
