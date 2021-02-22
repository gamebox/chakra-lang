module Env

open ChakraParser

type Capability =
    | StdioCapability
    | FileReadCapability
    | FileWriteCapability

type StdioFD =
    | Stdin
    | Stdout
    | Stderr

type ActorRef<'msg> = Actor of 'msg
// type Cmd =
//     | StdioPrintCommand
//     | SendCommand
//     | TimeoutCommand
//     | IntervalCommand
//     | FileOpenCommand
//     | FileReadCommand
//     | FileWriteCommand
//     | StdioReadCommand
//     | GetRandomCommand
//     | GetDateCommand

type SymbolInfo =
    | GlobalSymbol of string
    | ModuleSymbol of mod': string * name: string

type Type =
    // Concrete Types
    | StringType
    | NumberType
    | SymbolType of SymbolInfo
    | LiteralType of ChakraLiteral
    | TupleType of Type list
    | ListType of Type
    | MapType of key: Type * value: Type
    | StructType of fields: (string * Type) list * isOpen: bool * tag: SymbolInfo option
    | FunctionType of args: (string * Type) list * ret: Type
    // Opaque or Compiler types
    | CommandType // A command to the system
    | CapabilityType of Capability // A capability to perform an action
    | RefType of Type // A reference to an Actor that takes a msg of Type
    // ADT
    | GenericType of string // A type variable
    | UnionType of Type list // One or more different type
    | SumType of Type list // Multiple different types
    | PolymorphicType of string // Typeclass membership
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

type Frame =
    { Bindings: Map<string, BindingType> }

/// <summary>
/// A stack of maps that can be searched for symbols.
/// </summary>
///
type Env = { Current: Frame; Rest: Frame list; Filename: string }


(*
    Type Helpers
*)
let num = NumberType

let str = StringType
let gSym = SymbolType << GlobalSymbol
let mSym = SymbolType << ModuleSymbol
let lit = LiteralType
let tup = TupleType
let list = ListType
let map k v = MapType(k, v)
let strct = StructType
let fn args retrn = FunctionType(args, retrn)
let gen = GenericType
let union = UnionType
let sum = SumType
let cap = CapabilityType
let cmd = CommandType

let genA = gen "a"
let genB = gen "b"
let nextGen (prev: string) = (sprintf "%c" (char (int (prev.Chars 0) + 1)))
let bool = union [ gSym "true"; gSym "false" ]

let opt t =
    union [ lit (ChakraSymbol "none")
            tup [ lit (ChakraSymbol "some"); t ] ]



let stdlib =
    new Map<string, BindingType>(
        List.map
            (fun (s, t) -> (s, Typed t))
            [ ("/stdlib.add", fn [ "a", num; "b", num ] num)
              ("/stdlib.sub", fn [ "a", num; "b", num ] num)
              ("/stdlib.div", fn [ "a", num; "b", num ] num)
              ("/stdlib.mul", fn [ "a", num; "b", num ] num)
              ("/stdlib.math.pow", fn [ "a", num; "b", num ] num)
              ("/stdlib.math.floor", fn [ "a", num ] num)
              ("/stdlib.math.ceil", fn [ "a", num ] num)
              ("/stdlib.math.round", fn [ "a", num ] num)
              ("/stdlib.eq?", fn [ "a", genA; "b", genA ] bool)
              ("/stdlib.neq?", fn [ "a", genA; "b", genA ] bool)
              ("/stdlib.gt?", fn [ "a", num; "b", num ] bool)
              ("/stdlib.lt?", fn [ "a", num; "b", num ] bool)
              ("/stdlib.string.starts-with?", fn [ "query", str; "string", str ] bool)
              ("/stdlib.string.ends-with?", fn [ "query", str; "string", str ] bool)
              ("/stdlib.string.contains?", fn [ "query", str; "string", str ] bool)
              ("/stdlib.string.substring", fn [ "start", num; "end", num; "string", str ] str)
              ("/stdlib.string.join", fn [ "separator", str; "strings", list str ] str)
              ("/stdlib.map.has?", fn [ "key", genA; "map", map genA genB ] bool)
              ("/stdlib.map.get", fn [ "key", genA; "map", map genA genB ] (opt genB))
              ("/stdlib.map.set", fn [ "key", genA; "value", genB; "map", map genA genB ] (map genA genB))
              ("/stdlib.map.keys", fn [ "map", map genA genB ] (list genA))
              ("/stdlib.map.values", fn [ "map", map genA genB ] (list genB))
              ("/stdlib.map.pairs", fn [ "map", map genA genB ] (list (tup [ genA; genB ])))
              ("/stdlib.list.map", fn [ "fn", fn [ "item", genA ] genB; "list", list genA ] genB)
              ("/stdlib.list.append", fn [ "item", genA; "list", list genA ] (list genA))
              ("/stdlib.list.head", fn [ "list", list genA ] (opt genA))
              ("/stdlib.list.tail", fn [ "list", list genA ] (list genA))
              ("/stdlib.list.concat", fn [ "lists", list (list genA) ] (list genA))
              ("/stdlib.list.fold", fn [ "fn", fn [ "state", genB; "item", genA ] genB; "list", list (genA) ] genB)
              ("/stdlib.io.print", fn [ "cap", cap StdioCapability; "str", str ] (cmd)) ]
    )

let emptyWith bs =
    let bindings =
        new Map<string, BindingType>(List.map (fun (s, t) -> (s, Typed t)) bs)

    { Current = { Bindings = bindings }
      Rest = []
      Filename = "" }

let defaultEnv =
    { Current = { Bindings = stdlib  }
      Rest = []
      Filename = "" }

let typedBinding (str: string) ty = (str, Typed ty)

let untypedBinding (str: string) = (str, Untyped)

let newScope (bs: (string * BindingType) list) { Current = c; Rest = r }: Env =
    { Current = { Bindings = Map(List.toArray bs)  }
      Rest = (c :: r)
      Filename = "" }

let popScope =
    function
    | { Current = _; Rest = [] } as e -> e
    | { Current = _; Rest = top :: r } -> { Current = top; Rest = r; Filename = ""  }

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

let addBinding (string, typ) env: Env =
    { env with Current = { env.Current with Bindings = Map.add string typ env.Current.Bindings } }

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
            Rest = r}

let updateBinding (string, typ) env: Env =
    if env.Current.Bindings.ContainsKey(string) then
        { env with Current = { env.Current with Bindings = Map.add string typ env.Current.Bindings } }
    else
        let r = env.Rest
        match List.tryFindIndex (fun frame -> Map.containsKey string frame.Bindings) r with
        | Some i ->
            let newF = { Bindings = Map.add string typ (List.item i r).Bindings }
            { env with
                Rest = List.concat [List.take i r ; [newF]; List.skip (i + 1) r] }
        | None -> env


let createModuleSymbol { Filename = f } str = ModuleSymbol (f, str)

let lowerIntoStruct { Current = c} =
    let collectTypedBindings (name, b) =
        match b with
        | Typed t -> (name, t)
        | _ -> failwith "This should never happen"

    strct ((List.map collectTypedBindings (Map.toList c.Bindings)), false ,None)