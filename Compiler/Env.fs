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
    | FunctionType of args: Type list * ret: Type
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


type TypeError = TypeError of string

let genericTypeVars =
    List.map string ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ])

type BindingType =
    | Typed of Type
    | Untyped
    | Errored of TypeError

type Frame =
    { Bindings: Map<string, BindingType>
      Generic: int }

/// <summary>
/// A stack of maps that can be searched for symbols.
/// </summary>
///
type Env = { Current: Frame; Rest: Frame list }


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
let bool = union [ gSym "true"; gSym "false" ]

let opt t =
    union [ lit (ChakraSymbol "none")
            tup [ lit (ChakraSymbol "some"); t ] ]



let stdlib =
    new Map<string, BindingType>(
        List.map
            (fun (s, t) -> (s, Typed t))
            [ ("/stdlib.add", fn [ num; num ] num)
              ("/stdlib.sub", fn [ num; num ] num)
              ("/stdlib.div", fn [ num; num ] num)
              ("/stdlib.mul", fn [ num; num ] num)
              ("/stdlib.math.pow", fn [ num; num ] num)
              ("/stdlib.math.floor", fn [ num ] num)
              ("/stdlib.math.ceil", fn [ num ] num)
              ("/stdlib.math.round", fn [ num ] num)
              ("/stdlib.eq?", fn [ genA; genA ] bool)
              ("/stdlib.neq?", fn [ genA; genA ] bool)
              ("/stdlib.gt?", fn [ num; num ] bool)
              ("/stdlib.lt?", fn [ num; num ] bool)
              ("/stdlib.string.starts-with?", fn [ str; str ] bool)
              ("/stdlib.string.ends-with?", fn [ str; str ] bool)
              ("/stdlib.string.contains?", fn [ str; str ] bool)
              ("/stdlib.string.substring", fn [ num; str ] str)
              ("/stdlib.string.join", fn [ str; list str ] str)
              ("/stdlib.map.has?", fn [ genA; map genA genB ] bool)
              ("/stdlib.map.get", fn [ genA; map genA genB ] (opt genB))
              ("/stdlib.map.set", fn [ genA; genB; map genA genB ] (map genA genB))
              ("/stdlib.map.keys", fn [ map genA genB ] (list genA))
              ("/stdlib.map.values", fn [ map genA genB ] (list genB))
              ("/stdlib.map.pairs", fn [ map genA genB ] (list (tup [ genA; genB ])))
              ("/stdlib.list.map", fn [ fn [ genA ] genB; list genA ] genB)
              ("/stdlib.list.append", fn [ genA; list genA ] (list genA))
              ("/stdlib.list.head", fn [ list genA ] (opt genA))
              ("/stdlib.list.tail", fn [ list genA ] (list genA))
              ("/stdlib.list.concat", fn [ list (list genA) ] (list genA))
              ("/stdlib.list.fold", fn [ fn [ genB; genA ] genB; list (genA) ] genB)
              ("/stdlib.io.print", fn [ cap StdioCapability; str ] (cmd)) ]
    )

let emptyWith bs =
    let bindings =
        new Map<string, BindingType>(List.map (fun (s, t) -> (s, Typed t)) bs)

    { Current = { Bindings = bindings; Generic = 0 }
      Rest = [] }

let defaultEnv =
    { Current = { Bindings = stdlib; Generic = 0 }
      Rest = [] }

let typedBinding (str: string) ty = (str, Typed ty)

let untypedBinding (str: string) = (str, Untyped)

let newScope (bs: (string * BindingType) list) { Current = c; Rest = r }: Env =
    { Current =
          { Bindings = Map(List.toArray bs)
            Generic = 0 }
      Rest = (c :: r) }

let popScope =
    function
    | { Current = _; Rest = [] } as e -> e
    | { Current = _; Rest = top :: r } -> { Current = top; Rest = r }

let getTypeForBinding string { Current = c; Rest = r } =
    let rec innerFind =
        function
        | [] -> None
        | m :: tail ->
            match Map.tryFind string m with
            | Some (b) -> Some(b)
            | None -> innerFind tail

    innerFind (c.Bindings :: (List.map (fun f -> f.Bindings) r))

let addBinding (string, typ) { Current = c; Rest = r }: Env =
    { Current =
          { Bindings = Map.add string typ c.Bindings
            Generic = c.Generic }
      Rest = r }

let mergeAs (prefix) env =
    match env with
    | { Current = _; Rest = [] } -> env
    | { Current = c; Rest = (top :: r) } ->
        let newC =
            Map.toList c.Bindings
            |> List.map (fun (k, v) -> (prefix + k, v))
            |> List.append (Map.toList top.Bindings)
            |> Map.ofList

        { Current = { Bindings = newC; Generic = c.Generic }
          Rest = r }

let updateBinding (string, typ) { Current = c; Rest = r }: Env =
    if c.Bindings.ContainsKey(string) then
        { Current = 
            { Bindings = Map.add string typ c.Bindings
              Generic = c.Generic }
          Rest = r }
    else
        match List.tryFindIndex (fun frame -> Map.containsKey string frame.Bindings) r with
        | Some i ->
            let newF = { Bindings = Map.add string typ (List.item i r).Bindings ; Generic = c.Generic}
            { Current = c
              Rest = List.concat [List.take i r ; [newF]; List.skip (i + 1) r]}
        | None ->
            { Current = c; Rest = r }