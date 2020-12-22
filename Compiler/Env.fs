module Env

open ChakraParser

type Capability =
    | StdioCapability

type Type =
    | UnionType of Type list
    | SumType of Type list
    | StringType
    | NumberType
    | SymbolType
    | LiteralType of ChakraLiteral
    | TupleType of Type list
    | ListType of Type
    | MapType of (Type * Type)
    | StructType of (string * Type) list
    | FunctionType of (Type list * Type)
    | GenericType of string
    | UnknownType
    | CommandType
    | CapabilityType of Capability

let genericTypeVars =
    List.map string ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ])

type EnvBinding = { Typ: Type ; Expr: ChakraExpr }

/// <summary>
/// A stack of maps that can be searched for symbols.
/// </summary>
///
type Env = { Current: Map<string, Type> ; Rest: Map<string, Type> list }

let private num = NumberType
let private str = StringType
let private sym = SymbolType
let private lit l = LiteralType l
let private tup typs = TupleType typs
let private list t = ListType t
let private map k v = MapType (k, v)
let private strct fields = StructType fields
let private fn args retrn = FunctionType (args, retrn)
let private gen s = GenericType s
let private uk = UnknownType
let private union ts = UnionType ts
let private sum ts = SumType ts
let private cap c = CapabilityType c
let private cmd = CommandType

let private genA = gen "A"
let private genB = gen "B"
let private bool = union [lit (ChakraSymbol "true"); lit (ChakraSymbol "false")]
let private opt t = union [lit (ChakraSymbol "none"); tup [lit (ChakraSymbol "some"); t]]
let stdlib = new Map<string, Type>([
    ("/stdlib.add", fn [num; num] num)
    ("/stdlib.sub", fn [num; num] num)
    ("/stdlib.div", fn [num; num] num)
    ("/stdlib.mul", fn [num; num] num)
    ("/stdlib.math.pow", fn [num; num] num)
    ("/stdlib.math.floor", fn [num] num)
    ("/stdlib.math.ceil", fn [num] num)
    ("/stdlib.math.round", fn [num] num)
    ("/stdlib.eq?", fn [genA; genA] bool)
    ("/stdlib.neq?", fn [genA; genA] bool)
    ("/stdlib.gt?", fn [num; num] bool)
    ("/stdlib.lt?", fn [num; num] bool)
    ("/stdlib.string.starts-with?", fn [str; str] bool)
    ("/stdlib.string.ends-with?", fn [str; str] bool)
    ("/stdlib.string.contains?", fn [str; str] bool)
    ("/stdlib.string.substring", fn [num; str] str)
    ("/stdlib.string.join", fn [str; list str] str)
    ("/stdlib.map.has?", fn [genA; map genA genB] bool)
    ("/stdlib.map.get", fn [genA; map genA genB] (opt genB))
    ("/stdlib.map.set", fn [genA; genB; map genA genB] (map genA genB))
    ("/stdlib.map.keys", fn [map genA genB] (list genA))
    ("/stdlib.map.values", fn [map genA genB] (list genB))
    ("/stdlib.map.pairs", fn [map genA genB] (list (tup [genA; genB])))
    ("/stdlib.list.map", fn [fn [genA] genB; list genA] genB)
    ("/stdlib.list.append", fn [genA; list genA] (list genA))
    ("/stdlib.list.head", fn [list genA] (opt genA))
    ("/stdlib.list.tail", fn [list genA] (list genA))
    ("/stdlib.list.concat", fn [list (list genA)] (list genA))
    ("/stdlib.list.fold", fn [fn [genB; genA] genB; list (genA)] genB)
    ("/stdlib.io.print", fn [cap StdioCapability; str] (cmd))
])

let defaultEnv = { Current = stdlib ; Rest = []} 

let newScope (bs: (string * Type) list) { Current = c ; Rest = r} : Env =
    { Current = Map (List.toArray bs) ; Rest = (c::r) }

let popScope = function
    | { Current = _ ; Rest = [] } as e -> e
    | { Current = _ ; Rest = top::r} -> { Current = top ; Rest = r}

let getTypeForBinding string { Current = c ; Rest = r} =
    let rec innerFind = function
    | [] -> None
    | m::tail ->
        match Map.tryFind string m with
        | Some (b) -> Some (b)
        | None -> innerFind tail

    innerFind (c::r)

let addBinding string typ { Current = c ; Rest = r} : Env =
    { Current = Map.add string typ c
      Rest = r }

let mergeAs (prefix) env =
    match env with
    | { Current = _ ; Rest = [] } -> env
    | { Current = c ; Rest = (top::r) } ->
        let newC =
            Map.toList c
            |> List.map (fun (k, v) -> (prefix + k, v))
            |> List.append (Map.toList top)
            |> Map.ofList
         
        { Current = newC ; Rest = r }
        
    