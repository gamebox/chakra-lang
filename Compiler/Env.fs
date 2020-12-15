module Env

open ChakraParser

type Type =
    | UnionType of Type list
    | SumType of Type list
    | StringType
    | NumberType
    | SymbolType
    | LiteralType of ChakraLiteral
    | TupleType of Type list
    | ListType of Type
    | VectorType of Type
    | MapType of (Type * Type)
    | StructType of (string * Type) list
    | FunctionType of (Type list * Type)
    | GenericType of string
    | UnknownType

let genericTypeVars =
    List.map string ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ])
type EnvBinding = { Typ: Type ; Expr: ChakraExpr }
type Env = Collections.Map<string, Type>

let createEnv () = new Map<string, Type>([
    ("add", FunctionType ([NumberType; NumberType], NumberType))
])

let getTypeForBinding string (env: Env) = Map.tryFind string env

let addBinding string typ (env: Env) = Map.add string typ env

let merge (existing: Env) (n: Env) =
    let folder acc k v = addBinding k v acc
    Map.fold folder existing n