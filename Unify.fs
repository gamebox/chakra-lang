module Unify

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

type Env = Collections.Map<string, Type>

let createEnv () = new Map<string, Type>([])

let getTypeForBinding string (env: Env) = Map.tryFind string env

let addBinding string typ (env: Env) = Map.add string typ env

let rec print typ =
    match typ with
    | UnionType types -> sprintf "< %s >" (types |> List.map print |> String.concat " | ")

    | SumType types -> sprintf "< %s >" (types |> List.map print |> String.concat " + ")

    | StringType -> "string"

    | NumberType -> "number"

    | SymbolType -> "symbol"

    | LiteralType literal -> "literal"

    | TupleType types ->
        let typeList = List.map print types
        sprintf "( %s )" (typeList |> String.concat " ")

    | ListType genericType -> sprintf "[ %s ]" (print genericType)

    | VectorType genericType -> sprintf "{ %s }" (print genericType)

    | MapType (keyType, valueType) -> sprintf "[ %s = %s ]" (print keyType) (print valueType)

    | StructType fields ->
        sprintf
            "( %s )"
            (fields
             |> List.map (fun (name, typ) -> sprintf ".%s = %s" name (print typ))
             |> String.concat " ")

    | FunctionType (args, retrn) ->
        let argList =
            sprintf "( %s )" (args |> List.map print |> String.concat " ")

        sprintf "{ %s -> %s }" argList (print retrn)

    | GenericType typ -> sprintf "?%s" typ

    | UnknownType -> "???"

let gatherTypes acc item =
    if List.contains item acc then acc else item :: acc

let typeListToType types =
    if List.length types = 1 then List.head types else UnionType(List.sort types)

let rec exprType (env: Env) expr =
    match expr with
    | ChakraLiteralExpr (_, literal) -> literalType env literal
    | ChakraApplyExpr (_, (ChakraApply (ident, exprs))) ->
        let exprsTypes = exprs |> List.map (exprType env)

        FunctionType(exprsTypes, UnknownType)
    | ChakraMatchExpr (_, (ChakraMatch (matchingPattern, clauses))) ->
        clauses
        |> List.map (fun (ChakraMatchClause (pattern, exprList)) -> exprListType env exprList)
        |> List.fold gatherTypes []
        |> typeListToType


and reduceExprs env exprs =
    exprs
    |> List.map (exprType env)
    |> List.fold gatherTypes []
    |> typeListToType

and literalType env literal =
    match literal with
    | ChakraNumber _ -> NumberType
    | ChakraSymbol _ -> SymbolType
    | ChakraString _ -> StringType
    | ChakraTuple items -> TupleType(List.map (exprType env) items)
    | ChakraStruct fields ->
        fields
        |> List.map (fun (label, expr) -> (label, exprType env expr))
        |> StructType

    | ChakraList items -> items |> reduceExprs env |> ListType

    | ChakraMap entries ->
        let (keyTypes, valueTypes) =
            entries
            |> List.fold (fun (keys, values) (key, value) ->
                let keyType = literalType env key
                let valueType = exprType env value
                (keyType :: keys, valueType :: values)) ([], [])

        MapType
            (keyTypes
             |> List.fold gatherTypes []
             |> typeListToType,
             valueTypes
             |> List.fold gatherTypes []
             |> typeListToType)

    | ChakraVector items -> items |> reduceExprs env |> VectorType

    | ChakraVar name ->
        match getTypeForBinding name env with
        | Some t -> t
        | _ -> UnknownType

    | ChakraLambda (args, body) ->
        let argTypes =
            args
            |> List.indexed
            |> List.map (fun (idx, _) -> GenericType genericTypeVars.[idx])

        FunctionType(argTypes, (exprListType env body))

and exprListType env (ChakraExprList (bindings, expr)) =
    let reduceBindings =
        fun env1 (ChakraBinding (_, pattern, exprList)) ->
            match pattern with
            | ChakraSimpleBindingPattern name -> addBinding name (exprListType env1 exprList) env1
            | _ -> env1

    bindings
    |> List.fold reduceBindings env
    |> (fun env1 -> exprType env1 expr)

and bindingType (env: Env) (ChakraBinding (_, pattern, exprList)) = exprListType env exprList


(**************************************************************************************************
**
**    TESTS
**
**************************************************************************************************)

let literalTypeTests () =
    let literalTypeTest (label, input, typ) =
        (ParserLibrary.run chakraLiteral input)
        |> (fun result ->
            match result with
            | ParserLibrary.Success (literal, _) ->
                let resultType = literalType (createEnv ()) literal
                if resultType = typ
                then printfn "%s : %s" input (print resultType)
                else printfn "FAILED: %s\n%s <> %s" label (print resultType) (print typ)
            | _ ->
                printfn "FAILED: Could not parse '%s'" input
                ParserLibrary.printResult result)

    [ ("A number", "1", NumberType)
      ("A string", "\"\"", StringType)
      ("A symbol", "#one", SymbolType)
      ("A simple tuple", "( 1 2 )", TupleType [ NumberType; NumberType ])
      ("A simple list", "[ 1 2 3 ]", ListType NumberType)
      ("A complex list",
       "[ 1 #two \"three\" ]",
       ListType
           (UnionType [ StringType
                        NumberType
                        SymbolType ]))
      ("A simple vector", "{ 1 2 3 }", VectorType NumberType)
      ("A complex list",
       "{ 1 #two \"three\" }",
       VectorType
           (UnionType [ StringType
                        NumberType
                        SymbolType ]))
      ("A simple struct",
       "( something = 1 else = #two )",
       StructType [ ("something", NumberType)
                    ("else", SymbolType) ])
      ("A simple map", """[ "something" = 1 "else" = 2 ]""", MapType(StringType, NumberType))
      ("A list of structs",
       "[ ( something = 1 else = #two ) ( something = 1 else = #two ) ( something = 1 else = #two ) ]",
       ListType
           (StructType [ ("something", NumberType)
                         ("else", SymbolType) ])) ]
    |> List.map literalTypeTest
