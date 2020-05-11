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
    | FunctionType of ((Type list) * Type)
    | UnknownType

let rec print typ =
    match typ with
    | UnionType types ->
        sprintf "< %s >"
            (types
            |> List.map print
            |> String.concat " | ")

    | SumType types ->
        sprintf "< %s >"
            (types
            |> List.map print
            |> String.concat " + ")

    | StringType ->
        "string"

    | NumberType ->
        "number"

    | SymbolType ->
        "symbol"

    | LiteralType literal ->
        "literal"

    | TupleType types ->
        let typeList = List.map print types
        sprintf
            "( %s )"
            (typeList
            |> String.concat " ")

    | ListType genericType ->
        sprintf "[ %s ]" (print genericType)

    | VectorType genericType ->
        sprintf "{ %s }" (print genericType)

    | MapType (keyType, valueType) ->
        sprintf "[ %s = %s ]" (print keyType) (print valueType)

    | StructType fields ->
        sprintf "( %s )"
            (fields
            |> List.map (fun (name, typ) ->
                sprintf "%s = %s" name (print typ))
            |> String.concat " ")

    | FunctionType (args, retrn) ->
        let argList =
            sprintf "( %s )"
                (args
                |> List.map print
                |> String.concat " ")
        sprintf "{ %s -> %s}"
            argList
            (print retrn)

    | UnknownType ->
        "???"

let gatherTypes acc item = 
    if List.contains item acc then
        acc
    else item::acc

let typeListToType types =
    if List.length types = 1 then
        List.head types
    else
        UnionType (List.sort types)

let rec exprType expr =
    match expr with
    | ChakraLiteralExpr literal->
        literalType literal
    | _ -> UnknownType


and reduceExprs exprs =
    exprs
        |> List.map exprType
        |> List.fold gatherTypes []
        |> typeListToType

and literalType literal =
    match literal with
        | ChakraNumber _ -> NumberType
        | ChakraSymbol _ -> SymbolType
        | ChakraString _ -> StringType
        | ChakraTuple items -> TupleType (List.map (fun item -> 
            match item with
            | ChakraLiteralExpr literal ->
                literalType literal
            | _ -> UnknownType) items)

        | ChakraStruct fields ->
            fields
            |> List.map (fun (label, expr) ->
                (label, exprType expr))
            |> StructType

        | ChakraList items ->
            items
            |> reduceExprs
            |> ListType

        | ChakraMap entries ->
            let (keyTypes, valueTypes) = 
                entries
                |> List.fold (fun (keys, values) (key, value) ->
                    let keyType = literalType key
                    let valueType = exprType value
                    (keyType::keys, valueType::values))
                    ([], [])
            
            MapType
                (
                    keyTypes
                    |> List.fold gatherTypes []
                    |> typeListToType,
                    valueTypes
                    |> List.fold gatherTypes []
                    |> typeListToType
                )

        | ChakraVector items ->
            items
            |> reduceExprs
            |> VectorType
            
        | _ -> UnknownType

and exprListType (ChakraBinding (list, ChakraExprList (bindings, expr))) =
    match expr with
    | ChakraLiteralExpr literal ->
        literalType literal
    | _ -> UnknownType


(**************************************************************************************************
**
**    TESTS
**
**************************************************************************************************)

let literalTypeTests () =
    let literalTypeTest (label, input, typ) =
        (ParserLibrary.run chakraLiteral input)
        |> (fun (result) ->
            match result with
            | ParserLibrary.Success (literal, _) ->
                let resultType = literalType literal
                if resultType = typ then 
                    printfn "%s : %s" input (print resultType)
                else
                    printfn "FAILED: %s\n%s <> %s" label (print resultType) (print typ)
            | _ ->
                printfn "FAILED: Could not parse '%s'" input
                ParserLibrary.printResult result)

    [
        ("A number", "1", NumberType)
        ("A string", "\"\"", StringType)
        ("A symbol", "#one", SymbolType)
        ("A simple tuple", "( 1 2 )", TupleType [NumberType; NumberType])
        ("A simple list", "[ 1 2 3 ]", ListType NumberType)
        ("A complex list", "[ 1 #two \"three\" ]", ListType (UnionType [StringType; NumberType; SymbolType]))
        ("A simple struct", "( something = 1 else = #two )", StructType [("something", NumberType); ("else", SymbolType)])

    ]
    |> List.map literalTypeTest
