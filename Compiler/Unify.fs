module Unify

open ChakraParser
open Env

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
    | ChakraApplyExpr (_, (ChakraApply ((ident, _), exprs))) ->
        let fType = Option.defaultValue UnknownType (getTypeForBinding ident env)
        match fType with
        | FunctionType (_, t) -> t
        | _ -> fType
    | ChakraMatchExpr (_, (ChakraMatch (matchingPattern, clauses))) ->
        clauses
        |> List.map (fun (ChakraMatchClause (pattern, exprList)) -> exprListType env exprList)
        |> List.fold gatherTypes []
        |> typeListToType
    | ChakraNativeExpr (_) -> UnknownType


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
    | ChakraStruct  { Fields = fields } ->
        fields
        |> List.map (fun field-> (field.Name, exprType env field.Value))
        |> StructType

    | ChakraList { Items = items } -> items |> reduceExprs env |> ListType

    | ChakraMap { Pairs = entries } ->
        let (keyTypes, valueTypes) =
            entries
            |> List.fold (fun (keys, values) ({ Key = key ; Value = value}) ->
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

    | ChakraVar (name, fields) ->
        match getTypeForBinding name env with
        | Some t -> t
        | _ -> UnknownType

    | ChakraLambda c ->
        let foldLambda (args, e: Env) (idx, a) =
            let argType = GenericType genericTypeVars.[idx]
            let args' = argType :: args
            let env' = addBinding a argType e
            (args', env')

        let (argTypes, newEnv) =
            c.Args
            |> List.indexed
            |> List.fold foldLambda ([], env)

        FunctionType(List.rev argTypes, (exprListType newEnv c.Body))

and exprListType env (ChakraExprList (bindings, expr)) =
    let reduceBindings =
        fun env1 ({ ExprList = exprList ; Pattern = pattern }) ->
            match pattern with
            | ChakraSimpleBindingPattern name -> addBinding name (exprListType env1 exprList) env1
            | _ -> env1

    bindings
    |> List.fold reduceBindings env
    |> (fun env1 -> exprType env1 expr)

and bindingType (env: Env) ({ ExprList = exprList ; Pattern = pattern }) = exprListType env exprList


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
