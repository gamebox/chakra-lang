module Unify

open ChakraParser
open Env


let rec print typ =
    match typ with
    | UnionType types -> sprintf "< %s >" (types |> List.map print |> String.concat " | ")

    | SumType types -> sprintf "< %s >" (types |> List.map print |> String.concat " + ")

    | StringType -> "str"

    | NumberType -> "num"

    | SymbolType info ->
        match info with
        | GlobalSymbol s -> sprintf "#%s" s
        | ModuleSymbol (m, s) -> sprintf "#%s/%s" m s

    | LiteralType literal -> "literal"

    | TupleType types ->
        let typeList = List.map print types
        sprintf "( %s )" (typeList |> String.concat ", ")

    | ListType genericType -> sprintf "[ %s ]" (print genericType)

    | MapType (keyType, valueType) -> sprintf "%%[ %s = %s ]" (print keyType) (print valueType)

    | StructType (fields, isOpen, tag) ->
        sprintf
            "%%( %s %s)"
            (fields
             |> List.map (fun (name, typ) -> sprintf ".%s = %s" name (print typ))
             |> String.concat ", ")
            (if isOpen then "..." else "")

    | FunctionType (args, retrn) ->
        let argList =
            sprintf "( %s )" (args |> List.map print |> String.concat ", ")

        sprintf "{ %s -> %s }" argList (print retrn)

    | GenericType typ -> sprintf "?%s" typ

    | CapabilityType cap ->
        match cap with
        | StdioCapability -> "$stdio"
        | FileReadCapability -> "$fread"
        | FileWriteCapability -> "#fwrite"

    | CommandType -> "!"

    | PolymorphicType t -> sprintf "@%s" t

    | RefType t -> sprintf "&%s" (print t)

// let gatherTypes acc item =
//     if List.contains item acc then acc else item :: acc

// let typeListToType types =
//     if List.length types = 1 then List.head types else UnionType(List.sort types)

// let rec exprType (env: Env) expr =
//     match expr with
//     | ChakraLiteralExpr (_, literal) -> literalType env literal
//     | ChakraApplyExpr (_, (ChakraApply ((ident, _), exprs))) ->
//         let fType = Option.defaultValue UnknownType (getTypeForBinding ident env)
//         match fType with
//         | FunctionType (_, t) -> t
//         | _ -> fType
//     | ChakraMatchExpr (_, (ChakraMatch (matchingPattern, clauses))) ->
//         clauses
//         |> List.map (fun (ChakraMatchClause (pattern, exprList)) -> exprListType env exprList)
//         |> List.fold gatherTypes []
//         |> typeListToType
//     | ChakraNativeExpr (_) -> UnknownType

// and reduceExprs env exprs =
//     exprs
//     |> List.map (exprType env)
//     |> List.fold gatherTypes []
//     |> typeListToType

// and literalType env literal =
//     match literal with
//     | ChakraNumber _ -> NumberType
//     | ChakraSymbol s -> SymbolType s
//     | ChakraString _ -> StringType
//     | ChakraTuple items -> TupleType(List.map (exprType env) items)
//     | ChakraStruct  { Fields = fields } ->
//         fields
//         |> List.map (fun field-> (field.Name, exprType env field.Value))
//         |> StructType

//     | ChakraList { Items = items } -> items |> reduceExprs env |> ListType

//     | ChakraMap { Pairs = entries } ->
//         let (keyTypes, valueTypes) =
//             entries
//             |> List.fold (fun (keys, values) ({ Key = key ; Value = value}) ->
//                 let keyType = literalType env key
//                 let valueType = exprType env value
//                 (keyType :: keys, valueType :: values)) ([], [])

//         MapType
//             (keyTypes
//              |> List.fold gatherTypes []
//              |> typeListToType,
//              valueTypes
//              |> List.fold gatherTypes []
//              |> typeListToType)

//     | ChakraVar (name, fields) ->
//         match getTypeForBinding name env with
//         | Some t -> t
//         | _ -> UnknownType

//     | ChakraLambda c ->
//         let foldLambda (args, e: Env) (idx, a) =
//             let argType = GenericType genericTypeVars.[idx]
//             let args' = argType :: args
//             let env' = addBinding a argType e
//             (args', env')

//         let (argTypes, newEnv) =
//             c.Args
//             |> List.indexed
//             |> List.fold foldLambda ([], env)

//         FunctionType(List.rev argTypes, (exprListType newEnv c.Body))

// and exprListType env (ChakraExprList (bindings, expr)) =
//     let reduceBindings =
//         fun env1 ({ ExprList = exprList ; Pattern = pattern }) ->
//             match pattern with
//             | ChakraSimpleBindingPattern name -> addBinding name (exprListType env1 exprList) env1
//             | _ -> env1

//     bindings
//     |> List.fold reduceBindings env
//     |> (fun env1 -> exprType env1 expr)

let invert (rs: Result<'a, 'b> list): Result<'a list, 'b> =
    let inner r acc =
        match r with
        | Ok a -> Result.map (fun as' -> a::as') acc
        | Error e -> Error e
    List.foldBack inner rs (Ok [])

let rec unify (callerType: Type) (calleeType: Type) : Type option =
    if callerType = calleeType then
        Some callerType
    else
        match (callerType, calleeType) with
        | (_, UnionType ts) when List.contains callerType ts ->
            Some callerType
        | (GenericType t, _) ->
            Some calleeType
        | (TupleType callerTs, TupleType calleeTs) when List.length callerTs = List.length calleeTs ->
           Option.map TupleType (unifyAll callerTs calleeTs)
        | (ListType callerT, ListType calleeT) ->
            Option.map ListType (unify callerT calleeT)
        | (MapType (callerKT, callerVT), MapType (calleeKT, calleeVT)) ->
            match (unify callerKT calleeKT, unify callerVT calleeVT) with
            | (Some t1, Some t2) -> Some (MapType (t1, t2))
            | _ -> None
        | (StructType (callerFsT, callerIsOpen, callerTag), StructType (calleeFsT, calleeIsOpen, calleeTag)) when List.length callerFsT = List.length calleeFsT ->
            match (unifyAllFields callerFsT calleeFsT, callerTag = calleeTag) with
            | (Some t1, true) -> Some (StructType (t1, callerIsOpen, callerTag))
            | _ -> None
        | (FunctionType (callerArgsT, callerRetT), FunctionType (calleeArgsT, calleeRetT)) when List.length callerArgsT = List.length calleeArgsT ->
            match (unifyAll callerArgsT calleeArgsT, unify callerRetT calleeRetT) with
            | (Some t1, Some t2) -> Some (FunctionType (t1, t2))
            | _ -> None
        | (RefType callerT, RefType calleeT) ->
            Option.map RefType (unify callerT calleeT)
        | (_, GenericType _) ->
            Some callerType
        | (_, SumType _) ->
            None
        | (_, PolymorphicType _) ->
            None
        | _ -> None

and unifyAll callerTypes calleeTypes : (Type list) option =
    let zipped = List.zip callerTypes calleeTypes
    List.foldBack collectTypeResults zipped (Some [])

and collectTypeResults (callerT, calleeT) acc : (Type list) option =
        match acc with
        | Some ts ->
            match unify callerT calleeT with
            | Some t -> Some (t::ts)
            | None -> None
        | _ -> acc

and reduceTypeResults (t: Type) (acc: Type option) : Type option =
        match acc with
        | Some t' ->
            match unify t t' with
            | Some t'' -> Some t''
            | None -> None
        | _ -> acc

and unifyAllFields callerTypes calleeTypes : ((string * Type) list) option =
    let collectTypeResults ((callerName, callerT), (calleeName, calleeT)) acc : ((string * Type) list) option =
        match acc with
        | Some ts ->
            match (callerName = calleeName, unify callerT calleeT) with
            | (true, Some t) -> Some ((calleeName, t)::ts)
            | (false, Some t) -> None
            | (_, None) -> None
        | _ -> acc

    let zipped = List.zip callerTypes calleeTypes
    List.foldBack collectTypeResults zipped (Some [])

and literalType (env: Env) (lit: ChakraLiteral) =
    match lit with
    | ChakraNumber _ -> Ok(env, NumberType)
    | ChakraString _ -> Ok(env, StringType)
    | ChakraSymbol sym -> Ok(env, SymbolType (GlobalSymbol sym)) // TODO: Module symbol
    | ChakraTuple exprs ->
        collectExprTypes exprs (env, [])
        |> Result.map (fun (e, ts) -> (e, TupleType (List.rev ts)))
        
    | ChakraStruct strct ->
        let typedFields : Result<(string * Type) list, TypeError> =
            List.foldBack
                (fun (f: ChakraStructField) acc ->
                    match acc with
                    | Ok fs -> 
                        match exprType env f.Value with
                        | Ok (_, t) -> Ok ((f.Name, t)::fs)
                        | Error e -> Error e
                    | _ -> acc)
                strct.Fields
                (Ok [])

        Result.map (fun fs -> (env, StructType (fs, false, None))) typedFields
    | ChakraList list ->
        collectExprTypes list.Items (env, [])
        |> Result.bind (fun (e, ts) ->
            match List.foldBack reduceTypeResults ts (Some (List.head ts)) with
            | Some t -> Ok (e, ListType t)
            | _ -> Error (TypeError "List had discordant types"))
            
    | ChakraMap map when map.Pairs.Length > 0->
        let (keys, values) =
            List.fold
                (fun (ks, vs) p -> (p.Key::ks, p.Value::vs))
                ([], [])
                map.Pairs

        collectLiteralTypes keys (env, [])
        |> Result.bind (fun (e, ts) ->
            match List.foldBack reduceTypeResults ts (Some (List.head ts)) with
            | Some kt ->
                collectExprTypes values (e, [])
                |> Result.bind (fun (e', ts') ->
                    match List.foldBack reduceTypeResults ts' (Some (List.head ts')) with
                    | Some vt ->
                        Ok (e', MapType (kt, vt))
                    | _ -> Error (TypeError "Map values had discordant types"))
            | _ -> Error (TypeError "Map keys had discordant types"))
    | ChakraMap map ->
        Ok (env, MapType (genA, genB))
    | ChakraLambda l ->
        exprListType (newScope (List.map untypedBinding (l.Args)) env) l.Body
        |> Result.map(fun (e,t) -> (e, FunctionType ([], t)))
    | ChakraVar (root, path) when path.IsNone ->
        match getTypeForBinding root env with
        | Some (Typed t) ->
            Ok(env, t)
        | Some (Errored e) -> Error e
        | Some Untyped -> Error (TypeError "Untyped")
        | None -> Error (TypeError (sprintf "Binding %s not defined" root))
    | ChakraVar _ -> raise (System.Exception "Struct field access not yet supported")

and exprType (env: Env) (expr: ChakraExpr) =
    match expr with
    | ChakraLiteralExpr (_, lit) -> literalType env lit
    | ChakraApplyExpr (_, (ChakraApply ((root, path), exprs))) ->
        // Ensure the binding exists and is a function type
        // Ensure the supplied arguments match the parameters in length and type
        // If they do, and the arguments are untyped, update their type with that
        // of the parameter and then return the return type of the function
        // Otherwise, return the appropriate type error.
        let fullPath = String.concat "." (root::path)
        match getTypeForBinding fullPath env with
        | Some (Typed (FunctionType (argTs, retT))) when argTs.Length = exprs.Length && argTs.Length > 0 -> // Total application
            match invert (List.map (exprType env) exprs) with
            | Ok ts ->
                let env' = List.fold (fun acc (e, _) -> e) env ts
                let result = (checkArgumentTypes (List.map snd ts) argTs)
                Result.map (fun _ -> (env', retT)) result
            | Error e ->
                Error e
        | Some (Typed (FunctionType (argTs, retT))) when argTs.Length > exprs.Length && argTs.Length > 0 -> // Partial application
            match invert (List.map (exprType env) exprs) with
            | Ok ts ->
                let env' = List.fold (fun acc (e, _) -> e) env ts
                let result = (checkArgumentTypes (List.map snd ts) (List.take exprs.Length argTs))
                Result.map (fun _ -> (env', FunctionType (List.skip exprs.Length argTs, retT))) result
            | Error e ->
                Error e
        | Some (Untyped) -> // Recursive call
            match invert (List.map (exprType env) exprs) with
            | Ok ts ->
                let env' = List.fold (fun _ (e, _) -> e) env ts
                Ok (env', FunctionType ((List.map snd ts), (gen "A")))
            | Error e ->
                Error e
        | _ -> // Too many args, or can't be found
            Error (TypeError (sprintf "Could not find `%s`" fullPath))
    
    | ChakraMatchExpr (_, m) ->
        // TODO: This is the heavy hitter, what really moves the type system
        // Really shouldn't be done until patterns are done
        raise (System.Exception "Not yet supported")
    | ChakraNativeExpr _ -> raise (System.Exception "Should never be parsing native expressions")

and exprListType (env: Env) (ChakraExprList (bs, e)) =
    let handleBindingType (acc: Env) b =
        match bindingType acc b with
        | Ok (e, t) ->
            match bindingName b with
            | Some name -> addBinding (name, Typed t) e
            | None -> e
        | Error (e) -> 
           match bindingName b with
            | Some name -> addBinding (name, Errored e) acc
            | None -> acc
        
    let env' =
        List.fold
            handleBindingType
            env
            bs
    exprType env' e

and bindingName (b: ChakraBinding) =
    match b.Pattern with
    | ChakraSimpleBindingPattern bindingName ->
        Some bindingName
    | ChakraFunctionBindingPattern { Args = args ; Name = bindingName } ->
        Some bindingName
    | ChakraComplexBindingPattern patt ->
        None

and patternType (env: Env) (patt: ChakraPattern) =
    match patt with
    | CPIgnore _ ->
        Ok (env, genA)
    | CPVar (_, root, path) ->
        literalType env (ChakraVar (root, path))
    | CPNumber (_, n) ->
        Ok (env, NumberType)
    | CPSymbol (_, s) ->
        // TODO: Handle module symbols
        Ok (env, SymbolType (GlobalSymbol s))
    | CPString (_, s) ->
        Ok (env, StringType)
    | CPTuple (_, ps) ->
        Error (TypeError "Pattern not yet supported")
    | CPStruct (_, pstruct) ->
        Error (TypeError "Pattern not yet supported")
    | CPList (_, list) ->
        Error (TypeError "Pattern not yet supported")
    | CPMap (_, map) ->
        Error (TypeError "Pattern not yet supported")

and bindingType (env: Env) ({ ExprList = exprList ; Pattern = pattern }): Result<Env * Type, TypeError>  =
    match pattern with
    | ChakraSimpleBindingPattern bindingName ->
        exprListType (newScope [] env) exprList
    | ChakraFunctionBindingPattern { Args = args ; Name = bindingName } ->
        exprListType (newScope (List.map untypedBinding (bindingName::args)) env) exprList
    | ChakraComplexBindingPattern patt ->
        let elt = exprListType (newScope [] env) exprList
        Error (TypeError "Complex bindings not yet type-inferrable")

and collectTypes typeFn items (accEnv, accItemTs) =
    match items with
        | [] -> Ok (accEnv, accItemTs)
        | item::items -> 
            match typeFn accEnv item with
            | Ok (e, t) -> collectTypes typeFn items (e, t::accItemTs)
            | Error e -> Error e

and collectExprTypes exprs' (accEnv, accExprTs) =
    collectTypes exprType exprs' (accEnv, accExprTs)

and collectLiteralTypes lits (env, litTs) =
    collectTypes literalType lits (env, litTs)
            
    // TODO: Allow adding generic bindings?  Or do we do this here?
and collectArgTypes acc (argT, paramT) =
    match unify argT paramT with
    | Some t -> Result.map (fun ts -> t::ts) acc
    | None -> Error (TypeError (sprintf "Argument of type %s could not unify with parameter of type %s" (print argT) (print paramT)))

and checkArgumentTypes (argTypes: Type list) (paramTypes: Type list) : Result<Type list, TypeError> =
   (List.fold (collectArgTypes) (Ok []) (List.zip argTypes paramTypes))

// (**************************************************************************************************
// **
// **    TESTS
// **
// **************************************************************************************************)

// let literalTypeTests () =
//     let literalTypeTest (label, input, typ) =
//         (ParserLibrary.run chakraLiteral input)
//         |> (fun result ->
//             match result with
//             | ParserLibrary.Success (literal, _) ->
//                 let resultType = literalType (defaultEnv) literal
//                 if resultType = typ
//                 then printfn "%s : %s" input (print resultType)
//                 else printfn "FAILED: %s\n%s <> %s" label (print resultType) (print typ)
//             | _ ->
//                 printfn "FAILED: Could not parse '%s'" input
//                 ParserLibrary.printResult result)

//     [ ("A number", "1", NumberType)
//       ("A string", "\"\"", StringType)
//       ("A symbol", "#one", SymbolType "one")
//       ("A simple tuple", "( 1, 2 )", TupleType [ NumberType; NumberType ])
//       ("A simple list", "[ 1, 2, 3 ]", ListType NumberType)
//       ("A complex list",
//        "[ 1, #two, \"three\" ]",
//        ListType
//            (UnionType [ StringType
//                         NumberType
//                         SymbolType "three" ]))
//       ("A simple struct",
//        "%( something = 1, else = #two )",
//        StructType [ ("something", NumberType)
//                     ("else", SymbolType "two") ])
//       ("A simple map", """%[ "something" = 1, "else" = 2 ]""", MapType(StringType, NumberType))
//       ("A list of structs",
//        "[ %( something = 1, else = #two ), %( something = 1, else = #two ), %( something = 1, else = #two ) ]",
//        ListType
//            (StructType [ ("something", NumberType)
//                          ("else", SymbolType "two") ])) ]
//     |> List.map literalTypeTest
