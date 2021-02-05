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

    | LiteralType literal -> 
        Pretty.pretty 80 (Pretty.showLiteral literal)
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
        | Some Untyped -> Error (TypeError (sprintf "Untyped var %s" root))
        | None -> Error (TypeError (sprintf "Binding %s not defined" root))
    | ChakraVar _ ->
        // TODO: Handle struct field access
        raise (System.Exception "Struct field access not yet supported")

and exprType (env: Env) (expr: ChakraExpr) =
    match expr with
    | ChakraLiteralExpr (_, lit) -> literalType env lit
    | ChakraApplyExpr (_, ChakraNamedApply ((root, path), pairs)) ->
        // TODO: Match pair entries with arguments of function, then do the same as for regular
        // apply with the exception that the returned function of a partial application takes
        // the unfulfilled args
        Error (TypeError "Named apply not yet supported")
    | ChakraApplyExpr (_, (ChakraApply ((root, path), exprs))) ->
        // Ensure the binding exists and is a function type
        // Ensure the supplied arguments match the parameters in length and type
        // If they do, and the arguments are untyped, update their type with that
        // of the parameter and then return the return type of the function
        // Otherwise, return the appropriate type error.
        let fullPath = String.concat "." (root::path)
        match getTypeForBinding fullPath env with
        | Some (Typed (FunctionType (argTs, retT))) when argTs.Length = exprs.Length && argTs.Length > 0 -> // Total application
            match checkArgumentTypes env exprs argTs with
            | Ok (e, ts) ->
                Ok (e, retT)
            | Error e ->
                Error e

        | Some (Typed (FunctionType (argTs, retT))) when argTs.Length > exprs.Length && argTs.Length > 0 -> // Partial application
            match checkArgumentTypes env exprs (List.take exprs.Length argTs) with
            | Ok (e, ts) ->
                Ok (e, FunctionType (List.skip exprs.Length argTs, retT))
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
    | ChakraPipeExpr pipe ->
        // TODO: Basically take the head, wrap it in a expr, then add it to the exprs list of the
        // apply, and then wrap that in a expr and repeat.  Once we have a ChakraApplyExpr, pass
        // it back to this function  (Should this be done in the parse step?)
        Error (TypeError "Pipes not yet supported")
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
        // TODO
        Error (TypeError "Tuple Pattern not yet supported")
    | CPStruct (_, pstruct) ->
        // TODO
        Error (TypeError "Struct Pattern not yet supported")
    | CPList (_, list) ->
        // TODO
        Error (TypeError "List Pattern not yet supported")
    | CPMap (_, map) ->
        // TODO
        Error (TypeError "Map Pattern not yet supported")

and bindingType (env: Env) ({ ExprList = exprList ; Pattern = pattern }): Result<Env * Type, TypeError>  =
    match pattern with
    | ChakraSimpleBindingPattern bindingName ->
        exprListType (newScope [] env) exprList
    | ChakraFunctionBindingPattern { Args = args ; Name = bindingName } ->
        exprListType (newScope (List.map untypedBinding (bindingName::args)) env) exprList
        |> Result.bind (fun (env', t) ->
            // Get the type for each of the args, and pass it along in the function type
            // If any args are untyped give it generic
            let getArgTypesFromEnv arg acc =
                match acc with
                | Ok ((g: string), e', ts) ->
                    match getTypeForBinding arg e' with
                    | Some (Typed t) -> Ok (g, e', t::ts)
                    | Some Untyped ->
                        let t = typedBinding arg (gen g)
                        let e'' = updateBinding t e'
                        Ok (sprintf "%c" (char (int (g.Chars 0) + 1)), e'', (gen g)::ts)
                    | Some (Errored e') ->
                        Error e'
                    | None ->
                        Error (TypeError "Missing arg binding - THIS SHOULD NOT HAPPEN")
                
                | Error e -> Error e
                
            match List.foldBack getArgTypesFromEnv args (Ok ("a", env',  [])) with
            | Ok (_, e', argTs) -> Ok (popScope e', fn argTs t)
            | Error e' -> Error e')
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

and checkArgumentTypes' (argTypes: Type list) (paramTypes: Type list) : Result<Type list, TypeError> =
   (List.fold (collectArgTypes) (Ok []) (List.zip argTypes paramTypes))

and checkArgumentTypes (env: Env) (args: ChakraExpr list) (paramTypes: Type list) : Result<Env * Type list, TypeError> =
    let rec inner (env: Env) (args: ChakraExpr list) (paramTypes: Type list) (ts: Type list) : Result<Env * Type list, TypeError> =
        match (args, paramTypes) with
        | (arg::args', paramT::paramTypes') ->
            match checkArgumentExprAgainstParamType env arg paramT with
            | Ok (e, t) ->
                inner e args' paramTypes' (t::ts)
            | Error e ->
                Error e
        | _ -> Ok (env, ts)

    inner env args paramTypes []

and checkArgumentExprAgainstParamType (env: Env) (arg: ChakraExpr) (paramT: Type) : Result<Env * Type, TypeError> =
    match arg with
    | ChakraLiteralExpr (_, ChakraVar (root, _)) ->
        match getTypeForBinding root env with
        | Some (Typed t) ->
            match unify t paramT with
            | Some t' -> Ok (env, t')
            | None -> Error (TypeError (sprintf "Argument type does not match param type:\n\nArg Type:\n%s\n\nParam Type:\n%s" (print t) (print paramT)))
        | Some (Untyped) ->
            let env' = updateBinding (typedBinding root paramT) env
            Ok (env', paramT)
        | Some (Errored e) -> Error e
        | None -> Error (TypeError (sprintf "Undefined variable: %s" root))
    | _ ->
      exprType env arg