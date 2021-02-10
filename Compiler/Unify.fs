module Unify

open ChakraParser
open Env
open System.IO

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

let rec collectTypes<'a> typeFn (items: 'a list) (accEnv, accItemTs) =
    match items with
        | [] -> Ok (accEnv, accItemTs)
        | item::items -> 
            match typeFn accEnv item with
            | Ok (e, t) -> collectTypes typeFn items (e, t::accItemTs)
            | Error e -> Error e

let symbolType env (s: string) =
    let firstChar = (s.ToCharArray ()).[0]
    if System.Char.IsUpper firstChar then
        Ok (env, SymbolType (createModuleSymbol env s))
    else
        Ok (env, SymbolType (GlobalSymbol s))

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
    Option.bind (unify t) acc

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
    | ChakraSymbol sym -> symbolType env sym // TODO: Module symbol
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
    | ChakraMap _ ->
        Ok (env, MapType (genA, genB))
    | ChakraLambda l ->
        functionType env None l.Args l.Body
    | ChakraVar (root, None) ->
        match getTypeForBinding root env with
        | Some (Typed t) ->
            Ok(env, t)
        | Some (Errored e) -> Error e
        | Some Untyped -> Error (TypeError (sprintf "Untyped var %s" root))
        | None -> Error (TypeError (sprintf "Binding %s not defined" root))
    | ChakraVar (root, Some path) ->
        match getTypeForBinding root env with
        | Some (Typed (StructType (fs, _,_ ))) ->
            Result.map (fun t -> (env, t)) (structFieldType path fs)
        | Some (Typed t) -> Error (TypeError "Trying to access field of non-struct")
        | Some (Errored e) -> Error e
        | Some Untyped -> Error (TypeError (sprintf "Untyped var %s" root))
        | None -> Error (TypeError (sprintf "Binding %s not defined" root))

and exprType (env: Env) (expr: ChakraExpr) =
    match expr with
    | ChakraLiteralExpr (_, lit) -> literalType env lit
    | ChakraApplyExpr (_, ChakraNamedApply ((root, path), pairs)) ->
        // TODO: Match pair entries with arguments of function, then do the same as for regular
        // apply with the exception that the returned function of a partial application takes
        // the unfulfilled args
        Error (TypeError "Named apply not yet supported")
    | ChakraApplyExpr (_, (ChakraApply ((root, path), exprs))) ->
        let fullPath = String.concat "." (root::path)
        match getTypeForBinding fullPath env with
        | Some (Typed (FunctionType (argTs, retT))) when argTs.Length = exprs.Length && argTs.Length > 0 -> // Total application
            let gatherGenerics (acc: List<string>) (t: Type) : List<string> =
                match t with
                | GenericType g -> g::acc
                | _ -> acc
            let generics = List.fold gatherGenerics [] (retT::argTs)
            match checkArgumentTypes env exprs argTs with
            | Ok (e, ts) ->
                printfn "%O" ts
                // FIXME: Instantiate Generics
                // If there are generics in the args, but not in the return type
                // that is an error
                // If the return type is generic, but no generics in the args,
                // that is an error
                // Otherwise, collect generics and see if each generic has only
                // one unified type
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
        let addArgToApply app loc arg =
            match app with
            | ChakraApply (name, args) ->
                ChakraApplyExpr (loc, ChakraApply (name, List.append args [arg]))
            | _ -> failwith "Not yet supported"

        let addArgToApplyExpr expr arg =
            match expr with
            | ChakraApplyExpr (l, app) ->
                match app with
                | ChakraApply (name, args) ->
                    ChakraApplyExpr (l, ChakraApply (name, List.append args [arg]))
                | _ -> failwith "Not yet supported"
            | _ -> failwith "This should never happen"

        let head =
            match (pipe.Head, List.tryHead pipe.Tail) with
            | (ChakraPipeLiteralHead lit, Some (s, app)) ->
                ChakraLiteralExpr (pipe.Loc, lit)
                |> addArgToApply app s
            | (ChakraPipeApplyHead a, Some (s, app)) ->
                ChakraApplyExpr (pipe.Loc, a)
                |> addArgToApply app s
            | _ -> failwith "This should never happen"

        let tail = List.map (ChakraApplyExpr) pipe.Tail.Tail

        List.fold (addArgToApplyExpr) head tail
        |> (exprType env)
    | ChakraNativeExpr _ -> raise (System.Exception "Should never be parsing native expressions")

and exprListType (env: Env) (ChakraExprList (bs, e)) =
    let env' = collectBindings bs env
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
    | CPNumber _ ->
        Ok (env, NumberType)
    | CPSymbol (_, s) ->
        symbolType env s
    | CPString _ ->
        Ok (env, StringType)
    | CPTuple (_, ps) ->
        collectTypes patternType ps (env, [])
        |> Result.map (fun (e, t) -> (e, tup t))
    | CPStruct (_, pstruct) ->
        let typedFields : Result<(string * Type) list, TypeError> =
            List.foldBack
                (fun (f: CPStructField) acc ->
                    match acc with
                    | Ok fs -> 
                        match patternType env f.ValuePattern with
                        | Ok (_, t) -> Ok ((f.Name, t)::fs)
                        | Error e -> Error e
                    | _ -> acc)
                pstruct.Fields
                (Ok [])

        Result.map (fun fs -> (env, StructType (fs, pstruct.Rest.IsSome, None))) typedFields
    | CPList (_, list) ->
        collectTypes patternType list.Items (env, [])
        |> Result.bind (fun (e, ts) ->
            match List.foldBack reduceTypeResults ts (Some (List.head ts)) with
            | Some t -> Ok (e, ListType t)
            | _ -> Error (TypeError "List pattern had discordant types"))
    | CPMap (_, map) ->
        // TODO Implement typing Map patterns
        Error (TypeError "Map Pattern not yet supported")

and functionType (env: Env) (name: string option) (args: string list) (exprList: ChakraExprList) =
    let bindingsToAdd =
        match name with
        | Some s -> s::args
        | None -> args

    exprListType (newScope (List.map untypedBinding bindingsToAdd) env) exprList
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
                        Ok (nextGen g, e'', (gen g)::ts)
                    | Some (Errored e') ->
                        Error e'
                    | None ->
                        Error (TypeError "Missing arg binding - THIS SHOULD NOT HAPPEN")
                
                | Error e -> Error e
                
            match List.foldBack getArgTypesFromEnv args (Ok ("a", env',  [])) with
            | Ok (_, e', argTs) ->
                Ok (popScope e', fn argTs t)
            | Error e' -> Error e')

and bindingType (env: Env) ({ ExprList = exprList ; Pattern = pattern }): Result<Env * Type, TypeError>  =
    match pattern with
    | ChakraSimpleBindingPattern bindingName ->
        exprListType (newScope [] env) exprList
    | ChakraFunctionBindingPattern { Args = args ; Name = bindingName } ->
        functionType env (Some bindingName) args exprList
    | ChakraComplexBindingPattern patt ->
        // TODO: Finish this
        let elt = exprListType (newScope [] env) exprList
        Error (TypeError "Complex bindings not yet type-inferrable")

and collectExprTypes exprs' (accEnv, accExprTs) =
    collectTypes exprType exprs' (accEnv, accExprTs)

and collectLiteralTypes lits (env, litTs) =
    collectTypes literalType lits (env, litTs)
          
// BUG: Allow adding generic bindings?  Or do we do this here?
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
        | _ ->
            Ok (env, List.rev ts)

    inner env args paramTypes []

and checkArgumentExprAgainstParamType (env: Env) (arg: ChakraExpr) (paramT: Type) : Result<Env * Type, TypeError> =
    match arg with
    | ChakraLiteralExpr (_, ChakraVar (root, _)) ->
        match getTypeForBinding root env with
        | Some (Typed t) ->
            match unify t paramT with
            | Some t' ->
                Ok (env, t')
            | None ->
                Error (TypeError (sprintf "Argument type does not match param type:\n\nArg Type:\n%s\n\nParam Type:\n%s" (print t) (print paramT)))
        | Some (Untyped) ->
            let env' = updateBinding (typedBinding root paramT) env
            Ok (env', paramT)
        | Some (Errored e) ->
            Error e
        | None ->
            Error (TypeError (sprintf "Undefined variable: %s" root))
    | _ ->
      exprType env arg
and structFieldType (fieldSegments: string list) (fields: (string * Type) list) =
    if (fieldSegments.IsEmpty || fields.IsEmpty) then
        Error (TypeError "Struct access with no path or no fields")
    else
        let (fieldName::fs) = fieldSegments
        match (List.tryFind (((=) fieldName) << fst) fields, fs) with
        | (Some (_, t), []) ->
            Ok t
        | (Some (_, StructType (fs', _, _)), _) ->
            structFieldType fs fs'
        | _ ->
            Error (TypeError "Trying to access a field of a non-struct")

and collectBindings (bindings: ChakraBinding list) env : Env =
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
    
    List.fold
        handleBindingType
        env
        bindings

let unifyModule (path: string list) (m: ChakraModule) (envs: Map<string, Env>) : Result<Env, string> = 
    let rec collectImports (env: Env) imports: Result<Env, string> =
        match imports with
        | i::is ->
            match i with
            | ChakraLocalImport imp ->
                if imp.Relative then
                    let moduleName =
                        (List.rev path
                        |> List.tail
                        |> List.append [imp.Library]
                        |> List.rev
                        |> String.concat "/")

                    match Map.tryFind moduleName envs with
                    | Some m ->
                        let localBindings =
                            match imp.Typ with
                            | ChakraSimpleImportBinding b ->
                                [typedBinding b (lowerIntoStruct m)]
                            | ChakraDestructuredImportBinding destructurings ->
                                // TODO: Support destructured import bindings
                                failwith "Not supported yet"
                        
                        collectImports (addBindings localBindings env) is
                    | None ->
                        Error (sprintf "Cannot find module %s" moduleName)
                else
                    Ok env
            | ChakraPackageImport imp ->
                Ok env
        | [] -> Ok env


    collectImports defaultEnv m.Imports
    |> Result.map (collectBindings m.Bindings)
    |> Result.bind (fun env ->
        if List.forall (hasTypedBinding env) m.Exports then
            Ok env
        else
            Error "Missing exports")