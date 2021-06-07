module Generate

open TypedAST
open TypeSystem

let (.<?>.) opt fn =
    opt
    |> Option.map fn
let (.?>>.) opt fn =
    opt
    |> Option.bind fn
let (.>>.) opt fn = Result.bind fn opt
let (.<.>.) a b = a, b


let demoteChakraTypeToVoidPtr ty fromReg state =
    match ty with
    | NumberType -> state
    | StringType -> state
    | SymbolType _ -> state
    | _ -> state

let promoteVoidPtrToChakraType ty fromReg state =
    match ty with
    | NumberType -> state
    | StringType -> state
    | SymbolType _ -> state
    | _ -> state

let genMake initTy stateTy msgTy state =
    state

let rec generateExprList (TCExprList (bs, e)) state =
    IRState.createScope state
    .?>>. generateBindings bs
    .?>>. generateExpr e

and generateExpr (expr: TypedAST.TCExpr) (state: IRState.IRState) =
    match expr with
    | TCVar ((root, Some path), ty) ->
        // Find the register for the root
        // get the type of the root
        IRState.findIdentifierForVar root state
        .?>>. (fun (id, ty) ->
            // calculate getelementptr indexes from path and type of root
            Some state)
    | TCString s ->
        // Create constant for string
        // Return constant name
        Some state
    | TCApplyExpr (_, ty, app) ->
        match app with
        | TCApply ((root, []), args) ->
            None
        | TCApply ((root, path), args) ->
            // Find the register for the root
            // get the type of the root
            // calculate getelementptr indexes from path and type of root
            // insert a call instruction for the function at the register
            Some state
        | _ ->
            None
    | _ ->
        Some state

and generateBinding (i, b) state =
    match b.TypedPattern with
        | TCSimpleBindingPattern name ->
            generateExprList b.TypedExprList state
            .?>>. (fun s ->
                IRState.lastInstruction s
                .?>>. (fun i -> IRState.registerBinding name i b.Typ s))
        | TCFunctionBindingPattern info ->
            printfn "Should add a function for %s" info.Name
            let (TypeSystem.FunctionType (args, ret)) = b.Typ
            state
            |> IRState.addFunction info.Name ret (List.map snd args)
            .?>>. IRState.completeFunction
        | TCComplexBindingPattern patt ->
            Some state

and generateBindings bs state =
        List.fold
            (fun acc item ->
                match acc with
                | Some s -> generateBinding item s
                | None -> None)
            (Some state)
            (List.mapi (.<.>.) bs)
            

let generateModule state name (m: TypedAST.TCModule) =
    IRState.openModule name state
    .?>>. generateBindings m.Bindings
    .?>>. IRState.unloadModule
    |> Option.defaultWith (fun () -> raise (System.Exception (sprintf "Could not generate module %s" name)))

let instantiateGenerics state =
    state

let generate (proj: Map<string, TCModule>) (types: Set<Type>) =
    let triple = "x86_64-pc-linux-gnu"
    let layout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
    let state = IRState.empty triple layout
    
    Map.fold generateModule state proj
    |> instantiateGenerics
    |> IRState.print
    |> fun generated ->
        match generated with
        | Ok gen ->
            printfn "Generated\n-----------------\n%s\n------------------" gen
            Ok gen
        | Error e -> generated