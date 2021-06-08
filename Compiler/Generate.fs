module Generate

open TypedAST
open TypeSystem
open Operators

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

let inspect label s =
    match s with
    | Some x ->
        printfn "%s" label
        s
    | None ->
        printfn "FAILED: %s" label
        s

let generateStructAccess root path ty state =
    IRState.findIdentifierForVar root state
    |> inspect (sprintf "finding identifer for var %s" root)
    .?>>. (fun (id, ty') ->
        let (TypeSystem.StructType (fs, _, _)) = ty'
        let (idxs, _) =
            List.fold
                (fun (is, fields) segment ->
                    let idx = List.findIndex (fun (k, t) -> k = segment) fields
                    let (_, ty) = List.item idx fields
                    match ty with
                    | TypeSystem.StructType (fs, _, _) ->
                        (idx::is, fs)
                    | _ ->
                        (List.rev (idx::is), [(segment, ty)]))
                ([], fs)
                path
        // calculate getelementptr indexes from path and type of root
        IRState.addGepInstruction id ty' idxs state
        |> inspect "added GEP instruction")


let rec generateExprList (TCExprList (bs, e)) state =
    IRState.createScope state
    |> inspect "New scope created"
    .?>>. generateBindings bs
    |> inspect "Generated Bindings"
    .?>>. generateExpr e
    |> inspect "Generating expr list expr"
    .?>>. IRState.destroyScope

and generateExpr (expr: TypedAST.TCExpr) (state: IRState.IRState) =
    match expr with
    | TCVar ((root, Some path), ty) ->
        // Find the register for the root
        // get the type of the root
        generateStructAccess root path ty state
        .?>>. (fun s ->
            IRState.lastInstruction s
            .?>>. (fun i -> IRState.addLoadInstruction ty i s))
        |> inspect "Generated struct access for var"
    | TCString s ->
        // Create constant for string
        // Return constant name
        IRState.addConstant s state
        |> inspect "added constant for string"
    | TCApplyExpr (_, ty, app) ->
        match app with
        | TCApply ((root, []), args) ->
            None
        | TCApply ((root, path), args) ->
            // resolve all the arg expression and collect their type and indexes
            // Find the register for the root
            // get the type of the root
            // calculate getelementptr indexes from path and type of root
            // insert a call instruction for the function at the register
            let (args', state') =
                List.fold
                    (fun (args', state) (i, (arg: TCExpr)) ->
                        let blah s =
                            IRState.lastInstruction s
                            .<?>. (fun i -> ((i, arg.Typ)::args', Some s))
                            |> Option.defaultValue ([], None)
                        
                        let x = 
                            state
                            .?>>. generateExpr arg
                            .<?>. blah
                            |> Option.defaultValue ([], None)
                        
                        x)
                    ([], Some state)
                    (List.mapi (.<.>.) args)

            
            state'
            .?>>. generateStructAccess root path ty
            .?>>. (fun s ->
                let ty' = TypeSystem.fn (List.map (fun (_, t) -> ("", t)) (List.rev args')) ty
                IRState.lastInstruction s
                .?>>. (fun i -> IRState.addLoadInstruction ty' i s))
            |> inspect "generated struct access for applyee"
            .?>>. (fun s ->
                IRState.addCallInstruction ty (IRState.lastInstruction s |> Option.get) (List.rev args') s)
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
            let (TypeSystem.FunctionType (_, ret)) = b.Typ
            state
            |> IRState.addFunction info.Name ret info.TypedArgs
            |> inspect "Added function"
            .?>>. IRState.editBasicBlock
            |> inspect "Editing basic block"
            .?>>. generateExprList b.TypedExprList
            |> inspect "Generating expr list"
            .?>>. IRState.completeEntryBlockWithRet ret
            |> inspect "Completing entry block"
            .?>>. IRState.completeFunction
            |> inspect "Completed Function"
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

let addSymbolsForImport (i: AST.ChakraImport) state =
    match i with
    | AST.ChakraPackageImport info ->
        match info.Typ with
        | AST.ChakraDestructuredImportBinding bindings ->
            Some state
        | _ ->
            None
    | _ -> None

let populateImportSymbols (imports: AST.ChakraImport list) state =
    List.fold
        (fun acc i -> acc .?>>. addSymbolsForImport i)
        (Some state)
        imports

let generateModule state name (m: TypedAST.TCModule) =
    IRState.openModule name state
    |> inspect "Opened module"
    .?>>. IRState.createScope
    |> inspect "create scope for module"
    .?>>. generateBindings m.Bindings
    .?>>. IRState.unloadModule
    |> Option.defaultWith (fun () ->
        raise (System.Exception (sprintf "Could not generate module %s" name)))

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