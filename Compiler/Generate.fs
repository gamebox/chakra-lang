module Generate

open TypedAST
open TypeSystem

let (.<?>.) opt fn =
    opt
    |> Option.map fn

let (.?>>.) opt fn =
    opt
    |> Option.bind fn

let generateBinding state (i, b) =
    match b.TypedPattern with
        | TCSimpleBindingPattern s ->
            Some state
        | TCFunctionBindingPattern info ->
            printfn "Should add a function for %s" info.Name
            let (TypeSystem.FunctionType (args, ret)) = b.Typ
            state
            |> IRState.addFunction info.Name ret (List.map snd args)
            .?>>. IRState.completeFunction
        | TCComplexBindingPattern patt ->
            Some state

let (.<.>.) a b = a, b

let generateBindings bs state =
        List.fold
            (fun acc item ->
                match acc with
                | Some s -> generateBinding s item
                | None -> None)
            (Some state)
            (List.mapi (.<.>.) bs)
            

let generateModule name (m: TypedAST.TCModule) state =
    IRState.openModule name state
    .?>>. generateBindings m.Bindings
    .?>>. IRState.unloadModule
    |> Option.defaultWith (fun () -> raise (System.Exception (sprintf "Could not generate module %s" name)))

let generate (proj: Map<string, TCModule>) (types: Set<Type>) =
    Map.fold (fun acc name m -> generateModule name m acc) (IRState.empty "x86_64-pc-linux-gnu" "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128") proj
    |> IRState.print
    |> fun generated ->
        match generated with
        | Some gen ->
            printfn "Generated\n-----------------\n%s\n------------------" gen
            Ok gen
        | None -> Error "Could not generate"