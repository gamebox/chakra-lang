module Generate

open TypedAST
open TypeSystem

let printIRState state =
    ""

let generateModule name m state =
    state

let generate (proj: Map<string, TCModule>) (types: Set<Type>) =
    Map.fold (fun acc name m -> generateModule name m acc) (IRState.empty) proj
    |> printIRState
    |> Ok