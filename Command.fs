module Command

type Command =
    | Repl
    | Build of string option
    | Help
    | Version

let fromArgs argv =
    match argv with
    | [|"build"; str|] ->
        Ok (Build (Some str))
    | [|"build"|] ->
        Ok (Build None)
    | [|"repl"|] ->
        Ok Repl
    | [|"help"|] ->
        Ok Help
    | [| "version" |] ->
        Ok Version
    | [| str |]
    | [| str; _ |]
    | [| str; _; _ |]
    | [| str; _; _; _ |]
    | [| str; _; _; _; _ |] ->
        Error (sprintf "Unknown command: %s" str)
    | _ ->
        Error "Please specify a known command."
    
