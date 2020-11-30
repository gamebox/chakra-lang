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
    | [||] ->
        Ok Help
    | str ->
        Error (sprintf "Unknown command: %s" (str.[0]))
    
