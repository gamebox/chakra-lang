module Command

type Command =
    | Repl
    | Build of string option
    | Format of string
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
    | [| "format"; str|] ->
        Ok (Format str)
    | [| "format" |] ->
        Error ("Format requires a path to be supplied")
    | [|"help"|] ->
        Ok Help
    | [| "version" |] ->
        Ok Version
    | [||] ->
        Ok Help
    | str ->
        Error (sprintf "Unknown command: %s" (str.[0]))
    
