open Repl

let help =
    "\
    Chakra\n\
    ================\n\n\
    Available Commands\n\
    -------------------------------------------------------------------------\n\
    |                                                                       |\n\
    | help          Prints this help                                        |\n\
    | version       Shows version information                               |\n\
    | repl          Starts an interactive REPL shell                        |\n\
    | build [path]  [TODO] Build an app in the current directory if path is |\n\
    |               is specified, or the app in the directory specified.    |\n\
    |                                                                       |\n\
    -------------------------------------------------------------------------
    "

[<EntryPoint>]
let main argv =
    match Command.fromArgs argv with
    | Ok command ->
        match command with
        | Command.Repl ->
            replLoop ()
        | Command.Help ->
            printfn "%s" help
        | Command.Version ->
            printfn "Chakra v0.0.1"
        | Command.Build optPath ->
            printfn "Build not yet implemented"
            
        0
    | Error err ->
        printfn "%s" err
        printfn "%s" help
        0
