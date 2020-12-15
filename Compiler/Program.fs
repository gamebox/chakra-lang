//open Repl
open Build
let versionInfo = "chakra v0.0.1"
let help =
    sprintf "\
    %s\n\n\
    ┌────────────────────────────────────────────────────────────────────────┐\n\
    │                                                                        │\n\
    │ help          Prints this help                                         │\n\
    │ version       Shows version information                                │\n\
    │ repl          Starts an interactive REPL shell                         │\n\
    │ build [path]  [TODO] Build an app in the current directory if path is  │\n\
    │               is not specified, or the app in the directory specified. │\n\
    │                                                                        │\n\
    └────────────────────────────────────────────────────────────────────────┘" versionInfo

[<EntryPoint>]
let main argv =
    match Command.fromArgs [|"build"|] with
    | Ok command ->
        match command with
        | Command.Repl ->
            printfn "%s" versionInfo
            // replLoop ()
        | Command.Help ->
            printfn "%s" help
        | Command.Version ->
            printfn "%s" versionInfo
        | Command.Build optPath ->
            //Blah.test ChakraParser.chakraBinding
            build (Some "/home/anthony/dev/chakra-lang/Examples/ping-pong")
            //printfn "Build not yet implemented"
            
        0
    | Error err ->
        printfn "%s" err
        printfn "%s" help
        0
