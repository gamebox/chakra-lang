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
    │ build [path]  [TODO] Build an app in the current directory if path is  │\n\
    │               is not specified, or the app in the directory specified. │\n\
    │ format PATH   Formats the Chakra file at PATH                          │\n\
    │                                                                        │\n\
    └────────────────────────────────────────────────────────────────────────┘" versionInfo

[<EntryPoint>]
let main argv =
    match Command.fromArgs argv with
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
            build optPath
            //printfn "Build not yet implemented"
        | Command.Format str ->
            Pretty.format str
        
            
        0
    | Error err ->
        printfn "%s" err
        printfn "%s" help
        0
