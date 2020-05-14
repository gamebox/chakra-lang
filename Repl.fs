module Repl

type ReplInput =
    | ReplBindings of ChakraParser.ChakraBinding list
    | ReplExpr of ChakraParser.ChakraExpr

type ReplBindings = Map<string, ChakraParser.ChakraExprList>

let replLoop () =
    let rec loop (types:Unify.Env) (bindings:ReplBindings) lines =
        match lines with
        | [] ->
            stdout.Write "> "
        | _ -> ()

        let line = stdin.ReadLine ()
        if line = ":q" then
            ()
        elif line.EndsWith "." then
            let sanitizedLine = (line.Substring (0, line.Length - 2))
            let output =
                (sanitizedLine::lines)
                |> List.rev
                |> String.concat "\n"
                |> ParserLibrary.run ChakraParser.chakraExprList

            match output with
            | ParserLibrary.Success (expr, _) ->
                printfn "%s" (Unify.print (Unify.exprListType (Unify.createEnv ()) expr))
            | _ -> ParserLibrary.printResult output
            loop types bindings []
        else
            let newLines = line::lines
            loop types bindings newLines
    loop (Unify.createEnv ()) (new Map<string, ChakraParser.ChakraExprList> ([])) []

module Console =
    let csi =
        "\e["
    let cursorLeft n =
        printf "%s%dD" csi n

    let cursorMoveToX x =
        printf "%s%dG" csi x
