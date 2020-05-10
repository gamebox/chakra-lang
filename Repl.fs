module Repl

let replLoop () =
    let rec loop lines =
        match lines with
        | [] ->
            stdout.Write "> "
        | _ -> ()

        let line = stdin.ReadLine ()
        if line = "exit;;" then
            ()
        elif line.EndsWith ";;" then
            let sanitizedLine = (line.Substring (0, line.Length - 2))
            let output =
                (sanitizedLine::lines)
                |> List.rev
                |> String.concat "\n"
                |> ParserLibrary.run ChakraParser.chakraModule

            ParserLibrary.printResult output
            loop []
        else
            let newLines = line::lines
            loop newLines
    loop []