#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Format.fs"
#load "Console.fs"

open ParserLibrary
open ChakraParser
open Format

let modAsString =
    ("
;; This is some comments
;; They should be doc comments
;; Yay!
= %( foo, bar )

%( a, b ) = /awesome-package


;; Something
foo = 22.000000
bar = \"Hello world!\"".Trim([| '\n' |]))

let roundTripTest label parser input =
    match run parser input with
    | (Success (m, _)) ->
        printfn "%O" m
        let format1 =
            (pretty 80 (showModule m)).Trim([| '\n' |])

        match run parser format1 with
        | (Success (parse1, _)) ->
            let format2 =
                (pretty 80 (showModule parse1)).Trim([| '\n' |])

            match run parser format2 with
            | (Success (parse2, _)) ->
                if format1 = format2 then
                    printfn "%s" format2
                    if parse1 = parse2 then
                        printfn "%s Parse->Format Roundtrip: SUCCESS" label
                    else
                        printfn "%s"
                            (Console.red
                                (sprintf "%s Parse->Format Roundtrip: FAILURE\nParsed modules did not match, first:\n%s\n\second:\n%s"
                                     label (sprintf "%O" parse1) (sprintf "%O" parse2)))
                else
                    printfn "%s"
                        (Console.red
                            (sprintf "%s Parse->Format Roundtrip: FAILURE\nFormatted modules did not match, first:\n%s\n\second:\n%s"
                                 label format1 format2))
            | e ->
                printfn "%s"
                    (Console.red
                        (sprintf "%s Parse->Format Roundtrip: FAILURE\nCould not parse second format result:\n%s"
                             label (resultString e)))
        | e ->
            printfn "%s"
                (Console.red
                    (sprintf "%s Parse->Format Roundtrip: FAILURE\nCould not parse first format result:\n%s" label (resultString e)))
    | e ->
        printfn "%s"
            (Console.red
                (sprintf "%s Parse->Format Roundtrip: FAILURE\nCould not parse raw module string:\n%s" label (resultString e)))


roundTripTest "Module" chakraModule modAsString