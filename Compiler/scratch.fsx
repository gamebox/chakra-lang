#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "ChakraParser.fs"

open ParserLibrary
open ChakraParser

let m =
    """
a =
    foo(b) =
        add(1, b)
    else = 34
    thing = #symbol
    here = "Hi"

    io
    > foo(bar)
    > something(else)
    > last(thing, here)
    """.Trim ([| '\t'; '\n'; ' ' |])

printResult (run chakraBinding m)

#load "Format.fs"
open Format
"\n\tA\n\tB" = (pretty 80 (block 1 [text "A" ; text "B"]))