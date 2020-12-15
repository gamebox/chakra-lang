#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Format.fs"

open ParserLibrary
open ChakraParser
open Format

let m =
    """

;; Hello there!
this =
    something = ( ( boo, boo ) )
    tutu = []
    ( "Hello world!", 1324344234234, #hello!)
    """.Trim ([| '\t'; '\n' |])

match run chakraBinding m with
| Success (p, is) ->
    printfn "%s" (pretty 80 (showBinding p))
| f ->
    printResult f