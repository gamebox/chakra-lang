#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Env.fs"
#load "Pretty.fs"
#load "Unify.fs"

open Unify
open Env
open ChakraParser

let runTest parser typeFn bindings (text: string) =
    let env = emptyWith bindings
    match ParserLibrary.run parser (text.Trim([| '\n'; '\t'; ' ' |])) with
    | ParserLibrary.Success (cme, _) ->
        match typeFn env cme with
        | Ok (e, t) ->
            printfn "%s" (print t)
        | Error e ->
            printfn "%s" (CConsole.red (sprintf "%O" e))
    | ParserLibrary.Failure _ as f -> ParserLibrary.printResult f

let matchTest = runTest chakraMatchExpr exprType
let applyTest = runTest chakraApplyExpr exprType
let bindingTest = runTest chakraBinding bindingType
let patternTest = runTest chakraPattern patternType


let matchOne =
    """
    x ?
    | 1 -> ["one"]
    | 2 -> ["two"]
    | _ -> ["Other"]
    """

let patternOne =
    """
    %(
        foo = 2,
        bar = 1,
    ...)
    """

patternTest [] patternOne