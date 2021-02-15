#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Env.fs"
#load "Pretty.fs"
#load "Unify.fs"

open Unify
open Env
open ChakraParser

let runTest () =
    let text =
        """
        x ?
        | 1 -> ["one"]
        | 2 -> ["two"]
        | _ -> ["Other"]
        """.TrimStart([| '\n'; '\t'; ' ' |])


    let env = addBindings [untypedBinding "x"] (emptyWith [])


    let (ParserLibrary.Success (cme, _)) = ParserLibrary.run chakraMatchExpr text
    let (Ok (e, t)) = exprType env cme
    printfn "%s" (print t)
    let (Some (Typed t')) = getTypeForBinding "x" e
    printfn "x : %s" (print t')

runTest ()


let thread fn state items =
    List.fold (fun acc item -> Result.bind (fun s -> fn s item) acc) (Ok state) items