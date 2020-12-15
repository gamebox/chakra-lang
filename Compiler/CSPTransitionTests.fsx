#load "ParserLibrary.fs"
#load "ParserLibraryCSP.fs"
#load "Console.fs"

let areSame (r1: ParserLibrary.Result<'a>) (r2: ParserLibraryCSP.Result<'a>) =
    match (r1, r2) with
    | (ParserLibrary.Success (a, { Lines = l; Position = p }), ParserLibraryCSP.Success (a1, { Lines = l1; Position = p1 })) ->
        a = a1
        && l = l1
        && p.Line = p1.Line
        && p.Column = p1.Column
    | _ -> false

let test label parser cspParser input =
    let result1 = ParserLibrary.run parser input
    let result2 = ParserLibraryCSP.run cspParser input
    if areSame result1 result2 then
        printf (Console.greenFg (sprintf "%s" label))
    else
        printf (Console.redFg (sprintf "%s" label))