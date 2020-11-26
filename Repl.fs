module Repl

let (<?>) = ParserLibrary.setLabel
let (<.>>.>) = ParserLibrary.andThen

type ReplInput =
    | ReplBinding of ChakraParser.ChakraBinding
    | ReplExpr of ChakraParser.ChakraExpr
    | ReplExprList of ChakraParser.ChakraExprList
    | ReplModule of ChakraParser.ChakraModule

type ReplInputState =
    | Incomplete
    | ParseSuccess of (string * Env.Env * Map<string, ChakraParser.ChakraBinding>)
    | ParseError of ParserLibrary.Result<ReplInput * ParserLibrary.InputState>

type ReplBindings = Map<string, ChakraParser.ChakraBinding>

let mergeBindings (existing: ReplBindings) (n: ReplBindings) =
    let folder acc k v = Map.add k v acc
    Map.fold folder existing n

let replParser: ParserLibrary.Parser<ReplInput> =
    let (<|>) = ParserLibrary.orElse

    let replBinding: ParserLibrary.Parser<ReplInput> =
        ParserLibrary.mapP ReplBinding ChakraParser.chakraBinding
        <?> "replBinding"

    let replExpr: ParserLibrary.Parser<ReplInput> =
        ParserLibrary.mapP ReplExpr ChakraParser.chakraExpr
        <?> "replExpr"

    replBinding <|> replExpr

let replFileParser: ParserLibrary.Parser<ReplInput> =
    ParserLibrary.mapP ReplModule ChakraParser.chakraModule


let rec handleInput parser types bindings (input: string) =
    match ParserLibrary.run parser input with
    | ParserLibrary.Success (parsedInput, remaining) when ParserLibrary.atEndOfInput remaining ->
        match parsedInput with
        | ReplBinding b ->
            handleParsedBinding b types bindings
        | ReplExpr expr ->
            handleParsedExpr expr types bindings
        | ReplExprList exprList ->
            handleParsedExprList exprList types bindings 
        | ReplModule m ->
            handleParsedModule m types bindings 
    | ParserLibrary.Success (parsed, remaining) -> Incomplete
    | res -> ParseError res
and handleParsedBinding (ChakraParser.ChakraBinding (pos, name, exprList)) types bindings =
    let typ = Unify.exprListType types exprList

    let name' =
        match name with
        | ChakraParser.ChakraSimpleBindingPattern n -> n
        | _ -> ""

    let typDisplay = sprintf "%s: %s" name' (Unify.print typ)

    ParseSuccess
        (typDisplay,
         Env.addBinding name' typ types,
         Map.add name' (ChakraParser.ChakraBinding(pos, name, exprList)) bindings)

and handleParsedExpr expr types bindings =
    let typ = Unify.exprType types expr
    let typDisplay = sprintf "%s" (Unify.print typ)
    ParseSuccess(typDisplay, types, bindings)

and handleParsedExprList (ChakraParser.ChakraExprList (bs, expr)) types bindings =
    let foldFn acc binding =
        match acc with
        // If acc is currently a success, Handle binding input
        | ParseSuccess (td, ts, bs') ->
            let (ParseSuccess (td', ts', bs'')) = handleParsedBinding binding ts bs'
            // TODO (gamebox): If successful, add the result to the acc
            // else, switch the acc to the error.
            let (ChakraParser.ChakraBinding (pos, name, exprList)) = binding
            ParseSuccess
                (
                    String.concat "\n" [td ; td'],
                    Env.merge ts' ts,
                    mergeBindings bs' bs''
                )
        | _ -> acc

    let bindingParseResult =
        List.fold foldFn (ParseSuccess ("", types, bindings)) bs
    bindingParseResult
and handleParsedModule ({ Bindings = bs ; Exports = exports }) types bindings =
    let foldFn acc binding =
        match acc with
        // If acc is currently a success, Handle binding input
        | ParseSuccess (td, ts, bs') ->
            let (ParseSuccess (td', ts', bs'')) = handleParsedBinding binding ts bs'
            // TODO (gamebox): If successful, add the result to the acc
            // else, switch the acc to the error.
            let (ChakraParser.ChakraBinding (pos, name, exprList)) = binding
            ParseSuccess
                (
                    String.concat "\n" [td ; td'],
                    Env.merge ts' ts,
                    mergeBindings bs' bs''
                )
        | _ -> acc

    List.fold foldFn (ParseSuccess ("", types, bindings)) bs

let createInputFromLines lines = (String.concat "\n" lines).Trim [| '\n'; '\t'; '\r' |]

let createInput line lines =
    (line :: lines) |> List.rev |> createInputFromLines

let createInputFromFile fileLines =
    createInputFromLines fileLines

let replLoop () =
    let rec loop (types: Env.Env) (bindings: ReplBindings) lines =
        match lines with
        | [] -> stdout.Write "> "
        | _ -> ()

        let line = stdin.ReadLine()
        if line = ":q" then
            ()
        else if line.StartsWith(":l ") && lines.Length = 0 then
            let fileName = 
                System.IO.Path.GetFileName(line.Substring(2).Trim ())
            printfn "Loading %s" fileName
            let fileLines = System.IO.File.ReadAllLines(fileName)
            let runResult =
                createInputFromFile fileLines
                |> handleInput replFileParser types bindings
            
            match runResult with
            | ParseSuccess (typ, types', bindings') ->
                printfn "%s" typ
                loop types' bindings' []
            | ParseError res ->
                ParserLibrary.printResult res
                loop types bindings []
            | Incomplete ->
                printfn "This should not happen"
                loop types bindings []
        else
            let runResult =
                createInput line lines
                |> handleInput replParser types bindings

            match runResult with
            | ParseSuccess (typ, types', bindings') ->
                printfn "%s" typ
                loop types' bindings' []
            | ParseError res ->
                ParserLibrary.printResult res
                loop types bindings []
            | Incomplete -> loop types bindings (line :: lines)

    loop (Env.createEnv ()) (new Map<string, ChakraParser.ChakraBinding>([])) []

module Console =
    let csi = "\e["
    let cursorLeft n = printf "%s%dD" csi n

    let cursorMoveToX x = printf "%s%dG" csi x
