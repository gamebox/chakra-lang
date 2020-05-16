module Repl

type ReplInput =
    | ReplBinding of ChakraParser.ChakraBinding
    | ReplExpr of ChakraParser.ChakraExpr

type ReplInputState =
    | Incomplete
    | ParseSuccess of (string * Unify.Env * Map<string, ChakraParser.ChakraBinding>)
    | ParseError of ParserLibrary.Result<ReplInput * ParserLibrary.InputState>

type ReplBindings = Map<string, ChakraParser.ChakraBinding>

let replParser: ParserLibrary.Parser<ReplInput> =
    let (<|>) = ParserLibrary.orElse

    let replBinding: ParserLibrary.Parser<ReplInput> =
        ParserLibrary.mapP ReplBinding ChakraParser.chakraBinding

    let replExpr: ParserLibrary.Parser<ReplInput> =
        ParserLibrary.mapP ReplExpr ChakraParser.chakraExpr

    replBinding <|> replExpr


let handleInput types bindings input =
    match ParserLibrary.run replParser input with
    | ParserLibrary.Success (parsedInput, remaining) when ParserLibrary.atEndOfInput remaining ->
        match parsedInput with
        | ReplBinding (ChakraParser.ChakraBinding (name, exprList)) ->
            let typ = Unify.exprListType types exprList

            let name' =
                match name with
                | ChakraParser.ChakraSimpleBindingPattern n -> n
                | _ -> ""

            let typDisplay = sprintf "%s: %s" name' (Unify.print typ)

            ParseSuccess
                (typDisplay,
                 Unify.addBinding name' typ types,
                 Map.add name' (ChakraParser.ChakraBinding(name, exprList)) bindings)

        | ReplExpr expr ->
            let typ = Unify.exprType types expr
            let typDisplay = sprintf "%s" (Unify.print typ)
            ParseSuccess(typDisplay, types, bindings)
    | ParserLibrary.Success (parsed, remaining) -> Incomplete
    | res -> ParseError res

let createInput line lines =
    ((line :: lines) |> List.rev |> String.concat "\n").Trim [| '\n'; '\t'; '\r' |]

let replLoop () =
    let rec loop (types: Unify.Env) (bindings: ReplBindings) lines =
        match lines with
        | [] -> stdout.Write "> "
        | _ -> ()

        let line = stdin.ReadLine()
        if line = ":q" then
            ()
        else
            let runResult =
                createInput line lines
                |> handleInput types bindings

            match runResult with
            | ParseSuccess (typ, types', bindings') ->
                printfn "%s" typ
                loop types' bindings' []
            | ParseError res ->
                ParserLibrary.printResult res
                loop types bindings []
            | Incomplete -> loop types bindings (line :: lines)

    loop (Unify.createEnv ()) (new Map<string, ChakraParser.ChakraBinding>([])) []

module Console =
    let csi = "\e["
    let cursorLeft n = printf "%s%dD" csi n

    let cursorMoveToX x = printf "%s%dG" csi x
