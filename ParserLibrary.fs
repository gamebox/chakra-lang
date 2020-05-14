module ParserLibrary


open System



(*
** Types
*)
type ParserLabel = string
type ParserError = string

type ParserPosition =
    { currentLine: string
      line: int
      column: int }

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Position = { line: int; column: int }

type InputState =
    { lines: string []
      position: Position }

type Parser<'a> =
    { parseFn: InputState -> Result<'a * InputState>
      label: ParserLabel }




(*
** Text input handling
*)



let initalPos = { line = 0; column = 0 }
let incrCol pos = { pos with Position.column = pos.column + 1 }
let incrLine pos = { line = pos.line + 1; column = 0 }

let fromStr str =
    if String.IsNullOrEmpty(str) then
        { lines = [||]; position = initalPos }
    else
        let separators = [| "\r\n"; "\n" |]

        let lines =
            str.Split(separators, StringSplitOptions.None)

        { lines = lines; position = initalPos }

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then inputState.lines.[linePos] else "end of file"

let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column
    if linePos >= input.lines.Length then
        input, None
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.position
            let newState = { input with position = newPos }
            newState, Some char
        else
            let char = '\n'
            let newPos = incrLine input.position
            let newState = { input with position = newPos }
            newState, Some char

let rec readAllChars input =
    [ let remainingInput, charOpt = nextChar input
      match charOpt with
      | None ->
          // end of input
          ()
      | Some ch ->
          // return first character
          yield ch
          // return the remaining characters
          yield! readAllChars remainingInput ]

let parserPositionFromInputState (inputState: InputState) =
    { currentLine = currentLine inputState
      line = inputState.position.line
      column = inputState.position.column }

let printResult result =
    match result with
    | Success (value, input) -> printfn "%A" value
    | Failure (label, error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

(*
** Running
*)



let runOnInput parser input = parser.parseFn input

let run parser input = runOnInput parser (fromStr input)


let test label (p: Parser<'a>) input (expected: 'a) =
    run p input
    |> (fun result ->
        match result with
        | Success (value, input) when value = expected -> printfn "Success: %s" label
        | _ ->
            printfn "Test Failed: %s" label
            printResult result)

let testError (p: Parser<'a>) label input =
    run p input
    |> (fun result ->
        match result with
        | Failure (_) -> printfn "Success: %s" label
        | _ ->
            printfn "Test Failed: Expected a failure got a success: %s" label
            printResult result)
(*
** Utilities
*)



let createParserForwardedToRef<'a> () =

    let dummyParser =
        let innerFn input: Result<'a * InputState> = failwith "unfixed forwarded parser"
        { parseFn = innerFn; label = "unknown" }

    // ref to placeholder Parser
    let parserRef = ref dummyParser

    // wrapper Parser
    let innerFn input =
        // forward input to the placeholder
        runOnInput !parserRef input

    let wrapperParser = { parseFn = innerFn; label = "unknown" }

    wrapperParser, parserRef


let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s -> Success s

        | Failure (oldLabel, err, pos) -> Failure(newLabel, err, pos)

    { parseFn = newInnerFn
      label = newLabel }

let getLabel p = p.label

let (<?>) = setLabel

(*
** Combinators
*)

let bindP f p =
    let label = "unknown"

    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure (label, err, pos) -> Failure(label, err, pos)

        | Success (value1, remainingInput) ->
            let p2 = f value1
            runOnInput p2 remainingInput

    { parseFn = innerFn; label = label }

let (>>=) p f = bindP f p

let returnP x =
    let label = "unknown"
    let innerFn input = Success(x, input)

    { parseFn = innerFn; label = label }

let mapP f = bindP (f >> returnP)

let andThen p1 p2 =
    p1
    >>= (fun p1Result ->
        p2
        >>= (fun p2Result -> returnP (p1Result, p2Result)))
    <?> sprintf "%s andThen %s" (getLabel p1) (getLabel p2)

let (.>>.) = andThen

let rec parseZeroOrMore parser input =
    let firstResult = runOnInput parser input

    match firstResult with
    | Failure (label, err, pos) -> ([], input)

    | Success (firstValue, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse

        let values = firstValue :: subsequentValues
        (values, remainingInput)

let many parser =
    let rec innerFn input = Success(parseZeroOrMore parser input)

    { parseFn = innerFn
      label = "one or more " + parser.label }

let many1 p =
    p
    >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail)))

let applyP fP xP =
    fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

let orElse p1 p2 =
    let innerFn input =
        let result1 = runOnInput p1 input
        match result1 with
        | Failure (label, err, pos) -> runOnInput p2 input
        | Success res -> result1

    { parseFn = innerFn
      label = sprintf "%s orElse %s" p1.label p2.label }

let (<|>) = orElse

let choice listOfParsers = List.reduce (<|>) listOfParsers

let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = nextChar input
        match charOpt with
        | None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure(label, err, pos)

        | Some first ->
            if predicate first then
                Success(first, remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input

                Failure(label, err, pos)

    { parseFn = innerFn; label = label }

let (<!>) = mapP

let (|>>) x f = mapP f x

let (<*>) = applyP

let lift2 f xP yP = returnP f <*> xP <*> yP

let addP = lift2 (+)

let startsWith (str: string) (prefix: string) = str.StartsWith(prefix)

let startsWithP = lift2 startsWith

let cons head tail = head :: tail

let charListToStr charList = String(List.toArray charList)

let rec sequence parserList =
    let consP = lift2 cons

    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let (.>>) p1 p2 = p1 .>>. p2 |> mapP (fun (a, b) -> a)

let (>>.) p1 p2 = p1 .>>. p2 |> mapP (fun (a, b) -> b)

let between p1 p2 p3 = p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP =
        sep
        >>. p
        <?> (sprintf "%s followed by %s" (getLabel sep) (getLabel p))

    p
    .>>. many sepThenP
    |>> fun (p, pList) -> p :: pList

let sepBy p sep = sepBy1 p sep <|> returnP []

let (>>%) p x = p |>> (fun _ -> x)



(*********************************************************
**
** Basic Parsers
**
*********************************************************)



let pchar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = sprintf "%c" charToMatch
    satisfy predicate label

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice
    <?> sprintf "any of %A" listOfChars

let manyChars cp = many cp |>> charListToStr

let manyChars1 cp = many1 cp |>> charListToStr

let parseLowercase = anyOf [ 'a' .. 'z' ]

let parseDigit = anyOf [ '0' .. '9' ]

let pstring str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToStr
    <?> str

let whitespaceChar = satisfy Char.IsWhiteSpace "whitespace"
let whitespace = many whitespaceChar
let whitespace1 = many1 whitespaceChar

let digit =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let digits = many1 digit

let pint =
    let resultToInt (sign, digitList) =
        let i = String(List.toArray digitList) |> int
        match sign with
        | Some ch -> -i
        | None -> i

    opt (pchar '-')
    .>>. digits
    |>> resultToInt
    <?> "integer"

let pfloat =
    let resultToFloat (((sign, digits1), point), digits2) =
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match sign with
        | Some ch -> -fl
        | None -> fl

    let digits = manyChars1 digit

    opt (pchar '-')
    .>>. digits
    .>>. pchar '.'
    .>>. digits
    |> mapP resultToFloat
    <?> "float"

let pdoublequote = pchar '"' <?> "double quote"
let comma = pchar ',' <?> "comma"
let spaces = many whitespaceChar <?> "spaces"

let spaces1 =
    many1 whitespaceChar <?> "one or more spaces"
