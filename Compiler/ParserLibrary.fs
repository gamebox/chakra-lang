module ParserLibrary


open System



(*
** Types
*)



type ParserLabel = string
type ParserError = string

type ParserPosition =
    { CurrentLine: string
      Line: int
      Column: int }

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Position = { Line: int; Column: int }

type InputState =
    { Lines: string []
      Position: Position }

type Parser<'a> =
    { ParseFn: InputState -> Result<'a * InputState>
      Label: ParserLabel }

(*
    Result utils
*)

let resultMap f (r: Result<'a>) =
    match r with
    | Success a -> Success (f a)
    | Failure (l, e, p) -> Failure (l, e, p)

let resultfFMap (f: (Result<'a>) -> Result<'b>) (r: Result<'a>) =
    match r with
    | Failure (l, e, p) -> Failure (l, e, p)
    | _ -> f r

(*
** Text input handling
*)



let initalPos = { Line = 0; Column = 0 }

let incrCol pos =
    { pos with
          Position.Column = pos.Column + 1 }

let incrLine pos = { Line = pos.Line + 1; Column = 0 }

let fromStr str =
    if String.IsNullOrEmpty(str) then
        { Lines = [||]; Position = initalPos }
    else
        let separators = [| "\r\n"; "\n" |]

        let lines =
            str.Split(separators, StringSplitOptions.None)

        { Lines = lines; Position = initalPos }

let currentLine inputState =
    let linePos = inputState.Position.Line
    if linePos < inputState.Lines.Length then inputState.Lines.[linePos] else "end of file"

let atEndOfInput input =
    let linePos = input.Position.Line
    let colPos = input.Position.Column
    linePos = input.Lines.Length
    || (linePos = (input.Lines.Length - 1)
        && colPos >= (currentLine input).Length)

let nextChar input =
    let linePos = input.Position.Line
    let colPos = input.Position.Column
    if linePos >= input.Lines.Length then
        input, None
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.Position
            let newState = { input with Position = newPos }
            newState, Some char
        else
            let char = '\n'
            let newPos = incrLine input.Position
            let newState = { input with Position = newPos }
            newState, Some char

let charAtOffset offset input = 
    let colPos = input.Position.Column + offset
    if colPos > 0 then
        let line = currentLine input
        Some line.[colPos]
    else
        None

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
    { CurrentLine = currentLine inputState
      Line = inputState.Position.Line
      Column = inputState.Position.Column }

let resultString result =
    match result with
    | Success (value, input) -> sprintf "%A" value
    | Failure (label, error, parserPos) ->
        let errorLine = parserPos.CurrentLine
        let colPos = parserPos.Column
        let linePos = parserPos.Line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let printResult result = printfn "%s" (resultString result)

(*
** Running
*)



let runOnInput parser input = parser.ParseFn input

let run parser input = runOnInput parser (fromStr input)

(*
** Utilities
*)



let createParserForwardedToRef<'a> () =

    let dummyParser =
        let innerFn input: Result<'a * InputState> = failwith "unfixed forwarded parser"
        { ParseFn = innerFn; Label = "unknown" }

    // ref to placeholder Parser
    let parserRef = ref dummyParser

    // wrapper Parser
    let innerFn input =
        // forward input to the placeholder
        runOnInput !parserRef input

    let wrapperParser = { ParseFn = innerFn; Label = "unknown" }

    wrapperParser, parserRef


let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.ParseFn input
        match result with
        | Success s -> Success s

        | Failure (oldLabel, err, pos) -> Failure(newLabel, err, pos)

    { ParseFn = newInnerFn
      Label = newLabel }

let getLabel p = p.Label

let (<?>) = setLabel

let withPosition parser =
    let newInnerFn input =
        match parser.ParseFn input with
        | Success (value1, remainingInput) -> Success((input.Position, value1), remainingInput)
        | Failure (label, err, pos) -> Failure(label, err, pos)

    { ParseFn = newInnerFn
      Label = parser.Label }

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

    { ParseFn = innerFn; Label = label }

let (>>=) p f = bindP f p

let returnP x =
    let label = "unknown"
    let innerFn input = Success(x, input)

    { ParseFn = innerFn; Label = label }

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

    { ParseFn = innerFn
      Label = "one or more " + parser.Label }

let many1 p =
    p
    >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail)))

let applyP fP xP =
    fP >>= (fun f -> xP >>= (f >> returnP))

let orElse p1 p2 =
    let innerFn input =
        let result1 = runOnInput p1 input
        match result1 with
        | Failure (label, err, pos) -> runOnInput p2 input
        | Success res -> result1

    { ParseFn = innerFn
      Label = sprintf "%s orElse %s" p1.Label p2.Label }

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

    { ParseFn = innerFn; Label = label }

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