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

type ParserResult<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

let toResult parserResult =
    match parserResult with
    | Success a -> Ok(a)
    | Failure (label, err, p) -> Error(label, err, p)

type Position = { Line: int; Column: int }
type Span = { Start: Position; End: Position }

type InputState =
    { Lines: string []
      Position: Position }

type Parser<'a> =
    { ParseFn: InputState -> ParserResult<'a * InputState>
      Label: ParserLabel }

(*
    Result utils
*)

let resultMap f (r: ParserResult<'a>) =
    match r with
    | Success a -> Success(f a)
    | Failure (l, e, p) -> Failure(l, e, p)

let resultfFMap (f: (ParserResult<'a>) -> ParserResult<'b>) (r: ParserResult<'a>) =
    match r with
    | Failure (l, e, p) -> Failure(l, e, p)
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

    if linePos < inputState.Lines.Length then
        inputState.Lines.[linePos]
    else
        "end of file"

let lineOffset inputState offset =
    let linePos = inputState.Position.Line - offset

    if linePos < 0 || linePos > inputState.Lines.Length then
        None
    else
        Some inputState.Lines.[linePos]

let windowAroundLine inputState size linePos =
    let collectLines acc i =
        let lineNo = linePos + i

        match lineOffset inputState lineNo with
        | None -> acc
        | Some l -> (lineNo, l) :: acc

    let before = List.fold collectLines [] [ 0 .. size ]
    let after = List.fold collectLines [] [ 0 .. -size ]
    (before, (linePos, (currentLine inputState)), after)

let windowAroundCurrent inputState size =
    let linePos = inputState.Position.Line
    windowAroundLine inputState size linePos

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
    let createDiagnosticLine pos label diag =
        let errorLine = pos.CurrentLine
        let colPos = pos.Column
        let linePos = pos.Line
        let failureCaret = sprintf "%*s^%s" colPos "" diag

        sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret
        |> CConsole.red

    match result with
    | Success (value, input) when atEndOfInput input -> sprintf "%A" value |> CConsole.green
    | Success (_, is) ->
        let diag = "Ended parsing here"

        let pos =
            { Line = is.Position.Line
              Column = is.Position.Column
              CurrentLine = (currentLine is) }

        createDiagnosticLine pos "Parser stopped unexpectedly" diag
    | Failure (label, error, parserPos) -> createDiagnosticLine parserPos label error

let printResult result = printfn "%s" (resultString result)

(*ParserResult
** Running
*)



let runOnInput parser input =
    // printfn "[PARSER START] %s" parser.Label
    let r = parser.ParseFn input
    // match r with
    // | Success _ -> printfn "[PARSER SUCCESS] %s" parser.Label
    // | Failure _ -> printfn "[PARSER FAILED] %s" parser.Label
    r

let run parser input = runOnInput parser (fromStr input)

(*
** Utilities
*)



let createParserForwardedToRef<'a> () =

    let dummyParser =
        let innerFn input : ParserResult<'a * InputState> = failwith "unfixed forwarded parser"
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


let timed parser =
    let newInnerFn input =
        let timer = System.Diagnostics.Stopwatch.StartNew ()
        printfn "[%i:%i] Trying to parse %s" input.Position.Line input.Position.Column parser.Label
        match parser.ParseFn input with
        | Success (_, i) as r ->
            timer.Stop ()
            printfn "[%i:%i-%i:%i] Parsed %s in %i ms" input.Position.Line input.Position.Column i.Position.Line i.Position.Column parser.Label timer.ElapsedMilliseconds
            r
        | Failure (_, _, pos) as r ->
            timer.Stop ()
            printfn "[%i:%i] Failed to parse %s after %i ms" input.Position.Line input.Position.Column parser.Label timer.ElapsedMilliseconds
            r
    
    { ParseFn = newInnerFn ; Label = parser.Label }

let withSpan parser =
    let newInnerFn input =
        match parser.ParseFn input with
        | Success (value1, remainingInput) ->
            let span =
                { Start = input.Position
                  End = remainingInput.Position }

            Success((span, value1), remainingInput)
        | Failure (label, err, pos) -> Failure(label, err, pos)

    { ParseFn = newInnerFn
      Label = parser.Label }


(*
** Combinators
*)


let bindP f p =
    let label = sprintf "bindP[%s]" (getLabel p)

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
    let label = sprintf "returnP[%A]" x
    let innerFn input = Success(x, input)

    { ParseFn = innerFn; Label = label }

let mapP f = bindP (f >> returnP)

let andThen p1 p2 =
    p1
    >>= (fun p1Result ->
        p2
        >>= (fun p2Result -> returnP (p1Result, p2Result)))
    <?> sprintf "(%s and then %s)" (getLabel p1) (getLabel p2)

let (.>>.) = andThen

let parseZeroOrMore parser input : ('a list * InputState) =
    let rec inner parser input (values: 'a list) : ('a list * InputState) =
        match runOnInput parser input with
        | Failure (_) -> (List.rev values, input)
        | Success (firstValue, inputAfterFirstParse) -> inner parser inputAfterFirstParse (firstValue :: values)

    inner parser input []

let many parser =
    let rec innerFn input = Success(parseZeroOrMore parser input)

    { ParseFn = innerFn
      Label = sprintf "[%s]* " (getLabel parser) }

let many1 p =
    p
    >>= (fun head -> many p >>= (fun tail -> returnP (head :: tail)))
    <?> sprintf "[%s]+" (getLabel p)

let applyP fP xP = fP >>= (fun f -> xP >>= (f >> returnP))

let orElse p1 p2 =
    let innerFn input =
        let result1 = runOnInput p1 input

        match result1 with
        | Failure (label, err, pos) -> runOnInput p2 input
        | Success res -> result1

    { ParseFn = innerFn
      Label = sprintf "(%s or else %s)" p1.Label p2.Label }

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

let rec peeker str input i =
    let remainingInput, charOpt = nextChar input

    match charOpt with
    | None ->
        None

    | Some c ->
        let str' = sprintf "%s%c" str c
        if i = 0 then
            Some (str', remainingInput)
        else
            peeker str' remainingInput (i - 1)

let rec consumer str input (regex: System.Text.RegularExpressions.Regex) =
    let remainingInput, charOpt = nextChar input

    match charOpt with
    | None ->
        (str, input)

    | Some c ->
        let str' = sprintf "%s%c" str c
        if not (regex.IsMatch str') then
            (str, input)
        else
            consumer str' remainingInput regex


let regex (rx: System.Text.RegularExpressions.Regex) (peek: int) =
    let label = rx.ToString ()
    let innerFn input =
        // Collect `peek` chars into a string and then test them
        // against the regex
        match peeker "" input peek with
        | Some (str, input') ->
            if rx.IsMatch str then
                
                Success (str, input')
            else
                let err = sprintf "Failed"
                let pos = parserPositionFromInputState input
                Failure (label, err, pos) 
        | None ->
            let err = sprintf "Expected at least %i characters" peek
            let pos = parserPositionFromInputState input
            Failure (label, err, pos)
        // If it matches, keep taking until it no longer matches

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
    <?> sprintf "optionally a(n) %s" p.Label

let (.>>) p1 p2 = p1 .>>. p2 |> mapP (fun (a, b) -> a)

let (>>.) p1 p2 = p1 .>>. p2 |> mapP (fun (a, b) -> b)

let between p1 p2 p3 = p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP =
        sep >>. p
        <?> (sprintf "(%s followed by %s)" (getLabel sep) (getLabel p))

    p .>>. many sepThenP
    |>> fun (p, pList) -> p :: pList

let sepBy p sep = sepBy1 p sep <|> returnP []

let (>>%) p x = p |>> (fun _ -> x) <?> p.Label



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
    listOfChars |> List.map pchar |> choice
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

let whitespace =
    many whitespaceChar <?> "zero or more whitespace"

let whitespace1 =
    many1 whitespaceChar <?> "one or more whitespace"

let digit =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let digits = many1 digit <?> "digits"

let pint =
    let resultToInt (sign, digitList) =
        let i = String(List.toArray digitList) |> int

        match sign with
        | Some ch -> -i
        | None -> i

    opt (pchar '-') .>>. digits |>> resultToInt
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
