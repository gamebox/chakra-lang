#load "ParserLibrary.fsx"

open System
open ParserLibrary

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

let jNull = pstring "null" >>% JNull <?> "null"

run jNull "nil" |> printResult


let jBool =
    let jtrue = pstring "true" >>% JBool true

    let jfalse = pstring "false" >>% JBool false

    jtrue <|> jfalse <?> "bool"

let jUnescapedChar =
    let label = "char"

    satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

let jEscapedChar =
    [ ("\\\"", '\"') // quote
      ("\\\\", '\\') // reverse solidus
      ("\\/", '/') // solidus
      ("\\b", '\b') // backspace
      ("\\f", '\f') // formfeed
      ("\\n", '\n') // newline
      ("\\r", '\r') // cr
      ("\\t", '\t') ] // tab
    |> List.map (fun (toMatch, result) -> pstring toMatch >>% result)
    |> choice
    <?> "escaped char"

let jUnicodeChar =
    let backslash = pchar '\\'
    let uChar = pchar 'u'

    let hexdigit =
        anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])

    let convertToChar (((h1, h2), h3), h4) =
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str, Globalization.NumberStyles.HexNumber)
        |> char

    backslash
    >>. uChar
    >>. hexdigit
    .>>. hexdigit
    .>>. hexdigit
    .>>. hexdigit
    |>> convertToChar

let jString =
    let validChars =
        jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

    between pdoublequote (many validChars) pdoublequote
    |>> charListToStr
    |>> JString
    <?> "quoted string"

let (|>?) opt f =
    match opt with
    | None -> ""
    | Some x -> f x

let jNumber =
    let convertToJNumber (((sign, integer), fraction), exponent) =
        let signStr = sign |>? string

        let fractionStr =
            fraction |>? (fun digits -> "." + digits)

        let exponentStr =
            exponent
            |>? (fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits)

        (signStr + integer + fractionStr + exponentStr)
        |> float
        |> JNumber

    let optSign = opt (pchar '-')
    let zero = pstring "0"

    let digitOneNine =
        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"

    let point = pchar '.'

    let e = pchar 'e' <|> pchar 'e'

    let optPlusMinus = opt (pchar '-' <|> pchar '+')

    let nonZeroInt =
        digitOneNine
        .>>. manyChars digit
        |>> fun (first, rest) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionPart = point >>. manyChars1 digit

    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit

    optSign
    .>>. intPart
    .>>. opt fractionPart
    .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"


let jNumber_ =
    jNumber .>> (pchar ',' <|> whitespaceChar)

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


let jValue, jValueRef = createParserForwardedToRef<JValue> ()

let jArray =
    let left = pchar '[' .>> spaces
    let right = pchar ']' .>> spaces
    let comma = pchar ',' .>> spaces
    let value = jValue .>> spaces

    let values = sepBy1 value comma

    between left values right |>> JArray <?> "array"

let jObject =
    let quotedString =
        let quote = pchar '\"' <?> "quote"

        let jchar =
            jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

        between quote (manyChars jchar) quote

    let left = pchar '{' .>> spaces
    let right = pchar '}' .>> spaces
    let colon = pchar ':' .>> spaces
    let comma = pchar ',' .>> spaces
    let key = quotedString .>> spaces
    let value = jValue .>> spaces

    let keyValue = (key .>> colon) .>>. value
    let keyValues = sepBy1 keyValue comma

    between left keyValues right
    |>> Map.ofList
    |>> JObject
    <?> "object"

jValueRef
:= choice
    [ jNull
      jBool
      jNumber
      jString
      jArray
      jObject ]


let example1 =
    """{ "name" : "Scott", "isMale" : true, "bday" : {"year":2001, "month":12, "day":25 }, "favouriteColors" : ["blue", "green"] }"""

run jValue example1

let example2 =
    """{"widget": { "debug": "on", "window": { "title": "Sample Konfabulator Widget", "name": "main_window", "width": 500, "height": 500 }, "image": { "src": "Images/Sun.png", "name": "sun1", "hOffset": 250, "vOffset": 250, "alignment": "center" }, "text": { "data": "Click Here", "size": 36, "style": "bold", "name": "text1", "hOffset": 250, "vOffset": 100, "alignment": "center", "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;" } }} """

run jValue example2
