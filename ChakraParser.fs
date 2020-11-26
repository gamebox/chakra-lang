module ChakraParser

open System
open ParserLibrary


(* Types *)

type FunctionBindPatternInfo = { Name: string; Args: string list }

type ChakraIdentifier = ChakraIdentifier of string

type ChakraComment = { IsDoc: bool ; Content: string }

type ChakraBindingPattern =
    | ChakraSimpleBindingPattern of string
    | ChakraFunctionBindingPattern of FunctionBindPatternInfo
// | ChakraComplexBindingPattern of ChakraPattern
type ChakraLiteral =
    | ChakraVar of string
    | ChakraNumber of float
    | ChakraSymbol of string
    | ChakraString of string
    | ChakraTuple of ChakraExpr list
    | ChakraStruct of (string * ChakraExpr) list
    | ChakraList of ChakraExpr list
    | ChakraMap of (ChakraLiteral * ChakraExpr) list
    | ChakraVector of ChakraExpr list
    | ChakraLambda of ChakraLambda

and ChakraLambda = { Args: string list; Body: ChakraExprList }

and ChakraMatch = ChakraMatch of (ChakraLiteral * ChakraMatchClause list)

and ChakraMatchClause = ChakraMatchClause of (ChakraLiteral * ChakraExprList)

and ChakraBinding = ChakraBinding of (Position * ChakraBindingPattern * ChakraExprList)

and ChakraExprList = ChakraExprList of (ChakraBinding list * ChakraExpr)

and ChakraExpr =
    | ChakraLiteralExpr of (Position * ChakraLiteral)
    | ChakraMatchExpr of (Position * ChakraMatch)
    | ChakraApplyExpr of (Position * ChakraApply)
    // This expr type is for builtins.  The string is an identifier for the builtin.
    // It is illegal for these to be referenced in userland code.
    | ChakraNativeExpr of (string)

and ChakraApply = ChakraApply of (string * ChakraExpr list)


type ChakraModule = { DocComments: string option ; Exports: string list ; Bindings: ChakraBinding list }


(* Create Refs *)


let chakraLiteral, chakraLiteralRef =
    createParserForwardedToRef<ChakraLiteral> ()

let chakraExpr, chakraExprRef =
    createParserForwardedToRef<ChakraExpr> ()

let chakraExprList, chakraExprListRef =
    createParserForwardedToRef<ChakraExprList> ()


(* Basic punctuation *)


let leftParen =
    pchar '(' .>> whitespace <?> "left parenthesis"

let rightParen =
    whitespace >>. pchar ')' <?> "right parenthesis"

let leftBracket =
    pchar '[' .>> whitespace <?> "left bracket"

let rightBracket =
    whitespace >>. pchar ']' <?> "right bracket"

let leftCurly =
    pchar '{' .>> whitespace <?> "left curly brace"

let rightCurly =
    whitespace >>. pchar '}' <?> "right curly brace"

let matchOperator =
    whitespace1
    >>. pchar '?'
    .>> whitespace1
    <?> "match operator"

let patternSep =
    pchar '|' .>> whitespace1 <?> "pattern separator"

let patternArrow =
    whitespace1
    >>. pstring "->"
    .>> whitespace1
    <?> "pattern arrow"

let rawEqual = pchar '=' <?> "equal sign"

let equal =
    whitespace1
    >>. pchar '='
    .>> whitespace1
    <?> "bind operator"

let pipe =
    pchar '|' .>>. whitespace1 <?> "pattern separator"

let arrow =
    whitespace1
    .>>. pstring "->"
    .>> whitespace1
    <?> "pattern arrow"

let questionMark =
    whitespace1
    .>>. pchar '?'
    .>>. whitespace1
    <?> "match separator"

let semi =
    pstring ";"
    <?> "semicolon"

(* Literals *)


let pBaseIdentifier =
    let upperOrLowerAlpha = anyOf ([ 'A' .. 'Z' ] @ [ 'a' .. 'z' ])
    let lowerAlpha = anyOf [ 'a' .. 'z' ]
    let dash = pchar '-'
    let segment = upperOrLowerAlpha .>>. many lowerAlpha
    let questionOrBangOrStar = anyOf [ '?'; '!'; '*' ]

    let convertToIdentifier (((firstAlpha, restOfFirstSegment), otherSegments), maybeSign) =
        let firstSegment =
            charListToStr (firstAlpha :: restOfFirstSegment)

        let segmentPart =
            otherSegments
            |> List.map (fun (head, (firstLetter, rest)) -> sprintf "%c%c%s" head firstLetter (charListToStr rest))
            |> List.fold (+) ""

        let signPart =
            match maybeSign with
            | Some ch -> String([| ch |])
            | None -> ""

        firstSegment + segmentPart + signPart


    segment
    .>>. many (dash .>>. segment)
    .>>. opt questionOrBangOrStar
    |>> convertToIdentifier
    <?> "identifier /([A-Za-z]+[a-z]*)(\-{1,2}[A-Za-z]+[a-z]*)*[?!\*]{0,1}/"

let chakraVar =
    pBaseIdentifier
    |>> ChakraVar
    <?> "valid identifier"

let chakraSymbol =
    pchar '#'
    >>. pBaseIdentifier
    |>> ChakraSymbol
    <?> "symbol"

let (|>?) opt f =
    match opt with
    | None -> ""
    | Some x -> f x

let chakraNumber =
    let convertToNumber (((sign, integer), fraction), exponent) =
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
        |> ChakraNumber

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
    |>> convertToNumber
    <?> "number"

let validChars =
    let unescapedChar =
        let label = "char"

        satisfy (fun ch -> ch <> '\\' && ch <> '\"') label

    let escapedChar =
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

    let unicodeChar =
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

    unescapedChar <|> escapedChar <|> unicodeChar

let chakraString =
    between pdoublequote (many validChars) pdoublequote
    |>> charListToStr
    |>> ChakraString
    <?> "string"

let chakraIgnore = pchar '_' <?> "ignore"

let chakraRest = pstring "..." <?> "rest"

let rawTuple =
    between leftParen (sepBy chakraExpr whitespace) rightParen

let chakraTuple = rawTuple |>> ChakraTuple <?> "tuple"

let chakraStruct =
    let structPair =
        (pBaseIdentifier .>> equal) .>>. chakraExpr

    between leftParen (sepBy1 structPair whitespace) rightParen
    |>> ChakraStruct
    <?> "struct"


let chakraList =
    between leftBracket (sepBy chakraExpr whitespace) rightBracket
    |>> ChakraList
    <?> "list"

let chakraMap =
    let mapPair =
        (chakraLiteral .>> equal) .>>. chakraExpr

    let nonEmptyMap =
        between leftBracket (sepBy mapPair whitespace) rightBracket
        |>> ChakraMap

    let emptyMap =
        between leftBracket equal rightBracket
        >>% ChakraMap []

    emptyMap <|> nonEmptyMap <?> "map"

let chakraVector =
    between leftCurly (sepBy chakraExpr whitespace) rightCurly
    |>> ChakraVector
    <?> "vector"

let chakraLambda =
    let stringTuple =
        between leftParen (sepBy pBaseIdentifier whitespace) rightParen
    let createRecord (args, body) = { Args = args; Body = body }
    between leftCurly (stringTuple .>> arrow .>>. chakraExprList) rightCurly
    |>> createRecord
    |>> ChakraLambda

(* Expressions *)

let flattenTuple (a, (b, c)) = (a, b, c)

let chakraApply =
    ((pBaseIdentifier |>> string) .>> leftParen)
    .>>. sepBy chakraExpr whitespace
    .>> rightParen
    |>> ChakraApply
    <?> "apply"

let chakraMatchClause =
    pipe
    >>. chakraLiteral
    .>> arrow
    .>>. chakraExprList
    .>> whitespace
    |>> ChakraMatchClause
    <?> "match clause"

let chakraMatch =
    (chakraLiteral .>> questionMark)
    .>> whitespace
    .>>. many1 chakraMatchClause
    |>> ChakraMatch

let chakraBindingPattern =
    pBaseIdentifier |>> ChakraSimpleBindingPattern

let rec chakraBinding =
    (chakraBindingPattern .>> equal)
    .>>. chakraExprList
    |> withPosition
    |>> flattenTuple
    |>> ChakraBinding

let chakraLiteralExpr =
    chakraLiteral
    |> withPosition
    |>> ChakraLiteralExpr
    <?> "literal expression"

let chakraApplyExpr =
    chakraApply
    |> withPosition
    |>> ChakraApplyExpr
    <?> "function application"


let chakraMatchExpr =
    chakraMatch
    |> withPosition
    |>> ChakraMatchExpr
    <?> "match expression"

(* Comments *)

let notNewline =
    satisfy (fun c -> c <> '\n') "not a newline"
(*
    A line comment looks like this:

    ```chakra
    x = 1 ; This is a comment
    ```

    It starts with a semicolon followed by a space, and
    continues to the end of the line
*)
let lineComment =
    semi
    >>. many notNewline
    .>> pchar '\n'
    |>> charListToStr
    |>> fun content -> { IsDoc = false ; Content = content }
    <?> "Line comment"

(*
    A doc comment looks like this:

    ```chakra
    ;; This is a doc comment
    ;;
    ;; It will appear in generated documentation for this module,
    ;; as well as in intellisense documentation.
    x = 1
    ```

    Any number of such lines may appear in sequence at the top of a module, or
    above any top level binding
*)
let docComment =
    let docCommentLine =
        (semi .>>. semi)
        >>. many notNewline
        .>> pchar '\n'
        |>> charListToStr

    many1 docCommentLine
    |>> fun lines -> { IsDoc = false ; Content = String.concat "\n" lines }
    <?> "Doc comment"

let newline = pchar '\n'

(* Modules *)
let chakraModuleDef =
    let tupleLike =
        between leftParen (sepBy pBaseIdentifier whitespace1) rightParen

    rawEqual
    .>> pchar ' '
    >>. tupleLike
    .>> many1 newline
    <?> "Module export definition"

let chakraModule =
    opt docComment
    .>>. chakraModuleDef
    .>>. many1 chakraBinding
    |>> (fun ((c, exports), bindings) ->
        let comment =
            match c with
            | None -> None
            | Some ({ Content = com}) -> Some com
        {
            DocComments = comment
            Exports = exports
            Bindings = bindings
        })
    <?> "module"

(* Set refs *)

chakraLiteralRef
:= choice [ chakraVar
            chakraNumber
            chakraString
            chakraSymbol
            chakraTuple
            chakraList
            chakraVector
            chakraStruct
            chakraMap
            chakraLambda ]
<?> "literal"

chakraExprRef
:= chakraApplyExpr
<|> chakraMatchExpr
<|> chakraLiteralExpr
<?> "expression"

chakraExprListRef
:= many chakraBinding
.>>. chakraExpr
.>> whitespace
|>> ChakraExprList
<?> "expression list"
