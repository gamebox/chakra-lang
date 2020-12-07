module ChakraParser

open System
open ParserLibrary


(* Types *)

type FunctionBindPatternInfo = { Name: string ; Args: string list }

type ChakraIdentifier = ChakraIdentifier of string

type ChakraComment = { IsDoc: bool ; Content: string }

/// type PatternMatchInfo =

type ChakraBindingPattern =
    | ChakraSimpleBindingPattern of string
    | ChakraFunctionBindingPattern of FunctionBindPatternInfo
// | ChakraComplexBindingPattern of ChakraPattern

and ChakraLiteral =
    | ChakraVar of (string * (string list) option)
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

and ChakraBinding = ChakraBinding of (Position * ChakraBindingPattern * ChakraExprList * (string option))

and ChakraExprList = ChakraExprList of (ChakraBinding list * ChakraExpr)

and ChakraExpr =
    | ChakraLiteralExpr of (Position * ChakraLiteral)
    | ChakraMatchExpr of (Position * ChakraMatch)
    | ChakraApplyExpr of (Position * ChakraApply)
    // This expr type is for builtins.  The string is an identifier for the builtin.
    // It is illegal for these to be referenced in userland code.
    | ChakraNativeExpr of (string)

and ChakraApply = ChakraApply of ((string * (string list) option) * ChakraExpr list)

type ChakraImportBindingType =
    | ChakraSimpleImportBinding of string
    | ChakraDestructuredImportBinding of Map<string, string>

type ChakraLocalImportInfo = {
    Library: string
    Typ: ChakraImportBindingType
    Relative: bool
}

type ChakraPackageImportInfo = {
    PackageName: string
    Typ: ChakraImportBindingType
}
type ChakraImport =
    | ChakraLocalImport of ChakraLocalImportInfo
    | ChakraPackageImport of ChakraPackageImportInfo

type ChakraModule = {
    DocComments: string option
    Exports: string list
    Bindings: ChakraBinding list
    Imports: ChakraImport list }


(* Create Refs *)


let chakraLiteral, chakraLiteralRef =
    createParserForwardedToRef<ChakraLiteral> ()

let chakraExpr, chakraExprRef =
    createParserForwardedToRef<ChakraExpr> ()

let chakraExprList, chakraExprListRef =
    createParserForwardedToRef<ChakraExprList> ()


(* Basic punctuation *)

let altSigil = pchar '%'

let leftParen =
    pchar '(' .>> whitespace <?> "left parenthesis"

let structStart =
    altSigil .>> leftParen <?> "start of struct"

let rightParen =
    whitespace >>. pchar ')' <?> "right parenthesis"

let leftBracket =
    pchar '[' .>> whitespace <?> "left bracket"

let mapStart =
    altSigil .>> leftBracket <?> "start of map"

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

(*
    An identifier must follow this regex:
    ([A-Za-z]+[a-z]* )(\-{1,2}[A-Za-z]+[a-z]* )*[\?\!\*]{0,1}
*)
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
    .>>. opt (many (pchar '.' >>. pBaseIdentifier ))
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

let pValidChars =
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
    between pdoublequote (many pValidChars) pdoublequote
    |>> charListToStr
    |>> ChakraString
    <?> "string"

let pIgnore = pchar '_' <?> "ignore"

let pRest = pstring "..." <?> "rest"

let containerItemSeparator =
    pchar ',' .>> whitespace1

let emptyContainer start end' =
    start .>> end'

let container start end' item =
    let noTrailing = sepBy1 item containerItemSeparator
    let withTrailing = many1 (item .>> containerItemSeparator)
    between start (noTrailing <|> withTrailing) end'

let pTuple =
    let emptyTuple = (emptyContainer leftParen rightParen) >>% []
    let nonEmptyTuple = (container leftParen rightParen chakraExpr)
    emptyTuple <|> nonEmptyTuple

let chakraTuple = pTuple |>> ChakraTuple <?> "tuple"

let chakraStruct =
    let structPair =
        pBaseIdentifier .>> equal .>>. chakraExpr

    container structStart rightParen structPair
    |>> ChakraStruct
    <?> "struct"


let chakraList =
    container leftBracket rightBracket chakraExpr
    |>> ChakraList
    <?> "list"

let chakraMap =
    let mapPair =
        (chakraLiteral .>> equal) .>>. chakraExpr

    let nonEmptyMap =
        container mapStart rightBracket mapPair
        |>> ChakraMap

    let emptyMap =
        emptyContainer mapStart rightBracket
        >>% ChakraMap []

    emptyMap <|> nonEmptyMap <?> "map"

let chakraLambda =
    let stringTuple =
        container leftParen rightParen pBaseIdentifier
    let createRecord (args, body) = { Args = args; Body = body }
    between leftCurly (stringTuple .>> arrow .>>. chakraExprList) rightCurly
    |>> createRecord
    |>> ChakraLambda

(* Expressions *)

let flattenTuple (a, (b, c)) = (a, b, c)

let chakraApply =
    pBaseIdentifier
    .>>. opt (many1 (pchar '.' >>. pBaseIdentifier))
    .>>. container leftParen rightParen chakraExpr
    |>> ChakraApply
    <?> "apply"

let chakraMatchClause =
    // We need to be able to parse other forms to make literals
    // The basic literals: strings, numbers, and symbols
    // Tuples like: ( a b _ )
    //
    // Structs like:
    // %(
    //     a = b
    //     c
    // ..)
    //
    // Maps like:
    // %[
    //     %( foo = "bar" ) = x ; Destructuring keys
    // ...] ; May have other keys
    // or like:
    // %[
    //    y = x ; where y is bound already
    // ...]
    // or like:
    // %[
    //    "foo" = bar
    // ] ; exact form
    //
    // Lists like:
    // [ a _ c ...rest ] ; binding to some elements, ignoring others, collecting the tail
    pipe
    >>. chakraLiteral
    .>> arrow
    .>>. chakraExprList
    .>> whitespace1
    |>> ChakraMatchClause
    <?> "match clause"

let chakraMatch =
    (chakraLiteral .>> questionMark)
    .>> whitespace1
    .>>. many1 chakraMatchClause
    |>> ChakraMatch

let chakraBindingPattern =
    let simple =
        pBaseIdentifier
        |>> ChakraSimpleBindingPattern
        <?> "simple binding pattern"

    let func =
        pBaseIdentifier
        .>> leftParen
        .>>. sepBy pBaseIdentifier containerItemSeparator
        .>> rightParen
        |>> fun (name, args) -> ChakraFunctionBindingPattern { Name = name ; Args = args }
        <?> "function binding pattern"

    simple <|> func
    <?> "binding pattern"

let rec chakraBinding =
    (chakraBindingPattern .>> equal)
    .>>. chakraExprList
    |> withPosition
    |>> flattenTuple
    |>> fun (p, b, e) -> ChakraBinding (p, b, e, None)
    <?> "Binding"

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
    >>% '\n'
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
let cleanLine (s: string) = s.TrimStart ([| ' ' |])
let docComment =
    let docCommentLine =
        (semi .>>. semi)
        >>. many notNewline
        .>> pchar '\n'
        |>> charListToStr
        |>> cleanLine

    many1 docCommentLine
    |>> fun lines -> { IsDoc = false ; Content = String.concat "\n" lines }
    <?> "Doc comment"

let newline = pchar '\n'

let deadspace =
    (spaces1
    .>>. whitespace
    .>>.many (lineComment <|> newline)
    >>% '\n')
    <|> ((newline .>>. whitespace) >>% '\n')


(* Imports *)
let pRootImport =
    pstring "/root"
    >>. pchar '/' >>. pBaseIdentifier
    <?> "Root import"

let pPackageImport =
    pchar '/'
    >>. pBaseIdentifier
    <?> "Package import"

let pRelativeImport =
    pstring "./"
    >>. pBaseIdentifier
    <?> "Relative import"

let pImportDestructuring =
    between (structStart .>> (whitespace)) (sepBy pBaseIdentifier (comma .>> whitespace1)) (opt (comma) .>> whitespace .>> rightParen)
    <?> "Import destructuring"

let chakraImport =
    let simple = pBaseIdentifier |>> ChakraSimpleImportBinding
    let destructured =
        let addBinding map binding = Map.add binding binding map
        let createBindings bindings =
            List.fold addBinding (Map.empty) bindings

        pImportDestructuring
        |>> (createBindings >> ChakraDestructuredImportBinding)

    let binding = simple <|> destructured
    let rootImport (typ, string) = ChakraLocalImport { Library = string ; Typ = typ ; Relative = false }
    let relativeImport (typ, string) = ChakraLocalImport { Library = string ; Typ = typ ; Relative = true }
    let packageImport (typ, string) = ChakraPackageImport { PackageName = string ; Typ = typ }

    (binding .>> equal .>>. pRootImport |>> rootImport)
    <|> (binding .>> equal .>>. pPackageImport |>> packageImport)
    <|> (binding .>> equal .>>. pRelativeImport |>> relativeImport)
    <?> "import"

(* Modules *)

let chakraModuleDef =
    let tupleLike =
        between structStart (sepBy pBaseIdentifier (comma .>> whitespace1)) (opt (comma) .>> rightParen)

    rawEqual
    .>> pchar ' '
    >>. tupleLike
    .>> many1 newline
    <?> "Module export definition"

let chakraModule =
    let possibleImportSection =
        (many1 (chakraImport .>> (many1 newline)))

    let buildModule (((c, exports), imports), bindings) =
        let comment =
            match c with
            | None -> None
            | Some ({ Content = com}) -> Some com
        {
            DocComments = comment
            Exports = exports
            Bindings = bindings
            Imports = imports
        }
    
    let topLevelBindings =
        let content ({ Content = c ; IsDoc = _ }) = c
        many1 (opt docComment .>>. chakraBinding |>> fun (optComment, (ChakraBinding (p, b, e, _))) -> ChakraBinding (p, b, e, Option.map (content) optComment))

    opt docComment
    .>>. chakraModuleDef
    .>>. possibleImportSection
    .>>. topLevelBindings
    |>> buildModule
    <?> "module"

(* Set refs *)

chakraLiteralRef
:= choice [ chakraVar
            chakraNumber
            chakraString
            chakraSymbol
            chakraTuple
            chakraList
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
:= many (chakraBinding .>> (many1 newline))
.>>. chakraExpr
.>> many1 newline
|>> ChakraExprList
<?> "expression list"
