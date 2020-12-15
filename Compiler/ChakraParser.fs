module ChakraParser

open System
open ParserLibrary


(* Types *)

type ChakraPattern =
    | CPVar of Span * string
    | CPNumber of Span * float
    | CPSymbol of Span * string
    | CPString of Span * string
    | CPTuple of Span * ChakraPattern list
    | CPStruct of Span * CPStructPair list
    | CPList of Span * ChakraPattern list
    | CPMap of Span * CPMapPair list

and CPStructPair =
    { FieldName: string
      FieldValue: ChakraPattern }

and CPMapPair =
    { Key: ChakraPattern
      Value: ChakraPattern }

type FunctionBindPatternInfo = { Name: string; Args: string list }

type ChakraIdentifier = ChakraIdentifier of string

type ChakraComment = { IsDoc: bool; Content: string }

/// type PatternMatchInfo =

type ChakraBindingPattern =
    | ChakraSimpleBindingPattern of string
    | ChakraFunctionBindingPattern of FunctionBindPatternInfo
    | ChakraComplexBindingPattern of ChakraPattern

and ChakraLiteral =
    | ChakraVar of (string * (string list) option)
    | ChakraNumber of float
    | ChakraSymbol of string
    | ChakraString of string
    | ChakraTuple of ChakraExpr list
    | ChakraStruct of ChakraStruct
    | ChakraList of ChakraList
    | ChakraMap of ChakraMap
    | ChakraLambda of ChakraLambda

and ChakraVar =
    { First: string
      Rest: (string list) option }

and ChakraStructField =
    { Loc: Span
      Name: string
      Value: ChakraExpr }

and ChakraStruct =
    { Fields: ChakraStructField list
      Spread: (Span * ChakraVar) option }

and ChakraMapPair =
    { Loc: Span
      Key: ChakraLiteral
      Value: ChakraExpr }

and ChakraMap =
    { Pairs: ChakraMapPair list
      Spread: (Span * ChakraVar) option }

and ChakraList =
    { Items: ChakraExpr list
      Spread: (Span * ChakraVar) option}

and ChakraLambda =
    { Args: string list
      Body: ChakraExprList }

and ChakraMatch = ChakraMatch of (ChakraLiteral * ChakraMatchClause list)

and ChakraMatchClause = ChakraMatchClause of (ChakraLiteral * ChakraExprList)

and ChakraBinding =
    { Loc: Span
      Pattern: ChakraBindingPattern
      ExprList: ChakraExprList
      DocComment: string option }

and ChakraExprList = ChakraExprList of (ChakraBinding list * ChakraExpr)

and ChakraExpr =
    | ChakraLiteralExpr of (Span * ChakraLiteral)
    | ChakraMatchExpr of (Span * ChakraMatch)
    | ChakraApplyExpr of (Span * ChakraApply)
    // This expr type is for builtins.  The string is an identifier for the builtin.
    // It is illegal for these to be referenced in userland code.
    | ChakraNativeExpr of (string)

and ChakraApply = ChakraApply of ((string * string list) * ChakraExpr list)

type ChakraImportBindingType =
    | ChakraSimpleImportBinding of string
    | ChakraDestructuredImportBinding of Map<string, string>

type ChakraLocalImportInfo =
    { Library: string
      Typ: ChakraImportBindingType
      Relative: bool }

type ChakraPackageImportInfo =
    { PackageName: string
      Typ: ChakraImportBindingType }

type ChakraImport =
    | ChakraLocalImport of ChakraLocalImportInfo
    | ChakraPackageImport of ChakraPackageImportInfo

type ChakraModule =
    { DocComments: string option
      Exports: string list
      Bindings: ChakraBinding list
      Imports: ChakraImport list }


(* Create Refs *)

let chakraPattern, chakraPatternRef =
    createParserForwardedToRef<ChakraPattern> ()

let chakraLiteral, chakraLiteralRef =
    createParserForwardedToRef<ChakraLiteral> ()

let chakraExpr, chakraExprRef =
    createParserForwardedToRef<ChakraExpr> ()

let chakraExprList, chakraExprListRef =
    createParserForwardedToRef<ChakraExprList> ()


(* Comments *)

let semi = pstring ";" <?> "semicolon"

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
let newline = pchar '\n'

let lineComment =
    semi
    >>. many notNewline
    .>> newline
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
let cleanLine (s: string) = s.TrimStart([| ' ' |])

let docComment =
    let docCommentLine =
        (semi .>>. semi)
        >>. many notNewline
        .>> newline
        |>> charListToStr
        |>> cleanLine

    many1 docCommentLine
    |>> fun lines ->
            { IsDoc = false
              Content = String.concat "\n" lines }
    <?> "Doc comment"

let lineWhitespace =
    many (satisfy (fun c -> Char.IsWhiteSpace c && c <> '\n') "line whitespace")

let deadspace =
    many1
        ((lineWhitespace >>. lineComment)
         <|> (lineWhitespace >>. newline))
    .>> lineWhitespace
    <?> "deadspace"

let deadOrWhitespace = deadspace <|> whitespace1

(* Basic punctuation *)

let altSigil = pchar '%'

let leftParen =
    pchar '('
    .>> opt deadOrWhitespace
    <?> "left parenthesis"

let structStart =
    altSigil
    .>> leftParen
    .>> opt deadOrWhitespace
    <?> "start of struct"

let rightParen =
    whitespace >>. pchar ')' <?> "right parenthesis"

let leftBracket =
    pchar '['
    .>> opt deadOrWhitespace
    <?> "left bracket"

let mapStart =
    altSigil
    .>> pchar '['
    .>> opt deadOrWhitespace
    <?> "start of map"

let rightBracket =
    whitespace >>. pchar ']' <?> "right bracket"

let leftCurly =
    pchar '{'
    .>> opt deadOrWhitespace
    <?> "left curly brace"

let rightCurly =
    whitespace >>. pchar '}' <?> "right curly brace"

let matchOperator =
    whitespace1
    >>. pchar '?'
    .>> deadspace
    <?> "match operator"

let patternSep =
    pchar '|' .>> whitespace1 <?> "pattern separator"

let patternArrow =
    whitespace1
    >>. pstring "->"
    .>> deadOrWhitespace
    <?> "pattern arrow"

let rawEqual = pchar '=' <?> "equal sign"

let equal =
    whitespace1
    >>. pchar '='
    .>> deadOrWhitespace
    <?> "bind operator"

let pipe =
    pchar '|' .>>. whitespace1 <?> "pattern separator"

let arrow =
    whitespace1
    .>>. pstring "->"
    .>> deadOrWhitespace
    <?> "pattern arrow"

let questionMark =
    whitespace1
    .>>. pchar '?'
    .>>. deadspace
    <?> "match separator"


(* Literals *)

(*
    An identifier must follow this regex:
    ([A-Za-z]+[a-z]* )(\-{1,2}[A-Za-z]+[a-z]* )*[\?\!\*]{0,1}
*)

let pBaseIdentifier =
    let isUpperOrLowerAlphaASCII c =
        Char.IsLetter c && Convert.ToInt32(c) < 128

    let isLowerAlphaASCII c =
        isUpperOrLowerAlphaASCII c && Char.IsLower c

    let upperOrLowerAlpha =
        satisfy isUpperOrLowerAlphaASCII "upper or lowercase letter"

    let lowerAlpha =
        satisfy isLowerAlphaASCII "lowercase letter"

    let dash = pchar '-'

    let segment =
        upperOrLowerAlpha
        .>>. many lowerAlpha
        <?> "identifier segment"

    let questionOrBangOrStar =
        anyOf [ '?'; '!'; '*' ] <?> "idenitifier tail"

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
    <?> "identifier first segment"
    .>>. many (dash .>>. segment)
    <?> "identifier ongoing segments"
    .>>. opt questionOrBangOrStar
    |>> convertToIdentifier
    <?> "identifier /([A-Za-z]+[a-z]*)(\-{1,2}[A-Za-z]+[a-z]*)*[?!\*]{0,1}/"

let pVar =
    pBaseIdentifier
    .>>. opt (many (pchar '.' >>. pBaseIdentifier))

let chakraVar =
    pVar
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
        <?> "unicode character"

    unescapedChar
    <|> escapedChar
    <|> unicodeChar
    <?> "valid string characters"

let chakraString =
    between pdoublequote (many pValidChars) pdoublequote
    |>> charListToStr
    |>> ChakraString
    <?> "string"

let pIgnore = pchar '_' <?> "ignore"

let pRest = pstring "..." <?> "rest"

let containerItemSeparator = pchar ','

let emptyContainer start end' = start .>> end'

let container start end' item =
    let items =
        sepBy1 item (containerItemSeparator .>> deadOrWhitespace)
        .>> opt containerItemSeparator
        .>> opt deadOrWhitespace

    between start items end'

let spread =
    let createSpread (span, (first, rest)) =
        (span, { First = first; Rest = rest })
    pstring "..."
    >>. pVar
    |> withSpan
    |>> createSpread

let spreadableContainer start end' item =
    let items =
        sepBy1 item (containerItemSeparator .>> deadOrWhitespace)
        .>>. opt (containerItemSeparator .>> deadOrWhitespace >>. spread)
        .>> opt containerItemSeparator
        .>> opt deadOrWhitespace

    between start items end'

let pTuple =
    let emptyTuple =
        (emptyContainer leftParen rightParen) >>% []

    let nonEmptyTuple =
        (container leftParen rightParen chakraExpr)

    emptyTuple <|> nonEmptyTuple

let chakraTuple = pTuple |>> ChakraTuple <?> "tuple"

let chakraStruct =
    let structPair =
        let createPair (span, (name, value)) =
            { Loc = span; Name = name; Value = value}

        let regularPair =
            pBaseIdentifier .>> equal .>>. chakraExpr
            |> withSpan
            |>> createPair  


        let punned =
            pBaseIdentifier
            |> withSpan
            |>> fun (span, id) ->
                createPair (span, (id, ChakraLiteralExpr (span, ChakraVar (id, None))))

        (regularPair <|> punned)


    let createStruct (pairs, spread) =
        ChakraStruct { Fields = pairs; Spread = spread }


    let c = spreadableContainer structStart rightParen structPair

    c
    |>> createStruct
    <?> "struct"


let chakraList =
    let createList (items, spread) =
        ChakraList { Items = items; Spread = spread }
    let emptyList =
        emptyContainer leftBracket rightBracket
        >>%  ([], None)
        |>> createList
        <?> "empty list"

    let nonEmptyList =
        spreadableContainer leftBracket rightBracket chakraExpr
        |>> createList
        <?> "non empty list"

    let createList items =
        ChakraList { Items = items; Spread = None}

    (nonEmptyList <|> emptyList)
    <?> "list"

let chakraMap =
    let createPair (span, (key, value)) =
        { Loc = span ; Key = key; Value = value}

    let mapPair =
        (chakraLiteral .>> equal) .>>. chakraExpr
        |> withSpan
        |>> createPair

    let createMap (pairs, spread) =
        ChakraMap { Pairs = pairs; Spread = spread }

    let nonEmptyMap =
        spreadableContainer mapStart rightBracket mapPair
        |>> createMap

    let emptyMap =
        emptyContainer mapStart rightBracket
        >>% ChakraMap { Pairs = []; Spread = None }

    emptyMap <|> nonEmptyMap <?> "map"

let chakraLambda =
    let stringTuple =
        container leftParen rightParen pBaseIdentifier

    let createRecord (args, body) = { Args = args; Body = body }
    between leftCurly (stringTuple .>> arrow .>>. chakraExprList) rightCurly
    |>> createRecord
    |>> ChakraLambda

(* Patterns *)

let cpVar = pBaseIdentifier |> withSpan |>> CPVar

// let cpNumber = TODO
// let cpSymbol = TODO
// let cpString = TODO
// let cpTuple = TODO
// let cpStruct = TODO
// let cpList = TODO
// let cpMap = TODO


(* Expressions *)

let flattenTuple (a, (b, c)) = (a, b, c)

let chakraApply =
    pBaseIdentifier
    .>>. many (pchar '.' >>. pBaseIdentifier)
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
    .>> deadspace
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
        |>> fun (name, args) -> ChakraFunctionBindingPattern { Name = name; Args = args }
        <?> "function binding pattern"

    func <|> simple <?> "binding pattern"

let rec chakraBinding =
    let createBinding (s, (b, e)) =
        { Loc = s
          Pattern = b
          ExprList = e
          DocComment = None}

    (chakraBindingPattern .>> equal)
    .>>. chakraExprList
    |> withSpan
    |>> createBinding
    <?> "Binding"

let chakraLiteralExpr =
    chakraLiteral
    |> withSpan
    |>> ChakraLiteralExpr
    <?> "literal expression"

let chakraApplyExpr =
    chakraApply
    |> withSpan
    |>> ChakraApplyExpr
    <?> "function application"


let chakraMatchExpr =
    chakraMatch
    |> withSpan
    |>> ChakraMatchExpr
    <?> "match expression"

(* Imports *)

let pRootImport =
    pstring "/root"
    >>. pchar '/'
    >>. pBaseIdentifier
    <?> "Root import"

let pPackageImport =
    pchar '/' >>. pBaseIdentifier <?> "Package import"

let pRelativeImport =
    pstring "./"
    >>. pBaseIdentifier
    <?> "Relative import"

let pImportDestructuring =
    container structStart rightParen pBaseIdentifier
    <?> "Import destructuring"

let chakraImport =
    let simple =
        pBaseIdentifier |>> ChakraSimpleImportBinding

    let destructured =
        let addBinding map binding = Map.add binding binding map

        let createBindings bindings =
            List.fold addBinding (Map.empty) bindings

        pImportDestructuring
        |>> (createBindings >> ChakraDestructuredImportBinding)

    let binding = simple <|> destructured

    let rootImport (typ, string) =
        ChakraLocalImport
            { Library = string
              Typ = typ
              Relative = false }

    let relativeImport (typ, string) =
        ChakraLocalImport
            { Library = string
              Typ = typ
              Relative = true }

    let packageImport (typ, string) =
        ChakraPackageImport { PackageName = string; Typ = typ }

    let eq = (pchar ' ' .>> pchar '=' .>> pchar ' ')

    (binding .>> eq .>>. pRootImport |>> rootImport)
    <|> (binding
         .>> eq
         .>>. pPackageImport
         |>> packageImport)
    <|> (binding
         .>> eq
         .>>. pRelativeImport
         |>> relativeImport)
    <?> "import"

(* Modules *)

let chakraModuleDef =
    let tupleLike =
        container structStart rightParen pBaseIdentifier

    rawEqual
    .>> pchar ' '
    >>. tupleLike
    <?> "Module export definition"

let chakraModule =
    let possibleImportSection =
        sepBy chakraImport (deadspace) <?> "imports"

    let buildModule (((c, exports), imports), bindings) =
        let comment =
            match c with
            | None -> None
            | Some ({ Content = com }) -> Some com

        { DocComments = comment
          Exports = exports
          Bindings = bindings
          Imports = imports }

    let topLevelBindings =
        let content ({ Content = c; IsDoc = _ }) = c
        let createBinding (optComment, binding) =
                { binding with
                    DocComment = Option.map (content) optComment }

        let b =
            opt docComment
            .>>. chakraBinding
            |>> createBinding
            <?> "top level binding"

        sepBy1 b (deadspace)

    let t = returnP []

    (opt docComment)
    .>>. chakraModuleDef
    <?> "Module definition"
    .>> deadspace
    <?> "significant newlines"
    .>>. possibleImportSection
    <?> "import section"
    .>> opt deadspace
    <?> "significant newline"
    .>>. topLevelBindings
    <?> "module bindings"
    .>> deadspace
    <?> "significant newline"
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
:= many (chakraBinding .>> deadspace)
<?> "expression list bindings"
.>>. chakraExpr
<?> "expression list expression"
|>> ChakraExprList
<?> "expression list"
