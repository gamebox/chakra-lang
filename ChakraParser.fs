module ChakraParser

open System
open ParserLibrary


(* Types *)

type FunctionBindPatternInfo = { Name: string; Args: string list }

type ChakraIdentifier = ChakraIdentifier of string

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
    | ChakraLambda of (ChakraExpr list * ChakraExprList)

and ChakraMatch = ChakraMatch of (ChakraLiteral * ChakraMatchClause list)

and ChakraMatchClause = ChakraMatchClause of (ChakraLiteral * ChakraExprList)

and ChakraBinding = ChakraBinding of (ChakraBindingPattern * ChakraExprList)

and ChakraExprList = ChakraExprList of (ChakraBinding list * ChakraExpr)

and ChakraExpr =
    | ChakraLiteralExpr of ChakraLiteral
    | ChakraMatchExpr of ChakraMatch
    | ChakraApplyExpr of ChakraApply

and ChakraApply = ChakraApply of (string * ChakraExpr list)


type ChakraModule = ChakraModule of ChakraBinding list


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



(* Literals *)


let pBaseIdentifier =
    let upperOrLowerAlpha = anyOf ([ 'A' .. 'Z' ] @ [ 'a' .. 'z' ])
    let lowerAlpha = anyOf [ 'a' .. 'z' ]
    let dash = pchar '-'
    let segment = upperOrLowerAlpha .>>. many lowerAlpha
    let questionOrBangOrStar = anyOf [ '?'; '!'; '*' ]
    (* ((firstAlpha * restOfFirstSegment) * otherSegments * sign *)

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

let chakraString =
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

    let validChars =
        unescapedChar <|> escapedChar <|> unicodeChar

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

    between leftBracket (sepBy mapPair whitespace) rightBracket
    |>> ChakraMap
    <?> "map"

let chakraVector =
    between leftCurly (sepBy chakraExpr whitespace) rightCurly
    |>> ChakraVector
    <?> "vector"

let chakraLambda =
    between leftCurly (rawTuple .>> arrow .>>. chakraExprList) rightCurly
    |>> ChakraLambda


(* Expressions *)


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
    .>>. many1 chakraMatchClause
    |>> ChakraMatch

let chakraBindingPattern =
    pBaseIdentifier |>> ChakraSimpleBindingPattern

let rec chakraBinding =
    (chakraBindingPattern .>> equal)
    .>>. chakraExprList
    |>> ChakraBinding

let chakraLiteralExpr =
    chakraLiteral
    |>> ChakraLiteralExpr
    <?> "literal expression"

let chakraApplyExpr =
    chakraApply
    |>> ChakraApplyExpr
    <?> "function application"

let chakraMatchExpr =
    chakraMatch
    |>> ChakraMatchExpr
    <?> "match expression"


let chakraModule =
    many1 chakraBinding |>> ChakraModule <?> "module"

(* Set refs *)


chakraLiteralRef
:= choice
    [ chakraVar
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



(*******************************************************************************
**
** Tests
**
*******************************************************************************)

let something = (ChakraSimpleBindingPattern "something")
let identSomething = (ChakraVar "something")
let strSomething = (ChakraString "something")
let strElse = (ChakraString "else")
let symOne = (ChakraSymbol "one")
let symTwo = (ChakraSymbol "two")
let symOk = (ChakraSymbol "ok")
let numOne = (ChakraNumber 1.0)
let numTwo = (ChakraNumber 2.0)
let num200 = (ChakraNumber 200.0)
let litExpr literal = (ChakraLiteralExpr literal)

let simpleExprList literal = ChakraExprList([], litExpr literal)

let simpleBinding literal =
    (ChakraBinding(something, simpleExprList literal))

let complexBinding name bindings literal =
    (ChakraBinding(ChakraSimpleBindingPattern name, ChakraExprList(bindings, litExpr literal)))

let varTests () =
    printfn "Identifier tests"
    printfn "------------\n"
    test "one segment identifier with first upper" chakraVar "Something" (ChakraVar "Something")
    test "one segment identifier with first lower" chakraVar "something" (ChakraVar "something")
    test "multiple segments" chakraVar "something-else" (ChakraVar "something-else")
    test "multiple segments" chakraVar "Something-else" (ChakraVar "Something-else")
    test "multiple segments" chakraVar "something-Else" (ChakraVar "something-Else")
    test "multiple segments with bang" chakraVar "something-else!" (ChakraVar "something-else!")
    test "multiple segments with question mark" chakraVar "something-else?" (ChakraVar "something-else?")
    test "multiple segments with star" chakraVar "something-else*" (ChakraVar "something-else*")

    testError chakraVar "starting with number should fail" "1something"

    printfn ""

let numberTests () =
    printfn "Number tests"
    printfn "------------\n"
    test "zero" chakraNumber "0" (ChakraNumber 0.0)
    test "simple integer" chakraNumber "1" (ChakraNumber 1.0)
    test "long integer" chakraNumber "100000005" (ChakraNumber 100000005.0)
    test "simple decimal" chakraNumber "0.1" (ChakraNumber 0.1)
    test "long decimal" chakraNumber "1024.2048" (ChakraNumber 1024.2048)

    printfn ""

let stringTests () =
    printfn "String tests"
    printfn "------------\n"

    test "empty string" chakraString "\"\"" (ChakraString "")
    test "simple string" chakraString "\"This is a string\"" (ChakraString "This is a string")
    test "string with newlines" chakraString "\"One\nTwo\"" (ChakraString "One\nTwo")
    test "string with unicode" chakraString "\"\\u00f8\"" (ChakraString "\u00f8")

    printfn ""


let symbolTests () =
    printfn "Symbol tests"
    printfn "------------\n"
    test "one segment identifier with first upper" chakraSymbol "#Something" (ChakraSymbol "Something")
    test "one segment identifier with first lower" chakraSymbol "#something" (ChakraSymbol "something")
    test "multiple segments" chakraSymbol "#something-else" (ChakraSymbol "something-else")
    test "multiple segments" chakraSymbol "#Something-else" (ChakraSymbol "Something-else")
    test "multiple segments" chakraSymbol "#something-Else" (ChakraSymbol "something-Else")
    test "multiple segments with bang" chakraSymbol "#something-else!" (ChakraSymbol "something-else!")
    test "multiple segments with question mark" chakraSymbol "#something-else?" (ChakraSymbol "something-else?")
    test "multiple segments with star" chakraSymbol "#something-else*" (ChakraSymbol "something-else*")

    printfn ""



let tupleTests () =
    printfn "Tuple tests"
    printfn "------------\n"
    test "empty tuple" chakraTuple "()" (ChakraTuple [])
    test "one element number tuple" chakraTuple "(1)" (ChakraTuple [ (litExpr numOne) ])
    test "two element number tuple" chakraTuple "(1 2)" (ChakraTuple [ (litExpr numOne); (litExpr numTwo) ])
    test "one element symbol tuple" chakraTuple "(#one)" (ChakraTuple [ (litExpr symOne) ])
    test "two element symbol tuple" chakraTuple "(#one #two)" (ChakraTuple [ (litExpr symOne); (litExpr symTwo) ])
    test "mixed tuple with number and identifier" chakraTuple "(1 something)"
        (ChakraTuple
            [ (litExpr numOne)
              (litExpr identSomething) ])

    test "mixed tuple with symbol and number" chakraTuple "(#ok 200)"
        (ChakraTuple [ (litExpr symOk); (litExpr num200) ])

    test "mixed tuple with symbol and number with whitespace" chakraTuple "( #ok 200 )"
        (ChakraTuple [ (litExpr symOk); (litExpr num200) ])

    printfn ""

let listTests () =
    printfn "List tests"
    printfn "------------\n"
    test "empty list" chakraList "[]" (ChakraList [])
    test "one element number list" chakraList "[1]" (ChakraList [ (litExpr numOne) ])
    test "two element number list" chakraList "[1 2]" (ChakraList [ (litExpr numOne); (litExpr numTwo) ])
    test "one element symbol list" chakraList "[#one]" (ChakraList [ (litExpr symOne) ])
    test "two element symbol list" chakraList "[#one #two]" (ChakraList [ (litExpr symOne); (litExpr symTwo) ])
    test "two element symbol list with whitespace" chakraList "[\n\t#one\n\t#two\n]"
        (ChakraList [ (litExpr symOne); (litExpr symTwo) ])

    printfn ""

let vectorTests () =
    printfn "Vector tests"
    printfn "------------\n"
    test "empty vector" chakraVector "{}" (ChakraVector [])
    test "one element number vector" chakraVector "{1}" (ChakraVector [ (litExpr numOne) ])
    test "two element number vector" chakraVector "{1 2}" (ChakraVector [ (litExpr numOne); (litExpr numTwo) ])
    test "one element symbol vector" chakraVector "{#one}" (ChakraVector [ (litExpr symOne) ])
    test "two element symbol vector" chakraVector "{#one #two}" (ChakraVector [ (litExpr symOne); (litExpr symTwo) ])

    test "two element tuple vector" chakraVector "{(#one 1) (#two 2)}"
        (ChakraVector
            [ (litExpr (ChakraTuple [ (litExpr symOne); (litExpr numOne) ]))
              (litExpr (ChakraTuple [ (litExpr symTwo); (litExpr numTwo) ])) ])

    test "two element tuple vector with whitespace" chakraVector "{\n\t(#one 1)\n\t(#two 2)\n}"
        (ChakraVector
            [ (litExpr (ChakraTuple [ (litExpr symOne); (litExpr numOne) ]))
              (litExpr (ChakraTuple [ (litExpr symTwo); (litExpr numTwo) ])) ])

    printfn ""

let structTests () =
    printfn "Struct Tests"
    printfn "------------\n"

    test "one element struct" chakraStruct "(something = 1)" (ChakraStruct [ ("something", (litExpr numOne)) ])
    test "one element struct with whitespace" chakraStruct "(\n\tsomething = 1\n)"
        (ChakraStruct [ ("something", (litExpr numOne)) ])
    test "two element struct with whitespace" chakraStruct "(\n\tsomething = 1\n\telse = 2\n)"
        (ChakraStruct
            [ ("something", (litExpr numOne))
              ("else", (litExpr numTwo)) ])

    printfn ""

let mapTests () =
    printfn "Map Tests"
    printfn "------------\n"

    test "one element map" chakraMap "[\"something\" = 1]" (ChakraMap [ (ChakraString "something", (litExpr numOne)) ])
    test "one element map with whitespace" chakraMap "[\n\t\"something\" = 1\n]"
        (ChakraMap [ (ChakraString "something", (litExpr numOne)) ])
    test "two element map with whitespace" chakraMap "[\n\t\"something\" = 1\n\t\"else\" = 2\n]"
        (ChakraMap
            [ (ChakraString "something", (litExpr numOne))
              (ChakraString "else", (litExpr numTwo)) ])

    printfn ""

let lambdaTests () =
    printfn "Lambda Tests"
    printfn "------------\n"

    test "zero arg simple lambda" chakraLambda "{ () -> 1 }" (ChakraLambda([], simpleExprList numOne))

    printfn ""

let bindingTests () =
    printfn "Binding Tests"
    printfn "-------------\n"

    test "binding to identifier" chakraBinding "something = other" (simpleBinding (ChakraVar "other"))
    test "binding to number" chakraBinding "something = 1" (simpleBinding numOne)
    test "binding to string" chakraBinding "something = \"else\"" (simpleBinding strElse)
    test "binding to symbol" chakraBinding "something = #one" (simpleBinding symOne)
    test "binding to tuple" chakraBinding "something = ( #one #two )"
        (simpleBinding (ChakraTuple [ litExpr symOne; litExpr symTwo ]))
    test "binding to list" chakraBinding "something = [ 1 2 ]"
        (simpleBinding (ChakraList [ litExpr numOne; litExpr numTwo ]))
    test "binding to vector" chakraBinding "something = { 1 2 }"
        (simpleBinding (ChakraVector [ litExpr numOne; litExpr numTwo ]))
    test "binding to struct" chakraBinding "something = ( something = 1 )"
        (simpleBinding (ChakraStruct [ ("something", litExpr numOne) ]))
    test "binding to map" chakraBinding "something = [ \"something\" = 1 ]"
        (simpleBinding (ChakraMap [ (ChakraString "something", litExpr numOne) ]))

    let complexBindingEx =
        """
        complex =
            other = 1
            something = other
            [ other something ]
        """.Trim [| '\n'; '\t'; ' ' |]

    test "complex binding" chakraBinding complexBindingEx
        (complexBinding "complex"
             [ (ChakraBinding(ChakraSimpleBindingPattern "other", ChakraExprList([], litExpr numOne)))
               simpleBinding (ChakraVar "other") ]
             (ChakraList
                 [ (litExpr (ChakraVar "other"))
                   (litExpr (ChakraVar "something")) ]))

    printfn ""

let applyTests () =
    printfn "Apply Tests"
    printfn "-----------\n"

    test "Should parse a 1 arity apply" chakraApply "some-func(1)"
        (ChakraApply("some-func", [ ChakraLiteralExpr numOne ]))
    test "Should parse a 2 arity apply" chakraApply "some-func(1 1)"
        (ChakraApply
            ("some-func",
             [ ChakraLiteralExpr numOne
               ChakraLiteralExpr numOne ]))

    printfn ""

let moduleTests () =
    printfn "Binding Tests"
    printfn "-------------\n"

    let moduleEx =
        """
something = other

something = 1

something = "else"

something = #one

something = ( #one #two )

something = [ 1 2 ]

something = { 1 2 }

something = ( something = 1 )

something = [ "something" = 1 ]

complex =
    other = 1
    something = other
    [ other something ]
        """.Trim [| '\n'; '\t'; ' ' |]

    let expected =
        ChakraModule
            [ (simpleBinding (ChakraVar "other"))
              (simpleBinding numOne)
              (simpleBinding strElse)
              (simpleBinding symOne)
              (simpleBinding (ChakraTuple [ litExpr symOne; litExpr symTwo ]))
              (simpleBinding (ChakraList [ litExpr numOne; litExpr numTwo ]))
              (simpleBinding (ChakraVector [ litExpr numOne; litExpr numTwo ]))
              (simpleBinding (ChakraStruct [ ("something", litExpr numOne) ]))
              (simpleBinding (ChakraMap [ (ChakraString "something", litExpr numOne) ]))
              (complexBinding "complex"
                   [ (ChakraBinding(ChakraSimpleBindingPattern "other", ChakraExprList([], litExpr numOne)))
                     simpleBinding (ChakraVar "other") ]
                   (ChakraList
                       [ (litExpr (ChakraVar "other"))
                         (litExpr (ChakraVar "something")) ])) ]

    test "module parses" chakraModule moduleEx expected

    printfn ""

let runTests () =
    printfn "Running all tests..."
    varTests ()
    numberTests ()
    stringTests ()
    symbolTests ()
    tupleTests ()
    listTests ()
    vectorTests ()
    structTests ()
    mapTests ()
    bindingTests ()
    moduleTests ()
    printfn "Done."
