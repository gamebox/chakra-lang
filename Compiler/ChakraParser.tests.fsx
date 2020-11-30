#load "ParserLibrary.fs"
#load "ChakraParser.fs"

open ParserLibrary
open ChakraParser

type Test<'a> = { Name: string ; Input: string ; Expected: 'a }

type TestResult =
    | TestSuccess of string
    | TestFailure of string * string

let test label (p: Parser<'a>) input (expected: 'a) =
    run p input
    |> (fun result ->
        match result with
        | Success (value, input) when value = expected && atEndOfInput input -> TestSuccess label
        | Failure (r, _, _) ->
            TestFailure (label, r))

let testError (p: Parser<'a>) label input =
    run p input
    |> (fun result ->
        match result with
        | Failure (_) -> printfn "Success: %s" label
        | _ ->
            printfn "Test Failed: Expected a failure got a success: %s" label
            printResult result)


let suite name parser (tests: Test<'a> list) =
    let reducer acc t = 
        (test t.Name parser t.Input t.Expected) :: acc
    let results: TestResult list = List.fold reducer tests []
    results

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
let pos line col = { Line = line; Column = col }
let boi = pos 0 0

let litExpr line col literal =
    let pos = { Line = line; Column = col }
    ChakraLiteralExpr(pos, literal)

let simpleExprList line col literal =
    ChakraExprList([], (litExpr line col literal))

let simpleBinding bindingPos exprPos literal =
    ChakraBinding(bindingPos, something, (simpleExprList exprPos.Line exprPos.Column literal))

let complexBinding name bindings bindingPos exprPos literal =
    ChakraBinding
        (bindingPos,
         ChakraSimpleBindingPattern name,
         ChakraExprList(bindings, (litExpr exprPos.Line exprPos.Column literal)))


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
    test "one element number tuple" chakraTuple "(1)" (ChakraTuple [ (litExpr 0 1 numOne) ])
    test
        "two element number tuple"
        chakraTuple
        "(1 2)"
        (ChakraTuple [ (litExpr 0 1 numOne)
                       (litExpr 0 3 numTwo) ])
    test "one element symbol tuple" chakraTuple "(#one)" (ChakraTuple [ (litExpr 0 1 symOne) ])
    test
        "two element symbol tuple"
        chakraTuple
        "(#one #two)"
        (ChakraTuple [ (litExpr 0 1 symOne)
                       (litExpr 0 6 symTwo) ])
    test
        "mixed tuple with number and identifier"
        chakraTuple
        "(1 something)"
        (ChakraTuple [ (litExpr 0 1 numOne)
                       (litExpr 0 3 identSomething) ])

    test
        "mixed tuple with symbol and number"
        chakraTuple
        "(#ok 200)"
        (ChakraTuple [ (litExpr 0 1 symOk)
                       (litExpr 0 5 num200) ])

    test
        "mixed tuple with symbol and number with whitespace"
        chakraTuple
        "( #ok 200 )"
        (ChakraTuple [ (litExpr 0 2 symOk)
                       (litExpr 0 6 num200) ])

    printfn ""

let listTests () =
    printfn "List tests"
    printfn "------------\n"
    test "empty list" chakraList "[]" (ChakraList [])
    test "one element number list" chakraList "[1]" (ChakraList [ (litExpr 0 1 numOne) ])
    test
        "two element number list"
        chakraList
        "[1 2]"
        (ChakraList [ (litExpr 0 1 numOne)
                      (litExpr 0 3 numTwo) ])
    test "one element symbol list" chakraList "[#one]" (ChakraList [ (litExpr 0 1 symOne) ])
    test
        "two element symbol list"
        chakraList
        "[#one #two]"
        (ChakraList [ (litExpr 0 1 symOne)
                      (litExpr 0 6 symTwo) ])
    test "two element symbol list with whitespace" chakraList "[\n\t#one\n\t#two\n]"
        (ChakraList [ (litExpr 1 1 symOne)
                      (litExpr 2 1 symTwo) ])

    printfn ""

let vectorTests () =
    printfn "Vector tests"
    printfn "------------\n"
    test "empty vector" chakraVector "{}" (ChakraVector [])
    test "one element number vector" chakraVector "{1}" (ChakraVector [ (litExpr 0 1 numOne) ])
    test
        "two element number vector"
        chakraVector
        "{1 2}"
        (ChakraVector [ (litExpr 0 1 numOne)
                        (litExpr 0 3 numTwo) ])
    test "one element symbol vector" chakraVector "{#one}" (ChakraVector [ (litExpr 0 1 symOne) ])
    test
        "two element symbol vector"
        chakraVector
        "{#one #two}"
        (ChakraVector [ (litExpr 0 1 symOne)
                        (litExpr 0 6 symTwo) ])

    test
        "two element tuple vector"
        chakraVector
        "{(#one 1) (#two 2)}"
        (ChakraVector [ (litExpr
                             0
                             1
                             (ChakraTuple [ (litExpr 0 2 symOne)
                                            (litExpr 0 7 numOne) ]))
                        (litExpr
                            0
                             10
                             (ChakraTuple [ (litExpr 0 11 symTwo)
                                            (litExpr 0 16 numTwo) ])) ])

    test "two element tuple vector with whitespace" chakraVector "{\n\t(#one 1)\n\t(#two 2)\n}"
        (ChakraVector [ (litExpr
                             1
                             1
                             (ChakraTuple [ (litExpr 1 2 symOne)
                                            (litExpr 1 7 numOne) ]))
                        (litExpr
                            2
                             1
                             (ChakraTuple [ (litExpr 2 2 symTwo)
                                            (litExpr 2 7 numTwo) ])) ])

    printfn ""

let structTests () =
    printfn "Struct Tests"
    printfn "------------\n"

    test "one element struct" chakraStruct "(something = 1)" (ChakraStruct [ ("something", (litExpr 0 13 numOne)) ])
    test "one element struct with whitespace" chakraStruct "(\n\tsomething = 1\n)"
        (ChakraStruct [ ("something", (litExpr 1 13 numOne)) ])
    test "two element struct with whitespace" chakraStruct "(\n\tsomething = 1\n\telse = 2\n)"
        (ChakraStruct [ ("something", (litExpr 1 13 numOne))
                        ("else", (litExpr 2 8 numTwo)) ])

    printfn ""

let mapTests () =
    printfn "Map Tests"
    printfn "------------\n"

    test
        "one element map"
        chakraMap
        "[\"something\" = 1]"
        (ChakraMap [ (ChakraString "something", (litExpr 0 15 numOne)) ])
    test "one element map with whitespace" chakraMap "[\n\t\"something\" = 1\n]"
        (ChakraMap [ (ChakraString "something", (litExpr 1 15 numOne)) ])
    test "two element map with whitespace" chakraMap "[\n\t\"something\" = 1\n\t\"else\" = 2\n]"
        (ChakraMap [ (ChakraString "something", (litExpr 1 15 numOne))
                     (ChakraString "else", (litExpr 2 10 numTwo)) ])

    printfn ""

let lambdaTests () =
    printfn "Lambda Tests"
    printfn "------------\n"

    test
        "zero arg simple lambda"
        chakraLambda
        "{ () -> 1 }"
        (ChakraLambda
            { Args = []
              Body = simpleExprList 0 8 numOne })
    test
        "single arg simple lambda"
        chakraLambda
        "{ (arg) -> 1 }"
        (ChakraLambda
            { Args = [ litExpr 0 3 (ChakraVar "arg") ]
              Body = simpleExprList 0 11 numOne })

    let complexLambda =
        """
    { (a b c) ->
        x = something(a b)
        else(x c)
    }
        """.Trim([| '\n'; '\t'; ' ' |])

    test
        "multiple arg complex lambda"
        chakraLambda
        complexLambda
        (ChakraLambda
            { Args =
                  [ litExpr 0 3 (ChakraVar "a")
                    litExpr 0 5 (ChakraVar "b")
                    litExpr 0 7 (ChakraVar "c") ]
              Body =
                  ChakraExprList
                      ([ ChakraBinding
                          (pos 1 8,
                           ChakraSimpleBindingPattern "x",
                           ChakraExprList
                               ([],
                                ChakraApplyExpr
                                    (pos 1 12,
                                     ChakraApply
                                         ("something",
                                          [ litExpr 1 22 (ChakraVar "a")
                                            litExpr 1 24 (ChakraVar "b") ])))) ],
                       ChakraApplyExpr
                           (pos 2 8,
                            ChakraApply
                                ("else",
                                 [ litExpr 2 13 (ChakraVar "x")
                                   litExpr 2 15 (ChakraVar "c") ]))) })

    printfn ""

let bindingTests () =
    printfn "Binding Tests"
    printfn "-------------\n"

    test "binding to identifier" chakraBinding "something = other" (simpleBinding boi (pos 0 12) (ChakraVar "other"))
    test "binding to number" chakraBinding "something = 1" (simpleBinding boi (pos 0 12) numOne)
    test "binding to string" chakraBinding "something = \"else\"" (simpleBinding boi (pos 0 12) strElse)
    test "binding to symbol" chakraBinding "something = #one" (simpleBinding boi (pos 0 12) symOne)

    let tup =
        (ChakraTuple [ litExpr 0 14 symOne
                       litExpr 0 19 symTwo ])

    test "binding to tuple" chakraBinding "something = ( #one #two )" (simpleBinding boi (pos 0 12) tup)

    let list =
        (ChakraList [ litExpr 0 14 numOne
                      litExpr 0 16 numTwo ])

    test "binding to list" chakraBinding "something = [ 1 2 ]" (simpleBinding boi (pos 0 12) list)


    test
        "binding to vector"
        chakraBinding
        "something = { 1 2 }"
        (simpleBinding
            boi
             (pos 0 12)
             (ChakraVector [ litExpr 0 14 numOne
                             litExpr 0 16 numTwo ]))
    test
        "binding to struct"
        chakraBinding
        "something = ( something = 1 )"
        (simpleBinding boi (pos 0 12) (ChakraStruct [ ("something", litExpr 0 26 numOne) ]))
    test
        "binding to map"
        chakraBinding
        "something = [ \"something\" = 1 ]"
        (simpleBinding boi (pos 0 12) (ChakraMap [ (ChakraString "something", litExpr 0 28 numOne) ]))

    let complexBindingEx =
        """
complex =
    other = 1
    something = other
    [ other something ]
        """.Trim [| '\n'; '\t'; ' ' |]

    test
        "complex binding"
        chakraBinding
        complexBindingEx
        (complexBinding
            "complex"
             [ (ChakraBinding((pos 1 4), ChakraSimpleBindingPattern "other", ChakraExprList([], litExpr 1 12 numOne)))
               simpleBinding (pos 2 4) (pos 2 16) (ChakraVar "other") ]
             boi
             (pos 3 4)
             (ChakraList [ (litExpr 3 6 (ChakraVar "other"))
                           (litExpr 3 12 (ChakraVar "something")) ]))

    printfn ""

let matchTests () =
    printfn "Match Tests"
    printfn "-------------\n"

    let matchClause literal exprList = ChakraMatchClause(literal, exprList)

    test
        "should parse a simple match"
        chakraMatch
        ("""
something ?
    | 1 -> #one
    | 2 -> #two
    | a -> #other
        """.Trim [| '\t'; '\n'; ' ' |])
        (ChakraMatch
            (ChakraVar "something",
             [ matchClause (ChakraNumber 1.0) (ChakraExprList([], litExpr 1 11 (ChakraSymbol "one")))
               matchClause (ChakraNumber 2.0) (ChakraExprList([], litExpr 2 11 (ChakraSymbol "two")))
               matchClause (ChakraVar "a") (ChakraExprList([], litExpr 3 11 (ChakraSymbol "other"))) ]))

    printfn ""

let applyTests () =
    printfn "Apply Tests"
    printfn "-----------\n"

    test
        "Should parse a 1 arity apply"
        chakraApply
        "some-func(1)"
        (ChakraApply("some-func", [ ChakraLiteralExpr(pos 0 10, numOne) ]))
    test
        "Should parse a 2 arity apply"
        chakraApply
        "some-func(1 1)"
        (ChakraApply
            ("some-func",
             [ ChakraLiteralExpr(pos 0 10, numOne)
               ChakraLiteralExpr(pos 0 12, numOne) ]))

    printfn ""

let moduleTests () =
    printfn "Module Tests"
    printfn "-------------\n"

    let bol line = pos line 0

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
        ChakraModule [ (simpleBinding (bol 0) (pos 0 12) (ChakraVar "other"))
                       (simpleBinding (bol 2) (pos 2 12) numOne)
                       (simpleBinding (bol 4) (pos 4 12) strElse)
                       (simpleBinding (bol 6) (pos 6 12) symOne)
                       (simpleBinding
                           (bol 8)
                            (pos 8 12)
                            (ChakraTuple [ litExpr 8 14 symOne
                                           litExpr 8 19 symTwo ]))
                       (simpleBinding
                           (bol 10)
                            (pos 10 12)
                            (ChakraList [ litExpr 10 14 numOne
                                          litExpr 10 16 numTwo ]))
                       (simpleBinding
                           (bol 12)
                            (pos 12 12)
                            (ChakraVector [ litExpr 12 14 numOne
                                            litExpr 12 16 numTwo ]))
                       (simpleBinding (bol 14) (pos 14 12) (ChakraStruct [ ("something", litExpr 14 26 numOne) ]))
                       (simpleBinding
                           (bol 16)
                            (pos 16 12)
                            (ChakraMap [ (ChakraString "something", litExpr 16 28 numOne) ]))
                       (complexBinding
                           "complex"
                            [ (ChakraBinding
                                ((pos 19 4),
                                 ChakraSimpleBindingPattern "other",
                                 ChakraExprList([], litExpr 19 12 numOne)))
                              simpleBinding (pos 20 4) (pos 20 16) (ChakraVar "other") ]
                            (bol 18)
                            (pos 21 4)
                            (ChakraList [ (litExpr 21 6 (ChakraVar "other"))
                                          (litExpr 21 12 (ChakraVar "something")) ])) ]

    test "module parses" chakraModule moduleEx expected

    printfn ""


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
matchTests ()
lambdaTests ()
moduleTests ()
printfn "Done."
