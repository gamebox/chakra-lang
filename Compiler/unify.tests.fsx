#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Env.fs"
#load "Pretty.fs"
#load "Unify.fs"

open Unify
open Env
open ChakraParser

let cRed = CConsole.red
let cGreen = CConsole.green

let (<&>) a b = a , b

let (==) (a, b) t =
    fun () ->
        let res = Option.filter ((=) t) (unify a b)
        let msg = sprintf "%s <&> %s == %O" (print a) (print b) (print t)
        match res with
        | Some _ ->
            printfn "%s" (cGreen msg)
            Ok ()
        | None -> 
            printfn "%s" (cRed msg)
            Error msg

let collectTestResults (total, failures) r =
    match r () with
    | Ok _ -> (total + 1, failures)
    | Error m -> (total + 1, (m::failures))

let printResults name (total, failures) =
    let f = List.length failures
    printfn "%s\nTotal of %d tests ran. %d successes, %d failures." name total (total - f) f
    for f in (List.rev failures) do
        printfn "%s" (cRed f)
    (total, failures)

let test name tests =
    fun () ->
        tests
        |> List.fold collectTestResults (0, [])
        |> printResults name

let collectSuiteResults (accTotal, accFailures) t = 
    let (total, failures) = t ()
    (accTotal + total, List.concat [failures ; accFailures])

let suite name tests =
    List.fold collectSuiteResults (0, []) tests
    |> printResults name

let typeTestWithEnv prettier test' env p str t () =
    match ParserLibrary.run p str with
    | (ParserLibrary.Success((lit, _))) ->
        let msg = (sprintf "%s == %s" (Pretty.pretty 80 (prettier lit)) (print t))
        match test' env lit with
        | (Ok (_, t')) ->
            if t' = t then
                printfn "%s" (cGreen msg)
                Ok ()
            else
                printfn "%s" (cRed msg)
                printfn "%s" (cRed (sprintf "Got: %s" (print t')))
                Error msg
        | Error (TypeError e) ->
            printfn "%s" (cRed msg)
            printfn "%s" (cRed (sprintf "Problem: %s" e))
            Error msg
    | (ParserLibrary.Failure _) ->
        let msg = (sprintf "Failed to parse: %s" str)
        printfn "%s" (cRed msg)
        Error msg

let litTypeTestWithEnv env p str t () =
    typeTestWithEnv Pretty.showLiteral literalType env p str t ()

let litTypeTest p str t () =
    litTypeTestWithEnv defaultEnv p str t ()

let exprTypeTestWithEnv env p str t () =
    typeTestWithEnv Pretty.showExpr exprType env p str t ()

let exprTypeTest p str t () =
    exprTypeTestWithEnv defaultEnv p str t ()

let bindingTypeTestWithEnv env p str t () =
    typeTestWithEnv Pretty.showBinding bindingType env p str t ()

let exprTestsEnv = emptyWith [
    ("add", fn [ num; num ] num)
    ("starts-with?", fn [ str; str ] bool)
    ("gt?", fn [ num; num ] bool)
    ("mul", fn [ num; num ] num)
    ("map", fn [ fn [ genA ] genB; list genA ] genB)
]

suite "Unify" [
    test "unify" [
        num <&> num == num
        str <&> str == str
        gSym "ok" <&> gSym "ok" == gSym "ok"
        list num <&> list num == list num
        list str <&> list str == list str
        list (gSym "ok") <&> list (gSym "ok") == list (gSym "ok")
        map num num <&> map num num == map num num
        map str num <&> map str num == map str num
        map num str <&> map num str == map num str
        map str str <&> map str str == map str str
        strct ([ "foo", str ; "bar", num], false, None) <&> strct ([ "foo", str ; "bar", num], false, None) == strct ([ "foo", str ; "bar", num], false, None)
        fn [num; num] bool <&> fn [num; num] bool == fn [num; num] bool
        fn [num; num] bool <&> fn [genA; genA] bool == fn [num; num] bool
    ]

    test "literalType - successful" [
        litTypeTest chakraNumber "1" num
        litTypeTest chakraNumber "124252352342334" num
        litTypeTest chakraNumber "13.4645645645645666665" num
        litTypeTest chakraNumber "-13423423523525232432424241.092824024" num
        litTypeTest chakraString "\"\"" str
        litTypeTest chakraString "\"This is a string\"" str
        litTypeTest chakraSymbol "#symbol" (gSym "symbol")
        litTypeTest chakraSymbol "#symbol-That-I-Made" (gSym "symbol-That-I-Made")
        litTypeTest chakraList "[1, 2, 3]" (list num)
        litTypeTest chakraList "[\"a\", \"b\", \"c\"]" (list str)
        litTypeTest chakraTuple "(1, \"a\")" (tup [num; str])
        litTypeTest chakraStruct "%( a = 1, b = \"foo\")" (strct ([("a", num); ("b", str)], false, None))
        litTypeTest chakraLambda "{ (a) ->\n\tb = 1\n\tc = \"a\"\n\t(b, c)\n}" (fn [genA] (tup [num; str]))
        litTypeTestWithEnv (emptyWith ["a", str]) chakraVar "a" str
        litTypeTestWithEnv (emptyWith ["a", str]) chakraTuple "(1, a)" (tup [num; str])
        litTypeTestWithEnv (emptyWith ["foo", (strct ([("a", num); ("b", str)], false, None))]) chakraVar "foo.b" str
    ]

    test "exprType - successful" [
        exprTypeTest chakraLiteralExpr "1" num
        exprTypeTest chakraLiteralExpr "124252352342334" num
        exprTypeTest chakraLiteralExpr "13.4645645645645666665" num
        exprTypeTest chakraLiteralExpr "-13423423523525232432424241.092824024" num
        exprTypeTest chakraLiteralExpr "\"\"" str
        exprTypeTest chakraLiteralExpr "\"This is a string\"" str
        exprTypeTest chakraLiteralExpr "#symbol" (gSym "symbol")
        exprTypeTest chakraLiteralExpr "#symbol-That-I-Made" (gSym "symbol-That-I-Made")
        exprTypeTest chakraLiteralExpr "[1, 2, 3]" (list num)
        exprTypeTest chakraLiteralExpr "[\"a\", \"b\", \"c\"]" (list str)
        exprTypeTest chakraLiteralExpr "(1, \"a\")" (tup [num; str])
        exprTypeTest chakraLiteralExpr "%( a = 1, b = \"foo\")" (strct ([("a", num); ("b", str)], false, None))
        exprTypeTest chakraLiteralExpr "{ (a) ->\n\tb = 1\n\tc = \"a\"\n\t(b, c)\n}" (fn [genA] (tup [num; str]))
        exprTypeTestWithEnv (emptyWith ["a", str]) chakraLiteralExpr "a" str
        exprTypeTestWithEnv (emptyWith ["a", str]) chakraLiteralExpr "(1, a)" (tup [num; str])
        exprTypeTestWithEnv exprTestsEnv chakraApplyExpr "add(1, 1)" num
        exprTypeTestWithEnv exprTestsEnv chakraApplyExpr "add(1)" (fn [num] num)
        exprTypeTestWithEnv exprTestsEnv chakraApplyExpr "starts-with?(\"foo\", \"f\")" bool
        exprTypeTestWithEnv exprTestsEnv chakraApplyExpr "map({ (i) -> add(i, 1)}, [1, 2, 3])" (list num)
    ]

    test "bindingType - successful" [
        bindingTypeTestWithEnv exprTestsEnv chakraBinding "foo(a) =\n\tb = 1\n\tc = \"a\"\n\t(b, c)\n" (fn [genA] (tup [num; str]))
        bindingTypeTestWithEnv exprTestsEnv chakraBinding "foo(a, b, c) =\n\tsum = add(a, b)\n\n\tproduct = mul(sum, c)\n\n\tgt?(product, 100)\n" (fn [num; num; num] bool)
    ]
]
