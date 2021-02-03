#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "ChakraParser.fs"
#load "Env.fs"
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

let litTypeTestWithEnv env p str t () =
    let (ParserLibrary.Success((lit, _))) = ParserLibrary.run p str
    let msg = (sprintf "%s == %s" str (print t))
    let (Ok (_, t')) = literalType env lit
    if t' = t then
        printfn "%s" (cGreen msg)
        Ok ()
    else
        printfn "%s" (cRed msg)
        printfn "%s" (cRed (sprintf "Got: %s" (print t')))
        Error msg


let litTypeTest p str t () =
    litTypeTestWithEnv defaultEnv p str t ()

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
        litTypeTest chakraLambda "{ (a) ->\n\tb = 1\n\tc = \"a\"\n\t(b, c)\n}" (fn [] (tup [num; str]))
        litTypeTestWithEnv (emptyWith ["a", str]) chakraVar "a" str
        litTypeTestWithEnv (emptyWith ["a", str]) chakraTuple "(1, a)" (tup [num; str])
    ]
]
