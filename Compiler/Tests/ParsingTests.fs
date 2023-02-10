module ParsingTests

open NUnit.Framework
open ChakraFparsec
open System
open System.IO

let loadFixture path =
    let p = Path.Combine([|__SOURCE_DIRECTORY__; "Fixtures"; "parsing"; @$"{path}.chakra"|])
    printfn "Loading %s" p
    System.IO.File.ReadAllText(p)

let assertThat (bool: bool) (label: string) =
    if not bool then NUnit.Framework.Assert.Fail(label)

let createTest<'a, 'b when 'a: equality and 'b: equality> (parser: FParsec.Primitives.Parser<AST.ChakraExpr,unit>) (fn: FParsec.CharParsers.ParserResult<AST.ChakraExpr, unit> -> Option<('a * FParsec.Position)>) (eq: 'a -> 'b -> bool) =
    (fun (str: string) (expected: 'b) ->
        let label = $"Should parse `%s{str}` correctly"
        let parsed = (FParsec.CharParsers.run parser str)
        match fn parsed with
        | Some (actual, pos) ->
            let endColumn = int64 (str.Length)
            let consumedAll = (pos.Index = endColumn)
            assertThat consumedAll $"%s{label}: Consumed only %i{endColumn} of %i{pos.Index} characters"
            assertThat (eq actual expected) $"%s{label}: Received value:\n%O{actual}\n\nExpected value:\n%O{expected}"
        | None -> NUnit.Framework.Assert.Fail(label))
 
let createFailedTest parser =
    (fun str ->
        let label = $"Should fail to parse `%s{str}`"
        let parsed = (FParsec.CharParsers.run parser str)
        match parsed with
        | FParsec.CharParsers.ParserResult.Failure _ -> ()
        | FParsec.CharParsers.ParserResult.Success (_, _, pos) ->
            let consumedAll = (pos.Column = (int64 (1 + str.Length)))
            assertThat (not consumedAll) $"%s{label}: Expected \"%s{str}\" to fail parsing, but received %O{parsed}")

let numberTest =
    createTest
        ChakraFparsec.chakraNumber
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraNumber (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (=)

let numberFailedTest = createFailedTest ChakraFparsec.chakraNumber

let stringTest =
    createTest
        ChakraFparsec.chakraString
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraString (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (=)

let stringFailedTest = createFailedTest ChakraFparsec.chakraString

let listTest<'a when 'a : equality> valFn =
    createTest
        ChakraFparsec.chakraList
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraList (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (fun (actual: AST.ChakraList) (expected: (list<'a> * string option)) ->
            let (expectedItems, expectedSpread) = expected
            let itemsSameLength = List.length actual.Items = List.length expectedItems
            let itemsToCheck = List.zip actual.Items expectedItems
            let checkItem (item, ex) =
                let actual = valFn item
                actual = ex
            let itemsMatched = itemsSameLength && List.forall checkItem itemsToCheck
            let spread = Option.map (fun (_, s) -> s) actual.Spread
            let spreadMatched = spread = expectedSpread
            itemsMatched && spreadMatched)

let listFailedTest = createFailedTest ChakraFparsec.chakraList

let tuple2Test<'a, 'b when 'a : equality and 'b : equality> (valFn1, valFn2) =
    createTest
        ChakraFparsec.chakraTuple
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraTuple (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (fun (actual: AST.ChakraExpr list) (expected: ('a * 'b)) ->
            if List.length actual <> 2 then
                false
            else
                let [actual1; actual2] = actual
                let (expected1, expected2) = expected
                valFn1 actual1 = expected1 && valFn2 actual2 = expected2)

let struct1Test<'a when 'a : equality> valFn =
    createTest
        ChakraFparsec.chakraStruct
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraStruct (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (fun (actual: AST.ChakraStruct) (expected: ((string * 'a) * string option)) ->
            let (expectedItem, expectedSpread) = expected
            let fieldsSameLength = List.length actual.Fields = 1
            let itemsToCheck = List.zip actual.Fields [expectedItem]
            let checkItem ((csf, ex): AST.ChakraStructField * (string * 'a)) =
                let actual = valFn csf.Value
                let (expectedName, expectedValue) = ex
                actual = expectedValue && expectedName = csf.Name
            let itemsMatched = fieldsSameLength && List.forall checkItem itemsToCheck
            let spread = Option.map (fun (_, s) -> s) actual.Spread
            let spreadMatched = spread = expectedSpread
            itemsMatched && spreadMatched)

let struct2Test<'a, 'b when 'a : equality and 'b: equality> valFns =
    createTest
        ChakraFparsec.chakraStruct
        (fun parsed ->
            match parsed with
            | (FParsec.CharParsers.ParserResult.Success (AST.ChakraStruct (_, actual), _, pos)) -> Some (actual, pos)
            | _ -> None)
        (fun (actual: AST.ChakraStruct) (expected: ((string * 'a) * (string * 'b) * string option)) ->
            let (expectedItem1, expectedItem2, expectedSpread) = expected
            let fieldsSameLength = List.length actual.Fields = 2
            let (valFn1, valFn2) = valFns
            let item1Matched =
                let csf = actual.Fields.Item(0)
                let actual = valFn1 csf.Value
                let (expectedName, expectedValue) = expectedItem1
                actual = expectedValue && expectedName = csf.Name
            let item2Matched =
                let csf = actual.Fields.Item(1)
                let actual = valFn2 csf.Value
                let (expectedName, expectedValue) = expectedItem2
                actual = expectedValue && expectedName = csf.Name
            let itemsMatched = item1Matched && item2Matched
            let spread = Option.map (fun (_, s) -> s) actual.Spread
            let spreadMatched = spread = expectedSpread
            fieldsSameLength && itemsMatched && spreadMatched)

let exprNum expr =
    let (AST.ChakraNumber (_, num)) = expr
    num

let exprStr expr =
    match expr with
    | AST.ChakraString (_, str) -> str

[<NUnit.Framework.SetUp>]
let Setup () =
    ()

[<NUnit.Framework.Test>]
let ``Number Parsing`` () =
    numberTest "1" 1.0m
    numberTest "1.0" 1.0m
    numberTest "1.02345678" 1.02345678m
    numberTest "7654321.02345678" 7654321.02345678m
    numberTest "8.65432102345678" 8.65432102345678m
    numberTest "792281625142643" 792281625142643m
    numberTest "-1" -1.0m
    numberTest "-1.0" -1.0m
    numberTest "-1.02345678" -1.02345678m
    numberTest "-7654321.02345678" -7654321.02345678m
    numberTest "-8.65432102345678" -8.65432102345678m
    numberTest "-792281625142643" -792281625142643m

    numberFailedTest "1_000.0"
    numberFailedTest "a1.2"
    numberFailedTest "1.23m"
    // numberTest "High-precision decimal with large integer portion" "1342343284723984732849723894.0239048239048239423498230948" 1342343284723984732849723894.0239048239048239423498230948m

[<NUnit.Framework.Test>]
let ``String parsing`` () =
    stringTest "\"Hello\"" "Hello"

[<NUnit.Framework.Test>]
let ``List parsing`` () =
    let numberListTest = listTest exprNum

    numberListTest "[1, 2, 3]" ([1m; 2m; 3m], None)

[<NUnit.Framework.Test>]
let ``Tuple parsing`` () =
    (tuple2Test (exprNum, exprNum)) "(1, 2)" (1m, 2m)
    (tuple2Test (exprNum, exprStr)) "(1, \"Hello\")" (1m, "Hello")
    (tuple2Test (exprStr, exprNum)) "(\"Hello\", 2)" ("Hello", 2m)

[<NUnit.Framework.Test>]
let ``Struct parsing`` () =
    struct1Test exprNum "%( something = 1 )" (("something", 1m), None)
    struct2Test (exprNum, exprStr) "%( something = 1, other = \"Hello\")" (("something", 1m), ("other", "Hello"), None) 
    struct2Test (exprNum, exprStr) "%( something = 1, other = \"Hello\", ...spreader)" (("something", 1m), ("other", "Hello"), Some "spreader")

    let formatted1 = loadFixture "struct/structTest1"
    struct1Test exprNum formatted1 (("something", 1m), None)

    let formatted2 = loadFixture "struct/structTest2"
    struct2Test (exprNum, exprStr) formatted2 (("something", 1m), ("other", "Hello"), None)

    let formatted3 = loadFixture "struct/structTest3"
    struct2Test (exprNum, exprStr) formatted3 (("something", 1m), ("other", "Hello"), Some "spreader")

    (createFailedTest ChakraFparsec.chakraStruct) "%( something = 1; other = 2 )"
    (createFailedTest ChakraFparsec.chakraStruct) "%( something = 1  other = 2 )"
    (createFailedTest ChakraFparsec.chakraStruct) "$( something = 1, other = 2 )"
    (createFailedTest ChakraFparsec.chakraStruct) "#( something = 1, other = 2 )"

[<NUnit.Framework.Test>]
let ``Lambda parsing`` () =
    assertThat true ""


[<Test>]
let ``Complex module with every construct parsing`` () =
    let str = loadFixture "module/everything"
    let parsed = (FParsec.CharParsers.run (chakraModule "everything")  str)
    match parsed with
    | FParsec.CharParsers.ParserResult.Success _ -> ()
    | _ -> Assert.Fail "Failed to parse: Complex module with every construct parsing"