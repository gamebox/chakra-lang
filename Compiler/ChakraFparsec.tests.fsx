#r "nuget: FParsec"
#load "RunProcess.fs"
#load "Operators.fs"
#load "Graph.fs"
#load "Codegen.fs"
#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "TypeSystem.fs"
#load "AST.fs"
#load "Stdlib.fs"
#load "TypeError.fs"
#load "TypedAST.fs"
#load "ChakraFparsec.fs"

open FParsec
open ChakraFparsec

let trim (addlChars: char array) (s: string) = s.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd(Array.append [| '\t'; ' '|] addlChars)


(********************************************
*
* Fixtures
*
*********************************************)



let moduleDefFix =
    """
 = %(
     init
 )

    """.Trim([| '\n'; '\t'; ' '|])

let s1 =
    """
= %(
    init
)


%( io, string ) = /stdlib

    """.Trim([| '\n'; '\t'; ' '|])

let destructuredImportFix =
    """
%( io, string ) = /stdlib

    """.Trim([| '\n'; '\t'; ' '|])

let varFix = "caps"
let complexVarFix = "is-this-a-var?"
let numberIntFix = "1"
let numberFloatFix = "3.14159265359"
let listOfIntsFix = "[1, 2, 3, 4]"
let listOfIntsExprListFix = sprintf "%s\n" listOfIntsFix
let simpleApplyFix = "some-func(\"Hello?\")"
let numberOnlyExprListFix = "1\n"
let applyOnlyExprListFix = "some-func(1)\n"
let simplestBindingFix = "a = 1\n"
let simpleBindingWithStringFix =
    """f = "Hello"
    """
let simpleBindingWithListFix =
    """f = [1, x]
    """
let simpleBindingWithTupleFix =
    """f = ("ok", x)
    """
let simpleBindingWithStructFix =
    """f = %( contents = x )
    """
let simpleBindingWithMapFix =
    """f = %[ "Hello" = 1 ]
    """
let simpleBindingWithBindingInELFix =
    """
f =
    b = 2
    [x, b]
    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])
let simpleBindingWithStructBindingInELFix =
    """
f =
    b = %( contexts = x )
    [x, b]
    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])
let simplestFunctionBindingFix = "f(x) = 1\n"
let functionBindingWithStringFix =
    """f(x) = "Hello"
    """
let annotatedFunctionBindingWithStringFix =
    """f(x: a): "" = "Hello"
    """
let functionBindingWithSymbolFix =
    """f(x) = Testing
    """
let functionBindingWithListFix =
    """f(x) = [1, x]
    """
let functionBindingWithTupleFix =
    """f(x) = ("ok", x)
    """
let functionBindingWithStructFix =
    """f(x) = %( contents = x )
    """
let functionBindingWithMapFix =
    """f(x) = %[ "Hello" = 1 ]
    """
let fnBindingWithBindingInELFix =
    """
f(x) =
    b = 2
    [x, b]
    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])
let fnBindingWithApplyBindingInELFix =
    """
init(caps) =
    ping = spawn(pinger, cmd.send(caps.self, ""))
    (
        %( pinger = ping ),
        1
    )
    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])
let simpleVarHeadMatchFix = 
    """
caps ?
| _ -> 1
    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])
let simpleVarHeadMatchWithApplyFix =
    """
caps ?
| _ ->
    print(1)
| _ ->
    io.print(0)

    """.TrimStart([| '\n'; '\t'; ' '|]).TrimEnd([| '\t'; ' '|])

let simpleLambda = "{ (a) -> add(a, b) }"
let nullaryLambda = "{ () -> add(a, b) }"

let simpleCompleteModule =
    """
= %( init )

%( io ) = /std

init(caps) =
    io.print(caps.stdio, "Hello World!")
""" |> trim [||]

let complexCompleteModule =
    """
= %( init, receive )

%( io, cmd, spawn ) = /stdlib

init(caps) =
    ping = spawn(pinger, { () -> cmd.send(caps.self, "") })
    ( %( pinger = ping ), io.print(caps.stdio, "Hello World!") )

receive(state, msg) =
    msg ?
    | Ping ->
        ( state, cmd.send(state.pinger, Pong) )

pinger = %( init = pinger-init, receive = pinger-receive )

pinger-init(ping) =
    ( %( pinger = ping ), ping() )

pinger-receive(state, msg) =
    msg ?
    | Pong ->
        ( state, state.pinger() )
""" |> trim [||]

let addNewline = [| '\n' |]

let cm1 =
    """
= %( init, receive )
    """ |> trim addNewline

let cm2 =
    """
%( io, cmd, spawn ) = /stdlib
    """ |> trim addNewline

let cm3a =
    """
ping =
    spawn(pinger, cmd.send(caps.self, ""))
(%(pinger = ping), io.print(caps.stdio, "Hello World!"))
    """ |> trim [||]

let cm3 =
    """
init(caps) =
    ping = spawn(pinger, cmd.send(caps.self, ""))
    (%(pinger = ping), io.print(caps.stdio, "Hello World!"))
    """ |> trim addNewline

let cm4 =
    """
receive(state, msg) =
    msg ?
    | Ping ->
        ( state, cmd.send(state.pinger, Pong) )
    """ |> trim addNewline

let cm5 =
    """
pinger = %( init = pinger-init, receive = pinger-receive )
    """ |> trim addNewline

let cm6 =
    """
pinger-init(ping) =
    ( %( pinger = ping ), ping() )
    """ |> trim addNewline

let cm7 =
    """
pinger-receive(state, msg) =
    msg ?
    | Pong ->
        ( state, state.pinger() )
    """ |> trim addNewline

let simpleTypeDef =
    """
    Msg =
        | Ping
        | Pong
    """ |> trim addNewline


(********************************************
*
* Utility Parsers
*
*********************************************)


let possibleImportSection =
    sepEndBy chakraImport deadspace <??> "imports"

let p = 
    chakraModuleDef
    <?> "Module definition"
    .>> deadspace
    .>>.? possibleImportSection

let manyMatchClauses = sepEndBy1 chakraMatchClause deadspace

let manyBindings =
    many (chakraBinding .>>? deadspace)

let exprThenDeadspace =
    let bindingsThenExpr =
        manyBindings
        .>>.? (chakraExpr .>>? deadspace)

    (attempt (chakraExpr |>> (fun e -> ([], e))) <!> "just an expr")
    <|> (bindingsThenExpr <!> "bindings then expr")
    <!> "new-school expr list parser"


let structDeclWithOneField = TypeSystem.strct ([("a", TypeSystem.str); ("b", TypeSystem.str)], false)

(********************************************
*
* Tests
*
*********************************************)



type ChakraTest = ((unit -> string * Result<string, string * string>))

let test (label: string) parser (str: string) () =
    match run (parser .>> eof) str with
    | Success _ ->
        (label, Result.Ok label)
    | Failure (e, _, _) ->
        (label, Result.Error (label, e))


let typeTest (label: string) (decl: string) (ty: TypeSystem.Type) () =
    match run (chakraTypeExpr .>> eof) decl with
    | Success (parsed, _, _) ->
        let typ = parsed.ToType
        if typ = ty then
            (label, Result.Ok label)
        else
            let e = sprintf "%O did not match %O" typ ty
            (label, Result.Error (label, e))
    | Failure (e, _, _) ->
        (label, Result.Error (label, e))

let (tests: ChakraTest list) =
    [ test "Module def" chakraModuleDef moduleDefFix
      test "Something" p s1
      test "deadspace" deadspace "\n"
      test "deadspace with line comment" deadspace "; a comment\n"
      test "lots of deadspace" deadspace "\n\n\n\n\n; comment\n\n\n"
      test "spread" spread "...hello"
      test "Simple package import" chakraImport "stdlib = /stdlib"
      test "Simple relative import" chakraImport "something = ./something"
      test "Simple root import" chakraImport "lib = /root/lib"
      test "Destructured package import" chakraImport destructuredImportFix
      test "Destructured relative import" chakraImport "%( io, string ) = ./something"
      test "Destructured root import" chakraImport "%( io, string ) = /root/lib"
      test "Simple var" chakraVar varFix
      test "Complex var" chakraVar complexVarFix
      test "Struct access" chakraVar "struct.field.subfield"
      test "Integer" chakraNumber numberIntFix
      test "Float" chakraNumber numberFloatFix
      test "Simple String" chakraString (""" "This is a string" """.Trim([|' '|]))
      test "Simple tuple" chakraTuple "(1, 1)"
      test "Nested Tuple" chakraTuple "( 1, Ok(\"string\") )"
      test "Simple struct" chakraStruct "%( a = 1 )"
      test "Struct with two fields" chakraStruct "%( num = 1, string = \"Hello\" )"
      test "Struct with punning" chakraStruct "%( num, string )"
      test "Struct with rest" chakraStruct "%(\n\tnum = 1,\n\tstring = \"Hello\",\n\t...other\n)"
      test "List of Integers" chakraList listOfIntsFix
      test "List with rest" chakraList "[ 1, 2, 3, ...rest ]"
      test "Simple map" chakraMap "%[ \"a\" = 1 ]"
      test "Map with multiple pairs" chakraMap "%[\n\t\"a\" = 1,\n\t\"b\" = 2\n]"
      test "Map with multiple pairs and rest" chakraMap "%[\n\t\"a\" = 1,\n\t\"b\" = 2,\n\t...other\n]"
      test "Apply" chakraApply simpleApplyFix
      test "Simple var as expression" chakraExpr varFix
      test "Complex var as expression" chakraExpr complexVarFix
      test "Struct access as expression" chakraExpr "struct.field.subfield"
      test "Integer as expression" chakraExpr numberIntFix
      test "Float as expression" chakraExpr numberFloatFix
      test "Simple String as expression" chakraExpr (""" "This is a string" """.Trim([|' '|]))
      test "Simple tuple as expression" chakraExpr "(1, 1)"
      test "Nested Tuple as expression" chakraExpr "( 1, Ok(\"string\") )"
      test "Simple struct as expression" chakraExpr "%( a = 1 )"
      test "Struct with two fields as expression" chakraExpr "%( num = 1, string = \"Hello\" )"
      test "Struct with punning as expression" chakraExpr "%( num, string )"
      test "Struct with rest as expression" chakraExpr "%(\n\tnum = 1,\n\tstring = \"Hello\",\n\t...other\n)"
      test "List of Integers as expression" chakraExpr listOfIntsFix
      test "Simple map as expression" chakraExpr "%[ \"a\" = 1]"
      test "Map with multiple pairs as expression" chakraExpr "%[ \"a\" = 1, \"b\" = 2 ]"
      test "Apply expression" chakraApplyExpr simpleApplyFix
      test "Apply expression as expression" chakraExpr simpleApplyFix
      test "Pipe expression" chakraPipeExpr "something\n> else(1)\n"
      test "Pipe with all applies" chakraPipeExpr "head(list)\n> add(2)\n> mul(3)\n> div(6)\n"
      test "Simple binding" chakraBinding simplestBindingFix
      test "Simple Binding With String Fix" chakraBinding simpleBindingWithStringFix
      test "Simple Binding With List Fix" chakraBinding simpleBindingWithListFix
      test "Simple Binding With Tuple Fix" chakraBinding simpleBindingWithTupleFix
      test "Simple Binding With Struct Fix" chakraBinding simpleBindingWithStructFix
      test "Simple Binding With Map Fix" chakraBinding simpleBindingWithMapFix
      test "Simple Binding With Binding In El Fix" chakraBinding simpleBindingWithBindingInELFix
      test "Simple Binding With Struct Binding in El Fix" chakraBinding simpleBindingWithStructBindingInELFix
      test "Destructured binding" chakraBinding "%( a, b ) = get-struct(arg)"
      test "Number-only expr list" chakraExprList numberOnlyExprListFix
      test "List of integers expr list" chakraExprList listOfIntsExprListFix
      test "Function binding with binding in expr list" chakraBinding fnBindingWithBindingInELFix
      test "Function with apply binding in expression list" chakraBinding fnBindingWithApplyBindingInELFix
      test "Apply-only expr list" chakraExprList applyOnlyExprListFix
      test "Simple Match Head" matchHead "some-var ?\n"
      test "simple match clause" chakraMatchClause "| _ -> 1"
      test "many match clauses" manyMatchClauses "| _ -> 1\n| _ -> 2"
      test "Match expression" chakraMatchExpr simpleVarHeadMatchFix
      test "Match expression with apply expr list" chakraExprList simpleVarHeadMatchWithApplyFix
      test "Match expression as expression" chakraExpr simpleVarHeadMatchFix
      test "Simplest Function Binding" chakraBinding simplestFunctionBindingFix
      test "Function Binding With String" chakraBinding functionBindingWithStringFix
      test "Annotated Function Binding With String" chakraBinding annotatedFunctionBindingWithStringFix
      test "Function Binding With Symbol" chakraBinding functionBindingWithSymbolFix
      test "Function Binding With List" chakraBinding functionBindingWithListFix
      test "Function Binding With Tuple" chakraBinding functionBindingWithTupleFix
      test "Function Binding With Struct" chakraBinding functionBindingWithStructFix
      test "Function Binding With Map" chakraBinding functionBindingWithMapFix
      test "Simple lambda" chakraLambda simpleLambda
      test "Nullary lambda" chakraLambda nullaryLambda
      test "Type identifier" pTypeIdent "String"
      test "Zero arg type constructor" (attempt pTypeConstructor <|> noArgConstructor ()) "Cmd"
      test "One arg type constructor" (attempt pTypeConstructor <|> noArgConstructor ()) "Maybe(a)"
      test "Simple type definition" chakraTypeDef simpleTypeDef
      test "Simple complete module" (chakraModule "Testing") simpleCompleteModule
      test "Complex complete module" (chakraModule "Testing") complexCompleteModule
      test "Module Definition 1"  chakraModuleDef cm1
      test "Import" chakraImport cm2
      test "Expr list" chakraExprListRef.Value cm3a
      test "Binding 1"  chakraBinding cm3
      test "Binding 2"  chakraBinding cm4
      test "Binding 3"  chakraBinding cm5
      test "Binding 4"  chakraBinding cm6
      test "Binding 5"  chakraBinding cm7
      test "lib.chakra" (chakraModule "./libs/lib-a/lib.chakra") (System.IO.File.ReadAllText "../Examples/test/libs/lib-a/lib.chakra")
      test "ping-pong/main.chakra" (chakraModule "./main.chakra") (System.IO.File.ReadAllText "../Examples/ping-pong/main.chakra")
      typeTest "String declaration" "\"\"" TypeSystem.str
      typeTest "Number declaration" "#" TypeSystem.num
      typeTest "Tuple of string and string declaration" "( \"\", \"\" )" (TypeSystem.tup [TypeSystem.str; TypeSystem.str])
      typeTest "Tuple of string and nunber declaration" "( \"\", # )" (TypeSystem.tup [TypeSystem.str; TypeSystem.num])
      typeTest "Struct with one field declaration" "%( a = \"\", b = \"\" )" structDeclWithOneField
      typeTest "List declaration with generic type" "[ a ]" (TypeSystem.list (TypeSystem.genA))
      typeTest "Map declaration" "%[ \"\" = a ]" (TypeSystem.map TypeSystem.str TypeSystem.genA)
      typeTest "Custom type declaration" "Maybe(\"\")" (TypeSystem.CustomType ("Maybe", [TypeSystem.str]))
      typeTest "Custom type declaration with no args" "Msg" (TypeSystem.CustomType ("Msg", [])) ]



(********************************************
*
* # Test runner
*
*********************************************)



let runTest (filter: string) (successes: int, failures: int, skipped: int) (t: ChakraTest) =
    match t () with
    | (l, Result.Ok label) when l.Contains(filter) ->
        CConsole.green label
        |> printfn "%s"

        (successes + 1, failures, skipped)
    | (l, Result.Error (label, diag)) when l.Contains(filter) ->
        CConsole.red diag
        |> printfn "%s:\n%s" label

        (successes, failures + 1, skipped)
    | _ ->
        (successes, failures, skipped + 1)

let startBannerText = "Chakra Fparsec-base Parser Tests START"
let width = System.Console.BufferWidth
printfn "\n%s\n" (String.replicate width "=")
printfn "%s" startBannerText
printfn "\n%s\n" (String.replicate width "-")

let timer = System.Diagnostics.Stopwatch.StartNew ()
let (successes, failures, skipped) = List.fold (runTest "") (0, 0, 0) tests
timer.Stop ()

printfn "\n%s\n" (String.replicate width "-")
printfn "Successed: %i, Failed: %i, Skipped: %i" successes failures skipped
let endBannerText = sprintf "Chakra Fparsec-base Parser Tests FINISHED %i tests IN %i ms" (tests.Length) (timer.ElapsedMilliseconds)
printfn "%s" endBannerText
printfn "\n%s\n" (String.replicate width "=")
// if failures > 0 then
//     exit 1
// else
//     exit 0
