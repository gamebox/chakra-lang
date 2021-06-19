#r "nuget: FParsec"
#load "RunProcess.fs"
#load "Operators.fs"
#load "Graph.fs"
#load "Codegen.fs"
#load "CConsole.fs"
#load "ParserLibrary.fs"
#load "AST.fs"
#load "TypeSystem.fs"
#load "Stdlib.fs"
#load "TypeError.fs"
#load "TypedAST.fs"
#load "ChakraFparsec.fs"

open FParsec
open ChakraFparsec



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
let simplestBindingFix = "a =\n\t1\n"
let simplestFunctionBindingFix = "f(x) = 1\n"
let functionBindingWithStringFix =
    """f(x) = "Hello"
    """
let functionBindingWithSymbolFix =
    """f(x) = #testing
    """
let functionBindingWithListFix =
    """f(x) = [1, x]
    """
let functionBindingWithTupleFix =
    """f(x) = (#ok, x)
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


(********************************************
*
* Tests
*
*********************************************)



type ChakraTest = (unit -> Result<string, string * string>)

let test (label: string) parser (str: string) () =
    match run (parser .>> eof) str with
    | Success (r, _, _) ->
        Result.Ok label
    | Failure (e, _, _) ->
        Result.Error (label, e)

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
      test "Global symbol" chakraSymbol "#Some-Symbol"
      test "Local symbol" chakraSymbol "#ok"
      test "Simple tuple" chakraTuple "(1, 1)"
      test "Nested Tuple" chakraTuple "( 1, ( #ok, \"string\" ) )"
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
      test "Global symbol as expression" chakraExpr "#Some-Symbol"
      test "Local symbol as expression" chakraExpr "#ok"
      test "Simple tuple as expression" chakraExpr "(1, 1)"
      test "Nested Tuple as expression" chakraExpr "( 1, ( #ok, \"string\" ) )"
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
      test "Destructured binding" chakraBinding "%( a, b ) = get-struct(arg)"
      test "Number-only expr list" chakraExprList numberOnlyExprListFix
      test "List of integers expr list" chakraExprList listOfIntsExprListFix
      test "Function binding with binding in expr list" chakraBinding fnBindingWithBindingInELFix
      test "Apply-only expr list" chakraExprList applyOnlyExprListFix
      test "Simple Match Head" matchHead "some-var ?\n"
      test "simple match clause" chakraMatchClause "| _ -> 1"
      test "many match clauses" manyMatchClauses "| _ -> 1\n| _ -> 2"
      test "Match expression" chakraMatchExpr simpleVarHeadMatchFix
      test "Match expression with apply expr list" chakraExprList simpleVarHeadMatchWithApplyFix
      test "Match expression as expression" chakraExpr simpleVarHeadMatchFix
      test "Simplest Function Binding" chakraBinding simplestFunctionBindingFix
      test "Function Binding With String" chakraBinding functionBindingWithStringFix
      test "Function Binding With Symbol" chakraBinding functionBindingWithSymbolFix
      test "Function Binding With List" chakraBinding functionBindingWithListFix
      test "Function Binding With Tuple" chakraBinding functionBindingWithTupleFix
      test "Function Binding With Struct" chakraBinding functionBindingWithStructFix
      test "Function Binding With Map" chakraBinding functionBindingWithMapFix ]



(********************************************
*
* Test runner
*
*********************************************)



let runTest t =
    match t () with
    | Result.Ok label ->
        CConsole.green label
        |> printfn "%s"
    | Result.Error (label, diag) ->
        CConsole.red diag
        |> printfn "%s:\n%s" label

let startBannerText = "Chakra Fparsec-base Parser Tests START"
let width = System.Console.BufferWidth
printfn "\n%s\n" (String.replicate width "=")
printfn "%s" startBannerText
printfn "\n%s\n" (String.replicate width "-")

let timer = System.Diagnostics.Stopwatch.StartNew ()
List.iter runTest tests
timer.Stop ()

printfn "\n%s\n" (String.replicate width "-")
let endBannerText = sprintf "Chakra Fparsec-base Parser Tests FINISHED %i tests IN %i ms" (tests.Length) (timer.ElapsedMilliseconds)
printfn "%s" endBannerText
printfn "\n%s\n" (String.replicate width "=")
