module AST

open ParserLibrary
open System

[<Struct>]
type ChakraVar =
    { First: string
      Rest: (string list) option }

type ChakraPattern =
    | CPIgnore of Span
    | CPVar of Span * string
    | CPNumber of Span * Decimal
    | CPSymbol of Span * string
    | CPString of Span * string
    | CPTuple of Span * ChakraPattern list
    | CPStruct of Span * CPStruct
    | CPList of Span * CPList
    | CPMap of Span * CPMap

and CPStructField =
    { Loc: Span
      Name: string
      ValuePattern: ChakraPattern }

and CPStruct =
    { Fields: CPStructField list
      Rest: bool }

and CPMapPair =
    { Loc: Span
      KeyPattern: ChakraPattern
      ValuePattern: ChakraPattern }

and CPMap =
    { Pairs: CPMapPair list
      Rest: (Span * string) option }

and CPList =
    { Items: ChakraPattern list
      Rest: (Span * string) option }

[<Struct>]
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
    | ChakraNumber of Decimal
    | ChakraSymbol of string
    | ChakraString of string
    | ChakraTuple of ChakraExpr list
    | ChakraStruct of ChakraStruct
    | ChakraList of ChakraList
    | ChakraMap of ChakraMap
    | ChakraLambda of ChakraLambda

and ChakraStructField =
    { Loc: Span
      Name: string
      Value: ChakraExpr }

and ChakraStruct =
    { Fields: ChakraStructField list
      Spread: (Span * string) option }

and ChakraMapPair =
    { Loc: Span
      Key: ChakraLiteral
      Value: ChakraExpr }

and ChakraMap =
    { Pairs: ChakraMapPair list
      Spread: (Span * string) option }

and ChakraList =
    { Items: ChakraExpr list
      Spread: (Span * string) option }

and ChakraLambda =
    { Args: string list
      Body: ChakraExprList }

and ChakraMatch = ChakraMatch of (ChakraLiteral * ChakraMatchClause list)

and ChakraMatchClause = ChakraMatchClause of (ChakraPattern * ChakraExprList)

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
    | ChakraPipeExpr of ChakraPipe
    // This expr type is for builtins.  The string is an identifier for the builtin.
    // It is illegal for these to be referenced in userland code.
    | ChakraNativeExpr of (string)

and NamedApplyPair = Span * (string * ChakraExpr)

and ApplyIdentifier = string * string list

and ChakraApply =
    | ChakraApply of (ApplyIdentifier * ChakraExpr list)
    | ChakraNamedApply of (ApplyIdentifier * NamedApplyPair list)

and ChakraPipeHead =
    | ChakraPipeLiteralHead of ChakraLiteral
    | ChakraPipeApplyHead of ChakraApply

and ChakraPipe =
    { Loc: Span
      Head: ChakraPipeHead
      Tail: (Span * ChakraApply) list }

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
