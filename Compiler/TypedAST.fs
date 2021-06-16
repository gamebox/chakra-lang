module TypedAST

open AST
open ParserLibrary
open System
open TypeSystem

type TCPattern =
    | TCPIgnore of Span
    | TCPVar of Span * string
    | TCPNumber of Span * Decimal
    | TCPSymbol of Span * string
    | TCPString of Span * string
    | TCPTuple of Span * TCPattern list * Type list
    | TCPStruct of Span * TCPStruct
    | TCPList of Span * TCPList
    | TCPMap of Span * TCPMap

and TCPStructField =
    { Loc: Span
      Name: string
      ValuePattern: TCPattern
      Typ: Type }

and TCPStruct =
    { Fields: TCPStructField list
      Rest: bool
      Typ: Type }

and TCPMapPair =
    { Loc: Span
      KeyPattern: TCPattern
      ValuePattern: TCPattern
      KeyType: Type
      ValueType: Type }

and TCPMap =
    { Pairs: TCPMapPair list
      Rest: (Span * string) option
      KeyType: Type
      ValueType: Type }

and TCPList =
    { Items: TCPattern list
      Rest: (Span * string) option
      Typ: Type }

type TCBindingPattern =
    | TCSimpleBindingPattern of string
    | TCFunctionBindingPattern of TCFunctionBindPatternInfo
    | TCComplexBindingPattern of TCPattern

and TCFunctionBindPatternInfo =
    { TypedArgs: (string * Type) list
      Name: string }
and TCStructField =
    { Loc: Span
      Name: string
      TValue: TCExpr }

and TCStruct =
    { Fields: TCStructField list
      Spread: (Span * string) option
      Typ: Type }

and TCMapPair =
    { Loc: Span
      Key: TCExpr
      Value: TCExpr }

and TCMap =
    { Pairs: TCMapPair list
      Spread: (Span * string) option
      Typ: Type }

and TCList =
    { Items: TCExpr list
      Spread: (Span * string) option
      Typ: Type }

and TCArg =
    { Loc: Span
      ArgName: string
      Typ: Type }

and TCLambda = { Args: TCArg list; Body: TCExprList; Typ: Type }

and TCMatch = TCMatch of (TCExpr * Type * TCMatchClause list)

and TCMatchClause = TCMatchClause of (TCPattern * TCExprList)

and TCBinding =
    { Loc: Span
      TypedPattern: TCBindingPattern
      TypedExprList: TCExprList
      DocComment: string option
      Typ: Type }

and TCExprList =
    | TCExprList of (TCBinding list * TCExpr)

    member x.Typ =
        let (TCExprList (_, expr)) = x
        expr.Typ

and TCExpr =
    | TCVar of (string * (string list) option) * Type
    | TCNumber of Decimal
    | TCSymbol of string
    | TCString of string
    | TCTuple of TCExpr list * Type
    | TCStruct of TCStruct
    | TCList of TCList
    | TCMap of TCMap
    | TCApplyExpr of (Span * Type * TCApply)
    | TCLambda of TCLambda
    | TCMatchExpr of (Span * TCMatch)
    | TCPipeExpr of TCPipe
    | TCNativeExpr of (Span * Type)

    member x.Typ =
        match x with
        | TCVar (_, t) -> t
        | TCNumber _ -> num
        | TCSymbol string -> SymbolType string
        | TCString _ -> str
        | TCTuple (_, t) -> t
        | TCStruct { Typ = ty } -> ty
        | TCList p -> p.Typ
        | TCMap m -> m.Typ
        | TCLambda l -> l.Typ
        | TCMatchExpr (_, TCMatch (_, t, _)) -> t
        | TCApplyExpr (_, t, _) -> t
        | TCPipeExpr pipe -> pipe.Typ
        | TCNativeExpr (_, t) -> t

and TCApply =
    | TCApply of (ApplyIdentifier * TCExpr list)
    | TCNamedApply of (ApplyIdentifier * (Span * (string * TCExpr)) list)

and TCPipe =
    { Loc: Span
      Head: TCExpr
      Tail: (Span * TCApply) list
      Typ: Type }

type TCModule =
    { DocComments: string option
      ExportMap: Map<string, Type>
      Bindings: TCBinding list
      Imports: ChakraImport list }

let tcModuleAsStruct { ExportMap = es } = strct (Map.toList es, false, None)

let tcModule (cmod: ChakraModule) exports bs =
    { DocComments = cmod.DocComments
      ExportMap = exports
      Bindings = bs
      Imports = cmod.Imports }


let tcBinding (b: AST.ChakraBinding) patt el ty=
    { Loc = b.Loc
      TypedPattern = patt
      TypedExprList = el
      DocComment = b.DocComment
      Typ = ty }