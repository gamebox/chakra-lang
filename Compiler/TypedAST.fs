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
    | TCPTuple of Span * TCPattern list
    | TCPStruct of Span * TCPStruct
    | TCPList of Span * TCPList
    | TCPMap of Span * TCPMap

and TCPStructField =
    { Loc: Span
      Name: string
      ValuePattern: TCPattern }

and TCPStruct =
    { Fields: TCPStructField list
      Rest: bool }

and TCPMapPair =
    { Loc: Span
      KeyPattern: TCPattern
      ValuePattern: TCPattern }

and TCPMap =
    { Pairs: TCPMapPair list
      Rest: (Span * string) option }

and TCPList =
    { Items: TCPattern list
      Rest: (Span * string) option }

type TCBindingPattern =
    | TCSimpleBindingPattern of string
    | TCFunctionBindingPattern of FunctionBindPatternInfo
    | TCComplexBindingPattern of TCPattern

and TCLiteral =
    | TCVar of (string * (string list) option) * Type
    | TCNumber of Decimal
    | TCSymbol of string
    | TCString of string
    | TCTuple of TCExpr list
    | TCStruct of TCStruct
    | TCList of TCList
    | TCMap of TCMap
    | TCLambda of TCLambda

    member x.Typ =
        match x with
        | TCVar (_, t) -> t
        | TCNumber _ -> num
        | TCSymbol string -> SymbolType string
        | TCString _ -> str
        | TCTuple exprs -> exprs |> List.map (fun x -> x.Typ) |> tup
        | TCStruct { Fields = fs; Spread = s } ->
            let fields =
                List.map (fun f -> (f.Name, f.TValue.Typ)) fs

            strct (fields, s.IsSome, None)
        | TCList p -> p.Typ
        | TCMap m -> map m.KeyType m.ValueType
        | TCLambda l ->
            let args =
                List.map (fun a -> (a.ArgName, a.Typ)) l.Args

            fn args l.Body.Typ

and TCStructField =
    { Loc: Span
      Name: string
      TValue: TCExpr }

and TCStruct =
    { Fields: TCStructField list
      Spread: (Span * string) option }

and TCMapPair =
    { Loc: Span
      Key: TCLiteral
      Value: TCExpr }

and TCMap =
    { Pairs: TCMapPair list
      Spread: (Span * string) option
      KeyType: Type
      ValueType: Type }

and TCList =
    { Items: TCExpr list
      Spread: (Span * string) option
      Typ: Type }

and TCArg =
    { Loc: Span
      ArgName: string
      Typ: Type }

and TCLambda = { Args: TCArg list; Body: TCExprList }

and TCMatch = TCMatch of (TCLiteral * Type * TCMatchClause list)

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
    | TCLiteralExpr of (Span * TCLiteral)
    | TCMatchExpr of (Span * Type * TCMatch)
    | TCApplyExpr of (Span * Type * TCApply)
    | TCPipeExpr of TCPipe
    | TCNativeExpr of (Span * Type)

    member x.Typ =
        match x with
        | TCLiteralExpr (_, lit) -> lit.Typ
        | TCMatchExpr (_, t, _) -> t
        | TCApplyExpr (_, t, _) -> t
        | TCPipeExpr pipe -> pipe.Typ
        | TCNativeExpr (_, t) -> t

and TCApply =
    | TCApply of ((string * string list) * TCExpr list)
    | TCNamedApply of ((string * string list) * (Span * (string * TCExpr)) list)

and TCPipeHead =
    | TCPipeLiteralHead of TCLiteral
    | TCPipeApplyHead of TCApply

and TCPipe =
    { Loc: Span
      Head: TCPipeHead
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
