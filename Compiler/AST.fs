module AST

open System

type Span = ParserLibrary.Span

type ChakraTypeExpr =
  | StringDecl of Span
  | NumberDecl of Span
  | TupleDecl of Span * items: ChakraTypeExpr list
  | StructDecl of Span * fields: (string * ChakraTypeExpr) list
  | ListDecl of Span * ChakraTypeExpr
  | MapDecl of Span * ChakraTypeExpr * ChakraTypeExpr
  | GenericDecl of Span * string
  | CustomDecl of Span * name: string * vars: ChakraTypeExpr list
  | FunctionDecl of Span * ChakraTypeExpr list * ChakraTypeExpr

  member x.ToType =
    match x with
    | StringDecl _ -> TypeSystem.str
    | NumberDecl _ -> TypeSystem.num
    | TupleDecl (_, items) -> TypeSystem.tup (List.map (fun (x: ChakraTypeExpr) -> x.ToType) items)
    | StructDecl (_, fields) -> TypeSystem.strct ((List.map (fun (s, (x: ChakraTypeExpr)) -> (s, x.ToType)) fields), false)
    | ListDecl (_, item) -> TypeSystem.list item.ToType
    | MapDecl (_, key, value) -> TypeSystem.map key.ToType value.ToType 
    | GenericDecl (_, v) -> TypeSystem.gen v
    | CustomDecl (_, name, vars) -> TypeSystem.CustomType (name, (List.map (fun (x: ChakraTypeExpr) -> x.ToType) vars))
    | FunctionDecl (_, args, ret) -> TypeSystem.fn (List.map (fun (x: ChakraTypeExpr) -> ("", x.ToType)) args) ret.ToType

and ChakraTypeConstructor =
    { Name: string
      Args: ChakraTypeExpr list }

type ChakraCustomType =
  { Name: string
    Args: string list
    Constructors: ChakraTypeConstructor list }

type ChakraTypeAlias =
  { Name: string
    Args: string list
    Ty: ChakraTypeExpr }

type ChakraTypeDef =
    | ChakraCustomType of Span * ChakraCustomType
    | ChakraTypeAlias of Span * ChakraTypeAlias


[<Struct>]
type ChakraVar =
    { First: string
      Rest: (string list) option }

type ChakraPattern =
    | CPIgnore of Span
    | CPVar of Span * string
    | CPNumber of Span * Decimal
    | CPString of Span * string
    | CPTuple of Span * ChakraPattern list
    | CPStruct of Span * CPStruct
    | CPList of Span * CPList
    | CPMap of Span * CPMap
    | CPCustom of Span * CPCustom

and CPCustom =
    { Constructor: string
      Args: ChakraPattern list }

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
type FunctionBindPatternInfo = { Name: string; Ret: ChakraTypeExpr option; Args: (string * ChakraTypeExpr option) list }

type ChakraIdentifier = ChakraIdentifier of string

type ChakraComment = { IsDoc: bool; Content: string }

/// type PatternMatchInfo =

type ChakraBindingPattern =
    | ChakraSimpleBindingPattern of string
    | ChakraFunctionBindingPattern of FunctionBindPatternInfo
    | ChakraComplexBindingPattern of ChakraPattern

and ChakraStructField =
    { Loc: Span
      Name: string
      Value: ChakraExpr }

and ChakraStruct =
    { Fields: ChakraStructField list
      Spread: (Span * string) option }

and ChakraMapPair =
    { Loc: Span
      Key: ChakraExpr
      Value: ChakraExpr }

and ChakraMap =
    { Pairs: ChakraMapPair list
      Spread: (Span * string) option }

and ChakraList =
    { Items: ChakraExpr list
      Spread: (Span * string) option }

and ChakraLambda =
    { Args: (string * ChakraTypeExpr option) list
      Ret: ChakraTypeExpr option
      Body: ChakraExprList }

and ChakraMatch = ChakraMatch of (ChakraExpr * ChakraMatchClause list)

and ChakraMatchClause = ChakraMatchClause of (ChakraPattern * ChakraExprList)

and ChakraBinding =
    { Loc: Span
      Pattern: ChakraBindingPattern
      ExprList: ChakraExprList
      DocComment: string option }

and ChakraExprList = ChakraExprList of (ChakraBinding list * ChakraExpr)

and ChakraExpr =
    | ChakraVar of Span * (string * (string list) option)
    | ChakraNumber of Span * Decimal
    | ChakraString of Span * string
    | ChakraTuple of Span * ChakraExpr list
    | ChakraStruct of Span * ChakraStruct
    | ChakraList of Span * ChakraList
    | ChakraMap of Span * ChakraMap
    | ChakraLambda of Span * ChakraLambda
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

and ChakraPipe =
    { Loc: Span
      Head: ChakraExpr
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
      Imports: ChakraImport list
      Types: ChakraTypeDef list }
