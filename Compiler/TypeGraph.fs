module TypeGraph

open AST
open TypeSystem

type ASTNode =
    | BindingNode of ChakraBinding
    | ExprNode of ChakraExpr
    | ImportNode
    | ParamNode

type TypedASTNode =
    | TypedBindingNode of TypedAST.TCBinding
    | TypedExprNode of TypedAST.TCExpr

type Relation =
    | DependsOn
    | Argument of int
    | NamedArg of string
    | Parameter of int
    | Field of string
    | PairValue of int
    | PairKey of int
    | Item of int
    | Rest
    | Applyee

type TypeGraph =
    { Nodes: Map<string, ASTNode>
      UpRelations: Map<string, (Relation * string) list>
      DownRelations: Map<string, (Relation * string) list>
      Annotations: Map<string, Type>
      Types: Set<Type>
      AnnotatedNodes: Map<string, TypedASTNode> }

let empty =
    { Nodes = Map.empty
      UpRelations = Map.empty
      DownRelations = Map.empty
      Annotations = Map.empty
      Types = Set.empty
      AnnotatedNodes = Map.empty }

let private addNode id n tg =
    { tg with
          Nodes = Map.add id n tg.Nodes
          UpRelations = Map.add id [] tg.UpRelations
          DownRelations = Map.add id [] tg.DownRelations }

let addBindingNode id b tg = addNode id (BindingNode b) tg

let addExprNode id e tg = addNode id (ExprNode e) tg

let addImportNode id tg = addNode id ImportNode tg

let addParamNode id tg = addNode id ParamNode tg

let private addEdge from to' edge tg =
    if Map.containsKey from tg.Nodes
       && Map.containsKey to' tg.Nodes then
        let fromRelations =
            (edge, to') :: Map.find from tg.UpRelations

        let toRelations =
            (edge, from) :: Map.find to' tg.DownRelations

        { tg with
              UpRelations = Map.add from fromRelations tg.UpRelations
              DownRelations = Map.add to' toRelations tg.DownRelations }
    else
        tg

let addDependentEdge from to' tg = addEdge from to' (DependsOn) tg

let addArgumentEdge from to' n tg = addEdge from to' (Argument n) tg

let addNamedArgEdge from to' s tg = addEdge from to' (NamedArg s) tg

let addParameterEdge from to' n tg = addEdge from to' (Parameter n) tg

let addFieldEdge from to' s tg = addEdge from to' (Field s) tg

let addPairValueEdge from to' n tg = addEdge from to' (PairValue n) tg

let addPairKeyEdge from to' n tg = addEdge from to' (PairKey n) tg

let addItemEdge from to' n tg = addEdge from to' (Item n) tg

let addRestEdge from to' tg = addEdge from to' (Rest) tg

let addApplyeeEdge from to' tg = addEdge from to' (Applyee) tg

let addAnnotation n ty tg =
    match Map.find n tg.Nodes with
    | _ ->
        { tg with
              Annotations = Map.add n ty tg.Annotations
              Types = Set.add ty tg.Types }


let hasNode node { Nodes = nodes } =
    Map.tryFind node nodes
    |> Option.map (fun _ -> node)

let getNodeType node { Annotations = annos } = Map.tryFind node annos
let getBindingNode node { Nodes = nodes } =
    Map.tryFind node nodes
    |> Option.bind (fun n ->
        match n with
        | BindingNode b -> Some b
        | _ -> None)

(* Display *)

let private mermaidClasses = "
classDef binding fill:#009, color:#fff, stroke: white, stroke-width: 4px
classDef expr fill:#900, color:#fff
classDef type fill:#090, color:#fff
classDef param fill:#009, color:#fff, stroke: yellow, stroke-width: 4px
classDef import fill:#009, color:#fff, stroke: green, stroke-width: 4px
    "

let private mermaidLegend = "
subgraph legend
    LEGEND_BINDING((BINDING)):::binding
    LEGEND_PARAM((PARAM)):::param
    LEGEND_IMPORT((IMPORT)):::import
    LEGEND_EXPR[EXPR]:::expr
    LEGEND_TYPE[/TYPE/]:::type
end
    "

let private firstNChars (s: string) n =
    s.Substring(0, System.Math.Min(n, s.Length))

let private mermaidNode (id, node) =
    match node with
    | BindingNode b -> sprintf "%s((\"%s\")):::binding" id id
    | ExprNode e ->
        sprintf
            "%s[\"%s\"]:::expr"
            id
            ((firstNChars (Pretty.pretty 30 (Pretty.showExpr e)) 20)
                .Replace("\"", "&ldquo;"))
    | ImportNode -> sprintf "%s((\"%s\")):::import" id id
    | ParamNode -> sprintf "%s((\"%s\")):::param" id id

let rec private mermaidPrintType typ =
    match typ with
    | UnionType types ->
        sprintf
            "< %s >"
            (types
             |> List.map mermaidPrintType
             |> String.concat " | ")
    | SumType types ->
        sprintf
            "< %s >"
            (types
             |> List.map mermaidPrintType
             |> String.concat " + ")
    | StringType -> "str"
    | NumberType -> "num"
    | SymbolType info -> sprintf "#%s" info
    | ListType genericType -> sprintf "[ %s ]" (mermaidPrintType genericType)
    | MapType (keyType, valueType) -> sprintf "%%[ %s = %s ]" (mermaidPrintType keyType) (mermaidPrintType valueType)
    | GenericType typ -> sprintf "?%s" typ
    | CommandType -> "!"
    | PolymorphicType t -> sprintf "@%s" t
    | RefType t -> sprintf "&%s" (mermaidPrintType t)

    | TupleType types ->
        List.map mermaidPrintType types
        |> String.concat ", "
        |> sprintf "( %s )"

    | StructType (fields, isOpen, tag) ->
        sprintf
            "%%( %s %s)"
            (fields
             |> List.map (fun (name, typ) -> sprintf ".%s = %s" name (mermaidPrintType typ))
             |> String.concat ", ")
            (if isOpen then "..." else "")

    | FunctionType (args, retrn) ->
        let argList =
            sprintf
                "( %s )"
                (args
                 |> List.map (mermaidPrintType << snd)
                 |> String.concat ", ")

        sprintf "{ %s -> %s }" argList (mermaidPrintType retrn)

    | CapabilityType cap ->
        match cap with
        | StdioCapability -> "$stdio"
        | FileReadCapability -> "$fread"
        | FileWriteCapability -> "$fwrite"

let private mermaidCleanType (ty: string) =
    let r (a: string) (b: string) (s: string) = s.Replace(a, b)

    ty
    |> r "(" "LEFTPAREN"
    |> r ")" "RIGHTPAREN"
    |> r "[" "LEFTBRACKET"
    |> r "]" "RIGHTBRACKET"
    |> r "{" "LEFTBRACE"
    |> r "}" "ENDBRACE"
    |> r "->" "ARROW"
    |> r "<" "LEFTANGLE"
    |> r ">" "RIGHTANGLE"
    |> r "!" "BANG"
    |> r "@" "AT"
    |> r "=" "EQUAL"
    |> r "&" "AMP"
    |> r "%" "PERCENT"
    |> r "," "COMMA"
    |> r " " ""

let private mermaidType (ty: TypeSystem.Type) =
    let typ = mermaidPrintType ty
    sprintf "%s[/\"%s\"/]:::type" (mermaidCleanType typ) typ

let private mermaidRelToEdge rel =
    match rel with
    | DependsOn -> "-->"
    | Argument i -> sprintf "-->| arg %i |" i
    | NamedArg s -> sprintf "-->| arg '%s' |" s
    | Parameter i -> sprintf "-->| param %i |" i
    | Field s -> sprintf "-->| field '%s' |" s
    | PairValue i -> sprintf "-->| pairvalue %i |" i
    | PairKey i -> sprintf "-->| pairkey %i |" i
    | Item i -> sprintf "-->| listitem %i |" i
    | Rest -> "-->| rest |"
    | Applyee -> "-->| applyee |"

let private mermaidEdge (from, (edge, to')) =
    sprintf "%s %s %s" from (mermaidRelToEdge edge) to'

let private mermaidAnnoToEdge (nodeId, ty) =
    sprintf "%s === %s" nodeId (mermaidCleanType (mermaidPrintType ty))


let toMermaid
    { Nodes = nodes
      UpRelations = rels
      Types = tys
      Annotations = annos }
    (withLegend: bool)
    =
    let n =
        Map.toList nodes
        |> List.map mermaidNode
        |> List.toSeq
        |> String.concat "\n"

    let t =
        Set.toList tys
        |> List.map mermaidType
        |> List.toSeq
        |> String.concat "\n"

    let e =
        Map.toList rels
        |> List.map
            (fun (id, edges) ->
                List.map (fun e -> mermaidEdge (id, e)) edges
                |> String.concat "\n")
        |> List.toSeq
        |> String.concat "\n"

    printfn "Annos are\n===\n%O" annos

    let a =
        Map.toList annos
        |> List.map mermaidAnnoToEdge
        |> List.toSeq
        |> String.concat "\n"

    sprintf
        "
```mermaid
graph TB
%s

%s

%s

%s

%s
%s
```
    "
        n
        t
        e
        a
        mermaidClasses
        mermaidLegend