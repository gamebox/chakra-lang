module TypeGraph

open AST
open TypeSystem

type ASTNode =
    | BindingNode of ChakraBinding
    | ExprNode of ChakraExpr
    | ImportNode
    | ParamNode
    | AbstractNode

    member x.IsParam =
        match x with
        | ParamNode -> true
        | _ -> false

    member x.IsAbstract =
        match x with
        | AbstractNode -> true
        | _ -> false

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

let addAbstractNode id tg = addNode id AbstractNode tg

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

let addDependentEdge from to' tg =
    // printfn "Adding depedent edge from '%s' to '%s'" from to'
    addEdge from to' (DependsOn) tg

let addArgumentEdge from to' n tg = addEdge from to' (Argument n) tg

let addNamedArgEdge from to' s tg = addEdge from to' (NamedArg s) tg

let addParameterEdge from to' n tg = addEdge from to' (Parameter n) tg

let addFieldEdge from to' s tg = addEdge from to' (Field s) tg

let addPairValueEdge from to' n tg = addEdge from to' (PairValue n) tg

let addPairKeyEdge from to' n tg = addEdge from to' (PairKey n) tg

let addItemEdge from to' n tg = addEdge from to' (Item n) tg

let addRestEdge from to' tg = addEdge from to' (Rest) tg

let addApplyeeEdge from to' tg =
    // printfn "Adding applyee edge from '%s' to '%s'" from to'
    addEdge from to' (Applyee) tg

let rec addAnnotation n ty tg =
    let withNewAnno =
        { tg with
              Annotations = Map.add n ty tg.Annotations
              Types = Set.add ty tg.Types }

    match Map.tryFind n withNewAnno.Nodes with
    | Some (ExprNode (ChakraVar _)) ->
        Map.tryFind n withNewAnno.UpRelations
        |> Option.bind
            (fun rels ->
                let dependsOnParam (_, s) = (Map.find s withNewAnno.Nodes).IsParam
 
                List.tryFind dependsOnParam rels
                |> Option.map
                    (fun (_, s) ->
                        // printfn "The var at %s depends on param at %s" n s
                        addAnnotation s ty withNewAnno))
        |> Option.defaultValue withNewAnno
    | Some _ -> withNewAnno
    | None -> tg


let hasNode node { Nodes = nodes } =
    Map.containsKey node nodes

let getNodeType node { Annotations = annos } = Map.tryFind node annos

let getBindingNode node { Nodes = nodes } =
    Map.tryFind node nodes
    |> Option.bind
        (fun n ->
            match n with
            | BindingNode b -> Some b
            | _ -> None)

let getExprNode node { Nodes = nodes } =
    Map.tryFind node nodes
    |> Option.bind
        (fun n ->
            match n with
            | ExprNode e -> Some e
            | _ -> None)

let getDependents node { DownRelations = rels } =
    Map.tryFind node rels
    |> Option.map (fun deps -> List.map snd deps)
    |> Option.defaultWith (fun () -> [])

let private getAllDependencies<'a> node (edgePredicate: (Relation * string) -> 'a list) { UpRelations = rels }  =
    Map.tryFind node rels
    |> Option.map (fun deps -> List.collect edgePredicate deps)
    |> Option.defaultWith (fun () -> [])

let private getDependencies node (edgePredicate: (Relation * string) -> string list) { UpRelations = rels } =
    Map.tryFind node rels
    |> Option.bind (fun deps -> List.collect edgePredicate deps |> List.tryHead)

let getFields node tg =
    getAllDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Field f -> [ (f, s) ]
            | _ -> [])
        tg


let getArg node n tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Argument i when i = n -> [ s ]
            | _ -> [])
        tg

let getItem node n tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Item i when i = n -> [ s ]
            | _ -> [])
        tg

let getField node name tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Field f when f = name -> [ s ]
            | _ -> [])
        tg

let getPair node n tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | PairKey i when i = n -> [ s ]
            | _ -> [])
        tg
    |> Option.bind
        (fun key ->
            // printfn "Found key at %s" key

            getDependencies
                node
                (fun (rel, s) ->
                    match rel with
                    | PairValue i when i = n -> [ s ]
                    | _ -> [])
                tg
            |> Option.map (fun value -> (key, value)))

let getApplyee node tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Applyee -> [ s ]
            | _ -> [])
        tg

let getExprListExpr node tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | DependsOn -> [ s ]
            | _ -> [])
        tg

let getVarDef = getExprListExpr

let getParam node n tg =
    getDependencies
        node
        (fun (rel, s) ->
            match rel with
            | Parameter i when i = n -> [ s ]
            | _ -> [])
        tg

/// Finds the next node that can be annotated.  Skips nodes that are a parameter.
let findAnnotationTarget tg =
    let { Nodes = nodes
          UpRelations = uprels
          DownRelations = deprels
          Annotations = annos } =
        tg

    let primaryTarget =
        Map.toSeq uprels
        |> Seq.tryFind
            (fun (k, rels) ->
                let node = Map.find k nodes
                let isParamNode = node.IsParam
                let isAbstractNode = node.IsAbstract

                (List.filter (fun (rel, dep) -> (Map.containsKey dep annos)) rels)
                    .Length
                |> (=) rels.Length
                |> (&&) (not isParamNode)
                |> (&&) (not isAbstractNode)
                |> (&&) (not (Map.containsKey k annos)))
        |> Option.map fst

    let applyWithApplyeeAnnotated () =
        // printfn "Looking for an apply expr with its applyee annotated"
        Map.toSeq nodes
        |> Seq.tryFind
            (fun (n, node) ->
                match node with
                | ExprNode (ChakraApplyExpr _) ->

                    let applyeeIsAnnotated =
                        getApplyee n tg
                        |> Option.filter (fun applyee -> Map.containsKey applyee annos)
                        |> Option.isSome

                    (not (Map.containsKey n annos))
                    && applyeeIsAnnotated
                | _ -> false)
        |> Option.map fst

    let unannotatedParamDep () =
        // printfn "Looking for unannotated parameter dependency"
        Map.toSeq nodes
        |> Seq.filter (fun (n, node) ->
            node.IsParam && (not (Map.containsKey n annos)))
        |> Seq.collect (fun (n, node) ->
            Map.tryFind n deprels
            |> Option.map (fun rels ->
                List.filter
                    (fun (rel, s) ->
                        match rel with
                        | Parameter _ -> false
                        | _ -> true)
                    rels
                |> List.toSeq)
            |> Option.defaultWith (fun () -> Seq.empty))
        |> Seq.tryHead
        |> Option.map snd

    primaryTarget
    |> Option.orElseWith applyWithApplyeeAnnotated
    // |> Option.orElseWith unannotatedParamDep

let findAnnotationLeaf
    { Nodes = nodes
      UpRelations = rels
      Annotations = annos }
    =
    Map.tryFindKey
        (fun k (v: (Relation * string) list) ->
            let annotated =
                List.filter (fun (r, s) -> (Map.containsKey s annos)) v
            annotated.Length = v.Length && (not (Map.containsKey k annos)))
        rels

(* Display *)

let private mermaidClasses = "
classDef binding fill:#009, color:#fff, stroke: white, stroke-width: 4px
classDef expr fill:#900, color:#fff
classDef type fill:#090, color:#fff
classDef param fill:#009, color:#fff, stroke: yellow, stroke-width: 4px
classDef import fill:#009, color:#fff, stroke: green, stroke-width: 4px
classDef abstract fill:#fff, color:#009, stroke: white, stroke-width: 4px
    "

let private mermaidLegend = "
subgraph legend
    LEGEND_BINDING((BINDING)):::binding
    LEGEND_PARAM((PARAM)):::param
    LEGEND_IMPORT((IMPORT)):::import
    LEGEND_ABSTRACT((ABSTRACT)):::abstract
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
    | AbstractNode -> sprintf "%s((\"%s\")):::abstract" id id

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
            "%%( %s %s )"
            (fields
             |> List.map (fun (name, typ) -> sprintf "%s = %s" name (mermaidPrintType typ))
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
    |> r "?" "QUESTION"

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
    let annotated =
        Map.toList nodes
        |> List.filter (fun (s, _) -> Map.containsKey s annos)
        |> List.map mermaidNode
        |> List.toSeq
        |> String.concat "\n"

    let unannotated =
        Map.toList nodes
        |> List.filter (fun (s, _) -> Map.containsKey s annos |> not)
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

    let a =
        Map.toList annos
        |> List.map mermaidAnnoToEdge
        |> List.toSeq
        |> String.concat "\n"

    sprintf
        "
```mermaid
graph LR

subgraph annotated
%s
end

subgraph unannotated
%s
end

subgraph types
%s
end

%s

%s

%s
%s
```
    "
        annotated
        unannotated
        t
        e
        a
        mermaidClasses
        mermaidLegend
