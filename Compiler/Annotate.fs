module Annotate

let (.>>.) res fn = Result.bind fn res

let joinIds id ext =
    match id with
    | "" -> ext
    | _ -> sprintf "%s/%s" id ext

let exprId id = sprintf "%s/$" id

let populateImport (envs: Map<string, TypedAST.TCModule>) tg (imp: AST.ChakraImport) =
    match imp with
    | AST.ChakraLocalImport info -> tg
    | AST.ChakraPackageImport info ->
        printfn "%s\n" info.PackageName
        printfn "%O\n" envs

        let exmap =
            (Map.find info.PackageName envs).ExportMap
            |> Map.toList

        let exType = TypeSystem.strct (exmap, false, None)

        match info.Typ with
        | AST.ChakraSimpleImportBinding s ->
            TypeGraph.addImportNode s tg
            |> TypeGraph.addAnnotation s exType
        | AST.ChakraDestructuredImportBinding p ->
            let (TypeSystem.StructType (fields, _, _)) = exType
            let fmap = Map fields

            List.fold
                (fun graph (foreign, local) ->
                    TypeGraph.addImportNode local graph
                    |> TypeGraph.addAnnotation local (Map.find foreign fmap))
                tg
                (Map.toList p)

let populateImports (imps: AST.ChakraImport list) (envs: Map<string, TypedAST.TCModule>) =
    printf "Populating imports"

    List.fold (populateImport envs) (TypeGraph.empty) imps
    |> Ok

let popFrame (var: string) =
    var.Split("/")
    |> Seq.toList
    |> List.rev
    |> List.tail
    |> List.rev
    |> String.concat "/"

let rec findNodeForVar var tg' =
    match (var, TypeGraph.hasNode var tg') with
    | (_, Some node) -> Some node
    | ("", None) -> None
    | (_, None) -> findNodeForVar (popFrame var) tg'

let rec populateExpr bname (expr: AST.ChakraExpr) tg =
    let tg' =
        TypeGraph.addExprNode (exprId bname) expr tg

    match expr with
    | AST.ChakraNumber (_, _) -> TypeGraph.addAnnotation (exprId bname) TypeSystem.num tg'
    | AST.ChakraString (_, _) -> TypeGraph.addAnnotation (exprId bname) TypeSystem.str tg'
    | AST.ChakraSymbol (_, s) -> TypeGraph.addAnnotation (exprId bname) (TypeSystem.SymbolType s) tg'
    | AST.ChakraVar (_, (var, None)) ->
        match findNodeForVar var tg' with
        | Some node -> TypeGraph.addDependentNode (exprId bname) node tg'
        | None -> tg'
    | _ -> tg'

and populateExprList bname (AST.ChakraExprList (bs, expr)) tg =
    List.fold (populateBinding bname) tg (List.mapi (fun i b -> b, i) bs)
    |> populateExpr bname expr

and populateBinding bname tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s ->
        let n = joinIds bname s

        TypeGraph.addBindingNode n b tg
        |> populateExprList n b.ExprList
        |> TypeGraph.addDependentNode n (exprId n)
    | AST.ChakraComplexBindingPattern p -> tg
    | AST.ChakraFunctionBindingPattern p ->
        let n = joinIds bname p.Name

        TypeGraph.addBindingNode n b tg
        |> populateExprList n b.ExprList
        |> TypeGraph.addDependentNode n (exprId n)

let populateTopLevelBinding tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s -> TypeGraph.addBindingNode s b tg
    | AST.ChakraComplexBindingPattern p -> tg
    | AST.ChakraFunctionBindingPattern p -> TypeGraph.addBindingNode p.Name b tg

let private inspect label (tg: TypeGraph.TypeGraph) =
    printf "%s\n----$$$----\n%s\n----$$$----" label (TypeGraph.toMermaid tg true)
    tg


let populateTopLevelBindings (bs: AST.ChakraBinding list) (tg: TypeGraph.TypeGraph) =
    let bsi = List.mapi (fun i b -> b, i) bs

    bsi
    |> List.fold populateTopLevelBinding tg
    |> (fun tg' -> List.fold (populateBinding "") tg' bsi)
    |> inspect "Typegraph"
    |> Ok


let lowerIntoTypedAst (m: AST.ChakraModule) (tg: TypeGraph.TypeGraph) =
    (List.fold
        (fun acc b ->
            match acc with
            | Ok exmap -> acc
            | _ -> acc)
        (Ok Map.empty)
        m.Bindings)
    |> Result.map (fun exports -> TypedAST.tcModule m exports [])

/// This function attempts to take the Untyped AST of a module, as well
/// as the resolved export types of all modules it is dependent on and
/// and perform the following operations:
/// 1. Transform the syntax tree into a graph
/// 2. Work from the leaf nodes, down, annotating every node in the graph
///    with the most specific type
/// 3. Transform the fully annotated graph into a Typed AST for the module
let annotate moduleName (m: AST.ChakraModule) (envs: Map<string, TypedAST.TCModule>) =
    printfn "In annotate"

    populateImports m.Imports envs
    .>>. populateTopLevelBindings m.Bindings
    .>>. lowerIntoTypedAst m
