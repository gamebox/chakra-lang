module Annotate

let private inspect (label: string) (tg: TypeGraph.TypeGraph) =
    let path = sprintf "/home/anthony/.%s.md" label
    let diagram = TypeGraph.toMermaid tg true
    System.IO.File.WriteAllText(path, diagram)
    tg

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
                    printfn "Adding %s as %s from package %s" foreign local info.PackageName
                    printfn "Type is %O" (Map.find foreign fmap)

                    TypeGraph.addImportNode local graph
                    |> TypeGraph.addAnnotation local (Map.find foreign fmap))
                tg
                (Map.toList p)

let popFrame (var: string) =
    var.Split("/")
    |> Seq.toList
    |> List.rev
    |> List.tail
    |> List.rev
    |> String.concat "/"

let rec findNodeForVar var path tg' =
    printfn "findNodeForVar %s %s" var path

    match (path, TypeGraph.hasNode (joinIds path var) tg') with
    | (_, Some node) -> Some node
    | ("", None) -> None
    | (_, None) -> findNodeForVar var (popFrame path) tg'

let applyIdToString (root, path) = root


let withIndex list = List.mapi (fun i x -> (i, x)) list

let rec populateExpr bname (expr: AST.ChakraExpr) tg =
    printfn "populating expr %O" expr
    let n = exprId bname
    let tg' = TypeGraph.addExprNode n expr tg

    match expr with
    | AST.ChakraNumber (_, _) -> TypeGraph.addAnnotation n TypeSystem.num tg'
    | AST.ChakraString (_, _) -> TypeGraph.addAnnotation n TypeSystem.str tg'
    | AST.ChakraSymbol (_, s) -> TypeGraph.addAnnotation n (TypeSystem.SymbolType s) tg'
    | AST.ChakraVar (_, (var, None)) ->
        match findNodeForVar var n tg' with
        | Some node -> TypeGraph.addDependentEdge n node tg'
        | None -> tg'
    | AST.ChakraVar (_, (var, Some path)) -> tg'
    | AST.ChakraTuple (_, items) ->
        let addItems graph =
            List.fold
                (fun acc (i, expr) ->
                    let argId = joinIds n (sprintf "ITEM-%i" i)

                    populateExpr argId expr acc
                    |> TypeGraph.addItemEdge n (joinIds argId "$") i)
                graph
                (withIndex items)

        let addSpread graph = graph

        addItems tg'
    | AST.ChakraList (_, { Items = items; Spread = spread }) ->
        let addItems graph =
            List.fold
                (fun acc (i, expr) ->
                    let argId = joinIds n (sprintf "ITEM-%i" i)

                    populateExpr argId expr acc
                    |> TypeGraph.addItemEdge n (joinIds argId "$") i)
                graph
                (withIndex items)

        let addSpread graph = graph

        addItems tg' |> addSpread
    | AST.ChakraMap (_, { Pairs = pairs; Spread = spread }) ->
        let addPairs graph =
            List.fold
                (fun acc (i, { Key = key; Value = value }: AST.ChakraMapPair) ->
                    let keyId = joinIds n (sprintf "PAIR-%i-KEY" i)
                    let valueId = joinIds n (sprintf "PAIR-%i-VALUE" i)

                    populateExpr keyId key acc
                    |> TypeGraph.addPairKeyEdge n (joinIds keyId "$") i
                    |> populateExpr valueId value
                    |> TypeGraph.addPairValueEdge n (joinIds valueId "$") i)
                graph
                (withIndex pairs)

        let addSpread graph = graph

        addPairs tg' |> addSpread
    | AST.ChakraStruct (_, { Fields = fields; Spread = spread }) ->
        let addFields graph =
            List.fold
                (fun acc ({ Name = name; Value = value }: AST.ChakraStructField) ->
                    let argId = joinIds n (sprintf "FIELD-%s" name)

                    populateExpr argId value acc
                    |> TypeGraph.addFieldEdge n (joinIds argId "$") name)
                graph
                fields

        let addSpread graph = graph

        addFields tg' |> addSpread
    | AST.ChakraApplyExpr (span, app) ->
        let addApplyeeEdge id graph =
            findNodeForVar id n tg'
            |> Option.map (fun node -> TypeGraph.addApplyeeEdge n node graph)
            |> Option.defaultWith (fun _ -> graph)

        match app with
        | AST.ChakraNamedApply (id, pairs) ->
            let addArgs graph =
                List.fold
                    (fun acc (_, (fieldName, expr)) ->
                        let argId = joinIds n (sprintf "FIELD-%s" fieldName)

                        populateExpr argId expr acc
                        |> TypeGraph.addNamedArgEdge n (joinIds argId "$") fieldName)
                    graph
                    pairs

            addApplyeeEdge (applyIdToString id) tg' |> addArgs
        | AST.ChakraApply (id, exprs) ->
            let addArgs graph =
                List.fold
                    (fun acc (i, arg) ->
                        let argId = joinIds n (sprintf "%i" i)

                        populateExpr argId arg acc
                        |> TypeGraph.addArgumentEdge n (joinIds argId "$") i)
                    graph
                    (withIndex exprs)

            addApplyeeEdge (applyIdToString id) tg' |> addArgs
    | AST.ChakraLambda (_, l) ->
        let withParamNodes graph =
            List.fold
                (fun acc (i, p) ->
                    let id = joinIds n p

                    TypeGraph.addParamNode id acc
                    |> TypeGraph.addParameterEdge n id i)
                graph
                (List.mapi (fun i a -> (i, a)) l.Args)

        withParamNodes tg'
        |> populateExprList n l.Body
        |> TypeGraph.addDependentEdge n (exprId n)
    | AST.ChakraPipeExpr _ -> tg'
    | AST.ChakraMatchExpr _ -> tg'
    | AST.ChakraNativeExpr _ -> tg'

and populateExprList bname (AST.ChakraExprList (bs, expr)) tg =
    List.fold (populateBinding bname) tg (List.mapi (fun i b -> b, i) bs)
    |> populateExpr bname expr

and populateBinding bname tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s ->
        let n = joinIds bname s

        TypeGraph.addBindingNode n b tg
        |> populateExprList n b.ExprList
        |> TypeGraph.addDependentEdge n (exprId n)
    | AST.ChakraComplexBindingPattern p -> TypeGraph.addBindingNode (joinIds bname (sprintf "BINDING-%i" i)) b tg
    | AST.ChakraFunctionBindingPattern p ->
        let n = joinIds bname p.Name

        let withParamNodes graph =
            List.fold
                (fun acc (i, p) ->
                    let id = joinIds n p

                    TypeGraph.addParamNode id acc
                    |> TypeGraph.addParameterEdge n id i)
                graph
                (List.mapi (fun i a -> (i, a)) p.Args)

        TypeGraph.addBindingNode n b tg
        |> withParamNodes
        |> populateExprList n b.ExprList
        |> TypeGraph.addDependentEdge n (exprId n)

let populateTopLevelBinding tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s -> TypeGraph.addBindingNode s b tg
    | AST.ChakraComplexBindingPattern p -> tg
    | AST.ChakraFunctionBindingPattern p -> TypeGraph.addBindingNode p.Name b tg

let annotateFunctionLike node args graph =
    let getParams graph =
        List.fold (fun acc (i, _) ->
            acc
            |> Option.bind (fun argTypes ->
                TypeGraph.getParam node i graph
                |> Option.bind (fun a ->
                    TypeGraph.getNodeType a graph
                    |> Option.map (fun ty -> (a, ty)::argTypes)))) (Some []) (withIndex args)
            |> Option.map (fun tys -> List.rev tys)
    let getExprListExpr paramTypes =
        TypeGraph.getExprListExpr node graph
        |> Option.bind (fun e ->
            TypeGraph.getNodeType e graph
            |> Option.map (fun ty ->
                TypeGraph.addAnnotation node (TypeSystem.fn paramTypes ty) graph))

    getParams graph
    |> Option.bind getExprListExpr
    |> Option.defaultValue graph

let annotateBinding graph node (b: AST.ChakraBinding) =
    printfn "Annotating binding %s" node
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern _ ->
        TypeGraph.getExprListExpr node graph
        |> Option.bind (fun e ->
            TypeGraph.getNodeType e graph
            |> Option.map (fun ty -> TypeGraph.addAnnotation node ty graph))
        |> Option.defaultValue graph
    | AST.ChakraFunctionBindingPattern info ->
        printfn "This is a Function binding"
        annotateFunctionLike node info.Args graph
    | _ -> graph

let rec annotateExpr graph node expr =
    match expr with
    | AST.ChakraVar (_, (s, _)) ->
        printfn "Annotating var %s at %s" s node
        TypeGraph.getVarDef node graph
        |> Option.bind (fun def ->
            TypeGraph.getNodeType def graph
            |> Option.map (fun ty -> TypeGraph.addAnnotation node ty graph))
        |> Option.defaultValue graph
        |> inspect "Typegraph"
    | AST.ChakraList (_, { Items = items }) ->
        let getItemsTypes (acc: TypeSystem.Type list) ((i: int), _) =
            TypeGraph.getItem node i graph
            |> Option.bind (fun arg ->
                TypeGraph.getNodeType arg graph
                |> Option.map (fun ty -> ty::acc))
            |> Option.defaultValue acc

        let tys =
            List.fold getItemsTypes [] (withIndex items)
            |> Set

        if tys.Count = 1 then
            TypeGraph.addAnnotation node (TypeSystem.list tys.MaximumElement) graph
        else
            printfn "The list at %s had %i types" node tys.Count
            graph

    | AST.ChakraMap (_, { Pairs = pairs}) ->
        let getItemsTypes ((kacc, vacc): (TypeSystem.Type list * TypeSystem.Type list)) ((i: int), _) =
            TypeGraph.getPair node i graph
            |> Option.bind (fun (k, v) ->
                printfn "Found pair"
                TypeGraph.getNodeType k graph
                |> Option.bind (fun kty ->
                    printfn "Found type for key"
                    TypeGraph.getNodeType v graph
                    |> Option.map (fun vty ->
                        printfn "Found type for value"
                        (kty::kacc, vty::vacc))))
            |> Option.defaultValue (kacc, vacc)

        let (ktys, vtys) =
            List.fold getItemsTypes ([], []) (withIndex pairs)
            |> (fun (ktys, vtys) -> (Set ktys, Set vtys))

        if ktys.Count = 1 && vtys.Count = 1 then
            TypeGraph.addAnnotation node (TypeSystem.map ktys.MaximumElement vtys.MaximumElement) graph
        else
            printfn "The map at %s had %i key types and %i value types " node ktys.Count vtys.Count
            graph

    | AST.ChakraTuple (_, items) ->
        let getItemsTypes (acc: TypeSystem.Type list) ((i: int), _) =
            TypeGraph.getItem node i graph
            |> Option.bind (fun arg ->
                TypeGraph.getNodeType arg graph
                |> Option.map (fun ty -> ty::acc))
            |> Option.defaultValue acc

        let tys = List.fold getItemsTypes [] (withIndex items)
        TypeGraph.addAnnotation node (TypeSystem.tup tys) graph
    | AST.ChakraStruct (_, { Fields = fields }) ->
        let getItemsTypes (acc: (string * TypeSystem.Type) list) ({ Name = name }: AST.ChakraStructField) =
            TypeGraph.getField node name graph
            |> Option.bind (fun arg ->
                TypeGraph.getNodeType arg graph
                |> Option.map (fun ty -> (name, ty)::acc))
            |> Option.defaultValue acc

        let tys = List.fold getItemsTypes [] fields
        TypeGraph.addAnnotation node (TypeSystem.strct (tys, false, None)) graph
    | AST.ChakraApplyExpr (_, app) ->
        let annotateApply argFn graph =
            TypeGraph.getApplyee node graph
            |> Option.bind (fun applyee ->
                TypeGraph.getNodeType applyee graph
                |> Option.map (fun (TypeSystem.FunctionType (args, ret)) ->
                    TypeGraph.addAnnotation node (TypeSystem.FunctionType (args, ret)) graph
                    |> argFn args))
            |> Option.defaultValue graph
        match app with
        | AST.ChakraNamedApply ((id, _), pairs) ->
            annotateApply (fun args g -> g) graph
        | AST.ChakraApply ((id, _), exprs) ->
            // Get the type for applyee and then use it to annotate the arguments
            let annotate (args: (string * TypeSystem.Type) list) (g: TypeGraph.TypeGraph): TypeGraph.TypeGraph =
                List.fold
                    (fun acc (i, _) ->
                        List.tryItem i args
                        |> Option.map (fun (_, ty) ->
                            TypeGraph.addAnnotation (exprId (joinIds node (sprintf "%i" i ))) ty acc)
                        |> Option.defaultValue acc)
                    g
                    (withIndex exprs)

            annotateApply annotate graph
    | AST.ChakraLambda (_, l) ->
        annotateFunctionLike node l.Args graph
    | _ ->
        printfn "Annotating expr\n~~~~~~~~~~\n%s\n~~~~~~~~~~~" (Pretty.pretty 80 (Pretty.showExpr expr))
        graph

let populateImports (imps: AST.ChakraImport list) (envs: Map<string, TypedAST.TCModule>) =
    List.fold (populateImport envs) (TypeGraph.empty) imps
    |> Ok


let populateTopLevelBindings (bs: AST.ChakraBinding list) (tg: TypeGraph.TypeGraph) =
    let bsi = List.mapi (fun i b -> b, i) bs

    bsi
    |> List.fold populateTopLevelBinding tg
    |> (fun tg' -> List.fold (populateBinding "") tg' bsi)
    |> (fun tg ->
        printfn "What did we find? %O" (TypeGraph.findAnnotationTarget tg)
        tg)
    |> Ok


let walkAndAnnotate tg =
    let rec walk graph =
        match TypeGraph.findAnnotationTarget graph with
        | Some node ->
            printfn "Trying to annotate %s" node
            let graph' =
                TypeGraph.getBindingNode node graph
                |> Option.map (annotateBinding graph node)
                |> Option.defaultWith (fun () ->
                    TypeGraph.getExprNode node graph
                    |> Option.map (annotateExpr graph node)
                    |> Option.defaultValue graph)

            
            if graph = graph' then
                inspect "Typegraph" graph
            else
                graph'
                |> inspect "Typegraph"
                |> walk

        | None ->
            printfn "No more annotation targets"
            inspect "Typegraph" graph
    walk tg
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
    .>>. walkAndAnnotate
    .>>. lowerIntoTypedAst m
