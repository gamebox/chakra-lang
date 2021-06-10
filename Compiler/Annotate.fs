module Annotate

open Operators

(********************************************
*
* Utilities
*
*********************************************)

let generics =
    "abcdefghijklmnopqrstuvwxyz".ToCharArray()

let inspect (label: string) (tg: TypeGraph.TypeGraph) =
    // let path =
    //     sprintf "/home/anthony/.%s.md" label

    // let diagram = TypeGraph.toMermaid tg true
    // System.IO.File.WriteAllText(path, diagram)
    tg

let joinIds id ext =
    match id with
    | "" -> ext
    | _ -> sprintf "%s/%s" id ext

let exprId id = sprintf "%s/$" id

let structFieldId id name = sprintf "FIELD-%s" name |> joinIds id

let structAccessId root path =
    List.fold (fun previous segment -> structFieldId previous segment) root path

let popFrame (var: string) =
    var.Split("/")
    |> Seq.toList
    |> List.rev
    |> List.tail
    |> List.rev
    |> String.concat "/"

let rec findNodeForVar var path tg' =
    let id = joinIds path var

    match (path, TypeGraph.hasNode id tg') with
    | (_, true) -> Some id
    | ("", false) -> None
    | (_, false) -> findNodeForVar var (popFrame path) tg'

let applyIdToString (root, path) = structAccessId root path

let withIndex list = List.mapi (fun i x -> (i, x)) list



(********************************************
*
* Populate
*
*********************************************)


let populateStructAccess from root path graph =
    findNodeForVar root from graph
    |> Option.map
        (fun rootNode ->
            // Add a binding node for each segment of the path, adding an edge
            // from the previous to itself
            List.fold
                (fun (previous, acc) segment ->
                    let id = structFieldId previous segment

                    let acc' =
                        if TypeGraph.hasNode id acc then
                            acc
                        else
                            TypeGraph.addAbstractNode id acc
                            |> TypeGraph.addFieldEdge previous id segment

                    (segment, acc'))
                (rootNode, graph)
                path)
    |> Option.map snd
    |> Option.defaultValue graph

let populateStructTypeFields parent fields graph =
    List.fold
        (fun acc (n, ty) ->
            let id = structFieldId parent n

            TypeGraph.addAbstractNode id acc
            |> TypeGraph.addAnnotation id ty)
        graph
        fields

let populateImport (envs: Map<string, TypedAST.TCModule>) tg (imp: AST.ChakraImport) =
    match imp with
    | AST.ChakraLocalImport info -> tg
    | AST.ChakraPackageImport info ->
        let exmap =
            (Map.find info.PackageName envs).ExportMap
            |> Map.toList

        let exType = TypeSystem.strct (exmap, false, None)

        match info.Typ with
        | AST.ChakraSimpleImportBinding s ->
            TypeGraph.addImportNode s tg
            |> TypeGraph.addAnnotation s exType
        | AST.ChakraDestructuredImportBinding p ->
            let fmap = Map exmap

            List.fold
                (fun graph (foreign, local) ->
                    let ty = Map.find foreign fmap

                    TypeGraph.addImportNode local graph
                    |> TypeGraph.addAnnotation local ty
                    |> (fun g ->
                        match ty with
                        | TypeSystem.StructType (fields, _, _) -> populateStructTypeFields local fields g
                        | _ -> g))
                tg
                (Map.toList p)

let rec populateExpr bname (expr: AST.ChakraExpr) tg =
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
    | AST.ChakraVar (_, (var, Some path)) ->
        match findNodeForVar var n tg' with
        | Some node ->
            populateStructAccess node var path tg'
            |> TypeGraph.addDependentEdge n (structAccessId node path)
        | None -> tg'
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
        let addApplyeeEdge (root, path) graph =
            // printfn "Looking for apply"
            findNodeForVar root n tg'
            |> Option.map (fun node -> TypeGraph.addApplyeeEdge n (structAccessId node path) graph)
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

            addApplyeeEdge id tg' |> addArgs
        | AST.ChakraApply (id, exprs) ->
            let addArgs graph =
                List.fold
                    (fun acc (i, arg) ->
                        let argId = joinIds n (sprintf "%i" i)

                        populateExpr argId arg acc
                        |> TypeGraph.addArgumentEdge n (joinIds argId "$") i)
                    graph
                    (withIndex exprs)

            addApplyeeEdge id tg' |> addArgs
    | AST.ChakraLambda (_, l) ->
        let withParamNodes graph =
            List.fold
                (fun acc (i, p) ->
                    let id = joinIds n p

                    let ty =
                        generics.[i].ToString() |> TypeSystem.gen

                    TypeGraph.addParamNode id acc
                    |> TypeGraph.addParameterEdge n id i
                    |> TypeGraph.addAnnotation id ty)
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

                    let ty =
                        generics.[i].ToString() |> TypeSystem.gen


                    // printfn "Creating param %s with type %O" id ty

                    TypeGraph.addParamNode id acc
                    |> TypeGraph.addParameterEdge n id i)
                graph
                (withIndex p.Args)

        TypeGraph.addBindingNode n b tg
        |> withParamNodes
        |> populateExprList n b.ExprList
        |> TypeGraph.addDependentEdge n (exprId n)

let populateTopLevelBinding tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s -> TypeGraph.addBindingNode s b tg
    | AST.ChakraComplexBindingPattern p -> tg
    | AST.ChakraFunctionBindingPattern p -> TypeGraph.addBindingNode p.Name b tg

let populateImports (imps: AST.ChakraImport list) (envs: Map<string, TypedAST.TCModule>) =
    List.fold (populateImport envs) (TypeGraph.empty) imps
    |> Ok

let populateTopLevelBindings (bs: AST.ChakraBinding list) (tg: TypeGraph.TypeGraph) =
    let bsi = List.mapi (fun i b -> b, i) bs

    bsi
    |> List.fold populateTopLevelBinding tg
    |> (fun tg' -> List.fold (populateBinding "") tg' bsi)
    |> Ok



(********************************************
*
* Annotate
*
*********************************************)

let annotateStructAccess from root path ty graph =
    findNodeForVar root from graph
    |> Option.map
        (fun rootNode ->
            let id = structAccessId rootNode path
            TypeGraph.addAnnotation id ty graph)
    |> Option.defaultValue graph

let annotateFunctionLike node args graph =
    let getParams graph =
        List.fold
            (fun acc (i, _) ->
                acc
                |> Option.bind
                    (fun argTypes ->
                        TypeGraph.getParam node i graph
                        |> Option.bind
                            (fun a ->
                                TypeGraph.getNodeType a graph
                                |> Option.map (fun ty -> (a, ty) :: argTypes))))
            (Some [])
            (withIndex args)
        |> Option.map (fun tys -> List.rev tys)

    let getExprListExpr paramTypes =
        TypeGraph.getExprListExpr node graph
        |> Option.bind
            (fun e ->
                TypeGraph.getNodeType e graph
                |> Option.map (fun ty -> TypeGraph.addAnnotation node (TypeSystem.fn paramTypes ty) graph))

    getParams graph
    |> Option.bind getExprListExpr
    |> Option.defaultValue graph

let annotateBinding graph node (b: AST.ChakraBinding) =
    // printfn "Annotating binding %s" node

    match b.Pattern with
    | AST.ChakraSimpleBindingPattern _ ->
        TypeGraph.getExprListExpr node graph
        |> Option.bind
            (fun e ->
                TypeGraph.getNodeType e graph
                |> Option.map (fun ty -> TypeGraph.addAnnotation node ty graph))
        |> Option.defaultValue graph
    | AST.ChakraFunctionBindingPattern info -> annotateFunctionLike node info.Args graph
    | _ -> graph

let pushTypeToDependentEdge source ty graph =
    // printfn "Pushing type %O from '%s' to unannotated dependent node" ty source
    TypeGraph.getVarDef source graph
    |> Option.bind
        (fun def ->
            TypeGraph.getNodeType def graph
            |> Option.map (fun _ -> graph)
            |> Option.orElseWith (fun _ -> TypeGraph.addAnnotation def ty graph |> Some))

let rec annotateExpr graph node expr =
    match expr with
    | AST.ChakraVar (_, (s, path)) ->
        // printfn "Annotating var %s at %s" s node

        TypeGraph.getVarDef node graph
        |> Option.bind
            (fun def ->
                let id =
                    match path with
                    | Some p -> structAccessId def p
                    | None -> def

                TypeGraph.getNodeType id graph
                |> Option.map (fun ty -> TypeGraph.addAnnotation node ty graph))
        |> Option.defaultValue graph
    | AST.ChakraList (_, { Items = items }) ->
        let getItemsTypes (acc: TypeSystem.Type list) ((i: int), _) =
            TypeGraph.getItem node i graph
            |> Option.bind
                (fun arg ->
                    TypeGraph.getNodeType arg graph
                    |> Option.map (fun ty -> ty :: acc))
            |> Option.defaultValue acc

        let tys =
            List.fold getItemsTypes [] (withIndex items)
            |> Set

        if tys.Count = 1 then
            TypeGraph.addAnnotation node (TypeSystem.list tys.MaximumElement) graph
        else
            // printfn "The list at %s had %i types" node tys.Count
            graph

    | AST.ChakraMap (_, { Pairs = pairs }) ->
        let getItemsTypes ((kacc, vacc): (TypeSystem.Type list * TypeSystem.Type list)) ((i: int), _) =
            TypeGraph.getPair node i graph
            |> Option.bind
                (fun (k, v) ->
                    TypeGraph.getNodeType k graph
                    |> Option.bind
                        (fun kty ->
                            TypeGraph.getNodeType v graph
                            |> Option.map (fun vty -> (kty :: kacc, vty :: vacc))))
            |> Option.defaultValue (kacc, vacc)

        let (ktys, vtys) =
            List.fold getItemsTypes ([], []) (withIndex pairs)
            |> (fun (ktys, vtys) -> (Set ktys, Set vtys))

        if ktys.Count = 1 && vtys.Count = 1 then
            TypeGraph.addAnnotation node (TypeSystem.map ktys.MaximumElement vtys.MaximumElement) graph
        else
            // printfn "The map at %s had %i key types and %i value types " node ktys.Count vtys.Count
            graph

    | AST.ChakraTuple (_, items) ->
        let getItemsTypes (acc: TypeSystem.Type list) ((i: int), _) =
            TypeGraph.getItem node i graph
            |> Option.bind
                (fun arg ->
                    TypeGraph.getNodeType arg graph
                    |> Option.map (fun ty -> ty :: acc))
            |> Option.defaultValue acc

        let tys =
            List.fold getItemsTypes [] (withIndex items)

        TypeGraph.addAnnotation node (TypeSystem.tup tys) graph
    | AST.ChakraStruct (_, { Fields = fields }) ->
        let getItemsTypes (acc: (string * TypeSystem.Type) list) ({ Name = name }: AST.ChakraStructField) =
            TypeGraph.getField node name graph
            |> Option.bind
                (fun arg ->
                    TypeGraph.getNodeType arg graph
                    |> Option.map (fun ty -> (name, ty) :: acc))
            |> Option.defaultValue acc

        let tys = List.fold getItemsTypes [] fields
        TypeGraph.addAnnotation node (TypeSystem.strct (tys, false, None)) graph
    | AST.ChakraApplyExpr (_, app) ->
        let annotateApply annotateArgsFn fetchArgsFn graph =
            TypeGraph.getApplyee node graph
            |> Option.bind
                (fun applyee ->
                    TypeGraph.getNodeType applyee graph
                    |> Option.map
                        (fun ty ->
                            match ty with
                            | TypeSystem.FunctionType (args, ret) ->
                                TypeGraph.addAnnotation node ret graph
                                |> annotateArgsFn args
                            | TypeSystem.GenericType g ->
                                // fetch args
                                // annotate applyee as function with
                                // annotate node as generic type ?a
                                graph
                            | _ -> graph))
            |> Option.defaultValue graph

        match app with
        | AST.ChakraNamedApply ((id, _), pairs) ->
            let annotate (args: (string * TypeSystem.Type) list) g =
                List.fold
                    (fun acc (i, _) ->
                        List.tryItem i args
                        |> Option.bind
                            (fun (_, ty) ->
                                let id = (exprId (joinIds node (sprintf "%i" i)))

                                TypeGraph.addAnnotation id ty acc
                                |> pushTypeToDependentEdge id ty)
                        |> Option.defaultValue acc)
                    g
                    (withIndex pairs)

            let fetch g = []

            annotateApply annotate fetch graph
        | AST.ChakraApply ((id, _), exprs) ->
            // Get the type for applyee and then use it to annotate the arguments
            let annotate (args: (string * TypeSystem.Type) list) (g: TypeGraph.TypeGraph) : TypeGraph.TypeGraph =
                List.fold
                    (fun acc (i, _) ->
                        List.tryItem i args
                        |> Option.bind
                            (fun (_, ty) ->
                                let id = (exprId (joinIds node (sprintf "%i" i)))

                                TypeGraph.addAnnotation id ty acc
                                |> pushTypeToDependentEdge id ty)
                        |> Option.defaultValue acc)
                    g
                    (withIndex exprs)

            let fetch (g: TypeGraph.TypeGraph) = []

            annotateApply annotate fetch graph
    | AST.ChakraLambda (_, l) -> annotateFunctionLike node l.Args graph
    | _ ->
        // printfn "Annotating expr\n~~~~~~~~~~\n%s\n~~~~~~~~~~~" (Pretty.pretty 80 (Pretty.showExpr expr))
        graph

let annotateParam graph param =
    // printfn "Annotating param %s" param
    None

let annotateLeaf node graph =
    // printfn "Annotating leaf '%s'" node
    match TypeGraph.getFields node graph with
    | [] -> graph
    | fs ->
        let fields =
            List.map
                (fun (fieldName, node) ->
                    let ty =
                        TypeGraph.getNodeType node graph |> Option.get

                    (fieldName, ty))
                fs

        TypeGraph.addAnnotation node (TypeSystem.strct (fields, false, None)) graph

let walkAndAnnotate tg =
    let rec walk graph =
        match TypeGraph.findAnnotationTarget graph with
        | Some node ->
            // printfn "Trying to annotate %s" node

            let graph' =
                TypeGraph.getBindingNode node graph
                |> Option.map (annotateBinding graph node)
                |> Option.orElseWith
                    (fun () ->
                        TypeGraph.getExprNode node graph
                        |> Option.map (annotateExpr graph node))
                |> Option.defaultValue graph


            if graph = graph' then
                inspect "Typegraph" graph
            else
                graph' |> inspect "Typegraph" |> walk

        | None ->
            // printfn "No more annotation targets - looking for unannotated leaves"
            match TypeGraph.findAnnotationLeaf graph with
            | Some leaf -> annotateLeaf leaf graph |> walk
            | None -> inspect "Typegraph" graph

    walk tg |> Ok



(********************************************
*
* Lower
*
*********************************************)

let rec lowerExpr i (expr: AST.ChakraExpr) graph : TypedAST.TCExpr =
    let id = exprId i

    let ty () =
        TypeGraph.getNodeType id graph |> Option.get

    printfn "Expr ID is '%s' and the expr is:\n%O" id expr

    match expr with
    | AST.ChakraVar (loc, id) -> TypedAST.TCVar(id, (ty ()))
    | AST.ChakraNumber (_, d) -> TypedAST.TCNumber d
    | AST.ChakraSymbol (_, s) -> TypedAST.TCSymbol s
    | AST.ChakraString (_, s) -> TypedAST.TCString s
    | AST.ChakraTuple (_, exprs) ->
        List.map
            (fun (i, expr) ->
                let argId = joinIds id (sprintf "ITEM-%i" i)
                lowerExpr argId expr graph)
            (withIndex exprs)
        |> TypedAST.TCTuple
    | AST.ChakraStruct (_, _) -> raise (System.Exception())
    | AST.ChakraList (_, _) -> raise (System.Exception())
    | AST.ChakraMap (_, _) -> raise (System.Exception())
    | AST.ChakraLambda (_, _) -> raise (System.Exception())
    | AST.ChakraApplyExpr (loc, app) ->
        match app with
        | AST.ChakraApply (identifier, args) ->
            let typedArgs =
                List.map
                    (fun (i, expr) ->
                        let argId = joinIds id (sprintf "%i" i)
                        printfn "Going to lower apply arg '%s'" argId
                        lowerExpr argId expr graph)
                    (withIndex args)

            TypedAST.TCApplyExpr(loc, ((ty ())), TypedAST.TCApply(identifier, typedArgs))

        | AST.ChakraNamedApply (identifier, argPairs) ->
            TypedAST.TCApplyExpr(loc, ((ty ())), TypedAST.TCApply(identifier, []))

    | _ -> raise (System.Exception())

and lowerExprList id (AST.ChakraExprList (bs, expr)) graph : TypedAST.TCExprList =
    TypedAST.TCExprList(lowerBindings id bs graph, lowerExpr id expr graph)

and lowerBinding id (b: AST.ChakraBinding) graph : TypedAST.TCBinding =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s ->
        let id = joinIds id s

        let ty =
            TypeGraph.getNodeType id graph |> Option.get

        let el = lowerExprList id b.ExprList graph
        TypedAST.tcBinding b (TypedAST.TCSimpleBindingPattern s) el ty

    | AST.ChakraFunctionBindingPattern patt ->
        printfn "Lowering %s" patt.Name
        let id = joinIds id patt.Name

        let ty =
            TypeGraph.getNodeType id graph |> Option.get

        let el = lowerExprList id b.ExprList graph

        let args =
            List.map
                (fun arg ->
                    let id = joinIds id arg
                    printfn "Arg is %s" arg

                    let ty =
                        TypeGraph.getNodeType id graph |> Option.get

                    (arg, ty))
                patt.Args

        let p =
            (TypedAST.TCFunctionBindingPattern { Name = patt.Name; TypedArgs = args })

        TypedAST.tcBinding b p el ty

    | AST.ChakraComplexBindingPattern patt -> raise (System.Exception())

and lowerBindings id bindings graph : TypedAST.TCBinding list =
    List.map (fun b -> lowerBinding id b graph) bindings


let lowerIntoTypedAst (m: AST.ChakraModule) (tg: TypeGraph.TypeGraph) =
    let optToResult n opt =
        match opt with
        | Some v -> Ok v
        | None -> Error(TypeError.UntypedError n)

    let collectExportBindingTypes acc (b: AST.ChakraBinding) =
        match acc with
        | Ok exmap ->
            match b.Pattern with
            | AST.ChakraSimpleBindingPattern s ->
                TypeGraph.getNodeType s tg
                |> Option.map (fun ty -> Map.add s ty exmap)
                |> optToResult s
            | AST.ChakraFunctionBindingPattern patt ->
                TypeGraph.getNodeType patt.Name tg
                |> Option.map (fun ty -> Map.add patt.Name ty exmap)
                |> optToResult patt.Name
            | AST.ChakraComplexBindingPattern patt -> acc
        | _ -> acc

    let createTypedExports bindings =
        List.fold collectExportBindingTypes (Ok Map.empty) bindings


    createTypedExports m.Bindings
    |> Result.map (fun es -> (es, (lowerBindings "" m.Bindings tg)))
    |> Result.map (fun (exports, bindings) -> TypedAST.tcModule m exports bindings)





(********************************************
*
* Entry Point
*
*********************************************)

/// This function attempts to take the Untyped AST of a module, as well
/// as the resolved export types of all modules it is dependent on and
/// and perform the following operations:
/// 1. Transform the syntax tree into a graph
/// 2. Work from the leaf nodes, down, annotating every node in the graph
///    with the most specific type
/// 3. Transform the fully annotated graph into a Typed AST for the module
let annotate moduleName (m: AST.ChakraModule) (envs: Map<string, TypedAST.TCModule>) =
    populateImports m.Imports envs
    .>>. populateTopLevelBindings m.Bindings
    .>>. walkAndAnnotate
    .>>. lowerIntoTypedAst m
