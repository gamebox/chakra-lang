module Annotate

let (.>>.) res fn = Result.bind fn res

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

let addBinding tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s -> TypeGraph.addBindingNode s b tg
    | AST.ChakraComplexBindingPattern p -> tg
    | AST.ChakraFunctionBindingPattern p -> TypeGraph.addBindingNode p.Name b tg

let populateTopLevelBindings (bs: AST.ChakraBinding list) (tg: TypeGraph.TypeGraph) =
    printf "TYPEGRAPH\n----$$$----\n%O\n----$$$----" tg

    List.mapi (fun i b -> b, i) bs
    |> List.fold addBinding tg
    |> Ok


let lowerIntoTypedAst m tg = Ok(TypedAST.tcModule m (Map.empty) [])

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
