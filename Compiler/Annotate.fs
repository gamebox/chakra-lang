module Annotate

let (.>>.) res fn = Result.bind fn res

let populateImports imps envs =
    Ok TypeGraph.empty

let addBinding tg ((b: AST.ChakraBinding), i) =
    match b.Pattern with
    | AST.ChakraSimpleBindingPattern s ->
        TypeGraph.addBindingNode s b tg
    | AST.ChakraComplexBindingPattern p ->
        tg
    | AST.ChakraFunctionBindingPattern p ->
        TypeGraph.addBindingNode p.Name b tg

let populateTopLevelBindings (bs: AST.ChakraBinding list) (tg: TypeGraph.TypeGraph) =
    List.mapi (fun i b -> b, i) bs
    |> List.fold addBinding tg
    |> Ok


let lowerIntoTypedAst m tg =
    Ok (TypedAST.tcModule m (Map.empty) [])

/// This function attempts to take the Untyped AST of a module, as well
/// as the resolved export types of all modules it is dependent on and
/// and perform the following operations:
/// 1. Transform the syntax tree into a graph
/// 2. Work from the leaf nodes, down, annotating every node in the graph
///    with the most specific type
/// 3. Transform the fully annotated graph into a Typed AST for the module
let annotate moduleName (m: AST.ChakraModule) envs =
    populateImports m.Imports envs
    .>>. populateTopLevelBindings m.Bindings
    .>>. lowerIntoTypedAst m