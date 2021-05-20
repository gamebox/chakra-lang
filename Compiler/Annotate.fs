module Annotate

open AST
open TypeSystem

type ASTNode =
    | BindingNode of ChakraBinding
    | ExprNode of ChakraExpr

type Relation =
    | DependsOn
    | Argument of int
    | Parameter of int
    | Field of string
    | PairValue of int
    | PairKey of int
    | Item of int
    | Rest
    | Applyee

type TypeGraph =
    { Nodes : Map<string, ASTNode>
      UpRelations : Map<string, (Relation * string) list>
      DownRelations : Map<string, (Relation * string) list>
      Annotations: Map<string, Type>
      Types : Set<Type> }

/// This function attempts to take the Untyped AST of a module, as well
/// as the resolved export types of all modules it is dependent on and
/// and perform the following operations:
/// 1. Transform the syntax tree into a graph
/// 2. Work from the leaf nodes, down, annotating every node in the graph
///    with the most specific type
/// 3. Transform the fully annotated graph into a Typed AST for the module
let annotate moduleName m envs =
    TypedAST.tcModule m (Map.empty) []
    |> Ok