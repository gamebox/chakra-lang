module TypeGraph

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

let empty =
    { Nodes = Map.empty
      UpRelations = Map.empty
      DownRelations = Map.empty
      Annotations = Map.empty
      Types = Set.empty }

let addBindingNode id b tg =
    { tg with
        Nodes = Map.add id (BindingNode b) tg.Nodes
        UpRelations = Map.add id [] tg.UpRelations
        DownRelations = Map.add id [] tg.DownRelations }

let addExprNode id e tg =
    { tg with
        Nodes = Map.add id (ExprNode e) tg.Nodes
        UpRelations = Map.add id [] tg.UpRelations
        DownRelations = Map.add id [] tg.DownRelations }

let private addEdge from to' edge tg=
    if Map.containsKey from tg.Nodes && Map.containsKey to' tg.Nodes then
        let fromRelations = (edge, to')::Map.find from tg.UpRelations
        let toRelations = (edge, from)::Map.find to' tg.DownRelations
        { tg with
            UpRelations = Map.add from fromRelations tg.UpRelations
            DownRelations = Map.add to' toRelations tg.DownRelations }
    else
        tg

let addDependentNode from to' tg = 
    addEdge from to' (DependsOn) tg

let addArgumentNode from to' n tg =
    addEdge from to' (Argument n) tg

let addParameterNode from to' n tg =
    addEdge from to' (Parameter n) tg

let addFieldNode from to' s tg =
    addEdge from to' (Field s) tg

let addPairValueNode from to' n tg =
    addEdge from to' (PairValue n) tg

let addPairKeyNode from to' n tg =
    addEdge from to' (PairKey n) tg

let addItemNode from to' n tg = 
    addEdge from to' (Item n) tg

let addRestNode from to' tg =
    addEdge from to' (Rest) tg

let addApplyeeNode from to' tg =
    addEdge from to' (Applyee) tg