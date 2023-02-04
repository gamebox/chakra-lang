// module Unify2
#load "TypeSystem.fs"

type Typescheme =
    | Forall of string * Typescheme
    | Type of TypeSystem.Type

// let rec subs ss term =
//     match (ss, term) with
//     | ([], t) -> t
//     | (((t1,v1)::ss), (Tyvar name)) ->
//         if name = v1 then t1 else subs ss term
//     | (_, (term as Tyapp(_,[]))) ->
//         term
//     | (l, (Tyapp(name,args))) ->
//         let rec arglist r rs = 
//             match (r, rs) with
//             | (_, []) -> List.rev r
//             | (r, (h::t)) ->
//                 arglist ((subs l h)::r) t
//         Tyapp(name, arglist [] args)

// let rec compose ss s1 =
//     match (ss, s1) with
//     | ([], _) -> s1
//     | ((s::ss), s1) ->
//         let rec iter r s rs =
//             match rs with
//             | [] -> List.rev r
//             | ((t1,v1)::ss) ->
//                 iter (((subs [s] t1),v1)::r) s ss
//         compose ss (s::(iter [] s s1))

// exception Unify of string

// let unify t1 t2 =
//     let rec iter r t1 t2 =
//         let rec occurs v term =
//             match term with
//             | (Tyapp(name,[])) -> false
//             | (Tyapp(name,((Tyvar vn)::t))) ->
//                 if vn=v then true else occurs v (Tyapp(name,t))
//             | (Tyapp(name,(s::t))) ->
//                 occurs v s || occurs v (Tyapp(name,t))
//             | (Tyvar vn) -> vn = v

//         let rec unifyArgs r ls rs =
//             match (ls, rs) with
//             | ([], []) -> List.rev r
//             | ([], _) -> raise (Unify "Arity")
//             | (_, []) -> raise (Unify "Arity")
//             | ((t1::t1s), (t2::t2s)) ->
//                 unifyArgs (compose (iter [] (subs r t1) (subs r t2)) r) t1s t2s

//         match (t1,t2) with
//         | (Tyvar v1,Tyvar v2) -> if (v1 = v2) then [] else ((t1, v2)::r)
//         | (Tyvar v,Tyapp(_,[])) -> ((t2, v)::r)
//         | (Tyapp(_,[]),Tyvar v) -> ((t1, v)::r)
//         | (Tyvar v,Tyapp _) ->
//             if occurs v t2 then raise (Unify "Occurs") else ((t2, v)::r)
//         | (Tyapp _,Tyvar v) ->
//             if occurs v t1 then raise (Unify "Occurs") else ((t1, v)::r)
//         | (Tyapp(name1,args1),Tyapp(name2,args2)) ->
//             if (name1=name2) then
//                 unifyArgs r args1 args2
//             else
//                 raise (Unify "Const")

//     iter [] t1 t2

// let x = Tyvar "x"
// let y = Tyvar "y"
// let z = Tyvar "z"
// let apply s l = Tyapp(s,l)
// let a = apply "a" []
// let j (x, y, z) = apply "j" [x; y; z]
// let f (x, y) = apply "f" [x; y]
// let t1 = j (x,y,z)
// let t2 = j (f (y,y), f (z,z), f (a,a));
// let U = unify t1 t2;


// let rec ppterm term = 
//     match term with
//     | (Tyvar name) -> name
//     | (Tyapp(name,[])) -> name
//     | (Tyapp(name,args)) ->
//         let rec arglist r rs = 
//             match rs with
//             | [] -> r
//             | (h::t) ->
//                 arglist (r + (ppterm h) + (if t=[] then "" else ",")) t
//         name + (arglist "(" args) + ")"


// let ppsubs s =
//     let rec iter r rs =
//         match rs with
//         | [] -> r + "]"
//         | ((term,var)::t) ->
//             iter (r + (ppterm term) + "/" + var + (if t=[] then "" else ",")) t
//     iter "[" s

// printfn "%s" (ppsubs U)
