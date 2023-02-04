#load "HM.fs"

open HM

let (@^) e1 e2 = Comb(e1, e2)

let mkFunc name args = Tyapp(name,args)
let mkNullary name = mkFunc name []
let mkUnary name arg = mkFunc name [arg]
let mkBinary name arg1 arg2 = mkFunc name [arg1; arg2]
// let mkTernary name arg1 arg2 arg3 = mkFunc name [arg1; arg2; arg3]

let pairt t1 t2 = mkBinary "pair" t1 t2
let listt t = mkUnary "list" t
// let boolt = mkNullary "bool"

(* Type variables *)

let alpha = Tyvar "a"
let beta = Tyvar "b"
let alpha' = Tyvar "a'"
let beta' = Tyvar "b'"

(* Type-schemes *)

let stringt = mkNullary "String"

exception Fail of string

let rec mk_tyscheme terms t = 
   match terms with
   | [] -> Type t
   | ((Tyvar v)::vs) -> Forall (v, mk_tyscheme vs t)
   | _ -> raise (Fail "mk_tyscheme: Invalid type-scheme.")

let labs var e =
   match var with
   | (Var v) -> Abs(v,e)
   | _ -> raise (Fail "labs: Invalid argument")

(* Now we can construct type-schemes. For example here is a
   polymorphic function taking pairs of functions and two lists to a
   list of pairs: *)

let dmapts = mk_tyscheme [alpha; alpha'; beta; beta']
                  (pairt (alpha ^--> alpha') (beta ^--> beta')
                      ^--> listt alpha ^--> listt beta
                      ^--> pairt (listt alpha') (listt beta'));
printfn "%s" (ppts dmapts)
printfn "%s" (ppts (mk_tyscheme [alpha] (alpha ^--> listt alpha)))

let singletonarrowts = (alpha ^--> listt alpha)
let singletonfuncts = (mkFunc "fn" [alpha; listt alpha])

printfn "singletonarrowts: %s" (ppterm singletonarrowts)
printfn "singletonfuncts: %s" (ppterm singletonfuncts)
let G' = [
    "a", Type stringt
    "b", Forall ("a", Type (alpha ^--> listt alpha))
]

let (_, (subs, tau)) =
    try 
        W 0 G' (labs (Var "a") (Var "b" @^ Var "a"))
    with
        | Assum s ->
            printfn "Assum error: %s" s
            raise (System.Exception "")
        | Unify s ->
            printfn "Unify error: %s" s
            raise (System.Exception "")

printfn "[Resulting term]: %s" (ppterm tau)
printfn "[Resulting subs]: %s" (ppsubs subs)