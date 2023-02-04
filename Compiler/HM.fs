// #nowarn "62"
(* Hindley-Milner Type Inference. http://ian-grant.net/hm *)

[<CompilerMessage("This module is for ML compatibility. \
    This message can be disabled using '--nowarn:62' or '#nowarn \"62\"'.",
                  62,
                  IsHidden = true)>]
module HM

type Term =
    | Tyvar of string // Type variable
    | Tyapp of string * (Term list) // Concrete type with consisting on one or more type terms

// "abcdefghijklmnopqrstuvwxyz"

(* StandardML Compatability *)
let inline ord c = int c - int '0'

let inline chr (ascii: int) =
    System.Convert.ToChar(ascii + (int '0'))

let inline explode (str: string) = str.ToCharArray() |> Array.toList

let rec subs s term =
    match (s, term) with
    | ([], _) ->
      term
    | (((t1, v1) :: ss), (Tyvar name)) ->
      if name = v1 then t1 else subs ss term
    | (_, Tyapp (name, [])) ->
      term
    | (l, Tyapp (name, args)) ->
      let rec arglist r ts =
         match ts with
         | [] -> List.rev r
         | (h :: t) -> arglist ((subs l h) :: r) t

      Tyapp(name, arglist [] args)

let rec compose ss s1 =
    match (ss, s1) with
    | ([], _) -> s1
    | ((s :: ss), s1) ->
        let rec iter r s rs =
            match rs with
            | [] -> List.rev r
            | ((t1, v1) :: ss) -> iter (((subs [ s ] t1), v1) :: r) s ss

        compose ss (s :: (iter [] s s1))

exception Unify of string

let unify t1 t2 =
    let rec iter r t1 t2 =
        let rec occurs v term =
            match term with
            | (Tyapp (name, [])) -> false
            | (Tyapp (name, ((Tyvar vn) :: t))) ->
                if vn = v then
                    true
                else
                    occurs v (Tyapp(name, t))
            | (Tyapp (name, (s :: t))) -> occurs v s || occurs v (Tyapp(name, t))
            | (Tyvar vn) -> vn = v

        let rec unifyArgs r ls rs =
            match (ls, rs) with
            | ([], []) -> List.rev r
            | ([], _) -> raise (Unify "Arity")
            | (_, []) -> raise (Unify "Arity")
            | ((t1 :: t1s), (t2 :: t2s)) -> unifyArgs (compose (iter [] (subs r t1) (subs r t2)) r) t1s t2s

        match (t1, t2) with
        | (Tyvar v1, Tyvar v2) ->
            if (v1 = v2) then
                []
            else
                ((t1, v2) :: r)
        | (Tyvar v, Tyapp (_, [])) -> ((t2, v) :: r)
        | (Tyapp (_, []), Tyvar v) -> ((t1, v) :: r)
        | (Tyvar v, Tyapp _) ->
            if occurs v t2 then
                raise (Unify "Occurs")
            else
                ((t2, v) :: r)
        | (Tyapp _, Tyvar v) ->
            if occurs v t1 then
                raise (Unify "Occurs")
            else
                ((t1, v) :: r)
        | (Tyapp (name1, args1), Tyapp (name2, args2)) ->
            if (name1 = name2) then
                unifyArgs r args1 args2
            else
                raise (Unify "Const")

    iter [] t1 t2

type Typescheme =
    | Forall of string * Typescheme // A type with some type variable to account for
    | Type of Term // A type with no type variable to account for

/// A rename of List.contains
let mem = List.contains

/// Find all of the Free and Bound type variables(Tyvars) in a term
let rec fbtyvars free bound term =
    match term with
    | Tyvar v ->
        if (mem v bound) then
            (free, bound)
        else
            (v :: free, bound)
    | Tyapp (name, args) ->
        let rec iter r terms =
            match terms with
            | [] -> r
            | (t :: ts) ->
                let (f, b) = fbtyvars r bound t
                iter f ts

        let fvs = iter free args
        (fvs, bound)

/// Find all of the Free and Bound type variables in a typescheme
let rec fbtsvs free bound sigma =
    match sigma with
    | Forall (var, sigma') -> fbtsvs free (var :: bound) sigma'
    | Type term -> fbtyvars free bound term

/// Pulls all type variables out of a Term
let tyvars term =
    let (f, b) = fbtyvars [] [] term
    f @ b

/// Calculates a numeric value for a type variable
let varno var =
    match var with
    | "" -> ~~~ 1
    | _ ->
        let vl = explode var
        let letter = (ord (List.head vl)) - (ord 'a')

        let rec primes r vs =
            match vs with
            | [] -> r
            | (h :: t) ->
                if h = '\039' then
                    primes (r + 26) t
                else
                    ~~~ 1

        if letter >= 0 && letter <= 25 then
            primes letter (List.tail vl)
        else
            ~~~ 1


let lastusedtsvar nv sigma =
    let vars =
        let (f, b) = fbtsvs [] [] sigma
        f @ b

    let rec iter r ts =
        match ts with
        | [] -> r
        | (h :: t) ->
            let vn = varno h
            if vn > r then iter vn t else iter r t

    (iter nv vars)

let lastfreetsvar nv sigma =
    let (vars, _) = fbtsvs [] [] sigma

    let rec iter r ts =
        match ts with
        | [] -> r
        | (h :: t) ->
            let vn = varno h
            if vn > r then iter vn t else iter r t

    (iter nv vars)

/// Returns the next variable name in the sequence.  Lowercase a-z will be used in order,
/// when they have all been used a prime will be added to each letter, and so on, adding a
/// prime each time the letter sequence has been exhausted
let newvar v =
    let nv = v + 1

    let rec prime v n =
        match n with
        | 0 -> v
        | _ -> prime $"{v}'" (n - 1)

    let primes = nv / 26
    let var = $"{(chr ((ord 'a') + (nv % 26)))}"
    (nv, prime var primes)

let rec tssubs nv substitutions sigma =
    match substitutions with
    | [] -> (nv, sigma)
    | ((sub as (t, v)) :: restOfSubs) ->
        let (fvs, _) = fbtyvars [] [] t

        let rec iter nv rnss tvp ts =
            match (tvp, ts) with
            | (t, v), Forall (sv, sts) ->
                if (sv = v) then
                    (nv, ts)
                else if mem sv fvs then
                    let (nv, newv) = newvar nv
                    let (nv, sigma') = iter nv (compose [ (Tyvar newv, sv) ] rnss) tvp sts

                    (nv, Forall(newv, sigma'))
                else
                    let (nv, sigma') = iter nv rnss tvp sts

                    (nv, Forall(sv, sigma'))
            | (_, (Type term)) -> (nv, (Type(subs [ tvp ] (subs rnss term))))

        let (nv, sigma') = iter nv [] sub sigma

        tssubs nv restOfSubs sigma'


exception Assum of string

/// Looks through the type environment for a var `p` returning it's typescheme if found
/// and raises an Assum exception if not.
let assq p l =
   printfn "Looking for var %s" p
   let rec iter assums =
      match assums with
      | [] -> raise (Assum p)
      | (k, v) :: xs -> if (k = p) then v else iter xs

   iter l

/// Returns a list of all Free variables in the type environment (Gamma)
let fassumvars gamma =
    let rec iter f gamma =
        match gamma with
        | [] -> f
        | ((_, ts) :: gamma') ->
            let (fvs, _) = fbtsvs f [] ts
            iter (f @ fvs) gamma'

    iter [] gamma


/// Returns a list of all variables - free and bound - in the type environment (Gamma)
let assumvars gamma =
    let rec iter f gamma =
        match gamma with
        | [] -> f
        | ((_, ts) :: gamma') ->
            let (fvs, bvs) = fbtsvs f [] ts
            iter (f @ fvs @ bvs) gamma'

    iter [] gamma

let lastfreeassumvar gamma =
    let rec iter r gamma =
        match gamma with
        | [] -> r
        | ((_, sigma) :: gamma') -> iter (lastfreetsvar r sigma) gamma'

    iter ~~~ 1 gamma


let assumsubs nv S gamma =
    let rec iter r nv S gs =
        match gs with
        | [] -> (nv, List.rev r)
        | ((v, sigma) :: gamma') ->
            let (nv', sigma') = tssubs nv S sigma

            iter ((v, sigma') :: r) nv' S gamma'

    iter [] nv S gamma


let tsclosure gamma tau =
   let favs = fassumvars gamma
   let (ftvs, _) = fbtyvars [] [] tau

   let rec iter bvs vss =
      match vss with
      | [] -> Type tau
      | (v :: vs) ->
         if (mem v favs) || (mem v bvs) then
               iter bvs vs
         else
               Forall(v, iter (v :: bvs) vs)

   iter [] ftvs

type Exp =
   /// A Variable with a name
   | Var of string
   // Function application?
   | Comb of Exp * Exp
   /// A function declaration
   | Abs of string * Exp
   /// A `let` expression of the form `let x = e in x`
   | Let of (string * Exp) * Exp

// infixr -->
let (^-->) tau1 tau2 = Tyapp("%f", [ tau1; tau2 ])

let rec ppterm term =
    match term with
    | (Tyvar name) -> name
    | (Tyapp (name, [])) -> name
    | (Tyapp (name, args)) ->
      let rec arglist r rs =
         match rs with
         | [] -> r
         | (h :: t) ->
            arglist (r + (ppterm h) + (if t = [] then "" else ",")) t

      $"""{name}{arglist "(" args})"""


let ppsubs s =
    let rec iter r rs =
        match rs with
        | [] -> r + "]"
        | ((term, var) :: t) ->
            let ending = if t = [] then "" else ","
            iter $"{r}{ppterm term}/{var}{ending}" t

    iter "[" s

let rec W nv gamma exp =
   match exp with
   | Var v ->
      let rec tsinst nv ts =
         match ts with
         | (Type tau) -> (nv, tau)
         | (Forall (alpha, sigma)) ->
               let (nv', beta) = newvar (lastusedtsvar nv sigma)
               let (nv'', sigma') = (tssubs nv' [ (Tyvar beta, alpha) ] sigma)
               tsinst nv'' sigma'
      let (nv', tau) = tsinst nv (assq v gamma)
      (nv', ([], tau))
   | Comb (e1, e2) ->
      let (nv', (S1, tau1)) = W nv gamma e1
      let (nv'', S1Gamma) = assumsubs nv' S1 gamma
      let (nv''', (S2, tau2)) = W nv'' S1Gamma e2
      let S2tau1 = subs S2 tau1
      let (nv'''', beta) = newvar nv'''
      let V = unify S2tau1 (tau2 ^--> Tyvar beta)
      let Vbeta = subs V (Tyvar beta)
      let VS2S1 = compose V (compose S2 S1)
      (nv'''', (VS2S1, Vbeta))
   | Abs (v, e) ->
      let (nv, beta) = newvar nv
      let (nv, (S1, tau1)) = W nv ((v, Type(Tyvar beta)) :: gamma) e
      let S1beta = subs S1 (Tyvar beta)

      (nv, (S1, (S1beta ^--> tau1)))
   | Let ((v, e1), e2) ->
      let (nv, (S1, tau1)) = W nv gamma e1
      let (nv, S1Gamma) = assumsubs nv S1 gamma
      let (nv, (S2, tau2)) = W nv ((v, tsclosure S1Gamma tau1) :: S1Gamma) e2
      let S2S1 = compose S2 S1
      (nv, (S2S1, tau2))

let principalts gamma e =
    let (var, (S, tau)) = W (lastfreeassumvar gamma) gamma e
    let (_, SGamma) = assumsubs var S gamma

    tsclosure SGamma tau

let pptsterm tau =
    let rec iter prec term =
        match term with
        | (Tyvar name) -> name
        | (Tyapp (name, [])) -> name
        | (Tyapp ("%f", [ a1; a2 ])) ->
            let maybebracket s = if prec <= 10 then s else $"({s})"
            maybebracket $"{iter 11 a1} -> {iter 10 a2}"

        | (Tyapp (name, args)) ->
            let rec arglist r ts =
                match ts with
                | [] -> r
                | (h :: t) ->
                  arglist (r + (iter 30 h) + (if t = [] then "" else ", ")) t

            if (List.length args) > 1 then
                $"""{arglist "(" args}) {name}"""
            else
                $"""{arglist "" args} {name}"""


    iter 10 tau


let ppexp e =
   let rec ppe r e =
      match e with
      | (Var v) -> r + v
      | (Comb (e1, e2)) -> $"""{r}({ppe "" e1} {ppe "" e2})"""
      | (Abs (v, e)) -> $"""{r}(\\{v}.{ppe "" e})"""
      | (Let ((v, e1), e2)) ->
         $"""{r}let {v}={ppe "" e1} in {ppe "" e2}"""

   ppe "" e

let ppts sigma =
    let rec iter r ts =
        match ts with
        | (Forall (sv, sts)) -> iter $"{r}!{sv}." sts
        | (Type term) -> $"{r}{pptsterm term}"

    iter "" sigma

let ppassums gamma =
    let rec iter r ass =
        match ass with
        | [] -> r
        | ((v, ts) :: assums) ->
            iter
                (r + v + ":" + (ppts ts) + (if assums = [] then "" else ","))
                assums

    iter "" gamma


(* Examples *)

(* Unification *)

// let x = Tyvar "x"
// let y = Tyvar "y"
// let z = Tyvar "z"

// let apply s l = Tyapp(s,l)

// let a = apply "a" []
// let j (x, y, z) = apply "j" [x; y; z]
// let f (x, y) = apply "f" [x; y]

// let t1 = j(x,y,z)
// let t2 = j(f(y,y), f(z,z), f(a,a));

// ppterm t1
// ppterm t2

// let U = unify t1 t2
// ppsubs U

// print ((ppterm (subs U t1))^"\n");
// print ((ppterm (subs U t2))^"\n");

// (* Constructors for types *)

// let mkFunc name args = Tyapp(name,args)
// let mkNullary name = mkFunc name []
// let mkUnary name arg = mkFunc name [arg]
// let mkBinary name arg1 arg2 = mkFunc name [arg1; arg2]
// let mkTernary name arg1 arg2 arg3 = mkFunc name [arg1; arg2; arg3]

// let pairt t1 t2 = mkBinary "pair" t1 t2
// let listt t = mkUnary "list" t
// let boolt = mkNullary "bool"

(* Type variables *)

// let alpha = Tyvar "a"
// let beta = Tyvar "b"
// let alpha' = Tyvar "a'"
// let beta' = Tyvar "b'"

(* Type-schemes *)

// exception Fail of string

// let rec mk_tyscheme terms t = 
//    match terms with
//    | [] -> Type t
//    | ((Tyvar v)::vs) -> Forall (v, mk_tyscheme vs t)
//    | _ -> raise (Fail "mk_tyscheme: Invalid type-scheme.")

(* Now we can construct type-schemes. For example here is a
   polymorphic function taking pairs of functions and two lists to a
   list of pairs: *)

// let dmapts = mk_tyscheme [alpha; alpha'; beta; beta']
//                   (pairt (alpha ^--> alpha') (beta ^--> beta')
//                       ^--> listt alpha ^--> listt beta
//                       ^--> pairt (listt alpha') (listt beta'));
// ppts dmapts;

(* Lambda expressions with let bindings *)

// let labs var e =
//    match var with
//    | (Var v) -> Abs(v,e)
//    | _ -> raise (Fail "labs: Invalid argument")

// let llet var e1 e2 =
//    match var with
//    | (Var v) -> Let((v,e1),e2)
//    | _ -> raise (Fail "llet: Invalid argument")

// infix @:
// let (@^) e1 e2 = Comb(e1,e2)

// let rec lambda es e =
//    match es with
//    | [] -> e
//    | (x::xs) -> labs x (lambda xs e)

// let rec letbind vars e =
//    match vars with
//    | [] -> e
//    | ((v,e1)::bs) -> llet v e1 (letbind bs e)

// let rec lapply r exprs =
//    match exprs with
//    | [] -> r
//    | (e::es) -> lapply (r @^ e) es
(*
// (* Variables *)

let x = Var "x"
let y = Var "y"
let z = Var "z"
let p = Var "p"
let f = Var "f"
let m = Var "m"
let n = Var "n"
let s = Var "s"
let i = Var "i"

// (* Church numerals *)

let num n =
   let f = Var "f"
   let x = Var "x"
   let rec iter r n =
         match n with
         | 0 -> lambda [f; x] r
         | _ -> iter (f @^ r) (n - 1)

   iter x n

// (* Now we can construct assumptions and expressions *)

// (* S ZERO = (λ n f x.n f (f x)) λ f x.x  *)

let ZERO = num 0
// let succ n f x = n f (f x)
let SUCC = lambda [n; f; x] (n @^ f @^ (f @^ x))
// ppts (principalts [] (S @^ ZERO))

// (* PRED and PRED 6 *)

// let pair x y f = f x y
let PAIR = (lambda [x; y; f] (f @^ x @^ y))
// let fst p = p (fun [x; y] -> x)
let FST = (lambda [p] (p @^ (lambda [x; y] x)))
// let smd p = p (fun [x; y] -> y)
let SND = (lambda [p] (p @^ (lambda [x; y] y)))

// let G f p = (f (fst p), fst p)
let G = lambda [f; p] (PAIR @^ (f @^ (FST @^ p)) @^ (FST @^ p))
let PRED = lambda [n] (SND @^ (n @^ (G @^ SUCC) @^ (PAIR @^ ZERO @^ ZERO)))
let SUB = lambda [m;  n] (n @^ PRED @^ m);

// ppts (principalts [] PRED);
// ppts (principalts [] (PRED @: (num 6)));
// ppts (principalts [] SUB);

(* The definition of PRED from Larry Paulson's lecture notes *)

let PREDp = lambda [n; f; x] (SND @^ (n @^ (G @^ f) @^ (PAIR @^ x @^ x)))
let SUBp = lambda [m; n] (n @^ PREDp @^ m);

// ppts (principalts [] PREDp);
// ppts (principalts [] (PREDp @: (num 6)));

// ppexp SUBp;
// ppts (principalts [] SUBp);

// (* let i=λx.x in i i *)

let i = Var "i"
let x = Var "x"
let polylet = letbind [(i,lambda [x] x)] (i @^ i)
ppexp polylet
ppts (principalts [] polylet);

// (* map *)
let condts = mk_tyscheme [alpha] (boolt ^--> alpha ^--> alpha ^--> alpha)
let fixts = mk_tyscheme [alpha] ((alpha ^--> alpha) ^--> alpha)

let nullts = mk_tyscheme [alpha] (listt alpha ^--> boolt)
let nilts = mk_tyscheme [alpha] (listt alpha)
let consts = mk_tyscheme [alpha] (alpha ^--> listt alpha ^--> listt alpha)
let hdts = mk_tyscheme [alpha] (listt alpha ^--> alpha)
let tlts = mk_tyscheme [alpha] (listt alpha ^--> listt alpha)

let pairts = mk_tyscheme [alpha; beta] (alpha ^--> beta ^--> pairt alpha beta)
let fstts = mk_tyscheme [alpha; beta] (pairt alpha beta ^--> alpha)
let sndts = mk_tyscheme [alpha; beta] (pairt alpha beta ^--> beta)

let bool_assums = [("true",Type(boolt));("false",Type(boolt));("cond",condts)]
let pair_assums = [("pair",pairts);("fst",fstts);("snd",sndts)]
let fix_assums = [("fix",fixts)]
let list_assums = [("null",nullts);("nil",nilts);
                   ("cons",consts);("hd",hdts);("tl",tlts)]

(* let map = (fix (λ map f s.
                 (cond (null s) nil
                       (cons (f (hd s)) (map f (tl s)))))) in map *)

let assums = bool_assums@fix_assums@list_assums
*)
// let map' = Var "map"
// let fix = Var "fix"
// let null' = Var "null"
// let nil' = Var "nil"
// let cond = Var "cond"
// let cons = Var "cons"
// let hd' = Var "hd"
// let tl' = Var "tl"
// let f = Var "f"
// let s = Var "s"

// let mapdef =
//    letbind [(map',
//            (fix @^ (lambda [map'; f; s]
//                          (cond @^ (null' @^ s)
//                                @^ nil'
//                                @^ (cons @^ (f @^ (hd' @^ s))
//                                        @^ (map' @^ f @^ (tl' @^ s)))))))]
//             map'
(*
// ppassums assums;
// ppexp mapdef;
let mapdefts = principalts assums mapdef
// ppts mapdefts;

let x1 = Var "x1"
let x2 = Var "x2"
let x3 = Var "x3"
let x4 = Var "x4"
let x5 = Var "x5"
let pair = Var "pair"

// ppts (principalts [] mairson);

// (* handle expected exceptions *)

exception TestFail
let expect_Unify se f =
   try
      ignore (f ()) 
   with
   | Unify s -> if se = s then () else raise TestFail

// (* omega *)

// let omega = fun x -> x x
let omegaexp = Let(("omega",Abs ("x",Comb(Var "x",Var "x"))),Var "omega")
let omegadef = fun () ->
    principalts [] omegaexp;
expect_Unify "Occurs" omegadef;

// (* Y *)

// val r = Abs("x",Comb(Var "f",Comb(Var "x",Var "x")))
// val ydef = fn () => principalts [] (Let(("Y",Abs ("f",Comb(r,r))),Var "Y"));
// expect_Unify "Occurs" ydef;

// (* Church numerals *)
// val nassums = [];

// val cn_zerodef = Abs("f",Abs("x",Var "x"));
// ppassums nassums;
// ppexp cn_zerodef;
// val cn_zero = principalts [] cn_zerodef;
// ppts cn_zero;

// val cn_onedef = Abs("f",Abs("x",Comb(Var "f", Var "x")));
// ppassums nassums;
// ppexp cn_onedef;
// val cn_one = principalts [] cn_onedef;
// ppts cn_one;

// val cn_twodef = Abs("f",Abs("x",Comb(Var "f",Comb(Var "f",Var "x"))));
// ppassums nassums;
// ppexp cn_twodef;
// val cn_two = principalts [] cn_twodef;
// ppts cn_two;

// ppts cn_zero;
// ppts cn_one;
// ppts cn_two;

// (* Church numerals in SML *)

// fn f => fn x => x;
// fn f => fn x => f x;
// fn f => fn x => f (f x)

*)