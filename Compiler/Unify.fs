module Unify

open TypeSystem

type Typescheme =
    | Forall of string * Typescheme
    | Type of TypeSystem.Type

let rec subs ss term =
    match (ss, term) with
    | ([], t) -> t
    | (((t1,v1)::ss), (GenericType name)) ->
        if name = v1 then t1 else subs ss term
    | (_, CustomType (_, [])) ->
        term
    | (l, CustomType (name, args)) ->
        let rec arglist r rs = 
            match (r, rs) with
            | (_, []) -> List.rev r
            | (r, (h::t)) ->
                arglist ((subs l h)::r) t
        CustomType (name, arglist [] args)
    | (l, TupleType args) ->
        let rec arglist r rs = 
            match (r, rs) with
            | (_, []) -> List.rev r
            | (r, (h::t)) ->
                arglist ((subs l h)::r) t
        TupleType (arglist [] args)
    | (l, ListType ty) ->
        ListType (subs l ty)
    | (l, MapType (kty, vty)) ->
        MapType (subs l kty, subs l vty)
    | (l, StructType (fields, isOpen)) ->
        let subField (n, t) = (n, (subs l t))
        let fs = List.map (subField) fields
        StructType (fs, isOpen)
    | (l, FunctionType (args, ret)) ->
        let subArg (n, t) = (n, (subs l t))
        let fs = List.map (subArg) args
        FunctionType (fs, subs l ret)
    | (_, StringType) ->
        term
    | (_, NumberType) ->
        term

let rec compose ss s1 =
    match (ss, s1) with
    | ([], _) -> s1
    | ((s::ss), s1) ->
        let rec iter r s rs =
            match rs with
            | [] -> List.rev r
            | ((t1,v1)::ss) ->
                iter (((subs [s] t1),v1)::r) s ss
        compose ss (s::(iter [] s s1))

type UnifyError =
    | Arity
    | Occurs
    | Const
    | Field

exception Unify of UnifyError

let rec fieldNamesMatch fs1 fs2 =
    match (fs1, fs2) with
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> false
    | ((f1::f1s), (f2::f2s)) ->
        f1 = f2 && (fieldNamesMatch f1s f2s)

let fieldNames = List.map (fst)
let fieldTypes = List.map (snd)

let unify t1 t2 =
    let rec iter r t1 t2 =
        let rec occurs v term =
            match term with
            | (GenericType vn) -> vn = v
            | StringType -> false
            | NumberType -> false
            | ListType ty -> occurs v ty
            | MapType (kty, vty) ->
                occurs v kty || occurs v vty
            | TupleType tys ->
                List.exists (fun ty -> occurs v ty) tys
            | StructType (fields, _) ->
                List.exists (fun (_, ty) -> occurs v ty) fields
            | FunctionType (args, ret) ->
                (List.exists (fun (_, ty) -> occurs v ty) args) || occurs v ret
            | CustomType (_, vars) ->
                List.exists (fun ty -> occurs v ty) vars

        let rec unifyArgs r ls rs =
            match (ls, rs) with
            | ([], []) -> List.rev r
            | ([], _) -> raise (Unify Arity)
            | (_, []) -> raise (Unify Arity)
            | ((t1::t1s), (t2::t2s)) ->
                unifyArgs (compose (iter [] (subs r t1) (subs r t2)) r) t1s t2s

        match (t1,t2) with
        | (GenericType v1, GenericType v2) ->
            if (v1 = v2) then [] else ((t1, v2)::r)
        | (StringType, StringType) -> []
        | (NumberType, NumberType) -> []
        | (GenericType v, StringType)
        | (GenericType v, NumberType)
        | (GenericType v, CustomType (_, [])) -> ((t2, v)::r)
        | (GenericType v, _) ->
            if occurs v t2 then raise (Unify Occurs) else ((t2, v)::r)
        | (_, GenericType v) ->
            if occurs v t1 then raise (Unify Occurs) else ((t1, v)::r)
        | (ListType ta, ListType tb) -> iter r ta tb
        | (MapType (k1, v1), MapType (k2, v2)) ->
            unifyArgs r [k1; v1] [k2; v2]
        | (TupleType tys1, TupleType tys2) -> 
            unifyArgs r tys1 tys2
        | (StructType (fs1, _), StructType (fs2, _)) ->
            if fieldNamesMatch (fieldNames fs1) (fieldNames fs2) then
                unifyArgs r (fieldTypes fs1) (fieldTypes fs2)
            else raise (Unify Field)
        | (FunctionType (as1, ret1), FunctionType (as2, ret2)) ->
            List.concat [(unifyArgs r (List.map (snd) as1) (List.map (snd) as2)); (iter r ret1 ret2)]
        | (CustomType (n1, tys1), CustomType (n2, tys2)) when n1=n2 ->
            unifyArgs r tys1 tys2
        | _ -> raise (Unify Const)
        // These are just for reference
        // | (Tyvar v,Tyapp(_,[])) -> ((t2, v)::r)
        // | (Tyapp(_,[]),Tyvar v) -> ((t1, v)::r)
        // | (Tyvar v,Tyapp _) ->
        //     if occurs v t2 then raise (Unify Occurs) else ((t2, v)::r)
        // | (Tyapp _,Tyvar v) ->
        //     if occurs v t1 then raise (Unify Occurs) else ((t1, v)::r)
        // | (Tyapp(name1,args1),Tyapp(name2,args2)) ->

    iter [] t1 t2

(*
 *
 *   Infer - Algorithm W
 *    
 *)

exception Assum of string

let inline ord c = int c - int '0'

let inline chr (ascii: int) =
    System.Convert.ToChar(ascii + (int '0'))

let mem = List.contains

let inline explode (str: string) = str.ToCharArray() |> Array.toList

let newvar v =
    let nv = v + 1

    let rec prime v n =
        match n with
        | 0 -> v
        | _ -> prime $"{v}'" (n - 1)

    let primes = nv / 26
    let var = $"{(chr ((ord 'a') + (nv % 26)))}"
    (nv, prime var primes)

let rec fbtyvars free bound term =
    match term with
    | GenericType v ->
        if (mem v bound) then
            (free, bound)
        else
            (v :: free, bound)
    | StringType
    | NumberType
    | CustomType (_, []) ->
        (free, bound)
    // | Tyapp (name, args) ->
    //     let rec iter r terms =
    //         match terms with
    //         | [] -> r
    //         | (t :: ts) ->
    //             let (f, b) = fbtyvars r bound t
    //             iter f ts

    //     let fvs = iter free args
    //     (fvs, bound)

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
                    let (nv, sigma') = iter nv (compose [ (GenericType newv, sv) ] rnss) tvp sts

                    (nv, Forall(newv, sigma'))
                else
                    let (nv, sigma') = iter nv rnss tvp sts

                    (nv, Forall(sv, sigma'))
            | (_, (Type term)) -> (nv, (Type(subs [ tvp ] (subs rnss term))))

        let (nv, sigma') = iter nv [] sub sigma

        tssubs nv restOfSubs sigma'

let assq p l =
//    printfn "Looking for var %s" p
   let rec iter assums =
      match assums with
      | [] -> raise (Assum p)
      | (k, v) :: xs -> if (k = p) then v else iter xs

   iter l

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

let rec fbtsvs free bound sigma =
    match sigma with
    | Forall (var, sigma') -> fbtsvs free (var :: bound) sigma'
    | Type term -> fbtyvars free bound term

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

let rec W nv gamma exp : int * ((Type * string) list * Type) =
   match exp with
   | AST.ChakraVar (_, (v, _)) ->
      let rec tsinst nv ts =
         match ts with
         | (Type tau) -> (nv, tau)
         | (Forall (alpha, sigma)) ->
               let (nv', beta) = newvar (lastusedtsvar nv sigma)
               let (nv'', sigma') = (tssubs nv' [ (GenericType beta, alpha) ] sigma)
               tsinst nv'' sigma'
      let (nv', tau) = tsinst nv (assq v gamma)
      (nv', ([], tau))
    | AST.ChakraString _ ->
        (nv, ([], StringType))
    | AST.ChakraNumber _ ->
        (nv, ([], NumberType))
    | AST.ChakraTuple (_, exprs) ->
        let rec wexprs nv ss tys es =
            match es with
            | [] -> (nv, (ss, TupleType (List.rev tys)))
            | e::es ->
                let (nv', (ss', tau1)) = W nv gamma e
                let ss'' = compose ss' ss
                wexprs nv' ss'' ((subs ss'' tau1)::tys) es
        wexprs nv [] [] exprs
    | AST.ChakraStruct (_, s) ->
        let rec wfields nv ss tys (fs: AST.ChakraStructField list) =
            match fs with
            | [] -> (nv, (ss, StructType (List.sortBy (fst) tys, false)))
            | ({ Loc = _; Name = n; Value = e})::es ->
                let (nv', (ss', tau1)) = W nv gamma e
                let ss'' = compose ss' ss
                wfields nv' ss'' ((n, subs ss'' tau1)::tys) es
        wfields nv [] [] s.Fields
    | AST.ChakraMatchExpr (_, m) ->
        raise (System.ApplicationException "Match expression inference not implemented")
    | AST.ChakraPipeExpr p ->
        raise (System.ApplicationException "Pipe inference not implemented")
    | AST.ChakraLambda (_, l) ->
        raise (System.ApplicationException "Lambda inference not implemented")
    | AST.ChakraList (_, { Items = items }) ->
        // Infer the type for each expression, then unify the types of each
        let rec itemsubs nv gamma s ty exps =
            match exps with
            | [] -> 
                (nv, (s, (gamma, ty)))
            | (exp::exps) ->
                let (nv', (S1, tau1)) = W nv gamma exp
                let ss = unify ty tau1
                itemsubs nv' gamma (compose ss S1) tau1 exps
        let (nv', beta) = newvar nv
        let (nv'', (s, (gamma', ty))) = itemsubs nv' gamma [] (GenericType beta) items
        (nv'', (s, (ListType ty)))
    | AST.ChakraApplyExpr (span, AST.ChakraApply (id, args)) ->
        let (n, path) = id
        let (_, (_, fnTy)) = W nv gamma (AST.ChakraVar (span, (n, Some path)))
        match fnTy with
        | FunctionType (argTys, ret) ->
            let rec unifyArgs ss nv exprs tys =
                match (exprs, tys) with
                | (_ :: _, []) -> raise (Unify Arity)
                | ([], _ :: _) -> raise (Unify Arity)
                | ([], []) -> ss
                | (e :: es, t :: ts) ->
                    let (nv', (S1, tau1)) = W nv gamma e
                    let ss' = unify t tau1
                    unifyArgs (compose ss' S1) nv' es ts
            let ss = unifyArgs [] nv args (List.map (snd) argTys)
            let ret' = subs ss ret
            // if args all unify, use subs on ret
            // return result
            (0, (ss, ret'))
        | e ->
            raise (Assum (sprintf "Expected a function, found %O" e))
        
    //   printfn "Comb"
    //   let (nv', (S1, tau1)) = W nv gamma e1
    //   printfn "Got subs for e1\n[Term]: %s\n[Subs]: %s" (ppterm tau1) (ppsubs S1)
    //   let (nv'', S1Gamma) = assumsubs nv' S1 gamma
    //   let (nv''', (S2, tau2)) = W nv'' S1Gamma e2
    //   printfn "Got subs for e2\n[Term]: %s\n[Subs]: %s" (ppterm tau2) (ppsubs S2)
    //   let S2tau1 = subs S2 tau1
    //   printfn "Got the term for tau1 with the subs for e2: %s" (ppterm S2tau1)
    //   let (nv'''', beta) = newvar nv'''
    //   let V = unify S2tau1 (tau2 ^--> Tyvar beta)
    //   let Vbeta = subs V (Tyvar beta)
    //   let VS2S1 = compose V (compose S2 S1)
    //   printfn "End Comb"
    //   (nv'''', (VS2S1, Vbeta))