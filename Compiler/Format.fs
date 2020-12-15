(*
    This is an adaptation of Phillip Wadler's "A Prettier Printer" algorithm
    Original document can be found at: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
*)
module Format

open ChakraParser
open System.Text.RegularExpressions

(*
    infixr 5 :<|>
    infixr 6 :<>
    infixr 6 <>
*)
(*
    data DOC = NIL
        | DOC :<> DOC
        | NEST Int DOC
        | TEXT String
        | LINE
        | DOC :<|> DOC
*)

type Op =
    | NilOp
    | UnionOp of Op * Op
    | NestOp of int * Op
    | TextOp of string
    | LineOp
    | FlattenOp of Op * Op

(*
    data Doc = Nil
        | String ‘Text‘ Doc
        | Int ‘Line‘ Doc
*)

type Doc =
    | Nil
    | Text of string * Doc
    | Line of int * Doc

(* nil = NIL *)
let nil = NilOp

(* x <> y = x :<> y *)
let (<&>) x y = UnionOp(x, y)

(* nest i x = NEST i x *)
let nest i x = NestOp(i, x)

(* text s = TEXT s *)
let text s = TextOp s

(* line = LINE *)
let line = LineOp

(*
    flatten NIL = NIL
    flatten (x :<> y) = flatten x :<> flatten y
    flatten (NEST i x) = NEST i (flatten x)
    flatten (TEXT s) = TEXT s
    flatten LINE = TEXT " "
    flatten (x :<|> y) = flatten x
*)

let rec flatten op =
    match op with
    | NilOp -> NilOp
    | UnionOp (x, y) -> UnionOp(flatten x, flatten y)
    | NestOp (i, x) -> NestOp(i, flatten x)
    | TextOp s -> TextOp s
    | LineOp -> TextOp " "
    | FlattenOp (x, y) -> flatten x

(* group x = flatten x :<|> x *)
let group x = FlattenOp(flatten x, x)

(*
    layout Nil = ""
    layout (s ‘Text‘ x) = s ++ layout x
    layout (i ‘Line‘ x) = ’\n’ : copy i ’ ’ ++ layout x
*)

let rec layout doc =
    match doc with
    | Nil -> ""
    | Text (s, x) -> s + (layout x)
    | Line (i, x) -> "\n" + String.replicate i "\t" + layout x

(* copy i x = [ x | _ <- [1..i] ] *)
let copy i x = List.map (fun _ -> x) [ 1 .. i ]

(*
    be w k [] = Nil
    be w k ((i,NIL):z) = be w k z
    be w k ((i,x :<> y):z) = be w k ((i,x):(i,y):z)
    be w k ((i,NEST j x):z) = be w k ((i+j,x):z)
    be w k ((i,TEXT s):z) = s ‘Text‘ be w (k+length s) z
    be w k ((i,LINE):z) = i ‘Line‘ be w i z
    be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z))
    (be w k ((i,y):z))
*)

let rec be w k ops =
    match ops with
    | [] -> Nil
    | (i, NilOp) :: z -> be w k z
    | (i, UnionOp (x, y)) :: z -> be w k ((i, x) :: (i, y) :: z)
    | (i, NestOp (j, x)) :: z -> be w k ((i + j, x) :: z)
    | (i, TextOp s) :: z -> Text(s, be w (k + s.Length) z)
    | (i, LineOp) :: z -> Line(i, be w i z)
    | (i, FlattenOp (x, y)) :: z -> better w k (be w k ((i, x) :: z)) (be w k ((i, y) :: z))

(* better w k x y = if fits (w-k) x then x else y *)
and better w k x y = if fits (w - k) x then x else y

(*
    fits w x | w < 0 = False
    fits w Nil = True
    fits w (s ‘Text‘ x) = fits (w - length s) x
    fits w (i ‘Line‘ x) = True
*)
and fits w doc =
    if w < 0 then
        false
    else
        match doc with
        | Nil -> true
        | Text (s, x) -> fits (w - s.Length) x
        | Line (i, x) -> true

(* best w k x = be w k [(0,x)] *)
let best w k x = be w k [ (0, x) ]

(* pretty w x = layout (best w 0 x) *)
let pretty w x = layout (best w 0 x)


(* -- Utility functions *)

let hardLine = text "\n"


(* x <+> y = x <> text " " <> y *)
let (<+>) x y = x <&> text " " <&> y

(* x </> y = x <> line <> y *)
let (</>) x y = x <&> line <&> y

let (</|>) x y = x <&> hardLine <&> y
(*
    folddoc f [] = nil
    folddoc f [x] = x
    folddoc f (x:xs) = f x (folddoc f xs)
*)

let rec folddoc f ops =
    match ops with
    | [] -> nil
    | [ x ] -> x
    | x :: xs -> f x (folddoc f xs)

(* spread = folddoc (<+>) *)
let spread = folddoc (<+>)

(* stack = folddoc (</>) *)
let stack = folddoc (</>)
let hardStack = folddoc (</|>)
let onNewLine op = line <&> op

let block n ops =
    List.map (onNewLine >> (nest n)) ops
    |> folddoc (<&>)
(*
    bracket l x r = group (text l <>
        nest 2 (line <> x) <>
        line <> text r)
*)

let bracket l x r =
    group (text l <&> nest 1 (line <&> x) <&> line <&> text r)

(* x <+/> y = x <> (text " " :<|> line) <> y *)
let (<+/>) x y = x <&> FlattenOp(text " ", line) <&> y

let wordRegex = Regex "\W+"
let words = Array.toList << (wordRegex.Split)

(* fillwords = folddoc (<+/>) . map text . words *)

let fillwords =
    folddoc (<+/>) << (List.map text) << words
(*
    fill [] = nil
    fill [x] = x
    fill (x:y:zs) =
        (flatten x <+> fill (flatten y : zs))
        :<|> (x </> fill (y : zs))
*)

let rec fill ops =
    match ops with
    | [] -> nil
    | [ x ] -> x
    | x :: y :: zs -> FlattenOp((flatten x <+> fill (flatten y :: zs)), x </> fill (y :: zs))

let withComma s = TextOp(sprintf "%s," s)
let opWithComma op = op <&> TextOp ","
let opWithNewline op = op <&> line

let rec shouldStayOnOneLine expr nestingLevel =
    match expr with
    | ChakraApplyExpr (_, ChakraApply ((_, pathSegments), exprs)) -> (pathSegments.Length + exprs.Length) < 2
    | ChakraMatchExpr _ -> false
    | ChakraLiteralExpr (_, lit) ->
        match lit with
        | ChakraVar (s, path) -> true
        | ChakraNumber _ -> true
        | ChakraString s -> s.Length < 15
        | ChakraSymbol s -> s.Length < 15
        | ChakraLambda _ -> false
        | ChakraTuple items ->
            items.Length
            <= 1
            && nestingLevel = 0
            && shouldStayOnOneLine (List.head items) (nestingLevel + 1)
        | ChakraList { Items = items; Spread = spread } ->
            if spread <> None then
                false
            else
                items.Length
                <= 1
                && nestingLevel = 0
                && shouldStayOnOneLine (List.head items) (nestingLevel + 1)
        | ChakraStruct { Fields = fs; Spread = spread } ->
            if spread <> None then
                false
            else
                fs.Length
                <= 1
                && nestingLevel = 0
                && shouldStayOnOneLine (List.head fs).Value (nestingLevel + 1)
        | ChakraMap _ -> false
    | ChakraNativeExpr _ -> false

let rec showModule (mod': ChakraModule) =
    let doc =
        Option.map (fun d ->
            (showComment { Content = d; IsDoc = true })
            <&> line) mod'.DocComments
        |> Option.defaultValue NilOp

    doc
    <&> showModuleDef mod'.Exports
    <&> line
    <&> line
    <&> block 0 (List.map (showImport) mod'.Imports)
    <&> line
    <&> line
    <&> block 0 (List.map (opWithNewline << showBinding) mod'.Bindings)

and createFullId (id, maybePath) =
    (id
     + (Option.defaultValue [] maybePath
        |> String.concat "."))

and showLiteral (lit: ChakraLiteral) =
    match lit with
    | ChakraVar id -> TextOp(createFullId id)
    | ChakraNumber num -> TextOp(sprintf "%f" num)
    | ChakraSymbol s -> TextOp(sprintf "#%s" s)
    | ChakraString s -> TextOp(sprintf "\"%s\"" s)
    | ChakraTuple [] -> TextOp "()"
    | ChakraTuple [ item ] ->
        if shouldStayOnOneLine item 0 then
            TextOp "( " <&> showExpr item <&> TextOp " )"
        else
            text "("
            <&> (block 1 (List.map (opWithComma << showExpr) [ item ]))
            <&> line
            <&> text ")"
    | ChakraTuple items ->
        text "("
        <&> (block 1 (List.map (opWithComma << showExpr) items))
        <&> line
        <&> text ")"
    | ChakraStruct fields -> TextOp "%()"
    | ChakraList { Items = []; Spread = None } -> TextOp "[]"
    | ChakraList { Items = []; Spread = Some (_, var) } ->
        let flattenedVar =
            var.First
            + String.concat "." (Option.defaultValue [] var.Rest)

        TextOp "[ "
        <&> (text (sprintf "...%s" flattenedVar))
        <&> TextOp " ]"
    | ChakraList { Items = [ item ]; Spread = spread } -> TextOp "[ " <&> showExpr item <&> TextOp " ]"
    | ChakraList { Items = items; Spread = spread } ->
        text "["
        <&> (block 1 (List.map (opWithComma << showExpr) items))
        <&> line
        <&> text "]"
    | ChakraMap pairs -> TextOp "%[]"
    | ChakraLambda l -> TextOp "{ () -> }"

and showMatchClause (ChakraMatchClause (lit, exprList)) = TextOp "ChakraMatchClause"

and showExpr (expr: ChakraExpr) =
    match expr with
    | ChakraLiteralExpr (_, lit) -> showLiteral lit
    | ChakraMatchExpr (_, ChakraMatch (list, clauses)) -> TextOp "ChakraMatchExpr"
    | ChakraApplyExpr (_, ChakraApply ((id, path), [])) ->
        let fullId = (id + (String.concat "." path))
        TextOp(sprintf "%s()" fullId)
    | ChakraApplyExpr (_, ChakraApply ((id, path), [ arg ])) ->
        let fullId = (id + (String.concat "." path))

        if shouldStayOnOneLine arg 0 then
            TextOp(sprintf "%s( " fullId)
            <&> showExpr arg
            <&> TextOp " )"
        else
            text "("
            <&> (block 1 (List.map (opWithComma << showExpr) [ arg ]))
            <&> line
            // How to deal with this?
            <&> text ")" // THis is here
    | ChakraApplyExpr (_, ChakraApply ((id, path), args)) ->
        let fullId = (id + (String.concat "." path))

        text (sprintf "%s(" fullId)
        <&> (block 1 (List.map (opWithComma << showExpr) args))
        <&> line
        <&> text ")"
    | ChakraNativeExpr s -> TextOp(sprintf "$$NATIVE$$%s$$" s)

and showExprList (ChakraExprList (bs, expr)) =
    match bs with
    | [] -> showExpr expr
    | _ ->
        let bindingOps =
            (List.map (opWithNewline << showBinding) bs)

        let exprOpAsList = [ (showExpr expr) ]
        block 1 (List.concat [ bindingOps; exprOpAsList ])

and showBindingPattern (patt: ChakraBindingPattern) =
    match patt with
    | ChakraSimpleBindingPattern name -> TextOp(sprintf "%s =" name)
    | ChakraFunctionBindingPattern { Name = n; Args = a } -> TextOp(sprintf "%s(%s) =" n (String.concat ", " a))
    | ChakraComplexBindingPattern (_) -> TextOp "PATTERN ="

and showBinding ({ Loc = _; DocComment = optComment; Pattern = patt; ExprList = exprList }) =
    (Option.defaultValue
        NilOp
         (Option.map (fun c ->
             (showComment { Content = c; IsDoc = true })
             <&> line) optComment))
    <&> showBindingPattern patt
    <+> showExprList exprList

and showBindingType (typ: ChakraImportBindingType) =
    match typ with
    | ChakraSimpleImportBinding name -> TextOp(sprintf "%s =" name)
    | ChakraDestructuredImportBinding bMap when bMap.Count <= 1 ->
        let (k, v) = List.head (Map.toList bMap)
        if k = v then
            text "%( " <&> text k <&> text " )"
        else
            text "%( "
            <&> text (sprintf "%s = %s" k v)
            <&> text " )"

    | ChakraDestructuredImportBinding bMap when bMap.Count > 1 ->
        let pairs =
            let pairOp (k, v) =
                if k = v then text k else text (sprintf "%s = %s" k v)

            List.map pairOp (Map.toList bMap)

        text "%("
        <&> (block 1 (List.map (opWithComma) pairs))
        <&> line
        <&> text ") ="

and showImport (imp: ChakraImport) =
    match imp with
    | ChakraPackageImport info ->
        showBindingType info.Typ
        <+> TextOp(sprintf "/%s" info.PackageName)

    | ChakraLocalImport info ->
        showBindingType info.Typ
        <+> if info.Relative then TextOp(sprintf "/root/%s" info.Library) else TextOp(sprintf "./%s" info.Library)

and showComment (comment: ChakraComment) =
    let withSlash isDoc s =
        TextOp(sprintf "%s%s" (if isDoc then ";; " else "; ") s)

    (Array.map (withSlash comment.IsDoc) (comment.Content.Split("\n")))
    |> Array.toList
    |> block 0

and showModuleDef (exports: string list) =
    text "= %("
    <&> (block 1 (List.map (withComma) exports))
    <&> line
    <&> text ")"

let tabs i =
    (List.fold (fun acc _ -> sprintf "%s%s" "\t" acc) "" [ 1 .. i ])

let rec showOp op i =
    match op with
    | UnionOp (l, r) ->
        "(\n"
        + (tabs i)
        + (showOp l (i + 1))
        + " <> "
        + (showOp r (i + 1))
        + (tabs i)
        + "\n)"
    | FlattenOp (l, r) ->
        "(\n"
        + (tabs i)
        + (showOp l (i + 1))
        + " <|> "
        + (showOp r (i + 1))
        + (tabs i)
        + "\n)"
    | NilOp -> "0"
    | LineOp -> "/"
    | TextOp s -> "\"" + s.Replace("\n", "\\n") + "\""
    | NestOp (n, o) -> (sprintf "(\n%s%i>> %s%s\n)" (tabs i) n (showOp o (i + 1)) (tabs i))
