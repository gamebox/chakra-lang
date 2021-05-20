(*
    This is an adaptation of Phillip Wadler's "A Prettier Printer" algorithm
    Original document can be found at: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
*)
module Pretty

open AST
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
let private (<&>) x y = UnionOp(x, y)

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
    | Line (i, x) -> "\n" + String.replicate i "	" + layout x

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
let private (<+>) x y = x <&> text " " <&> y

(* x </> y = x <> line <> y *)
let private (</>) x y = x <&> line <&> y

let private (</|>) x y = x <&> hardLine <&> y
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
let private (<+/>) x y = x <&> FlattenOp(text " ", line) <&> y

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

let createFullId (id, maybePath) =
    (String.concat "." (id :: (Option.defaultValue [] maybePath)))

let rec shouldStayOnOneLine expr nestingLevel =
    match expr with
    | ChakraApplyExpr (_, ChakraApply ((_, pathSegments), exprs)) -> (pathSegments.Length + exprs.Length) < 2
    | ChakraApplyExpr (_, ChakraNamedApply ((_, pathSegments), exprs)) -> (pathSegments.Length + exprs.Length) < 2
    | ChakraMatchExpr _ -> false
    | ChakraVar (s, path) -> true
    | ChakraNumber _ -> true
    | ChakraString (_, s) -> s.Length < 15
    | ChakraSymbol (_, s) -> s.Length < 15
    | ChakraLambda _ -> false
    | ChakraTuple (_, items) ->
        items.Length <= 1
        && nestingLevel = 0
        && shouldStayOnOneLine (List.head items) (nestingLevel + 1)
    | ChakraList (_, { Items = items; Spread = spread }) ->
        if spread <> None then
            false
        else
            items.Length <= 1
            && nestingLevel = 0
            && shouldStayOnOneLine (List.head items) (nestingLevel + 1)
    | ChakraStruct (_, { Fields = fs; Spread = spread }) ->
        if spread <> None then
            false
        else
            fs.Length <= 1
            && nestingLevel = 0
            && shouldStayOnOneLine (List.head fs).Value (nestingLevel + 1)
    | ChakraMap _ -> false
    | ChakraNativeExpr _ -> false
    | ChakraPipeExpr _ -> false

let rec showPattern (patt: ChakraPattern) =
    match patt with
    | CPIgnore _ -> text "_"
    | CPVar (_, s) -> text (sprintf "%s" s)
    | CPNumber (_, f) -> text (sprintf "%M" f)
    | CPSymbol (_, s) -> text (sprintf "#%s" s)
    | CPString (_, s) -> text (sprintf "\"%s\"" s)
    | CPTuple (_, items) ->
        match items with
        | [] -> text "()"
        | items ->
            let itemsOp =
                block 1 (List.map (opWithComma << showPattern) items)

            bracket "(" itemsOp ")"
    | CPStruct (_, st) ->
        let shouldPun name value =
            match value with
            | CPVar (_, str) -> name = str
            | _ -> false

        let fieldOp ({ Name = name; ValuePattern = value }: CPStructField) =
            if shouldPun name value then
                text name
            else
                text name <+> text "=" <+> showPattern value

        let spreadOps = if st.Rest then [ text "..." ] else []

        match st.Fields with
        | [] ->
            if spreadOps.Length = 0 then
                text "%()"
            else
                text "%( " <&> spreadOps.Head <&> text " )"
        | [ field ] when spreadOps.Length = 0 -> text "%( " <&> fieldOp field <&> text " )"
        | fields ->
            text "%( "
            <&> block
                    1
                    (List.map
                        (opWithComma)
                        (List.concat [ List.map (fieldOp) fields
                                       spreadOps ]))
            <&> line
            <&> text " )"
    | CPList (_, l) ->
        let spreadOps =
            match l.Rest with
            | Some (_, var) -> [ text (sprintf "...%s" var) ]
            | _ -> []

        match l.Items with
        | [] ->
            if spreadOps.Length = 0 then
                text "[]"
            else
                text "[" <+> spreadOps.Head <+> text "]"
        | items ->
            text "["
            <&> block
                    1
                    (List.map
                        (opWithComma)
                        (List.concat [ List.map (showPattern) items
                                       spreadOps ]))
            <&> line
            <&> text "]"
    | CPMap (_, m) ->
        let pairOp
            { KeyPattern = key
              ValuePattern = value }
            =
            showPattern key <+> text "=" <+> showPattern value

        let spreadOps =
            match m.Rest with
            | Some (_, var) -> [ text (sprintf "...%s" var) ]
            | _ -> []

        match m.Pairs with
        | [] ->
            if spreadOps.Length = 0 then
                text "%[]"
            else
                text "%[" <+> spreadOps.Head <+> text "]"
        | pairs ->
            text "%["
            <&> block
                    1
                    (List.map
                        (opWithComma)
                        (List.concat [ List.map (pairOp) pairs
                                       spreadOps ]))
            <&> line
            <&> text "]"

let shouldPun name value =
    match value with
    | ChakraVar (span, (str, None)) -> name = str
    | _ -> false

let shouldPunExpr name value = shouldPun name value

let rec showModule (mod': ChakraModule) =
    let doc =
        Option.map
            (fun d ->
                (showComment { Content = d; IsDoc = true })
                <&> line)
            mod'.DocComments
        |> Option.defaultValue NilOp

    doc
    <&> showModuleDef mod'.Exports
    <&> line
    <&> line
    <&> block 0 (List.map (showImport) mod'.Imports)
    <&> line
    <&> line
    <&> block 0 (List.map (opWithNewline << showBinding) mod'.Bindings)

and showMatchClause (ChakraMatchClause (lit, exprList)) = TextOp "ChakraMatchClause"

and showApply app =
    match app with
    | ChakraApply ((id, path), []) ->
        let fullId = (id + (String.concat "." path))
        TextOp(sprintf "%s()" fullId)
    | ChakraApply ((id, path), [ arg ]) ->
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
            <&> text ")"
    | ChakraApply ((id, path), args) ->
        let fullId = (id + (String.concat "." path))

        text (sprintf "%s(" fullId)
        <&> (block 1 (List.map (opWithComma << showExpr) args))
        <&> line
        <&> text ")"
    | ChakraNamedApply ((id, path), args) ->
        let fullId = (id + (String.concat "." path))

        match args with
        | [] -> text (sprintf "%s()" fullId)
        | [ (_, (name, value)) ] when shouldStayOnOneLine value 0 ->
            text (sprintf "%s(" fullId)
            <&> text (sprintf "%s = " name)
            <&> showExpr value
            <&> text ")"
        | _ ->
            let argFolder (_, (name, value)) =
                if shouldPunExpr name value then
                    text name
                else
                    text name <+> text "=" <+> showExpr value

            text (sprintf "%s(" fullId)
            <&> block 1 (List.map (opWithComma << argFolder) args)
            </> text ")"

and showClause (ChakraMatchClause (patt, exprList)) =
    text "|"
    <+> showPattern patt
    <+> text "->"
    <+> showExprList exprList

and showExpr (expr: ChakraExpr) =
    match expr with
    | ChakraVar (_, id) -> text (createFullId id)
    | ChakraNumber (_, num) -> text (sprintf "%M" num)
    | ChakraSymbol (_, s) -> text (sprintf "#%s" s)
    | ChakraString (_, s) -> text (sprintf "\"%s\"" s)
    | ChakraTuple (_, []) -> text "()"
    | ChakraTuple (_, [ item ]) ->
        if shouldStayOnOneLine item 0 then
            TextOp "( " <&> showExpr item <&> TextOp " )"
        else
            text "("
            <&> (block 1 (List.map (opWithComma << showExpr) [ item ]))
            <&> line
            <&> text ")"
    | ChakraTuple (span, items) ->
        text "("
        <&> (block 1 (List.map (opWithComma << showExpr) items))
        <&> line
        <&> text ")"
    | ChakraStruct (span, { Fields = fields; Spread = spread }) ->
        let fieldOp { Name = name; Value = value } =
            if shouldPunExpr name value then
                text name
            else
                text name <+> text "=" <+> showExpr value

        let spreadOps =
            match spread with
            | Some (_, var) -> [ text var ]
            | _ -> []

        match fields with
        | [] ->
            text "%("
            <&> List.reduce (<+>) spreadOps
            <&> text ")"
        | [ field ] when shouldStayOnOneLine field.Value 0 -> text "%(" <+> fieldOp field <+> text ")"
        | _ ->
            text "%("
            <&> block
                    1
                    (List.concat [ (List.map (opWithComma << fieldOp) fields)
                                   spreadOps ])
            <&> line
            <&> text ")"
    | ChakraList (span, { Items = []; Spread = None }) -> text "[" <&> text "]"
    | ChakraList (span, { Items = []; Spread = Some (_, var) }) ->

        TextOp "[ "
        <&> (text (sprintf "...%s" var))
        <&> TextOp " ]"
    | ChakraList (span, { Items = [ item ]; Spread = spread }) ->
        let itemOp = showExpr item

        let spreadOps =
            match spread with
            | Some (_, var) -> [ text (sprintf "...%s" var) ]
            | _ -> []

        text "["
        <&> (block 1 (List.map (opWithComma) (itemOp :: spreadOps)))
        <&> line
        <&> text "]"
    | ChakraList (span, { Items = items; Spread = spread }) ->
        let spreadOps =
            match spread with
            | Some (_, var) ->
                let op = text (sprintf "...%s" var)
                [ op ]
            | _ -> []

        text "["
        <&> (block
                 1
                 (List.map
                     (opWithComma)
                     (List.concat [ List.map showExpr items
                                    spreadOps ])))
        <&> line
        <&> text "]"
    | ChakraMap (span, { Pairs = pairs; Spread = spread }) ->
        let pairOp { Key = name; Value = value } =
            showExpr name <+> text " = "
            <&> showExpr value
            <&> text ","

        let spreadOps =
            match spread with
            | Some (_, var) -> [ text var ]
            | _ -> []

        match pairs with
        | [] ->
            text "%["
            <&> List.reduce (<+>) spreadOps
            <&> text "]"
        | [ pair ] when shouldStayOnOneLine pair.Value 0 -> text "%[" <&> pairOp pair <&> text "]"
        | _ ->
            text "%["
            <&> block
                    1
                    (List.concat [ (List.map pairOp pairs)
                                   spreadOps ])
            <&> line
            <&> text "]"
    | ChakraLambda (span, l) ->
        let argsOp =
            List.reduce (<&>) (List.map (opWithComma << text) l.Args)

        text "{ ("
        <&> argsOp
        <&> text ") ->"
        <&> nest 1 (showExprList l.Body)
        <&> text " }"
    | ChakraMatchExpr (_, ChakraMatch (lit, clauses)) ->
        showExpr lit <+> text "?"
        <&> block 0 (List.map showClause clauses)
    | ChakraApplyExpr (_, app) -> showApply app
    | ChakraPipeExpr { Head = h; Tail = t } ->
        let tailFolder acc (_, app) =
            acc <&> text "> " <&> (showApply app) <&> line

        let tail = List.fold tailFolder line t

        (showExpr h) <&> tail
    | ChakraNativeExpr s -> TextOp(sprintf "$$NATIVE$$%s$$" s)

and showExprList (ChakraExprList (bs, expr)) =
    let bindingOps =
        (List.map (opWithNewline << showBinding) bs)

    let exprOpAsList = [ (showExpr expr) ]
    block 1 (List.concat [ bindingOps; exprOpAsList ])

and showBindingPattern (patt: ChakraBindingPattern) =
    match patt with
    | ChakraSimpleBindingPattern name -> TextOp(sprintf "%s =" name)
    | ChakraFunctionBindingPattern { Name = n; Args = a } -> TextOp(sprintf "%s(%s) =" n (String.concat ", " a))
    | ChakraComplexBindingPattern patt -> (showPattern patt) <+> text "="

and showBinding
    ({ Loc = _
       DocComment = optComment
       Pattern = patt
       ExprList = exprList })
    =
    (Option.defaultValue
        NilOp
        (Option.map
            (fun c ->
                (showComment { Content = c; IsDoc = true })
                <&> line)
            optComment))
    <&> showBindingPattern patt
    <+> showExprList exprList

and showBindingType (typ: ChakraImportBindingType) =
    match typ with
    | ChakraSimpleImportBinding name -> TextOp(sprintf "%s = " name)
    | ChakraDestructuredImportBinding bMap when bMap.Count <= 1 ->
        let (k, v) = List.head (Map.toList bMap)

        if k = v then
            text "%( " <&> text k <&> text " ) ="
        else
            text "%( "
            <&> text (sprintf "%s = %s" k v)
            <&> text " ) ="

    | ChakraDestructuredImportBinding bMap when bMap.Count > 1 ->
        let pairs =
            let pairOp (k, v) =
                if k = v then
                    text k
                else
                    text (sprintf "%s = %s" k v)

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
        <+> if info.Relative then
                TextOp(sprintf "/root/%s" info.Library)
            else
                TextOp(sprintf "./%s" info.Library)

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
    (List.fold (fun acc _ -> sprintf "%s%s" "	" acc) "" [ 1 .. i ])

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


let format path =
    let lines = System.IO.File.ReadAllLines(path)

    let parseResult =
        ParserLibrary.run (chakraModule "Test") (String.concat "\n" lines)

    match parseResult with
    | ParserLibrary.Success (p, _) -> printfn "%s" (pretty 80 (showModule p))
    | _ -> ParserLibrary.printResult parseResult
