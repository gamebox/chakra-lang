module ChakraFparsec

open System
open FParsec
open AST

let withSpan (p: Parser<'a, unit>): Parser<Span * 'a, unit> =
    getPosition
    .>>.? p
    .>>.? getPosition
    |>> (fun ((pos1, r), pos2) ->
        let (span: Span) =
                { Start = { Line = pos1.Line |> int
                            Column = pos1.Column |> int }
                  End = { Line = pos2.Line |> int
                          Column = pos2.Column |> int } }
        (span, r))


let flattenTuple (a, (b, c)) = (a, b, c)

let tossRight (l, _) = l

let charListToStr charList = String(List.toArray charList)


(* Create Refs *)

let chakraPattern, chakraPatternRef =
    createParserForwardedToRef<ChakraPattern, unit> ()

// let chakraLiteral, chakraLiteralRef =
//     createParserForwardedToRef<ChakraLiteral> ()

let chakraExpr, chakraExprRef =
    createParserForwardedToRef<ChakraExpr, unit> ()

let chakraExprList, chakraExprListRef =
    createParserForwardedToRef<ChakraExprList, unit> ()

let moduleName = ref ""

let chakraTypeExpr, chakraTypeExprRef =
    createParserForwardedToRef<ChakraTypeExpr, unit> ()

(* Comments *)

let semi = pstring ";" <?> "semicolon"

let notNewline =
    satisfy (fun c -> c <> '\n')

let newline = pchar '\n'

(*
    A line comment looks like this:

    ```chakra
    ```
    x = 1 ; This is a comment

    It starts with a semicolon followed by a space, and
    continues to the end of the line
*)
let lineComment =
    semi >>? many notNewline .>>?newline >>% '\n'
    <?> "Line comment"

let cleanLine (s: string) = s.TrimStart([| ' ' |])

(*
    A doc comment looks like this:

    ```chakra
    ;; This is a doc comment
    ;;
    ;; It will appear in generated documentation for this module,
    ;; as well as in intellisense documentation.
    x = 1
    ```

    Any number of such lines may appear in sequence at the top of a module, or
    above any top level binding
*)
let docComment =
    let docCommentLine =
        (semi .>>.? semi) >>? many notNewline .>>? newline
        |>> charListToStr
        |>> cleanLine

    many1 docCommentLine
    |>> fun lines ->
            { IsDoc = false
              Content = String.concat "\n" lines }
    <?> "Doc comment"

let lineWhitespace =
    skipManySatisfy (fun c -> Char.IsWhiteSpace c && c <> '\n')

let deadspace =
    let eolWithComment = lineWhitespace >>? lineComment
    let eol = lineWhitespace >>? newline
    let deadspaceSegments = choice [ eolWithComment; eol ]
    
    many deadspaceSegments
    .>>? lineWhitespace
    >>? skipManySatisfy (isAnyOf [| ' '; '\t' |])
    <?> "deadspace"

let deadOrWhitespace = deadspace <|> spaces1

(* Basic punctuation *)

let altSigil = pchar '%'

let leftParen =
    pchar '(' .>>? opt deadOrWhitespace
    <?> "left parenthesis"

let structStart =
    altSigil .>>?leftParen .>>?opt deadOrWhitespace
    <?> "start of struct"

let rightParen =
    spaces >>? pchar ')' <?> "right parenthesis"

let leftBracket =
    pchar '[' .>>?opt deadOrWhitespace
    <?> "left bracket"

let mapStart =
    altSigil .>>?pchar '[' .>>?opt deadOrWhitespace
    <?> "start of map"

let rightBracket =
    spaces >>? pchar ']' <?> "right bracket"

let leftCurly =
    pchar '{' .>>?opt deadOrWhitespace
    <?> "left curly brace"

let rightCurly =
    spaces >>? pchar '}' <?> "right curly brace"

let matchOperator =
    spaces1 >>? pchar '?' .>>?deadspace
    <?> "match operator"

let (patternSep: Parser<char, unit>) =
    pchar '|' .>>?spaces1 <?> "pattern separator"

let patternArrow =
    spaces1 >>? pstring "->" .>>?deadOrWhitespace
    <?> "pattern arrow"

let rawEqual = pchar '=' <?> "equal sign"

let equal =
    spaces1 >>? pchar '=' .>>?deadOrWhitespace
    <?> "bind operator"

let pipe =
    pchar '|' .>>.? spaces1 <?> "pattern separator"

let arrow =
    spaces1 .>>.? pstring "->" .>>?deadOrWhitespace
    <?> "pattern arrow"

let questionMark =
    spaces1 .>>.? pchar '?' .>>.? deadspace
    <?> "match separator"

(* Literals *)

(*
    An identifier must follow this regex:
    ([A-Za-z]+[a-z]* )(\-{1,2}[A-Za-z]+[a-z]* )*[\?\!\*]{0,1}
*)

let pBaseIdentifier =
    let isUpperOrLowerAlphaASCII c =
        Char.IsLetter c && Convert.ToInt32(c) < 128

    let isLowerAlphaASCII c =
        isUpperOrLowerAlphaASCII c && Char.IsLower c

    let upperOrLowerAlpha =
        satisfy isUpperOrLowerAlphaASCII

    let lowerAlpha =
        satisfy isLowerAlphaASCII

    let dash = pchar '-'

    let segment =
        upperOrLowerAlpha .>>.? many lowerAlpha

    let questionOrBangOrStar =
        anyOf [ '?'; '!'; '*' ]

    let convertToIdentifier (((firstAlpha, restOfFirstSegment), otherSegments), maybeSign) =
        let firstSegment =
            charListToStr (firstAlpha :: restOfFirstSegment)

        let segmentPart =
            otherSegments
            |> List.map (fun (head, (firstLetter, rest)) -> sprintf "%c%c%s" head firstLetter (charListToStr rest))
            |> List.fold (+) ""

        let signPart =
            match maybeSign with
            | Some ch -> String([| ch |])
            | None -> ""

        firstSegment + segmentPart + signPart

    let laterSegment = dash .>>.? segment
    
    segment
    .>>.? many laterSegment
    .>>.? opt questionOrBangOrStar
    |>> convertToIdentifier
    // <?> "identifier /([A-Za-z]+[a-z]*)(\-[A-Za-z]+[a-z]*)*[?!\*]{0,1}/"

let pVarTypeItem =
    pBaseIdentifier .>> (spaces1 .>> pchar '=' .>> spaces1) .>>. chakraTypeExpr

let pAnnotation =
    (pchar ':' .>> spaces1 >>. chakraTypeExpr)

let pArgItemWithAnno =
    pBaseIdentifier .>>. opt pAnnotation

let pVar =
    pBaseIdentifier
    .>>.? opt (many (pchar '.' >>? pBaseIdentifier))

let chakraVar =
    pVar |> withSpan |>> ChakraVar
    <??> "Var"

let (|>?) opt f =
    match opt with
    | None -> ""
    | Some x -> f x

let pNumber =
    pfloat
    |>> (fun f -> Decimal f)
    <?> "number"

let pValidChars =
    let unescapedChar =
        satisfy (fun ch -> ch <> '\\' && ch <> '\"')

    let escapedChar =
        [ ("\\\"", '\"') // quote
          ("\\\\", '\\') // reverse solidus
          ("\\/", '/') // solidus
          ("\\b", '\b') // backspace
          ("\\f", '\f') // formfeed
          ("\\n", '\n') // newline
          ("\\r", '\r') // cr
          ("\\t", '\t') ] // tab
        |> List.map (fun (toMatch, result) -> pstring toMatch >>% result)
        |> choice
        <?> "escaped char"

    let unicodeChar =
        let backslash = pchar '\\'
        let uChar = pchar 'u'

        let hexdigit =
            anyOf ([ '0' .. '9' ] @ [ 'A' .. 'F' ] @ [ 'a' .. 'f' ])

        let convertToChar (((h1, h2), h3), h4) =
            let str = sprintf "%c%c%c%c" h1 h2 h3 h4

            Int32.Parse(str, Globalization.NumberStyles.HexNumber)
            |> char

        backslash >>? uChar >>? hexdigit
        .>>.? hexdigit
        .>>.? hexdigit
        .>>.? hexdigit
        |>> convertToChar
        <?> "unicode character"

    unescapedChar <|> escapedChar <|> unicodeChar
    <?> "valid string characters"

let chakraNumber = pNumber |> withSpan |>> ChakraNumber <?> "number"

let pCString =
    between (pchar '\"') (pchar '\"') (many pValidChars)
    |>> charListToStr

let chakraString =
    pCString |> withSpan |>> ChakraString <?> "string"

let pIgnore = pchar '_' <?> "ignore"

let (pRest: Parser<string, unit>) = pstring "..." <?> "rest"

let containerItemSeparator = pchar ','

let emptyContainer start end' = start .>>? end'

let container start end' (item: Parser<'a, unit>): Parser<'a list, unit> =
    let items =
        sepEndBy1 item (containerItemSeparator .>>? deadOrWhitespace)
        .>>?opt containerItemSeparator
        .>>? opt deadOrWhitespace

    between start end' items

let spread =
    pstring "..." >>? pBaseIdentifier |> withSpan

let (<!>) (p: Parser<_,_>) (label: string) : Parser<_,_> =
    p
    // let label' = label.Replace(' ', '-')
    // fun stream ->
    //     printfn """<%s> <!-- %A -->""" (label') stream.Position
    //     let reply = p stream
    //     printfn "</%s> <!-- %A %A -->" label' reply.Status stream.Position
    //     reply

let spreadableContainer start end' item' =
    let item = item' <!> "item"
    let sep = containerItemSeparator .>>? deadOrWhitespace <!> "sep"
    let items item =
        item
        .>>. many (sep >>? item)
        |>> fun (head, tail) -> head::tail
        .>>. preturn None
        <!> "items"

    let itemsWithRest item =
        item
        .>>. many (sep >>? item)
        |>> fun (head, tail) -> head::tail
        .>>. ((sep >>? spread) |>> Some <!> "maybe spread")
        <!> "items"

    let items' = choice [ attempt (itemsWithRest item'); (items item') ]

    between start end' items'

let pTuple item =
    let emptyTuple =
        (emptyContainer leftParen rightParen) >>% []

    let nonEmptyTuple = (container leftParen rightParen item)

    emptyTuple <|> nonEmptyTuple

let chakraTuple =
    pTuple chakraExpr |> withSpan |>> ChakraTuple
    <?> "tuple"

let pStruct
    start
    (value: Parser<'a, unit>)
    (pairConstructor: (Span * (String * 'a) -> 'b))
    (punnedConstructor: (Span * string) -> 'b)
    =
    let structPair =
        let regularPair =
            pBaseIdentifier .>>? equal .>>.? value |> withSpan
            |>> pairConstructor


        let punned =
            pBaseIdentifier |> withSpan |>> punnedConstructor

        (regularPair <|> punned)

    spreadableContainer start rightParen structPair

let chakraStruct =
    let punnedPairConstructor (span, id) =
        { Loc = span
          Name = id
          Value = ChakraVar(span, (id, None)) }

    let createPair (span, (name, value)) =
        { Loc = span
          Name = name
          Value = value }

    let createStruct (span, (pairs, spread)) =
        ChakraStruct(span, { Fields = pairs; Spread = spread })

    pStruct structStart chakraExpr createPair punnedPairConstructor
    |> withSpan
    |>> createStruct
    <?> "struct"

let pCList item =
    let emptyList =
        emptyContainer leftBracket rightBracket
        >>% ([], None)
        <?> "empty list"

    let nonEmptyList =
        spreadableContainer leftBracket rightBracket item
        <?> "non empty list"
        <!> "non empty list"

    (attempt nonEmptyList <|> emptyList)

let chakraList =
    let createList (span, (items, spread)) =
        ChakraList(span, { Items = items; Spread = spread })

    pCList chakraExpr |> withSpan |>> createList
    <?> "list"

let pMap createPair key value =
    let mapPair =
        (key .>>? equal) .>>.? value |> withSpan
        |>> createPair

    let nonEmptyMap =
        spreadableContainer mapStart rightBracket mapPair

    let emptyMap =
        emptyContainer mapStart rightBracket
        >>% ([], None)

    emptyMap <|> nonEmptyMap <?> "map"

let chakraMap =
    let createPair (span, (key, value)) =
        { Loc = span; Key = key; Value = value }


    let createMap (span, (pairs, spread)) =
        ChakraMap(span, { Pairs = pairs; Spread = spread })

    pMap createPair chakraExpr chakraExpr |> withSpan
    |>> createMap

let chakraLambda =
    let createRecord (span, ((args, ret), body)) =  ChakraLambda (span, { Args = args; Ret = ret; Body = body })
    let args: Parser<(string * ChakraTypeExpr option) list, unit> = pTuple pArgItemWithAnno

    let contents =
        args
        .>>. opt pAnnotation
        .>>? arrow
        .>>.? chakraExprList

    between leftCurly rightCurly (args .>>. opt pAnnotation .>>? arrow .>>.? chakraExprList)
    |> withSpan
    |>> createRecord
    <?> "Lambda"



(* ## Types *)



let pTypeIdent =
    let segment =
        asciiUpper
        .>>. many1 asciiLower
        |>> List.Cons
        |>> charListToStr
    
    many1 segment
    |>> (fun ss -> String.Join("", ss))

let pTypeVar =
    many1 asciiLower
    |>> charListToStr

let pTypeConstructor =
    pTypeIdent
    .>>. container leftParen rightParen chakraTypeExpr

let stringDecl =
    pstring "\"\""
    |> withSpan
    |>> fst
    |>> StringDecl
    <?> "String type declaration"

let numberDecl =
    pchar '#'
    |> withSpan
    |>> fst
    |>> NumberDecl
    <?> "Number type declaration"

//   | TupleDecl of Span * items: ChakraTypeExpr list
let tupleDecl =
    container leftParen rightParen chakraTypeExpr
    |> withSpan
    |>> TupleDecl
    <?> "Tuple type declaration"
//   | StructDecl of Span * fields: (string * ChakraTypeExpr) list
let structDecl =
    container structStart rightParen pVarTypeItem
    |> withSpan
    |>> StructDecl
    <?> "Struct type declaration"
//   | ListDecl of Span * ChakraTypeExpr
let listDecl =
    leftBracket
    .>> spaces
    >>. chakraTypeExpr
    .>> spaces
    .>> rightBracket
    |> withSpan
    |>> ListDecl
    <?> "List type declaration"
//   | MapDecl of Span * ChakraTypeExpr * ChakraTypeExpr
let mapDecl =
    mapStart
    >>. chakraTypeExpr
    .>> (spaces1 .>> pchar '=' .>> spaces1)
    .>>. chakraTypeExpr
    .>> rightBracket
    |> withSpan
    |>> flattenTuple
    |>> MapDecl
    <?> "Map type declaration"
//   | GenericDecl of Span * string
let genericDecl =
    pTypeVar
    |> withSpan
    |>> GenericDecl
    <?> "List type declaration"
//   | CustomDecl of Span * string * (ChakraTypeExpr list)
let noArgConstructor () : Parser<string * 'a list, unit> =
    pTypeIdent
    |>> (fun name -> (name, []))

let customDecl =

    let constructor = attempt pTypeConstructor <|> noArgConstructor ()
    
    constructor
    |> withSpan
    |>> flattenTuple
    |>> CustomDecl
    <?> "Custom type declaration"
//   | FunctionDecl of Span * ChakraTypeExpr list * ChakraTypeExpr
let functionDecl =
    let args = container leftParen rightParen chakraTypeExpr

    let contents = args .>> (spaces1 .>> pstring "->" .>> spaces1) .>>. chakraTypeExpr

    between leftCurly rightCurly contents
    |> withSpan
    |>> flattenTuple
    |>> FunctionDecl


chakraTypeExprRef.Value <- choiceL
        [ stringDecl
          numberDecl
          tupleDecl
          structDecl
          listDecl
          mapDecl
          genericDecl
          customDecl
          functionDecl ]
        "Type declaration"

let pTypeDefHead =
    let withArgs =
        pTypeIdent
        .>>. container leftParen rightParen pTypeVar
    let noArgs =
        pTypeIdent
        |>> (fun id -> (id, []))

    (attempt withArgs) <|> noArgs


// Example:
// Maybe(a) =
//     | Some(value: a)
//     | None
let pChakraCustomType =

    let constructors = 
        let withArgConstructor =
            (pTypeConstructor |>> (fun (n, args) -> { Name = n; Args = args }))
        let noArgConstructor =
            (pTypeIdent |>> (fun id -> { Name = id; Args = []}))
        many1 (pchar '|' .>> spaces1 >>. (attempt withArgConstructor <|> noArgConstructor) .>> deadspace) 
    
    pTypeDefHead
    .>> equal
    .>>. constructors
    |>> (fun ((name, args), constructors) -> { Name = name; Args = args; Constructors = constructors })
    |> withSpan
    |>> ChakraCustomType


// Example:
// ResultList(a) = List(Result(a, Error))
let pChakraTypeAlias =
    pTypeDefHead
    .>> equal
    .>>. chakraTypeExpr
    |>> (fun ((name, args), ty) -> { Name = name; Args = args; Ty = ty })
    |> withSpan
    |>> ChakraTypeAlias


let chakraTypeDef =
    (attempt pChakraCustomType) <|> pChakraTypeAlias


(* Patterns *)

let cpIgnore =
    pIgnore |> withSpan |>> tossRight |>> CPIgnore

let cpVar = pBaseIdentifier |> withSpan |>> CPVar

let cpNumber = pNumber |> withSpan |>> CPNumber

let cpString = pCString |> withSpan |>> CPString

let cpTuple =
    pTuple chakraPattern |> withSpan |>> CPTuple

let cpStruct =
    let punnedPairConstructor (span, id) =
        { Loc = span
          Name = id
          ValuePattern = CPVar(span, id) }

    let createPair (span, (name, value)) =
        { Loc = span
          Name = name
          ValuePattern = value }

    let createStruct (span, (pairs, (rest: bool))) =
        CPStruct(span, { Fields = pairs; Rest = rest })

    let item =
        let regularPair =
            pBaseIdentifier .>>? equal .>>.? chakraPattern
            |> withSpan
            |>> createPair
            <?> "regular struct pair"


        let punned =
            pBaseIdentifier |> withSpan
            |>> punnedPairConstructor
            <?> "punned struct pair"

        (regularPair <|> punned) <?> "struct pair"

    let sep =
        containerItemSeparator .>>? deadOrWhitespace
        <?> "struct pair separator"

    let form =
        (structStart >>? sepEndBy1 item sep .>>? rightParen
         |>> fun is -> (is, false)
         <?> "regular struct")
        <|> (structStart >>? sepEndBy1 item sep
             .>>?sep
             .>>?(pstring "...)" <?> "rest operator")
             |>> (fun is -> (is, true))
             <?> "struct with rest")

    form |> withSpan |>> createStruct <?> "struct"

let cpList =
    let createList (span, (items, spread)) =
        CPList(span, { Items = items; Rest = spread })

    pCList chakraPattern |> withSpan |>> createList

let cpMap =
    let createPair (span, (key, value)) =
        { Loc = span
          KeyPattern = key
          ValuePattern = value }

    let createMap (span, (pairs, spread)) =
        CPMap(span, { Pairs = pairs; Rest = spread })

    pMap createPair chakraPattern chakraPattern
    |> withSpan
    |>> createMap

let cpCustom =
    let pTypeConstructor =
        pTypeIdent
        .>>. container leftParen rightParen chakraPattern
    let constructor = attempt pTypeConstructor <|> noArgConstructor ()

    constructor
    |>> (fun (name, ps) -> { Constructor = name; Args = ps }) 
    |> withSpan
    |>> CPCustom


(* Expressions *)


let chakraOrderedApply =
    pVar
    .>>.? pTuple chakraExpr
    |>> (fun ((root, maybePath), exprs) -> ChakraApply ((root, Option.defaultValue [] maybePath), exprs))
    <?> "ordered application"

let chakraNamedApply =
    let pc (span, id) =
        (span, (id, ChakraVar(span, (id, None))))

    let createPair (span, (name, value)) = (span, (name, value))
    let createApply ((name, maybePath), (pairs, spread)) = ChakraNamedApply((name, Option.defaultValue [] maybePath), pairs)

    pVar
    .>>.? pStruct leftParen chakraExpr id pc
    |>> createApply
    <?> "named application"

let chakraApply =
    chakraOrderedApply <|> chakraNamedApply
    <??> "function applicationS"

let chakraMatchClause =
    // We need to be able to parse other forms to make literals
    // The basic literals: strings, numbers, and symbols
    // Tuples like: ( a b _ )
    //
    // Structs like:
    // %(
    //     a = b
    //     c
    // ..)
    //
    // Maps like:
    // %[
    //     %( foo = "bar" ) = x ; Destructuring keys
    // ...] ; May have other keys
    // or like:
    // %[
    //    y = x ; where y is bound already
    // ...]
    // or like:
    // %[
    //    "foo" = bar
    // ] ; exact form
    //
    // Lists like:
    // [ a _ c ...rest ] ; binding to some elements, ignoring others, collecting the tail
    pipe >>? (chakraPattern <!> "clause pattern") .>>?arrow
    .>>.? (chakraExprList <!> "clause expr list")
    |>> ChakraMatchClause
    <?> "match clause"
    <!> "match clause"

let chakraApplyExpr =
    chakraApply |> withSpan |>> ChakraApplyExpr
    <??> "function application"

let matchHeadExpr =
    choice [ attempt chakraApplyExpr <!> "match head apply"
             chakraVar <!> "match head var" ]
    <?> "Match head expression"
    <!> "match head"

let matchHead = matchHeadExpr .>>? questionMark
let chakraMatch =
    matchHead
    .>>.? (sepEndBy1 chakraMatchClause deadspace <!> "clauses")
    |>> ChakraMatch
    <?> "match"
    <!> "match"

let pPipe = pchar '>'

let chakraPipe =
    let pipeHeadExpr =
        choiceL
            [ attempt chakraApplyExpr
              chakraVar
              chakraNumber
              chakraString
              chakraList
              chakraMap
              chakraLambda ]
            "pipe head"

    let pipeStep =
        pPipe
        >>? pchar ' '
        >>? withSpan chakraApply

    pipeHeadExpr
    .>>? deadspace
    .>>.? sepEndBy1 pipeStep deadspace
    |> withSpan
    |>> fun (span, (head, tail)) -> { Loc = span; Head = head; Tail = tail }

let chakraBindingPattern =
    let simple =
        pBaseIdentifier |>> ChakraSimpleBindingPattern
        <?> "simple binding pattern"

    let func =
        pBaseIdentifier
        .>>.? container leftParen rightParen pArgItemWithAnno
        .>>. opt pAnnotation
        |>> fun ((name, args), ret) -> ChakraFunctionBindingPattern { Name = name; Ret = ret; Args = args }
        <?> "function binding pattern"

    let complex =
        chakraPattern |>> ChakraComplexBindingPattern
        <?> "destructured binding pattern"

    func <|> simple <|> complex <?> "binding pattern"

let rec chakraBinding =
    let createBinding (s, (b, e)) =
        { Loc = s
          Pattern = b
          ExprList = e
          DocComment = None }

    chakraBindingPattern .>>? equal .>>.? chakraExprList
    |> withSpan
    |>> createBinding
    <??> "Binding"

let chakraMatchExpr =
    chakraMatch |> withSpan |>> ChakraMatchExpr
    <??> "match expression"

let chakraPipeExpr =
    chakraPipe |>> ChakraPipeExpr
    <?> "Pipe expression"

(* Imports *)

let pRootImport =
    pstring "/root" >>? pchar '/' >>? pBaseIdentifier
    <??> "Root import"

let pPackageImport =
    pchar '/' >>? pBaseIdentifier <??> "Package import"

let pRelativeImport =
    pstring "./" >>? pBaseIdentifier
    <??> "Relative import"

let pImportDestructuring =
    container structStart rightParen pBaseIdentifier
    <?> "Import destructuring"

let chakraImport =
    let simple =
        pBaseIdentifier |>> ChakraSimpleImportBinding

    let destructured =
        let addBinding map binding = Map.add binding binding map

        let createBindings bindings =
            List.fold addBinding (Map.empty) bindings

        pImportDestructuring
        |>> (createBindings >> ChakraDestructuredImportBinding)

    let binding = simple <|> destructured

    let rootImport (typ, string) =
        ChakraLocalImport
            { Library = string
              Typ = typ
              Relative = false }

    let relativeImport (typ, string) =
        ChakraLocalImport
            { Library = string
              Typ = typ
              Relative = true }

    let packageImport (typ, string) =
        ChakraPackageImport
            { PackageName = sprintf "%s" string
              Typ = typ }

    let eq = (pchar ' ' .>>? pchar '=' .>>? pchar ' ')

    (binding .>>? eq .>>.? pRootImport |>> rootImport)
    <|> (binding .>>? eq .>>.? pPackageImport
         |>> packageImport)
    <|> (binding .>>? eq .>>.? pRelativeImport
         |>> relativeImport)
    <?> "import"

(* Modules *)

let chakraModuleDef =
    let tupleLike =
        container structStart rightParen pBaseIdentifier

    rawEqual .>>?pchar ' ' >>? tupleLike
    <??> "Module export definition"

let chakraModule modName =
    moduleName.Value <- modName

    let possibleImportSection =
        sepEndBy chakraImport deadspace <??> "imports"

    let buildModule ((((docComment, exports), imports), tys), bindings) =
        // let comment =
        //     match c with
        //     | None -> None
        //     | Some ({ Content = com }) -> Some com

        let docCommentString =
            Option.map (fun c -> c.Content) docComment

        { DocComments = docCommentString
          Exports = exports
          Bindings = bindings
          Imports = imports
          Types = tys }

    let topLevelTypeDefs =
        let b =
            opt docComment .>>.? chakraTypeDef
            |>> snd
            <??> "type def"

        sepEndBy b (deadspace)

    let topLevelBindings =
        let content ({ Content = c; IsDoc = _ }) = c

        let createBinding (optComment, binding) =
            { binding with
                  DocComment = Option.map (content) optComment }

        let b =
            opt docComment .>>.? chakraBinding
            |>> createBinding
            <??> "top level binding"

        sepEndBy1 b (deadspace)

    (opt docComment) <??> "Documentation comments"
    .>>. chakraModuleDef <??> "Module definition"
    .>>? deadspace
    .>>.? possibleImportSection <??> "import section"
    .>>? opt deadspace
    .>>.? topLevelTypeDefs
    .>>? opt deadspace
    .>>.? topLevelBindings <??> "top level bindings"
    .>>? eof
    |>> buildModule



(* Metadata *)



let chakraMetdata =
    moduleName.Value <- "METADATA"

    let metadataExpr =
        choice [ chakraNumber
                 chakraString
                 chakraList
                 chakraMap ]

    let metadataBinding =
        pBaseIdentifier .>>?equal .>>.? metadataExpr
        <?> "Metadata binding"

    let meta =
        rawEqual .>> spaces1
        >>? structStart
        >>? sepEndBy1 metadataBinding ((pstring "," .>>? spaces) .>>? deadOrWhitespace)
        .>>? opt ((pstring "," .>>? spaces) .>>? deadOrWhitespace)
        .>>? rightParen
        |>> Map
        <?> "Metadata file"

    meta

(* Set refs *)

chakraPatternRef.Value <- choiceL
        [ cpNumber
          cpString
          cpTuple
          cpList
          cpStruct
          cpMap
          cpIgnore
          cpCustom
          cpVar ]
        "pattern"

chakraExprRef.Value <- choiceL
        [ chakraNumber <!> "chakraNumber"
          chakraString <!> "chakraString"
          chakraTuple <!> "chakraTuple"
          chakraList <!> "chakraList"
          chakraStruct <!> "chakraStruct"
          chakraMap <!> "chakraMap"
          chakraLambda <!> "chakraLambda"
          chakraPipeExpr <!> "chakraPipeExpr"
          chakraMatchExpr <!> "chakraMatchExpr"
          attempt chakraApplyExpr <!> "chakraApplyExpr"
          chakraVar <!> "chakraVar" ]
        "expression"
    <??> "expression"
    <!> "expression"

let elBinding =
    chakraBinding
    .>>? deadspace
    <??> "expression list bindings"

chakraExprListRef.Value <-
    let bindingsThenExpr = many (attempt elBinding) .>>.? chakraExpr <!> "bindings then expr"

    bindingsThenExpr
    .>>? deadspace
    |>> ChakraExprList
    <!> "new-school expr list parser"
    <??> "expression list"

