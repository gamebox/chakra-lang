module UnifyTests

open NUnit.Framework
open TypeSystem
open Unify

let (.=.) (a: 'a) (b: 'a) =
    Assert.That((a = b))

let (.!=.) (a: 'a) (b: 'a) =
    Assert.That((a <> b))

[<SetUp>]
let Setup () =
    ()

let fooType = CustomType ("Foo", [])
let genA = GenericType "a"
let genB = GenericType "b"
let ss = [fooType, "a"]

[<Test>]
let ``String type is not substituted`` () = (subs ss (StringType)) .=. StringType
[<Test>]
let ``Number type is not substituted`` () = (subs ss (NumberType)) .=. NumberType

[<Test>]
let ``Generic Type is substituted correctly`` () = (subs ss genA) .=. fooType
[<Test>]
let ``List type is substituted correctly`` () = (subs ss (ListType genA)) .=. (ListType fooType)
[<Test>]
let ``Map type is substituted correctly`` () = (subs ss (MapType (StringType, genA))) .=. (MapType (StringType, fooType))
[<Test>]
let ``TupleType is substituted correctly`` () = (subs ss (TupleType [StringType; genA])) .=. (TupleType [StringType; fooType])
[<Test>]
let ``StructType is substituted correctly`` () = (subs ss (StructType ([("foo", StringType); ("bar", genA)], false))) .=. (StructType ([("foo", StringType); ("bar", fooType)], false))
[<Test>]
let ``FunctionType is substituted correctly`` () = (subs ss (FunctionType ([("foo", StringType); ("bar", genA)], genA))) .=. (FunctionType ([("foo", StringType); ("bar", fooType)], fooType))

[<Test>]
let ``Generic Type has no substitutions when generic not found`` () = (subs ss genB) .=. genB
[<Test>]
let ``List type has no substitutions when generic not found`` () = (subs ss (ListType genB)) .=. (ListType genB)
[<Test>]
let ``Map type has no substitutions when generic not found`` () = (subs ss (MapType (StringType, genB))) .=. (MapType (StringType, genB))
[<Test>]
let ``TupleType has no substitutions when generic not found`` () = (subs ss (TupleType [StringType; genB])) .=. (TupleType [StringType; genB])
[<Test>]
let ``StructType has no substitutions when generic not found`` () = (subs ss (StructType ([("foo", StringType); ("bar", genB)], false))) .=. (StructType ([("foo", StringType); ("bar", genB)], false))
[<Test>]
let ``FunctionType has no substitutions when generic not found`` () = (subs ss (FunctionType ([("foo", StringType); ("bar", genB)], genB))) .=. (FunctionType ([("foo", StringType); ("bar", genB)], genB))

let unifies ta tb = (unify ta tb) .=. []
let (.=^=.) = unifies
let (.=>=.) = unify

[<Test>]
let ``Two identical generics unify`` () = genA .=^=. genA
[<Test>]
let ``Two string types unify`` () = StringType .=^=.StringType
[<Test>]
let ``Two number types unify`` () = NumberType .=^=. NumberType
[<Test>]
let ``Two List(number) types unify`` () = (ListType NumberType) .=^=. (ListType NumberType)
[<Test>]
let ``Two identical map types unify`` () = (MapType (StringType, StringType)) .=^=. (MapType (StringType, StringType))
[<Test>]
let ``Two indentical tuple types unify`` () = (TupleType [StringType; NumberType]) .=^=. (TupleType [StringType; NumberType])
[<Test>]
let ``Two identical struct types unify`` () = (StructType ([("foo", StringType)], false)) .=^=. (StructType ([("foo", StringType)], false))
[<Test>]
let ``Two identical function types unify`` () = (FunctionType ([("foo", StringType); ("bar", StringType)], StringType)) .=^=. (FunctionType ([("foo", StringType); ("bar", StringType)], StringType))
[<Test>]
let ``Two identical custom types unify`` () =
    (CustomType ("Foo", [])) .=^=. (CustomType ("Foo", []))
    (CustomType ("Bar", [genA])) .=^=. (CustomType ("Bar", [genA]))
    (CustomType ("Bar", [StringType])) .=^=. (CustomType ("Bar", [StringType]))
[<Test>]
let ``'a and List(String) unify`` () =
    (GenericType "a") .=>=. (ListType StringType) .=. [((ListType StringType), "a")]
    (ListType StringType) .=>=. (GenericType "a") .=. [((ListType StringType), "a")]
[<Test>]
let ``'a and SomeCustomType(String) unify`` () =
    (GenericType "a") .=>=. (CustomType ("SomeCustomType", [StringType])) .=. [(CustomType ("SomeCustomType", [StringType]), "a")]
    (CustomType ("SomeCustomType", [StringType])) .=>=. (GenericType "a") .=. [(CustomType ("SomeCustomType", [StringType]), "a")]
[<Test>]
let ``'a and Map(StringType, NumberType) unify`` () =
    (GenericType "a") .=>=. (MapType (StringType, NumberType)) .=. [(MapType (StringType, NumberType), "a")]
[<Test>]
let ``'a and ( StringType, NumberType ) unify`` () =
    (GenericType "a") .=>=. (TupleType [StringType; NumberType]) .=. [(TupleType [StringType; NumberType], "a")]
[<Test>]
let ``'a and %( foo = String ) unify`` () =
    (GenericType "a") .=>=. (StructType ([("foo", StringType)], false)) .=. [(StructType ([("foo", StringType)], false), "a")]
[<Test>]
let ``'a and { (String) -> String } nuify`` () =
    (GenericType "a") .=>=. (FunctionType ([("foo", StringType)], StringType)) .=. [(FunctionType ([("foo", StringType)], StringType), "a")]

let nullSpan: (AST.Span) = { Start = { Line = 0; Column = 0 }; End = { Line = 0; Column = 0 } }
let stringL = AST.ChakraString (nullSpan, "")
let numL = AST.ChakraNumber (nullSpan, System.Decimal 0)
let varL v = AST.ChakraVar (nullSpan, (v, None))
let listL items = AST.ChakraList (nullSpan, { Items = items; Spread = None })
let tupL items = AST.ChakraTuple (nullSpan, items)
let stringT = StringType
let numT = NumberType
let listT t = ListType t
let appL n args = AST.ChakraApplyExpr (nullSpan, AST.ChakraApply ((n, []), args))
let structL fields = AST.ChakraStruct (nullSpan, { Fields = List.map (fun (n, e) -> { Loc = nullSpan; Name = n; Value = e }) fields; Spread  = None })

[<Test>]
let ``String literal should be inferred as a string`` () =
    (W 0 [] stringL) .=. (0, ([], stringT))

[<Test>]
let ``Number literal should be inferred as a number`` () =
    (W 0 [] numL) .=. (0, ([], numT))

[<Test>]
let ``Variable should be inferred as it's defined type if available`` () =
    (W 0 ["a", Type stringT] (varL "a")) .=. (0, ([], stringT))

let throws<'a when 'a :> exn> (expr, gamma) =
    let d = fun () -> W 0 gamma expr |> ignore
    NUnit.Framework.Assert.That(d, NUnit.Framework.Throws.InstanceOf<'a>())

let infers (expr, gamma) ty =
    let (_, (subs, t)) = (W 0 gamma expr)
    t .=. ty
let fails (expr, gamma) ty =
    let (_, (_, t)) = (W 0 gamma expr)
    t .!=. ty
    
let throwsExplicit (expr, gamma) (ex: 'a when 'a :> exn) =
    try
        W 0 gamma expr |> ignore
        NUnit.Framework.Assert.Fail ()
    with
        | :? NUnit.Framework.AssertionException -> reraise ()
        | e -> NUnit.Framework.Assert.AreEqual(e, ex)

[<Test>]
let ``List of string literals should be inferred as List(String)`` () =
    infers (listL [ stringL ], [])  (listT stringT)
[<Test>]
let ``List of number literals should be inferred as List(Number)`` () =
    infers (listL [ numL ], []) (listT numT)
[<Test>]
let ``List of string var expressions should be inferred as List(String)`` () =
    infers (listL [ varL "a"], ["a", Type stringT]) (listT stringT)
[<Test>]
let ``List of different string type expressions should be inferred as List(String)`` () =
    infers (listL [ stringL; varL "a"; stringL], ["a", Type stringT]) (listT stringT)
[<Test>]
let ``List of different string type expressions with a generic var should be inferred as List(String)`` () =
    infers (listL [ stringL; varL "a"; stringL], ["a", Type genA]) (listT stringT)
[<Test>]
let ``List of expressions of different types should not unify`` () =
    throws (listL [ stringL; varL "a"; stringL], ["a", Type numT])
[<Test>]
let ``List of expressions where there is an undefined var should not unify`` () =
    throws (listL [ stringL; varL "a"; stringL], [])

[<Test>]
let ``Apply literals should be inferred correctly`` () =
    infers (appL "test" [stringL], ["test", Type (FunctionType ([("a", StringType)], StringType))]) (StringType)
    infers (appL "test" [varL "a"], ["test", Type (FunctionType ([("a", StringType)], StringType)); ("a", Type genA)]) (StringType)
    infers (appL "test" [varL "a"], ["test", Type (FunctionType ([("a", genA)], genA)); ("a", Type StringType)]) (StringType)
    throwsExplicit (appL "test" [stringL], ["test", Type (FunctionType ([("a", StringType); ("b", NumberType)], StringType))]) (Unify Arity)
    throwsExplicit (appL "test" [stringL; numL], ["test", Type (FunctionType ([("a", StringType)], StringType))]) (Unify Arity)
    throwsExplicit (appL "test" [numL], ["test", Type (FunctionType ([("a", StringType)], StringType))]) (Unify Const)
    infers (appL "test" [stringL], ["test", Type (FunctionType ([("a", StringType)], StringType))]) (StringType)
    infers (appL "test" [stringL], ["test", Type (FunctionType ([("a", StringType)], StringType))]) (StringType)

[<Test>]
let ``Unit literal should be inferred as ()`` () =
    infers (tupL [], []) (TupleType [])

[<Test>]
let ``Tuple literal with one string literal should be inferred as (String)`` () =
    infers (tupL [stringL], []) (TupleType [stringT])

[<Test>]
let ``Tuple literal with one string var should be inferred as (String)`` () =
    infers (tupL [varL "a"], ["a", Type stringT]) (TupleType [stringT])

[<Test>]
let ``Tuple literal with a string literal and a number literal should be inferred as (String, Number)`` () =
    infers (tupL [stringL; numL], []) (TupleType [stringT; numT])

[<Test>]
let ``Struct literal with one string literal field should be inferred as %(one = String)`` () =
    infers (structL ["one", stringL], []) (StructType (["one", stringT], false))

[<Test>]
let ``Struct literal with one field assigned to a string var should be inferred as %(one = String)`` () =
    infers (structL ["one", varL "a"], ["a", Type stringT]) (StructType (["one", stringT], false))