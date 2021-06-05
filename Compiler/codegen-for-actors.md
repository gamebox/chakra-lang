# Code Generation for actors

## Main module

The main module of an application must conform to one two types - StatelessMainActor or StatefulMainActor.  Stateful is to be the default, but at the time of writing Stateless is the default.

A StatelessMainActor is of the type `%( init = { (Capabilities) -> ! } )`
A StatefulMainActor is of the type `%( init = { (Capabilities) -> (?state, !) }, receive = { (?state, ?msg) -> (?state, !) } )`

Since capabilities is a Known Type - no special affordances in code gen should be needed.

## Other Actors

Other actors have the type Actor which is `%( init = { (?init) -> (?state, !) }, receive = { (?state, ?msg) -> (?state, !) } )`.  Since the runtime has no way to know what type `?init`, `?state`, and `?msg` are, they must be demoted to a `i8*`(void* in C) and then bitcast inside the function.  These changes have to be done during code generation.  A receive function might look like this in LLVMIR "naturally" from code generation.

```chakra
= %( ... )

%( commands ) = /stdlib

; Other stuff

receive(state, msg) =
    (state, msg) ?
    | ( #on, #toggle ) -> (#off, commands.none())
    | ( #off, #toggle ) -> (#on, commands.none())
```

```llvm
define { i64, %struct.Envelope* }* @Main_Actor__receive(i64 %0, i64 %1) {
    switch i64 %0, label %12 [
        i64 100, label %3
        i64 101, label %4
    ]

3:
    br label %5

4:
    br label %5

5:
    %6 = phi i64 [ 101, %3 ], [100, %4 ]
    %7 = tail call %struct.Envelope* @Chakra_stdlib__commands__none()
    %8 = tail call dereferenceable_or_null(9) i8* @malloc(i64 9)
    %9 = bitcast i8* %8 to { i64, %struct.Envelope* }*
    %10 = getelementptr { i64, %struct.Envelope* }, { i64, %struct.Envelope* }* %9, i64 0, i32 0 ; pointer to result state
    store i64 %6, i64* %10, align 8
    %11 = getelementptr { i64, %struct.Envelope* }, { i64, %struct.Envelope* }* %9, i64 0, i32 1 ; pointer to result msg
    store %struct.Envelope* %7, %struct.Envelope** %11, align 8
    br label %12

12:
    %13 = phi { i64, %struct.Envelope* }* [ %9, %5], [ null, %2]
    ret { i64, %struct.Envelope* }* %13
}
```

We could insert some instructions to promote the args and demote the fields of the return value. But since these functions _could_ also be exported and used by Chakra code, we want to take a different tack - one that has a small cost.  We instead don't change the raw function, we instead create a NEW function and add it as a field on a new data structure Actor which can not be created except through one of the function in stdlib's `actor` module.  Under the hood the outputted structure for that actor type has fields for init and receive that are wrapped similar to this - this is what is passed to the runtime.

```lvm
define %struct.TurnResult* @test_other__receive_ACTOR(i8* %0, i8* %1) {
    %3 = bitcast i8* %0 to i64*
    %4 = load i64, i64* %3
    %5 = bitcast i8* %0 to i64*
    %6 = load i64, i64* %3
    %7 = tail call { i64, %struct.Envelope* }* @test_other__receive(i64 %4, i64 %6)
    %8 = tail call dereferenceable_or_null(16) i8* @malloc(i64 16)
    %9 = bitcast i8* %8 to %struct.TurnResult*
    ; lower into generic type
    ret %struct.TurnResult* %9
}
```

The functions are parameterized based on the generics, and a version is generated for each version.  This means we need to also create a generalized system in IR State for handling generic functions as well.  Obviously this is needed for stdlib functions for `map`s and `list`s and even functions(composition functions).  Like, for instance, `/stdlib.list.map`

```chakra
map(fn, list) =
    | [] -> list
    | [ head, ...rest ] ->
        tail = map(fn, rest)
        [ fn(head), ...tail ]
```

This has the signature `{ ({ (?a) -> ? b }, [?a]) -> [?b] }` so it would be a prototype with the name template of `Chakra_stdlib__list__map__{?a}.{?b}` where the `{}`s denote a template slot, so at an applyee that instantiates this prototype with `?a` = `str` and `?b` = `num`, the name would be `Chakra_stdlib__list__map__str.num`.  So when we encounter a function with generic type parameters, we store a prototype with the root name(string), and the template parts(list of strings), as well as a list of instantiations(map of name to a map of generic id to concrete type)