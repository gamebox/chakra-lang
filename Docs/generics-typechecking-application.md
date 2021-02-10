# Typechecking Function Application with generics

An example of applying a function that takes a function parameter that has generic type parameters.

```chakra
;; add : { (num, num) -> num }
;; add1 : { (num) -> num }
add1 = add(1)
;; map : { ( { (?a) -> ?b}, [ ?a ]) -> ? [ ?b ] }
map(add1, [ 1, 2, 3 ])
```

Let's work through this step by step:

1. Find the type of the binding `map`
2. It is `{ ( { (?a) -> ?b }, [ ?a ]) -> ? [ ?b ] }`
3. Collect generics from the parameters and the return type
4. Here the set of generics are `?a` and `?b`
5. Now, to instantiate the generics:
   1. Look at the first argument which `{ (num) -> num }`
   2. Compare the slots of the argument to the slots of the parameter
      1. Slot 1 is the argument `num` which lines up with `?a` in the parameter
         1. So `?a` is instantiated to be `num`
      2. Slot 2 is the return type `num` which lined up with `?b` in the parameter
         1. So `?b` is instantiated to be `num`
   3. Look at the second argument, which is a `[ num ]` and compare to the second parameter `[ ?a ]`
   4. Compare the slots of the argument to the slots of the parameter
      1. Slot 1 is type of the list, which here is `num` and lines up with the instantiated type of `?a` in the parameter
         1. This matches the instantiated type, so succeed
   5. The fully instantiated type of the function binding is `{ ( { (num) -> num }, [ num ]) -> ? [ num ] }`
   6. Since everything succeeded in parameter matching, the type of this expression is return value of the instantiated function, or `[ num ]`

## Example of a similar situation to the above, but with the fn parameter being untyped

```chakra
some-function(untyped-fn) =
    ;; map : { ( { (?a) -> ?b}, [ ?a ]) -> ? [ ?b ] }
    map(untyped-fn, [ 1, 2, 3 ])
```

Let's work through this step by step:

1. Add an untyped binding for `untyped-fn`
2. Find the type of the binding `map`
3. It is `{ ( { (?a) -> ?b }, [ ?a ]) -> ? [ ?b ] }`
4. Collect generics from the parameters and the return type
5. Here the set of generics are `?a` and `?b`
6. Now, to instantiate the generics:
   1. Look at the first arugment, which is untyped
      1. Assign it the type of the parameter `{ (?a) -> ?b}`
   2. Look at the next argument, which is `[ num ]`
      1. Instantiate `?a` as `[ num ]`
      2. Update the `?a` slot of the first argument in it's type
   3. The generic `?b` was never instantiated
   4. The instantiated type of the function is `{ ( { (num) -> ?b }, [ num ]) -> ? [ ?b ] }`
   5. The type of the expression is therefore `[ ?b ]`
7. Get out the type of the binding `untyped-fn`, which is `{ (num) -> ?b }`
8. The type of the binding would be `{ ( { (num) -> ?b }) -> ? [ ?b ] }`
   1. There are generics found, lower them in to the lowest sequence, where the lowest generic `?b` becomes `?a`
   2. The final type of the function is `{ ( { (num) -> ?a }, [ num ]) -> ? [ ?a ] }`