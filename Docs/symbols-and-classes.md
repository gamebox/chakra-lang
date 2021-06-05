# Symbols and Classes

Symbols are a data type for fixed member discrimination.  Since the Chakra type system is not a fully value-dependent type system, the details of symbols allow them to be used build constrained callee interfaces.

Classes are a static data structure (tuple or struct) that is "tagged" with a symbol, so that they can be discriminated in the same way.

## Symbols

Symbols are implemented as unsigned 64-bit integers and there is a single monotonic counter for symbols for the entire application, incrementing as the code generator encounters a new symbol.

## Classes

Class tuples and Class structs are at their root implemented as LLVM structs with an additional u64 field appended for the symbol tag.

## Unioning and discrimination

Global symbols (and classes using a global symbol tag) can be placed into arbitrary unions, by different global symbols (or global symbol tagged classes) being returned from different branches of a function, and be discriminated through pattern matching.  Module symbols can be used in the same manner, but a Module symbol only references the same symbol within that same Module.  A Module symbol can be exported in a global constant and matched in a different module, but a class tag can not be extracted, and a Module symbol tag cannot be discriminated outside of it's own module.

Global and module symbols can not be unioned
Symbols and classes can not be unioned
Classes can have zero fields

```chakra
< #gt | #lt | #eq > ; Legal
< #Hello | #World > ; Legal
< #something | #Else > ; Not legal - can't union global and Module symbol.

< #ok(value) | #err(e) > ; legal
< #some(value) | #none() > ; legal
< #some(value) | #none > ; Not legal - can't union bare symbol and class
```