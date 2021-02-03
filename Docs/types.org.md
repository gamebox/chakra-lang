
# Chakra Type System

## Scalar Types

The basic types used to represent data and perform calculations.

### Number

All numbers are represented in the DEC64 format created by Douglas Crockford.  It allows for performant arithmetic operations on a very large range of numeric values
with a level of accuracy not possible with typical binary float point formats.

### String

Strings are blobs of UTF-8 codepoints.

### Symbol

Symbols are static constant values that can not be created programmtically.  There
are two variants, global - starting with a lowercase letter; and module - starting
with an uppercase letter.  Global symbols of the same name are all the same value.
Module symbols are only the same value if they appear in the same module.

I.e.:

`#Symbol` in Foo.chakra is not the same value as `#Symbol` in Bar.chakra

Therefore these values can _not_ be pattern matched upon in other modules. The
intention is for these to be used for opaque data types.  See [Class Structs](#Class_Structs) for more information.

Each unique symbol is a singleton type (a type that contains only a single value).

#### Note

There is no boolean type.  Boolean values in the stdlib are represented by the union of `#true` and `#false` symbols, and should be used by library and application
authors by convention.

## Aggregate Types

These are the types used to combine and collect several scalar values.

### Tuple

An ordered, statically-sized, heterogeneous collection of values accessed through pattern matching mostly.

### Struct

A heterogeneous collection of values accessed through static, named fields.

#### Actor

This is a well-known shape of struct with two required fields, `init` and `receive`.  It is the definition for a concurrent unit of computation.

#### Class Struct

This is a special struct that has a discriminant field assigned to a symbol, this symbol may be public or Global, with the usual restrictions on symbol in play.  It uses a special syntax to create it and to pattern match on it (if possible):

```chakra
%some( value = 23 ) ; Like a struct, with the symbol name between the % sigil and open paren.
```

### List

An ordered collection of homogeneous values of dynamic size.  Fast head acccess and append, as well as fast left-to-right sequential operations.

### Map

A collection of homogeneous keys of one type and homogeneous values of another with dynamic size.

## Concurrent Types

These are the types that power the concurrency and security system of Chakra.

### Command

An opaque type that represents an effectual operation to be performed by the runtime.  These operations are only performed when the value is returned from the `init` function of an actor definition during the `spawn` operation or the `receive` function of a running actor when called in response to a message received. Examples: writing to and reading from the console, files, or the network; asking for random number generation; getting the current date and time; creating or destroying actors; sending messages to actors.

### Ref

This is a reference to an actor.  The actor it refers to may no longer be running.  It is impossible to send a message to an actor without a reference to it.  References can be passed around. See [Message Passing](./message-passing.md) for details on how to create refs and send messages.  Refs are parameterized by the message type of the actor it refers to.

### Capability

A unique, immutable token that can be used to access certain system resources.  The most powerful and general capabilities are passed into the `init` function of the main actor of an application.  Capabilities can not be created, but more limited capabilties can be created from more general ones and shared.  For instance the `stdio` capability can be restricted only allow writing to stderr and then sent to a child actor.

## Generics (Parametric Polymorphism)

TODO: It is possible to have types - mostly aggregates - that work with values don't need to care what the type.  Builtin types like list and map are an example, but some builtin types like `option` and `result` are another.

## Ad-Hoc Polymorphism

TODO: An interface type that can be defined at some point, and then authors can specify implementations for specific types.  A function or aggregate type with a generic type can constrain what they take by the existence of a definition for one or more of these interfaces for a type.  An example builtin is `@Ord` which allows a type to be orderable.

## Row-type Polymorphism

TODO: Struct types, which are the only type to have distinct fields to differentiate on, can be either open or closed.  An open type struct allows for more generic code to be written

## Union Types

TODO: multiple types may be returned from a function, or stored in a homogeneous collection, but it affects performance as the union of types must be pattern matched eventually to do something with it.  There are a number of built in union types in the stdlib, like `option`, `bool`, and `result`, as well `Command` and the message and state type of an actor.

## Sum Types

TODO:  The combination of any number polymorphic constraints with each other and some number of open struct types

`%( field-a = string, ...) + @Comparable`



# Type inference

Chakra promises to support 100% type inference.  There is no syntax in the language for the specification of types - outside of interface definitions, which is a meta language done in documentation comments.

## Inferring literals

If the literal is a scalar type, typing is as easy as using the appropriate type of the literal.

If the literal is an aggregate type, typing consists of collecting the type of each expression in the aggregate.  If there is more than one type detected, it is a type error.  It attempts to accomodate the most generic type possible to allow for different data types to be stored in an aggrgate.  As an example, if a list contains both a string and a number, it will unify as a `[ @Ord + @Comp ]`.  If a later usage demands that the list be one of those types in specific it will fail as a type error.

## Inferring simple bindings

First we place an untyped binding with the bindings name in the Env, and then attempt to unify the expression list.  If no error, the binding is updated with the type of the expression list

## Inferring function bindings

Similar to above, but also a binding is created for each of the arguments, with the type for each informed by any destructuring in the arg list.  If no error, each arg should be fully typed, and the function binding itself updated with the return type as the type of the expression list.

## Inferring expression list

We attempt to unify each binding in the expression list in turn. If all are successful, we then infer the type of the expression.  The type of the expression list is the type of the expression.  After the type is updated, the top Env should be popped.

## Inferring expression

### Apply expression

The binding should exist, unify all args in the apply against the args in the binding.  If no error, the type of the expression is the return type of the function binding.  Any variables that are untyped should have their type be inferred to be the type of the arg at their position (or with it's name).  Any open types(polymorphic) should be constrained if they match.  As an example of `[ @Ord ]` that is passed to an arg of `[ num ]` should be constrained to `[ num ]`.

If this is recursive, this is an error.  This should be handled in the case of a match clause to discover the real type.  If not a match clause, it is an infinite recursion error.

### Match expression

Unify the matchee with each match clause head.  If no errors, for each clause, do the following:

- Add a binding for the matchee with the most constrained type of the match clause head
- Add bindings for each bound element with as much type information as possible.
- Infer the type of the expression list
- Save that type in a buffer, pop the Env

If at any time, a clause has a type that is not unionable or summable with those before it, it is a type error.  If no errors, the combined type of all clauses is the type of the expression

### Literal expression

See Inferring Literals