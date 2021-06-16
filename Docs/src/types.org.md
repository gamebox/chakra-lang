# Chakra Type System

## Scalar Types

The basic types used to represent data and perform calculations.

### Number

All numbers are represented in the DEC64 format created by Douglas Crockford. It allows for performant arithmetic operations on a very large range of numeric values
with a level of accuracy not possible with typical binary float point formats.

### String

Strings are blobs of UTF-8 codepoints.

### Symbol

Symbols are static constant values that can not be created programmtically. There
are two variants, global - starting with a lowercase letter; and module - starting
with an uppercase letter. Global symbols of the same name are all the same value.
Module symbols are only the same value if they appear in the same module.

I.e.:

`#Symbol` in Foo.chakra is not the same value as `#Symbol` in Bar.chakra

Therefore these values can _not_ be pattern matched upon in other modules. The
intention is for these to be used for opaque data types. See [Class Structs](#class-structs) for more information.

Each unique symbol is a singleton type (a type that contains only a single value).

#### Note

There is no boolean type. Boolean values in the stdlib are represented by the union of `#true` and `#false` symbols, and should be used by library and application
authors by convention.

## Aggregate Types

These are the types used to combine and collect several scalar values.

### Tuple

An ordered, statically-sized, heterogeneous collection of values accessed through pattern matching mostly.

#### Class tuple

This is a special tuple that has a discriminant field assigned to a symbol, this symbol may be public or Global, with the usual restrictions on symbol in play. It uses a special syntax to create it and to pattern match on it (if possible):

```chakra
#some(23) ; Like a tuple, with a symbol appearing directly before the openining paren.
```

### Struct

A heterogeneous collection of values accessed through static, named fields.

#### Class Structs

This is a special struct that has a discriminant field assigned to a symbol, this symbol may be public or Global, with the usual restrictions on symbol in play. It uses a special syntax to create it and to pattern match on it (if possible):

```chakra
%some( value = 23 ) ; Like a struct, with the symbol name between the % sigil and open paren.
; Note that the above is distinct from Class tuples above by the sigil, this is to facilitate field punning in Class Structs
```

### List

An ordered collection of homogeneous values of dynamic size. Fast head acccess and append, as well as fast left-to-right sequential operations.

### Map

A collection of homogeneous keys of one type and homogeneous values of another with dynamic size.

### Union Types

Multiple types may be returned from a function, or stored in a homogeneous collection. The types that are legal for entering a union are symbols, Class tuples, and Class structs of the same access level.

There are a number of built in union types in the stdlib, like `option`, `bool`, and `result`, as well `Command` and is often used for the `msg` and `state` type of an actor.

Use of this type feature affects performance as the union of types must be pattern matched eventually to do something with it.

## Concurrent Types

These are the types that power the concurrency and security system of Chakra.

### Actor

This is a type that is parameterized on it's `init` and `msg` types, and is used solely to be sent to the `spawn` function. A message of your choosing will be returned with the `Ref` to a running instance of this Actor.

### Command

An opaque type that represents an effectual operation to be performed by the runtime. These operations are only performed when the value is returned from the `init` function of an actor definition during the `spawn` operation or the `receive` function of a running actor when called in response to a message received. Examples: writing to and reading from the console, files, or the network; asking for random number generation; getting the current date and time; creating or destroying actors; sending messages to actors.

### Ref

This is a reference to an actor. The actor it refers to may no longer be running. It is impossible to send a message to an actor without a reference to it. References can be passed around. See [Message Passing](./message-passing.md) for details on how to create refs and send messages. Refs are parameterized by the message type of the actor it refers to.

### Capability

A unique, immutable token that can be used to access certain system resources. The most powerful and general capabilities are passed into the `init` function of the main actor of an application. Capabilities can not be created, but more limited capabilties can be created from more general ones and shared. For instance the `stdio` capability can be restricted only allow writing to stderr and then sent to a child actor.

## Polymorphism

Chakra supports a number of forms of polymorphism to allow code to be written simply and declaratively as much as possible. Use of these features will affect code size - as they require code generation for each concrete instatiation of the function, which may affect overall system performance. But usually this cost is small compared to the gains in developer productivity and code reuse and readability.

### Generics (Parametric Polymorphism)

It is possible to have types - aggregates and functions - that work with values don't need to care what the concrete type is. Builtin types like list and map are an example, but some stdlib types like `option` and `result` are another.

### Ad-Hoc Polymorphism

_Note_: This feature is not yet available, and will probably arrive shortly before v1.0

An interface type that can be defined at some point, and then authors can specify implementations for specific types. A function or aggregate type with a generic type can constrain what they take by the existence of a definition for one or more of these interfaces for a type. An example builtin is `@Ord` which allows a type to be orderable.

### Row-type Polymorphism

TODO: Struct types, which are the only type to have distinct fields to differentiate on, can be either open or closed. An open type struct allows for more generic code to be written.

### Sum Types

TODO: The combination of any number polymorphic constraints with each other and some number of open struct types

`%( field-a = string, ...) + @Comparable`
