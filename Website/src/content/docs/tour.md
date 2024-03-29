---
title: Tour
category: Basics
---

# Basic Syntax

Chakra is syntactically a very simple language. There are for all
intents and purposes four forms that you will find everywhere. The first
is the binding.

## Binding Form

```chakra
some-var = 1
```

The above is a simple, one-line binding. You will notice that there is a
symbol on the left-hand side, a space, an `=` sign, a space, and then a
number literal. A symbol must follow the following regular expression

```regexp
([A-Za-z]+[a-z]*)(\-{1,2}[A-Za-z]+[a-z]*)*[?!\*]{0,1}
```

So, the following are valid and invalid symbols:

Symbol Valid?

| Symbol        | Is Valid? |
| ------------- | --------- |
| a             | yes       |
| b-a           | yes       |
| T-T           | yes       |
| SOMETHING     | no        |
| var22         | no        |
| someVar       | no        |
| some~var~     | no        |
| Some-Var      | yes       |
| do-something! | yes       |
| some-mod--fn  | yes       |
| empty?        | yes       |
| empty???      | no        |

Most bindings will not necessarily bind to a single literal or
expression, but will bind to the result of an _expression list_ An
expression list is a series of bindings, ending in an expression or
literal. Expression list must appear on the following line, indented one
level from the line that begins the binding. Here are some examples

```chakra
some-binding =
  sub-binding-one =
    sub-binding-one--sub-binding = 1
    add(sub-binding-one--sub-binding 2)
  (sub-binding-one 3)
```

Here we see an expression list, which contains another binding that
itself is bound to another expression list, and then a literal for type
of value we\'ll discuss later, the value of the expression is the bound
value of the binding.

There are more variations of the Binding form, but they will be covered
in later sections.

## Pattern Matching Form

Pattern matching is very powerful primitive facility of the language,
and has it\'s own special form. The Pattern Matching Form consist of two
elements in an expression context, the `?` binary operator, and the
Pattern Form

### The `?` operator

The `?` operator is a primitive operator that means \"match\". It is
preceeded by a symbol or literal and a space, and followed by a list of
Pattern Forms. The value of the match expression is the value of the
Pattern Form that is evaluated - which is the first Pattern Form whose
pattern matches the value represented by the symbol or literal
preceeding the `?`. An example follows:

```chakra
some-binding =
  some-other-val ?
    | () ->
      "An empty tuple"
    | (_) ->

      "A 1-element tuple"
    | (_ _) ->
      "A 2-element tuple"
    | (_ _ _) ->
      "A 3-element tuple"
```

Here we see that we are matching on `some-other-val`, which based on the
patterns supplied, must be a tuple with between 0 and 3 elements. Tuples
will be explained in the section on _Container Types_. But to understand
why, we need to explain the Pattern Sub-Form itself

### Pattern Sub-Form

The Pattern Sub-Form is one of the most useful forms to learn in Chakra.
At this point, we are learning how it is used in a match expression
context, but it has other uses that will be explained later.

A Pattern Sub-Form is of the following form

    | /pattern/ [: /guard/ ]-> /expression/|/expression-list/

Where an expression list must appear, indented, on the following line. A
pattern is similar in form to literals, but allows symbols to be
inserted in parts of the literal form to extract the values found there.

In the example above we saw:

1.  `()` used in the first pattern, this matches the literal form of a
    0-element tuple.
2.  `(_)` used in the second pattern, this matches the literal form of a
    1-element tuple, with `_` indicating that the value located in that
    position will be ignored.
3.  `(_ _)` matches the literal form of a 2-element array, using `_` for
    both space separated elements.

And so on. It is possible to match on exact literal values like `1` or
`"Hello World"` or even `(1 2 3)`, as well as partial literal forms like
`(1 2 x)`, which will bind x to the 3rd element of a 3-element tuple if
found.

## Application Form

Functions in Chakra are applied in a very straightforward way which may
look conventional at first glance.

`some-fn(arg-one, arg-two, [more-args])`

But this belies some of the beauty of application in Chakra. Chakra also
supports _named application_ and _partial application_, and these can
even be combined. Any function with named parameters(\*) can have their
arguments applied by name.

Assume a function defined as:

`some-fn num-twinkies num-hohos = ...`

Calling this function can have all the following forms:

`some-fn(22, 10)`

`some-fn(num-twinkies = 22, num-hohos = 10)`

`some-fn(num-hohos = 10, num-twinkies = 22)`

Notice that named application looks like a struct literal, and that
positional application looks like a tuple literal, and that\'s exactly
the right intuition. Refer to _Functions_ below for more information on
that. There is also partial application. This is where you assert that
the arguments you wish to apply are not the total set to apply. This can
be done even if you have applied all of the arguments, but wish to delay
evaluation of the function. The value of such an expression is a
function that may be applied with any other valid arguments that have
not been fulfilled.

It looks like this:

`some-fn(22, ...)`

# Primitive Types

Primitive types are unboxed, immutable, scalar values. They are the raw
stuff that makes up more complex types and represent the different types
of value that carry information in a program.

## Number

There is only a single number type in Chakra, and it is represented as a
DEC64, a unique number type that has defined semantics for a large set
of integer and decimal values with no loss in precision.

Numbers are values represented with literals of the following forms:

`1` `323424242244483838` `3.14` `0.000000000000000000292`

There are operations to drop decimal precision(`ceil`, `floor`, `round`,
and `max-precision`), and all of the usual operations exist, with
(`add`, `sub`, `mul`, `div`, `pow`, and `sqrt` being in the prelude).

## Text

Text is analogous to the String type found in most other languages. They
are capable of containing text encoding in UTF-32. Like most languages
they are represented with literals of the following form:

\"This is a text\"

Within a Text literal, there can be arbitrary Unicode characters, either
written literal or with escapes.

Create example of Unicode strings

\"\"

Also a notable difference from Strings in other languages is that there
is one literal form for single line and multi-line texts, with the `"`
symbols acting as braces in a collection type. In multi-line texts, the
first character must come after a newline at the next level of
indentation. All whitespace before that indentation level is ignored,
and any characters in that area is considered an error. The closing `"`
must appear after a newline after the last character at the same level
of indentation as the starting `"`. This enforces a uniform style. An
example:

```chakra
some-val =
  a-text = "
    This is a long text, like really, really long.  It contains a lot of
    characters.  It's important to note that newlines found in source are
    ignored.\n
    \n
    Literal newlines are respected.
  "
  another-text =
    "
      This is also considered idiomatic style
    "
```

There are a number of functions for working with Texts, including
`format`, `length`, `trim`, `pad`, `piece`, and many more

## Symbol

Symbols are one of the superpowers of Chakra. They are immutable,
compile-time created values that refer only to themselves. They come in
two varieties, Global and Module.

Global Symbols occupy a namespace that is accessible to any part of a
program by just writing the literal. It has the following form:

`#some-symbol`

Any code can use and refer to this symbol and it will refer to the same
value as anywhere else. But there are cases when a symbol is needed that
only has meaning within a particular module, either to enforce
encapsulation, or to signal that a data structure is meant to be handled
in a certain manner. For this use case their are Module Symbols, and
they are of the form:

`#Mod-Symbol`

To use or refer to this symbol outside of the module in which it is
defined, it must be bound to a field of the exported module. It is also
possible to export a function that matches on one or more Module symbols
in some data structure in a pattern matching guard.

It is important to note that you can have a `#Mod-Symbol` is any number
of modules, but that literal will always resolve to a value unique to
that module. All instances of that symbol in a module will refer to a
singleton value. Since all indenitifers are localized to their origin
module through the import system, it is always clear that a module
symbol literal always refers to a symbol specific to that module. These
can be used to easily allow libraries and packages to hide
implementation details of data structures.

The pattern match guarding system will be discussed in a later section.
There is a function in the prelude for working with symbols, `sym?` that
can distinguish symbols from other value types

## Boolean

Chakra does not have a true Boolean type, but the Symbols `#true` and
`#false`

There are a few functions for booleans in the prelude that are useful
like `not`, `and`, and `or`

# Container Types

Container types allow one to hold multiple other values and provide
affordances for access, manipulation, and more. They are all persistent,
immutable data structures. That means that though they are immutable,
functions that would seem to require a deep copy, instead are able to
share structure with the other value so that these operations are very
performant. It also improves the message passing performance, since
state can be \"shared\" without mutability. This is similar to a growing
list of global constants that are garbage collected based on reference
counting.

## Product Types

Product types are densely packed collections of hetergeneous values with
constant access time. They are statically defined by analyzing the used
values in the program. They cannot be constructed with a function, only
with a literal. They can be accessed through a static access like
pattern matching or `.` field access.

### Tuple

Tuple are a product type that have unnamed, ordered fields. They cannot
be iterated on. Their values are accessed through pattern matching or
\`.\` numbered field access This is a tuple\'s literal form:

`(1 2)`

Numbered field access looks like this:

```chakra
some-tup = (1, 2)
some-int = some-tup.1 ; Not supported yet
```

Functions for working with tuples in the prelude include `tuple?` and
`size`

### Struct

Structs compiled down to the same data structure as tuples, but allow
fields to be explicitly named. This is useful for documenting intent.
This is a struct\'s literal form:

`%(field-one = 1, field-two = 2)`

It is typical and idiomatic to write struct literals with multiple
fields on multiple lines like so:

```chakra
some-struct = %(
  field-one = 1,
  field-two = 2
)
```

Struct fields can be accessed by name like so:

`some-struct.field-one`

Functions for working with Structs in the prelude are `struct?`.

## Sequential Types

### List

A list in chakra is conventional singly-linked list, which allows for
performant sequential access from head to tail, and O(1) concatenation
to the head. It has the simplest implementation of the `seq` interface.

The list has the following literal form:

`[1, 2, 3, 4]`

List functions abound: `head`, `tail`, `drop`, `take`, `map`, `filter`,
`reduce`, `add`, `length` and more. Most list functions are part of the
`seq` interface.

### Map

Maps present the semantics of a hash table, with O(1)-O(n) time
complexity for search, insert, and delete. Useful when you want
associative semantics for sparse data.

The set has the following literal form:

`%['a = 1, 'b = 2]`

Keys and values can be of any type, but keys and values not of the same
type lead to a complex typing and worse performance characteristics.

Map implements the `seq` and `collection` interfaces, like list, but the
signatures for these functions use a key-value tuple for the element.
They also implement the `assoc` interface, which provides the `keys`
and `values` functions.

### Set

The Set is actually semantic sugar for a Hash Map where the keys are
always bound to a singleton value `'present`.

Sorted sequences of sets are available
with the `sort` function. Like maps, the types of the values should be
the same, but may be different if you are willing to take a performance
hit.

Sets also implement `collection` so `drop`, `take`, `map`, `filter`,
`reduce`, `contains?`, `empty?` etc. are available.

# Functions

## Single-head

The simplest, least flexible way to define a function follows the form:

```chakra
some-fn(a, b) = ...
```

This defines a straightforward binding of a function that can be applied
with a two-element tuple. The types of these arguments is unclear
without seeing the definition of the function.

## Multi-head (?)

This style of function binding allows for different definitions to be
defined for a single binding. These different function heads can have
different numbers of arguments.

```chakra
some-fn =
  | (a) = ...
  | (a b) = ...
  | (a b ...) = ...
```

One head may make a tail call to another head, making this great for
recursive algorithms.

# Modules

## Modules As Structs

Modules have a 1-to-1 correspondence with files. Each file exports a
module. The module will have the same name as the filename minus the
extension. This means that filenames for `.chakra` files must follow
Chakra identifier naming conventions. Specifically, it must follow the
regex `[a-z]+(\-[a-z]+)*` which is a subset of that used for symbols

```chakra
<module-name> = (
  some-const = s
  some-fn = f
  something-else = e
)

<module-name> = (
  some-const
  some-fn
  something-else
)
```

Two things to note from the above:

1.  The exact module name MUST be used, AND it MUST match the filename
    of the file.
2.  It MUST be the first line of source in the file(not including doc
    comments).
3.  The value of the binding MUST be a struct. It MAY have explicit
    bindings or utilize name punning for the bindings.
4.  It MAY NOT explicitly bind to anything but symbols pointing to
    bindings made later in the file. These can include symbols bound
    from _Imports_.

# Imports

## Import Syntax

In a given file, directly after the module export must come any imports.
Imports in Chakra are just bindings, but have access to a special
namespace called the Tree. The Tree begins with the operator `/`
followed immediately by a series of names seperated by `/` ending in
some symbol or optionally followed by a `.` and then a symbol of a
binding from the struct. The first path is either `root` which
represents your application or library\'s code. Here are some examples
to illustrate.

```chakra
some-mod = /root/lib/sublib/mod
some-pkg-mod = /pkg-name/lib/mod
some-other-mod-fn = /root/lib/mod.fn-name
```

Imports may be interleaved with whitespace, but no other bindings. And
expression lists can not be used, only a simple reference to a member in
the Tree. But a module can be destructed like so:

```chakra
%( some-fn ) = /root/lib/sublib/mod
```

# Files

Chakra programs are organized into files. Files export modules. These
modules can be referenced in other files. This is accomplished by all
modules being topographically sorted into a data structure called the
Tree.

## Naming

As stated in _Modules_, files must be given a name that conforms to the
follow regular expression:

`[a-z]+(\-[a-z]+)*\.chakra`

## Structure

A file must maintain the following structure:

1.  Module declaration. See _Modules_ for more information.
2.  At least two blank lines.
3.  Imports. See _Imports_ for more information.
4.  At least two blank lines.
5.  All other bindings.

# Libraries

Libraries are a means of encapsulation and organization. They map to
directories found in a `libs` directory in the root of an application or
package. The definition is in a `.chakra` file in said directory with
the same name as the directory itself. So, a library named `some-lib` is
defined in `<project-root>/libs/some-lib/some-lib.chakra`. It has access
to all modules from files in the same directory, as well as the
libraries contained in sub-directories that are immediate children of
it\'s directory.

So, imagine this layout of `<project-root>/libs/`:

`some-lib/a.chakra` `some-lib/b.chakra` `some-lib/some-lib.chakra`
`some-lib/a-sub-lib/c.chakra` `some-lib/a-sub-lib/a-sub-lib.chakra`
`some-lib/a-sub-lib/a-sub-sub-lib/d.chakra`
`some-lib/a-sub-lib/a-sub-sub-lib/a-sub-sub-lib.chakra`
`some-other-lib/some-other-lib.chakra`

In this scenario, `some-lib` will have access to modules `a` and `b`, as
well as the library `a-sub-lib`, but nothing else. Library files cannot
access other libraries in the Tree. If you need access to the Tree, it
should come be done in a Module File.

## Naming

Libraries have to adhere to the same naming standards as files. See
_Files_ for more information. Additionally, their name must be the same
as the enclosing directory.

## Structure

The library file must have the following structure. Similar to module
files.

1.  Library declaration. Sames structure as a _Modules_ declaration.
2.  At least two blank lines.
3.  Imports. See _Imports_ for more information. Note the import
    restrictions listed above.
4.  At least two blank lines.
5.  All other bindings.

# Packages

Packages are a discrete, distributable unit of code. It is named,
versioned, and has a well defined API. It can tell the tooling that it
requires certain other packages as dependencies. The API of the package
is defined in a package file, which must be named `package.chakra`. The
metadata of the package, such as name, author, license, and dependencies
defined in `metadata.chakra`.

## Naming

Packages extend the naming scheme used for files, modules, and libraries
in the following way:

`[a-z]+(\-[a-z]+)*((\-v\d{1,3})|(\-alpha)|(\-beta))?`

This allows for breaking versions to be published under separate
distinct names. Versions are solely a distinct hash of the project
folders contents. Since breaking changes can not be released under the
same package name, all changes are additive from an API standpoint.

## Structure

### Package File

The package file follows the same structure as a library file, but the
export must be named `package`.

### Metadata File

The metadata file must have a single binding, `metadata` as the first
line of source. Like with _Modules_, it must be a struct. It must have
the following fields:

`name` `authors` `license` `deps`

### Directory Structure

package.chakra metadata.chakra libs/ lib-one/ lib-two/ lib-three/ ...
Makefile .dependencies/ ...

## Dependencies

Dependencies are retrieved and stored in a hidden directory
`.dependencies`.

# Applications

Applications are very similar to packages, but instead of a
`package.chakra` file, it has a `main.chakra` file that must export an
Actor. This is the equivalent of `main()` in a language like C. Actors
are covered in a separate article.
