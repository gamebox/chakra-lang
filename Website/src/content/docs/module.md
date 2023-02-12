---
title: Modules
category: Basics
---

# Modules

Here is an example of a full Chakra module.

```chakra
; At the top of the file, a module must have a module definition.
; It defines the bindings that are exported from the module.
= %(
    main,
)

; After the module definition must be any imports
%( io, Actor, Cmd, Capabilities, cmd, actor ) = /stdlib

; After any imports comes all top-level bindings

; This is type definition
PongMsg =
    | Ping

; This is destructured binding
%( print ) = io

; This is a constant binding
main = Task(init)

; This is a function binding
init(caps) =
    %( stdio ) = caps
    print(stdio, "Hello world!")
```

Below we'll go into more detail about each section, which should appear in order.

## Module Definition

This section contain any bindings that you wish to be exposed to other modules.  It must contains at least one export.

The syntax is as follows

```
= %( import1, import2, ...<more imports> )
```

Where each import is the name of a binding and nothing else.  Each one should be seperated by a comma, the last import may have a trailing comma. Newlines are welcome and are conventional.  More examples

```chakra
= %( some-binding )
```
```chakra
= %( binding-one, binding-two )
```
```chakra
= %(
    binding-one,
    binding-two,
)
```

## Imports

Imports must appear directly after the module definition, each one takes the form:

```chakra
local-name = /some-pkg
```

Here, the left hand side of the bind operator contains a binding pattern and the right hand side is a _module identifier_.  

### Module Identifiers

Module identifiers have the following three forms

#### Package import

```chakra
/some-pkg
```

A package import will import a third-party package that you have listed as a dependency in your `meta.chakra` file.  There is no way to import a module from inside the package.

#### Root import

```chakra
/root/libs/some-lib
```

A root import allows you to import from a library defined in the `libs` folder of your project.  Only the `lib.chakra` module's exports are available for import, and there is no way to access modules inside the library.

#### Relative import

```chakra
./some-module
```

Relative imports allow you to import from other modules inside the same directory, or sub-libraries defined in further directories.  There is no access to modules inside of sub-libraries directly.

### Import binding

Like with all constant bindings, the value being imported may be destructured using a pattern.  If the pattern does not match the shape of the imported module, the module will fail to compile.

## Type definitions

Type definitions are covered in a [separate documentation page](./typedefs.md).
## Bindings