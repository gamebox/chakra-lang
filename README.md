# Chakra

Chakra is a capabilities-secure, purely functional, type- and memory-safe language
providing fast, simple concurrency with Actors.

Visit the [website](https://chakra-lang.dev) to learn more.

[![Build Status](https://github.com/gamebox/chakra-lang/actions/workflows/makefile.yml/badge.svg?branch=master&event=push)](https://github.com/gamebox/chakra-lang/actions/workflows/makefile.yml)

{% warning %}
**WARNING!** This project is in very early, but active development. Nothing really works.
{% endwarning %}

## Capabilities Secure

Capabilities are privileges given to Actors to access system resources. In normal
languages, capabilities are ambient in the environment of all code, and all threads.
This allows for dangerous patterns that lead to memory management errors, data races,
security vulnerabilities, and a severly reduce reasonability of the code.

In Chakra, all system capabilities are given solely to the Main Actor, which is the
root of execution for user-defined code in a program. They provide access to file
systems, network, I/O, and random number generation facilities. Capabilities required
by other actors in the system must be explicitly given to those actors.

<a id="org09a548f"></a>

## Purely Functional

<!-- TODO: Complete this section -->

In Chakra, all values are immutable. Side effects are not executed directly by the program
itself, but by commands sent to the runtime. These side effects are never synchronous, but
will be executed in order. This results in a application that looks and feels declarative.


## Type-safe - without annotations

<!-- TODO: Complete this section -->

Having an explicit type system provides a lot of utility. It empowers the compiler to ensure
that your application will run without type-level errors easily, and enables a very robust
set of tooling that is hard to replicate otherwise. But it also tends to introduce a lot of
boilerplate.

Chakra has an implicit, structural type system. Combined with the module-local, opaque symbol
type, it gives all the expressive power of explicit type systems - with no fuss. Types may be
expressed as documentation in docstrings, but are never required.

## Memory-safe

<a id="orgeabff17"></a>

<!-- TODO: Complete this section -->

Chakra uses a share-nothing memory model for individual concurrent units of computation called
Actors. You can think about them like memory-isolated state machines that communicate through
message passing, and message passing alone. And since the message passing semantics in Chakra
are causal, there are strong guarantees around being data-race free as well.

## Fast, simple concurrency

<!--  TODO: Complete this section -->

Chakra compiles a lean, fast runtime that manages Actor workloads across distinct threads on up
to as many as your machine can support. You simply write the non-blocking code that makes up
each component of computation needed. With causal message passing semantics, and a share-nothing
memory model, the runtime can move between hundreds of thousands of Actors very efficiently. And
the author of the program need not worry about Mutexes, Locks, or Semaphores.
