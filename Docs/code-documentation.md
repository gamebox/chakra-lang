# Chakra Code Documentation

_The following is an *idea* that needs vetting and sanity-checking, but is one that I quite like at the moment_

## The blessings and curses of documentation

One of the great things about a static type system is that it provides living documentation of your code: What types are the parameters to this function? What does it return? What is the structure of this type and what is it's purpose? To accomplish this in basically every statically typed language in any sort of use, these languages require some amount of type annotation. A good amount of this vanishes in ML-like languages that provide very good type inference - but best practices usually call for explicit annotation of at least public members of a module. When mixed with explicit declaration of types in code, this (along with annotations needed due to gaps in type inference systems) can lead to source files with a lot of code that is not really part of the logic and reduces readability of the code for those that understand the data model already.

Documentation comments also have proven themselves to provide great value. Many newer languages now add specific syntax for creating them, and older languages have repurposed block comments for the same purpose - creating wholly new text syntaxes inside of these comments to provide a number of different annotations used by the compiler, language servers, documentation generators, as well as humans reading the code as well. They, unfortunately, also add a serious amount of noise to a module - sometimes counting for more than half of the raw LOC count. These syntaxes are also most times unique to a language or language family(.Net), and must be learned from whole cloth whenever someone learns said language. They have a whole slew of ways in which they differ: XML annotations, symbol linking, text decoration, usage examples, etc.

This paradigm of documentation is so common, that adding a documentation comment syntax node to Chakra was done without even questioning it. This is interesting, as one of the design goals of Chakra was to provide the niceties of statically-typed and well-tooled languages with nearly no ceremony inside the source files themselves. The _100% type inference with no support for annotation_ design principle is part of this. So, how to accomplish this for documentation? It has to be written somewhere.

In thinking of an apporach to align with the above design principle, one may think of what happens to documentation comments in most languages today. The content of those comments are read, parsed, and associated with the symbols that are directly above. Sometimes the compiler will do some static analysis of it against the information the compiler has on the symbol. But in the end, it is usually used to output text files, nowadays largely in Markdown, that can be assembled into comprehensive online documentation.

## Using what's already there

Markdown is a particularly flexible and readable tool for documentation, and at this point in history is known to almost any dev. It's used in note-taking apps, on GitHub, and even supported in comments on popular social platforms. It is used as the target format of many documentation generators. It's easy to author (even if fraught with gotchas), and easy to edit in a plain text editor. And it can be parsed quickly and transformed into a number of formats.

With the negatives about documentation comments - and type annotations - listed above, and the design principles already explained above, may utilizing this format be a way to achieve the alignment sought after? This document proposes just such a thing.

## Documentation and Module interface together, outside of source

### The unheralded header file

In many current languages, like C, C++, OCaml, and the .Net family, there is a concept of a header or interface file. This is where a compilation unit documents its publicly available interface: types, functions, constants and more. It is also a tidy place to document all of those things in a human readable way. It keeps source code tidy from access modifiers and documentation comments. But there are typically some pitfalls here. In C, a header file can define static constants and structures - which is convenient but relies on convention to decide where those things should be. In .Net languages there is a lot of source duplication between interface files and implementation files (luckily the former can be helpfully generated from the latter and hand edited), and documentation comments are allowed in both.

Another issue is that these files often have different syntaxes (or alternatives to the main syntax), different keywords, and sometimes arcane rules about how they interact with source. This complicates the language parser, and has a small impact on cognitive load on the developer while writing them. One could argue that this issue is marginal, but there is an effect and it could have an impact on the developer.

The last issue that could be identified is that the structure of the generated documentation is often somewhat constrained by source order. This sometimes leads to source being organized around what best serves documentation rather than what best enables understanding and navigation of the source.

### Markdown as plain-text, human readable header file

Markdown, being a simple format for annotating text, is extremely flexible, but it does impose a very useful hierarchy on the document. It is quite possible to use this format to allow for documentation to be written in a consistent manner, but with a wide variety of inline and block presentation styles so that the content is imminently readable to all end users. It can be parsed very quickly and then - if following a simple convention - be used to extract a wealth of metadata that can be statically analyzed, such as:

- Type signatures
- Type definitions (naming structures and unions)
- Exported symbol names
- Documentation tests - runnable and verifiable examples

It is also quite trivial to transform compiler metadata into a template for a module automatically, even allowing for updating of existing documenentation without truncating or altering prose. And obviously, we can ensure that the documentation is always up to date with the source.

## A proposed implementation in Chakra

In Chakra source, the `DocComment` type could be removed, as well as all fields that reference it in `AST` and `TypedAST` types. Instead, a `CModuleDocument` type could be created that would contain the raw source(and/or Markdown AST) of a `[MODULE-NAME].md` file that corresponds to a `[MODULE-NAME].chakra` file, as well as a representation of all relevant metadata parsed out of that document. It would like like so:

```
main.chakra
main.md
libs/
libs/lib-a/lib.md
libs/lib-a/lib.chakra
libs/lib-a/other-module.md
liba/lib-a/other-module.chakra
...
```

For applications, a set of HTML documentation could be generated from the above that would document each and every module. For packages, only the documentation for the publicly available members could be output.

The Markdown inside of these files would be bog-standard Markdown of some specification that we choose (GFM or CommonMark). The power would lie in conventional structures to list information that is relevant.

Parsing and validation of these documents could be done only when requested or maybe done as part of a verification command with the CLI.

### Conventional Structures

#### Overall document structure

A module document would consist of the following broad segments

- Module Summary
- Table of contents
- Free-form exposition mixed with type and exported member documentation

#### Module summary

- [ ] Write This

#### Table of contents

#### Type documentation

#### Exported member documentation

### Generated Output

In the HTML output, there would be an index file that would consist of either the `main.md` or `package.md` content, and then a sidebar that links through the hierarchy of libraries in the application or package respectively. Inside of a module there would another sidebar containing a table of contents and a tree of type definitions and member definitions for quick access. The footer would contain important application or package metadata like version, authors, license, etc. Affordances for search, toggling light/dark modes, and keyboard navigation should be present.

The raw MD files themselves are written in a such a way that they would be accessible for navigation in a regular text editor or IDE that supports markdown as well.
