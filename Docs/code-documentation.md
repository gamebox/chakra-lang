# Chakra Code Documentation

*The following is an _idea_ that needs vetting and sanity-checking, but is one that I quite like at the moment*

## Documentation apart from code, but checked against it

One of the great things about a static type system is that it provides living documentation of your code: What types are the parameters to this function? What does it return? What is the structure of this type and what is it's purpose?  To accomplish this in basically every statically typed language in any sort of use, these languages require some amount of type annotation.  A good amount of this vanishes in ML-like languages that provide very good type inference - but best practices usually call for explicit annotation of at least public members of a module. When mixed with explicit declaration of types in code, this (along with annotations needed due to gaps in type inference systems) can lead to code with a lot of code that is not really part of logic and reduces readability of the code for those that understand the data model already.

Documentation comments also have proven themselves to provide great value.  Many newer languages now add specific syntax for creating them, and older languages have repurposed block comments for the same purpose - creating wholly new text syntaxes inside of these comments to provide a number of different annotations used by the compiler, language servers, documentation generators, as well as humans reading the code as well.  They, unfortunately, also add a serious amount of noise to a module - sometimes counting for more than half of the raw LOC count.  These syntaxes are also most times unique to a language or language family(.Net), and must be learned from whole cloth whenever someone learns said language.  They have a whole slew of ways in which they differ: XML annotations, symbol linking, text decoration, usage examples, etc.

This paradigm of documentation is so common, that adding a documentation comment syntax node to Chakra was done without even questioning it.  This is interesting, as one of the design goals of Chakra was to provide the niceties of statically-typed and well-tooled languages with nearly no ceremony inside the source files themselves.  The _100% type inference with no support for annotation_ design principle is part of this.  So, how to accomplish this for documentation?  It has to be written somewhere.

In thinking of an apporach to align with the above design principle, one may think of what happens to documentation comments in most languages today.  The content of those comments are read, parsed, and associated with the symbols that are directly above.  Sometimes the compiler will do some static analysis of it against the information the compiler has on the symbol.  But in the end, it is usually used to output text files, nowadays largely in Markdown, that can be assembled into comprehensive online documentation.

## Why not use what's already there?

Markdown is a particularly flexible and readable tool for documentation, and at this point in history is known to almost any dev.  It's used in note-taking apps, on GitHub, and even supported in comments on popular social platforms.  It is used as the target format of many documentation generators.  It's easy to author (even if fraught with gotchas), and easy to edit in a plain text editor.  And it can be parsed quickly and transformed into a number of formats.

With the negatives about documentation comments - and type annotations - listed above, and the design principles already explained above, may utilizing this format be a way to achieve the alignment sought after?  This document proposed just such a thing.

## Documentation and Module interface together, outside of source




TODO: finish section talking about using markdown as the documentation itself, using a lightweight template and convention.