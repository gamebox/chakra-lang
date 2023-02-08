---
title: Installation
category: Getting Started
---

Chakra is currently in pre-alpha, so installation is definitely not straightforward


## Install from source

### Requirements

You will need `clang`/`llvm`, `dotnet`, and `make` to install Chakra.

### Steps

Clone the repository from Github:

```sh
$ git clone https://github.com/gamebox/chakra-lang
$ cd chakra-lang
```

Then use `make` to install

```sh
$ make
$ make install
```

This should build the compiler and the runtime object file and put them the former into a place likely to already be in your `PATH`.