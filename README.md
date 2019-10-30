
[![Build Status](https://travis-ci.org/cpeikert/Lol.svg?branch=master)](https://travis-ci.org/cpeikert/Lol)

This branch is master. It should always be compiling!

--------------------------------------------------------------------------------

This repository contains several Haskell libraries:

  * The `lol` directory contains the Haskell library Λ ○ λ (Lol),
    described in the paper
    [Λ ○ λ: Functional Lattice Cryptography](https://eprint.iacr.org/2015/1134). More
    documentation can be found on
    [Hackage](https://hackage.haskell.org/package/lol). This is the
    core of the project, and you'll need to install it to use anything
    else.

  * The `lol-apps` directory contains example cryptographic applications
    built using Lol. If you are interested in using our example
    applications, you will need this library. It is on Hackage
    [here](https://hackage.haskell.org/package/lol-apps). If you are
    just writing your own applications, you don't need to install this
    library.

  * The `lol-cpp` directory contains a C++ backend "tensor"
    implementation.

Developing in the Lol ecosystem:

Compiling the Lol packages takes a long time. If you are just doing
development, you probably don't need to compile (with optimizations)
the whole ecosystem. Instead, you can develop interactively. Run
```
> stack build lol lol-cpp lol-apps lol-tests lol-benches --dependencies-only
```
to build all third-party dependencies of the Lol ecosystem, then you can
develop with
```
> ./ghci path/to/file
``` 
This command builds the C++ library for `lol-cpp` and loads all
imported files from the Lol ecosystem from source.

You can load all top-level executables with `./ghci AllMain.hs`.

Installing Lol:

If you want to run benchmarks or tests, you'll need to compile the ecosystem
with optimizations. The easiest way to do this is to use stack, which is
included in the [Haskell Platform](https://www.haskell.org/platform/).
```
> stack setup
> stack build lol
```
or
```
> stack setup
> stack install lol-apps
```
You can run unit tests with `stack test <package>`. You can run
microbenchmarks with `stack bench <package>`. Currently `lol-cpp`
and `lol-apps` have tests and benchmarks.

The `lol-apps` package includes examples of how to use each
application. These are built automatically when you install
`lol-apps`.
