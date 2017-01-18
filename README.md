This branch (master) was previously known as 'split-packages'. The main problem preventing us from creating a new Hackage release based on this branch is that microbenchmark performance is severely degraded relative to the fast-microbenechmarks branch.

--------------------------------------------------------------------------------

This repository contains several Haskell libraries:

  * The `lol` directory contains the Haskell library Λ ○ λ (Lol),
    described in the paper
    [Λ ○ λ: Functional Lattice Cryptography](https://eprint.iacr.org/2015/1134). More
    documentation can be found on
    [Hackage](https://hackage.haskell.org/package/lol). This is the
    core of the project, and you'll need to install it to use anything
    else.

  * The `apps` directory contains example cryptographic applications
    built using Lol. If you are interested in using our example
    applications, you will need this library. It is on Hackage
    [here](https://hackage.haskell.org/package/lol-apps). If you are
    just writing your own applications, you don't need to install this
    library.

  * The `challenges` directory contains code to generate and verify
    RLWE and RLWR challenges, which are described [here](https://web.eecs.umich.edu/~cpeikert/rlwe-challenges).

  * The `compiler` directory contains an unmaintained, primitive FHE
    compiler for Lol. Eventually, this will work in conjuction with
    lol-apps to transform plaintext descriptions of algorithms into
    their homomorphic counterparts.

Developing in the lol ecosystem:
Compiling the lol ecosystem takes a long time. If you are just doing development,
you probably don't need to compile (with optimizations) the ecosystem. Instead,
you can develop interactively. Run
```
> stack build lol lol-cpp lol-repa lol-tests lol-benches rlwe-challenges lol-apps --dependencies-only
```
to build all third-party dependencies of the lol ecosystem, then you can
develop with
```
> ./ghci path/to/file
```
This command builds the C++ library for `lol-cpp` and the loads
all imported files from the lol ecosystem from source.

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
You can run unit tests with `stack test <package>`. You can run microbenchmarks with `stack bench <package>`. Currently `lol-cpp`, `lol-repa`, and `lol-apps`
have tests and benchmarks.

The `lol-apps` package includes examples of how to use each
application. These are built automatically when you install
lol-apps.
