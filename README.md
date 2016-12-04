This branch (split-packages) is a fork of master. Relative to master, it modularizes the library
into many fine-grained packages including Tensor backends, code for tests/benchmarks,
and the core library. Although it is the acting master branch (where all commits happen),
we have not yet merged it to master due to performance degredation of microbenchmarks.

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

Installing lol:

The easiest way to install Lol is to use stack, which is included in
the [Haskell Platform](https://www.haskell.org/platform/).
```
> stack setup
> stack install lol
```
You can run unit tests with `stack test lol`. You can run
microbenchmarks with `stack bench lol`. You can configure the
benchmarks by editing `lol/benchmarks/BenchConfig.hs`.

Installing lol-apps:
```
> stack install lol-apps
```
You can run unit tests with `stack test lol-apps`. You can run
benchmarks with `stack bench lol-apps`. An example of how to use each
application is included and is built automatically when you install
lol-apps.

Developing in the lol ecosystem:

./ghci path/to/file

This command builds the C++ library for lol-cpp and the loads
all imported files from lol* packages from source. You may
need to run 'stack bench lol-cpp' or similar first to install
the necessary dependencies.

You can load all top-level executables with ./ghci AllMain.hs