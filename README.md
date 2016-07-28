This repository contains several Haskell libraries:

  * The folder `lol` contains the Haskell library Λ ○ λ (Lol), described in the paper <https://eprint.iacr.org/2015/1134 Λ ○ λ: A Functional Library for Lattice Cryptography>. More documentation can be found on <https://hackage.haskell.org/package/lol Hackage>. This is the core of the project, and you'll need to install it to use anything else.

  * The folder `apps` contains example cryptographic applications built using Lol. If you are interested in using our example applications, you will need this library. It is on Hackage <https://hackage.haskell.org/package/lol-apps here>. If you are just writing your own applications, you don't need to install this library.

  * The folder `challenges` contains code to generate and verify the (as yet unpublished) RLWE challenges. For now you can ignore this folder.

  * The folder `compiler` contains an unmaintained, primitive FHE compiler for Lol. Eventually, this will work in conjuction with lol-apps to transform plaintext descriptions of algorithms into their homomorphic counterparts.

Installing lol:

The easiest way to install Lol is to use stack, which is included in the <https://www.haskell.org/platform/ Haskell Platform>.

> stack setup
> stack install lol

You can run unit tests with `stack test lol`. You can run microbenchmarks with `stack bench lol`. You can configure the benchmarks by editing `lol/benchmarks/BenchConfig.hs`.

Installing lol-apps:

> stack install lol-apps

You can run unit tests with `stack test lol-apps`. You can run benchmarks with `stack bench lol-apps`. An example of how to use each application is included and is built automatically when you install lol-apps.