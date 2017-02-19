Changelog for lol project
================================

0.6.0.0
----
 * Support for serializing ring products, linear functions, and TypeReps.
 * Split previous lol package into separate tensor packages lol-cpp and lol-repa
   and utility packages lol-benches and lol-tests.
 * Moved test and benchmark code to packages lol-tests and lol-benches, respectively.
 * Simpler tests and benchmarks (though microbenchmarks might be slightly slower
   than 0.5.0.2).
 * Fixed minor bug in IrreducibleChar2.hs
 * Moved tensor-specific instances (Elt, Unbox, etc) to tensor packages
   (lol-cpp and lol-repa).

0.5.0.2
----
 * Updates to README.

0.5.0.1
----
 * Benchmarks now compile.

0.5.0.0
----
 * Dramatically improved CT performance using Haskell INLINE/INLINABLE pragmas.
 * Removed fmapTM from Tensor.
 * Removed valuePPs, totientPPs, radicalPPs, oddRadicalPPs from Factored.
 * K_q (i.e. base ring RRq) is now serialized with a discrete modulus.
 * Fixed a bug in UCyc.divG. The result is `divG` is now split into `divGPow`
   `divGDec`, and `divGCRTC`.
 * Added `UCycPC` type synonym to UCyc.
 * Added data-level interface for Factored.
 * Added Random instance for FiniteField.

0.4.0.0
----
 * Added support for GHC 8.0, also compatible with 7.10.3
 * Converted documentation to MathJax
 * Renamed Tensor.Matrix -> Tensor.Kron
 * Added Crypto.Lol.Types interface file

0.3.0.0
-----
 * Support for protocol-buffers
 * Support for reifying Factored types
 * Support for reals (RR) mod q
 * Replaced C backend with C++ backend
 * Renamed LatticePrelude -> Prelude
 * Added monad argument to CRTrans

0.2.0.0
-----
 * Added benchmarks
 * Better performance for C backend with RNS base ring
 * UCyc exposes bases as type for safety
 * Other safety improvements throughout
 * Easier index representation with TemplateHaskell
 * Split SymmSHE into new package lol-apps.

0.1.0.0
-----
 * Fixed bug in Box-Muller sampling routine.
 * Changed how we lift linear functions for better noise control.
 * Split entailment functions in Tensor.
 * Increased performance in FastCyc by better handling Sub constructors.