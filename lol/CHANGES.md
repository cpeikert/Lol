Changelog for lol project
================================

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