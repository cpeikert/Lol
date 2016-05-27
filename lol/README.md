Λ ○ λ
-----

Overview of key modules, roughly from highest- to lowest-level:

* Cyc.hs, which defines an interface for using cyclotomic fields,
  rings R, and quotient rings Rq=R/qR; as well as many other
  commonly used operations, e.g., converting
  between rings, decoding and decomposing elements, modulus
  reduction/rounding, etc. etc. Cyc is a safe wrapper around the
  UCyc type, which exposes some representation-dependent operations.
  UCyc (and hence Cyc) is implemented using a generic Tensor (described below).

* Tensor.hs, which defines a class that encapsulates all the necessary
  linear transformations for operating on representations of R- and
  Rq-elements, e.g., the CRT transform, converting between the
  powerful and decoding bases, generating error terms, etc.

* RepaTensor.hs, which gives an
  implementation of the Tensor class based on the "repa"
  package, a highly optimized and parallelizable array library.

* CTensor.hs, which gives an
  implementation of the Tensor class using a C backend via Haskell's FFI.

* FiniteField.hs, which gives an unoptimized implementation of finite
  field arithmetic. To use this module, you will need an instance of
  IrreduciblePoly.  These instances provide irreducible polynomials
  for various degrees and base fields.  One instance is provided for
  characteristic 2 fields of size up to 2^32 in IrreducibleChar2.hs.

* ZqBasic.hs, which is a basic implementation of Zq=Z/qZ arithmetic.

* Factored.hs, which contains support code for "reifying"
  runtime-chosen integers as static types (mainly, the types q and m
  that are floating around as parameters of many other data types),
  and "reflecting" those types as integers back to the code.
