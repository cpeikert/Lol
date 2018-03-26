/*
Module      : tensor.h
Description : Templates for the tensor DSL.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

A miniature DSL for converting (some) prime-index operators into arbitrary-index
operators via the tensorial decomposition for cyclotomic rings. In particular,
this DSL applies when the prime-power operator is just the tensor (with identity)
of the prime-index operator.
*/

#ifndef TENSOR_CPP_
#define TENSOR_CPP_

#include "types.h"
#include "common.h"
#ifdef __cplusplus

// templated function pointer for prime-index transformations
template <typename ring>
using primeFunc = void (*) (ring*, hShort_t, hDim_t, hDim_t, hDim_t);

// templated function pointer for prime-index CRT-style transformations, which
// also have pointers to roots of unity
template <typename ringy, typename ringru>
using primeCRTFunc = void (*) (ringy*, hShort_t, hDim_t, hDim_t, PrimeExponent, ringru*);

// Turns a prime-index transformation A_p which satisfies into
// A_{p^e} = I_{p^(e-1)} \otimes A_p into an arbitrary-index transformation.
// This mimics the tensor DSL in Haskell by turning A_m into
// A_{pp1} \otimes A_{pp2} \otimes ... \otimes A_{ppk}, where the prime-power
// factorization of m is pp1*pp2*...*ppk. It then applies turns each prime-power
// component into a tensor of prime-index transformations.
template <typename ring> void
  tensorFuserPrime (ring* y, hShort_t tupSize, primeFunc<ring> f, hDim_t totm,
                    PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  // The first matrix to be applied (starting on the right) is
  // (I_totient(pp2..ppk) \otimes A_pp1 \otimes I_1).
  // From there, we shift pp2 to the right tensor, then pp3, etc.
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    // compute p^(e-1)
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    // totient(ppi)
    hDim_t dim = (pe.prime-1) * ipow_pe;
    // remove the totient for this operator from the left
    lts /= dim;
    // apply the matrix
    // (I_totient((ppi+1)..ppk) \otimes A_ppi \otimes I_(pp1..(ppi-1)))
    // for a prime power
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      // as we move through the tuple, update the global modulus (if available)
      if(qs) {
        Zq::q = qs[tupIdx]; // global update
      }
      // since A_ppi (where ppi = p^e) is I_{p^(e-1)} \otimes A_p,
      // add an additional p^(e-1) factor to lts and use the function
      // pointer to call the *prime*-index transform.
      (*f) (y+tupIdx, tupSize, lts*ipow_pe, rts, pe.prime);
    }
    // Add the prime power for the transform we just applied to the right tensor.
    rts *= dim;
  }
}

template <typename ringy, typename ringru> void tensorFuserCRT (ringy* y, hShort_t tupSize, primeCRTFunc<ringy,ringru> f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ringru** ru, hInt_t* qs)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      if(qs) {
        Zq::q = qs[tupIdx]; // global update
      }
      (*f) (y+tupIdx, tupSize, lts, rts, pe, ru[i]+tupIdx);
    }
    rts *= dim;
  }
}

template <typename ringy, typename ringru> void tensorFuserCRTNew (ringy* y, primeCRTFunc<ringy,ringru> f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ringru** ru, hInt_t q)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    // TODO: Is q == 0 a good enough "not-defined" condition?
    if(q) {
      Zq::q = q; // global update
    }
    (*f) (y, 1, lts, rts, pe, ru[i]);
    rts *= dim;
  }
}

#endif /* __cplusplus */
#endif /* TENSOR_CPP_ */
