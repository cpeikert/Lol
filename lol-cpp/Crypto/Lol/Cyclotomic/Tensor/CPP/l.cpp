/*
Module      : l.cpp
Description : Powerful <-> Decoding basis conversion.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Prime- and arbitrary-index transformations for the L operator
(see LPR13: https://eprint.iacr.org/2013/293) which converts between the
powerful and decoding basis representation for cyclotomic ring elements.
L converts decoding -> powerful represenation, L^{-1} is the reverse.
*/

#include "types.h"
#include "tensor.h"

/* The prime-index transform that converts decoding basis coefficients (over any
 * abelian group) to powerful basis coefficients.
 * 'y' is an array of decoding basis coefficients in a three-dimensional tensor:
 * [lts]x[rts]x[p-1].
 * We can think of the operator as  (I_lts \otimes L_p \otimes I_rts).
 */
template <typename abgrp> void lp (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  // L_2 = id
  if(p == 2) {return;}

  // operate on the chunk of 'y' corresponding to each matrix on the diagonal.
  // each square diagonal matrix in I_lts \otimes L_p \otimes I_rts has
  // size (p-1)*rts
  // lidx is the offset into y corresponding to the block diagonal matrix
  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      // The new value y'_i = Σ_{j=1}^i y_i
      // We do this from left to right, letting y'_i = y_i + y'_{i-1}
      for (hDim_t off = ridx + rts; off < ridx + (p-1)*rts; off += rts) {
          y[off] += y[off-rts];
      }
    }
  }
}

/* The prime-index transform that converts powerful basis coefficients (over any
 * abelian group) to powerful basis coefficients.
 * 'y' is an array of powerful basis coefficients in a three-dimensional tensor:
 * [lts]x[rts]x[p-1].
 * We can think of the operator as  I_lts \otimes L_p^{-1} \otimes I_rts).
 */
template <typename abgrp> void lpInv (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  // (L_2)^{-1} = id
  if(p == 2) {return;}

  // operate on the chunk of 'y' corresponding to each matrix on the diagonal.
  // each square diagonal matrix in I_lts \otimes (L_p \otimes I_rts) has
  // size (p-1)*rts
  // lidx is the offset into y corresponding to the block diagonal matrix
  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      // The new value y'_i = y_i - y_{i-1} when i > 1 and y'_1 = y_1
      // We do this from right to left, letting y'_i = y_i - y_{i-1}
      for (hDim_t off = ridx + (p-2)*rts; off > ridx; off -= rts) {
        y[off] -= y[off-rts];
      }
    }
  }
}

/* Arbitrary-index transformation that converts decoding basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * powerful basis coefficients. The input 'y' represents a three-dimensional
 * tensor indexed as [lts]x[rts]x[p-1]
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, lp, totm, peArr, sizeOfPE, q);
  // Haskell expects each Z_q coefficient to be in the range 0 <= x < q_i, so
  // ensure that is the case.
  canonicalizeZq(y,totm,q);
}

/* Arbitrary-index transformation that converts decoding basis coefficients
 * (over a ring R ~ Z[x]/(f(x)), so the coefficients are in Z) into
 * powerful basis coefficients. The input 'y' represents a three-dimensional
 * tensor indexed as [lts]x[rts]x[p-1].
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lp, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lp, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLRRq (RRq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lp, totm, peArr, sizeOfPE, 0);
  canonicalizeRRq(y,totm);
}

extern "C" void tensorLC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lp, totm, peArr, sizeOfPE, 0);
}

/* Arbitrary-index transformation that converts powerful basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * decoding basis coefficients. The input 'y' represents a three-dimensional
 * tensor indexed as [lts]x[rts]x[p-1]
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLInvRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, lpInv, totm, peArr, sizeOfPE, q);
  // Haskell expects each Z_q coefficient to be in the range 0 <= x < q_i, so
  // ensure that is the case.
  canonicalizeZq(y,totm,q);
}

/* Arbitrary-index transformation that converts powerful basis coefficients
 * (over a ring R ~ Z[x]/(f(x)), so the coefficients are in Z) into
 * decoding basis coefficients. The input 'y' represents a three-dimensional
 * tensor indexed as [lts]x[rts]x[p-1].
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLInvR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lpInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLInvDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lpInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLInvRRq (RRq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lpInv, totm, peArr, sizeOfPE, 0);
  canonicalizeRRq(y,totm);
}

extern "C" void tensorLInvC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, lpInv, totm, peArr, sizeOfPE, 0);
}
