/*
Module      : u.cpp
Description : Powerful <-> Decoding basis conversion.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Prime- and arbitrary-index transformations for the U operator
(see LPR13: https://eprint.iacr.org/2013/293) which converts between the
powerful and decoding basis representation for cyclotomic ring elements.
U converts decoding -> powerful represenation, U^{-1} is the reverse.
*/

#include "types.h"
#include "tensor.h"

/* The prime-index transform that converts decoding basis coefficients (over any
 * abelian group) to powerful basis coefficients.
 * 'y' is an array of decoding basis coefficients in a three-dimensional tensor:
 * [lts]x[rts]x[p-1].
 * We can think of the operator as  (I_lts \otimes U_p \otimes I_rts).
 */
template <typename abgrp> void up (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t ltsOffset;
  hDim_t rtsOffset;
  int i;

  // U_2 = id
  if(p == 2) {return;}

  // each square diagonal matrix in I_lts \otimes (U_p \otimes I_rts) has
  // size rts*(p-1)
  hDim_t ltsBockSize = rts*(p-1);
  // operate on the chunk of 'y' corresponding to each matrix on the diagonal
  for (ltsOffset = 0; ltsOffset < lts; ++ltsOffset) {
    // the offset into y corresponding to the block diagonal matrix
    hDim_t blockIdx = ltsOffset*ltsBockSize;
    // operate on slices of 'y' of size 'rts'
    for (rtsOffset = 0; rtsOffset < rts; ++rtsOffset) {
      hDim_t tensorOffset = blockIdx + rtsOffset;
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      hDim_t idx = tensorOffset + (p-2)*rts;
      // the actual work, stepping backwards: y_{i-1} = y_{i-1} + y_i
      for (i = p-2; i != 0; --i) {
        y[idx-rts] += y[idx];
        // decrease the pointer by the size of the slice: rts
        idx -= rts;
      }
    }
  }
}

/* The prime-index transform that converts powerful basis coefficients (over any
 * abelian group) to powerful basis coefficients.
 * 'y' is an array of powerful basis coefficients in a three-dimensional tensor:
 * [lts]x[rts]x[p-1].
 * We can think of the operator as  (I_lts \otimes UU_p)^{-1} \otimes I_rts).
 */
template <typename abgrp> void upInv (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t ltsOffset;
  hDim_t rtsOffset;
  int i;

  // (U_2)^{-1} = id
  if(p == 2) {return;}

  // each square diagonal matrix in I_lts \otimes (U_p \otimes I_rts) has
  // size rts*(p-1)
  hDim_t ltsBockSize = rts*(p-1);
  // operate on the chunk of 'y' corresponding to each matrix on the diagonal
  for (ltsOffset = 0; ltsOffset < lts; ++ltsOffset) {
    // the offset into y corresponding to the block diagonal matrix
    hDim_t blockIdx = ltsOffset*ltsBockSize;
    // operate on slices of 'y' of size 'rts'
    for (rtsOffset = 0; rtsOffset < rts; ++rtsOffset) {
      hDim_t tensorOffset = blockIdx + rtsOffset;
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      hDim_t idx = tensorOffset;
      // the actual work, stepping forwards: y_i = y_i - y_{i+1}
      for (i = p-2; i != 0; --i) {
        y[idx] -= y[idx+rts] ;
        // advance the pointer by the size of the slice: rts
        idx += rts;
      }
    }
  }
}

/* Arbitrary-index transformation that converts decoding basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * powerful basis coefficients. The input 'y' represents a four-dimensional
 * tensor indexed as [lts]x[rts]x[p-1]
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorURq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, up, totm, peArr, sizeOfPE, q);
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
extern "C" void tensorUR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, up, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorUDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, up, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorUC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, up, totm, peArr, sizeOfPE, 0);
}

/* Arbitrary-index transformation that converts powerful basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * decoding basis coefficients. The input 'y' represents a three-dimensional
 * tensor indexed as [lts]x[rts]x[p-1]
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorUInvRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, upInv, totm, peArr, sizeOfPE, q);
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
extern "C" void tensorUInvR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, upInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorUInvDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, upInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorUInvC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, upInv, totm, peArr, sizeOfPE, 0);
}
