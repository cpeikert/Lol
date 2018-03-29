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

// TODO: Fix all comments that mention tupSize

/* The prime-index transform that converts decoding basis coefficients (over any
 * ring) to powerful basis coefficients.
 * 'y' is an array of decoding basis coefficients in a four-dimensional tensor:
 * [tupSize]x[lts]x[rts]x[p-1].
 * The 'tupSize' outermost dimension is handled in tensorFuserPrime, which turns
 * this prime-index transformation into an arbitrary-index transformation. Thus
 * we can think of the operator as  (I_lts \otimes L_p \otimes I_rts).
 */
template <typename ring> void lp (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t ltsOffset;
  hDim_t rtsOffset;
  int i;

  // L_2 = id
  if(p == 2) {return;}

  // each square diagonal matrix in I_lts \otimes (L_p \otimes I_rts) has
  // size rts*(p-1)
  hDim_t ltsBockSize = rts*(p-1);
  // operate on the chunk of 'y' corresponding to each matrix on the diagonal
  for (ltsOffset = 0; ltsOffset < lts; ++ltsOffset) {
    // the offset into y corresponding to the block diagonal matrix
    hDim_t blockIdx = ltsOffset*ltsBockSize;
    // operate on slices of 'y' of size 'rts'
    for (rtsOffset = 0; rtsOffset < rts; ++rtsOffset) {
      hDim_t idx1 = blockIdx + rtsOffset;       // y[ltsOffset][rtsOffset][0]
      hDim_t idx2 = blockIdx + rtsOffset + rts; // y[ltsOffset][rtsOffset][1]
      // the actual work: y_i = y_i + y_{i-1}
      for (i = 1; i < p-1; ++i) {
        y[idx2*tupSize] += y[idx1*tupSize];
        // advance the pointer by the size of the slice: rts
        idx2 += rts;
        idx1 += rts;
      }
    }
  }
}

/* The prime-index transform that converts powerful basis coefficients (over any
 * ring) to powerful basis coefficients.
 * 'y' is an array of powerful basis coefficients in a four-dimensional tensor:
 * [tupSize]x[lts]x[rts]x[p-1].
 * The 'tupSize' outermost dimension is handled in tensorFuserPrime, which turns
 * this prime-index transformation into an arbitrary-index transformation. Thus
 * we can think of the operator as  (I_lts \otimes (L_p)^{-1} \otimes I_rts).
 */
template <typename ring> void lpInv (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t ltsOffset;
  hDim_t rtsOffset;
  int i;

  // (L_2)^{-1} = id
  if(p == 2) {return;}

  // each square diagonal matrix in I_lts \otimes (L_p \otimes I_rts) has
  // size rts*(p-1)
  hDim_t ltsBockSize = rts*(p-1);
  // operate on the chunk of 'y' corresponding to each matrix on the diagonal
  for (ltsOffset = 0; ltsOffset < lts; ++ltsOffset) {
    // the offset into y corresponding to the block diagonal matrix
    hDim_t blockIdx = ltsOffset*ltsBockSize;
    // operate on slices of 'y' of size 'rts'
    for (rtsOffset = 0; rtsOffset < rts; ++ rtsOffset) {
      hDim_t tensorOffset = blockIdx + rtsOffset;
      hDim_t idx1 = tensorOffset + (p-3) * rts; // y[ltsOffset][rtsOffset][p-3]
      hDim_t idx2 = tensorOffset + (p-2) * rts; // y[ltsOffset][rtsOffset][p-2]
      // the actual work: forward direction takes adjacent sums, so the
      // reverse direction starts at the end and takes adjacent differences
      for (i = p-2; i != 0; --i) {
        y[idx2*tupSize] -= y[idx1*tupSize] ;
        // advance the pointer by the size of the slice: rts
        idx2 -= rts;
        idx1 -= rts;
      }
    }
  }
}

/* Arbitrary-index transformation that converts decoding basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * powerful basis coefficients. The input 'y' represents a four-dimensional
 * tensor indexed as [tupSize]x[lts]x[rts]x[p-1], where each component of the
 * first coordinate is with respect to the corresponding modulus in 'qs'.
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  tensorFuserPrimeNew (y, lp, totm, peArr, sizeOfPE, q);
  // Haskell expects each Z_q coefficient to be in the range 0 <= x < q_i, so
  // ensure that is the case.
  canonicalizeZqNew(y,totm,q);
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
  tensorFuserPrimeNew (y, lp, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrimeNew (y, lp, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrimeNew (y, lp, totm, peArr, sizeOfPE, 0);
}

/* Arbitrary-index transformation that converts powerful basis coefficients
 * (over a ring R mod (q1xq2x...), where the q_i's are pairwise coprime) into
 * decoding basis coefficients. The input 'y' represents a four-dimensional
 * tensor indexed as [tupSize]x[lts]x[rts]x[p-1], where each component of the
 * first coordinate is with respect to the corresponding modulus in 'qs'.
 *
 * Use "extern "C"" to avoid C++ name mangling, which makes it hard to call
 * from Haskell.
 */
extern "C" void tensorLInvRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  tensorFuserPrimeNew (y, lpInv, totm, peArr, sizeOfPE, q);
  // Haskell expects each Z_q coefficient to be in the range 0 <= x < q_i, so
  // ensure that is the case.
  canonicalizeZqNew(y,totm,q);
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
  tensorFuserPrimeNew (y, lpInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLInvDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrimeNew (y, lpInv, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorLInvC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrimeNew (y, lpInv, totm, peArr, sizeOfPE, 0);
}
