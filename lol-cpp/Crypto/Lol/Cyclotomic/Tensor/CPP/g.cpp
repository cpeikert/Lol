/*
Module      : g.cpp
Description : Multiplication and division by 'g' in different bases.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"
#include "tensor.h"
#include "common.h"

template <typename abgrp> void gPow (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      // The new value y'_i = y_i + y_{p-1} - y_{i-1} where i > 0
      abgrp y_last = y[ridx + (p-2)*rts];
      for (hDim_t off = ridx + (p-2)*rts; off > ridx; off -= rts) {
        y[off] += (y_last - y[off-rts]);
      }
      // y_0 = y_0 + y_{p-2}
      y[ridx] += y_last;
    }
  }
}

template <typename abgrp> void gDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      abgrp sum = y[ridx];
      // The new value y'_i = y_i - y_{i-1} where i > 0
      for (hDim_t off = ridx + (p-2)*rts; off > ridx; off -= rts) {
        sum += y[off];
        y[off] -= y[off-rts];
      }
      // At this point, sum = Σ y_i
      // y_0 = y_0 + Σ y_i
      y[ridx] += sum;
    }
  }
}

template <typename abgrp> void gInvPow (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      // The new value y'_i = (p-i+1) * Σ_{j=1}^i y_j - i *
      // Σ_{j=i+1}^{p-1} y_j We do this by letting sum = Σ y_i and
      // having acc = sum at first. On each iteration, we let acc +=
      // (sum - p*y_i)
      abgrp sum;
      sum = 0;
      for (hDim_t off = ridx; off < ridx + (p-1)*rts; off += rts) {
        sum += y[off];
      }
      abgrp acc = sum;
      for (hDim_t off = ridx + (p-2)*rts; off >= ridx; off -= rts) {
        abgrp tmp = y[off] * p;
        y[off] = acc;
        acc += (sum - tmp);
      }
    }
  }
}

template <typename abgrp> void gInvDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      // sum = Σ (i+1)*y_i, i=0, ..., p-2
      abgrp sum;
      sum = 0;
      for (hDim_t i = 0, off = ridx; i < p-1; ++i, off += rts) {
        sum += (y[off] * (i+1));
      }
      // The new value y'_i = Σ j*y_j - p*Σ{j=i+1}^{p-1} y_j
      // We do this by setting y'_i = sum and then letting sum -= p*y_i
      for (hDim_t off = ridx + (p-2)*rts; off >= ridx; off -= rts) {
        abgrp tmp = y[off];
        y[off] = sum;
        sum -= (tmp * p);
      }
    }
  }
}

extern "C" void tensorGPowR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gPow, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorGPowRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime(y, gPow, totm, peArr, sizeOfPE, q);
  canonicalizeZq(y,totm,q);
}

extern "C" void tensorGPowDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gPow, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorGPowC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gPow, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorGDecR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gDec, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorGDecRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, gDec, totm, peArr, sizeOfPE, q);
  canonicalizeZq(y,totm,q);
}

extern "C" void tensorGDecDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gDec, totm, peArr, sizeOfPE, 0);
}

extern "C" void tensorGDecC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gDec, totm, peArr, sizeOfPE, 0);
}

hInt_t oddRad(PrimeExponent* peArr, hShort_t sizeOfPE) {
  hInt_t oddrad;
  oddrad = 1;
  for(int i = 0; i < sizeOfPE; i++) {
    hShort_t p = peArr[i].prime;
    if (p != 2) {
      oddrad *= peArr[i].prime;
    }
  }
  return oddrad;
}

extern "C" hShort_t tensorGInvPowR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvPow, totm, peArr, sizeOfPE, 0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < totm; i++) {
    if (y[i] % oddrad == 0) {
      y[i] /= oddrad;
    }
    else {
      return 0;
    }
  }
  return 1;
}

extern "C" hShort_t tensorGInvPowRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, gInvPow, totm, peArr, sizeOfPE, q);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  hInt_t ori = reciprocal(Zq::q, oddrad);
  Zq oddradInv;
  oddradInv = ori;
  if (ori == 0) {
    return 0; // error condition
  }
  for(hDim_t j = 0; j < totm; j++) {
    y[j] *= oddradInv;
  }

  canonicalizeZq(y,totm,q);
  return 1;
}

extern "C" hShort_t tensorGInvPowDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvPow, totm, peArr, sizeOfPE, 0);

  double oddrad = (double)oddRad(peArr, sizeOfPE);

  for(int i = 0; i < totm; i++) {
    y[i] /= oddrad;
  }
  return 1;
}

extern "C" hShort_t tensorGInvPowC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvPow, totm, peArr, sizeOfPE, 0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);
  Complex oddradC = Complex((double)oddrad, 0.0);

  for(int i = 0; i < totm; i++) {
    y[i] /= oddradC;
  }
  return 1;
}

extern "C" hShort_t tensorGInvDecR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvDec, totm, peArr, sizeOfPE, 0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < totm; i++) {
    if (y[i] % oddrad == 0) {
      y[i] /= oddrad;
    }
    else {
      return 0;
    }
  }
  return 1;
}

extern "C" hShort_t tensorGInvDecRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
  Zq::q = q;
  tensorFuserPrime (y, gInvDec, totm, peArr, sizeOfPE, q);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  hInt_t ori = reciprocal(Zq::q, oddrad);
  Zq oddradInv;
  oddradInv = ori;
  if (ori == 0) {
    return 0; // error condition
  }
  for(hDim_t j = 0; j < totm; j++) {
    y[j] *= oddradInv;
  }

  canonicalizeZq(y,totm,q);
  return 1;
}

extern "C" hShort_t tensorGInvDecDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvDec, totm, peArr, sizeOfPE, 0);

  double oddrad = (double)oddRad(peArr, sizeOfPE);

  for(int i = 0; i < totm; i++) {
    y[i] /= oddrad;
  }
  return 1;
}

extern "C" hShort_t tensorGInvDecC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, gInvDec, totm, peArr, sizeOfPE, 0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);
  Complex oddradC = Complex((double)oddrad, 0.0);

  for(int i = 0; i < totm; i++) {
    y[i] /= oddradC;
  }
  return 1;
}
