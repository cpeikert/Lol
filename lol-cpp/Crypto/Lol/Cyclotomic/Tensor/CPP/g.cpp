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
  hDim_t blockOffset, modOffset;
  hDim_t i;
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp1 = blockOffset * (p-1)*rts;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp1 + modOffset;
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      abgrp first = y[tensorOffset];
      // the actual work, stepping forwards: y_i = y_1 - y_{i+1}
      for (i = 0; i < p-2; ++i) {
        hDim_t idx = tensorOffset + i*rts;
        y[idx] += (first - y[idx+rts]);
      }
      // last += first
      y[tensorOffset + (p-2)*rts] += first;
    }
  }
}

template <typename abgrp> void gDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp1 = blockOffset * (p-1)*rts;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      hDim_t tensorOffset = tmp1 + modOffset;
      // By the end of the for loop, acc will be Σ_{i=1}^{p-2} y_i
      abgrp acc;
      acc = 0;
      // y_i = y_i - y_{i+1}
      for (i = 0; i != p-2; ++i) {
        hDim_t idx = tensorOffset + i * rts;
        acc += y[idx];
        y[idx] -= y[idx+rts];
      }
      // last = acc + 2*last
      hDim_t last_idx = tensorOffset + (p-2)*rts;
      y[last_idx] += (acc + y[last_idx]);
    }
  }
}

template <typename abgrp> void gInvPow (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t blockOffset, modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp1 = blockOffset * (p-1)*rts;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp1 + modOffset;
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      abgrp relts;
      relts = 0;
      // relts = Σ y_i
      for (i = 0; i < p-1; ++i) {
        relts += y[tensorOffset + i*rts];
      }
      abgrp lelts;
      lelts = 0;
      // How to compute the new value y'_i:
      // y'_i = i*Σ_{j=i}^{p-1} y_j + (i-1-p)*Σ_{j=1}^{i-1} y_j
      // That is, i*right_coeffs + (i-1-p)*left_coeffs. That's what the below algorithm does,
      // albeit 0-indexed.
      for (i = 0; i != p-1; ++i) {
        hDim_t idx = tensorOffset + i*rts;
        abgrp z = y[idx];
        y[idx] = relts*(i+1) - lelts*(p-1-i);
        relts -= z;
        lelts += z;
      }
    }
  }
}

template <typename abgrp> void gInvDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp1 = blockOffset* (p-1)*rts;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      // The vector we're working with appears as a column in a matrix. The vector is
      // y[tensorOffset], y[tensorOffset+rts], y[tensorOffset+2*rts], ..., y[tensorOffset+(p-2)*rts]
      hDim_t tensorOffset = tmp1 + modOffset;
      abgrp acc;
      // acc = Σ_{i=1}^{p-1} (p-i)*y_i
      acc = 0;
      for (i=0; i != p-1; ++i) {
        acc += (y[tensorOffset + i*rts] * (p-1-i));
      }
      // How to compute the new value y'_i:
      // y'_i = -Σ_{j=1}^{j=i-1} j*y_j + Σ_{j=i}^{p-1} (p-j)*y_j
      // The LHS is 0 to begin with. We remove terms from the RHS and add to the LHS by subtracting
      // off from acc.
      for (i=0; i != p-1; ++i) {
        hDim_t idx = tensorOffset + i*rts;
        abgrp tmp = acc;
        acc -= (y[idx] * p);
        y[idx] = tmp;
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
