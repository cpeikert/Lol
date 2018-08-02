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
      // the actual work, stepping backwards
      hDim_t off = ridx + (p-2)*rts;
      abgrp last = y[off];
      while(off > ridx) {
        hDim_t newoff = off-rts;
        y[off] += (last - y[newoff]);
        off = newoff;
      }
      y[ridx] += last;
    }
  }
}

template <typename abgrp> void gDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      abgrp acc, last;
      hDim_t off = ridx + (p-2)*rts;
      acc = last = y[off];
      while(off > ridx + rts) {
        hDim_t newoff = off - rts;
        y[off] -= y[newoff];
        off = newoff;
        acc += y[off];
      }
      y[off] += acc + y[ridx];
      y[ridx] -= last;
    }
  }
}

template <typename abgrp> void gInvPow (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      abgrp sum;
      sum = 0;
      for (hDim_t i = 0, off = ridx; i < p-1; ++i, off += rts) {
        sum += y[off];
      }
      abgrp acc, relts;
      acc = sum;
      relts = 0;
      for(hDim_t off = ridx + (p-2)*rts; off >= ridx; off -= rts) {
        abgrp z = y[off] * p;
        y[off] = acc - relts;
        relts += z;
        acc += sum;
      }
    }
  }
}

template <typename abgrp> void gInvDec (abgrp* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}

  for (hDim_t lblock = 0, lidx = 0; lblock < lts; ++lblock, lidx += (p-1)*rts) {
    for (hDim_t rblock = 0, ridx = lidx; rblock < rts; ++rblock, ++ridx) {
      abgrp acc = y[ridx] * (p-1);
      for (hDim_t i=1, off = ridx + rts; i < p-1; ++i, off += rts) {
        acc += y[off] * i;
      }

      abgrp sub = y[ridx] * p;
      y[ridx] = acc;

      for (hDim_t i = p-2, off = ridx + (p-2)*rts; i > 0; --i, off -= rts) {
        acc -= sub;
        sub = y[off] * p;
        y[off] = acc;
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
