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

template <typename ring> void gPow (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t tmp1 = rts*(p-1);
  hDim_t tmp2 = tmp1 - rts;
  hDim_t blockOffset, modOffset;
  hDim_t i;
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp3 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp3 + modOffset;
      ring last = y[(tensorOffset + tmp2)*tupSize];
      for (i = p-2; i != 0; --i) {
        hDim_t idx = tensorOffset + i * rts;
        y[idx*tupSize] += (last - y[(idx-rts)*tupSize]);
      }
      y[tensorOffset*tupSize] += last;
    }
  }
}

template <typename ring> void gDec (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t tmp1 = rts*(p-1);
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring acc = y[tensorOffset*tupSize];
      for (i = p-2; i != 0; --i) {
        hDim_t idx = tensorOffset + i * rts;
        acc += y[idx*tupSize];
        y[idx*tupSize] -= y[(idx-rts)*tupSize];
      }
      y[tensorOffset*tupSize] += acc;
    }
  }
}

template <typename ring> void gInvPow (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t tmp1 = rts * (p-1);
  hDim_t blockOffset, modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring lelts;
      lelts = 0;
      for (i = 0; i < p-1; ++i) {
        lelts += y[(tensorOffset + i*rts)*tupSize];
      }
      ring relts;
      relts = 0;
      for (i = p-2; i >= 0; --i) {
        hDim_t idx = tensorOffset + i*rts;
        ring z = y[idx*tupSize];
        ring lmul, rmul;
        lmul = p-1-i;
        rmul = i+1;
        y[idx*tupSize] = lmul * lelts - rmul * relts;
        lelts -= z;
        relts += z;
      }
    }
  }
}

template <typename ring> void gInvDec (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  if (p == 2) {return;}
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;
  hDim_t tmp1 = rts*(p-1);

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring lastOut;
      lastOut = 0;
      for (i=1; i < p; ++i) {
        ring ri;
        ri = i;
        lastOut += (ri * y[(tensorOffset + (i-1)*rts)*tupSize]);
      }
      ring rp;
      rp = p;
      ring acc = lastOut;
      for (i = p-2; i > 0; --i) {
        hDim_t idx = tensorOffset + i*rts;
        ring tmp = acc;
        acc -= y[idx*tupSize]*rp;
        y[idx*tupSize] = tmp;
      }
      y[tensorOffset*tupSize] = acc;
    }
  }
}

extern "C" void tensorGPowR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gPow, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGPowRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gPow, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}

extern "C" void tensorGPowC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gPow, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGDecR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gDec, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGDecRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gDec, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}

extern "C" void tensorGDecC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gDec, totm, peArr, sizeOfPE, (hInt_t*)0);
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

extern "C" hShort_t tensorGInvPowR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < tupSize*totm; i++) {
    if (y[i] % oddrad) {
      y[i] /= oddrad;
    }
    else {
      return 0;
    }
  }
  return 1;
}

extern "C" hShort_t tensorGInvPowRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, qs);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < tupSize; i++) {
    Zq::q = qs[i]; // global update
    hInt_t ori = reciprocal(Zq::q, oddrad);
    Zq oddradInv;
    oddradInv = ori;
    if (ori == 0) {
      return 0; // error condition
    }
    for(hDim_t j = 0; j < totm; j++) {
      y[j*tupSize+i] *= oddradInv;
    }
  }

  canonicalizeZq(y,tupSize,totm,qs);
  return 1;
}

extern "C" hShort_t tensorGInvPowC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);
  Complex oddradInv;
  oddradInv = 1 / oddrad;
  for(int i = 0; i < tupSize*totm; i++) {
    y[i] *= oddradInv;
  }
  return 1;
}

extern "C" hShort_t tensorGInvDecR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvDec, totm, peArr, sizeOfPE, (hInt_t*)0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < tupSize*totm; i++) {
    if (y[i] % oddrad) {
      y[i] /= oddrad;
    }
    else {
      return 0;
    }
  }
  return 1;
}

extern "C" hShort_t tensorGInvDecRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gInvDec, totm, peArr, sizeOfPE, qs);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);

  for(int i = 0; i < tupSize; i++) {
    Zq::q = qs[i]; // global update
    hInt_t ori = reciprocal(Zq::q, oddrad);
    Zq oddradInv;
    oddradInv = ori;
    if (ori == 0) {
      return 0; // error condition
    }
    for(hDim_t j = 0; j < totm; j++) {
      y[j*tupSize+i] *= oddradInv;
    }
  }

  canonicalizeZq(y,tupSize,totm,qs);
  return 1;
}

extern "C" hShort_t tensorGInvDecC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvDec, totm, peArr, sizeOfPE, (hInt_t*)0);

  hInt_t oddrad = oddRad(peArr, sizeOfPE);
  Complex oddradInv;
  oddradInv = 1 / oddrad;
  for(int i = 0; i < tupSize*totm; i++) {
    y[i] *= oddradInv;
  }
  return 1;
}