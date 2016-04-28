#include "tensorTypes.h"

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
      ring acc = lastOut / rp;
      ASSERT ((acc * rp) == lastOut);  // this line asserts that lastOut % p == 0, without calling % operator
      for (i = p-2; i > 0; --i) {
        hDim_t idx = tensorOffset + i*rts;
        ring tmp = acc;
        acc -= y[idx*tupSize]; // we already divided acc by p, do not multiply y[idx] by p
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

extern "C" void tensorGInvPowR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGInvPowRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}

extern "C" void tensorGInvPowC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGInvDecR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, gInvDec, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorGInvDecRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, gInvDec, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}
