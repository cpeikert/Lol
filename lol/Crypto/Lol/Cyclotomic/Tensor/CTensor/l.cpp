#include "tensorTypes.h"

template <typename ring> void lp (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  if(p == 2) {return;}

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t idx = tmp2 + modOffset + rts;
      for (i = 1; i < p-1; ++i) {
        y[idx*tupSize] += y[(idx-rts)*tupSize];
        idx += rts;
      }
    }
  }
}

template <typename ring> void lpInv (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  if(p == 2) {return;}

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++ modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      hDim_t idx = tensorOffset + (p-2) * rts;
      for (i = p-2; i != 0; --i) {
        y[idx*tupSize] -= y[(idx-rts)*tupSize] ;
        idx -= rts;
      }
    }
  }
}

extern "C" void tensorLRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, lp, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}

extern "C" void tensorLR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lp, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorLDouble (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lp, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorLC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lp, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorLInvRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  tensorFuserPrime (y, tupSize, lpInv, totm, peArr, sizeOfPE, qs);
  canonicalizeZq(y,tupSize,totm,qs);
}

extern "C" void tensorLInvR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lpInv, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorLInvDouble (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lpInv, totm, peArr, sizeOfPE, (hInt_t*)0);
}

extern "C" void tensorLInvC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  tensorFuserPrime (y, tupSize, lpInv, totm, peArr, sizeOfPE, (hInt_t*)0);
}
