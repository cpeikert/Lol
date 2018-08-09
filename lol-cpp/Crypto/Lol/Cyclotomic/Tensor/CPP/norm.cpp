/*
Module      : norm.cpp
Description : Compute g*norm(x)^2.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"
#include "tensor.h"

template <typename ring> void pNormSq (ring* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;

  if(p==2) {return;}

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring sum = 0;
      for (i = 0; i < p-1; ++i) {
        sum += y[(tensorOffset + i*rts)];
      }
      for (i = 0; i < p-1; ++i) {
        y[(tensorOffset + i*rts)] += sum;
      }
    }
  }
}

extern "C" void tensorNormSqR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  hInt_t* tempSpace = (hInt_t*)lolAlloc(totm*sizeof(hInt_t));
  for(hDim_t i = 0; i < totm; i++) {
    tempSpace[i]=y[i];
  }

  tensorFuserPrime(y, pNormSq, totm, peArr, sizeOfPE, 0);

  //do dot product and return in index 0
  hInt_t dotprod = 0;
  for(hDim_t i = 0; i < totm; i++) {
    dotprod += (tempSpace[i]*y[i]);
  }

  y[0] = dotprod;

  free(tempSpace);
}

extern "C" void tensorNormSqD (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  double* tempSpace = (double*)lolAlloc(totm*sizeof(double));
  for(hDim_t i = 0; i < totm; i++) {
    tempSpace[i]=y[i];
  }
  tensorFuserPrime(y, pNormSq, totm, peArr, sizeOfPE, 0);

  //do dot product and return in index 0
  double dotprod = 0;
  for(hDim_t i = 0; i < totm; i++) {
    dotprod += (tempSpace[i]*y[i]);
  }

  y[0] = dotprod;

  free(tempSpace);
}
