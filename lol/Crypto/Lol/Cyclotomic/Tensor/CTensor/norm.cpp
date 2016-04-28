#include "tensorTypes.h"

template <typename ring> void pNormSq (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
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
        sum += y[(tensorOffset + i*rts)*tupSize];
      }
      for (i = 0; i < p-1; ++i) {
        y[(tensorOffset + i*rts)*tupSize] += sum;
      }
    }
  }
}

extern "C" void tensorNormSqR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  hInt_t* tempSpace = (hInt_t*)malloc(totm*tupSize*sizeof(hInt_t));
  for(hDim_t i = 0; i < totm*tupSize; i++) {
    tempSpace[i]=y[i];
  }

  tensorFuserPrime(y, tupSize, pNormSq, totm, peArr, sizeOfPE, (hInt_t*)0);

  //do dot product and return in index 0
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    hInt_t dotprod = 0;
    for(hDim_t i = 0; i < totm; i++) {
      dotprod += (tempSpace[i*tupSize+tupIdx]*y[i*tupSize+tupIdx]);
    }

    y[tupIdx] = dotprod;
  }

  free(tempSpace);
}

extern "C" void tensorNormSqD (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
  double* tempSpace = (double*)malloc(totm*tupSize*sizeof(double));
  for(hDim_t i = 0; i < totm*tupSize; i++) {
    tempSpace[i]=y[i];
  }
  tensorFuserPrime(y, tupSize, pNormSq, totm, peArr, sizeOfPE, (hInt_t*)0);

  //do dot product and return in index 0
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    double dotprod = 0;
    for(hDim_t i = 0; i < totm; i++) {
      dotprod += (tempSpace[i*tupSize+tupIdx]*y[i*tupSize+tupIdx]);
    }

    y[tupIdx] = dotprod;
  }

  free(tempSpace);
}