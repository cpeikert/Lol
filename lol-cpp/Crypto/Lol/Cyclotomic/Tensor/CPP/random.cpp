/*
Module      : random.cpp
Description : Gaussian sampling on a lattice.
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
#include <math.h>

// I had been negating the ru-idx, but this was causing a *negative* mod, resulting in a hard-to-find bug
// current behavior (taking rus, rather than ruInv) matches RT
void primeD (double *y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, Complex* ru)
{
	if(p == 2) {
    return;
  }
  hDim_t blockOffset, modOffset, tensorOffset;
	double *tempSpace = (double*)malloc((p-1)*sizeof(double));
  hDim_t temp1 = rts*(p-1);
  for(blockOffset = 0; blockOffset < lts; blockOffset++) {
    hDim_t temp2 = blockOffset*temp1;
    for(modOffset = 0; modOffset < rts; modOffset++) {
      tensorOffset = temp2 + modOffset;
      hDim_t row, col;

      for(row = 0; row < p-1; row++) {
        double acc = 0;
        for(col = 1; col <= (p>>1); col++) {
          acc += 2 * ru[((row*col) % p)*rustride*tupSize].real * y[(tensorOffset+rts*(col-1))*tupSize];
        }
        for(col = (p>>1)+1; col <= p-1; col++) {
          acc += 2 * ru[((row*col) % p)*rustride*tupSize].imag * y[(tensorOffset+rts*(col-1))*tupSize];
        }
        tempSpace[row] = acc/sqrt(2);
      }

      for(row = 0; row < p-1; row++) {
        y[(tensorOffset+rts*row)*tupSize] = tempSpace[row];
      }
    }
  }
  free(tempSpace);
}

void ppD (double *y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, Complex *ru)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
  hDim_t mprime = ipow(p,e-1);
  primeD (y, tupSize, lts*mprime, rts, p, mprime, ru);
}

//the contents of y will be destroyed, but should be initialized in Haskell-land to independent Guassians over the reals
extern "C" void tensorGaussianDec (hShort_t tupSize, double *y, hDim_t totm, PrimeExponent *peArr, hShort_t sizeOfPE, Complex** ru)
{
	tensorFuserCRT (y, tupSize, ppD, totm, peArr, sizeOfPE, ru, (hInt_t*)0);
}