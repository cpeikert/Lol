
#include <math.h>
#include <stdlib.h>
#include "tensorTypes.h"

// this function takes *inverse* RUs, so no negation is needed on the indexing
// I had been negating the ru-idx, but this was causing a *negative* mod, resulting in a hard-to-find bug
void primeD (double *y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, complex_t* ruinv)
{
	if(p == 2)
  {
      return;
  }
  hDim_t blockOffset, modOffset, tensorOffset;
	double *tempSpace = (double*)malloc((p-1)*sizeof(double));
  hDim_t temp1 = rts*(p-1);
  for(blockOffset = 0; blockOffset < lts; blockOffset++)
  {
    hDim_t temp2 = blockOffset*temp1;
    for(modOffset = 0; modOffset < rts; modOffset++)
    {
      tensorOffset = temp2 + modOffset;
      hDim_t row, col;
      
      for(row = 0; row < p-1; row++)
      {
        double acc = 0;
        for(col = 1; col <= (p>>1); col++)
        {
          acc += 2 * ruinv[((row*col) % p)*rustride].real * y[tensorOffset+rts*(col-1)];
        }
        for(col = (p>>1)+1; col <= p-1; col++)
        {
          acc += 2 * ruinv[((row*col) % p)*rustride].imag * y[tensorOffset+rts*(col-1)];
        }
        tempSpace[row] = acc/sqrt(2);
      }
      
      for(row = 0; row < p-1; row++)
      {
        y[tensorOffset+rts*row] = tempSpace[row];
      }
    }
  }
  free(tempSpace);
}

void ppD (void *y, hDim_t lts, hDim_t rts, PrimeExponent pe, void *ruinv, hInt_t q)
{
    hDim_t p = pe.prime;
    hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
    ASSERT(e != 0);
#endif
    hDim_t mprime = ipow(p,e-1);
    primeD (y, lts*mprime, rts, p, mprime, (complex_t*)ruinv);
}

//the contents of y will be destroyed, but should be initialized in Haskell-land to independent Guassians over the reals
void tensorGaussianDec (double *y, hDim_t totm, PrimeExponent *peArr, hShort_t sizeOfPE, complex_t** ruinv)
{
  void** ruinvs = (void**)malloc(sizeOfPE*sizeof(void*));
  hShort_t i;
  for(i = 0; i < sizeOfPE; i++)
  {
      ruinvs[i] = (void*) (ruinv[i]);
  }
    
	tensorFuserCRT (y, ppD, totm, peArr, sizeOfPE, ruinvs, 0);
	
	free(ruinvs);
}