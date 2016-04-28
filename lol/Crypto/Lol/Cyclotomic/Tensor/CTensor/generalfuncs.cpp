#include "tensorTypes.h"

//see note in zq.cc
hInt_t Zq::q;

hDim_t ipow(hDim_t base, hShort_t exp)
{
	hDim_t result = 1;
  while (exp) {
    if (exp & 1) {
      result *= base;
    }
    exp >>= 1;
    base *= base;
  }
  return result;
}

// a is the field size. we are looking for reciprocal of b
hInt_t reciprocal (hInt_t a, hInt_t b)
{
	hInt_t fieldSize = a;

	hInt_t y = 1;
	hInt_t lasty = 0;
	while (b != 0) {
		hInt_t quotient = a / b;
		hInt_t tmp = a % b;
		a = b;
		b = tmp;
		tmp = y;
		y  = lasty - quotient*y;
		lasty = tmp;
	}
	ASSERT (a==1);  // if this one fails, then b is not invertible mod a

	// this actually returns EITHER the reciprocal OR reciprocal + fieldSize
	hInt_t res = lasty + fieldSize;
	return res;
}

void canonicalizeZq (Zq* y, hShort_t tupSize, hDim_t totm, hInt_t* qs) {
  for(int tupIdx = 0; tupIdx<tupSize; tupIdx++) {
    hInt_t q = qs[tupIdx];
    for(hDim_t j = 0; j < totm; j++) {
      if(y[j*tupSize+tupIdx].x<0) {
        y[j*tupSize+tupIdx].x+=q;
      }
    }
  }
}


//for square transforms
template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, void (*f) (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      if(qs) {
        Zq::q = qs[tupIdx]; // global update
      }
      (*f) (y+tupIdx, tupSize, lts*ipow_pe, rts, pe.prime);
    }
    rts *= dim;
  }
}

template void tensorFuserPrime (Zq* y, hShort_t tupSize, void (*f) (Zq* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template void tensorFuserPrime (hInt_t* y, hShort_t tupSize, void (*f) (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template void tensorFuserPrime (Complex* y, hShort_t tupSize, void (*f) (Complex* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template void tensorFuserPrime (double* y, hShort_t tupSize, void (*f) (double* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t pe), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

template <typename ringy, typename ringru> void tensorFuserCRT (ringy* y, hShort_t tupSize, void (*f) (ringy* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, ringru* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ringru** ru, hInt_t* qs)
{
  hDim_t lts = totm;
  hDim_t rts = 1;
  hShort_t i;

  for (i = 0; i < sizeOfPE; ++i) {
    PrimeExponent pe = peArr[i];
    hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
    hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
    lts /= dim;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      if(qs) {
        Zq::q = qs[tupIdx]; // global update
      }
      (*f) (y+tupIdx, tupSize, lts, rts, pe, ru[i]+tupIdx);
    }
    rts *= dim;
  }
}
template void tensorFuserCRT (Zq* y, hShort_t tupSize, void (*f) (Zq* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, Zq* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Zq** ru, hInt_t* qs);
template void tensorFuserCRT (Complex* y, hShort_t tupSize, void (*f) (Complex* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, Complex* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Complex** ru, hInt_t* qs);
template void tensorFuserCRT (double* y, hShort_t tupSize, void (*f) (double* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, Complex* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Complex** ru, hInt_t* qs);
