/*
Module      : tensor.h
Description : Templates for the tensor DSL.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#ifndef TENSOR_CPP_
#define TENSOR_CPP_

#include "types.h"
#include "common.h"
#ifdef __cplusplus
template <typename ring>
using primeFunc = void (*) (ring*, hShort_t, hDim_t, hDim_t, hDim_t);

template <typename ringy, typename ringru>
using primeCRTFunc = void (*) (ringy*, hShort_t, hDim_t, hDim_t, PrimeExponent, ringru*);

//for square transforms
template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, primeFunc<ring> f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
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

template <typename ringy, typename ringru> void tensorFuserCRT (ringy* y, hShort_t tupSize, primeCRTFunc<ringy,ringru> f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ringru** ru, hInt_t* qs)
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
#endif /* __cplusplus */
#endif /* TENSOR_CPP_ */
