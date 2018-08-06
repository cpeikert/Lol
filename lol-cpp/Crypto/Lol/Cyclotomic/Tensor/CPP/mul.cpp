/*
Module      : mul.cpp
Description : zipWith (*).
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"

template <typename ring> void zipWithStar (ring* a, ring* b, hDim_t totm, hInt_t q)
{
  for(int i = 0; i < totm; i++) {
    a[i] *= b[i];
  }
}

//a = zipWith (*) a b
extern "C" void mulRq (Zq* a, Zq* b, hDim_t totm, hInt_t q)
{
  Zq::q = q;
  zipWithStar(a, b, totm, q);
}

extern "C" void mulC (Complex* a, Complex* b, hDim_t totm)
{
  zipWithStar(a, b, totm, 0);
}

extern "C" void mulDouble (double* a, double* b, hDim_t totm)
{
  zipWithStar(a, b, totm, 0);
}
