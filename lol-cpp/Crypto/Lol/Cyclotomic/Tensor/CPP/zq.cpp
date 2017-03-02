/*
Module      : zq.cpp
Description : Implementation of Z_q-specific functions.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"
#include "common.h"

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
  // if a!=1, then b is not invertible mod a
  if(a!=1) {
    return 0;
  }

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