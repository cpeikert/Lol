/*
Module      : zq.cpp
Description : Implementation of Z_q-specific functions.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Helper functions for modular arithmetic, including computing the inverse of a
mod-q value, and obtaining the canonical representative of a Z_q element.
*/

#include "types.h"
#include "common.h"

// Compute b^{-1} mod a (not necessarily in canonical form)
// Returns 0 if b is not invertible mod a.
hInt_t reciprocal (hInt_t a, hInt_t b)
{
  // extended Euclidean algorithm, matching description from wikipedia:
  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
  // In this implementation, a an b track the 'r' sequence, t0/t1 track
  // the t sequence, which computes the inverse of b.

  hInt_t fieldSize = a;

  // t0/t1 help compute the Bezout coefficient for b (i.e., b inverse mod a)
  hInt_t t0 = 0;
  hInt_t t1 = 1;

  // Euclid's algorithm is done when we get a remainder of 0
  while (b != 0) {
    // a = b*quotient + tmp
    hInt_t quotient = a / b;
    hInt_t tmp = a % b;
    // prepare for the next iteration
    a = b;
    b = tmp;
    // store the next 't' coefficient into t1, and move the old t1 into t0
    tmp = t1;
    t1 = t0 - quotient*t1;
    t0 = tmp;
  }
  // if a (the last non-zero remainder) !=1, then b is not invertible mod a
  if(a!=1) {
    return 0;
  }

  // this actually returns EITHER the reciprocal OR reciprocal + fieldSize
  hInt_t res = t0 + fieldSize;
  return res;
}

/* Put a Z_q coefficient in the range -q < x < q into the range 0 <= x < q*/
void canonicalizeZq (Zq* y, hDim_t totm, hInt_t q) {
  // canonicalize every coefficient with this modulus
  for(hDim_t j = 0; j < totm; j++) {
    if(y[j].x<0) {
      y[j].x+=q;
    }
  }
}
