/*
Module      : common.cpp
Description : Shared functions and data.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"

hInt_t Zq::q; // should be in zq.cpp; here due to GHC #12152

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
