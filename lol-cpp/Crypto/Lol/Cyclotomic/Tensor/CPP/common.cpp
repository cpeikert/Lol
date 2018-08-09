/*
Module      : common.cpp
Description : Shared functions and data.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include <cstdio>
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

void* lolAlloc(size_t size)
{
    void *ret = malloc(size);
    if (ret == NULL && size != 0) {
        fprintf(stderr, "FATAL: Out of memory");
        fflush(stderr);
        exit(1);
    }
    return ret;
}
