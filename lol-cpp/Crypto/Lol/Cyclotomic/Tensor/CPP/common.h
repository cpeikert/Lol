/*
Module      : common.h
Description : Shared functions and data.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#ifndef COMMON_H_
#define COMMON_H_

#include <cstddef>
#include "types.h"

// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);

// thin wrapper around malloc
void* lolAlloc(size_t size);

#endif /* COMMON_H_ */
