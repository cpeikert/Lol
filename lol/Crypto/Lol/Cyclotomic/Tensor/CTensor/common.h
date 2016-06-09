#ifndef COMMON_H_
#define COMMON_H_

#include <stdio.h>
#include <stdlib.h>
#include "types.h"

#define ASSERT(EXP) { \
  if (!(EXP)) { \
    fprintf (stderr, "Assertion in file '%s' line %d : " #EXP "  is false\n", __FILE__, __LINE__); \
    exit(-1); \
  } \
}

// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);

#endif /* COMMON_H_ */
