/*
Module      : rrq.cpp
Description : Implementation of Z_q-specific functions.
Copyright   : (c) Chris Peikert, 2018
License     : GPL-3
Maintainer  : cpeikert@alum.mit.edu
Stability   : experimental
Portability : POSIX

A helper function for arithmetic in R/qZ: obtains the canonical representative of a RR_q element.
*/

#include "types.h"

// Put a RR_q coefficient in the range -1 < x < 1 into the range 0 <= x < 1
void canonicalizeRRq (RRq* y, hDim_t totm) {
  // canonicalize every coefficient with this modulus
  for (hDim_t i = 0; i < totm; i++) {
    if (y[i].x < 0) {
      y[i].x += 1;
    }
  }
}
