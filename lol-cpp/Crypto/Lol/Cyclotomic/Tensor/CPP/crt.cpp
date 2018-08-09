/*
Module      : crt.cpp
Description : Chinese remainder transform.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX
*/

#include "types.h"
#include "tensor.h"
#include "common.h"

// If this macro is modified, make sure to update all functions below with
// cascading if/else statments so that temp space is allocated when necessary
// (i.e., for all primes >= DFTP_GENERIC_SIZE)
#define DFTP_GENERIC_SIZE 11

hDim_t bitrev (PrimeExponent pe, hDim_t j) {
  hShort_t e;
  hDim_t p = pe.prime;
  hDim_t tempj = j;
  hDim_t acc = 0;

  for(e = pe.exponent-1; e >= 0; e--) {
    div_t qr = div(tempj,p);
    acc += qr.rem * ipow(p,e);
    tempj = qr.quot;
  }
  return acc;
}

template <typename ring> void crtTwiddle (ring* y, hDim_t lts, hDim_t rts, PrimeExponent pe, ring* ru)
{
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  pe.exponent -= 1; // used for an argument to bitrev

  if(p == 2) {
    hDim_t mprime = 1<<(e-1);
    hDim_t blockDim = rts*mprime; // size of block in block diagonal tensor matrix

    for(hDim_t i0 = 1; i0 < mprime; i0++) { // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
      hDim_t temp2 = i0*rts;
      ring twid = ru[bitrev(pe, i0)];

      for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++) {
        hDim_t temp3 = blockIdx*blockDim + temp2;
        for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          hDim_t idx = (temp3 + modOffset);
          y[idx] *= twid;
        }
      }
    }
  }
  else { // This loop is faster, probably due to the division in the loop above.
  // cilk also slows it down
    hDim_t mprime = ipow(p,e-1);
    hDim_t blockDim = rts*(p-1)*mprime; // size of block in block diagonal tensor matrix

    for(hDim_t i0 = 1; i0 < mprime; i0++) { // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
      hDim_t temp1 = i0*(p-1);
      for(hDim_t i1 = 0; i1 < (p-1); i1++) { // loops over i%(p-1) for i = 0..(m'-1)
        hDim_t temp2 = (temp1+i1)*rts;
        ring twid = ru[bitrev(pe, i0)*(i1+1)];

        for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++) {
          hDim_t temp3 = blockIdx*blockDim + temp2;
          for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
            hDim_t idx = (temp3 + modOffset);
            y[idx] *= twid;
          }
        }
      }
    }
  }
}

// dim is power of p
template <typename ring> void dftTwiddle (ring* y, hDim_t lts, hDim_t rts,
                 PrimeExponent pe, hDim_t dim, hDim_t rustride, ring* ru)
{
  hDim_t idx;
  hDim_t p = pe.prime;

  pe.exponent -= 1; // used for an argument to bitrev

  if(p == 2) {
    hDim_t mprime = dim>>1; // divides evenly
    hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
    for(hDim_t i0 = 1; i0 < mprime; i0++) { // loops over i/p for i = 0..(dim-1), but we skip i0=0
      hDim_t temp3 = rts*(i0*p+1);
      ring twid = ru[bitrev(pe,i0)*rustride];

      for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
        hDim_t temp2 = blockOffset*temp1 + temp3;
        for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          idx = (temp2 + modOffset);
          y[idx] *= twid;
        }
      }
    }
  }
  else {
    hDim_t mprime = dim/p; // divides evenly
    hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
    for(hDim_t i0 = 1; i0 < mprime; i0++) { // loops over i/p for i = 0..(dim-1), but we skip i0=0
      for(hDim_t i1 = 1; i1 < p; i1++) { // loops over i%p for i = 0..(dim-1), but we skip i1=0
        hDim_t temp3 = rts*(i0*p+i1);
        ring twid = ru[bitrev(pe,i0)*i1*rustride];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
          hDim_t temp2 = blockOffset*temp1 + temp3;
          for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
            idx = (temp2 + modOffset);
            y[idx] *= twid;
          }
        }
      }
    }
  }
}

//implied length of ru is rustride*p
//implied length of tempSpace is p, if p is not a special case
// temp is allowed to be NULL if p < DFTP_GENERIC_SIZE
template <typename ring> void dftp (ring* y, hDim_t lts, hDim_t rts,
             hDim_t p, hDim_t rustride, ring* ru, ring* tempSpace)
{
  hDim_t tensorOffset;

  if(p == 2) {
    hDim_t temp1 = rts<<1;

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring u = y[tensorOffset];
        ring t = y[(tensorOffset+rts)];
        y[tensorOffset] = u + t;
        y[(tensorOffset+rts)] = u - t;
      }
    }
  }
  else if(p == 3) {
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];
    hDim_t temp1 = rts*3;

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        //q is <32 bits, so we can do 3 additions without overflow
        y[tensorOffset]           += (y2 + y3);
        y[(tensorOffset+rts)]      = y1 + (ru1*y2) + (ru2*y3);
        y[(tensorOffset+(rts<<1))] = y1 + (ru2*y2) + (ru1*y3);
      }
    }
  }
  else if(p == 5) {
    hDim_t temp1 = rts*5;
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];
    ring ru3 = ru[(rustride*3)];
    ring ru4 = ru[(rustride<<2)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          tensorOffset = temp2 + modOffset;
          ring y1, y2, y3, y4, y5;
          y1 = y[tensorOffset];
          y2 = y[(tensorOffset+rts)];
          y3 = y[(tensorOffset+(rts<<1))];
          y4 = y[(tensorOffset+3*rts)];
          y5 = y[(tensorOffset+(rts<<2))];
          y[tensorOffset]           += y2 + y3 + y4 + y5;
          y[(tensorOffset+rts)]      = y1 + (ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5);
          y[(tensorOffset+(rts<<1))] = y1 + (ru2*y2) + (ru4*y3) + (ru1*y4) + (ru3*y5);
          y[(tensorOffset+rts*3)]    = y1 + (ru3*y2) + (ru1*y3) + (ru4*y4) + (ru2*y5);
          y[(tensorOffset+(rts<<2))] = y1 + (ru4*y2) + (ru3*y3) + (ru2*y4) + (ru1*y5);
      }
    }
  }
  else if(p == 7) {
    hDim_t temp1 = rts*7;
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];
    ring ru3 = ru[(rustride*3)];
    ring ru4 = ru[(rustride<<2)];
    ring ru5 = ru[(rustride*5)];
    ring ru6 = ru[(rustride*6)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, y5, y6, y7;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        y4 = y[(tensorOffset+3*rts)];
        y5 = y[(tensorOffset+(rts<<2))];
        y6 = y[(tensorOffset+rts*5)];
        y7 = y[(tensorOffset+rts*6)];
        y[tensorOffset]           += y2 +     y3 +     y4 +     y5 +     y6 +     y7;
        y[(tensorOffset+rts)]      = y1 + (ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5) + (ru5*y6) + (ru6*y7);
        y[(tensorOffset+(rts<<1))] = y1 + (ru2*y2) + (ru4*y3) + (ru6*y4) + (ru1*y5) + (ru3*y6) + (ru5*y7);
        y[(tensorOffset+rts*3)]    = y1 + (ru3*y2) + (ru6*y3) + (ru2*y4) + (ru5*y5) + (ru1*y6) + (ru4*y7);
        y[(tensorOffset+(rts<<2))] = y1 + (ru4*y2) + (ru1*y3) + (ru5*y4) + (ru2*y5) + (ru6*y6) + (ru3*y7);
        y[(tensorOffset+rts*5)]    = y1 + (ru5*y2) + (ru3*y3) + (ru1*y4) + (ru6*y5) + (ru4*y6) + (ru2*y7);
        y[(tensorOffset+rts*6)]    = y1 + (ru6*y2) + (ru5*y3) + (ru4*y4) + (ru3*y5) + (ru2*y6) + (ru1*y7);
      }
    }
  }
  else {
    hDim_t temp1 = rts*p;
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        for(hDim_t row = 0; row < p; row++) {
          tempSpace[row] = 0;
          //p is small (<< 30 bits), so we can do p additions of mod-q values without overflow
          for(hDim_t col = 0; col < p; col++) {
            tempSpace[row] += (y[(tensorOffset+col*rts)]*ru[((col*row) % p)*rustride]);
          }
        }

        for(hDim_t row = 0; row < p; row++) {
          y[(tensorOffset+rts*row)] = tempSpace[row];
        }
      }
    }
  }
}

template <typename ring> void crtp (ring* y, hDim_t lts, hDim_t rts,
             hDim_t p, hDim_t rustride, ring* ru)
{
  hDim_t tensorOffset;
  if(p == 2) {
      return;
  }
  else if(p == 3) {
    hDim_t temp1 = rts*2;
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y[tensorOffset]      += (ru1*y2);
        y[(tensorOffset+rts)] = y1 + (ru2*y2);
      }
    }
  }
  else if(p == 5) {
    hDim_t temp1 = rts*4;
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];
    ring ru3 = ru[(rustride*3)];
    ring ru4 = ru[(rustride<<2)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        y4 = y[(tensorOffset+3*rts)];

        y[tensorOffset]           += ((ru1*y2) + (ru2*y3) + (ru3*y4));
        y[(tensorOffset+rts)]      = y1 + (ru2*y2) + (ru4*y3) + (ru1*y4);
        y[(tensorOffset+(rts<<1))] = y1 + (ru3*y2) + (ru1*y3) + (ru4*y4);
        y[(tensorOffset+rts*3)]    = y1 + (ru4*y2) + (ru3*y3) + (ru2*y4);
      }
    }
  }
  else if(p == 7) {
    hDim_t temp1 = rts*6;
    ring ru1 = ru[rustride];
    ring ru2 = ru[(rustride<<1)];
    ring ru3 = ru[(rustride*3)];
    ring ru4 = ru[(rustride<<2)];
    ring ru5 = ru[(rustride*5)];
    ring ru6 = ru[(rustride*6)];
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, y5, y6;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        y4 = y[(tensorOffset+3*rts)];
        y5 = y[(tensorOffset+(rts<<2))];
        y6 = y[(tensorOffset+rts*5)];
        y[tensorOffset]           += ((ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5) + (ru5*y6));
        y[(tensorOffset+rts)]      = y1 + (ru2*y2) + (ru4*y3) + (ru6*y4) + (ru1*y5) + (ru3*y6);
        y[(tensorOffset+(rts<<1))] = y1 + (ru3*y2) + (ru6*y3) + (ru2*y4) + (ru5*y5) + (ru1*y6);
        y[(tensorOffset+rts*3)]    = y1 + (ru4*y2) + (ru1*y3) + (ru5*y4) + (ru2*y5) + (ru6*y6);
        y[(tensorOffset+(rts<<2))] = y1 + (ru5*y2) + (ru3*y3) + (ru1*y4) + (ru6*y5) + (ru4*y6);
        y[(tensorOffset+rts*5)]    = y1 + (ru6*y2) + (ru5*y3) + (ru4*y4) + (ru3*y5) + (ru2*y6);
      }
    }
  }
  else {
    ring* tempSpace = (ring*)lolAlloc((p-1)*sizeof(ring));
    hDim_t temp1 = rts*(p-1);
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;

        for(hDim_t row = 1; row < p; row++) {
          tempSpace[row-1] = 0;
          for(hDim_t col = 0; col < p-1; col++) {
            tempSpace[row-1] += (y[(tensorOffset+col*rts)]*ru[((col*row) % p)*rustride]);
          }
        }

        for(hDim_t row = 0; row < p-1; row++) {
          y[(tensorOffset+rts*row)] = tempSpace[row];
        }
      }
    }
    free(tempSpace);
  }
}

//takes inverse rus
template <typename ring> void crtpinv (ring* y, hDim_t lts, hDim_t rts,
                hDim_t p, hDim_t rustride, ring* ruinv)
{
  hDim_t tensorOffset;
  if(p == 2) {
      return;
  }
  else if(p == 3) {
    hDim_t temp1 = rts*2;
    ring ru1 = ruinv[rustride];
    ring ru2 = ruinv[(rustride<<1)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, shift;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];

        shift = (ru2*y1) + (ru1*y2);

        y[tensorOffset]      +=                 y2  - shift;
        y[(tensorOffset+rts)] = (ru1*y1) + (ru2*y2) - shift;
      }
    }
  }
  else if(p == 5) {
    hDim_t temp1 = rts*4;
    ring ru1 = ruinv[rustride];
    ring ru2 = ruinv[(rustride<<1)];
    ring ru3 = ruinv[(rustride*3)];
    ring ru4 = ruinv[(rustride<<2)];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, shift;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        y4 = y[(tensorOffset+3*rts)];

        shift = (ru4*y1) + (ru3*y2) + (ru2*y3) + (ru1*y4);

        y[tensorOffset]           +=                 y2  +      y3  +      y4  - shift;
        y[(tensorOffset+rts)]      = (ru1*y1) + (ru2*y2) + (ru3*y3) + (ru4*y4) - shift;
        y[(tensorOffset+(rts<<1))] = (ru2*y1) + (ru4*y2) + (ru1*y3) + (ru3*y4) - shift;
        y[(tensorOffset+rts*3)]    = (ru3*y1) + (ru1*y2) + (ru4*y3) + (ru2*y4) - shift;
      }
    }
  }
  else if(p == 7) {
    hDim_t temp1 = rts*6;
    ring ru1 = ruinv[rustride];
    ring ru2 = ruinv[(rustride<<1)];
    ring ru3 = ruinv[(rustride*3)];
    ring ru4 = ruinv[(rustride<<2)];
    ring ru5 = ruinv[(rustride*5)];
    ring ru6 = ruinv[(rustride*6)];
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, y5, y6, shift;
        y1 = y[tensorOffset];
        y2 = y[(tensorOffset+rts)];
        y3 = y[(tensorOffset+(rts<<1))];
        y4 = y[(tensorOffset+3*rts)];
        y5 = y[(tensorOffset+(rts<<2))];
        y6 = y[(tensorOffset+rts*5)];

        shift = (ru6*y1) + (ru5*y2) + (ru4*y3) + (ru3*y4) + (ru2*y5) + (ru1*y6);

        y[tensorOffset]           +=                 y2  +      y3  +      y4  +      y5  +      y6  - shift;
        y[(tensorOffset+rts)]      = (ru1*y1) + (ru2*y2) + (ru3*y3) + (ru4*y4) + (ru5*y5) + (ru6*y6) - shift;
        y[(tensorOffset+(rts<<1))] = (ru2*y1) + (ru4*y2) + (ru6*y3) + (ru1*y4) + (ru3*y5) + (ru5*y6) - shift;
        y[(tensorOffset+rts*3)]    = (ru3*y1) + (ru6*y2) + (ru2*y3) + (ru5*y4) + (ru1*y5) + (ru4*y6) - shift;
        y[(tensorOffset+(rts<<2))] = (ru4*y1) + (ru1*y2) + (ru5*y3) + (ru2*y4) + (ru6*y5) + (ru3*y6) - shift;
        y[(tensorOffset+rts*5)]    = (ru5*y1) + (ru3*y2) + (ru1*y3) + (ru6*y4) + (ru4*y5) + (ru2*y6) - shift;
      }
    }
  }
  else {
    ring* tempSpace = (ring*)lolAlloc((p-1)*sizeof(ring));
    hDim_t temp1 = rts*(p-1);
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring shift;
        shift = 0;
        for(hDim_t row = 0; row < p-1; row++) {
          shift += (y[(tensorOffset+row*rts)]*ruinv[(p-row-1)*rustride]);
          tempSpace[row] = 0;
          for(hDim_t col = 0; col < p-1; col++) {
            tempSpace[row] += (y[(tensorOffset+col*rts)]*ruinv[((row*(col+1)) % p)*rustride]);
          }
        }

        for(hDim_t row = 0; row < p-1; row++) {
          y[(tensorOffset+rts*row)] = tempSpace[row] - shift;
        }
      }
    }
    free(tempSpace);
  }
}

template <typename ring> void ppDFT (ring* y, hDim_t lts, hDim_t rts,
              PrimeExponent pe, hDim_t rustride, ring* ru, ring* temp)
{
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  if(e == 0) {
    return;
  }

  hDim_t primeRuStride = rustride*ipow(p,e-1);

  hShort_t i;

  hDim_t ltsScale = ipow(p,e-1);
  hDim_t rtsScale = 1;
  hDim_t twidRuStride = rustride;
  for(i = 0; i < e; i++) {
    hDim_t rtsDim = rts*rtsScale;
    dftp (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);
    dftTwiddle (y, lts, rtsDim, pe, ltsScale*p, twidRuStride, ru);

    ltsScale /= p;
    rtsScale *= p;
    twidRuStride *= p;
    pe.exponent -= 1;
  }
}

template <typename ring> void ppDFTInv (ring* y, hDim_t lts, hDim_t rts,
                 PrimeExponent pe, hDim_t rustride, ring* ru, ring* temp)
{
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  if(e == 0) {
    return;
  }
  hDim_t primeRuStride = rustride*ipow(p,e-1);

  hShort_t i;

  hDim_t ltsScale = 1;
  hDim_t rtsScale = ipow(p,e-1);
  hDim_t twidRuStride = primeRuStride;
  pe.exponent = 1;
  for(i = 0; i < e; i++) {
    hDim_t rtsDim = rts*rtsScale;
    hDim_t ltsScaleP = ltsScale*p;
    dftTwiddle (y, lts, rtsDim, pe, ltsScaleP, twidRuStride, ru);
    dftp (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);

    ltsScale = ltsScaleP;
    rtsScale /= p;
    twidRuStride /= p;
    pe.exponent += 1;
  }
}

template <typename ring> void ppcrt (ring* y, hDim_t lts, hDim_t rts,
              PrimeExponent pe, ring* ru)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
  hDim_t mprime = ipow(p,e-1);
  ring* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (ring*)lolAlloc(p*sizeof(ring));
  }

  crtp (y, lts*mprime, rts, p, mprime, ru);
  crtTwiddle (y, lts, rts, pe, ru);
  pe.exponent -= 1;
  ppDFT (y,  lts, rts*(p-1), pe, p, ru, temp);
  pe.exponent += 1;

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

template <typename ring> void ppcrtinv (ring* y, hDim_t lts, hDim_t rts,
                 PrimeExponent pe, ring* ru)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
  hDim_t mprime = ipow(p,e-1);
  ring* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (ring*)lolAlloc(p*sizeof(ring));
  }

  pe.exponent -= 1;
  ppDFTInv (y, lts, rts*(p-1), pe, p, ru, temp);
  pe.exponent += 1;
  crtTwiddle (y, lts, rts, pe, ru);
  crtpinv (y, lts*mprime, rts, p, mprime, ru);

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

extern "C" void tensorCRTRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Zq** ru, hInt_t q)
{
  Zq::q = q;
  tensorFuserCRT (y, ppcrt, totm, peArr, sizeOfPE, ru, q);
  canonicalizeZq(y,totm,q);
}

//takes inverse rus
extern "C" void tensorCRTInvRq (Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE,
                    Zq** ruinv, Zq* mhatInv, hInt_t q)
{
  Zq::q = q;
  // TODO: Make mhatInv not a pointer?
  tensorFuserCRT (y, ppcrtinv, totm, peArr, sizeOfPE, ruinv, q);
  for (hDim_t j = 0; j < totm; j++) {
    y[j] = y[j] * (*mhatInv);
  }
  canonicalizeZq(y,totm,q);
}

extern "C" void tensorCRTC (Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Complex** ru)
{
  tensorFuserCRT (y, ppcrt, totm, peArr, sizeOfPE, ru, 0);
}

//takes inverse rus
extern "C" void tensorCRTInvC (Complex* y, hDim_t totm, PrimeExponent* peArr,
                    hShort_t sizeOfPE, Complex** ruinv, Complex* mhatInv)
{
  tensorFuserCRT (y, ppcrtinv, totm, peArr, sizeOfPE, ruinv, 0);
  for (hDim_t j = 0; j < totm; j++) {
    y[j] *= (*mhatInv);
  }
}
