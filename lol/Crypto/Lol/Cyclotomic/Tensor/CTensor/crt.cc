#include "tensorTypes.h"
#include <time.h>
#include <stdlib.h>

// there should be a special cases that do NOT require temp space to be allocated for all primes *smaller* than DFTP_GENERIC_SIZE
#define DFTP_GENERIC_SIZE 11





#ifdef STATS
int crtRqCtr = 0;
int crtInvRqCtr = 0;
int crtCCtr = 0;
int crtInvCCtr = 0;

struct timespec crttime1 = {0,0};
struct timespec crttime2 = {0,0};
struct timespec crttime3 = {0,0};
struct timespec crttime4 = {0,0};

struct timespec crtInvRqTime = {0,0};
struct timespec crtCTime = {0,0};
struct timespec crtInvCTime = {0,0};
#endif

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

template <typename ring> void crtTwiddle (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
                   PrimeExponent pe, ring* ru)
{
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;
    
#ifdef DEBUG_MODE
  ASSERT(e != 0);
#endif
  pe.exponent -= 1; // used for an argument to bitrev
  
  if(p == 2) {
    hDim_t mprime = 1<<(e-1);
    hDim_t blockDim = rts*mprime; // size of block in block diagonal tensor matrix

    for(hDim_t i0 = 1; i0 < mprime; i0++) { // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
      hDim_t temp2 = i0*rts;
      ring twid = ru[bitrev(pe, i0)*tupSize];

      for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++) {
        hDim_t temp3 = blockIdx*blockDim + temp2;
        for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          hDim_t idx = (temp3 + modOffset)*tupSize;
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
        ring twid = ru[bitrev(pe, i0)*(i1+1)*tupSize];

        for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++) {
          hDim_t temp3 = blockIdx*blockDim + temp2;
          for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
            hDim_t idx = (temp3 + modOffset)*tupSize;
            y[idx] *= twid;
          }
        }
      }
    }
  }
}

// dim is power of p
template <typename ring> void dftTwiddle (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
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
      ring twid = ru[bitrev(pe,i0)*rustride*tupSize];

      for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
        hDim_t temp2 = blockOffset*temp1 + temp3;
        for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          idx = (temp2 + modOffset)*tupSize;
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
        ring twid = ru[bitrev(pe,i0)*i1*rustride*tupSize];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
          hDim_t temp2 = blockOffset*temp1 + temp3;
          for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
            idx = (temp2 + modOffset)*tupSize;
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
template <typename ring> void dftp (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
             hDim_t p, hDim_t rustride, ring* ru, ring* tempSpace)
{
  hDim_t tensorOffset;

  if(p == 2) {
    hDim_t temp1 = rts<<1;

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring u = y[tensorOffset*tupSize];
        ring t = y[(tensorOffset+rts)*tupSize];
        y[tensorOffset*tupSize] = u + t;
        y[(tensorOffset+rts)*tupSize] = u - t;
      }
    }
  }
  else if(p == 3) {
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];
    hDim_t temp1 = rts*3;

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3;
        y1 = y[tensorOffset*tupSize];
        y2 = y[(tensorOffset+rts)*tupSize];
        y3 = y[(tensorOffset+(rts<<1))*tupSize];
        //q is <32 bits, so we can do 3 additions without overflow
        y[tensorOffset*tupSize]           += (y2 + y3);
        y[(tensorOffset+rts)*tupSize]      = y1 + (ru1*y2) + (ru2*y3);
        y[(tensorOffset+(rts<<1))*tupSize] = y1 + (ru2*y2) + (ru1*y3);
      }
    }
  }
  else if(p == 5) {
    hDim_t temp1 = rts*5;
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];
    ring ru3 = ru[(rustride*3)*tupSize];
    ring ru4 = ru[(rustride<<2)*tupSize];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
          tensorOffset = temp2 + modOffset;
          ring y1, y2, y3, y4, y5;
          y1 = y[tensorOffset*tupSize];
          y2 = y[(tensorOffset+rts)*tupSize];
          y3 = y[(tensorOffset+(rts<<1))*tupSize];
          y4 = y[(tensorOffset+3*rts)*tupSize];
          y5 = y[(tensorOffset+(rts<<2))*tupSize];
          y[tensorOffset*tupSize]           += y2 + y3 + y4 + y5;
          y[(tensorOffset+rts)*tupSize]      = y1 + (ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5);
          y[(tensorOffset+(rts<<1))*tupSize] = y1 + (ru2*y2) + (ru4*y3) + (ru1*y4) + (ru3*y5);
          y[(tensorOffset+rts*3)*tupSize]    = y1 + (ru3*y2) + (ru1*y3) + (ru4*y4) + (ru2*y5);
          y[(tensorOffset+(rts<<2))*tupSize] = y1 + (ru4*y2) + (ru3*y3) + (ru2*y4) + (ru1*y5);
      }
    }
  }
  else if(p == 7) {
    hDim_t temp1 = rts*7;
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];
    ring ru3 = ru[(rustride*3)*tupSize];
    ring ru4 = ru[(rustride<<2)*tupSize];
    ring ru5 = ru[(rustride*5)*tupSize];
    ring ru6 = ru[(rustride*6)*tupSize];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, y5, y6, y7;
        y1 = y[tensorOffset*tupSize];
        y2 = y[(tensorOffset+rts)*tupSize];
        y3 = y[(tensorOffset+(rts<<1))*tupSize];
        y4 = y[(tensorOffset+3*rts)*tupSize];
        y5 = y[(tensorOffset+(rts<<2))*tupSize];
        y6 = y[(tensorOffset+rts*5)*tupSize];
        y7 = y[(tensorOffset+rts*6)*tupSize];
        y[tensorOffset*tupSize]           += y2 +     y3 +     y4 +     y5 +     y6 +     y7;
        y[(tensorOffset+rts)*tupSize]      = y1 + (ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5) + (ru5*y6) + (ru6*y7);
        y[(tensorOffset+(rts<<1))*tupSize] = y1 + (ru2*y2) + (ru4*y3) + (ru6*y4) + (ru1*y5) + (ru3*y6) + (ru5*y7);
        y[(tensorOffset+rts*3)*tupSize]    = y1 + (ru3*y2) + (ru6*y3) + (ru2*y4) + (ru5*y5) + (ru1*y6) + (ru4*y7);
        y[(tensorOffset+(rts<<2))*tupSize] = y1 + (ru4*y2) + (ru1*y3) + (ru5*y4) + (ru2*y5) + (ru6*y6) + (ru3*y7);
        y[(tensorOffset+rts*5)*tupSize]    = y1 + (ru5*y2) + (ru3*y3) + (ru1*y4) + (ru6*y5) + (ru4*y6) + (ru2*y7);
        y[(tensorOffset+rts*6)*tupSize]    = y1 + (ru6*y2) + (ru5*y3) + (ru4*y4) + (ru3*y5) + (ru2*y6) + (ru1*y7);
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
            tempSpace[row] += (y[(tensorOffset+col*rts)*tupSize]*ru[((col*row) % p)*rustride*tupSize]);
          }
        }
        
        for(hDim_t row = 0; row < p; row++) {
          y[(tensorOffset+rts*row)*tupSize] = tempSpace[row];
        }
      }
    }
  }
}

template <typename ring> void crtp (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
             hDim_t p, hDim_t rustride, ring* ru)
{
  hDim_t tensorOffset;
  if(p == 2) {
      return;
  }
  else if(p == 3) {
    hDim_t temp1 = rts*2;
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2;
        y1 = y[tensorOffset*tupSize];
        y2 = y[(tensorOffset+rts)*tupSize];
        y[tensorOffset*tupSize]      += (ru1*y2);
        y[(tensorOffset+rts)*tupSize] = y1 + (ru2*y2);
      }
    }
  }
  else if(p == 5) {
    hDim_t temp1 = rts*4;
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];
    ring ru3 = ru[(rustride*3)*tupSize];
    ring ru4 = ru[(rustride<<2)*tupSize];

    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4;
        y1 = y[tensorOffset*tupSize];
        y2 = y[(tensorOffset+rts)*tupSize];
        y3 = y[(tensorOffset+(rts<<1))*tupSize];
        y4 = y[(tensorOffset+3*rts)*tupSize];

        y[tensorOffset*tupSize]           += ((ru1*y2) + (ru2*y3) + (ru3*y4));
        y[(tensorOffset+rts)*tupSize]      = y1 + (ru2*y2) + (ru4*y3) + (ru1*y4);
        y[(tensorOffset+(rts<<1))*tupSize] = y1 + (ru3*y2) + (ru1*y3) + (ru4*y4);
        y[(tensorOffset+rts*3)*tupSize]    = y1 + (ru4*y2) + (ru3*y3) + (ru2*y4);
      }   
    }
  }
  else if(p == 7) {
    hDim_t temp1 = rts*6;
    ring ru1 = ru[rustride*tupSize];
    ring ru2 = ru[(rustride<<1)*tupSize];
    ring ru3 = ru[(rustride*3)*tupSize];
    ring ru4 = ru[(rustride<<2)*tupSize];
    ring ru5 = ru[(rustride*5)*tupSize];
    ring ru6 = ru[(rustride*6)*tupSize];
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        ring y1, y2, y3, y4, y5, y6;
        y1 = y[tensorOffset*tupSize];
        y2 = y[(tensorOffset+rts)*tupSize];
        y3 = y[(tensorOffset+(rts<<1))*tupSize];
        y4 = y[(tensorOffset+3*rts)*tupSize];
        y5 = y[(tensorOffset+(rts<<2))*tupSize];
        y6 = y[(tensorOffset+rts*5)*tupSize];
        y[tensorOffset*tupSize]           += ((ru1*y2) + (ru2*y3) + (ru3*y4) + (ru4*y5) + (ru5*y6));
        y[(tensorOffset+rts)*tupSize]      = y1 + (ru2*y2) + (ru4*y3) + (ru6*y4) + (ru1*y5) + (ru3*y6);
        y[(tensorOffset+(rts<<1))*tupSize] = y1 + (ru3*y2) + (ru6*y3) + (ru2*y4) + (ru5*y5) + (ru1*y6);
        y[(tensorOffset+rts*3)*tupSize]    = y1 + (ru4*y2) + (ru1*y3) + (ru5*y4) + (ru2*y5) + (ru6*y6);
        y[(tensorOffset+(rts<<2))*tupSize] = y1 + (ru5*y2) + (ru3*y3) + (ru1*y4) + (ru6*y5) + (ru4*y6);
        y[(tensorOffset+rts*5)*tupSize]    = y1 + (ru6*y2) + (ru5*y3) + (ru4*y4) + (ru3*y5) + (ru2*y6);
      }
    }
  }
  else {
    ring* tempSpace = (ring*)malloc((p-1)*sizeof(ring));
    hDim_t temp1 = rts*(p-1);
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        
        for(hDim_t row = 1; row < p; row++) {
          tempSpace[row-1] = 0;
          for(hDim_t col = 0; col < p-1; col++) {
            tempSpace[row-1] += (y[(tensorOffset+col*rts)*tupSize]*ru[((col*row) % p)*rustride*tupSize]);
          }
        }
        
        for(hDim_t row = 0; row < p-1; row++) {
          y[(tensorOffset+rts*row)*tupSize] = tempSpace[row];
        }
      }
    }
    free(tempSpace);
  }
}

//takes inverse rus
template <typename ring> void crtpinv (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
                hDim_t p, hDim_t rustride, ring* ruinv)
{
  if(p ==2) {
    // need this case so that we can divide overall by mhat^(-1)
    return;
  }
  else {
    hDim_t tensorOffset,i;
    ring* tempSpace = (ring*)malloc((p-1)*sizeof(ring));
    hDim_t temp1 = rts*(p-1);
    for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++) {
      hDim_t temp2 = blockOffset*temp1;
      for(hDim_t modOffset = 0; modOffset < rts; modOffset++) {
        tensorOffset = temp2 + modOffset;
        
        for(i = 0; i < p-1; i++) {
          tempSpace[i] = (int)0;
          int j;
          for(j = 0; j < p-1; j++) {
            int ruIdx = ((j+1)*i) % p;
            tempSpace[i] += (y[(tensorOffset+j*rts)*tupSize] * ruinv[ruIdx*rustride*tupSize]);
          }
        }

        ring shift; // can't assign to a constant on the same line(?)
        shift=0;
        for(i = 0; i < p-1; i++) {
          // we were given the inverse rus, so we need to negate the indices
          shift += (y[(tensorOffset+i*rts)*tupSize] * ruinv[rustride*(p-(i+1))*tupSize]);
        }

        for(i = 0; i < p-1; i++) {
          y[(tensorOffset+i*rts)*tupSize] = tempSpace[i] - shift; 
        }
      }
    }
  }
}

template <typename ring> void ppDFT (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
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
    dftp (y, tupSize, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);
    dftTwiddle (y, tupSize, lts, rtsDim, pe, ltsScale*p, twidRuStride, ru);
    
    ltsScale /= p;
    rtsScale *= p;
    twidRuStride *= p;
    pe.exponent -= 1;
  }
}

template <typename ring> void ppDFTInv (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
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
    dftTwiddle (y, tupSize, lts, rtsDim, pe, ltsScaleP, twidRuStride, ru);
    dftp (y, tupSize, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);
    
    ltsScale = ltsScaleP;
    rtsScale /= p;
    twidRuStride /= p;
    pe.exponent += 1;
  }
}

void ppcrtRq (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
              PrimeExponent pe, void* ru, hInt_t* qs)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
  ASSERT(e != 0);
#endif
  hDim_t mprime = ipow(p,e-1);
    
#ifdef DEBUG_MODE
  printf("lts is %" PRId32 "\trts is %" PRId32 "\n", lts, rts);
  printf("rus for p=%" PRId32 ", e=%" PRId16 "\t[", pe.prime, pe.exponent);
  hDim_t i;
  for(i = 0; i < ipow(p,e); i++) {
    printf("%" PRId64 ",", ((hInt_t*)ru)[i*tupSize+tupIdx]);
  }
  printf("]\n");
#endif

  Zq* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (Zq*)malloc(p*sizeof(Zq));
  }
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    Zq* z = ((Zq*)y)+tupIdx;
    q = qs[tupIdx]; // global update
    Zq* ruOffset = ((Zq*)ru)+tupIdx;
    crtp (z, tupSize, lts*mprime, rts, p, mprime, ruOffset);
    crtTwiddle (z, tupSize, lts, rts, pe, ruOffset);
    pe.exponent -= 1;
    ppDFT (z,  tupSize, lts, rts*(p-1), pe, p, ruOffset, temp);
    pe.exponent += 1;
  }

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

void ppcrtinvRq (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, 
                 PrimeExponent pe, void* ru, hInt_t* qs)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
  ASSERT(e != 0);
#endif
  hDim_t mprime = ipow(p,e-1);
#ifdef DEBUG_MODE
  printf("lts is %" PRId32 "\trts is %" PRId32 "\n", lts, rts);
  printf("rus for p=%" PRId32 ", e=%" PRId16 "\t[", pe.prime, pe.exponent);
  hDim_t i;
  for(i = 0; i < ipow(p,e); i++) {
    printf("%" PRId64 ",", ((hInt_t*)ru)[i*tupSize+tupIdx]);
  }
  printf("]\n");
#endif

  Zq* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (Zq*)malloc(p*sizeof(Zq));
  }
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    Zq* z = ((Zq*)y)+tupIdx;
    q = qs[tupIdx]; // global update
    Zq* ruOffset = ((Zq*)ru)+tupIdx;

    pe.exponent -= 1;
    ppDFTInv (z, tupSize, lts, rts*(p-1), pe, p, ruOffset, temp);
    pe.exponent += 1;
    crtTwiddle (z, tupSize, lts, rts, pe, ruOffset);
    crtpinv (z, tupSize, lts*mprime, rts, p, mprime, ruOffset);
  }

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

// EAC: Somebody who knows C/C++ should find a better way to handle pointers-to-pointers in a generic way
void tensorCRTRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t* qs)
{
  hDim_t i;
#ifdef STATS
  struct timespec s1,s2,s3,s4,t1,t2,t3,t4;

  crtRqCtr++;

  clock_gettime(CLOCK_REALTIME, &s1);
  clock_gettime(CLOCK_MONOTONIC, &s2);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &s3);
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &s4);
#endif
#ifdef DEBUG_MODE
  printf("\n\nEntered tensorCRTRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\ttupSize=%" PRId16 "\n[", totm, sizeOfPE,tupSize);
  for(i = 0; i < tupSize; i++) {
    printf("%" PRId64 ",", qs[i]);
  }
  printf("]\n[");
  for(i = 0; i < totm*tupSize; i++) {
    printf("%" PRId64 ",", y[i]);
  }
  printf("]\n[");
  for(i = 0; i < sizeOfPE; i++) {
    printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
  }
  printf("]\n");
#endif

  void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
  for(i = 0; i < sizeOfPE; i++) {
    rus[i] = (void*) (ru[i]);
  }

  tensorFuserCRT (y, tupSize, ppcrtRq, totm, peArr, sizeOfPE, rus, qs);

  for(i = 0; i < tupSize; i++) {
    hInt_t q = qs[i];
    for(hDim_t j = 0; j < totm; j++) {
      if(y[j*tupSize+i]<0) {
        y[j*tupSize+i]+=q;
      }
#ifdef DEBUG_MODE
      if(y[j*tupSize+i]<0) {
        printf("TENSOR CRT^T INV\n");
      }
#endif
    }
  }

  free(rus);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  clock_gettime(CLOCK_MONOTONIC, &t2);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t3);
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &t4);

  crttime1 = tsAdd(crttime1, tsSubtract(t1,s1));
  crttime2 = tsAdd(crttime2, tsSubtract(t2,s2));
  crttime3 = tsAdd(crttime3, tsSubtract(t3,s3));
  crttime4 = tsAdd(crttime4, tsSubtract(t4,s4));
#endif
}

//takes inverse rus
void tensorCRTInvRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, 
                    hInt_t** ruinv, hInt_t* mhatInv, hInt_t* qs)
{
  hDim_t i;
#ifdef STATS
  struct timespec s1,t1;
  crtInvRqCtr++;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
  printf("\n\nEntered tensorCRTInvRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\ttupSize=%" PRId16 "\nqs[", totm, sizeOfPE, tupSize);
  for(i = 0; i < tupSize; i++) {
    printf("%" PRId64 ",", qs[i]);
  }
  printf("]\nmhatInv[");
  for(i = 0; i < tupSize; i++) {
    printf("%" PRId64 ",", mhatInv[i]);
  }
  printf("]\ny[");
  for(i = 0; i < totm*tupSize; i++) {
    printf("%" PRId64 ",", y[i]);
  }
  printf("]\npps[");
  for(i = 0; i < sizeOfPE; i++) {
    printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
  }
  printf("]\n");
#endif

void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
  for(i = 0; i < sizeOfPE; i++) {
    rus[i] = (void*) (ruinv[i]);
  }
  tensorFuserCRT (y, tupSize, ppcrtinvRq, totm, peArr, sizeOfPE, rus, qs);

  for (i = 0; i < tupSize; i++) {
    hInt_t q = qs[i];
    for (hDim_t j = 0; j < totm; j++) {
      y[j*tupSize+i] = (y[j*tupSize+i]*mhatInv[i])%q;
      if(y[j*tupSize+i] < 0) {
        y[j*tupSize+i] +=q;
      }
#ifdef DEBUG_MODE
      if(y[j*tupSize+i]<0)
      {
        printf("TENSOR CRT INV\n");
      }
#endif
    }
  }

  free(rus);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  crtInvRqTime = tsAdd(crtInvRqTime, tsSubtract(t1,s1));
#endif
}




































void ppcrtC (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t* qs)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
  ASSERT(e != 0);
#endif
  hDim_t mprime = ipow(p,e-1);

#ifdef DEBUG_MODE
  printf("rus for p=%" PRId32 ", e=%" PRId16 "\t[", pe.prime, pe.exponent);
  hDim_t i;
  for(i = 0; i < ipow(p,e); i++) {
    printf("(%f,%f),", ((Complex*)ru)[i].real, ((Complex*)ru)[i].imag);
  }
  printf("]\n");
#endif

  Complex* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (Complex*)malloc(p*sizeof(Complex));
  }

  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    Complex* z = ((Complex*)y)+tupIdx;
    Complex* ruOffset = ((Complex*)ru)+tupIdx;
    crtp (z, tupSize, lts*mprime, rts, p, mprime, ruOffset);
    crtTwiddle (z, tupSize, lts, rts, pe, ruOffset);
    pe.exponent -= 1;
    ppDFT (z, tupSize, lts, rts*(p-1), pe, p, ruOffset, temp);
    pe.exponent += 1;
  }

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

void ppcrtinvC (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t* qs)
{
  hDim_t p = pe.prime;
  hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
  ASSERT(e != 0);
#endif
  hDim_t mprime = ipow(p,e-1);
    
  Complex* temp = 0;
  if(p >= DFTP_GENERIC_SIZE) {
    temp = (Complex*)malloc(p*sizeof(Complex));
  }

  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    Complex* z = ((Complex*)y)+tupIdx;
    Complex* ruOffset = ((Complex*)ru)+tupIdx;
    pe.exponent -= 1;
    ppDFTInv (z, tupSize, lts, rts*(p-1), pe, p, ruOffset, temp);
    pe.exponent += 1;
    crtTwiddle (z, tupSize, lts, rts, pe, ruOffset);
    crtpinv (z, tupSize, lts*mprime, rts, p, mprime, ruOffset);
  }

  if(p >= DFTP_GENERIC_SIZE) {
    free(temp);
  }
}

void tensorCRTC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, Complex** ru)
{
#ifdef STATS
  struct timespec s1,t1;
  crtCCtr++;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
  printf("\n\nEntered tensorCRTC\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\n[", totm, sizeOfPE);
  hDim_t j;
  for(j = 0; j < totm; j++) {
    printf("(%f,%f),", y[j].real, y[j].imag);
  }
  printf("]\n[");
  for(j = 0; j < sizeOfPE; j++) {
    printf("(%" PRId32 ",%" PRId16 "),", peArr[j].prime, peArr[j].exponent);
  }
  printf("]\n");
#endif
  void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
  hShort_t i;
  for(i = 0; i < sizeOfPE; i++) {
    rus[i] = (void*) (ru[i]);
  }
  tensorFuserCRT (y, tupSize, ppcrtC, totm, peArr, sizeOfPE, rus, (hInt_t*)0);
  free(rus);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  crtCTime = tsAdd(crtCTime, tsSubtract(t1,s1));
#endif
}

//takes inverse rus
void tensorCRTInvC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, 
                    hShort_t sizeOfPE, Complex** ruinv, Complex* mhatInv)
{
#ifdef STATS
  struct timespec s1,t1;
  crtInvCCtr++;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  hDim_t i;
  
  void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
  for(i = 0; i < sizeOfPE; i++) {
    rus[i] = (void*) (ruinv[i]);
  }
  
  tensorFuserCRT (y, tupSize, ppcrtinvC, totm, peArr, sizeOfPE, rus, (hInt_t*)0);

  for (i = 0; i < tupSize; i++) {
    for (hDim_t j = 0; j < totm; j++) {
      CMPLX_IMUL(y[j*tupSize+i], mhatInv[i]);
    }
  }
  
  free(rus);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  crtInvCTime = tsAdd(crtInvCTime, tsSubtract(t1,s1));
#endif
}
