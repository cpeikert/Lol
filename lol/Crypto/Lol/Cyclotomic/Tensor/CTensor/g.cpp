#include "tensorTypes.h"

template <typename ring> void gPow (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t tmp1 = rts*(p-1);
  hDim_t tmp2 = tmp1 - rts;
  hDim_t blockOffset, modOffset;
  hDim_t i;
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp3 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp3 + modOffset;
      ring last = y[(tensorOffset + tmp2)*tupSize];
      for (i = p-2; i != 0; --i) {
        hDim_t idx = tensorOffset + i * rts;
        y[idx*tupSize] += (last - y[(idx-rts)*tupSize]);
      }
      y[tensorOffset*tupSize] += last;
    }
  }
}

template <typename ring> void ppGPow (ring* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts)
{
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;
   
  if (p != 2) {
    gPow (y, tupSize, lts*ipow(p,e-1), rts, p);
  }
}

template <typename ring> void gDec (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t tmp1 = rts*(p-1);
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring acc = y[tensorOffset*tupSize];
      for (i = p-2; i != 0; --i) {
        hDim_t idx = tensorOffset + i * rts;
        acc += y[idx*tupSize];
        y[idx*tupSize] -= y[(idx-rts)*tupSize];
      }
      y[tensorOffset*tupSize] += acc;
    }
  }
}

template <typename ring> void ppGDec (ring* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts)
{
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  if (p != 2) {
    gDec (y, tupSize, lts*ipow(p,e-1), rts, p);
  }
}

template <typename ring> void gInvPow (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t tmp1 = rts * (p-1);
  hDim_t blockOffset, modOffset;
  hDim_t i;

  for (blockOffset = 0; blockOffset < lts; ++blockOffset)
  {
    hDim_t tmp2 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset)
    {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring lelts;
      lelts = 0;
      for (i = 0; i < p-1; ++i)
      {
        lelts += y[(tensorOffset + i*rts)*tupSize];
      }
      ring relts;
      relts = 0;
      for (i = p-2; i >= 0; --i)
      {
        hDim_t idx = tensorOffset + i*rts;
        ring z = y[idx*tupSize];
        ring lmul, rmul;
        lmul = p-1-i;
        rmul = i+1;
        y[idx*tupSize] = lmul * lelts - rmul * relts;
        lelts -= z;
        relts += z;
      }
    }
  }
}

template <typename ring> void ppGInvPow (ring* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts)
{
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  if (p != 2) {
    gInvPow (y, tupSize, lts*ipow(p,e-1), rts, p);
  }
}

template <typename ring> void gInvDec (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t blockOffset;
  hDim_t modOffset;
  hDim_t i;
  hDim_t tmp1 = rts*(p-1);

  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      ring lastOut;
      lastOut = 0;
      for (i=1; i < p; ++i) {
        ring ri;
        ri = i;
        lastOut += (ri * y[(tensorOffset + (i-1)*rts)*tupSize]);
      }
      ring rp;
      rp = p;
      ring acc = lastOut / rp;
      ASSERT ((acc * rp) == lastOut);  // this line asserts that lastOut % p == 0, without calling % operator
      for (i = p-2; i > 0; --i) {
        hDim_t idx = tensorOffset + i*rts;
        ring tmp = acc;
        acc -= y[idx*tupSize]; // we already divided acc by p, do not multiply y[idx] by p
        y[idx*tupSize] = tmp;
      }
      y[tensorOffset*tupSize] = acc;
    }
  }
}

template <typename ring> void ppGInvDec (ring* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts)
{
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
  hDim_t p = pe.prime;
  hShort_t e = pe.exponent;

  if (p != 2) {
    gInvDec (y, tupSize, lts*ipow(p,e-1), rts, p);
  }
}

#ifdef STATS
int gprCtr = 0;
int gprqCtr = 0;
int gdrCtr = 0;
int gdrqCtr = 0;
int giprCtr = 0;
int giprqCtr = 0;
int gidrCtr = 0;
int gidrqCtr = 0;
int gcrqCtr = 0;
int gccCtr = 0;
int gicrqCtr = 0;
int giccCtr = 0;

struct timespec gprTime = {0,0};
struct timespec gprqTime = {0,0};
struct timespec gdrTime = {0,0};
struct timespec gdrqTime = {0,0};
struct timespec giprTime = {0,0};
struct timespec giprqTime = {0,0};
struct timespec gidrTime = {0,0};
struct timespec gidrqTime = {0,0};
struct timespec gcrqTime = {0,0};
struct timespec gccTime = {0,0};
#endif

extern "C" void tensorGPowR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  gprCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGPow, totm, peArr, sizeOfPE, (hInt_t*)0);

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gprTime = tsAdd(gprTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGPowRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
#ifdef STATS
  gprqCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGPow, totm, peArr, sizeOfPE, qs);

  hDim_t j;
  for(int tupIdx = 0; tupIdx<tupSize; tupIdx++) {
    hInt_t q = qs[tupIdx];
    for(j = 0; j < totm; j++) {
      if(y[j*tupSize+tupIdx].x<0) {
        y[j*tupSize+tupIdx].x+=q;
      }
    }
  }

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gprqTime = tsAdd(gprqTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGPowC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  gpcCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGPow, totm, peArr, sizeOfPE, (hInt_t*)0);

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gpcTime = tsAdd(gpcTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGDecR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  gdrCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGDec, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gdrTime = tsAdd(gdrTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGDecRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
#ifdef STATS
  gdrqCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGDec, totm, peArr, sizeOfPE, qs);

  hDim_t j;
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    hInt_t q = qs[tupIdx];
    for(j = 0; j < totm; j++) {
        if(y[j*tupSize+tupIdx].x<0) {
            y[j*tupSize+tupIdx].x+=q;
        }
    }
  }
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gdrqTime = tsAdd(gdrqTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGInvPowR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  giprCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  giprTime = tsAdd(giprTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGInvPowRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
#ifdef STATS
  giprqCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGInvPow, totm, peArr, sizeOfPE, qs);

  hDim_t j;
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    hInt_t q = qs[tupIdx];
    for(j = 0; j < totm; j++) {
      if(y[j*tupSize+tupIdx].x<0) {
        y[j*tupSize+tupIdx].x+=q;
      }
    }
  }
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  giprqTime = tsAdd(giprqTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGInvPowC (hShort_t tupSize, Complex* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  gipcCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGInvPow, totm, peArr, sizeOfPE, (hInt_t*)0);

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gipcTime = tsAdd(gipcTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGInvDecR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
  gidrCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGInvDec, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gidrTime = tsAdd(gidrTime, tsSubtract(t1,s1));
#endif
}

extern "C" void tensorGInvDecRq (hShort_t tupSize, Zq* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
#ifdef STATS
  gidrqCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser2 (y, tupSize, ppGInvDec, totm, peArr, sizeOfPE, qs);

  hDim_t j;
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    hInt_t q = qs[tupIdx];
    for(j = 0; j < totm; j++) {
      if(y[j*tupSize+tupIdx].x<0) {
        y[j*tupSize+tupIdx].x+=q;
      }
    }
  }

#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  gidrqTime = tsAdd(gidrqTime, tsSubtract(t1,s1));
#endif
}
