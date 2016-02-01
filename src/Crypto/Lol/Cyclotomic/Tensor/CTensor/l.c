#include "tensorTypes.h"

void lpRq (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;
  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t idx = tmp2 + modOffset + rts;
      for (i = 1; i < p-1; ++i) {
        hInt_t temp = y[(idx-rts)*tupSize] + y[idx*tupSize];
        if (temp >= q) y[idx*tupSize]=temp-q;
        else y[idx*tupSize] = temp;
        idx += rts;
      }
    }
  }
}

void lpR (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t idx = tmp2 + modOffset + rts;
      for (i = 1; i < p-1; ++i) {
        y[idx*tupSize] += y[(idx-rts)*tupSize];
        idx += rts;
      }
    }
  }
}

void lpDouble (double* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t idx = tmp2 + modOffset + rts;
      for (i = 1; i < p-1; ++i) {
        y[idx*tupSize] += y[(idx-rts)*tupSize];
        idx += rts;
      }
    }
  }
}

void lpC (complex_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset) {
      hDim_t idx = tmp2 + modOffset + rts;
      for (i = 1; i < p-1; ++i) {
        CMPLX_IADD (y[idx*tupSize], y[(idx-rts)*tupSize]);
        idx += rts;
      }
    }
  }
}

void lpInvRq (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++ modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      hDim_t idx = tensorOffset + (p-2) * rts;
      for (i = p-2; i != 0; --i) {
        hInt_t temp = y[idx*tupSize] - y[(idx-rts)*tupSize] + q;
        if (temp >= q) y[idx*tupSize]=temp-q;
        else y[idx*tupSize] = temp;
        idx -= rts;
      }
    }
  }
}

void lpInvR (hInt_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++ modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      hDim_t idx = tensorOffset + (p-2) * rts;
      for (i = p-2; i != 0; --i) {
        y[idx*tupSize] -= y[(idx-rts)*tupSize] ;
        idx -= rts;
      }
    }
  }
}

void lpInvDouble (double* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++ modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      hDim_t idx = tensorOffset + (p-2) * rts;
      for (i = p-2; i != 0; --i) {
        y[idx*tupSize] -= y[(idx-rts)*tupSize] ;
        idx -= rts;
      }
    }
  }
}

void lpInvC (complex_t* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p) {
  hDim_t blockOffset;
  hDim_t modOffset;
  int i;

  hDim_t tmp1 = rts*(p-1);
  for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
    hDim_t tmp2 = blockOffset*tmp1;
    for (modOffset = 0; modOffset < rts; ++ modOffset) {
      hDim_t tensorOffset = tmp2 + modOffset;
      hDim_t idx = tensorOffset + (p-2) * rts;
      for (i = p-2; i != 0; --i) {
        CMPLX_ISUB (y[idx*tupSize], y[(idx-rts)*tupSize]);
        idx -= rts;
      }
    }
  }
}

void ppLRq (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpRq (((hInt_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p, qs[tupIdx]);
    }
}

void ppLR (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpR (((hInt_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}

void ppLDouble (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpDouble (((double*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}

void ppLC (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpC (((complex_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}


void ppLInvRq (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpInvRq (((hInt_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p, qs[tupIdx]);
    }
}

void ppLInvR (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpInvR (((hInt_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}

void ppLInvDouble (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpInvDouble (((double*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}

void ppLInvC (void* y, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs) {
#ifdef DEBUG_MODE
  ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
      lpInvC (((complex_t*)y)+tupIdx, tupSize, lts*ipow(p,e-1), rts, p);
    }
}

#ifdef STATS
int lrqCtr = 0;
int lrCtr = 0;
int ldCtr = 0;
int lcCtr = 0;
int lirqCtr = 0;
int lirCtr = 0;
int lidCtr = 0;
int licCtr = 0;

struct timespec lrqTime = {0,0};
struct timespec lrTime = {0,0};
struct timespec ldTime = {0,0};
struct timespec lcTime = {0,0};
struct timespec lirqTime = {0,0};
struct timespec lirTime = {0,0};
struct timespec lidTime = {0,0};
struct timespec licTime = {0,0};
#endif


void tensorLRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs) {
#ifdef STATS
    lrqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    hDim_t i;
    printf("\n\nEntered tensorLRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\tq=%" PRId64 "\n[", totm, sizeOfPE, q);
    /*for(i = 0; i < totm; i++) {
        printf("%" PRId64 ",", y[i]);
    }*/
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif
  tensorFuser (y, tupSize, ppLRq, totm, peArr, sizeOfPE, qs); // don't need to shift here
#ifdef DEBUG_MODE
  for(i = 0; i < totm*tupSize; i++) {
      if(y[i]<0) {
          printf("tensorLRq\n");
      }
  }
#endif
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lrqTime = tsAdd(lrqTime, tsSubtract(t1,s1));
#endif
}

void tensorLR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lrCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorLR\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\n[", totm, sizeOfPE);
    hDim_t i;
    for(i = 0; i < totm; i++) {
        printf("%" PRId64 ",", y[i]);
    }
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif
  tensorFuser (y, tupSize, ppLR, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lrTime = tsAdd(lrTime, tsSubtract(t1,s1));
#endif
}

void tensorLDouble (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    ldCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorLDouble\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\n[", totm, sizeOfPE);
    hDim_t i;
    for(i = 0; i < totm; i++) {
        printf("%f,", y[i]);
    }
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif
  tensorFuser (y, tupSize, ppLDouble, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    ldTime = tsAdd(ldTime, tsSubtract(t1,s1));
#endif
}

void tensorLC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lcCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorLC\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\n[", totm, sizeOfPE);
    hDim_t i;
    for(i = 0; i < totm; i++) {
        printf("(%f,%f),", y[i].real, y[i].imag);
    }
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif
  tensorFuser (y, tupSize, ppLC, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lcTime = tsAdd(lcTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs) {
#ifdef STATS
    lirqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser (y, tupSize, ppLInvRq, totm, peArr, sizeOfPE, qs);
#ifdef DEBUG_MODE
  hDim_t i;
  for(i = 0; i < totm*tupSize; i++)
  {
      if(y[i]<0)
      {
          printf("tensorLInvRq\n");
      }
  }
#endif
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lirqTime = tsAdd(lirqTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvR (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lirCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser (y, tupSize, ppLInvR, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lirTime = tsAdd(lirTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvDouble (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lidCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser (y, tupSize, ppLInvDouble, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lidTime = tsAdd(lidTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    licCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
  tensorFuser (y, tupSize, ppLInvC, totm, peArr, sizeOfPE, (hInt_t*)0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    licTime = tsAdd(licTime, tsSubtract(t1,s1));
#endif
}

