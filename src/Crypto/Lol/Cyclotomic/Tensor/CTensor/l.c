#include "tensorTypes.h"

void lpRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q) {
	hDim_t blockOffset;
	hDim_t modOffset;
	int i;
	hDim_t tmp1 = rts*(p-1);
	for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset) {
			hDim_t idx = tmp2 + modOffset + rts;
			for (i = 1; i < p-1; ++i) {
        hInt_t temp = y[idx-rts] + y[idx];
        if (temp >= q) y[idx]=temp-q;
        else y[idx] = temp;
				idx += rts;
			}
		}
	}
}

void lpR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p) {
	hDim_t blockOffset;
	hDim_t modOffset;
	int i;

	hDim_t tmp1 = rts*(p-1);
	for (blockOffset = 0; blockOffset < lts; ++blockOffset)	{
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset) {
			hDim_t idx = tmp2 + modOffset + rts;
			for (i = 1; i < p-1; ++i) {
				y[idx] += y[idx-rts];
				idx += rts;
			}
		}
	}
}

void lpDouble (double* y, hDim_t lts, hDim_t rts, hDim_t p) {
	hDim_t blockOffset;
	hDim_t modOffset;
	int i;

	hDim_t tmp1 = rts*(p-1);
	for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset) {
			hDim_t idx = tmp2 + modOffset + rts;
			for (i = 1; i < p-1; ++i) {
				y[idx] += y[idx-rts];
				idx += rts;
			}
		}
	}
}

void lpC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p) {
	hDim_t blockOffset;
	hDim_t modOffset;
	int i;

	hDim_t tmp1 = rts*(p-1);
	for (blockOffset = 0; blockOffset < lts; ++blockOffset) {
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset) {
			hDim_t idx = tmp2 + modOffset + rts;
			for (i = 1; i < p-1; ++i) {
				CMPLX_IADD (y[idx], y[idx-rts]);
				idx += rts;
			}
		}
	}
}

void lpInvRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q) {
	hDim_t blockOffset;
	hDim_t modOffset;
	int i;

	hDim_t tmp1 = rts*(p-1);
	for (blockOffset = 0; blockOffset < lts; ++blockOffset)	{
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++ modOffset) {
			hDim_t tensorOffset = tmp2 + modOffset;
			hDim_t idx = tensorOffset + (p-2) * rts;
			for (i = p-2; i != 0; --i) {
        hInt_t temp = y[idx] - y[idx-rts] + q;
        if (temp >= q) y[idx]=temp-q;
        else y[idx] = temp;
				idx -= rts;
			}
		}
	}
}

void lpInvR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p) {
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
				y[idx] -= y[idx-rts] ;
				idx -= rts;
			}
		}
	}
}

void lpInvDouble (double* y, hDim_t lts, hDim_t rts, hDim_t p) {
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
				y[idx] -= y[idx-rts] ;
				idx -= rts;
			}
		}
	}
}

void lpInvC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p) {
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
				CMPLX_ISUB (y[idx], y[idx-rts]);
				idx -= rts;
			}
		}
	}
}

void ppLRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
}

void ppLR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
}

void ppLDouble (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpDouble ((double*)y, lts*ipow(p,e-1), rts, p);
}

void ppLC (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpC ((complex_t*)y, lts*ipow(p,e-1), rts, p);
}


void ppLInvRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpInvRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
}

void ppLInvR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpInvR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
}

void ppLInvDouble (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpInvDouble ((double*)y, lts*ipow(p,e-1), rts, p);
}

void ppLInvC (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q) {
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	lpInvC ((complex_t*)y, lts*ipow(p,e-1), rts, p);
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


void tensorLRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q) {
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
	tensorFuser (y, ppLRq, totm, peArr, sizeOfPE, q); // don't need to shift here
#ifdef DEBUG_MODE
	for(i = 0; i < totm; i++) {
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

void tensorLR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
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
	tensorFuser (y, ppLR, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lrTime = tsAdd(lrTime, tsSubtract(t1,s1));
#endif
}

void tensorLDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
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
	tensorFuser (y, ppLDouble, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    ldTime = tsAdd(ldTime, tsSubtract(t1,s1));
#endif
}

void tensorLC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
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
	tensorFuser (y, ppLC, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lcTime = tsAdd(lcTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q) {
#ifdef STATS
    lirqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppLInvRq, totm, peArr, sizeOfPE, q);  // don't need to shift here
#ifdef DEBUG_MODE
	hDim_t i;
	for(i = 0; i < totm; i++)
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

void tensorLInvR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lirCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppLInvR, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lirTime = tsAdd(lirTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvDouble (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    lidCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppLInvDouble, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    lidTime = tsAdd(lidTime, tsSubtract(t1,s1));
#endif
}

void tensorLInvC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE) {
#ifdef STATS
    licCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppLInvC, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    licTime = tsAdd(licTime, tsSubtract(t1,s1));
#endif
}

