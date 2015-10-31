#include "tensorTypes.h"


void gPowR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p)
{
  hDim_t tmp1 = rts*(p-1);
  hDim_t tmp2 = tmp1 - rts;
  hDim_t blockOffset, modOffset;
  hDim_t i;
  for (blockOffset = 0; blockOffset < lts; ++blockOffset)
  {
    hDim_t tmp3 = blockOffset * tmp1;
    for (modOffset = 0; modOffset < rts; ++modOffset)
    {
      hDim_t tensorOffset = tmp3 + modOffset;
      hInt_t last = y[tensorOffset + tmp2];
      for (i = p-2; i != 0; --i)
      {
        hDim_t idx = tensorOffset + i * rts;
        y[idx] += last - y[idx-rts];
      }
      y[tensorOffset] += last;
    }
  }
}

void gPowRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q)
{
	hDim_t tmp1 = rts*(p-1);
	hDim_t tmp2 = tmp1 - rts;
	hDim_t blockOffset, modOffset;
	hDim_t i;
	for (blockOffset = 0; blockOffset < lts; ++blockOffset)
	{
		hDim_t tmp3 = blockOffset * tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset)
		{
			hDim_t tensorOffset = tmp3 + modOffset;
			hInt_t last = y[tensorOffset + tmp2];
			for (i = p-2; i != 0; --i)
			{
				hDim_t idx = tensorOffset + i * rts;
				y[idx] = (y[idx] + last - y[idx-rts]) % q;
			}
			y[tensorOffset] = (y[tensorOffset] + last) % q;
		}
	}
}


void ppGPowR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
     
	if (p != 2)
	{
		gPowR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
	}
}


void ppGPowRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gPowRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
	}
}



void gDecR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p)
{
	hDim_t tmp1 = rts*(p-1);
	hDim_t blockOffset;
	hDim_t modOffset;
	hDim_t i;

	for (blockOffset = 0; blockOffset < lts; ++blockOffset)
	{
		hDim_t tmp2 = blockOffset * tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset)
		{
			hDim_t tensorOffset = tmp2 + modOffset;
			hInt_t acc = y[tensorOffset];
			for (i = p-2; i != 0; --i)
			{
				hDim_t idx = tensorOffset + i * rts;
				acc += y[idx];
				y[idx] -= y[idx-rts];
			}
			y[tensorOffset] += acc;
		}
	}
}

void gDecRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q)
{
	hDim_t tmp1 = rts*(p-1);
	hDim_t blockOffset;
	hDim_t modOffset;
	hDim_t i;

	for (blockOffset = 0; blockOffset < lts; ++blockOffset)
	{
		hDim_t tmp2 = blockOffset * tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset)
		{
			hDim_t tensorOffset = tmp2 + modOffset;
			hInt_t acc = y[tensorOffset];
			for (i = p-2; i != 0; --i)
			{
				hDim_t idx = tensorOffset + i * rts;
        // acc is at most p*q << 64 bits, so no need to mod
				acc = acc + y[idx];
				y[idx] = (y[idx] - y[idx-rts]) % q;
			}
			y[tensorOffset] = (y[tensorOffset] + acc) % q;
		}
	}
}

void ppGDecR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gDecR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
	}
}

void ppGDecRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gDecRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
	}
}


void gInvPowR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p)
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
			hInt_t lelts = 0;
			for (i = 0; i < p-1; ++i)
			{
				lelts += y[tensorOffset + i*rts];
			}
			hInt_t relts = 0;
			for (i = p-2; i >= 0; --i)
			{
				hDim_t idx = tensorOffset + i*rts;
				hInt_t z = y[idx];
				y[idx] = (p-1-i) * lelts - (i+1)*relts;
				lelts -= z;
				relts += z;
			}
		}
	}
}

void gInvPowRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q)
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
			hInt_t lelts = 0;
      //lelts is at most p*q, so we can mod once at the end
			for (i = 0; i < p-1; ++i)
			{
				lelts = lelts + y[tensorOffset + i*rts];
			}
      lelts = lelts % q;
      //in the next loop, lelts <= p*q and relts <= p*q
      //products are <= p*p*q, and diff is <= 2*p*p*q
      //so we assume 2*p^2 << 31 bits
			hInt_t relts = 0;
			for (i = p-2; i >= 0; --i)
			{
				hDim_t idx = tensorOffset + i*rts;
				hInt_t z = y[idx];
				y[idx] = (((p-1-i) * lelts) - ((i+1)*relts)) % q;
				lelts -= z;
				relts += z;
			}
		}
	}
}


void ppGInvPowR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gInvPowR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
	}
}

void ppGInvPowRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gInvPowRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
	}
}

//do not call for p=2!
void gCRTRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t* gcoeffs, hInt_t q)
{
    hDim_t gindex;
    hDim_t blockOffset, modOffset, idx;
    hDim_t temp1 = rts*(p-1);
    
    for(gindex = 0; gindex < p-1; gindex++)
    {
        hInt_t coeff = gcoeffs[gindex];
        hDim_t temp3 = gindex*rts;
        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1 + temp3;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                idx = temp2 + modOffset;
                y[idx] = (y[idx]*coeff)%q;
            }
        }
    }
}

//do not call for p=2!
void gCRTC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p, complex_t* gcoeffs)
{
    hDim_t gindex;
    hDim_t blockOffset, modOffset, idx;
    hDim_t temp1 = rts*(p-1);
    
    for(gindex = 0; gindex < p-1; gindex++)
    {
        complex_t coeff = gcoeffs[gindex];
        hDim_t temp3 = gindex*rts;
        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1 + temp3;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                idx = temp2 + modOffset;
                CMPLX_IMUL(y[idx],coeff);
            }
        }
    }
}

void ppGCRTRq (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* gcoeffs, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
#ifdef DEBUG_MODE
    printf("gcoeffs for p=%" PRId32 ", e=%" PRId16 "\t[", pe.prime, pe.exponent);
    int i;
    for(i = 0; i < ((p-1)*ipow(p,e-1)); i++) {
        printf("%" PRId64 ",", ((hInt_t*)gcoeffs)[i]);
    }
    printf("]\n");
#endif
    
	if (p != 2)
	{
		gCRTRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, (hInt_t*)gcoeffs, q);
	}
}

void ppGCRTC (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* gcoeffs, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
#ifdef DEBUG_MODE
    printf("gcoeffs for p=%" PRId32 ", e=%" PRId16 "\t[", pe.prime, pe.exponent);
    int i;
    for(i = 0; i < ((p-1)*ipow(p,e-1)); i++) {
        printf("(%f,%f),", ((complex_t*)gcoeffs)[i].real, ((complex_t*)gcoeffs)[i].imag);
    }
    printf("]\n");
#endif
    
	if (p != 2)
	{
		gCRTC ((complex_t*)y, lts*ipow(p,e-1), rts, p, (complex_t*)gcoeffs);
	}
}

void gInvDecR (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p)
{
	hDim_t blockOffset;
	hDim_t modOffset;
	hDim_t i;
	hDim_t tmp1 = rts*(p-1);

	for (blockOffset = 0; blockOffset < lts; ++blockOffset)
	{
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset)
		{
			hDim_t tensorOffset = tmp2 + modOffset;
			hInt_t lastOut = 0;
			for (i=1; i < p; ++i)
			{
				lastOut += i * y[tensorOffset + (i-1)*rts];
			}
			hInt_t acc = lastOut / p;
			ASSERT (acc * p == lastOut);  // this line asserts that lastOut % p == 0, without calling % operator
			for (i = p-2; i > 0; --i)
			{
				hDim_t idx = tensorOffset + i*rts;
				hInt_t tmp = acc;
				acc -= y[idx]; // we already divided acc by p, do not multiply y[idx] by p
				y[idx] = tmp;
			}
			y[tensorOffset] = acc;
		}
	}
}

void gInvDecRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hInt_t q)
{
	hDim_t blockOffset;
	hDim_t modOffset;
	hDim_t i;
	hDim_t tmp1 = rts*(p-1);
	hInt_t reciprocalOfP = reciprocal (q,p);

	for (blockOffset = 0; blockOffset < lts; ++blockOffset)
	{
		hDim_t tmp2 = blockOffset*tmp1;
		for (modOffset = 0; modOffset < rts; ++modOffset)
		{
			hDim_t tensorOffset = tmp2 + modOffset;
			hInt_t lastOut = 0;
			for (i=1; i < p; ++i)
			{
				lastOut += (i * y[tensorOffset + (i-1)*rts]);
			}
      //in the previous loop, |lastOut| <= p*p*q
      lastOut = lastOut % q;
			hInt_t acc = (lastOut * reciprocalOfP) % q;
      // |acc| <= p*q
			for (i = p-2; i > 0; --i)
			{
				hDim_t idx = tensorOffset + i*rts;
				hInt_t tmp = acc;
				acc = acc - y[idx];
				y[idx] = tmp % q;
			}
			y[tensorOffset] = acc % q;
		}
	}
}

void ppGInvDecR (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
#ifdef DEBUG_MODE
	ASSERT (q==0);
#endif
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gInvDecR ((hInt_t*)y, lts*ipow(p,e-1), rts, p);
	}
}

void ppGInvDecRq (void* y, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
	if (p != 2)
	{
		gInvDecRq ((hInt_t*)y, lts*ipow(p,e-1), rts, p, q);
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

void tensorGPowR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
    gprCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGPowR, totm, peArr, sizeOfPE, 0);

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gprTime = tsAdd(gprTime, tsSubtract(t1,s1));
#endif
}

void tensorGPowRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
#ifdef STATS
    gprqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGPowRq, totm, peArr, sizeOfPE, q);

  hDim_t j;
	for(j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        y[j]+=q;
	    }
	}

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gprqTime = tsAdd(gprqTime, tsSubtract(t1,s1));
#endif
}

void tensorGDecR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
    gdrCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGDecR, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gdrTime = tsAdd(gdrTime, tsSubtract(t1,s1));
#endif
}

void tensorGDecRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
#ifdef STATS
    gdrqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGDecRq, totm, peArr, sizeOfPE, q);

  hDim_t j;
	for(j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        y[j]+=q;
	    }
	}
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gdrqTime = tsAdd(gdrqTime, tsSubtract(t1,s1));
#endif
}

void tensorGInvPowR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
    giprCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGInvPowR, totm, peArr, sizeOfPE, 0);

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    giprTime = tsAdd(giprTime, tsSubtract(t1,s1));
#endif
}

void tensorGInvPowRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
#ifdef STATS
    giprqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGInvPowRq, totm, peArr, sizeOfPE, q);

  hDim_t j;
	for(j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        y[j]+=q;
	    }
	}
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    giprqTime = tsAdd(giprqTime, tsSubtract(t1,s1));
#endif
}

void tensorGInvDecR (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE)
{
#ifdef STATS
    gidrCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	tensorFuser (y, ppGInvDecR, totm, peArr, sizeOfPE, 0);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gidrTime = tsAdd(gidrTime, tsSubtract(t1,s1));
#endif
}

void tensorGInvDecRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q)
{
#ifdef STATS
    gidrqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    tensorFuser (y, ppGInvDecRq, totm, peArr, sizeOfPE, q);

  hDim_t j;
	for(j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        y[j]+=q;
	    }
	}

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gidrqTime = tsAdd(gidrqTime, tsSubtract(t1,s1));
#endif
}

void tensorGCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t q)
{
#ifdef STATS
    gcrqCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorGCRTRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\tq=%" PRId64 "\n[", totm, sizeOfPE, q);
    hDim_t j;
    for(j = 0; j < totm; j++) {
        printf("%" PRId64 ",", y[j]);
    }
    printf("]\n[");
    for(j = 0; j < sizeOfPE; j++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[j].prime, peArr[j].exponent);
    }
    printf("]\n");
#endif
    void** vgcoeffs = (void**)malloc(sizeOfPE*sizeof(void*));
    hDim_t i;
    for(i = 0; i < sizeOfPE; i++)
    {
        vgcoeffs[i] = (void*) (gcoeffs[i]);
    }

    tensorFuserCRT (y, ppGCRTRq, totm, peArr, sizeOfPE, vgcoeffs, q);

#ifdef DEBUG_MODE
    for(j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        printf("tensorGCRTRq\n");
	    }
	}
#endif
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gcrqTime = tsAdd(gcrqTime, tsSubtract(t1,s1));
#endif
}
void tensorGCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs)
{
#ifdef STATS
    gccCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorGCRTC\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\n[", totm, sizeOfPE);
    hDim_t j;
    for(j = 0; j < totm; j++) {
        printf("(%f,%f),", (y[j]).real, (y[j]).imag);
    }
    printf("]\n[");
    for(j = 0; j < sizeOfPE; j++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[j].prime, peArr[j].exponent);
    }
    printf("]\n");
#endif
    void** vgcoeffs = (void**)malloc(sizeOfPE*sizeof(void*));
    hDim_t i;
    for(i = 0; i < sizeOfPE; i++)
    {
        vgcoeffs[i] = (void*) (gcoeffs[i]);
    }

    tensorFuserCRT (y, ppGCRTC, totm, peArr, sizeOfPE, vgcoeffs, 0);

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    gccTime = tsAdd(gccTime, tsSubtract(t1,s1));
#endif
}
void tensorGInvCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t q)
{
#ifdef STATS
    gicrqCtr++;
#endif
    tensorGCRTRq (y, totm, peArr, sizeOfPE, gcoeffs, q); //output is already shifted
}
void tensorGInvCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs)
{
#ifdef STATS
    giccCtr++;
#endif
    tensorGCRTC (y, totm, peArr, sizeOfPE, gcoeffs);
}

