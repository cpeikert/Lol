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

void crtTwiddleRq (hInt_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hInt_t* ru, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
#ifdef DEBUG_MODE
    ASSERT(e != 0);
#endif
    pe.exponent -= 1; // used for an argument to bitrev
    
    if(p == 2)
    {
        hDim_t mprime = 1<<(e-1);
        hDim_t blockDim = rts*mprime; // size of block in block diagonal tensor matrix

        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
        {
            hDim_t temp2 = i0*rts;
            hInt_t twid = ru[bitrev(pe, i0)];

            for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++)
            {
                hDim_t temp3 = blockIdx*blockDim + temp2;
                for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                {
                    hDim_t idx = temp3 + modOffset;
                    y[idx] = (y[idx]*twid) % q;
                }
            }
        }
    }
    else // This loop is faster, probably due to the division in the loop above.
    // cilk also slows it down
    {
        hDim_t mprime = ipow(p,e-1);
        hDim_t blockDim = rts*(p-1)*mprime; // size of block in block diagonal tensor matrix
        
        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
        {
            hDim_t temp1 = i0*(p-1);
            for(hDim_t i1 = 0; i1 < (p-1); i1++) // loops over i%(p-1) for i = 0..(m'-1)
            {        
                hDim_t temp2 = (temp1+i1)*rts;
                hInt_t twid = ru[bitrev(pe, i0)*(i1+1)];

                for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++)
                {
                    hDim_t temp3 = blockIdx*blockDim + temp2;
                    for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                    {
                        hDim_t idx = temp3 + modOffset;
                        y[idx] = (y[idx]*twid) % q;
                    }
                }
            }
        }
    }
}

// dim is power of p
void dftptwidRq (hInt_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t dim, hDim_t rustride, hInt_t* ru, hInt_t q)
{
    hDim_t idx;
    hDim_t p = pe.prime;

    pe.exponent -= 1; // used for an argument to bitrev

    if(p == 2) {
        hDim_t mprime = dim>>1; // divides evenly
        hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/p for i = 0..(dim-1), but we skip i0=0
        {
            hDim_t temp3 = rts*(i0*p+1);
            hInt_t twid = ru[bitrev(pe,i0)*rustride];

            for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
            {
                hDim_t temp2 = blockOffset*temp1 + temp3;
                for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                {
                    idx = temp2 + modOffset;
                    y[idx] = (y[idx]*twid) % q;
                }
            }
        }
    }
    else
    {
        hDim_t mprime = dim/p; // divides evenly
        hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/p for i = 0..(dim-1), but we skip i0=0
        {
            for(hDim_t i1 = 1; i1 < p; i1++) // loops over i%p for i = 0..(dim-1), but we skip i1=0
            {
                hDim_t temp3 = rts*(i0*p+i1);
                hInt_t twid = ru[bitrev(pe,i0)*i1*rustride];

                for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
                {
                    hDim_t temp2 = blockOffset*temp1 + temp3;
                    for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                    {
                        idx = temp2 + modOffset;
                        y[idx] = (y[idx]*twid) % q;
                    }
                }
            }
        }
    }
}

//implied length of ru is rustride*p
//implied length of tempSpace is p, if p is not a special case
// temp is allowed to be NULL if p < DFTP_GENERIC_SIZE
void dftpRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, hInt_t* ru, hInt_t* tempSpace, hInt_t q)
{
    hDim_t tensorOffset;
    
    if(p == 2)
    {
        hDim_t temp1 = rts<<1;

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t u = y[tensorOffset];
                hInt_t t = y[tensorOffset+rts];
                y[tensorOffset] = (u + t) % q;
                y[tensorOffset+rts] = (u - t) % q;
            }
        }
    }
    else if(p == 3)
    {
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];
        hDim_t temp1 = rts*3;

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2, y3;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                //q is <32 bits, so we can do 3 additions without overflow
                y[tensorOffset]          = (y1 + y2 + y3) % q;
                y[tensorOffset+rts]      = (y1 + ((ru1*y2) % q) + ((ru2*y3) % q)) % q;
                y[tensorOffset+(rts<<1)] = (y1 + ((ru2*y2) % q) + ((ru1*y3) % q)) % q;
            }   
        }

    }
    else if(p == 5)
    {
        hDim_t temp1 = rts*5;
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];
        hInt_t ru3 = ru[rustride*3];
        hInt_t ru4 = ru[rustride<<2];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2, y3, y4, y5;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y[tensorOffset]          = (y1 + y2 + y3 + y4 + y5) % q;
                y[tensorOffset+rts]      = (y1 + ((ru1*y2) % q) + ((ru2*y3) % q) + ((ru3*y4) % q) + ((ru4*y5) % q)) % q;
                y[tensorOffset+(rts<<1)] = (y1 + ((ru2*y2) % q) + ((ru4*y3) % q) + ((ru1*y4) % q) + ((ru3*y5) % q)) % q;
                y[tensorOffset+rts*3]    = (y1 + ((ru3*y2) % q) + ((ru1*y3) % q) + ((ru4*y4) % q) + ((ru2*y5) % q)) % q;
                y[tensorOffset+(rts<<2)] = (y1 + ((ru4*y2) % q) + ((ru3*y3) % q) + ((ru2*y4) % q) + ((ru1*y5) % q)) % q;
            }
        }
    }
    else if(p == 7)
    {
        hDim_t temp1 = rts*7;
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];
        hInt_t ru3 = ru[rustride*3];
        hInt_t ru4 = ru[rustride<<2];
        hInt_t ru5 = ru[rustride*5];
        hInt_t ru6 = ru[rustride*6];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2, y3, y4, y5, y6, y7;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y6 = y[tensorOffset+rts*5];
                y7 = y[tensorOffset+rts*6];
                y[tensorOffset]          = (y1 +     y2 +     y3 +     y4 +     y5 +     y6 +     y7) % q;
                y[tensorOffset+rts]      = (y1 + ((ru1*y2) % q) + ((ru2*y3) % q) + ((ru3*y4) % q) + ((ru4*y5) % q) + ((ru5*y6) % q) + ((ru6*y7) % q)) % q;
                y[tensorOffset+(rts<<1)] = (y1 + ((ru2*y2) % q) + ((ru4*y3) % q) + ((ru6*y4) % q) + ((ru1*y5) % q) + ((ru3*y6) % q) + ((ru5*y7) % q)) % q;
                y[tensorOffset+rts*3]    = (y1 + ((ru3*y2) % q) + ((ru6*y3) % q) + ((ru2*y4) % q) + ((ru5*y5) % q) + ((ru1*y6) % q) + ((ru4*y7) % q)) % q;
                y[tensorOffset+(rts<<2)] = (y1 + ((ru4*y2) % q) + ((ru1*y3) % q) + ((ru5*y4) % q) + ((ru2*y5) % q) + ((ru6*y6) % q) + ((ru3*y7) % q)) % q;
                y[tensorOffset+rts*5]    = (y1 + ((ru5*y2) % q) + ((ru3*y3) % q) + ((ru1*y4) % q) + ((ru6*y5) % q) + ((ru4*y6) % q) + ((ru2*y7) % q)) % q;
                y[tensorOffset+rts*6]    = (y1 + ((ru6*y2) % q) + ((ru5*y3) % q) + ((ru4*y4) % q) + ((ru3*y5) % q) + ((ru2*y6) % q) + ((ru1*y7) % q)) % q;
            }   
        }
    }
    else
    {
        hDim_t temp1 = rts*p;
        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;                
                for(hDim_t row = 0; row < p; row++)
                {
                    hInt_t acc = 0;
                    //p is small (<< 30 bits), so we can do p additions of mod-q values without overflow
                    for(hDim_t col = 0; col < p; col++)
                    {
                        acc += ((y[tensorOffset+col*rts]*ru[((col*row) % p)*rustride])%q);
                    }
                    tempSpace[row] = acc % q;
                }
                
                for(hDim_t row = 0; row < p; row++)
                {
                    y[tensorOffset+rts*row] = tempSpace[row];
                }
            }
        }
    }
}

void crtpRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, hInt_t* ru, hInt_t q)
{
    hDim_t tensorOffset;
    if(p == 2)
    {
        return;
    }
    else if(p == 3)
    {
        hDim_t temp1 = rts*2;
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y[tensorOffset]     = (y1 + ((ru1*y2)%q)) % q;
                y[tensorOffset+rts] = (y1 + ((ru2*y2)%q)) % q;
            }   
        }
    }
    else if(p == 5)
    {
        hDim_t temp1 = rts*4;
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];
        hInt_t ru3 = ru[rustride*3];
        hInt_t ru4 = ru[rustride<<2];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2, y3, y4;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];

                y[tensorOffset]          = (y1 + ((ru1*y2) % q) + ((ru2*y3) % q) + ((ru3*y4) % q)) % q;
                y[tensorOffset+rts]      = (y1 + ((ru2*y2) % q) + ((ru4*y3) % q) + ((ru1*y4) % q)) % q;
                y[tensorOffset+(rts<<1)] = (y1 + ((ru3*y2) % q) + ((ru1*y3) % q) + ((ru4*y4) % q)) % q;
                y[tensorOffset+rts*3]    = (y1 + ((ru4*y2) % q) + ((ru3*y3) % q) + ((ru2*y4) % q)) % q;
            }   
        }
    }
    else if(p == 7)
    {
        hDim_t temp1 = rts*6;
        hInt_t ru1 = ru[rustride];
        hInt_t ru2 = ru[rustride<<1];
        hInt_t ru3 = ru[rustride*3];
        hInt_t ru4 = ru[rustride<<2];
        hInt_t ru5 = ru[rustride*5];
        hInt_t ru6 = ru[rustride*6];

        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hInt_t y1, y2, y3, y4, y5, y6;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y6 = y[tensorOffset+rts*5];
                y[tensorOffset]          = (y1 + ((ru1*y2) % q) + ((ru2*y3) % q) + ((ru3*y4) % q) + ((ru4*y5) % q) + ((ru5*y6) % q)) % q;
                y[tensorOffset+rts]      = (y1 + ((ru2*y2) % q) + ((ru4*y3) % q) + ((ru6*y4) % q) + ((ru1*y5) % q) + ((ru3*y6) % q)) % q;
                y[tensorOffset+(rts<<1)] = (y1 + ((ru3*y2) % q) + ((ru6*y3) % q) + ((ru2*y4) % q) + ((ru5*y5) % q) + ((ru1*y6) % q)) % q;
                y[tensorOffset+rts*3]    = (y1 + ((ru4*y2) % q) + ((ru1*y3) % q) + ((ru5*y4) % q) + ((ru2*y5) % q) + ((ru6*y6) % q)) % q;
                y[tensorOffset+(rts<<2)] = (y1 + ((ru5*y2) % q) + ((ru3*y3) % q) + ((ru1*y4) % q) + ((ru6*y5) % q) + ((ru4*y6) % q)) % q;
                y[tensorOffset+rts*5]    = (y1 + ((ru6*y2) % q) + ((ru5*y3) % q) + ((ru4*y4) % q) + ((ru3*y5) % q) + ((ru2*y6) % q)) % q;
            }
        }
    }
    else
    {
        hInt_t* tempSpace = (hInt_t*)malloc((p-1)*sizeof(hInt_t));
        hDim_t temp1 = rts*(p-1);
        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                
                for(hDim_t row = 1; row < p; row++)
                {
                    hInt_t acc = 0;
                    for(hDim_t col = 0; col < p-1; col++)
                    {
                        acc += ((y[tensorOffset+col*rts]*ru[((col*row) % p)*rustride]) % q);
                    }
                    tempSpace[row-1] = acc % q;
                }
                
                for(hDim_t row = 0; row < p-1; row++)
                {
                    y[tensorOffset+rts*row] = tempSpace[row];
                }
            }
        }
        free(tempSpace);
    }
}

//takes inverse rus
void crtpinvRq (hInt_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, hInt_t* ruinv, hInt_t q)
{
    if(p ==2)
    {
        // need this case so that we can divide overall by mhat^(-1)
        return;
    }
    else
    {
        hDim_t tensorOffset,i;
        hInt_t* tempSpace = (hInt_t*)malloc((p-1)*sizeof(hInt_t));
        hDim_t temp1 = rts*(p-1);
        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                
                for(i = 0; i < p-1; i++)
                {
                    hInt_t sum = 0;
                    int j;
                    for(j = 0; j < p-1; j++)
                    {
                        int ruIdx = ((j+1)*i) % p;
                        sum += ((y[tensorOffset+j*rts] * ruinv[ruIdx*rustride]) % q);
                    }
                    tempSpace[i] = sum % q;
                }

                hInt_t shift = 0;
                for(i = 0; i < p-1; i++)
                {
                    // we were given the inverse rus, so we need to negate the indices
                    shift += ((y[tensorOffset+i*rts] * ruinv[rustride*(p-(i+1))]) % q);
                }

                for(i = 0; i < p-1; i++)
                {
                    y[tensorOffset+i*rts] = (tempSpace[i] - shift) % q; 
                }
            }
        }
    }
}

void ppDFTRq (hInt_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t rustride, hInt_t* ru, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
    if(e == 0)
    {
        return;
    }
    
    hDim_t primeRuStride = rustride*ipow(p,e-1);
    hInt_t* temp = 0;
    if(p >= DFTP_GENERIC_SIZE)
    {
        temp = (hInt_t*)malloc(p*sizeof(hInt_t));
    }
    hShort_t i;
    
    hDim_t ltsScale = ipow(p,e-1);
    hDim_t rtsScale = 1;
    hDim_t twidRuStride = rustride;
    for(i = 0; i < e; i++)
    {
        hDim_t rtsDim = rts*rtsScale;
        dftpRq (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp, q);
        dftptwidRq (y, lts, rtsDim, pe, ltsScale*p, twidRuStride, ru, q);
        
        ltsScale /= p;
        rtsScale *= p;
        twidRuStride *= p;
        pe.exponent -= 1;
    }
    
    if(p >= DFTP_GENERIC_SIZE)
    {
        free(temp);
    }
}

void ppDFTInvRq (hInt_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t rustride, hInt_t* ru, hInt_t q)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
    if(e == 0)
    {
        return;
    }
    hDim_t primeRuStride = rustride*ipow(p,e-1);
    hInt_t* temp = 0;
    if(p >= DFTP_GENERIC_SIZE)
    {
        temp = (hInt_t*)malloc(p*sizeof(hInt_t));
    }
    hShort_t i;
    
    hDim_t ltsScale = 1;
    hDim_t rtsScale = ipow(p,e-1);
    hDim_t twidRuStride = primeRuStride;
    pe.exponent = 1;
    for(i = 0; i < e; i++)
    {
        hDim_t rtsDim = rts*rtsScale;
        hDim_t ltsScaleP = ltsScale*p;
        dftptwidRq (y, lts, rtsDim, pe, ltsScaleP, twidRuStride, ru, q);
        dftpRq (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp, q);
        
        ltsScale = ltsScaleP;
        rtsScale /= p;
        twidRuStride /= p;
        pe.exponent += 1;
    }
    
    if(p >= DFTP_GENERIC_SIZE)
    {
        free(temp);
    }
}

void ppcrtRq (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t q)
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
        printf("%" PRId64 ",", ((hInt_t*)ru)[i]);
    }
    printf("]\n");
#endif
    
    crtpRq ((hInt_t*)y, lts*mprime, rts, p, mprime, (hInt_t*)ru, q);
    crtTwiddleRq ((hInt_t*)y, lts, rts, pe, (hInt_t*)ru, q);
    pe.exponent -= 1;
    ppDFTRq ((hInt_t*)y, lts, rts*(p-1), pe, p, (hInt_t*)ru, q);
}

void ppcrtinvRq (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t q)
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
        printf("%" PRId64 ",", ((hInt_t*)ru)[i]);
    }
    printf("]\n");
#endif
    pe.exponent -= 1;
    ppDFTInvRq ((hInt_t*)y, lts, rts*(p-1), pe, p, (hInt_t*)ru, q);
    pe.exponent += 1;
    crtTwiddleRq ((hInt_t*)y, lts, rts, pe, (hInt_t*)ru, q);
    crtpinvRq ((hInt_t*)y, lts*mprime, rts, p, mprime, (hInt_t*)ru, q);
}

// EAC: Somebody who knows C/C++ should find a better way to handle pointers-to-pointers in a generic way
void tensorCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t q)
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
    printf("\n\nEntered tensorCRTRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\tq=%" PRId64 "\n[", totm, sizeOfPE, q);

    for(i = 0; i < totm; i++) {
        printf("%" PRId64 ",", y[i]);
    }
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif
    void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
    
    for(i = 0; i < sizeOfPE; i++)
    {
        rus[i] = (void*) (ru[i]);
    }
	tensorFuserCRT (y, ppcrtRq, totm, peArr, sizeOfPE, rus, q);
	
	for(hDim_t j = 0; j < totm; j++)
	{
	    if(y[j]<0)
	    {
	        y[j]+=q;
	    }
#ifdef DEBUG_MODE
	    if(y[j]<0)
	    {
	        printf("TENSOR CRT^T INV\n");
	    }
#endif
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
void tensorCRTInvRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ruinv, hInt_t mhatInv, hInt_t q)
{
	hDim_t i;
#ifdef STATS
    struct timespec s1,t1;
    crtInvRqCtr++;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
#ifdef DEBUG_MODE
    printf("\n\nEntered tensorCRTInvRq\ttotm=%" PRId32 "\tnumFacts=%" PRId16 "\tminv=%" PRId64 "\tq=%" PRId64 "\n[", totm, sizeOfPE, mhatInv, q);
    for(i = 0; i < totm; i++) {
        printf("%" PRId64 ",", y[i]);
    }
    printf("]\n[");
    for(i = 0; i < sizeOfPE; i++) {
        printf("(%" PRId32 ",%" PRId16 "),", peArr[i].prime, peArr[i].exponent);
    }
    printf("]\n");
#endif

	void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
    for(i = 0; i < sizeOfPE; i++)
    {
        rus[i] = (void*) (ruinv[i]);
    }
	
	tensorFuserCRT (y, ppcrtinvRq, totm, peArr, sizeOfPE, rus, q);

	for (hDim_t j = 0; j < totm; j++)
	{
	    y[j] = (y[j]*mhatInv)%q;
	    if(y[j] < 0)
	    {
	        y[j] +=q;
	    }
#ifdef DEBUG_MODE
	    if(y[j]<0)
	    {
	        printf("TENSOR CRT INV\n");
	    }
#endif
	}

	free(rus);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    crtInvRqTime = tsAdd(crtInvRqTime, tsSubtract(t1,s1));
#endif
}














void crtTwiddleC (complex_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, complex_t* ru)
{
    hDim_t idx;
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
#ifdef DEBUG_MODE
    ASSERT(e != 0);
#endif

    pe.exponent -= 1; // used for an argument to bitrev
    
    if(p == 2)
    {
        hDim_t mprime = 1<<(e-1);
        hDim_t blockDim = rts*mprime; // size of block in block diagonal tensor matrix

        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
        {
            hDim_t temp2 = i0*rts;
            complex_t twid = ru[bitrev(pe,i0)];

            for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++)
            {
                hDim_t temp3 = blockIdx*blockDim + temp2;
                for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                {
                    idx = temp3 + modOffset;
                    CMPLX_IMUL(y[idx],twid);
                }
            }
        }
    }
    else
    {
        hDim_t mprime = ipow(p,e-1);
        hDim_t blockDim = rts*(p-1)*mprime; // size of block in block diagonal tensor matrix

        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/(p-1) for i = 0..(m'-1), we can skip i0 = 0
        {
            hDim_t temp1 = i0*(p-1);
            for(hDim_t i1 = 0; i1 < (p-1); i1++) // loops over i%(p-1) for i = 0..(m'-1)
            {        
                hDim_t temp2 = (temp1+i1)*rts;
                complex_t twid = ru[bitrev(pe,i0)*(i1+1)];

                for(hDim_t blockIdx = 0; blockIdx < lts; blockIdx++)
                {
                    hDim_t temp3 = blockIdx*blockDim + temp2;
                    for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                    {
                        idx = temp3 + modOffset;
                        CMPLX_IMUL(y[idx],twid);
                    }
                }
            }
        }
    }
}
    
// dim is power of p
void dftptwidC (complex_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t dim, hDim_t rustride, complex_t* ru)
{
    hDim_t idx;
    hDim_t p = pe.prime;
    pe.exponent -= 1; // used for an argument to bitrev
    
    if(p == 2)
    {
        hDim_t mprime = dim>>1; // divides evenly
        hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/p for i = 0..(dim-1), but we skip i0=0
        {
            hDim_t temp3 = rts*(i0*p+1);
            complex_t twid = ru[bitrev(pe,i0)*rustride];

            for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
            {
                hDim_t temp2 = blockOffset*temp1 + temp3;
                for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                {
                    idx = temp2 + modOffset;
                    CMPLX_IMUL(y[idx],twid);
                }
            }
        }
    }
    else
    {
        hDim_t mprime = dim/p; // divides evenly
        hDim_t temp1 = rts*dim; // for use in computing [modified] tensorOffset
        for(hDim_t i0 = 1; i0 < mprime; i0++) // loops over i/p for i = 0..(dim-1), but we skip i0=0
        {
            for(hDim_t i1 = 1; i1 < p; i1++) // loops over i%p for i = 0..(dim-1), but we skip i1=0
            {
                hDim_t temp3 = rts*(i0*p+i1);
                complex_t twid = ru[bitrev(pe,i0)*i1*rustride];

                for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
                {
                    hDim_t temp2 = blockOffset*temp1 + temp3;
                    for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
                    {
                        idx = temp2 + modOffset;
                        CMPLX_IMUL(y[idx],twid);
                    }
                }
            }
        }
    }
}

//implied length of ru is rustride*p
//implied length of tempSpace is p, if p is not a special case
void dftpC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, complex_t* ru, complex_t* tempSpace)
{
    hDim_t blockOffset, modOffset, tensorOffset;
    
    if(p == 2)
    {
        hDim_t temp1 = rts<<1;
        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t u = y[tensorOffset];
                complex_t t = y[tensorOffset+rts];
                y[tensorOffset] = CMPLX_ADD(u,t);
                y[tensorOffset+rts] = CMPLX_SUB(u,t);
            }
        }
    }
    else if(p == 3)
    {
        hDim_t temp1 = rts*3;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2, y3;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y[tensorOffset]          = CMPLX_ADD3(y1,               y2,                y3);
                y[tensorOffset+rts]      = CMPLX_ADD3(y1, CMPLX_MUL(ru1,y2), CMPLX_MUL(ru2,y3));
                y[tensorOffset+(rts<<1)] = CMPLX_ADD3(y1, CMPLX_MUL(ru2,y2), CMPLX_MUL(ru1,y3));
            }   
        }
    }
    else if(p == 5)
    {
        hDim_t temp1 = rts*5;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];
        complex_t ru3 = ru[rustride*3];
        complex_t ru4 = ru[rustride<<2];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2, y3, y4, y5;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y[tensorOffset]          = CMPLX_ADD5(y1,               y2,                y3,                y4,                y5);
                y[tensorOffset+rts]      = CMPLX_ADD5(y1, CMPLX_MUL(ru1,y2), CMPLX_MUL(ru2,y3), CMPLX_MUL(ru3,y4), CMPLX_MUL(ru4,y5));
                y[tensorOffset+(rts<<1)] = CMPLX_ADD5(y1, CMPLX_MUL(ru2,y2), CMPLX_MUL(ru4,y3), CMPLX_MUL(ru1,y4), CMPLX_MUL(ru3,y5));
                y[tensorOffset+rts*3]    = CMPLX_ADD5(y1, CMPLX_MUL(ru3,y2), CMPLX_MUL(ru1,y3), CMPLX_MUL(ru4,y4), CMPLX_MUL(ru2,y5));
                y[tensorOffset+(rts<<2)] = CMPLX_ADD5(y1, CMPLX_MUL(ru4,y2), CMPLX_MUL(ru3,y3), CMPLX_MUL(ru2,y4), CMPLX_MUL(ru1,y5));
            }   
        }
    }
    else if(p == 7)
    {
        hDim_t temp1 = rts*7;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];
        complex_t ru3 = ru[rustride*3];
        complex_t ru4 = ru[rustride<<2];
        complex_t ru5 = ru[rustride*5];
        complex_t ru6 = ru[rustride*6];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2, y3, y4, y5, y6, y7;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y6 = y[tensorOffset+rts*5];
                y7 = y[tensorOffset+rts*6];
                y[tensorOffset]          = CMPLX_ADD7(y1,               y2,                y3,                y4,                y5,                y6,                y7);
                y[tensorOffset+rts]      = CMPLX_ADD7(y1, CMPLX_MUL(ru1,y2), CMPLX_MUL(ru2,y3), CMPLX_MUL(ru3,y4), CMPLX_MUL(ru4,y5), CMPLX_MUL(ru5,y6), CMPLX_MUL(ru6,y7));
                y[tensorOffset+(rts<<1)] = CMPLX_ADD7(y1, CMPLX_MUL(ru2,y2), CMPLX_MUL(ru4,y3), CMPLX_MUL(ru6,y4), CMPLX_MUL(ru1,y5), CMPLX_MUL(ru3,y6), CMPLX_MUL(ru5,y7));
                y[tensorOffset+rts*3]    = CMPLX_ADD7(y1, CMPLX_MUL(ru3,y2), CMPLX_MUL(ru6,y3), CMPLX_MUL(ru2,y4), CMPLX_MUL(ru5,y5), CMPLX_MUL(ru1,y6), CMPLX_MUL(ru4,y7));
                y[tensorOffset+(rts<<2)] = CMPLX_ADD7(y1, CMPLX_MUL(ru4,y2), CMPLX_MUL(ru1,y3), CMPLX_MUL(ru5,y4), CMPLX_MUL(ru2,y5), CMPLX_MUL(ru6,y6), CMPLX_MUL(ru3,y7));
                y[tensorOffset+rts*5]    = CMPLX_ADD7(y1, CMPLX_MUL(ru5,y2), CMPLX_MUL(ru3,y3), CMPLX_MUL(ru1,y4), CMPLX_MUL(ru6,y5), CMPLX_MUL(ru4,y6), CMPLX_MUL(ru2,y7));
                y[tensorOffset+rts*6]    = CMPLX_ADD7(y1, CMPLX_MUL(ru6,y2), CMPLX_MUL(ru5,y3), CMPLX_MUL(ru4,y4), CMPLX_MUL(ru3,y5), CMPLX_MUL(ru2,y6), CMPLX_MUL(ru1,y7));
            }   
        }
    }
    else
    {
        hDim_t temp1 = rts*p;
        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hDim_t row, col;
                
                for(row = 0; row < p; row++)
                {
                    complex_t acc = ((complex_t){0,0});
                    for(col = 0; col < p; col++)
                    {
                        CMPLX_IADD(acc, CMPLX_MUL(y[tensorOffset+col*rts], ru[((col*row) % p)*rustride]));
                    }
                    tempSpace[row] = acc;
                }
                
                for(row = 0; row < p; row++)
                {
                    y[tensorOffset+rts*row] = tempSpace[row];   
                }
            }
        }
    }
}

void crtpC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, complex_t* ru)
{
    hDim_t blockOffset, modOffset, tensorOffset;
    
    if(p == 2)
    {
        return;
    }
    else if(p == 3)
    {
        hDim_t temp1 = rts*2;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y[tensorOffset]     = CMPLX_ADD(y1, CMPLX_MUL(ru1,y2));
                y[tensorOffset+rts] = CMPLX_ADD(y1, CMPLX_MUL(ru2,y2));
            }   
        }
    }
    else if(p == 5)
    {
        hDim_t temp1 = rts*4;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];
        complex_t ru3 = ru[rustride*3];
        complex_t ru4 = ru[rustride<<2];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2, y3, y4;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y[tensorOffset]          = CMPLX_ADD4(y1, CMPLX_MUL(ru1,y2), CMPLX_MUL(ru2,y3), CMPLX_MUL(ru3,y4));
                y[tensorOffset+rts]      = CMPLX_ADD4(y1, CMPLX_MUL(ru2,y2), CMPLX_MUL(ru4,y3), CMPLX_MUL(ru1,y4));
                y[tensorOffset+(rts<<1)] = CMPLX_ADD4(y1, CMPLX_MUL(ru3,y2), CMPLX_MUL(ru1,y3), CMPLX_MUL(ru4,y4));
                y[tensorOffset+rts*3]    = CMPLX_ADD4(y1, CMPLX_MUL(ru4,y2), CMPLX_MUL(ru3,y3), CMPLX_MUL(ru2,y4));
            }   
        }
    }
    else if(p == 7)
    {
        hDim_t temp1 = rts*6;
        complex_t ru1 = ru[rustride];
        complex_t ru2 = ru[rustride<<1];
        complex_t ru3 = ru[rustride*3];
        complex_t ru4 = ru[rustride<<2];
        complex_t ru5 = ru[rustride*5];
        complex_t ru6 = ru[rustride*6];

        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                complex_t y1, y2, y3, y4, y5, y6;
                y1 = y[tensorOffset];
                y2 = y[tensorOffset+rts];
                y3 = y[tensorOffset+(rts<<1)];
                y4 = y[tensorOffset+3*rts];
                y5 = y[tensorOffset+(rts<<2)];
                y6 = y[tensorOffset+rts*5];
                y[tensorOffset]          = CMPLX_ADD6(y1, CMPLX_MUL(ru1,y2), CMPLX_MUL(ru2,y3), CMPLX_MUL(ru3,y4), CMPLX_MUL(ru4,y5), CMPLX_MUL(ru5,y6));
                y[tensorOffset+rts]      = CMPLX_ADD6(y1, CMPLX_MUL(ru2,y2), CMPLX_MUL(ru4,y3), CMPLX_MUL(ru6,y4), CMPLX_MUL(ru1,y5), CMPLX_MUL(ru3,y6));
                y[tensorOffset+(rts<<1)] = CMPLX_ADD6(y1, CMPLX_MUL(ru3,y2), CMPLX_MUL(ru6,y3), CMPLX_MUL(ru2,y4), CMPLX_MUL(ru5,y5), CMPLX_MUL(ru1,y6));
                y[tensorOffset+rts*3]    = CMPLX_ADD6(y1, CMPLX_MUL(ru4,y2), CMPLX_MUL(ru1,y3), CMPLX_MUL(ru5,y4), CMPLX_MUL(ru2,y5), CMPLX_MUL(ru6,y6));
                y[tensorOffset+(rts<<2)] = CMPLX_ADD6(y1, CMPLX_MUL(ru5,y2), CMPLX_MUL(ru3,y3), CMPLX_MUL(ru1,y4), CMPLX_MUL(ru6,y5), CMPLX_MUL(ru4,y6));
                y[tensorOffset+rts*5]    = CMPLX_ADD6(y1, CMPLX_MUL(ru6,y2), CMPLX_MUL(ru5,y3), CMPLX_MUL(ru4,y4), CMPLX_MUL(ru3,y5), CMPLX_MUL(ru2,y6));
            }   
        }
    }
    else
    {
        complex_t* tempSpace = (complex_t*)malloc((p-1)*sizeof(complex_t));
        hDim_t temp1 = rts*(p-1);
        for(blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;
                hDim_t row, col;
                
                for(row = 1; row < p; row++)
                {
                    complex_t acc = ((complex_t){0,0});
                    for(col = 0; col < p-1; col++)
                    {
                        CMPLX_IADD(acc, CMPLX_MUL(y[tensorOffset+col*rts], ru[((col*row) % p)*rustride]));
                    }
                    tempSpace[row-1] = acc;
                }
                
                for(row = 0; row < p-1; row++)
                {
                    y[tensorOffset+rts*row] = tempSpace[row];   
                }
            }
        }
        free(tempSpace);
    }
}

//takes inverse rus
void crtpinvC (complex_t* y, hDim_t lts, hDim_t rts, hDim_t p, hDim_t rustride, complex_t* ruinv)
{
    if(p ==2)
    {
        // need this case so that we can divide overall by mhat^(-1)
        return;
    }
    else
    {
        hDim_t tensorOffset,i;
        complex_t* tempSpace = (complex_t*)malloc(p*sizeof(complex_t));
        hDim_t temp1 = rts*(p-1);
        for(hDim_t blockOffset = 0; blockOffset < lts; blockOffset++)
        {
            hDim_t temp2 = blockOffset*temp1;
            for(hDim_t modOffset = 0; modOffset < rts; modOffset++)
            {
                tensorOffset = temp2 + modOffset;

                for(i = 0; i < p-1; i++)
                {
                    complex_t sum = ((complex_t){0,0});
                    int j;
                    for(j = 0; j < p-1; j++)
                    {
                        int ruIdx = (((j+1)*i) % p)*rustride;
                        CMPLX_IADD(sum, CMPLX_MUL(y[tensorOffset+j*rts],ruinv[ruIdx]));
                    }
                    tempSpace[i] = sum;
                }

                complex_t shift = ((complex_t){0,0});
                for(i = 0; i < p-1; i++)
                {
                    // we were given the inverse rus, so we need to negate the indices
                    int ruIdx = p-(i+1);
                    CMPLX_IADD(shift, CMPLX_MUL(y[tensorOffset+i*rts], ruinv[rustride*ruIdx]));
                }

                for(i = 0; i < p-1; i++)
                {
                    y[tensorOffset+i*rts] = CMPLX_SUB(tempSpace[i], shift); 
                }
            }
        }
    }
}

void ppDFTC (complex_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t rustride, complex_t* ru)
{
    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
    if(e == 0)
    {
        return;
    }
    
    hDim_t primeRuStride = rustride*ipow(p,e-1);    
    complex_t* temp = 0;
    if(p >= DFTP_GENERIC_SIZE)
    {
        temp = (complex_t*)malloc(p*sizeof(complex_t));
    }
    hShort_t i;
    
    hDim_t ltsScale = ipow(p,e-1);
    hDim_t rtsScale = 1;
    hDim_t twidRuStride = rustride;
    for(i = 0; i < e; i++)
    {
        hDim_t rtsDim = rts*rtsScale;
        dftpC (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);
        dftptwidC (y, lts, rtsDim, pe, ltsScale*p, twidRuStride, ru);
        
        ltsScale /= p;
        rtsScale *= p;
        twidRuStride *= p;
        pe.exponent -= 1;
    }
    
    if(p >= DFTP_GENERIC_SIZE)
    {
        free(temp);
    }
}

void ppDFTInvC (complex_t* y, hDim_t lts, hDim_t rts, PrimeExponent pe, hDim_t rustride, complex_t* ru)
{

    hDim_t p = pe.prime;
    hShort_t e = pe.exponent;
    
    if(e == 0)
    {
        return;
    }
    hDim_t primeRuStride = rustride*ipow(p,e-1);
    complex_t* temp = 0;
    if(p >= DFTP_GENERIC_SIZE)
    {
        temp = (complex_t*)malloc(p*sizeof(complex_t));
    }
    hShort_t i;
    
    hDim_t ltsScale = 1;
    hDim_t rtsScale = ipow(p,e-1);
    hDim_t twidRuStride = primeRuStride;
    pe.exponent = 1;
    for(i = 0; i < e; i++)
    {
        hDim_t rtsDim = rts*rtsScale;
        hDim_t ltsScaleP = ltsScale*p;
        dftptwidC (y, lts, rtsDim, pe, ltsScaleP, twidRuStride, ru);
        dftpC (y, lts*ltsScale, rtsDim, p, primeRuStride, ru, temp);
        
        ltsScale = ltsScaleP;
        rtsScale /= p;
        twidRuStride /= p;
        pe.exponent += 1;
    }
    
    if(p >= DFTP_GENERIC_SIZE)
    {
        free(temp);
    }
}

void ppcrtC (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t q)
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
        printf("(%f,%f),", ((complex_t*)ru)[i].real, ((complex_t*)ru)[i].imag);
    }
    printf("]\n");
#endif

    crtpC ((complex_t*)y, lts*mprime, rts, p, mprime, (complex_t*)ru);
    crtTwiddleC ((complex_t*)y, lts, rts, pe, (complex_t*)ru);
    pe.exponent -= 1;
    ppDFTC ((complex_t*)y, lts, rts*(p-1), pe, p, (complex_t*)ru);
}

void ppcrtinvC (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t q)
{
    hDim_t p = pe.prime;
    hDim_t e = pe.exponent;
#ifdef DEBUG_MODE
    ASSERT(e != 0);
#endif
    hDim_t mprime = ipow(p,e-1);
    
    pe.exponent -= 1;
    ppDFTInvC ((complex_t*)y, lts, rts*(p-1), pe, p, (complex_t*)ru);
    pe.exponent += 1;
    crtTwiddleC ((complex_t*)y, lts, rts, pe, (complex_t*)ru);
    crtpinvC ((complex_t*)y, lts*mprime, rts, p, mprime, (complex_t*)ru);
}

void tensorCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru)
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
    for(i = 0; i < sizeOfPE; i++)
    {
        rus[i] = (void*) (ru[i]);
    }
	tensorFuserCRT (y, ppcrtC, totm, peArr, sizeOfPE, rus, 0);
	free(rus);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    crtCTime = tsAdd(crtCTime, tsSubtract(t1,s1));
#endif
}

//takes inverse rus
void tensorCRTInvC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ruinv, double mhatInv)
{
#ifdef STATS
    struct timespec s1,t1;
    crtInvCCtr++;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
	hDim_t i;
	
	void** rus = (void**)malloc(sizeOfPE*sizeof(void*));
    for(i = 0; i < sizeOfPE; i++)
    {
        rus[i] = (void*) (ruinv[i]);
    }
	
	tensorFuserCRT (y, ppcrtinvC, totm, peArr, sizeOfPE, rus, 0);
	complex_t minvcmplx = ((complex_t){mhatInv,0});

	for (hDim_t j = 0; j < totm; j++)
	{
	    CMPLX_IMUL(y[j], minvcmplx);
	}
	
	free(rus);
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    crtInvCTime = tsAdd(crtInvCTime, tsSubtract(t1,s1));
#endif
}
