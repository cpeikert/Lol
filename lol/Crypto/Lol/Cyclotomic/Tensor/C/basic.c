#include "tensorTypes.h"
#ifdef CINTRIN
#include <immintrin.h>
#endif

#ifdef STATS
int mulCtr = 0;
struct timespec mulTime = {0,0};

int addCtr = 0;
struct timespec addTime = {0,0};
#endif

//a = zipWith (*) a b
void mulRq (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm, hInt_t* qs) {
#ifdef STATS
    mulCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
        hInt_t q = qs[tupIdx];
        for(int i = 0; i < totm; i++) {
            a[i*tupSize+tupIdx] = (a[i*tupSize+tupIdx]*b[i*tupSize+tupIdx])%q;
        }
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    mulTime = tsAdd(mulTime, tsSubtract(t1,s1));
#endif
}
/*
void mulMq (hShort_t tupSize, hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q) {
#ifdef STATS
    mulCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    hInt_t mask = (1<<logr)-1; // R-1

    for(int i = 0; i < totm; i++) {
        hInt_t x = a[i]*b[i];
        hInt_t s = k*(x & mask);
        hInt_t m = s & mask;
        a[i] = (x+m*q)>>logr;
    }

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    mulTime = tsAdd(mulTime, tsSubtract(t1,s1));
#endif
}
*/
void mulC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm) {
#ifdef STATS
    mulCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    for(int i = 0; i < totm*tupSize; i++)
    {
        CMPLX_IMUL(a[i],b[i]);
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    mulTime = tsAdd(mulTime, tsSubtract(t1,s1));
#endif
}

//a = zipWith (+) a b
void addRq (hShort_t tupSize, hInt_t* a, const hInt_t* b, const hDim_t totm, const hInt_t* qs) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
/*
need to check this code more carefully when tupSize > 1
#ifdef CINTRIN

    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
        hInt_t q = qs[tupIdx];
        __m128i qss = _mm_set1_epi64x(q);
        for(int i = 0; i < totm; i+=2) {
            __m128i xs = _mm_load_si128((const __m128i*)(a+(i*tupSize+tupIdx)));
            __m128i ys = _mm_load_si128((const __m128i*)(b+(i*tupSize+tupIdx)));
            __m128i zs = _mm_add_epi64(xs,ys);
            zs = _mm_rem_epi64(zs,qss);
            _mm_store_si128((__m128i*)(a+(i*tupSize+tupIdx)),zs);
        }
    }
#else
*/
    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
        hInt_t q = qs[tupIdx];
        for(int i = 0; i < totm; i++) {
            hInt_t temp = a[i*tupSize+tupIdx]+b[i*tupSize+tupIdx];
            if (temp >= q) a[i*tupSize+tupIdx]=temp-q;
            else a[i*tupSize+tupIdx] = temp;
        }
    }
//#endif
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}
/*
void addMq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    hInt_t twoq = q<<1;
    for(int i = 0; i < totm; i++) {
        hInt_t temp = (a[i]+b[i]);
        if (temp >= twoq) a[i]=temp-twoq;
        else a[i] = temp;
    }

#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}
*/
//a = zipWith (+) a b
void addR (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    for(int i = 0; i < totm*tupSize; i++)    {
        a[i] += b[i];
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}

void addC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    for(int i = 0; i < totm*tupSize; i++)
    {
        CMPLX_IADD(a[i],b[i]);
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}

void addD (hShort_t tupSize, double* a, double* b, hDim_t totm) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif
    for(int i = 0; i < totm*tupSize; i++)
    {
        a[i]+=b[i];
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}

