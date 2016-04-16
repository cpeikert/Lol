#include "tensorTypes.h"

#ifdef STATS
int mulCtr = 0;
struct timespec mulTime = {0,0};

int addCtr = 0;
struct timespec addTime = {0,0};
#endif

//a = zipWith (*) a b
extern "C" void mulRq (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm, hInt_t* qs) {
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

extern "C" void mulC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm) {
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
extern "C" void addRq (hShort_t tupSize, hInt_t* a, const hInt_t* b, const hDim_t totm, const hInt_t* qs) {
#ifdef STATS
    addCtr++;
    struct timespec s1,t1;
    clock_gettime(CLOCK_REALTIME, &s1);
#endif

    for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
        hInt_t q = qs[tupIdx];
        for(int i = 0; i < totm; i++) {
            hInt_t temp = a[i*tupSize+tupIdx]+b[i*tupSize+tupIdx];
            if (temp >= q) a[i*tupSize+tupIdx]=temp-q;
            else a[i*tupSize+tupIdx] = temp;
        }
    }
#ifdef STATS
    clock_gettime(CLOCK_REALTIME, &t1);
    addTime = tsAdd(addTime, tsSubtract(t1,s1));
#endif
}

//a = zipWith (+) a b
extern "C" void addR (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm) {
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

extern "C" void addC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm) {
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

extern "C" void addD (hShort_t tupSize, double* a, double* b, hDim_t totm) {
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

