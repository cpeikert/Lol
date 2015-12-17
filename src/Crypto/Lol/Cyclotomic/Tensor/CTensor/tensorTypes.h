
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_


// remove next line for more efficient code
//#define DEBUG_MODE


#include <stdbool.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


#define ASSERT(EXP) { \
	if (!(EXP)) { \
		fprintf (stderr, "Assertion in file '%s' line %d : " #EXP "  is false\n", __FILE__, __LINE__); \
		exit(-1); \
	} \
}


//timers and counters
#ifdef STATS
extern int crtRqCtr;
extern int crtInvRqCtr;
extern int crtCCtr;
extern int crtInvCCtr;

extern int gprCtr;
extern int gprqCtr;
extern int gdrCtr;
extern int gdrqCtr;
extern int giprCtr;
extern int giprqCtr;
extern int gidrCtr;
extern int gidrqCtr;
extern int gcrqCtr;
extern int gccCtr;
extern int gicrqCtr;
extern int giccCtr;

extern int lrqCtr;
extern int lrCtr;
extern int ldCtr;
extern int lcCtr;
extern int lirqCtr;
extern int lirCtr;
extern int lidCtr;
extern int licCtr;

extern int normrCtr;

extern int mulCtr;
extern struct timespec mulTime;
extern int addCtr;
extern struct timespec addTime;

extern struct timespec lrqTime;
extern struct timespec lrTime;
extern struct timespec ldTime;
extern struct timespec lcTime;
extern struct timespec lirqTime;
extern struct timespec lirTime;
extern struct timespec lidTime;
extern struct timespec licTime;

extern struct timespec normrTime;

extern struct timespec crttime1;
extern struct timespec crttime2;
extern struct timespec crttime3;
extern struct timespec crttime4;
extern struct timespec crtInvRqTime;
extern struct timespec crtCTime;
extern struct timespec crtInvCTime;

extern struct timespec gprTime;
extern struct timespec gprqTime;
extern struct timespec gdrTime;
extern struct timespec gdrqTime;
extern struct timespec giprTime;
extern struct timespec giprqTime;
extern struct timespec gidrTime;
extern struct timespec gidrqTime;
extern struct timespec gcrqTime;
extern struct timespec gccTime;
#endif

typedef int64_t hInt_t ;
typedef int32_t hDim_t ;
typedef int16_t hShort_t ;
typedef int8_t hByte_t ;

typedef struct
{
	hDim_t prime;
	hShort_t exponent;
}  PrimeExponent;


typedef struct
{
	double real;
	double imag;
} complex_t;

//complex_t _add (complex_t a, complex_t b);
//complex_t _mul (complex_t a, complex_t b);

#define CMPLX_ADD(a,b)  ((complex_t){((a).real + (b).real), ((a).imag + (b).imag)})
#define CMPLX_ADD3(a,b,c)  ((complex_t){((a).real + (b).real + (c).real), ((a).imag + (b).imag + (c).imag)})
#define CMPLX_ADD4(a,b,c,d)  ((complex_t){((a).real + (b).real + (c).real + (d).real), ((a).imag + (b).imag + (c).imag + (d).imag)})
#define CMPLX_ADD5(a,b,c,d,e)  ((complex_t){((a).real + (b).real + (c).real + (d).real + (e).real), ((a).imag + (b).imag + (c).imag + (d).imag + (e).imag)})
#define CMPLX_ADD6(a,b,c,d,e,f)  ((complex_t){((a).real + (b).real + (c).real + (d).real + (e).real + (f).real), ((a).imag + (b).imag + (c).imag + (d).imag + (e).imag + (f).imag)})
#define CMPLX_ADD7(a,b,c,d,e,f,g)  ((complex_t){((a).real + (b).real + (c).real + (d).real + (e).real + (f).real + (g).real), ((a).imag + (b).imag + (c).imag + (d).imag + (e).imag + (f).imag + (g).imag)})

#define CMPLX_SUB(a,b)  ((complex_t){((a).real - (b).real), ((a).imag - (b).imag)})
#define CMPLX_MUL(a,b)  ((complex_t){((a).real*(b).real - (a).imag*(b).imag), \
	                                  (a).real*(b).imag + (a).imag*(b).real})
#define CMPLX_DIV(a,b)  ((complex_t){((a).real*(b).real + (a).imag*(b).imag)/((b).real*(b).real+(b).imag*(b).imag), \
                                     ((a).imag*(b).real - (a).real*(b).imag)/((b).real*(b).real+(b).imag*(b).imag)})

// 'inside' operators
#define CMPLX_IADD(a,b)  { (a).real += (b).real;  (a).imag += (b).imag; }
#define CMPLX_ISUB(a,b)  { (a).real -= (b).real;  (a).imag -= (b).imag; }
#define CMPLX_IMUL(a,b)  { double temp = ((a).real*(b).real - (a).imag*(b).imag); \
	                       (a).imag = ((a).real*(b).imag + (a).imag*(b).real); \
	                       (a).real = temp; }


/*
// check if both vectors are identical
bool eqInt (hInt_t a[], hInt_t b[], size_t n);
// operations on integer vectors point-wise
void addInt (hInt_t result[], hInt_t a[], hInt_t b[], size_t n);
void mulInt (hInt_t result[], hInt_t a[], hInt_t b[], size_t n);
*/

// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);
complex_t cmplxpow(complex_t base, hShort_t exp);
hInt_t qpow(hInt_t base, hShort_t exp, hInt_t q);

hInt_t reciprocal (hInt_t a, hInt_t b);

struct  timespec  tsSubtract (struct  timespec  time1, struct  timespec  time2);
struct  timespec  tsAdd (struct  timespec  time1, struct  timespec  time2);
const  char  *tsShow (struct  timespec  binaryTime, bool  inLocal, const  char  *format);

void getStats();

void mulRq (hInt_t* a, hInt_t* b, hDim_t totm, hInt_t q);
void mulMq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q);
void mulC (complex_t* a, complex_t* b, hDim_t totm);

void addR (hInt_t* a, hInt_t* b, hDim_t totm);
void addRq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hInt_t q);
void addMq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q);
void addC (complex_t* a, complex_t* b, hDim_t totm);
void addD (double* a, double* b, hDim_t totm);

typedef void (*funcPtr) (void* outputVec, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t q);
void tensorFuser (void* y, funcPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

typedef void (*normFuncPtr) (void* outputVec, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* output, hInt_t q);
void tensorFuserNorm (void* y, normFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* output, hInt_t q);

typedef void (*crtFuncPtr) (void* y, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t q);
void tensorFuserCRT (void* y, crtFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, void** ru, hInt_t q);

void tensorGPowR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGPowRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorGDecR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGDecRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorGInvPowR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGInvPowRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorGInvDecR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGInvDecRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorGCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t q);

void tensorGInvCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t q);

void tensorGCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs);

void tensorGInvCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs);



void tensorLRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorLR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorNormSqR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLDouble (double* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLC (complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvRq (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t q);

void tensorLInvR (hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvDouble (double* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvC (complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);



void tensorCRTRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t q);

void tensorCRTC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru);

void tensorCRTInvRq (hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t minv, hInt_t q);

void tensorCRTInvC (complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru, double minv);

void tensorGaussianDec (double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru);


#endif /* TENSORTYPES_H_ */

