
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
extern int gpcCtr;
extern int gprqCtr;
extern int gdrCtr;
extern int gdrqCtr;
extern int giprCtr;
extern int gipcCtr;
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
extern struct timespec gpcTime;
extern struct timespec gprqTime;
extern struct timespec gdrTime;
extern struct timespec gdrqTime;
extern struct timespec giprTime;
extern struct timespec gipcTime;
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

// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);
complex_t cmplxpow(complex_t base, hShort_t exp);
hInt_t qpow(hInt_t base, hShort_t exp, hInt_t q);

hInt_t reciprocal (hInt_t a, hInt_t b);

struct  timespec  tsSubtract (struct  timespec  time1, struct  timespec  time2);
struct  timespec  tsAdd (struct  timespec  time1, struct  timespec  time2);
const  char  *tsShow (struct  timespec  binaryTime, bool  inLocal, const  char  *format);

void getStats();

void mulRq (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm, hInt_t* qs);
//void mulMq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q);
void mulC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm);

void addR (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm);
void addRq (hShort_t tupSize, hInt_t* a, const hInt_t* b, const hDim_t totm, const hInt_t* qs);
//void addMq (hInt_t* a, const hInt_t* b, const hDim_t totm, const hByte_t logr, const hInt_t k, const hInt_t q);
void addC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm);
void addD (hShort_t tupSize, double* a, double* b, hDim_t totm);

typedef void (*funcPtr) (void* outputVec, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs);
void tensorFuser (void* y, hShort_t tupSize, funcPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

typedef void (*normFuncPtr) (void* outputVec, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* output, hInt_t* qs);
void tensorFuserNorm (void* y, hShort_t tupSize, normFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* output, hInt_t q);

typedef void (*crtFuncPtr) (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t* q);
void tensorFuserCRT (void* y, hShort_t tupSize, crtFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, void** ru, hInt_t* q);

void tensorGPowR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGPowRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorGPowC (hShort_t tupSize, complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGDecR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGDecRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorGInvPowR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGInvPowRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorGInvPowC (hShort_t tupSize, complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGInvDecR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorGInvDecRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorGCRTRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t* qs);

void tensorGInvCRTRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** gcoeffs, hInt_t* qs);

void tensorGCRTC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs);

void tensorGInvCRTC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** gcoeffs);



void tensorLRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorLR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLDouble (hShort_t tupSize, double* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLC (hShort_t tupSize, complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvRq (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

void tensorLInvR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvDouble (hShort_t tupSize, double* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);

void tensorLInvC (hShort_t tupSize, complex_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);



void tensorCRTRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t* qs);

void tensorCRTC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru);

void tensorCRTInvRq (hShort_t tupSize, hInt_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t** ru, hInt_t* minv, hInt_t* qs);

void tensorCRTInvC (hShort_t tupSize, complex_t* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru, complex_t* minv);



void tensorGaussianDec (hShort_t tupSize, double* y, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, complex_t** ru);

void tensorNormSqR (hShort_t tupSize, hInt_t* x, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE);


#endif /* TENSORTYPES_H_ */

