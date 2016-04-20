
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
#include <vector>

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

hInt_t reciprocal (hInt_t a, hInt_t b);

class Zq
{
public:
  hInt_t x;

  static hInt_t q; // declared here, defined in generalfuncs.cpp

  Zq& operator=(const hInt_t& c)
  {
    this->x = c % q;
    return *this;
  }
  Zq& operator+=(const Zq& b)
  {
    this->x += b.x;
    this->x %= q;
    return *this;
  }
  Zq& operator-=(const Zq& b)
  {
    this->x -= b.x;
    this->x %= q;
    return *this;
  }
  Zq& operator*=(const Zq& b)
  {
    this->x *= b.x;
    this->x %= q;
    return *this;
  }
  Zq& operator/=(const Zq& b)
  {
    Zq binv;
    binv = reciprocal(q,b.x);
    *this *= binv;
    return *this;
  }
};
inline char operator==(Zq a, const Zq& b)
{
  return (a.x == b.x);
}
inline Zq operator+(Zq a, const Zq& b)
{
  a += b;
  return a;
}
inline Zq operator-(Zq a, const Zq& b)
{
  a -= b;
  return a;
}
inline Zq operator*(Zq a, const Zq& b)
{
  a *= b;
  return a;
}
inline Zq operator/(Zq a, const Zq& b)
{
  a /= b;
  return a;
}

//http://stackoverflow.com/a/4421719
class ZqProd
{
public:
  hInt_t* xs;

  static hInt_t* qs; // declared here, defined in generalfuncs.cpp
  static hShort_t tupSize;

  ZqProd(void) {
    this->xs = (hInt_t*)malloc(tupSize*sizeof(hInt_t));
  }

  ZqProd(hInt_t* ys) {
    this->xs = (hInt_t*)malloc(tupSize*sizeof(hInt_t));
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] = ys[i];
    }
  }

  ZqProd(hInt_t c) {
    this->xs = (hInt_t*)malloc(tupSize*sizeof(hInt_t));
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] = c % qs[i];
    }
  }

  ZqProd(const ZqProd& a) {
    this->xs = (hInt_t*)malloc(tupSize*sizeof(hInt_t));
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] = a.xs[i];
    }
  }
  
  ~ZqProd() {
    free(xs);
  }

  ZqProd& operator=(const ZqProd& b) {
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] = b.xs[i];
    }
    return *this;
  }
  ZqProd& operator+=(const ZqProd& b)
  {
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] += b.xs[i];
      this->xs[i] %= qs[i];
    }
    return *this;
  }
  ZqProd& operator-=(const ZqProd& b)
  {
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] -= b.xs[i];
      this->xs[i] %= qs[i];
    }
    return *this;
  }
  ZqProd& operator*=(const ZqProd& b)
  {
    for(hShort_t i = 0; i < tupSize; i++) {
      this->xs[i] *= b.xs[i];
      this->xs[i] %= qs[i];
    }
    return *this;
  }
  ZqProd& operator/=(const ZqProd& b)
  {
    ZqProd binv;
    for(hShort_t i = 0; i < tupSize; i++) {
      binv.xs[i] = reciprocal(qs[i],b.xs[i]);
    }
    *this *= binv;
    return *this;
  }
  void canonicalize() {
    for(hShort_t i = 0; i < tupSize; i++) {
      if(xs[i] < 0) {
        xs[i] += qs[i];
      }
    }
  }
};
inline ZqProd operator+(ZqProd a, const ZqProd& b)
{
  a += b;
  return a;
}
inline ZqProd operator-(ZqProd a, const ZqProd& b)
{
  a -= b;
  return a;
}
inline ZqProd operator*(ZqProd a, const ZqProd& b)
{
  a *= b;
  return a;
}
inline ZqProd operator/(ZqProd a, const ZqProd& b)
{
  a /= b;
  return a;
}

extern ZqProd* toZqProd(hShort_t tupSize, hDim_t totm, hInt_t* y, hInt_t* qs);

class Complex
{
public:
  double real;
  double imag;

  Complex() {}
  Complex(hInt_t c) {
    this->real = c;
    this->imag = 0;
  }
  Complex& operator=(const hInt_t& c)
  {
    this->real = c;
    this->imag = 0;
    return *this;
  }
  Complex& operator+=(const Complex& b)
  {
    this->real = this->real+b.real;
    this->imag = this->imag+b.imag;
    return *this;
  }
  Complex& operator-=(const Complex& b)
  {
    this->real = this->real-b.real;
    this->imag = this->imag-b.imag;
    return *this;
  }
  Complex& operator*=(const Complex& b)
  {
    double a = this->real;
    this->real = (a*b.real)-(this->imag*b.imag);
    this->imag = (a*b.imag)+(this->imag*b.real);
    return *this;
  }
};
/*inline char operator==(Complex a, const Complex& b)
{
  return (a.real == b.real) && (a.imag == b.imag);
}*/
inline Complex operator+(Complex a, const Complex& b)
{
  a += b;
  return a;
}
inline Complex operator-(Complex a, const Complex& b)
{
  a -= b;
  return a;
}
inline Complex operator*(Complex a, const Complex& b)
{
  a *= b;
  return a;
}

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



struct  timespec  tsSubtract (struct  timespec  time1, struct  timespec  time2);
struct  timespec  tsAdd (struct  timespec  time1, struct  timespec  time2);
const  char  *tsShow (struct  timespec  binaryTime, bool  inLocal, const  char  *format);

void getStats();

typedef void (*funcPtr) (void* outputVec, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* qs);
void tensorFuser (void* y, hShort_t tupSize, funcPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);

typedef void (*normFuncPtr) (void* outputVec, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts, hInt_t* output, hInt_t* qs);
void tensorFuserNorm (void* y, hShort_t tupSize, normFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* output, hInt_t q);

typedef void (*crtFuncPtr) (void* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, void* ru, hInt_t* q);
void tensorFuserCRT (void* y, hShort_t tupSize, crtFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, void** ru, hInt_t* qs);

//#ifdef __cplusplus
template <typename ring> void tensorFuser2 (ring* y, hShort_t tupSize, void (*f) (ring* outputVec, hShort_t tupSize, PrimeExponent pe, hDim_t lts, hDim_t rts), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template <typename ring> void tensorFuserCRT2 (ring* y, hShort_t tupSize, void (*f) (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, ring* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ring** ru, hInt_t* qs);
template <typename ring> void tensorFuserCRT3 (ring* y, void (*f) (ring* y, hDim_t lts, hDim_t rts, PrimeExponent pe, ring* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ring** ru);

//#endif




#endif /* TENSORTYPES_H_ */
