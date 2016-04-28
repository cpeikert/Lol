
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

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

typedef int64_t hInt_t ;
typedef int32_t hDim_t ;
typedef int16_t hShort_t ;
typedef int8_t hByte_t ;

typedef struct
{
	hDim_t prime;
	hShort_t exponent;
} PrimeExponent;

hInt_t reciprocal (hInt_t a, hInt_t b);

//http://stackoverflow.com/a/4421719
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

void canonicalizeZq (Zq* y, hShort_t tupSize, hDim_t totm, hInt_t* qs);

class Complex
{
public:
  double real;
  double imag;

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
// calculates base ** exp
hDim_t ipow(hDim_t base, hShort_t exp);

template <typename ring> void tensorFuserPrime (ring* y, hShort_t tupSize, void (*f) (ring* y, hShort_t tupSize, hDim_t lts, hDim_t rts, hDim_t p), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs);
template <typename ringy, typename ringru> void tensorFuserCRT (ringy* y, hShort_t tupSize, void (*f) (ringy* y, hShort_t tupSize, hDim_t lts, hDim_t rts, PrimeExponent pe, ringru* ru), hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, ringru** ru, hInt_t* qs);

#endif /* TENSORTYPES_H_ */
