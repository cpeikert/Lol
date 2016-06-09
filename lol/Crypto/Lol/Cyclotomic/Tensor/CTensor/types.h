
#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

#include <inttypes.h>

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

//http://stackoverflow.com/questions/37572628
#ifdef __cplusplus
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

#endif /* __cplusplus */
#endif /* TENSORTYPES_H_ */
