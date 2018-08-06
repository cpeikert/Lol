/*
Module      : types.h
Description : C++ types and function declarations.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Typedefs and data type for modular arithmetic.
*/

#ifndef TENSORTYPES_H_
#define TENSORTYPES_H_

#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <cstdlib>

typedef int64_t hInt_t;
typedef int32_t hDim_t;
typedef int16_t hShort_t;
typedef int8_t hByte_t;

// data type for a prime/exponent pair
typedef struct
{
  hShort_t prime;
  hShort_t exponent;
} PrimeExponent;

// computes the reciprocal of b mod a
hInt_t reciprocal (hInt_t a, hInt_t b);

#define ASSERT(EXP) { \
  if (!(EXP)) { \
    fprintf (stderr, "Assertion in file '%s' line %d : " #EXP "  is false\n", __FILE__, __LINE__); \
    exit(-1); \
  } \
}

// GHC can't handle C++ code in header files, so we must put guards around C++
// code in header files: http://stackoverflow.com/questions/37572628
#ifdef __cplusplus
//http://stackoverflow.com/a/4421719

// Class for integers modulo a number q.
// For efficiency, all operators output values in the range -q < x < q.
// This allows us to do the final conversion to canonical form only once at the
// end.
class Zq
{
public:
  // value in the range -q < x < q
  hInt_t x;

  // global modulus, which allows us to cast values to a Zq type for efficiency.
  static hInt_t q; // declared here, defined in common.cpp due to GHC #12152.

  // default constructor
  Zq() : x(0) {}

  inline Zq operator-()
  {
    Zq out;
    out.x = -x;
    return out;
  }

  // turn an hInt_t into a Zq with modular reduction.
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
  Zq& operator*=(const hInt_t& b)
  {
    Zq c;
    c = b;
    *this *= c;
    return *this;
  }
  // fails if b is not invertible mod q
  Zq& operator/=(const Zq& b)
  {
    Zq binv;
    binv = reciprocal(q,b.x);
    ASSERT (binv.x); // binv == 0 indicates that x is not invertible mod q
    *this *= binv;
    return *this;
  }
};

// binary operators defined in terms of the unary operators
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
inline Zq operator*(Zq a, const hInt_t& b)
{
  a *= b;
  return a;
}
inline Zq operator/(Zq a, const Zq& b)
{
  a /= b;
  return a;
}

// Converts a Zq value into the range 0 <= x < q. This should be called
// before returning to Haskell.
void canonicalizeZq (Zq* y, hDim_t totm, hInt_t q);


// Helper function. Outputs the fractional part of a given double, i.e., things in the range (-1, 1)
inline double fractional_part(double x)
{
    double _dummy;
    return modf(x, &_dummy);
}

// Class for real numbers modulo qZ.
// For efficiency, all operators output values in the range -1 < x < 1.
// This allows us to do the final conversion to canonical form only once at the
// end.
class RRq
{
public:
  // value in the range -1 < x < 1
  double x;

  // default constructor
  RRq() : x(0) {}

  inline RRq operator-()
  {
    RRq out;
    out.x = -x;
    return out;
  }

  // turn an hInt_t into a Zq with modular reduction.
  RRq& operator=(const double& c)
  {
    this->x = fractional_part(c);
    return *this;
  }
  RRq& operator+=(const RRq& b)
  {
    this->x += b.x;
    this->x = fractional_part(this->x);
    return *this;
  }
  RRq& operator-=(const RRq& b)
  {
    this->x -= b.x;
    this->x = fractional_part(this->x);
    return *this;
  }
};

// binary operators defined in terms of the unary operators
inline RRq operator+(RRq a, const RRq& b)
{
  a += b;
  return a;
}
inline RRq operator-(RRq a, const RRq& b)
{
  a -= b;
  return a;
}

// Converts an RRq value into the range 0 <= x < 1. This should be called
// before returning to Haskell
void canonicalizeRRq (RRq* y, hDim_t totm);

class Complex
{
public:
  double real;
  double imag;

  // default constructor
  Complex() : real(0.0), imag(0.0) {}

  Complex(const double& a, const double& b) {
      this->real = a;
      this->imag = b;
  }

  inline Complex operator-()
  {
    Complex out;
    out.real = -real;
    out.imag = -imag;
    return out;
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
  Complex& operator*=(const hInt_t& b)
  {
    Complex c;
    c = b;
    *this *= c;
    return *this;
  }
  Complex& operator/=(const Complex& b)
  {
    Complex bconj;
    bconj.real = b.real;
    bconj.imag = -b.imag;
    *this *= bconj;
    double den = (b.real*b.real+b.imag*b.imag);
    this->real /= den;
    this->imag /= den;
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
inline Complex operator*(Complex a, const hInt_t& b)
{
  a *= b;
  return a;
}
inline Complex operator/(Complex a, const Complex& b)
{
  a /= b;
  return a;
}

#endif /* __cplusplus */
#endif /* TENSORTYPES_H_ */
