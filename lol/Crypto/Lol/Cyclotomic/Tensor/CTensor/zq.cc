
#include "tensorTypes.h"

/*
Cabal doesn't seem to like C++ code in .h files, so as a workaround 
we define C++ code in .cc files, and then include .cc files.
Zq has a static modulus which is declared below, but cannot be defined
in this file, since we include this file directly. Thus the definition
of `q` must be a file that is *not* included by anything else. We define
`q` in generalfuncs.cc.
*/

//http://stackoverflow.com/a/4421719
class Zq
{
public:
  hInt_t x;

  static hInt_t q;

  Zq& operator=(const hInt_t& c)
  {
    this->x = c;
    return *this;
  }
  Zq& operator+=(const Zq& b)
  {
    this->x = (this->x+b.x) % q;
    return *this;
  }
  Zq& operator-=(const Zq& b)
  {
    this->x = (this->x-b.x) % q;
    return *this;
  }
   Zq& operator*=(const Zq& b)
  {
    this->x = (this->x*b.x) % q;
    return *this;
  }
};

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