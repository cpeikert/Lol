
#include "tensorTypes.h"

/*
Cabal doesn't seem to like C++ code in .h files, so as a workaround 
we define C++ code in .cc files, and then include .cc files.
Zq uses a global modulus (so that the signature for functions on
on different types aligns), but this can't be in an included .cc file,
or we get linking errors. As a result, q is defined in tensorTypes.h,
which only contains vanilla C code.
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