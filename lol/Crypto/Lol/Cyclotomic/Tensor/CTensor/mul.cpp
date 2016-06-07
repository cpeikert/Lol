#include "tensorTypes.h"

template <typename ring> void zipWithStar (ring* a, ring* b, hShort_t tupSize, hDim_t totm, hInt_t* qs)
{
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    if(qs) {
      Zq::q = qs[tupIdx];
    }
    for(int i = 0; i < totm; i++) {
      a[i*tupSize+tupIdx] *= b[i*tupSize+tupIdx];
    }
  }
}

//a = zipWith (*) a b
extern "C" void mulRq (hShort_t tupSize, Zq* a, Zq* b, hDim_t totm, hInt_t* qs)
{
  zipWithStar(a, b, tupSize, totm, qs);
}

extern "C" void mulC (hShort_t tupSize, Complex* a, Complex* b, hDim_t totm)
{
  zipWithStar(a, b, tupSize, totm, (hInt_t*)0);
}
