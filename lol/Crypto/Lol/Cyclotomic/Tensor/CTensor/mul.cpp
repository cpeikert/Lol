#include "tensorTypes.h"

#ifdef STATS
int mulCtr = 0;
struct timespec mulTime = {0,0};

int addCtr = 0;
struct timespec addTime = {0,0};
#endif

template <typename ring> void zipWithStar (ring* a, ring* b, hShort_t tupSize, hDim_t totm, hInt_t* qs)
{
  #ifdef STATS
  mulCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  for(int tupIdx = 0; tupIdx < tupSize; tupIdx++) {
    if(qs) {
      Zq::q = qs[tupIdx];
    }
    for(int i = 0; i < totm; i++) {
      a[i*tupSize+tupIdx] *= b[i*tupSize+tupIdx];
    }
  }
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  mulTime = tsAdd(mulTime, tsSubtract(t1,s1));
#endif
}

//a = zipWith (*) a b
extern "C" void mulRq (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm, hInt_t* qs) {
  zipWithStar((Zq*)a, (Zq*)b, tupSize, totm, qs);
}

extern "C" void mulC (hShort_t tupSize, complex_t* a, complex_t* b, hDim_t totm) {
  zipWithStar((Complex*)a, (Complex*)b, tupSize, totm, (hInt_t*)0);
}
