#include "tensorTypes.h"

#ifdef STATS
int mulCtr = 0;
struct timespec mulTime = {0,0};

int addCtr = 0;
struct timespec addTime = {0,0};
#endif

template <typename ring> void zipWithStar (ring* a, ring* b, hDim_t totm)
{
  #ifdef STATS
  mulCtr++;
  struct timespec s1,t1;
  clock_gettime(CLOCK_REALTIME, &s1);
#endif
  for(int i = 0; i < totm; i++) {
    a[i] *= b[i];
  }
#ifdef STATS
  clock_gettime(CLOCK_REALTIME, &t1);
  mulTime = tsAdd(mulTime, tsSubtract(t1,s1));
#endif
}

//a = zipWith (*) a b
extern "C" void mulRq (hShort_t tupSize, hInt_t* a, hInt_t* b, hDim_t totm, hInt_t* qs) {
  ZqProd* zqa = toZqProd(tupSize, totm, a, qs);
  ZqProd* zqb = toZqProd(tupSize, totm, b, qs);

  zipWithStar(zqa, zqb, totm);

  free(zqa);
  free(zqb);
}

extern "C" void mulC (hShort_t tupSize, Complex* a, Complex* b, hDim_t totm) {
  zipWithStar(a, b, totm);
}
