#include "tensorTypes.h"

hDim_t ipow(hDim_t base, hShort_t exp)
{
#ifdef DEBUG_MODE
    ASSERT(exp >= 0);
#endif
	hDim_t result = 1;
    while (exp)
    {
        if (exp & 1)
        {
            result *= base;
        }
        exp >>= 1;
        base *= base;
    }
    return result;
}

complex_t cmplxpow(complex_t base, hShort_t exp)
{
	complex_t result = (complex_t){1,0};
    while (exp)
    {
        if (exp & 1)
        {
            CMPLX_IMUL(result,base);
        }
        exp >>= 1;
        CMPLX_IMUL(base,base);
    }
    return result;
}

hInt_t qpow(hInt_t base, hShort_t exp, hInt_t q)
{
	hInt_t result = 1;
    while (exp)
    {
        if (exp & 1)
        {
            result = (result*base)%q;
        }
        exp >>= 1;
        base = (base*base)%q;
    }
    return result;
}

// a is the field size. we are looking for reciprocal of b
hInt_t reciprocal (hInt_t a, hInt_t b)
{
	hInt_t fieldSize = a;

	hInt_t y = 1;
	hInt_t lasty = 0;
	while (b != 0)
	{
		hInt_t quotient = a / b;
		hInt_t tmp = a % b;
		a = b;
		b = tmp;
		tmp = y;
		y  = lasty - quotient*y;
		lasty = tmp;
	}
	ASSERT (a==1);  // if this one fails, then b is not invertible mod a

	// this actually returns EITHER the reciprocal OR reciprocal + fieldSize
	hInt_t res = lasty + fieldSize;
#ifdef DEBUG_MODE
	ASSERT (0);
	ASSERT ((res >= 0) && (res < fieldSize + fieldSize));
	hInt_t test = res * b % fieldSize;
	ASSERT (test == 1);
#endif
	return res;

}

//for square transforms
void tensorFuser (void* y, hShort_t tupSize, funcPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, hInt_t* qs)
{
    hDim_t lts = totm;
    hDim_t rts = 1;
    hShort_t i;

    for (i = 0; i < sizeOfPE; ++i)
    {
        PrimeExponent pe = peArr[i];
        hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
        hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
        lts /= dim;
        (*f) (y, tupSize, pe, lts, rts, qs);
        rts  *= dim;
    }
}

void tensorFuserCRT (void* y, hShort_t tupSize, crtFuncPtr f, hDim_t totm, PrimeExponent* peArr, hShort_t sizeOfPE, void** ru, hInt_t* q)
{
    hDim_t lts = totm;
    hDim_t rts = 1;
    hShort_t i;

    for (i = 0; i < sizeOfPE; ++i)
    {
        PrimeExponent pe = peArr[i];
        hDim_t ipow_pe = ipow(pe.prime, (pe.exponent-1));
        hDim_t dim = (pe.prime-1) * ipow_pe;  // the totient of pe
        lts /= dim;
        (*f) (y, tupSize, lts, rts, pe, ru[i], q);
        rts  *= dim;
    }
}

struct  timespec  tsSubtract (struct  timespec  time1, struct  timespec  time2)
{    /* Local variables. */
    struct  timespec  result ;

/* Subtract the second time from the first. */

    if ((time1.tv_sec < time2.tv_sec) ||
        ((time1.tv_sec == time2.tv_sec) &&
         (time1.tv_nsec <= time2.tv_nsec))) {		/* TIME1 <= TIME2? */
        result.tv_sec = result.tv_nsec = 0 ;
    } else {						/* TIME1 > TIME2 */
        result.tv_sec = time1.tv_sec - time2.tv_sec ;
        if (time1.tv_nsec < time2.tv_nsec) {
            result.tv_nsec = time1.tv_nsec + 1000000000L - time2.tv_nsec ;
            result.tv_sec-- ;				/* Borrow a second. */
        } else {
            result.tv_nsec = time1.tv_nsec - time2.tv_nsec ;
        }
    }

    return (result) ;
}

struct  timespec  tsAdd (struct  timespec  time1, struct  timespec  time2)
{    /* Local variables. */
    struct  timespec  result ;

/* Add the two times together. */

    result.tv_sec = time1.tv_sec + time2.tv_sec ;
    result.tv_nsec = time1.tv_nsec + time2.tv_nsec ;
    if (result.tv_nsec >= 1000000000L) {		/* Carry? */
        result.tv_sec++ ;  result.tv_nsec = result.tv_nsec - 1000000000L ;
    }

    return (result) ;
}

const  char  *tsShow (struct  timespec  binaryTime, bool  inLocal, const  char  *format)
{    /* Local variables. */
    struct  tm  calendarTime ;
#define  MAX_TIMES  4
    static  char  asciiTime[MAX_TIMES][64] ;
    static  int  current = 0 ;

/* Convert the TIMESPEC to calendar time: year, month, day, etc. */

#ifdef VXWORKS
    if (inLocal)
        localtime_r ((time_t *) &binaryTime.tv_sec, &calendarTime) ;
    else
        gmtime_r ((time_t *) &binaryTime.tv_sec, &calendarTime) ;
#else
    if (inLocal)
        calendarTime = *(localtime ((time_t *) &binaryTime.tv_sec)) ;
    else
        calendarTime = *(gmtime ((time_t *) &binaryTime.tv_sec)) ;
#endif

/* Format the time in ASCII. */

    current = (current + 1) % MAX_TIMES ;

    if (format == NULL) {
        strftime (asciiTime[current], 64, "%Y-%j-%H:%M:%S", &calendarTime) ;
        sprintf (asciiTime[current] + strlen (asciiTime[current]),
                 ".%06ld", (binaryTime.tv_nsec % 1000000000L) / 1000L) ;
    } else {
        strftime (asciiTime[current], 64, format, &calendarTime) ;
        sprintf (asciiTime[current] + strlen (asciiTime[current]),
                 ".%06ld", (binaryTime.tv_nsec % 1000000000L) / 1000L) ;
    }

    return (asciiTime[current]);
}



const char* timeformat = "%M:%S";

void getStats() { 

#ifdef STATS
    struct timespec total;
    printf("CRT Stats:\n");
    printf("CRT_Rq times: Real:%s\tMono:%s\tProc:%s\tThread:%s\n", tsShow(crttime1, false, timeformat),tsShow(crttime2, false, timeformat),tsShow(crttime3, false, timeformat),tsShow(crttime4, false, timeformat));
    printf("CTR_Rq: %d\t%s\t%d\t%s\n", crtRqCtr, tsShow(crttime1, false, timeformat), crtInvRqCtr, tsShow(crtInvRqTime, false, timeformat));
    printf("CTR_C: %d\t%s\t%d\t%s\n", crtCCtr, tsShow(crtCTime, false, timeformat), crtInvCCtr, tsShow(crtInvCTime, false, timeformat));
    
    printf("\nG Stats:\n");
    printf("GPow_R: %d\t%s\t%d\t%s\n", gprCtr, tsShow(gprTime, false, timeformat), giprCtr, tsShow(giprTime, false, timeformat));
    printf("GPow_Rq: %d\t%s\t%d\t%s\n", gprqCtr, tsShow(gprqTime, false, timeformat), giprqCtr, tsShow(giprqTime, false, timeformat));
    printf("GPow_C: %d\t%s\t%d\t%s\n", gpcCtr, tsShow(gpcTime, false, timeformat), gipcCtr, tsShow(gipcTime, false, timeformat));
    printf("GDec_R: %d\t%s\t%d\t%s\n", gdrCtr, tsShow(gdrTime, false, timeformat), gidrCtr, tsShow(gidrTime, false, timeformat));
    printf("GDec_Rq: %d\t%s\t%d\t%s\n", gdrqCtr, tsShow(gdrqTime, false, timeformat), gidrqCtr, tsShow(gidrqTime, false, timeformat));
    printf("GCRT_Rq: %d\t%d\t%s\n", gcrqCtr, gicrqCtr, tsShow(gcrqTime, false, timeformat));
    printf("GCRT_C: %d\t%d\t%s\n", gccCtr, giccCtr, tsShow(gccTime, false, timeformat));

    printf("\nL Stats:\n");
    printf("L_R: %d\t%s\t%d\t%s\n", lrCtr, tsShow(lrTime, false, timeformat), lirCtr, tsShow(lirTime, false, timeformat));
    printf("L_Rq: %d\t%s\t%d\t%s\n", lrqCtr, tsShow(lrqTime, false, timeformat), lirqCtr, tsShow(lirqTime, false, timeformat));
    printf("L_D: %d\t%s\t%d\t%s\n", ldCtr, tsShow(ldTime, false, timeformat), lidCtr, tsShow(lidTime, false, timeformat));
    printf("L_C: %d\t%s\t%d\t%s\n", lcCtr, tsShow(lcTime, false, timeformat), licCtr, tsShow(licTime, false, timeformat));

    printf("\nNorm Stats:\n");
    printf("NormSq_R %d\t%s\n", normrCtr, tsShow(normrTime, false, timeformat));

    printf("\nBasic Stats:\n");
    printf("Mul: %d\t%s\n", mulCtr, tsShow(mulTime, false, timeformat));
    printf("Add: %d\t%s\n", addCtr, tsShow(addTime, false, timeformat));

    total = tsAdd(norrTime, tsAdd(crttime1, tsAdd(crtInvRqTime, tsAdd(crtCTime, tsAdd(crtInvCTime, tsAdd(gprTime, tsAdd(giprTime, tsAdd(gdrTime, tsAdd(gidrTime, tsAdd(gprqTime, tsAdd(giprqTime, tsAdd(gdrqTime, tsAdd(gidrqTime, tsAdd(gcrqTime, tsAdd(gccTime, tsAdd(lrTime, tsAdd(lirTime, tsAdd(lrqTime, tsAdd(lirqTime, tsAdd(ldTime, tsAdd(lidTime, tsAdd(lcTime, tsAdd(licTime, tsAdd(mulTime,addTime))))))))))))))))))))))));

    printf("\nTotal C Time: %s\n\n", tsShow(total, false, timeformat));

    crtRqCtr = 0;
    crtInvRqCtr = 0;
    crtCCtr = 0;
    crtInvCCtr = 0;

    gprCtr = 0;
    gpcCtr = 0;
    gprqCtr = 0;
    gdrCtr = 0;
    gdrqCtr = 0;
    giprCtr = 0;
    gipcCtr = 0;
    giprqCtr = 0;
    gidrCtr = 0;
    gidrqCtr = 0;
    gcrqCtr = 0;
    gccCtr = 0;
    gicrqCtr = 0;
    giccCtr = 0;

    lrqCtr = 0;
    lrCtr = 0;
    ldCtr = 0;
    lcCtr = 0;
    lirqCtr = 0;
    lirCtr = 0;
    lidCtr = 0;
    licCtr = 0;

    normrCtr = 0;

    mulCtr = 0;
    addCtr = 0;

    mulTime = (struct timespec){0,0};
    addTime = (struct timespec){0,0};

    lrqTime = (struct timespec){0,0};
    lrTime = (struct timespec){0,0};
    ldTime = (struct timespec){0,0};
    lcTime = (struct timespec){0,0};
    lirqTime = (struct timespec){0,0};
    lirTime = (struct timespec){0,0};
    lidTime = (struct timespec){0,0};
    licTime = (struct timespec){0,0};

    normrTime = (struct timespec){0,0};

    gprTime = (struct timespec){0,0};
    gpcTime = (struct timespec){0,0};
    gprqTime = (struct timespec){0,0};
    gdrTime = (struct timespec){0,0};
    gdrqTime = (struct timespec){0,0};
    giprTime = (struct timespec){0,0};
    gipcTime = (struct timespec){0,0};
    giprqTime = (struct timespec){0,0};
    gidrTime = (struct timespec){0,0};
    gidrqTime = (struct timespec){0,0};
    gcrqTime = (struct timespec){0,0};
    gccTime = (struct timespec){0,0};

    crttime1 = (struct timespec){0,0};
    crttime2 = (struct timespec){0,0};
    crttime3 = (struct timespec){0,0};
    crttime4 = (struct timespec){0,0};

    crtInvRqTime = (struct timespec){0,0};
    crtCTime = (struct timespec){0,0};
    crtInvCTime = (struct timespec){0,0};
#endif
    fflush(stdout);
}


