#include <Rinternals.h> /* R_xlen_t, ... */

#ifndef R_INT_MIN
#define R_INT_MIN -INT_MAX
#endif

#ifndef R_INT_MAX
#define R_INT_MAX  INT_MAX
#endif

#ifndef BOOL
#define BOOL unsigned char
#endif

#ifndef SUBSETTED_MODE_INDEX
#define SUBSETTED_MODE_INDEX

#define SUBSETTED_ALL 0
#define SUBSETTED_INTEGER 1
#define SUBSETTED_REAL 2
#define SUBSETTED_NA 3

#endif

/* As in <R>/src/include/Defn.h */
#ifdef HAVE_LONG_DOUBLE
#define LDOUBLE long double
#else
#define LDOUBLE double
#endif

/* Backward compatibility with R (< 3.0.0)
   As in <R>/src/include/Rinternals.h */
#ifndef R_XLEN_T_MAX
  typedef int R_xlen_t;
  #define R_XLEN_T_MAX R_LEN_T_MAX
  #ifndef xlength
    #define xlength length
  #endif
#endif

#include <limits.h>
#ifdef LONG_VECTOR_SUPPORT
  #define NA_R_XLEN_T -4503599627370497
#else
  #define NA_R_XLEN_T INT_MIN
#endif

/* Macro to check for user interrupts every 2^20 iteration */
#define R_CHECK_USER_INTERRUPT(i) if (i % 1048576 == 0) R_CheckUserInterrupt()
