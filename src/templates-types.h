#include <Rinternals.h>
#include "macros.h"


#undef X_C_TYPE
#undef X_IN_C
#undef X_ISNAN
#undef X_ISNA
#undef X_ABS
#undef X_PSORT
#undef X_QSORT_I
#undef X_NA

#undef Y_C_TYPE
#undef Y_IN_C
#undef Y_ISNAN
#undef Y_ISNA
#undef Y_ABS
#undef Y_PSORT
#undef Y_QSORT_I

#undef ANS_SXP
#undef ANS_NA
#undef ANS_ISNAN
#undef ANS_ISNA
#undef ANS_C_TYPE
#undef ANS_IN_C


/*
 Data type macros for argument 'x'
 */
#if X_TYPE == 'i'
  #define X_C_TYPE int
  #define X_IN_C INTEGER
  #define X_ISNAN(x) (x == NA_INTEGER)
  #define X_ISNA(x) (x == NA_INTEGER)
  #define X_ABS(x) abs(x)
  #define X_PSORT iPsort
  #define X_QSORT_I R_qsort_int_I
  #define X_NA NA_INTEGER
#elif X_TYPE == 'r'
  #define X_C_TYPE double
  #define X_IN_C REAL
  #define X_ISNAN(x) ISNAN(x) /* NA or NaN */
  #define X_ISNA(x) ISNA(x) /* NA only */
  #define X_ABS(x) fabs(x)
  #define X_PSORT rPsort
  #define X_QSORT_I R_qsort_I
  #define X_NA NA_REAL
#elif X_TYPE == 'l'
  #define X_C_TYPE int
  #define X_IN_C LOGICAL
  #define X_ISNAN(x) (x == NA_LOGICAL)
  #define X_NA NA_LOGICAL
#else
  #error "INTERNAL ERROR: Failed to set C macro X_C_TYPE etc.: Unknown X_TYPE"
#endif


/*
 Data type macros for argument 'y'
 */
#ifdef Y_TYPE
  #if Y_TYPE == 'i'
    #define Y_C_TYPE int
    #define Y_IN_C INTEGER
    #define Y_ISNAN(x) (x == NA_INTEGER)
    #define Y_ISNA(x) (x == NA_INTEGER)
    #define Y_ABS(x) abs(x)
    #define Y_PSORT iPsort
    #define Y_QSORT_I R_qsort_int_I
  #elif Y_TYPE == 'r'
    #define Y_C_TYPE double
    #define Y_IN_C REAL
    #define Y_ISNAN(x) ISNAN(x) /* NA or NaN */
    #define Y_ISNA(x) ISNA(x) /* NA only */
    #define Y_ABS(x) fabs(x)
    #define Y_PSORT rPsort
    #define Y_QSORT_I R_qsort_I
  #elif Y_TYPE == 'l'
    #define Y_C_TYPE int
    #define Y_IN_C LOGICAL
    #define Y_ISNAN(x) (x == NA_LOGICAL)
  #else
    #error "INTERNAL ERROR: Failed to set C macro Y_C_TYPE etc.: Unknown Y_TYPE"
  #endif
//#else
//  #define Y_TYPE '.'
#endif



/*
 Data type macros for result ('ans')
 */
#ifndef ANS_TYPE
  /* Default to same as 'x' */
  #define ANS_TYPE X_TYPE
#endif


#if ANS_TYPE == 'i'
  #define ANS_SXP INTSXP
  #define ANS_NA NA_INTEGER
  #define ANS_ISNAN(x) (x == NA_INTEGER)
  #define ANS_ISNA(x) (x == NA_INTEGER)
  #define ANS_C_TYPE int
  #define ANS_IN_C INTEGER
#elif ANS_TYPE == 'r'
  #define ANS_SXP REALSXP
  #define ANS_NA NA_REAL
  #define ANS_ISNAN(x) ISNAN(x) /* NA or NaN */
  #define ANS_ISNA(x) ISNA(x) /* NA only */
  #define ANS_C_TYPE double
  #define ANS_IN_C REAL
#elif ANS_TYPE == 'l'
  #define ANS_SXP LGLSXP
  #define ANS_NA NA_LOGICAL
  #define ANS_ISNAN(x) (x == NA_LOGICAL)
  #define ANS_C_TYPE int
  #define ANS_IN_C LOGICAL
#else
  #error "INTERNAL ERROR: Failed to set C macro ANS_C_TYPE: Unknown ANS_TYPE"
#endif


/*
 Method name based on 'x' (and 'y') types
 */
#ifndef METHOD_NAME
  #if X_TYPE == 'i'
    #if Y_TYPE == 'i'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Integer_Integer)
    #elif Y_TYPE == 'r'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Integer_Real)
    #elif Y_TYPE == 'l'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Integer_Logical)
    #else
      #define METHOD_NAME CONCAT_MACROS(METHOD, Integer)
    #endif
  #elif X_TYPE == 'r'
    #if Y_TYPE == 'i'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Real_Integer)
    #elif Y_TYPE == 'r'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Real_Real)
    #elif Y_TYPE == 'l'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Real_Logical)
    #else
      #define METHOD_NAME CONCAT_MACROS(METHOD, Real)
    #endif
  #elif X_TYPE == 'l'
    #if Y_TYPE == 'i'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Logical_Integer)
    #elif Y_TYPE == 'r'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Logical_Real)
    #elif Y_TYPE == 'l'
      #define METHOD_NAME CONCAT_MACROS(METHOD, Logical_Logical)
    #else
      #define METHOD_NAME CONCAT_MACROS(METHOD, Logical)
    #endif
  #else
    #error "INTERNAL ERROR: Failed to set C macro METHOD_NAME: Unknown X_TYPE"
  #endif
#endif


#undef NUM_OF_ROWS
#undef ROW_INDEX
#undef ROWS_C_TYPE
#undef METHOD_NAME_ROWS

#undef NUM_OF_COLS
#undef COL_INDEX
#undef COLS_C_TYPE
#undef METHOD_NAME_ROWS_COLS

#ifdef ROWS_TYPE
  #define NUM_OF_ROWS nrows
//  #define ROW_INDEX(rows, ii) ((R_xlen_t)rows[ii]-1)
  #if ROWS_TYPE == 'i'
    #define ROWS_C_TYPE int
    #define ROW_INDEX(rows, ii) (rows[ii] == NA_INTEGER ? NA_R_XLEN_T : (R_xlen_t)rows[ii]-1)
    #define METHOD_NAME_ROWS CONCAT_MACROS(METHOD_NAME, intRows)
  #elif ROWS_TYPE == 'r'
    #define ROWS_C_TYPE double
    #define ROW_INDEX(rows, ii) (ISNAN(rows[ii]) ? NA_R_XLEN_T : (R_xlen_t)rows[ii]-1)
    #define METHOD_NAME_ROWS CONCAT_MACROS(METHOD_NAME, realRows)
  #elif ROWS_TYPE == 'n'
    #define ROWS_C_TYPE int
    #define ROW_INDEX(rows, ii) NA_R_XLEN_T
    #define METHOD_NAME_ROWS CONCAT_MACROS(METHOD_NAME, naRows)
  #else
    #error "INTERNAL ERROR: Failed to set C macro METHOD_NAME: Unknown ROWS_TYPE"
  #endif
#else
  #define NUM_OF_ROWS nrow
  #define ROW_INDEX(rows, ii) ii
  #define ROWS_C_TYPE void
  #define METHOD_NAME_ROWS CONCAT_MACROS(METHOD_NAME, noRows)
#endif

#ifdef COLS_TYPE
  #define NUM_OF_COLS ncols
//  #define COL_INDEX(cols, jj) ((R_xlen_t)cols[jj]-1)
  #if COLS_TYPE == 'i'
    #define COLS_C_TYPE int
    #define COL_INDEX(cols, jj) (cols[jj] == NA_INTEGER ? NA_R_XLEN_T : (R_xlen_t)cols[jj]-1)
    #define METHOD_NAME_ROWS_COLS CONCAT_MACROS(METHOD_NAME_ROWS, intCols)
  #elif COLS_TYPE == 'r'
    #define COLS_C_TYPE double
    #define COL_INDEX(cols, jj) (ISNAN(cols[jj]) ? NA_R_XLEN_T : (R_xlen_t)cols[jj]-1)
    #define METHOD_NAME_ROWS_COLS CONCAT_MACROS(METHOD_NAME_ROWS, realCols)
  #elif COLS_TYPE == 'n'
    #define COLS_C_TYPE int
    #define COL_INDEX(cols, jj) NA_R_XLEN_T
    #define METHOD_NAME_ROWS_COLS CONCAT_MACROS(METHOD_NAME_ROWS, naCols)
  #else
    #error "INTERNAL ERROR: Failed to set C macro METHOD_NAME: Unknown ROWS_TYPE"
  #endif
#else
  #define NUM_OF_COLS ncol
  #define COL_INDEX(cols, jj) jj
  #define COLS_C_TYPE void
  #define METHOD_NAME_ROWS_COLS CONCAT_MACROS(METHOD_NAME_ROWS, noCols)
#endif

#undef METHOD_NAME_noRows
#undef METHOD_NAME_noRows_noCols
#undef METHOD_NAME_noRows_intCols
#undef METHOD_NAME_noRows_realCols
#undef METHOD_NAME_noRows_naCols
#undef METHOD_NAME_intRows
#undef METHOD_NAME_intRows_noCols
#undef METHOD_NAME_intRows_intCols
#undef METHOD_NAME_intRows_realCols
#undef METHOD_NAME_intRows_naCols
#undef METHOD_NAME_realRows
#undef METHOD_NAME_realRows_noCols
#undef METHOD_NAME_realRows_intCols
#undef METHOD_NAME_realRows_realCols
#undef METHOD_NAME_realRows_naCols
#undef METHOD_NAME_naRows
#undef METHOD_NAME_naRows_noCols
#undef METHOD_NAME_naRows_intCols
#undef METHOD_NAME_naRows_realCols
#undef METHOD_NAME_naRows_naCols

#define METHOD_NAME_noRows CONCAT_MACROS(METHOD_NAME, noRows)
#define METHOD_NAME_noRows_noCols CONCAT_MACROS(METHOD_NAME_noRows, noCols)
#define METHOD_NAME_noRows_intCols CONCAT_MACROS(METHOD_NAME_noRows, intCols)
#define METHOD_NAME_noRows_realCols CONCAT_MACROS(METHOD_NAME_noRows, realCols)
#define METHOD_NAME_noRows_naCols CONCAT_MACROS(METHOD_NAME_noRows, naCols)
#define METHOD_NAME_intRows CONCAT_MACROS(METHOD_NAME, intRows)
#define METHOD_NAME_intRows_noCols CONCAT_MACROS(METHOD_NAME_intRows, noCols)
#define METHOD_NAME_intRows_intCols CONCAT_MACROS(METHOD_NAME_intRows, intCols)
#define METHOD_NAME_intRows_realCols CONCAT_MACROS(METHOD_NAME_intRows, realCols)
#define METHOD_NAME_intRows_naCols CONCAT_MACROS(METHOD_NAME_intRows, naCols)
#define METHOD_NAME_realRows CONCAT_MACROS(METHOD_NAME, realRows)
#define METHOD_NAME_realRows_noCols CONCAT_MACROS(METHOD_NAME_realRows, noCols)
#define METHOD_NAME_realRows_intCols CONCAT_MACROS(METHOD_NAME_realRows, intCols)
#define METHOD_NAME_realRows_realCols CONCAT_MACROS(METHOD_NAME_realRows, realCols)
#define METHOD_NAME_realRows_naCols CONCAT_MACROS(METHOD_NAME_realRows, naCols)
#define METHOD_NAME_naRows CONCAT_MACROS(METHOD_NAME, naRows)
#define METHOD_NAME_naRows_noCols CONCAT_MACROS(METHOD_NAME_naRows, noCols)
#define METHOD_NAME_naRows_intCols CONCAT_MACROS(METHOD_NAME_naRows, intCols)
#define METHOD_NAME_naRows_realCols CONCAT_MACROS(METHOD_NAME_naRows, realCols)
#define METHOD_NAME_naRows_naCols CONCAT_MACROS(METHOD_NAME_naRows, naCols)
