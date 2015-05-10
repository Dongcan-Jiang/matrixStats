/***********************************************************************
 TEMPLATE:
  void rowCounts_<Integer|Real|Logical>[ROWS_TYPE][COLS_TYPE](X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, X_C_TYPE value, int narm, int hasna, int *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols)

 GENERATES:
  void rowCounts_Real[ROWS_TYPE][COLS_TYPE](double *x, R_xlen_t nrow, R_xlen_t ncol, double value, int narm, int hasna, int *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols)
  void rowCounts_Integer[ROWS_TYPE][COLS_TYPE](int *x, R_xlen_t nrow, R_xlen_t ncol, int value, int narm, int hasna, int *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols)
  void rowCounts_Logical[ROWS_TYPE][COLS_TYPE](int *x, R_xlen_t nrow, R_xlen_t ncol, int value, int narm, int hasna, int *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols)

 Arguments:
   The following macros ("arguments") should be defined for the 
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i', 'r', or 'l'

 Copyright: Henrik Bengtsson, 2014
 ***********************************************************************/ 
#include "types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, [METHOD_NAME])
 */
#include "templates-types.h" 


RETURN_TYPE METHOD_NAME_ROWS_COLS(ARGUMENTS_LIST) {
  R_xlen_t ii, jj;
  R_xlen_t colBegin, idx;
  int count;
  X_C_TYPE xvalue;

#ifdef ROWS_TYPE
  ROWS_C_TYPE *crows = (ROWS_C_TYPE*) rows;
#endif
#ifdef COLS_TYPE
  COLS_C_TYPE *ccols = (COLS_C_TYPE*) cols;
#endif

  if (what == 0) {  /* all */
    for (ii=0; ii < NUM_OF_ROWS; ii++) ans[ii] = 1;

    /* Count missing values? [sic!] */
    if (X_ISNAN(value)) {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          /* Skip? */
          if (ans[ii]) {
            idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
            xvalue = R_GET(x, idx, X_NA);
            if (!X_ISNAN(xvalue)) {
              ans[ii] = 0;
              /* Found another value! Skip from now on */
            }
          }
        }
      }
    } else {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          /* Skip? */
          if (ans[ii]) {
            idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
            xvalue = R_GET(x, idx, X_NA);
            if (xvalue == value) {
            } else if (narm && X_ISNAN(xvalue)) {
              /* Skip */
            } else if (!narm && X_ISNAN(xvalue)) {
              /* Early stopping is not possible, because if we do
               find an element that is not 'value' later, then
               we know for sure that all = FALSE regardless of
               missing values. In other words, at this point
               the answer can be either NA or FALSE.*/
              ans[ii] = NA_INTEGER;
            } else {
              /* Found another value! Skip from now on */
              ans[ii] = 0;
            }
          }
        } /* for (ii ...) */
      } /* for (jj ...) */
    }
  } else if (what == 1) {  /* any */
    for (ii=0; ii < NUM_OF_ROWS; ii++) ans[ii] = 0;

    /* Count missing values? [sic!] */
    if (X_ISNAN(value)) {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          /* Skip? */
          if (!ans[ii]) {
            idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
            xvalue = R_GET(x, idx, X_NA);
            if (X_ISNAN(xvalue)) {
              ans[ii] = 1;
              /* Found value! Skip from now on */
            }
          }
        }
      }
    } else {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          /* Skip? */
          if (!ans[ii]) {
            idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
            xvalue = R_GET(x, idx, X_NA);
            if (xvalue == value) {
              /* Found value! Skip from now on */
              ans[ii] = 1;
            } else if (narm && X_ISNAN(xvalue)) {
              /* Skip */
            } else if (!narm && X_ISNAN(xvalue)) {
              /* Early stopping is not possible, because if we do
               find an element that is 'value' later, then
               we know for sure that any = TRUE regardless of
               missing values. In other words, at this point
               the answer can be either NA or TRUE.*/
              ans[ii] = NA_INTEGER;
            }
          }
        } /* for (ii ...) */
      } /* for (jj ...) */
    }
  } else if (what == 2) {  /* count */
    for (ii=0; ii < NUM_OF_ROWS; ii++) ans[ii] = 0;

    /* Count missing values? [sic!] */
    if (X_ISNAN(value)) {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
          xvalue = R_GET(x, idx, X_NA);
          if (X_ISNAN(xvalue)) ans[ii] = ans[ii] + 1;
        }
      }
    } else {
      for (jj=0; jj < NUM_OF_COLS; jj++) {
        colBegin = R_INDEX_OP(COL_INDEX(ccols,jj), *, nrow);
        for (ii=0; ii < NUM_OF_ROWS; ii++) {
          count = ans[ii];
          /* Nothing more to do on this row? */
          if (count == NA_INTEGER) continue;
  
          idx = R_INDEX_OP(colBegin, +, ROW_INDEX(crows,ii));
          xvalue = R_GET(x, idx, X_NA);
          if (xvalue == value) {
            ans[ii] = count + 1;
          } else {
            if (!narm && X_ISNAN(xvalue)) {
              ans[ii] = NA_INTEGER;
              continue;
            }
          }
        } /* for (ii ...) */
      } /* for (jj ...) */
    }
  } else {
    error("INTERNAL ERROR: Unknown value of 'what' for colCounts: %d", what);
  } /* if (what ...) */
}


/***************************************************************************
 HISTORY:
 2015-04-13 [DJ]
  o Supported subsetted computation.
 2014-11-06 [HB]
  o CLEANUP: Moving away from R data types in low-level C functions.
 2014-11-01 [HB]
  o SPEEDUP: Now using ansp = INTEGER(ans) once and then querying/assigning
    'ansp[i]' instead of INTEGER(ans)[i].
 2014-06-02 [HB]
  o Created.
 **************************************************************************/
