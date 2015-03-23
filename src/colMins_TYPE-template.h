/***********************************************************************
 TEMPLATE:
  void colMins_<Integer|Real>(X_C_TYPE *x, R_xlen_t M, R_xlen_t ROWS, int *cols, R_xlen_t COLS)

 GENERATES:
  void colMins_Real(double *x, R_xlen_t M, int *row, R_xlen_t ROWS, int *cols, R_xlen_t COLS)
  void colMins_Integer(int *x, R_xlen_t M, R_xlen_t ncol, int value, int narm, int hasna, int *ans)

 Arguments:
   The following macros ("arguments") should be defined for the 
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i', 'r'

 ***********************************************************************/ 
#include "types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, [METHOD_NAME])
 */
#include "templates-types.h" 


SEXP METHOD_NAME(X_C_TYPE *x, R_xlen_t M, int *rows, R_xlen_t ROWS, int *cols, R_xlen_t COLS) {
  SEXP ans = PROTECT(allocVector(X_RC_XP, COLS));
  R_xlen_t i, j, k;

  for (i = 0; i < COLS; ++ i) {
    k = M * (cols[i] - 1);
    X_C_TYPE minValue = x[k+rows[0]-1];
    for (j = 1; j < ROWS; ++ j) {
      X_C_TYPE value = x[k+rows[j]-1];
      if (value < minValue) minValue = value;
    }
    X_IN_C(ans)[i] = minValue;
  }
  UNPROTECT(1);
  return ans;
}

/* Undo template macros */
#include "templates-types_undef.h" 


/***************************************************************************
 HISTORY:
 2015-03-23
  o Created.
 **************************************************************************/
