/***********************************************************************
 TEMPLATE:
  void validateIndices_<Integer|Real>[ROWS_TYPE][COLS_TYPE](X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)

 GENERATES:
  void validateIndices_Real[ROWS_TYPE][COLS_TYPE](double *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)
  void validateIndices_Integer[ROWS_TYPE][COLS_TYPE](int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)

 Arguments:
   The following macros ("arguments") should be defined for the 
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i', 'r', or 'l'

 ***********************************************************************/ 
#include "types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, [METHOD_NAME])
 */
#include "templates-types.h"


#ifndef RETURN_VALIDATED_ANS
#define RETURN_VALIDATED_ANS(type, n, cond, item) \
type *ans = (type*) R_alloc(count, sizeof(type)); \
jj = 0;                                           \
for (ii = 0; ii < n; ++ ii) {                     \
  if (cond) ans[jj ++] = item;                    \
}                                                 \
return ans
#endif


/** idxs must not be NULL, which should be checked before calling this function. **/
void* METHOD_NAME(X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs, int *subsettedType) {
  *subsettedType = SUBSETTED_INTEGER;

  R_xlen_t ii, jj;
  int state = 0;
  R_xlen_t count = 0;

  for (ii = 0; ii < nidxs; ++ ii) {
    R_xlen_t idx = idxs[ii];
    if (idx > 0 || X_ISNAN(idxs[ii])) {
      if (!X_ISNAN(idx)) {
        if (state < 0) error("only 0's may be mixed with negative subscripts");
        if (idx > maxIdx) error("subscript out of bounds");
#if X_TYPE == 'r'
        if (idx > R_XLEN_T_MAX) Rf_error("%d exceeds R_XLEN_T_MAX", idx);
        if (idx > R_INT_MAX) *subsettedType = SUBSETTED_REAL;
#endif
      }
      state = 1;
      ++ count;

    } else if (idxs[ii] < 0) {
      if (state > 0) error("only 0's may be mixed with negative subscripts");
      state = -1;
    }
  }

  if (state >= 0) *ansNidxs = count;
  if (count == nidxs) { // must have: state >= 0
    *subsettedType = SUBSETTED_DEFAULT;
    return idxs;
  }

  if (state >= 0) {
    if (*subsettedType == SUBSETTED_INTEGER) {
      RETURN_VALIDATED_ANS(int, nidxs, idxs[ii], idxs[ii]);
      /*
      int *ans = (int*) R_alloc(count, sizeof(int));
      jj = 0;
      for (ii = 0; ii < nidxs; ++ ii) {
        // idxs[ii] can be positive or 0
        if (idxs[ii]) ans[jj ++] = idxs[ii];
      }
      return ans;
      */

    } else {
      RETURN_VALIDATED_ANS(double, nidxs, idxs[ii], idxs[ii]);
      /*
      double *ans = (double*) R_alloc(count, sizeof(double));
      jj = 0;
      for (ii = 0; ii < nidxs; ++ ii) {
        // idxs[ii] can be positive or 0
        if (idxs[ii]) ans[jj ++] = idxs[ii];
      }
      return ans;
      */
    }
  }

  // state < 0
  BOOL filter[maxIdx];
  count = maxIdx;
  memset(filter, 0, sizeof(filter));
  for (ii = 0; ii < nidxs; ++ ii) {
    R_xlen_t idx = -idxs[ii];
    if (idx > 0 && idx <= maxIdx) {
      if (filter[idx-1] == 0) {
        -- count;
        filter[idx-1] = 1;
      }
    }
  }

  *ansNidxs = count;
  if (count == 0) return NULL;

  R_xlen_t upperBound;
  for (upperBound = maxIdx-1; upperBound >= 0; -- upperBound) {
    if (!filter[upperBound]) break;
  }
  ++ upperBound;
  if (upperBound > R_INT_MAX) *subsettedType = SUBSETTED_REAL;

  if (*subsettedType == SUBSETTED_INTEGER) {
    RETURN_VALIDATED_ANS(int, upperBound, !filter[ii], ii + 1);
    /*
    int *ans = (int*) R_alloc(count, sizeof(int));
    jj = 0;
    for (ii = 0; ii < upperBound; ++ ii) {
      if (!filter[ii]) ans[jj ++] = ii + 1;
    }
    return ans;
    */

  } else {
    RETURN_VALIDATED_ANS(double, upperBound, !filter[ii], ii + 1);
    /*
    double *ans = (double*) R_alloc(count, sizeof(double));
    jj = 0;
    for (ii = 0; ii < upperBound; ++ ii) {
      if (!filter[ii]) ans[jj ++] = ii + 1;
    }
    return ans;
    */
  }
}


#include "templates-types_undef.h"
