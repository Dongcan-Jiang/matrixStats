/***********************************************************************
 TEMPLATE:
  void validateIndices_<Integer|Real|Logical>[ROWS_TYPE][COLS_TYPE](X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)

 GENERATES:
  void validateIndices_Real[ROWS_TYPE][COLS_TYPE](double *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)
  void validateIndices_Integer[ROWS_TYPE][COLS_TYPE](int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)
  void validateIndices_Logical[ROWS_TYPE][COLS_TYPE](int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs)

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


/** idxs must not be NULL, which should be checked before calling this function. **/
X_C_TYPE* METHOD_NAME(X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  R_xlen_t ii, jj;
  int state = 0;
  R_xlen_t count = 0;
  for (ii = 0; ii < nidxs; ++ ii) {
    if (idxs[ii] > 0 || X_ISNAN(idxs[ii])) {
      if (!X_ISNAN(idxs[ii])) {
        if (state < 0) error("only 0's may be mixed with negative subscripts");
        if (idxs[ii] > maxIdx) error("subscript out of bounds");
      }
      state = 1;
      ++ count;

    } else if (idxs[ii] < 0) {
      if (state > 0) error("only 0's may be mixed with negative subscripts");
      state = -1;
    }
  }

  if (state >= 0) *ansNidxs = count;
  if (count == nidxs) return idxs; // must have: state >= 0
  if (state == 0) return (X_C_TYPE*) R_alloc(0, sizeof(X_C_TYPE)); // count == 0

  if (state > 0) {
    X_C_TYPE *ans = (X_C_TYPE*) R_alloc(count, sizeof(X_C_TYPE));
    jj = 0;
    for (ii = 0; ii < nidxs; ++ ii) {
      // idxs[ii] can be positive or 0
      if (idxs[ii]) ans[jj ++] = idxs[ii];
    }
    return ans;
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

  X_C_TYPE *ans = (X_C_TYPE*) R_alloc(count, sizeof(int));
  jj = 0;
  for (ii = 0; ii < maxIdx; ++ ii) {
    if (!filter[ii]) ans[jj ++] = ii + 1;
  }

  return ans;
}


#include "templates-types_undef.h"
