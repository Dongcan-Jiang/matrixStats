#include <Rdefines.h>
#include "types.h"
#include "templates-types.h"


X_C_TYPE* METHOD_NAME(X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  /*
  if (idxs == NULL) {
    *ansNidxs = maxIdx;
    return NULL;
  }
  */

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


#ifndef _LOGICAL_VALIDATE_INDICES_
#define _LOGICAL_VALIDATE_INDICES_
R_xlen_t* validateIndices2_Logical(int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  if (nidxs == 1) {
    *ansNidxs = idxs[0] ? maxIdx : 0;
    return NULL;
  }

  R_xlen_t ii, jj;
  R_xlen_t count = 0;
  R_xlen_t n = nidxs;
  if (n > maxIdx) n = maxIdx;
  for (ii = 0; ii < n; ++ ii) {
    if (idxs[ii]) ++ count;
  }
  *ansNidxs = count;
  R_xlen_t *ans = (R_xlen_t*) R_alloc(count, sizeof(R_xlen_t));
  jj = 0;
  for (ii = 0; ii < n; ++ ii) {
    if (idxs[ii]) ans[jj ++] = ii + 1;
  }
  return ans;
}
#endif

#include "templates-types_undef.h"
