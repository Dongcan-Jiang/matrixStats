/***************************************************************************
 Public methods:
 SEXP validate(SEXP idxs, SEXP maxIdx)

 **************************************************************************/
#include <string.h>
#include "utils.h"

#define METHOD validateIndices

#define X_TYPE 'i'
#include "validateIndices_TYPE-template.h"

#define X_TYPE 'r'
#include "validateIndices_TYPE-template.h"


/** idxs must not be NULL, which should be checked before calling this function. **/
R_xlen_t* validateIndices_Logical(int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
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


void *validateIndices(SEXP idxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  if (isNull(idxs)) {
    *ansNidxs = maxIdx;
    return NULL;
  }
  R_xlen_t nidxs = xlength(idxs);
  int mode = TYPEOF(idxs);
  switch (mode) {
    case INTSXP:
      return validateIndices_Integer(INTEGER(idxs), nidxs, maxIdx, ansNidxs);
    case REALSXP:
      return validateIndices_Real(REAL(idxs), nidxs, maxIdx, ansNidxs);
    case LGLSXP:
      return validateIndices_Logical(LOGICAL(idxs), nidxs, maxIdx, ansNidxs);
    default:
      error("idxs can only be integer, numeric, or logical.");
  }
  return NULL;
}


SEXP validate(SEXP idxs, SEXP maxIdx) {
  if (isNull(idxs)) return R_NilValue;

  SEXP ans;
  R_xlen_t ansNidxs;
  R_xlen_t cmaxIdx = asInteger(maxIdx);
  R_xlen_t nidxs = xlength(idxs);

  int mode = TYPEOF(idxs);
  switch (mode) {
    case INTSXP: {
      int *cidxs = validateIndices_Integer(INTEGER(idxs), nidxs, cmaxIdx, &ansNidxs);
//      if (cidxs == NULL && ansNidxs) return R_NilValue;
      ans = PROTECT(allocVector(INTSXP, ansNidxs));
      memcpy(INTEGER(ans), cidxs, ansNidxs*sizeof(int));
      UNPROTECT(1);
      return ans;
    }
    case REALSXP: {
      double *cidxs = validateIndices_Real(REAL(idxs), nidxs, cmaxIdx, &ansNidxs);
//      if (cidxs == NULL && ansNidxs) return R_NilValue;
      ans = PROTECT(allocVector(REALSXP, ansNidxs));
      memcpy(REAL(ans), cidxs, ansNidxs*sizeof(double));
      UNPROTECT(1);
      return ans;
    }
    case LGLSXP: {
      R_xlen_t *cidxs = validateIndices_Logical(LOGICAL(idxs), nidxs, cmaxIdx, &ansNidxs);
//      if (cidxs == NULL && ansNidxs) return R_NilValue;
      ans = PROTECT(allocVector(REALSXP, ansNidxs));
      R_xlen_t ii;
      for (ii = 0; ii < ansNidxs; ++ ii) REAL(ans)[ii] = cidxs[ii];
      UNPROTECT(1);
      return ans;
    }
    case NILSXP: break;
    default:
      error("idxs can only be integer, numeric, or logical.");
  }
  return R_NilValue;
}
