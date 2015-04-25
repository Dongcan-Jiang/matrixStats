/***************************************************************************
 Public methods:
 SEXP validate(SEXP idxs, SEXP maxIdx)

 **************************************************************************/
#include <string.h>
#include "utils.h"

#define METHOD validateIndices2

#define X_TYPE 'i'
#include "validate_TYPE-template.h"

#define X_TYPE 'r'
#include "validate_TYPE-template.h"


void *validateIndices2(SEXP idxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  if (isNull(idxs)) {
    *ansNidxs = maxIdx;
    return NULL;
  }
  R_xlen_t nidxs = xlength(idxs);
  int mode = TYPEOF(idxs);
  switch (mode) {
    case INTSXP:
      return validateIndices2_Integer(INTEGER(idxs), nidxs, maxIdx, ansNidxs);
    case REALSXP:
      return validateIndices2_Real(REAL(idxs), nidxs, maxIdx, ansNidxs);
    case LGLSXP:
      return validateIndices2_Logical(LOGICAL(idxs), nidxs, maxIdx, ansNidxs);
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
      int *cidxs = validateIndices2_Integer(INTEGER(idxs), nidxs, cmaxIdx, &ansNidxs);
//      if (cidxs == NULL && ansNidxs) return R_NilValue;
      ans = PROTECT(allocVector(INTSXP, ansNidxs));
      memcpy(INTEGER(ans), cidxs, ansNidxs*sizeof(int));
      UNPROTECT(1);
      return ans;
    }
    case REALSXP: {
      double *cidxs = validateIndices2_Real(REAL(idxs), nidxs, cmaxIdx, &ansNidxs);
//      if (cidxs == NULL && ansNidxs) return R_NilValue;
      ans = PROTECT(allocVector(REALSXP, ansNidxs));
      memcpy(REAL(ans), cidxs, ansNidxs*sizeof(double));
      UNPROTECT(1);
      return ans;
    }
    case LGLSXP: {
      R_xlen_t *cidxs = validateIndices2_Logical(LOGICAL(idxs), nidxs, cmaxIdx, &ansNidxs);
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
