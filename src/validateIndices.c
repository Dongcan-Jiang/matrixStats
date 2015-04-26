/***************************************************************************
 Public methods:
 SEXP validate(SEXP idxs, SEXP maxIdx)

 **************************************************************************/
#include <string.h>
#include "utils.h"

#define METHOD validateIndices

#define X_TYPE 'i'
#define SUBSETTED_DEFAULT SUBSETTED_INTEGER
#include "validateIndices_TYPE-template.h"
#undef SUBSETTED_DEFAULT

#define X_TYPE 'r'
#define SUBSETTED_DEFAULT SUBSETTED_REAL
#include "validateIndices_TYPE-template.h"
#undef SUBSETTED_DEFAULT


/** idxs must not be NULL, which should be checked before calling this function. **/
void* validateIndices_Logical(int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs, int *subsettedType) {
  // Single TRUE: select all
  if (nidxs == 1 && idxs[0]) return NULL;

  R_xlen_t ii, jj;
  R_xlen_t count = 0;
  R_xlen_t n = nidxs;
  if (n > maxIdx) n = maxIdx; // n = min(nidxs, maxIdx)

  // count how many idx items
  for (ii = 0; ii < n; ++ ii) {
    if (idxs[ii]) {
      ++ count;
      if (ii + 1 > R_INT_MAX) *subsettedType = SUBSETTED_REAL;
    }
  }
  *ansNidxs = count;

  // fill TURE idxs into ans
  if (*subsettedType == SUBSETTED_INTEGER) {
    RETURN_VALIDATED_ANS(int, n, idxs[ii], ii + 1);
    /*
    int *ans = (int*) R_alloc(count, sizeof(int));
    jj = 0;
    for (ii = 0; ii < n; ++ ii) {
      if (idxs[ii]) ans[jj ++] = ii + 1;
    }
    return ans;
    */

  } else {
    RETURN_VALIDATED_ANS(double, n, idxs[ii], ii + 1);
    /*
    double *ans = (double*) R_alloc(count, sizeof(double));
    jj = 0;
    for (ii = 0; ii < n; ++ ii) {
      if (idxs[ii]) ans[jj ++] = ii + 1;
    }
    return ans;
    */
  }
}


/*************************************************************
  * The most important function which is widely called.
  * If `idxs` is NULL, NULL will be returned, which indicates selecting.
  * the whole to-be-computed vector(matrix).
  * `maxIdx` is the to-be-computed vector(matrix)'s length (rows/cols).
  * `ansNidxs` is used for returning the new idxs array's length.
  * `subsettedType` is used for returning the new idxs array's datatype.
  ************************************************************/
void *validateIndices(SEXP idxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs, int *subsettedType) {
  R_xlen_t nidxs = xlength(idxs);
  int mode = TYPEOF(idxs);
  switch (mode) {
    case INTSXP:
      return validateIndices_Integer(INTEGER(idxs), nidxs, maxIdx, ansNidxs, subsettedType);
    case REALSXP:
      return validateIndices_Real(REAL(idxs), nidxs, maxIdx, ansNidxs, subsettedType);
    case LGLSXP:
      return validateIndices_Logical(LOGICAL(idxs), nidxs, maxIdx, ansNidxs, subsettedType);
    case NILSXP:
      *subsettedType = SUBSETTED_ALL;
      *ansNidxs = maxIdx;
      return NULL;
    default:
      error("idxs can only be integer, numeric, or logical.");
  }
  return NULL; // useless sentence. won't be executed.
}


/*************************************************************
  * This function can be called by R.
  * If `idxs` is NULL, NULL will be returned, which indicates selecting.
  * the whole to-be-computed vector(matrix).
  * `maxIdx` is the to-be-computed vector(matrix)'s length (rows/cols).
  ************************************************************/
SEXP validate(SEXP idxs, SEXP maxIdx) {
  SEXP ans;
  R_xlen_t ansNidxs;
  int subsettedType;
  R_xlen_t cmaxIdx = asR_xlen_t(maxIdx, 0);
  R_xlen_t nidxs = xlength(idxs);
  void *cidxs;

  int mode = TYPEOF(idxs);
  switch (mode) {
    case INTSXP:
      cidxs = validateIndices_Integer(INTEGER(idxs), nidxs, cmaxIdx, &ansNidxs, &subsettedType);
      break;
    case REALSXP:
      cidxs = validateIndices_Real(REAL(idxs), nidxs, cmaxIdx, &ansNidxs, &subsettedType);
      break;
    case LGLSXP:
      cidxs = validateIndices_Logical(LOGICAL(idxs), nidxs, cmaxIdx, &ansNidxs, &subsettedType);
      break;
    case NILSXP:
      return R_NilValue;
    default:
      error("idxs can only be integer, numeric, or logical.");
  }

  if (subsettedType == SUBSETTED_INTEGER) {
    ans = PROTECT(allocVector(INTSXP, ansNidxs));
    memcpy(INTEGER(ans), cidxs, ansNidxs*sizeof(int));
    UNPROTECT(1);
    return ans;
  }
  // else: subsettedType == SUBSETTED_REAL
  ans = PROTECT(allocVector(REALSXP, ansNidxs));
  memcpy(REAL(ans), cidxs, ansNidxs*sizeof(double));
  UNPROTECT(1);
  return ans;
}
