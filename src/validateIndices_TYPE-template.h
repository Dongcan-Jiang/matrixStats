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


/** idxs must not be NULL, which should be checked before calling this function. **/
void* METHOD_NAME(X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs, int *subsettedType) {
  // Single NA
  if (nidxs == 1 && X_ISNAN(idxs[0])) {
    *ansNidxs = maxIdx;
    *subsettedType = SUBSETTED_NA;
    return NULL;
  }

  // For a un-full positive legal idxs array, we should use SUBSETTED_INTEGER as default.
  *subsettedType = SUBSETTED_INTEGER;

  R_xlen_t ii, jj;
  int state = 0;
  R_xlen_t count = 0;

  // figure out whether idxs are all positive or all negative.
  for (ii = 0; ii < nidxs; ++ ii) {
    X_C_TYPE idx = idxs[ii];
    if (idx > 0 || X_ISNAN(idx)) {
      if (!X_ISNAN(idx)) {
        if (state < 0) error("only 0's may be mixed with negative subscripts");
        if (idx > maxIdx) error("subscript out of bounds");
#if X_TYPE == 'r'
        if (idx > R_XLEN_T_MAX) Rf_error("%d exceeds R_XLEN_T_MAX", idx);
        if (idx > R_INT_MAX) *subsettedType = SUBSETTED_REAL;
#endif
      }// else error("NA index is not supported"); // NOTE: currently
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

  // fill positive idxs into ans
  if (state >= 0) {
    if (*subsettedType == SUBSETTED_INTEGER) {
      // NOTE: braces is needed here, because of macro-defined function
#if X_TYPE == 'i'
      RETURN_VALIDATED_ANS(int, nidxs, idxs[ii], idxs[ii]);
#else // Y_TYPE == 'r'
      RETURN_VALIDATED_ANS(int, nidxs, idxs[ii], IntegerFromReal(idxs[ii]));
#endif
    }
    // *subsettedType == SUBSETTED_REAL
#if X_TYPE == 'i'
    RETURN_VALIDATED_ANS(double, nidxs, idxs[ii], RealFromInteger(idxs[ii]));
#else // Y_TYPE == 'r'
    RETURN_VALIDATED_ANS(double, nidxs, idxs[ii], idxs[ii]);
#endif
  }

  // state < 0
  // use filter as bitset to find out all required idxs
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

  // find the biggest number 'upperBound'
  R_xlen_t upperBound;
  for (upperBound = maxIdx-1; upperBound >= 0; -- upperBound) {
    if (!filter[upperBound]) break;
  }
  ++ upperBound;
  if (upperBound > R_INT_MAX) *subsettedType = SUBSETTED_REAL;

  // fill required idxs into ans
  if (*subsettedType == SUBSETTED_INTEGER) {
    // NOTE: braces is needed here, because of macro-defined function
    RETURN_VALIDATED_ANS(int, upperBound, !filter[ii], ii + 1);
  } 
  // *subsettedType == SUBSETTED_REAL
  RETURN_VALIDATED_ANS(double, upperBound, !filter[ii], ii + 1);
}


#include "templates-types_undef.h"
