/***********************************************************************
 TEMPLATE:
  void validateIndices_<Integer|Real>[ROWS_TYPE][COLS_TYPE](X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, int allowOutOfBound, R_xlen_t *ansNidxs, int *subsettedType)

 GENERATES:
  void validateIndices_Real[ROWS_TYPE][COLS_TYPE](double *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, int allowOutOfBound, R_xlen_t *ansNidxs, int *subsettedType)
  void validateIndices_Integer[ROWS_TYPE][COLS_TYPE](int *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, int allowOutOfBound, R_xlen_t *ansNidxs, int *subsettedType)

 Arguments:
   The following macros ("arguments") should be defined for the 
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i', 'r'

 ***********************************************************************/ 
#include <Rdefines.h>
#include "types.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, [METHOD_NAME])
 */
#include "templates-types.h"

#undef IntegerFromIndex_TYPE
#undef RealFromIndex_TYPE
#if X_TYPE == 'i'
#define IntegerFromIndex_TYPE CONCAT_MACROS(IntegerFromIndex, Integer)
#define RealFromIndex_TYPE CONCAT_MACROS(RealFromIndex, Integer)
#elif X_TYPE == 'r'
#define IntegerFromIndex_TYPE CONCAT_MACROS(IntegerFromIndex, Real)
#define RealFromIndex_TYPE CONCAT_MACROS(RealFromIndex, Real)
#endif

static R_INLINE int IntegerFromIndex_TYPE(X_C_TYPE x, R_xlen_t maxIdx) {
  if (X_ISNAN(x)) return NA_INTEGER;
#if X_TYPE == 'r'
  if (x > R_INT_MAX || x < R_INT_MIN) return NA_INTEGER; // including the cases of Inf
#endif
  if (x > maxIdx) return NA_INTEGER;
  return x;
}

static R_INLINE int RealFromIndex_TYPE(X_C_TYPE x, R_xlen_t maxIdx) {
  if (X_ISNAN(x)) return NA_REAL;
#if X_TYPE == 'r'
  if (IS_INF(x)) return NA_REAL;
#endif
  if (x > maxIdx) return NA_REAL;
  return x;
}

/** idxs must not be NULL, which should be checked before calling this function. **/
void* METHOD_NAME(X_C_TYPE *idxs, R_xlen_t nidxs, R_xlen_t maxIdx, int allowOutOfBound, R_xlen_t *ansNidxs, int *subsettedType) {
  // For a un-full positive legal idxs array, we should use SUBSETTED_INTEGER as default.
  *subsettedType = SUBSETTED_INTEGER;

  R_xlen_t ii, jj;
  int state = 0;
  R_xlen_t count = 0;
  BOOL needReAlloc = 0;

  // figure out whether idxs are all positive or all negative.
  for (ii = 0; ii < nidxs; ++ ii) {
    X_C_TYPE idx = idxs[ii];
    if (idx > 0 || X_ISNAN(idx)
#if X_TYPE == 'r'
        || IS_INF(idx)
#endif
        ) {
      if (state < 0) error("only 0's may be mixed with negative subscripts");

#if X_TYPE == 'r'
      if (IS_INF(idx)) {
        needReAlloc = 1; // need to realloc indices array to set inf to NA
      } else
#endif
      if (!X_ISNAN(idx)) {
        if (idx > maxIdx) {
          if (!allowOutOfBound) error("subscript out of bounds");
          needReAlloc = 1;
        }
#if X_TYPE == 'r'
        if (idx > R_INT_MAX) *subsettedType = SUBSETTED_REAL;
#endif
      }// else error("NA index is not supported"); // NOTE: currently
      state = 1;
      ++ count;

    } else if (idx < 0) {
      if (state > 0) error("only 0's may be mixed with negative subscripts");
      state = -1;
      needReAlloc = 1;

    } else { // idx == 0, need to realloc indices array
      needReAlloc = 1;
    }
  }

  if (state >= 0) *ansNidxs = count;
  if (!needReAlloc) { // must have: state >= 0
    *subsettedType = SUBSETTED_DEFAULT;
    return idxs;
  }

  // fill positive idxs into ans
  if (state >= 0) {
    if (*subsettedType == SUBSETTED_INTEGER) {
      // NOTE: braces is needed here, because of macro-defined function
//#if X_TYPE == 'i'
      RETURN_VALIDATED_ANS(int, nidxs, idxs[ii], IntegerFromIndex_TYPE(idxs[ii],maxIdx),);
//#else // X_TYPE == 'r'
//      RETURN_VALIDATED_ANS(int, nidxs, idxs[ii], idxs[ii] > maxIdx || IS_INF(idxs[ii]) ? NA_INTEGER : IntegerFromReal(idxs[ii]),);
//#endif
    }
    // *subsettedType == SUBSETTED_REAL
//#if X_TYPE == 'i'
    RETURN_VALIDATED_ANS(double, nidxs, idxs[ii], RealFromIndex_TYPE(idxs[ii],maxIdx),);
//#else // X_TYPE == 'r'
//    RETURN_VALIDATED_ANS(double, nidxs, idxs[ii], idxs[ii] > maxIdx ? NA_REAL : idxs[ii],);
//#endif
  }

  // state < 0
  // use filter as bitset to find out all required idxs
  BOOL *filter = Calloc(maxIdx, BOOL);
  count = maxIdx;
  memset(filter, 0, maxIdx*sizeof(BOOL));
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
    RETURN_VALIDATED_ANS(int, upperBound, !filter[ii], ii + 1, Free(filter););
  } 
  // *subsettedType == SUBSETTED_REAL
  RETURN_VALIDATED_ANS(double, upperBound, !filter[ii], ii + 1, Free(filter););
}


#include "templates-types_undef.h"
