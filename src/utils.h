#include <Rinternals.h>
#include "types.h"

#define R_TYPE_LGL  1 /* 0b0001 */
#define R_TYPE_INT  2 /* 0b0010 */
#define R_TYPE_REAL 4 /* 0b0100 */


static R_INLINE void assertArgVector(SEXP x, int type, char *xlabel) {
  /* Argument 'x': */
  if (!isVectorAtomic(x)) {
    error("Argument '%s' must be a matrix or a vector.", xlabel);
  }
  switch (TYPEOF(x)) {
    case LGLSXP:
      if (!(type & R_TYPE_LGL))
        error("Argument '%s' cannot be logical.", xlabel);
      break;

    case INTSXP:
      if (!(type & R_TYPE_INT))
        error("Argument '%s' cannot be integer.", xlabel);
      break;

    case REALSXP:
      if (!(type & R_TYPE_REAL))
        error("Argument '%s' cannot be numeric.", xlabel);
      break;
  } /* switch */
} /* assertArgVector() */


static R_INLINE void assertArgDim(SEXP dim, double max, char *maxlabel) {
  double nrow, ncol;

  /* Argument 'dim': */
  if (!isVectorAtomic(dim) || xlength(dim) != 2 || !isInteger(dim)) {
    error("Argument 'dim' must be an integer vector of length two.");
  }
  nrow = (double)INTEGER(dim)[0];
  ncol = (double)INTEGER(dim)[1];
  if (nrow < 0) {
    error("Argument 'dim' specifies a negative number of rows (dim[1]): %d", nrow); 
  } else if (ncol < 0) {
    error("Argument 'dim' specifies a negative number of columns (dim[2]): %d", ncol); 
  } else if (nrow * ncol != max) {
    error("Argument 'dim' does not match length of argument '%s': %g * %g != %g", maxlabel, nrow, ncol, max);
  }
} /* assertArgDim() */


static R_INLINE void assertArgMatrix(SEXP x, SEXP dim, int type, char *xlabel) {
  /* Argument 'x': */
  if (isMatrix(x)) {
  } else if (isVectorAtomic(x)) {
  } else {
    error("Argument '%s' must be a matrix or a vector.", xlabel);
  }
  switch (TYPEOF(x)) {
    case LGLSXP:
      if (!(type & R_TYPE_LGL))
        error("Argument '%s' cannot be logical.", xlabel);
      break;

    case INTSXP:
      if (!(type & R_TYPE_INT))
        error("Argument '%s' cannot be integer.", xlabel);
      break;

    case REALSXP:
      if (!(type & R_TYPE_REAL))
        error("Argument '%s' cannot be numeric.", xlabel);
      break;
  } /* switch */

  /* Argument 'dim': */
  assertArgDim(dim, xlength(x), "x");
} /* assertArgMatrix() */ 



static R_INLINE int asLogicalNoNA(SEXP x, char *xlabel) {
  int value = 0;

  if (length(x) != 1)
    error("Argument '%s' must be a single value.", xlabel);
  if (isLogical(x)) {
    value = asLogical(x);
  } else if (isInteger(x)) {
    value = asInteger(x);
  } else {
    error("Argument '%s' must be a logical.", xlabel);
  }
  if (value != TRUE && value != FALSE)
    error("Argument '%s' must be either TRUE or FALSE.", xlabel); 
  
  return value;
} /* asLogicalNoNA() */


static R_INLINE int modeSubsettedIndex(SEXP x, void *idxs) {
  if (idxs == NULL) return 0;
  int mode = TYPEOF(x);
  switch (mode) {
    case INTSXP: return 1;
    case REALSXP: return 2;
    case LGLSXP: return 3;
    default: error("only integer, numeric, logical are supported.");
  }
}


static R_INLINE R_xlen_t asR_xlen_t(SEXP x, R_xlen_t i) {
  int mode = TYPEOF(x);
  switch (mode) {
    case INTSXP: return INTEGER(x)[i];
    case REALSXP: return REAL(x)[i];
    default: error("only integer and numeric are supported.");
  }
  return 0;
}


/** idxs must not be NULL, which should be checked before calling this function. **/
static R_INLINE int* validateIndices(SEXP idxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs) {
  assertArgVector(idxs, R_TYPE_INT, "idxs");
  R_xlen_t nidxs = xlength(idxs);
  int *cidxs = INTEGER(idxs);
  R_xlen_t ii, jj;

  int state = 0;
  R_xlen_t count = 0;
  for (ii = 0; ii < nidxs; ++ ii) {
    if (cidxs[ii] > 0 || cidxs[ii] == NA_INTEGER) {
      if (cidxs[ii] != NA_INTEGER) {
        if (state < 0) error("only 0's may be mixed with negative subscripts");
        if (cidxs[ii] > maxIdx) error("subscript out of bounds");
      }
      state = 1;
      ++ count;

    } else if (cidxs[ii] < 0) {
      if (state > 0) error("only 0's may be mixed with negative subscripts");
      state = -1;
    }
  }

  if (state >= 0) *ansNidxs = count;
  if (count == nidxs) return cidxs; // must have: state >= 0
  if (state == 0) return NULL;

  if (state > 0) {
    int *ans = (int*) R_alloc(count, sizeof(int));
    jj = 0;
    for (ii = 0; ii < nidxs; ++ ii) {
      // cidxs[ii] can be positive or 0
      if (cidxs[ii]) ans[jj ++] = cidxs[ii];
    }
    return ans;
  }

  // state < 0
  BOOL filter[maxIdx];
  count = maxIdx;
  memset(filter, 0, sizeof(filter));
  for (ii = 0; ii < nidxs; ++ ii) {
    R_xlen_t idx = -cidxs[ii];
    if (idx > 0 && idx <= maxIdx) {
      if (filter[idx-1] == 0) {
        -- count;
        filter[idx-1] = 1;
      }
    }
  }

  *ansNidxs = count;
  if (count == 0) return NULL;

  int *ans = (int*) R_alloc(count, sizeof(int));
  jj = 0;
  for (ii = 0; ii < maxIdx; ++ ii) {
    if (!filter[ii]) ans[jj ++] = ii + 1;
  }

  return ans;
}

void *validateIndices2(SEXP idxs, R_xlen_t maxIdx, R_xlen_t *ansNidxs);

