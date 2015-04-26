/***************************************************************************
 Public methods:
 SEXP colCounts(SEXP x, SEXP value, SEXP naRm, SEXP hasNA, SEXP rows, SEXP cols)

 Copyright Henrik Bengtsson, 2014
 **************************************************************************/
#include <Rdefines.h>
#include "types.h"
#include "utils.h"


#define METHOD colCounts
#define RETURN_TYPE void
#define ARGUMENTS_LIST X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, X_C_TYPE value, int what, int narm, int hasna, int *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols

#define X_TYPE_I
#define X_TYPE_R
#define X_TYPE_L
#include "templates-gen.h"


SEXP colCounts(SEXP x, SEXP dim, SEXP value, SEXP what, SEXP naRm, SEXP hasNA, SEXP rows, SEXP cols) {
  SEXP ans;
  int narm, hasna, what2;
  R_xlen_t nrow, ncol;

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_LGL | R_TYPE_INT | R_TYPE_REAL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  /* Argument 'value': */
  if (length(value) != 1)
    error("Argument 'value' must be a single value.");

  if (!isNumeric(value))
    error("Argument 'value' must be a numeric value.");

  R_xlen_t nrows, ncols;
  int rowsType, colsType;
  void *crows = validateIndices(rows, nrow, &nrows, &rowsType);
  void *ccols = validateIndices(cols, ncol, &ncols, &colsType);

  /* Argument 'what': */
  what2 = asInteger(what);

  /* Argument 'naRm': */
  narm = asLogicalNoNA(naRm, "na.rm");

  /* Argument 'hasNA': */
  hasna = asLogicalNoNA(hasNA, "hasNA");

  /* R allocate an integer vector of length 'ncol' */
  PROTECT(ans = allocVector(INTSXP, ncols));

  if (isReal(x)) {
    colCounts_Real[rowsType][colsType](REAL(x), nrow, ncol, asReal(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  } else if (isInteger(x)) {
    colCounts_Integer[rowsType][colsType](INTEGER(x), nrow, ncol, asInteger(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  } else if (isLogical(x)) {
    colCounts_Logical[rowsType][colsType](LOGICAL(x), nrow, ncol, asLogical(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  }

  UNPROTECT(1);

  return(ans);
} // colCounts()


SEXP count(SEXP x, SEXP value, SEXP what, SEXP naRm, SEXP hasNA, SEXP idxs) {
  SEXP ans;
  int narm, hasna, what2;
  R_xlen_t nx;

  /* Argument 'x' and 'dim': */
  assertArgVector(x, (R_TYPE_LGL | R_TYPE_INT | R_TYPE_REAL), "x");
  nx = xlength(x);

  /* Argument 'value': */
  if (length(value) != 1)
    error("Argument 'value' must be a single value.");

  if (!isNumeric(value))
    error("Argument 'value' must be a numeric value.");

  R_xlen_t nrows = 1, ncols;
  int rowsType = SUBSETTED_ALL, colsType;
  void *crows = NULL;
  void *ccols = validateIndices(idxs, nx, &ncols, &colsType);

  /* Argument 'what': */
  what2 = asInteger(what);

  /* Argument 'naRm': */
  narm = asLogicalNoNA(naRm, "na.rm");

  /* Argument 'hasNA': */
  hasna = asLogicalNoNA(hasNA, "hasNA");

  /* R allocate a integer scalar */
  PROTECT(ans = allocVector(INTSXP, 1));

  if (isReal(x)) {
    colCounts_Real[rowsType][colsType](REAL(x), nx, 1, asReal(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  } else if (isInteger(x)) {
    colCounts_Integer[rowsType][colsType](INTEGER(x), nx, 1, asInteger(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  } else if (isLogical(x)) {
    colCounts_Logical[rowsType][colsType](LOGICAL(x), nx, 1, asLogical(value), what2, narm, hasna, INTEGER(ans), crows, nrows, ccols, ncols);
  }

  UNPROTECT(1);

  return(ans);
} // count()


/***************************************************************************
 HISTORY:
 2015-04-21 [DJ]
  o Supported subsetted computation.
 2014-11-14 [HB]
  o Created from rowCounts.c.
 **************************************************************************/
