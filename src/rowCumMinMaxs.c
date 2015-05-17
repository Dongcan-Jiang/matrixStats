/***************************************************************************
 Public methods:
 SEXP rowCummins(SEXP x, SEXP naRm, SEXP hasNA)
 SEXP colCummins(SEXP x, SEXP naRm, SEXP hasNA)

 Authors: Henrik Bengtsson

 Copyright Henrik Bengtsson, 2014
 **************************************************************************/
#include <Rdefines.h>
#include "types.h"
#include "utils.h"

#define RETURN_TYPE void
#define ARGUMENTS_LIST X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, int byrow, ANS_C_TYPE *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols

#define METHOD rowCummins
#define COMP '<'
#define X_TYPE_I
#define X_TYPE_R
#define METHOD_TEMPLATE_H "rowCumMinMaxs_TYPE-template.h"
#include "templates-gen.h"

SEXP rowCummins(SEXP x, SEXP dim, SEXP byRow, SEXP rows, SEXP cols) {
  int byrow;
  SEXP ans = NILSXP;
  R_xlen_t nrow, ncol;

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  R_xlen_t nrows, ncols;
  int rowsType, colsType;
  void *crows = validateIndices(rows, nrow, 0, &nrows, &rowsType);
  void *ccols = validateIndices(cols, ncol, 0, &ncols, &colsType);

  /* Argument 'byRow': */
  byrow = asLogical(byRow);

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    PROTECT(ans = allocMatrix(REALSXP, nrows, ncols));
    rowCummins_Real[rowsType][colsType](REAL(x), nrow, ncol, byrow, REAL(ans), crows, nrows, ccols, ncols);
    UNPROTECT(1);
  } else if (isInteger(x)) {
    PROTECT(ans = allocMatrix(INTSXP, nrows, ncols));
    rowCummins_Integer[rowsType][colsType](INTEGER(x), nrow, ncol, byrow, INTEGER(ans), crows, nrows, ccols, ncols);
    UNPROTECT(1);
  }

  return(ans);
} /* rowCummins() */

#undef COMP
#undef METHOD


#define RETURN_TYPE void
#define ARGUMENTS_LIST X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, int byrow, ANS_C_TYPE *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols

#define METHOD rowCummaxs
#define COMP '>'
#define X_TYPE_I
#define X_TYPE_R
#define METHOD_TEMPLATE_H "rowCumMinMaxs_TYPE-template.h"
#include "templates-gen.h"

SEXP rowCummaxs(SEXP x, SEXP dim, SEXP byRow, SEXP rows, SEXP cols) {
  int byrow;
  SEXP ans = NILSXP;
  R_xlen_t nrow, ncol;

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  R_xlen_t nrows, ncols;
  int rowsType, colsType;
  void *crows = validateIndices(rows, nrow, 0, &nrows, &rowsType);
  void *ccols = validateIndices(cols, ncol, 0, &ncols, &colsType);

  /* Argument 'byRow': */
  byrow = asLogical(byRow);

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    PROTECT(ans = allocMatrix(REALSXP, nrows, ncols));
    rowCummaxs_Real[rowsType][colsType](REAL(x), nrow, ncol, byrow, REAL(ans), crows, nrows, ccols, ncols);
    UNPROTECT(1);
  } else if (isInteger(x)) {
    PROTECT(ans = allocMatrix(INTSXP, nrows, ncols));
    rowCummaxs_Integer[rowsType][colsType](INTEGER(x), nrow, ncol, byrow, INTEGER(ans), crows, nrows, ccols, ncols);
    UNPROTECT(1);
  }

  return(ans);
} /* rowCummaxs() */

#undef COMP


/***************************************************************************
 HISTORY:
 2014-11-26 [HB]
 o Created from rowVars.c.
 **************************************************************************/
