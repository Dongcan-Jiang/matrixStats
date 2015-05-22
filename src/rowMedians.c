/***************************************************************************
 Public methods:
 SEXP rowMedians(SEXP x, ...)

 Authors: Adopted from rowQuantiles.c by R. Gentleman.

 Copyright Henrik Bengtsson, 2007
 **************************************************************************/
#include <Rdefines.h>
#include "types.h"
#include "utils.h"

#define METHOD rowMedians
#define RETURN_TYPE void
#define ARGUMENTS_LIST X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, int narm, int hasna, int byrow, double *ans, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols

#define X_TYPE_I
#define X_TYPE_R
#include "templates-gen.h"


SEXP rowMedians(SEXP x, SEXP dim, SEXP naRm, SEXP hasNA, SEXP byRow, SEXP rows, SEXP cols) {
  int narm, hasna, byrow;
  SEXP ans;
  R_xlen_t nrow, ncol;

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL), "x");
  /* Get dimensions of 'x'. */
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  R_xlen_t nrows, ncols;
  int rowsType, colsType;
  void *crows = validateIndices(rows, nrow, 0, &nrows, &rowsType);
  void *ccols = validateIndices(cols, ncol, 0, &ncols, &colsType);

  /* Argument 'naRm': */
  narm = asLogicalNoNA(naRm, "na.rm");

  /* Argument 'hasNA': */
  hasna = asLogicalNoNA(hasNA, "hasNA");

  /* Argument 'byRow': */
  byrow = asLogical(byRow);

  if (!byrow) {
    SWAP(R_xlen_t, nrow, ncol);
    SWAP(void*, crows, ccols);
    SWAP(R_xlen_t, nrows, ncols);
    SWAP(int, rowsType, colsType);
  }

  /* R allocate a double vector of length 'nrows'
     Note that 'nrows' means 'ncols' if byrow=FALSE. */
  PROTECT(ans = allocVector(REALSXP, nrows));

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    rowMedians_Real[rowsType][colsType](REAL(x), nrow, ncol, narm, hasna, byrow, REAL(ans), crows, nrows, ccols, ncols);
  } else if (isInteger(x)) {
    rowMedians_Integer[rowsType][colsType](INTEGER(x), nrow, ncol, narm, hasna, byrow, REAL(ans), crows, nrows, ccols, ncols);
  }

  UNPROTECT(1);

  return(ans);
} /* rowMedians() */


/***************************************************************************
 HISTORY:
 2013-01-13 [HB]
 o Added argument 'byRow' to rowMedians() and dropped colMedians().
 o Using internal arguments 'by_row' instead of 'by_column'.
 2011-12-11 [HB]
 o BUG FIX: rowMediansReal(..., na.rm=TRUE) did not handle NaN:s, only NA:s.
   Note that NaN:s does not exist for integers.
 2011-10-12 [HJ]
 o Added colMedians().
 o Now rowMediansInteger/Real() can operate also by columns, cf. argument
   'by_column'.
 2007-08-14 [HB]
 o Added checks for user interrupts every 1000 line.
 o Added argument 'hasNA' to rowMedians().
 2005-12-07 [HB]
 o BUG FIX: When calculating the median of an even number (non-NA) values,
    the length of the second sort was one element too short, which made the
    method to freeze, i.e. rPsort(rowData, qq, qq) is now (...qq+1, qq).
 2005-11-24 [HB]
  o By implementing a special version for integers, there is no need to
    coerce to double in R, which would take up twice the amount of memory.
  o rowMedians() now handles NAs too.
  o Adopted from rowQuantiles.c in Biobase of Bioconductor.
 **************************************************************************/
