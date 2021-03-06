/***************************************************************************
 Public methods:
 SEXP rowMads(SEXP x, ...)

 Authors: Henrik Bengtsson

 Copyright Henrik Bengtsson, 2014
 **************************************************************************/
#include <Rdefines.h>
#include "types.h"
#include "utils.h"


#define METHOD rowMads
#define RETURN_TYPE void
#define ARGUMENTS_LIST X_C_TYPE *x, R_xlen_t nrow, R_xlen_t ncol, void *rows, R_xlen_t nrows, void *cols, R_xlen_t ncols, double scale, int narm, int hasna, int byrow, double *ans, int cores

#define X_TYPE 'i'
#include "templates-gen-matrix.h"
#define X_TYPE 'r'
#include "templates-gen-matrix.h"


SEXP rowMads(SEXP x, SEXP dim, SEXP rows, SEXP cols, SEXP constant, SEXP naRm, SEXP hasNA, SEXP byRow, SEXP cores) {
  int narm, hasna, byrow, cores2;
  SEXP ans;
  R_xlen_t nrow, ncol;
  double scale;

  /* Argument 'x' and 'dim': */
  assertArgMatrix(x, dim, (R_TYPE_INT | R_TYPE_REAL), "x");
  nrow = asR_xlen_t(dim, 0);
  ncol = asR_xlen_t(dim, 1);

  /* Argument 'constant': */
  if (!isNumeric(constant))
    error("Argument 'constant' must be a numeric scale.");
  scale = asReal(constant);

  /* Argument 'naRm': */
  narm = asLogicalNoNA(naRm, "na.rm");

  /* Argument 'hasNA': */
  hasna = asLogicalNoNA(hasNA, "hasNA");

  /* Argument 'rows' and 'cols': */
  R_xlen_t nrows, ncols;
  int rowsType, colsType;
  void *crows = validateIndices(rows, nrow, 0, &nrows, &rowsType);
  void *ccols = validateIndices(cols, ncol, 0, &ncols, &colsType);

  /* Argument 'byRow': */
  byrow = asLogical(byRow);

#ifdef _USE_PTHREAD_
  /* Argument 'cores': */
  cores2 = asInteger(cores);
  if (cores2 <= 0)
    error("Argument 'cores' must be a positive value.");
#else
  cores2 = 1;
#endif

  if (!byrow) {
    SWAP(R_xlen_t, nrow, ncol);
    SWAP(void*, crows, ccols);
    SWAP(R_xlen_t, nrows, ncols);
    SWAP(int, rowsType, colsType);
  }

  /* R allocate a double vector of length 'nrow'
     Note that 'nrow' means 'ncol' if byrow=FALSE. */
  PROTECT(ans = allocVector(REALSXP, nrows));

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    rowMads_Real[rowsType][colsType](REAL(x), nrow, ncol, crows, nrows, ccols, ncols, scale, narm, hasna, byrow, REAL(ans), cores2);
  } else if (isInteger(x)) {
    rowMads_Integer[rowsType][colsType](INTEGER(x), nrow, ncol, crows, nrows, ccols, ncols, scale, narm, hasna, byrow, REAL(ans), cores2);
  }

  UNPROTECT(1);

  return(ans);
} /* rowMads() */


/***************************************************************************
 HISTORY:
 2015-08-13 [DJ]
  o Pthread processing.
 2015-06-13 [DJ]
  o Supported subsetted computation.
 2014-11-17 [HB]
 o Created from rowMedians.c.
 **************************************************************************/
