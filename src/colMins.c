/***************************************************************************
 Public methods:
  SEXP colMins(SEXP x, SEXP rows, SEXP cols)
 **************************************************************************/
#include <Rdefines.h>
#include "types.h"
#include "utils.h"
#include <R_ext/Error.h>

#define METHOD colMins

#define X_TYPE 'i'
#include "colMins_TYPE-template.h"

#define X_TYPE 'r'
#include "colMins_TYPE-template.h"

#undef METHOD

SEXP colMins(SEXP x, SEXP rows, SEXP cols, SEXP cores) {

  SEXP ans;

  R_xlen_t ROWS = xlength(rows);
  R_xlen_t COLS = xlength(cols);

  R_xlen_t M = INTEGER(GET_DIM(x))[0];

  int CORES = asInteger(cores);

  /* Argument 'x': */
  assertArgVector(x, (R_TYPE_INT | R_TYPE_REAL), "x");

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    ans = PROTECT(allocVector(REALSXP, COLS));
    pthread_colMins_Real(REAL(x), M, INTEGER(rows), ROWS, INTEGER(cols), COLS, CORES, REAL(ans));

  } else if (isInteger(x)) {
    ans = PROTECT(allocVector(INTSXP, COLS));
    pthread_colMins_Integer(INTEGER(x), M, INTEGER(rows), ROWS, INTEGER(cols), COLS, CORES, INTEGER(ans));

  } else error("Unsupported type");

  UNPROTECT(1);
  return ans;
} // colMins()


/***************************************************************************
 HISTORY:
 2015-03-15 [HB]
  o Created.
 **************************************************************************/
