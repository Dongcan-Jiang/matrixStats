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


SEXP colMins(SEXP x, SEXP rows, SEXP cols) {
  SEXP ans;

  R_xlen_t ROWS = xlength(rows);
  R_xlen_t COLS = xlength(cols);

  R_xlen_t M = INTEGER(GET_DIM(x))[0];

  /* Argument 'x': */
  assertArgVector(x, (R_TYPE_INT | R_TYPE_REAL), "x");

  /* Double matrices are more common to use. */
  if (isReal(x)) {
    ans = colMins_Real(REAL(x), M, INTEGER(rows), ROWS, INTEGER(cols), COLS);

  } else if (isInteger(x)) {
    ans = colMins_Integer(INTEGER(x), M, INTEGER(rows), ROWS, INTEGER(cols), COLS);
  }

  return ans;
} // colMins()


/***************************************************************************
 HISTORY:
 2015-03-15 [HB]
  o Created.
 **************************************************************************/
