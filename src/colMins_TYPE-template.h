/***********************************************************************
 TEMPLATE:
  void colMins_<Integer|Real>(X_C_TYPE *x, R_xlen_t M, R_xlen_t ROWS, int *cols, R_xlen_t COLS)

 GENERATES:
  void colMins_Real(double *x, R_xlen_t M, int *row, R_xlen_t ROWS, int *cols, R_xlen_t COLS)
  void colMins_Integer(int *x, R_xlen_t M, R_xlen_t ncol, int value, int narm, int hasna, int *ans)

 Arguments:
   The following macros ("arguments") should be defined for the 
   template to work as intended.

  - METHOD_NAME: the name of the resulting function
  - X_TYPE: 'i', 'r'

 ***********************************************************************/ 
#include <pthread.h>
#include "types.h"
#include "utils.h"

/* Expand arguments:
    X_TYPE => (X_C_TYPE, X_IN_C, [METHOD_NAME])
 */
#include "templates-types.h" 

void METHOD_NAME(X_C_TYPE *x, R_xlen_t M, int *rows, R_xlen_t ROWS, int *cols, R_xlen_t COLS,
    X_C_TYPE *ans) {
  R_xlen_t i, j, k;

  for (i = 0; i < COLS; ++ i) {
    k = M * (cols[i] - 1);
    X_C_TYPE minValue = x[k+rows[0]-1];
    for (j = 1; j < ROWS; ++ j) {
      X_C_TYPE value = x[k+rows[j]-1];
      if (value < minValue) minValue = value;
    }
    ans[i] = minValue;
  }
}

static void *METHOD_NAME_WRAPPER(void *args) {
  X_C_TYPE *x = ((X_C_TYPE**)args)[0];
  R_xlen_t M = (R_xlen_t)((void**)args)[1];
  int *rows = ((int**)args)[2];
  R_xlen_t ROWS = (R_xlen_t)((void**)args)[3];
  int *cols = ((int**)args)[4];
  R_xlen_t COLS = (R_xlen_t)((void**)args)[5];
  X_C_TYPE *ans = ((X_C_TYPE**)args)[6];
  METHOD_NAME(x, M, rows, ROWS, cols, COLS, ans);
  return NULL;
}

void PTHREAD_METHOD_NAME(X_C_TYPE *x, R_xlen_t M,
    int *rows, R_xlen_t ROWS, int *cols, R_xlen_t COLS,
    int CORES, X_C_TYPE *ans) {
  if (CORES <= 1) {
    METHOD_NAME(x, M, rows, ROWS, cols, COLS, ans);
    return;
  }
  void *args[CORES][7];
  pthread_t threads[CORES];
  int begin = 0;
  int GAP = (COLS + CORES - 1) / CORES;

  for (int i = 0; i < CORES; ++ i) {
    // WARNING: Assume that sizeof(void*) >= sizeof(R_xlen_t)
    int end = min(begin + GAP, COLS);
    int *sub_cols = cols + begin;
    R_xlen_t SUB_COLS = end - begin;
    X_C_TYPE *sub_ans = ans + begin;

    args[i][0] = (void*) x;
    args[i][1] = (void*) M;
    args[i][2] = (void*) rows;
    args[i][3] = (void*) ROWS;
    args[i][4] = (void*) sub_cols;
    args[i][5] = (void*) SUB_COLS;
    args[i][6] = (void*) sub_ans;

    if (SUB_COLS > 0)
//      METHOD_NAME_WRAPPER((void*)args[i]);
      pthread_create(threads+i, NULL, (void*)&METHOD_NAME_WRAPPER, (void*)args[i]);
    begin = end;
  }

  for (int i = 0; i < CORES; ++ i) {
    pthread_join(threads[i], NULL);
  }
}

/* Undo template macros */
#include "templates-types_undef.h" 


/***************************************************************************
 HISTORY:
 2015-03-23
  o Created.
 **************************************************************************/
