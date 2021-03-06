library("matrixStats")

rowLogSumExps_R <- function(x, ...) {
  apply(x, MARGIN=1L, FUN=function(rx, ...) {
    log(sum(exp(rx), ...))
  }, ...)
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Multicore tests
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
source("utils/validateIndicesFramework.R")
x <- matrix(runif(6*6, min=-6, max=6), nrow=6, ncol=6)
for (rows in indexCases) {
  for (cols in indexCases) {
    validateIndicesTestMatrix(x, rows, cols, ftest=rowLogSumExps, fsure=rowLogSumExps_R, mc.cores=2L)
    validateIndicesTestMatrix(x, rows, cols, fcoltest=colLogSumExps, fsure=rowLogSumExps_R, mc.cores=2L)
  }
}
