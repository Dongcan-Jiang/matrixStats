library("matrixStats")

rowRanks_R <- function(x, ties.method="average", ...) {
  ans <- t(apply(x, MARGIN=1L, FUN=rank, na.last="keep", ties.method=ties.method))
  dim(ans) <- dim(x)
  ans
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Multicore tests
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
source("utils/validateIndicesFramework.R")
x <- matrix(runif(6*6, min=-6, max=6), nrow=6, ncol=6)
storage.mode(x) <- "integer"
for (rows in indexCases) {
  for (cols in indexCases) {
    validateIndicesTestMatrix(x, rows, cols, ftest=rowRanks, fsure=rowRanks_R, ties.method="average", mc.cores=2L, debug=TRUE)

    validateIndicesTestMatrix(x, rows, cols, ftest=function(x, rows, cols, ...) {
      t(colRanks(t(x), rows=cols, cols=rows, preserveShape=TRUE, ...))
    }, fsure=rowRanks_R, ties.method="average", mc.cores=2L, debug=TRUE)
  }
}
