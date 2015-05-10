library(matrixStats)

source("utils/validateIndicesFramework.R")
x <- 1:6
for (idxs in indexCases) {
  for (mode in c("integer", "numeric", "logical")) {
    if (!is.null(idxs))
      storage.mode(idxs) <- mode

    validateIndicesTestVector(x, idxs, ftest=function(x, idxs) {
      validateIndices(idxs, length(x))

    }, fsure=function(x, idxs) {
      x[idxs]
    })
  }
}

