library(matrixStats)

validateIndicesTest <- function(x, idxs, mode, expect) {
  storage.mode(idxs) <- mode

  y <- tryCatch(validateIndices(idxs, length(x)), error=function(c) "error")
  if (missing(expect))
    expect <- tryCatch(x[idxs], error=function(c) "error")

  stopifnot(identical(y, expect))
}

#for (mode in c("integer", "numeric")) {
  mode <- "integer"
  x <- 1:5

  # mixed positive and negative indices
  validateIndicesTest(x, 1:-1, mode)
  validateIndicesTest(x, -1:1, mode)

  # midex positive, negative and zero indices
  validateIndicesTest(x, c(1, 0, 0, -6), mode)
  validateIndicesTest(x, c(-4, 0, 0, 1), mode)

  # negative indices with duplicates
  validateIndicesTest(x, c(-4, 0, 0, -3, -1, -3, -1), mode)

  # positive indices
  validateIndicesTest(x, c(3, 5, 1), mode)

  # positive indices with duplicates
  validateIndicesTest(x, c(3, 5, 1, 5, 5), mode)

  # positive indices out of ranges
  validateIndicesTest(x, 4:9, mode, "error")

  # negative out of ranges: just ignore
  validateIndicesTest(x, c(-5, 0, -3, -1), mode)



#}
