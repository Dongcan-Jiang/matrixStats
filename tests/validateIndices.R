library(matrixStats)

validateIndicesTest <- function(x, idxs, mode, expect) {
  if (!missing(mode))
    storage.mode(idxs) <- mode

  actual <- tryCatch(validateIndices(idxs, length(x)), error=function(c) "error")
  if (missing(expect))
    expect <- tryCatch(x[idxs], error=function(c) "error")

  if (!missing(mode) && !identical(actual,"error")) {
    storage.mode(actual) <- mode
    storage.mode(expect) <- mode
  }
  stopifnot(identical(actual, expect))
}

X <- 1:5

for (mode in c("integer", "numeric")) {
  # mixed positive and negative indices
  validateIndicesTest(X, 1:-1, mode)
  validateIndicesTest(X, -1:1, mode)

  # midex positive, negative and zero indices
  validateIndicesTest(X, c(1, 0, 0, -6), mode)
  validateIndicesTest(X, c(-4, 0, 0, 1), mode)

  # negative indices with duplicates
  validateIndicesTest(X, c(-4, 0, 0, -3, -1, -3, -1), mode)

  # positive indices
  validateIndicesTest(X, c(3, 5, 1), mode)

  # positive indices with duplicates
  validateIndicesTest(X, c(3, 5, 1, 5, 5), mode)

  # positive indices out of ranges
  validateIndicesTest(X, 4:9, mode, "error")

  # negative out of ranges: just ignore
  validateIndicesTest(X, c(-5, 0, -3, -1), mode)

  # negative indices exclude all
  validateIndicesTest(X, -1:-5, mode)

  # idxs is single  number
  validateIndicesTest(X, 4, mode)
  validateIndicesTest(X, -4, mode)
  validateIndicesTest(X, 0, mode)

  # idxs is empty
  validateIndicesTest(X, c(), mode)

  # NA in idxs
  # validateIndicesTest(X, c(NA, -2), mode)
  # validateIndicesTest(X, c(NA, 0, 2), mode)
  validateIndicesTest(X, c(NA, -2), mode, "error")
  validateIndicesTest(X, c(NA, 0, 2), mode, "error")

  # idxs is single NA
  # validateIndicesTest(X, NA, mode)

  # XX is empty
  XX <- integer(0)
  validateIndicesTest(XX, -4, mode)
  validateIndicesTest(XX, 0, mode)
  validateIndicesTest(XX, 4, mode, "error")
}

# idxs is NULL
acutal <- validateIndices(NULL, length(X))
expect <- X
stopifnot(identical(acutal, expect))

# single TRUE
validateIndicesTest(X, TRUE)
validateIndicesTest(X, FALSE)

# full logical idxs
validateIndicesTest(X, c(FALSE, TRUE, FALSE, TRUE, TRUE))

# too many logical idxs
validateIndicesTest(X, c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE), expect=as.integer(c(2, 4, 5)))

# insufficient idxs
validateIndicesTest(X, c(FALSE, TRUE, TRUE), expect=as.integer(c(2, 3)))
