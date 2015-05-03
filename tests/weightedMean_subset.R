library("matrixStats")

x <- runif(5, min=-5, max=5)
w <- runif(5, min=-5, max=5)

validateIndicesTest <- function(x, w, idxs, mode, expect) {
  if (!missing(mode))
    storage.mode(idxs) <- mode

  actual <- tryCatch(weightedMean(x,w,idxs), error=function(c) "error")
  if (missing(expect)) {
    expect <- tryCatch(weighted.mean(x[idxs],w[idxs]), error=function(c) "error")
  }

  cat(sprintf("idxs=%s\nactual=%s\nexpect=%s\n",
        toString(idxs), toString(actual), toString(expect)))
  stopifnot(all.equal(actual, expect))
}

for (mode in c("integer", "numeric")) {
  # mixed positive and negative indices
  validateIndicesTest(x, w, 1:-1, mode)
  validateIndicesTest(x, w, -1:1, mode)

  # midex positive, negative and zero indices
  validateIndicesTest(x, w, c(1, 0, 0, -6), mode)
  validateIndicesTest(x, w, c(-4, 0, 0, 1), mode)

  # negative indices with duplicates
  validateIndicesTest(x, w, c(-4, 0, 0, -3, -1, -3, -1), mode)

  # positive indices
  validateIndicesTest(x, w, c(3, 5, 1), mode)

  # positive indices with duplicates
  validateIndicesTest(x, w, c(3, 5, 1, 5, 5), mode)

  # positive indices out of ranges
  validateIndicesTest(x, w, 4:9, mode)

  # negative out of ranges: just ignore
  validateIndicesTest(x, w, c(-5, 0, -3, -1), mode)

  # negative indices exclude all
  validateIndicesTest(x, w, -1:-5, mode)

  # idxs is single  number
  validateIndicesTest(x, w, 4, mode)
  validateIndicesTest(x, w, -4, mode)
  validateIndicesTest(x, w, 0, mode)

  # idxs is empty
  validateIndicesTest(x, w, c(), mode)

  # NA in idxs
  validateIndicesTest(x, w, c(NA, -2), mode)
  validateIndicesTest(x, w, c(NA, 0, 2), mode)

  # idxs is single NA
  validateIndicesTest(x, w, NA, mode)

  # XX is empty
  xx <- integer(0)
  ww <- integer(0)
  validateIndicesTest(xx, ww, -4, mode)
  validateIndicesTest(xx, ww, 0, mode)
  validateIndicesTest(xx, ww, 4, mode)
}

# idxs is NULL
actual <- weightedMean(x, w, NULL)
expect <- weighted.mean(x, w)
stopifnot(all.equal(actual, expect))

# single TRUE
validateIndicesTest(x, w, TRUE)
validateIndicesTest(x, w, FALSE)

# full logical idxs
validateIndicesTest(x, w, c(FALSE, TRUE, FALSE, TRUE, TRUE))

# too many logical idxs
validateIndicesTest(x, w, c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))

# insufficient idxs
validateIndicesTest(x, w, c(FALSE, TRUE, TRUE))
