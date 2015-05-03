library("matrixStats")


validateIndicesTestVector_w <- function(x, w, idxs, ftest, fsure, expect, ...) {
  actual <- tryCatch(ftest(x,w,idxs,...), error=function(c) "error")
  if (missing(expect)) {
    expect <- tryCatch(fsure(x[idxs],w[idxs],...), error=function(c) "error")
  }
  cat(sprintf("idxs=%s\n", toString(idxs)))
  cat(sprintf("actual=%s\nexpect=%s\n", toString(actual), toString(expect)))

  stopifnot(all.equal(actual, expect))
}

validateIndicesTestMatrix <- function(x, rows, cols, ftest, fsure, expect, ...) {
  actual <- tryCatch(ftest(x,rows,cols,...), error=function(c) "error")
  if (missing(expect)) {
    expect <- tryCatch(fsure(x[rows,cols],...), error=function(c) "error")
  }
  cat(sprintf("rows=%s; cols=%s\n", toString(rows), toString(cols)))
  cat(sprintf("actual=%s\nexpect=%s\n", toString(actual), toString(expect)))

  stopifnot(all.equal(actual, expect))
}


indexCases <- list()
# mixed positive and negative indices
indexCases[[length(indexCases)+1]] <- 1:-1
indexCases[[length(indexCases)+1]] <- -1:1

# midex positive, negative and zero indices
indexCases[[length(indexCases)+1]] <- c(1, 0, 0, -6)
indexCases[[length(indexCases)+1]] <- c(-4, 0, 0, 1)

# negative indices with duplicates
indexCases[[length(indexCases)+1]] <- c(-4, 0, 0, -3, -1, -3, -1)

# positive indices
indexCases[[length(indexCases)+1]] <- c(3, 5, 1)

# positive indices with duplicates
indexCases[[length(indexCases)+1]] <- c(3, 5, 1, 5, 5)

# positive indices out of ranges
indexCases[[length(indexCases)+1]] <- 4:9

# negative out of ranges: just ignore
indexCases[[length(indexCases)+1]] <- c(-5, 0, -3, -1)

# negative indices exclude all
indexCases[[length(indexCases)+1]] <- -1:-5

# idxs is single number
indexCases[[length(indexCases)+1]] <- 4
indexCases[[length(indexCases)+1]] <- -4
indexCases[[length(indexCases)+1]] <- 0

# idxs is empty
indexCases[[length(indexCases)+1]] <- c()

# NA in idxs
indexCases[[length(indexCases)+1]] <- c(NA, -2)
indexCases[[length(indexCases)+1]] <- c(NA, 0, 2)

# idxs is single NA
indexCases[[length(indexCases)+1]] <- NA

# single TRUE
indexCases[[length(indexCases)+1]] <- TRUE
indexCases[[length(indexCases)+1]] <- FALSE

# full logical idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE)

# too many logical idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)

# insufficient idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, TRUE)
