library("matrixStats")

validateIndicesTestVector <- function(x, idxs, ftest, fsure, expect, ...) {
  cat(sprintf("idxs=%s, type=%s\n", toString(idxs), toString(typeof(idxs))))
  if (identical(idxs, "NULL")) idxs <- NULL

  actual <- tryCatch(ftest(x,idxs,...), error=function(c) "error")
  if (missing(expect)) {
    if (is.null(idxs)) idxs <- 1:length(x)
    expect <- tryCatch(fsure(x[idxs],...), error=function(c) "error")
  }
  cat(sprintf("actual=%s\nexpect=%s\n", toString(actual), toString(expect)))

  stopifnot(all.equal(actual, expect))
}

validateIndicesTestVector_w <- function(x, w, idxs, ftest, fsure, expect, ...) {
  cat(sprintf("idxs=%s, type=%s\n", toString(idxs), toString(typeof(idxs))))
  if (identical(idxs, "NULL")) idxs <- NULL

  actual <- tryCatch(ftest(x,w,idxs,...), error=function(c) "error")
  if (missing(expect)) {
    if (is.null(idxs)) idxs <- 1:length(x)
    expect <- tryCatch(fsure(x[idxs],w[idxs],...), error=function(c) "error")
  }
  cat(sprintf("actual=%s\nexpect=%s\n", toString(actual), toString(expect)))

  stopifnot(all.equal(actual, expect))
}

validateIndicesTestMatrix <- function(x, rows, cols, ftest, fsure, expect, ...) {
  cat(sprintf("rows=%s; type=%s\n", toString(rows), toString(typeof(rows))))
  cat(sprintf("cols=%s; type=%s\n", toString(cols), toString(typeof(cols))))
  if (identical(rows, "NULL")) rows <- NULL
  if (identical(cols, "NULL")) cols <- NULL

  actual <- tryCatch(ftest(x,rows,cols,...), error=function(c) "error")
  if (missing(expect)) {
    if (is.null(rows)) rows <- 1:dim(x)[1]
    if (is.null(cols)) cols <- 1:dim(x)[2]
    expect <- tryCatch(fsure(x[rows,cols],...), error=function(c) "error")
  }
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
indexCases[[length(indexCases)+1]] <- c(3, 0, 0, 5, 1, 5, 5)

# positive indices out of ranges
indexCases[[length(indexCases)+1]] <- 4:9

# negative out of ranges: just ignore
indexCases[[length(indexCases)+1]] <- c(-5, 0, -3, -1, -9)

# negative indices exclude all
indexCases[[length(indexCases)+1]] <- -1:-6

# idxs is single number
indexCases[[length(indexCases)+1]] <- 4
indexCases[[length(indexCases)+1]] <- -4
indexCases[[length(indexCases)+1]] <- 0

# idxs is empty
indexCases[[length(indexCases)+1]] <- integer()

# NA in idxs
indexCases[[length(indexCases)+1]] <- c(NA, -2)
indexCases[[length(indexCases)+1]] <- c(NA, 0, 2)

# single Logical
indexCases[[length(indexCases)+1]] <- NA
indexCases[[length(indexCases)+1]] <- TRUE
indexCases[[length(indexCases)+1]] <- FALSE

# full logical idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)

# too many logical idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)

# insufficient idxs
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE)
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, NA)
indexCases[[length(indexCases)+1]] <- c(FALSE, TRUE, NA, FALSE)

# NULL
indexCases[[length(indexCases)+1]] <- "NULL"
