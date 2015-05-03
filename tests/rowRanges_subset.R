library("matrixStats")

x <- matrix(runif(5*5, min=-5, max=5), nrow=5, ncol=5)

rowRanges_R <- function(x, ...) {
  suppressWarnings({
    ans <- t(apply(x, MARGIN=1L, FUN=range, ...))
  })
  dim(ans) <- c(dim(x)[1], 2)
  ans
} # rowRanges_R()

validateIndicesTest <- function(x, rows, cols, mode, expect) {
  if (!missing(mode)) {
    storage.mode(rows) <- mode
    storage.mode(cols) <- mode
  }

  actual <- tryCatch(rowRanges(x,rows,cols), error=function(c) "error")
  if (missing(expect)) {
    expect <- tryCatch(rowRanges_R(x[rows,cols]), error=function(c) "error")
  }
  cat(sprintf("rows=%s, cols=%s\n", toString(rows), toString(cols)))
  cat(sprintf("actual=%s\nexpect=%s\n", toString(actual), toString(expect)))

  stopifnot(all.equal(actual, expect))
}


indicesCases <- list()
# mixed positive and negative indices
indicesCases[[length(indicesCases)+1]] <- 1:-1
indicesCases[[length(indicesCases)+1]] <- -1:1

# midex positive, negative and zero indices
indicesCases[[length(indicesCases)+1]] <- c(1, 0, 0, -6)
indicesCases[[length(indicesCases)+1]] <- c(-4, 0, 0, 1)

# negative indices with duplicates
indicesCases[[length(indicesCases)+1]] <- c(-4, 0, 0, -3, -1, -3, -1)

# positive indices
indicesCases[[length(indicesCases)+1]] <- c(3, 5, 1)

# positive indices with duplicates
indicesCases[[length(indicesCases)+1]] <- c(3, 5, 1, 5, 5)

# positive indices out of ranges
#indicesCases[[length(indicesCases)+1]] <- 4:9

# negative out of ranges: just ignore
indicesCases[[length(indicesCases)+1]] <- c(-5, 0, -3, -1)

# negative indices exclude all
indicesCases[[length(indicesCases)+1]] <- -1:-5

# idxs is single  number
indicesCases[[length(indicesCases)+1]] <- 4
indicesCases[[length(indicesCases)+1]] <- -4
indicesCases[[length(indicesCases)+1]] <- 0

# idxs is empty
#indicesCases[[length(indicesCases)+1]] <- integer(0)

# NA in idxs
indicesCases[[length(indicesCases)+1]] <- c(NA, -2)
indicesCases[[length(indicesCases)+1]] <- c(NA, 0, 2)

# idxs is single NA
indicesCases[[length(indicesCases)+1]] <- NA

# single TRUE
indicesCases[[length(indicesCases)+1]] <- TRUE
indicesCases[[length(indicesCases)+1]] <- FALSE

# full logical idxs
indicesCases[[length(indicesCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE)

# too many logical idxs
indicesCases[[length(indicesCases)+1]] <- c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)

# insufficient idxs
indicesCases[[length(indicesCases)+1]] <- c(FALSE, TRUE, TRUE)


for (rows in indicesCases) {
  for (cols in indicesCases) {
    validateIndicesTest(x, rows, cols)
  }
}

# positive indices out of ranges
#rows <- 4:9
#cols <- 4:9
#actual <- rowRanges(x, rows, cols)
#rows <- rows[rows>dim(x)[1]] <- NA
#cols <- cols[cols>dim(x)[2]] <- NA
#expect <- rowRanges_R(x[rows,cols])
