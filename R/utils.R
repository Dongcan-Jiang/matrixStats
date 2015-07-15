# Split `1:nx` into `cl` ranges
.splitIndexRanges <- function(nx, cl) {
  t <- seq(1, nx, ceiling(nx/cl))
  mapply(c, t, c(t[-1] - 1, nx), SIMPLIFY=FALSE)
}


# Extract error message from y
# Used to handle errors in multicore processing
.extractErrorMessage <- function(y) {
  str_trim(tail(strsplit(y[[1]][1], "\n")[[1]], n=1))
}


############################################################################
# HISTORY:
# 2015-07-15 [DJ]
# o Created.
############################################################################
