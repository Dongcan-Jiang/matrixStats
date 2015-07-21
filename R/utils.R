# Split `1:nx` into `cl` ranges
.splitIndexRanges <- function(nx, cl) {
  t <- seq(1, nx, ceiling(nx/cl))
  mapply(c, t, c(t[-1] - 1, nx), SIMPLIFY=FALSE, USE.NAMES=FALSE)
}


############################################################################
# HISTORY:
# 2015-07-15 [DJ]
# o Created.
############################################################################
