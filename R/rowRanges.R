###########################################################################/**
# @RdocFunction rowRanges
# @alias colRanges
# @alias rowMins
# @alias rowMaxs
# @alias colMins
# @alias colMaxs
#
# @title "Gets the range of values in each row (column) of a matrix"
#
# \description{
#   @get "title".
# }
#
# \usage{
#   @usage rowRanges
#   @usage colRanges
#   @usage rowMins
#   @usage colMins
#   @usage rowMaxs
#   @usage colMaxs
# }
#
# \arguments{
#  \item{x}{A @numeric NxK @matrix.}
#  \item{rows, cols}{A @vector indicating subset of rows (and/or columns)
#     to operate over. If @NULL, no subsetting is done.}
#  \item{na.rm}{If @TRUE, @NAs are excluded first, otherwise not.}
#  \item{dim.}{An @integer @vector of length two specifying the
#              dimension of \code{x}, also when not a @matrix.}
#  \item{mc.cores}{The number of cores to use, i.e. at most how many child
#     processes will be run simultaneously. No effect on Windows.}
#  \item{...}{Not used.}
# }
#
# \value{
#   \code{rowRanges()} (\code{colRanges()}) returns a
#   @numeric Nx2 (Kx2) @matrix, where
#   N (K) is the number of rows (columns) for which the ranges are
#   calculated.
#
#   \code{rowMins()/rowMaxs()} (\code{colMins()/colMaxs()}) returns a
#   @numeric @vector of length N (K).
# }
#
# @author "HB"
#
# \seealso{
#   @see "rowOrderStats" and @see "base::pmin.int".
# }
#
# @keyword array
# @keyword iteration
# @keyword robust
# @keyword univar
#*/###########################################################################
rowRanges <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    nrow <- nrow(x)
    # Recalculate how many rows are there
    if (!is.null(rows)) {
      rows <- validateIndices(rows, nrow, allowOutOfBound=FALSE)
      nrow <- length(rows)
    }

    if (nrow > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > nrow) mc.cores <- nrow
      ranges <- .splitIndexRanges(nrow, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate rows from ranges
        if (is.null(rows)) subRows <- range[1]:range[2]
        else subRows <- rows[range[1]:range[2]]
        # Call itself to run on one core
        rowRanges(x, rows=subRows, cols=cols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(rbind, y)
      return(y)
    }
  }

  .Call("rowRanges", x, dim., rows, cols, 2L, na.rm, TRUE, PACKAGE="matrixStats")
}

rowMins <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    nrow <- nrow(x)
    # Recalculate how many rows are there
    if (!is.null(rows)) {
      rows <- validateIndices(rows, nrow, allowOutOfBound=FALSE)
      nrow <- length(rows)
    }

    if (nrow > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > nrow) mc.cores <- nrow
      ranges <- .splitIndexRanges(nrow, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate rows from ranges
        if (is.null(rows)) subRows <- range[1]:range[2]
        else subRows <- rows[range[1]:range[2]]
        # Call itself to run on one core
        rowMins(x, rows=subRows, cols=cols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("rowRanges", x, dim., rows, cols, 0L, na.rm, TRUE, PACKAGE="matrixStats")
}

rowMaxs <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    nrow <- nrow(x)
    # Recalculate how many rows are there
    if (!is.null(rows)) {
      rows <- validateIndices(rows, nrow, allowOutOfBound=FALSE)
      nrow <- length(rows)
    }

    if (nrow > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > nrow) mc.cores <- nrow
      ranges <- .splitIndexRanges(nrow, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate rows from ranges
        if (is.null(rows)) subRows <- range[1]:range[2]
        else subRows <- rows[range[1]:range[2]]
        # Call itself to run on one core
        rowMaxs(x, rows=subRows, cols=cols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("rowRanges", x, dim., rows, cols, 1L, na.rm, TRUE, PACKAGE="matrixStats")
}


colRanges <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    ncol<- ncol(x)
    # Recalculate how many cols are there
    if (!is.null(cols)) {
      cols <- validateIndices(cols, ncol, allowOutOfBound=FALSE)
      ncol<- length(cols)
    }

    if (ncol > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > ncol) mc.cores <- ncol
      ranges <- .splitIndexRanges(ncol, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate cols from ranges
        if (is.null(cols)) subCols <- range[1]:range[2]
        else subCols <- cols[range[1]:range[2]]
        # Call itself to run on one core
        colRanges(x, rows=rows, cols=subCols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(rbind, y)
      return(y)
    }
  }

  .Call("colRanges", x, dim., rows, cols, 2L, na.rm, TRUE, PACKAGE="matrixStats")
}

colMins <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    ncol<- ncol(x)
    # Recalculate how many cols are there
    if (!is.null(cols)) {
      cols <- validateIndices(cols, ncol, allowOutOfBound=FALSE)
      ncol<- length(cols)
    }

    if (ncol > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > ncol) mc.cores <- ncol
      ranges <- .splitIndexRanges(ncol, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate cols from ranges
        if (is.null(cols)) subCols <- range[1]:range[2]
        else subCols <- cols[range[1]:range[2]]
        # Call itself to run on one core
        colMins(x, rows=rows, cols=subCols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("colRanges", x, dim., rows, cols, 0L, na.rm, TRUE, PACKAGE="matrixStats")
}

colMaxs <- function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm)

  # Multicore processing
  mc.cores <- as.integer(mc.cores)
  if (mc.cores > 1L && .Platform$OS.type != "windows") {
    ncol<- ncol(x)
    # Recalculate how many cols are there
    if (!is.null(cols)) {
      cols <- validateIndices(cols, ncol, allowOutOfBound=FALSE)
      ncol<- length(cols)
    }

    if (ncol > 1L) {
      # Calculate how many cores are actually needed
      if (mc.cores > ncol) mc.cores <- ncol
      ranges <- .splitIndexRanges(ncol, mc.cores)

      hasWarning <- FALSE
      y <- withCallingHandlers(mclapply(ranges, FUN=function(range) {
        # Generate cols from ranges
        if (is.null(cols)) subCols <- range[1]:range[2]
        else subCols <- cols[range[1]:range[2]]
        # Call itself to run on one core
        colMaxs(x, rows=rows, cols=subCols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop(.extractErrorMessage(y))

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("colRanges", x, dim., rows, cols, 1L, na.rm, TRUE, PACKAGE="matrixStats")
}


############################################################################
# HISTORY:
# 2015-07-17 [DJ]
# o Supported multicore processing.
# 2015-05-25 [DJ]
# o Supported subsetted computation.
# 2014-12-17 [HB]
# o CLEANUP: Made col- and rowRanges() plain R functions.
# 2014-11-16
# o SPEEDUP: Implemented in native code.
# 2013-07-28
# o SPEEDUP: Made (col|row)Mins() and (col|row)Maxs() faster.
# o BUG FIX: rowRanges(x) on an Nx0 matrix 'x' would give an error.
#   Ditto for colRanges(x).
# 2009-02-01
# o BUG FIX: colRanges(x) would give an error if nrow(x) == 0.
# 2008-03-25
# o Since colOrderStats() cannot handle missing values we use the slower
#   colRanges() for the case when na.rm=TRUE.
# o Added {row|col}{Min|Max}s().
# o Created {row|col}Ranges() for scratch. Handles NAs.
############################################################################
