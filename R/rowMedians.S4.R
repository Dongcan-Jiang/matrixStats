###########################################################################/**
# @RdocFunction rowMedians
# @alias colMedians
# \alias{rowMedians,matrix-method}
# \alias{colMedians,matrix-method}
#
# @title "Calculates the median for each row (column) in a matrix"
#
# \description{
#   @get "title".
# }
#
# \usage{
#  @usage rowMedians
#  @usage colMedians
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
#   Returns a @numeric @vector of length N (K).
# }
#
# \details{
#   The implementation of \code{rowMedians()} and \code{colMedians()}
#   is optimized for both speed and memory.
#   To avoid coercing to @doubles (and hence memory allocation), there
#   is a special implementation for @integer matrices.
#   That is, if \code{x} is an @integer @matrix, then
#   \code{rowMedians(as.double(x))} (\code{rowMedians(as.double(x))})
#   would require three times the memory of \code{rowMedians(x)}
#   (\code{colMedians(x)}), but all this is avoided.
# }
#
# @author "HB, HJ"
#
# \seealso{
#   See @see "rowMedians" and \code{colMedians()} for weighted medians.
#   For mean estimates, see \code{rowMeans()} in @see "base::colSums".
# }
#
# @keyword array
# @keyword iteration
# @keyword robust
# @keyword univar
#*/###########################################################################
setGeneric("rowMedians", function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  standardGeneric("rowMedians");
})

setMethod("rowMedians", signature(x="matrix"), function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm);
  hasNAs <- TRUE;  # Add as an argument? /2007-08-24

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
        rowMedians(x, rows=subRows, cols=cols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop("error")

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("rowMedians", x, dim., rows, cols, na.rm, hasNAs, TRUE, PACKAGE="matrixStats");
})


setGeneric("colMedians", function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  standardGeneric("colMedians");
})

setMethod("colMedians", signature(x="matrix"), function(x, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(x), mc.cores=1L, ...) {
  dim. <- as.integer(dim.)
  na.rm <- as.logical(na.rm);
  hasNAs <- TRUE;  # Add as an argument? /2007-08-24

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
        colMedians(x, rows=rows, cols=subCols, na.rm=na.rm, dim.=dim., mc.cores=1L, ...)
      }, mc.cores=mc.cores), warning=function(w) hasWarning <<- TRUE)

      # warning means error
      if (hasWarning) stop("error")

      y <- Reduce(c, y)
      return(y)
    }
  }

  .Call("rowMedians", x, dim., rows, cols, na.rm, hasNAs, FALSE, PACKAGE="matrixStats");
})


############################################################################
# HISTORY:
# 2015-07-18 [DJ]
# o Supported multicore processing.
# 2015-05-28 [DJ]
# o Supported subsetted computation.
# 2011-10-13 [HJ]
# o In the past, colMedians(x) was accomplished as rowMedians(t(x));
#   it is now done directly.
# 2008-03-25
# o Added colMedians() - a wrapper around rowMedians() for now.
# o Turned into a S4 method as it used to be in Biobase.
# 2007-08-14
# o Added argument 'hasNA'.
# 2005-11-25
# o Created.
############################################################################
