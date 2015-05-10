############################################################################/**
# @RdocFunction validateIndices
#
# @title "Validate indices"
#
# \description{
#   Computes validated positive indices from given indices.
# }
#
# \arguments{
#   \item{idxs}{A @integer @vector. If @NULL, all indices are considered.}
#   \item{maxIdx}{The possible max index.}
# }
#
# \value{
#   Returns a validated integers list indicating the indices.
# }
#
# @examples "../incl/validateIndices.Rex"
#*/############################################################################
validateIndices <- function(idxs=NULL, maxIdx, allowOutOfBound=TRUE) {
  y <- .Call('validate', idxs, maxIdx, allowOutOfBound, PACKAGE='matrixStats')
  if (is.null(y)) y <- 1:maxIdx
  y
}
