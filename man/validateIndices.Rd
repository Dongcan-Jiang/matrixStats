%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  validateIndices.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{validateIndices}
\alias{validateIndices}


\title{Validate indices}

\description{
  Computes validated positive indices from given indices.
}

\arguments{
  \item{idxs}{A \code{\link[base]{integer}} \code{\link[base]{vector}}. If \code{\link[base]{NULL}}, all indices are considered.}
  \item{maxIdx}{The possible max index.}
}

\value{
  Returns a validated integers list indicating the indices.
}

\examples{
idxs <- validateIndices(c(-4, 0, -3, -1), 5)     # [2,5]

idxs <- validateIndices(c(4, 4, 8, 2, 3), 8)     # [4, 4, 8, 2, 3]


}

