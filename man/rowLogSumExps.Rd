%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  rowLogSumExps.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{rowLogSumExps}
\alias{rowLogSumExps}

\alias{colLogSumExps}
\alias{rowLogSumExps,matrix-method}
\alias{colLogSumExps,matrix-method}

\title{Accurately computes the logarithm of the sum of exponentials across rows or columns}

\description{
  Accurately computes the logarithm of the sum of exponentials across rows or columns.
}

\usage{
 rowLogSumExps(lx, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(lx), mc.cores=1L, ...)
 colLogSumExps(lx, rows=NULL, cols=NULL, na.rm=FALSE, dim.=dim(lx), mc.cores=1L, ...)
}

\arguments{
 \item{lx}{A \code{\link[base]{numeric}} NxK \code{\link[base]{matrix}}.
  Typically \code{lx} are \eqn{log(x)} values.}
 \item{rows, cols}{A \code{\link[base]{vector}} indicating subset of rows (and/or columns)
  to operate over. If \code{\link[base]{NULL}}, no subsetting is done.}
 \item{na.rm}{If \code{\link[base:logical]{TRUE}}, any missing values are ignored, otherwise not.}
 \item{dim.}{An \code{\link[base]{integer}} \code{\link[base]{vector}} of length two specifying the
              dimension of \code{x}, also when not a \code{\link[base]{matrix}}.}
 \item{mc.cores}{The number of cores to use, i.e. at most how many child
  threads will be run simultaneously.}
 \item{...}{Not used.}
}

\value{
 A \code{\link[base]{numeric}} \code{\link[base]{vector}} of length N (K).
}

\section{Benchmarking}{
  These methods are implemented in native code and have been optimized
  for speed and memory.
}

\author{
  Native implementation by Henrik Bengtsson.
  Original R code by Nakayama ??? (Japan).
}

\seealso{
  To calculate the same on vectors, \code{\link{logSumExp}}().
}


\keyword{array}
