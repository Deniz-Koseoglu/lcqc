% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peakfind_helpers.R
\name{markcross_chk}
\alias{markcross_chk}
\title{Check whether peak markers cross (when they are not supposed to)}
\usage{
markcross_chk(this_mark, next_mark, this_lim, next_lim)
}
\arguments{
\item{this_mark}{An \code{numeric} peak marker index to check against \code{next_mark} for crossing.}

\item{next_mark}{A \code{numeric} peak marker index to compare to \code{this_mark}.}

\item{this_lim}{The peak apex or another peak marker corresponding to \code{this_mark}. Thus, \code{this_mark} should be closer to \code{this_lim}
than \code{next_lim}.}

\item{next_lim}{Similarly to \code{this_lim}, represents a peak marker corresponding to \code{next_mark}.}
}
\value{
A \code{numeric} vector of \code{c(this_mark, next_mark)} where wrong markers (if any) are replaced with \code{NA}.
}
\description{
\strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.

The function checks whether peak markers cross each other when they are not supposed to. For example, if an inflection point \code{this_mark}
is farther away from its corresponding peak apex \code{this_lim} and closer to the adjacent peak \code{next_lim}, there is incorrect crossing
and inflection point detection should be revised (or removed).
}
\seealso{
\code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{det_infs}}
}
\keyword{internal}
