% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{insect}
\alias{insect}
\title{Find intersection between two lines}
\usage{
insect(l1, l2)
}
\arguments{
\item{l1}{Line 1 characterized by slope and intercept. A numeric vector of length 2 in the format: c(intercept, slope).}

\item{l2}{Line 2 characterized by slope and intercept. Format is identical to that of \code{l1}.}
}
\value{
A numeric vector with \code{c(x,y)} coordinates of the intersection point (if any).
}
\description{
Locates the intersection point of two lines characterized by a slope and intercept.
}
\examples{
#Define two lines by slope and intercept
#Format: c(intercept, slope)
l1 <- c(10, -2)
l2 <- c(0, 2)
isect <- insect(l1, l2)
}
\seealso{
\code{\link{slope}}, \code{\link{intercept}}
}
