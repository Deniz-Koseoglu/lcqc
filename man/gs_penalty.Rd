% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icf_helpers.R
\name{gs_penalty}
\alias{gs_penalty}
\alias{egh_penalty}
\alias{emg_penalty}
\alias{etg_penalty}
\title{RMSE calculation for iterative curve fitting optimization}
\usage{
gs_penalty(pars, input, vars = c("x", "y"))

egh_penalty(pars, input, vars = c("x", "y"))

emg_penalty(pars, input, vars = c("x", "y"), repr = "emg1")

etg_penalty(pars, input, vars = c("x", "y"), infs)
}
\arguments{
\item{pars}{A \code{numeric} vector of parameters to pass to curve fitting function.}

\item{input}{A \code{data.frame} containing \strong{x} and \strong{y} data from which to model the curve.}

\item{vars}{A \code{character} vector of length 2 specifying the column names of \strong{x} and \strong{y} variables in \code{input}.
Defaults to \code{c("x","y")}.}

\item{repr}{Some models may be written/represented in more than one way mathematically (some are more stable than others for optimization).
Current representations include: EMG (\code{"emg1"} or \code{"emg2"}).}

\item{infs}{\strong{Exclusive to \code{\link{etg_penalty}}}. A \code{numeric} vector of inflection point times, \strong{alternating}
between \strong{left} (leading) and \strong{right} (trailing) inflection points.}
}
\value{
The \code{numeric} Root Mean Squared Error (RMSE) of the model derived using chosen \code{pars}.
}
\description{
\strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.

The Root Mean Squared Error (RMSE) calculation function to use for iterative optimization of various Iterative Curve Fitting (ICF) models.
Currently available models include the simple Gaussian (GS), Expontentially-Modified Gaussian (EMG), Exponential-Gaussian Hybrid (EGH),
and Empricially Transformed Gaussian (ETG). See \code{\link{chrom_icf}} for further details.
}
\seealso{
\code{\link{chrom_icf}}, \code{\link{icf_GS}}, \code{\link{icf_EGH}}, \code{\link{icf_EMG}}, \code{\link{icf_ETG}}
}
\keyword{internal}
