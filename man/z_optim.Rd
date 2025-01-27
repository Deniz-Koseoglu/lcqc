% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_optim.R
\name{z_optim}
\alias{z_optim}
\title{Optimization of z-score thresholding algorithm}
\usage{
z_optim(
  x,
  lag_rng = seq(0.01, 0.05, 0.005),
  thres_rng = seq(1, 6, 0.5),
  peak_thres = 0.02,
  noise_thres = 0.005,
  mode = "percent"
)
}
\arguments{
\item{x}{Input numeric vector of equi-spaced signal values.}

\item{lag_rng}{Sequence of lag values to test, as outlined in \code{\link{z_thres}}.}

\item{thres_rng}{Sequence of threshold values to test, as outlined in \code{\link{z_thres}}.}

\item{peak_thres}{Threshold value. Minimum number (or fraction) of consecutive points in \code{x} required to initially accept a peak.}

\item{noise_thres}{Threshold value. Minimum number (or fraction) of consecutive points in \code{x} required to keep a peak in the final stage of optimization. Non-conforming peaks (which are too narrow) are removed as noise.}

\item{mode}{One of \code{"percent"} or \code{"absolute"}. Determines whether \code{peak_thres} and \code{noise_thres} are interpreted as fractions of total points in \code{x}, or as absolute values.}
}
\value{
A list of length two containing the following elements:
\describe{
\item{\code{Peak_Limits}}{A 2-column \code{data.frame} containing the start and end indices of detected peaks along \code{x}.}
\item{\code{Best_ZScore_Run}}{The results of the "best" z-score thresholding run, formatted as per the \code{\link{z_thres}} function.}
}
}
\description{
The function tests a specific range of input values for the \pkg{lcqc} z-scores peak picking algorithm (see \code{\link{z_thres}}) using a numeric vector.
The combination of parameters resulting in the highest number of legitimate peaks (along a numeric vector \code{x}) at the lowest values of \code{lag} and \code{threshold} is selected.
These parameters are returned alongside the start and end indices of corresponding detected peak regions.
}
\examples{
yvals <- lcqc::simlc1[,"Signal"]
z_optim(yvals)

}
\seealso{
\code{\link{z_thres}}
}
