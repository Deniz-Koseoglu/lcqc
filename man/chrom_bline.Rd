% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseline_correction.R
\name{chrom_bline}
\alias{chrom_bline}
\title{Baseline correction using various methods}
\usage{
chrom_bline(
  input,
  method = "als",
  pars = "default",
  plotres = "print",
  asprat = 0.71,
  rm_neg = TRUE,
  slnt = FALSE
)
}
\arguments{
\item{input}{Input numeric vector containing signal to be baseline-corrected.}

\item{method}{Baseline correction method. One of: \code{"als"}, \code{"chang"}, \code{"poly"}, \code{"isrea"}, or \code{"none"} (for convenience when incorporating function into flexible workflows).}

\item{pars}{A \strong{named} vector of parameters specific to each baseline correction \code{method}. Applies default parameters when set to \code{"default"} - these are listed in \strong{Details}.}

\item{plotres}{Should a summary plot of baseline correction be created and/or printed? One of: \code{"none"}, \code{"plot"}, or \code{"print"}.}

\item{asprat}{Numeric value of the summary plot aspect ratio (defaults to \code{0.71}).}

\item{rm_neg}{A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).}

\item{slnt}{A \code{TRUE/FALSE logical}. Displays information about progress and results when \code{FALSE} (default).}
}
\value{
A list containing usual baseline correction results (see \code{\link{bline_poly}}, for example) and, when \code{plotres!="none"}, a \code{Bline_Plot} element containing a summary plot.
}
\description{
This is the main baseline correction function able to apply several methods, such as Asymmetric Least Squares (\code{\link{bline_als}}),
Modified Polynomial Fit (\code{\link{bline_poly}}), Chang's method (\code{\link{bline_chang}}), and ISREA (\code{\link{bline_isrea}}). Optionally, the results may be summarised visually using \code{\link{bline_plot}}.
}
\details{
This function incorporates all 4 approaches to baseline correction included in \pkg{lcqc}, each requiring unique parameters to be passed to its respective function.
These can be provided in argument \code{pars} as a vector, i.e. \code{c()}, or simply set to \code{"default"} to use sensible defaults. These are listed below:
\describe{
\item{For ALS (function \code{\link{bline_als}})}{\code{c(lambda = 6, p = 0.001, prec = 1e-08, maxit = 50)}}
\item{For Chang (function \code{\link{bline_chang}})}{\code{c(threshold = 0.5, alpha = 0.95, bfrac = 0.2, segments = 100, sig_window = 10, fit = "linear")}}
\item{For ModPolyFit (function \code{\link{bline_poly}})}{\code{c(deg = 4, prec = 0.001, maxit = 100)}}
\item{For ISREA (function \code{\link{bline_isrea}})}{\code{c(eta = 10, maxit = 100)}}
}
}
\examples{
chrom_bline(lcqc::exgc1[,"Signal"], method = "als", plotres = "plot")

}
\seealso{
\code{\link{bline_als}}, \code{\link{bline_chang}}, \code{\link{bline_poly}}, \code{\link{bline_isrea}}, \code{\link{bline_plot}}
}
