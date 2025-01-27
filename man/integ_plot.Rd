% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skim_helpers.R
\name{integ_plot}
\alias{integ_plot}
\title{Visualize traditional integration results}
\usage{
integ_plot(
  input,
  cols = "default",
  plabs = "default",
  norm = FALSE,
  plot_max = FALSE,
  txt_max = FALSE,
  asprat = 0.71
)
}
\arguments{
\item{input}{The \code{list} of data output from \code{\link{chrom_skim}}.}

\item{cols}{Plot colour palette. Either \code{"default"} or a \strong{named} \code{character} vector of colours.
Names denote the following plot elements: main chromatogram (\code{"main"}), peak group baselines (\code{"grp"}),
perpendicular droplines (\code{"pd"}), tangent skim lines (\code{"ts"}), exponential skim lines (\code{"es"}),
and Gaussian skim lines (\code{"gs"}).}

\item{plabs}{Plot labels. Either \code{"default"} or a \strong{named} \code{character} vector of labels for the following elements:
main plot title (\code{"main"}), retention time (\code{"x"}), and signal (\code{"y"}).}

\item{norm}{A \code{logical} specifying whether to normalize the chromatographic signal (y-axis) to a percentage scale (0 to 100).
Defaults to \code{FALSE}.}

\item{plot_max}{A \code{logical} switch specifying whether to show peak apices with shape markers. Defaults to \code{FALSE}.}

\item{txt_max}{A \code{logical} switch specifying whether to plot peak numbers at apices. Defaults to \code{FALSE}.}

\item{asprat}{Aspect ratio of the plot (defaults to \code{0.71}).}
}
\value{
A \code{ggplot}-class object containing the plot.
}
\description{
Visualizes the results of traditional integration of data obtained from \code{\link{chrom_skim}}, including the peak apices,
group baselines, as well as perpendicular drop and/or tangent/exponential/Gaussian front or tail skim lines.
}
\examples{
\dontrun{
#Get and plot data
skim_res <- lcqc:::wf_ints
integ_plot(skim_res, plot_max = TRUE)

#With y-axis normalisation to 0-100 and text peak labels
integ_plot(skim_res, plot_max = TRUE, plabs = c(main = ""), txt_max = TRUE, norm = TRUE)
}
}
\seealso{
\code{\link{chrom_skim}}
}
