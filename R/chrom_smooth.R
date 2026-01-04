#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Smoothing signals or derivatives
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Chromatogram smoothing
#'
#' @description Smoothes an equi-spaced signal using various common approaches, including rectangular, triangular, and Savitsky-Golay (S-G) smoothing.
#'
#' @param x Input numeric vector of equi-spaced values.
#' @param method Smoothing method. One of: \code{"rect"} (rectangular), \code{"tri"} (triangular), \code{"sg_quad"} (S-G quadratic), or \code{"sg_quart"} (S-G quartic, see \strong{Details}).
#' @param pts Number of points to use for smoothing (defaults to \code{3}).
#' @param passes Number of smoothing passes to apply (defaults to \code{3}).
#'
#' @return The smoothed signal (as a numeric vector).
#' @export
#'
#' @details
#' The function includes several smoothing approaches. The simplest \code{method}, \code{"rect"}, is a \emph{rectangular boxcar} algoithm,
#' which replaces each points with the \code{\link[base]{mean}} of several neighboring points. For example, for a 3-point smooth:
#' \deqn{S_j=\frac{Y_{j-1}+Y_j+Y_{j+1}}{3}}
#' Triangular smoothing (\code{"tri"}) implements a weighted algorithm. For example, a 5-point smooth becomes:
#' \deqn{S_j=\frac{Y_{j-2}+2Y_{j-1}+3Y_j+2Y_{j+1}+Y_{j+2}}{9}}
#' Finally, the Savitsky-Golay (S-G) smoothing methods, also known as digital smoothing polynomial filters, implement moving average-based
#' smoothing via a quadratic (\code{"sg_quad"}) or quartic (\code{"sg_quart"}) polynomial regression. The latter usually performs better
#' with narrower peaks, but may distort wider peaks.
#'
#' @examples
#' chrom_smooth(lcqc::simlc5[,"Signal"], method = "tri")
#'
#' @references O'Haver, T. (2023), \emph{A Pragmatic Introduction to Signal Processing with Applications in Scientific Measurement}, independently published, Maryland, USA, available at: \url{https://terpconnect.umd.edu/~toh/spectrum/} (last accessed 16.04.2024).
#'
#' Isnanto, R. Rizal (2011), 'Comparison of Several Smoothing Methods in Nonparametric Regression', \emph{Jurnal Sistem Komputer} \strong{1} (1), pp. 41-47.
#'
#' Savitsky, A., Golay, J.E. (1964), 'Smoothing and Differentiation of Data by Simplified Least Squares Procedures', \emph{Analytical Chemistry} \strong{36} (8), pp. 1627-1639, DOI: \url{https://doi.org/10.1021/ac60214a047}.
#'
#'@seealso \code{\link{chrom_width}}
#'
#' @importFrom stats convolve
chrom_smooth <- function(x, method = "rect", pts = 3, passes = 3) {

  if(method=="none") {
    cat("\nNo smoothing was carried out since 'method' is set to 'none'...")
    return(x)
  }

  #Preliminary checks
  if(!is.atomic(x) | !is.numeric(x)) {
    stop("A numeric vector must be provided for smoothing!")
  }

  if(!any(c("rect", "tri", "sg_quad", "sg_quart") %in% method)) {
    stop("Wrong smoothing method provided! Possible values are: 'rect', 'tri', 'sg_quad', or 'sg_quart'!")
  }

  if(is.even(pts)) {
    cat("\nSmoothing window must be an odd integer! Changing to the nearest odd number...")
    pts <- pts+1
  }

  if(pts <= 1) stop("Smoothing window must be an odd integer greater than 1!")

  #Create vector of NA to store smoothed values
  ssig <- rep(NA, length(x))

  #Carry out smoothing
  pts <- (pts-1)/2

  #Only for RECTANGULAR or TRIANGULAR Moving Average (MA)
  pts_seq <- seq(-pts, pts, 1)

  coeff_s1 <- seq(1, length(seq(0, pts, 1)), 1)
  coeff_seq <- c(coeff_s1, rev(coeff_s1)[-1])

  #Loop through the number of passes required
  for(i in 1:passes) {

    offset_min <- ifelse(i==1, min(which(!is.na(x))), min(which(!is.na(ssig))))
    offset_max <- ifelse(i==1, max(which(!is.na(x))), max(which(!is.na(ssig))))

    if(any(c("sg_quad", "sg_quart") %in% method)) {

      #Savitsky-Golay (SG) WITHOUT DERIVATIVES - i.e. the derivative order is zero!

      smooth_input <- if(i==1) x[offset_min:offset_max] else ssig[offset_min:offset_max]

      ford <- if(method == "sg_quad") 2 else 4
      out <- moorepen(outer(-pts:pts, 0:ford, FUN = "^"))
      out <- convolve(smooth_input, rev(out[(0 + 1), ]), type = "o")
      out <- out[(pts + 1):(length(out) - pts)]
      ssig <- (-1)^0 * out
      ssig <- c(rep(NA, offset_min-1), ssig, rep(NA, length(x)-offset_max))

    } else if(any(c("rect", "tri") %in% method)) {

      #RECTANGULAR or TRIANGULAR Moving Average (MA)

      offset_min <- offset_min + pts
      offset_max <- offset_max - pts

      smooth_input <- if(i==1) x else ssig
      ssig[c(1:offset_min, offset_max:length(ssig))] <- NA

      if(method=="rect") {

        for(j in offset_min:offset_max) {
          ssig[j] <- sum(sapply(pts_seq, function(y) smooth_input[j+y]), na.rm = TRUE)/(2*pts+1)
        }

      } else if(method=="tri") {

        for(j in offset_min:offset_max) {
          ssig[j] <- sum(sapply(seq_along(coeff_seq), function(y) coeff_seq[y]*smooth_input[j+pts_seq[y]]), na.rm = TRUE)/sum(coeff_seq, na.rm = TRUE)
        }
      }
    }
  }
  return(ssig)
}
