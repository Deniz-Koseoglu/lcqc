#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Amplitude Limit determination via multiple methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find amplitude limit for peak detection
#'
#' @description Detects a suitable signal amplitude limit for peak detection using a simple quantile, relative differences between quantiles, or z-scores (see \code{\link{z_thres}}).
#'
#' @param x Numeric vector of signal from which to derive an amplitude limit.
#' @param method One of: \code{"quant"} (simple quantiles), \code{"diff"} (relative differences between quantiles), or \code{"zscore"} (z-scores).
#' @param pars Parameters required for calculation depending on the chosen \code{method}:
#' \describe{
#'  \item{For \code{"quant"}}{The quantile probability to use as the amplitude limit. Given as a single \code{numeric} percentage between 0.001 and 50.}
#'  \item{For \code{"diff"}}{The percentage (between 0.001 and 50) of maximum difference between quantiles (calculated via a probability \code{seq(0, 1, 0.005)}) used to locate the amplitude limit.}
#'  \item{For \code{"zscore"}}{A numeric vector of 3 parameters:
#'  the \code{integer} number of observations used to calculate moving average and standard deviation (\code{lag} in \code{\link{z_thres}}),
#'  the \code{numeric} threshold (as a factor of moving standard deviation) at which to signal the presence of a peak (\code{threshold} in \code{\link{z_thres}}),
#'  and \code{numeric} sensitivity of the amplitude limit to the average standard deviation throughout the signal (see \strong{Details}).}
#' }
#'
#' @details When \code{method} is \code{"quant"}, the amplitude limit is simply determined as the quantile at the specified probability percentage between 0.001 and 50.
#'
#' Method \code{"diff"} first calculates the quantile range of input vector \code{x} using \code{quantile(x, probs = c(0, 1, 0.005))},
#' derives the maximum difference between successive quantiles (via \code{max(diff())}),
#' locates the earliest (lowest) quantile where a percentage of this difference between 0.001 and 50 (given in the \code{pars} argument) is exceeded,
#' and uses this quantile as the amplitude limit.
#'
#' Finally, \code{method = "zscore"} calculates \strong{global mean values} of both the moving average \eqn{\overline{MX}} and the moving standard deviation \eqn{\overline{SD}}
#' of the signal (derived via \code{z_thres}) and uses a sensitivity parameter \eqn{sens} (provided in \code{pars[3]}) to calculate the amplitude limit as follows:
#' \deqn{AmpLim = \overline{MX} + (1/sens)\times\overline{SD}}
#' Thus, \eqn{AmpLim} decreases with increasing \eqn{sens} (\code{pars[3]}).
#'
#' @return A single \code{numeric} value of the amplitude limit
#' @export
#'
#' @examples
#' sig <- lcqc::simlc1[,"Signal"]
#' lim1 <- chrom_amplim(sig, method = "quant", pars = 0.05)
#' lim2 <- chrom_amplim(sig, method = "diff", pars = 0.05)
#' lim3 <- chrom_amplim(sig, method = "zscore", pars = c(30, 5, 2))
#' lim3_alt <- chrom_amplim(sig, method = "zscore", pars = c(30, 5, 10))
#'
#' @seealso \code{\link{z_thres}}
#'
#' @importFrom stats quantile
chrom_amplim <- function(x, method = "diff", pars = 1) {

  #Preliminary checks
  if(!is.atomic(x) | !is.numeric(x)) {
    stop("A numeric vector must be provided as input!")
  }

  if(!any(c("diff", "quant", "zscore") %in% method)) {
    stop("Method must be one of: 'diff', 'quant', or 'zscore'!")
  }

  if(method=="zscore" & length(pars)!=3) {
    stop("When 'zscore' method is chosen, a vector of 3 numeric parameters (lag, threshold, and sensitivity) must be provided!")
  } else if(any(c("quant", "diff") %in% method)) {
    parlims <- c(0.001,50) #if(method=="quant") c(0.01, 1.00) else c(1, 50)
    if(length(pars)!=1 | !is.numeric(pars) | pars < parlims[1] | pars > parlims[2]) {
      stop(paste0("The 'pars' argument must be a single numeric value between ", parlims[1], " and ", parlims[2], " when 'method' is '", method, "'!"))
    }
  } else if(method=="zscore" & any(pars < 0)) stop("All parameters ('pars') must be positive values when the 'zscore' method is used!")

  if(method=="quant") {
    amplim <- quantile(x, probs = pars/100, na.rm = TRUE)
  } else if(method=="diff") {
    amp_quants <- quantile(x, probs = seq(0, 1, 0.005), na.rm = TRUE)
    diffs <- c(0, diff(amp_quants))
    difrac <- max(diffs)*(pars/100)
    amplim <- diffs[min(which(diffs >= difrac), na.rm = TRUE)]
  } else if(method=="zscore" & length(pars)==3 & is.numeric(pars)) {
    zscore_res <- z_thres(x, pars[1], pars[2], 0)
    amplim <- mean(zscore_res[["avgFilter"]], na.rm = TRUE) + (1/pars[3])*mean(zscore_res[["stdFilter"]], na.rm = TRUE)
  }
  return(amplim)
}
