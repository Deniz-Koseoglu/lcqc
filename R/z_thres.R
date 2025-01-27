#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Thresholding algorithm based on z-scores
#(see: https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/22640362#22640362)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Thresholding based on z-scores
#'
#' @description Simple, robust peak picking algorithm based on calculating and thresholding z-scores.
#'
#' @importFrom stats sd
#'
#' @param y Numeric vector of equi-spaced y-values. Length needs to be \code{>=lag+2}.
#' @param lag Number of observations to base average and standard deviation on (e.g. 5).
#' @param threshold The z-score (factor to multiply standard deviation of the moving average by) at which the algorithm signals the presence of positive or negative peaks (e.g. 3.5).
#' @param influence Factor between 0 and 1 denoting the relative influence/weight that new data points have on the calculation of moving average and moving standard deviation (e.g. 0.5).
#'
#' @return A list of 4 elements:
#' \describe{
#'  \item{\code{signals}}{Vector of length equal to \code{y} denoting presence or absence of peaks with three possible values: \code{0} (no peak), \code{1} (positive peak), \code{-1} (negative peak).}
#'  \item{\code{avgFilter}}{Vector of length equal to \code{y} containing moving averages.}
#'  \item{\code{stdFilter}}{Vector of length equal to \code{y} containing moving standard deviations.}
#'  \item{\code{params}}{A vector of length 3 containing the used \code{lag}, \code{threshold}, and \code{influence} parameters passed to the function.}
#' }
#' @export
#'
#' @references Brakel, J.P.G. van (2014). "Robust peak detection algorithm using z-scores". Stack Overflow. Available at: \url{https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data/22640362#22640362} (version: 2020-11-08).
#'
#' @examples
#' z_thres(lcqc::exlc1[,"Signal"], lag = 13, threshold = 4, influence = 0)
#'
#' @seealso \code{\link{z_optim}}
z_thres <- function(y,lag,threshold,influence) {

  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] <- sd(y[0:lag], na.rm=TRUE)
  for (i in (lag+1):length(y)){

    if (!is.na(y[i]) & !is.na(avgFilter[i-1]) & !is.na(stdFilter[i-1]) & abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] <- sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter, params = c(lag = lag, threshold = threshold, influence = influence)))
}
