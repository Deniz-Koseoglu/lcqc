#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Calculation of derivatives
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate first and second derivatives
#'
#' @description Calculate first and second derivatives from \code{x} and \code{y} data.
#'
#' @param x Numeric vector of \code{x} values.
#' @param y Numeric vector of \code{y} values equal in length to that of \code{x}.
#'
#' @return A \code{list} of two numeric vectors equal to \code{x} in length, containing first (\code{[[1]]}) and second (\code{[[2]]}) derivatives.
#' @export
#'
#' @examples
#' xvals <- lcqc::simlc5[,"Time"]
#' yvals <- lcqc::simlc5[,"Signal"]
#' chrom_deriv(xvals, yvals)
chrom_deriv <- function(x, y) {
  #Preliminary checks
  if(!is.numeric(x) | !is.numeric(y)) {
    stop("X and Y input vectors must be numeric!")
  }

  fder <- sder <- rep(NA, length(y))

  i_start <- which(!is.na(y))[1]
  i_end <- max(which(!is.na(y)))

  for(i in (i_start+1):(i_end-1)) {
    fder[i] <- (y[i+1]-y[i-1])/(x[i+1]-x[i-1])
  }

  for(i in (i_start+2):(i_end-2)) {
    sder[i] <- (fder[i+1]-fder[i-1])/(x[i+1]-x[i-1])
  }
  return(list(fder, sder))
}
