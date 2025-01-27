#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Finding local maxima (peaks) and minima (valleys)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect maxima in vector
#'
#' @description Detects maximum values (peaks) in a series of numbers.
#'
#' @param x Numeric vector in which to detect maxima.
#' @param m The number of data points on either side (i.e. neighbourhood of points) used to validate maxima and minima.
#'
#' @return Position(s) of maxima in vector.
#' @export
#'
#' @details The function may also be used to detect minima by simple passing the inverse of a vector, i.e. \code{-x}.
#'
#' @examples
#' vec <- c(1,2,3,2,1,1,2,1)
#' find_peaks(vec, m = 3)
#' find_peaks(-vec, m = 3)
find_peaks <- function (x, m = 3){

  if(!is.numeric(x)) {
    stop("The input data must be a numeric vector!")
  }

  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
