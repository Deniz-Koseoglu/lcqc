#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate accurate inflection points from 'chrom_detect' approximation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Interpolate accurate inflection points
#'
#' @description Interpolates the exact xy-coordinates (i.e. retention time and signal) of inflection point from \code{\link{chrom_detect}} approximation
#' by surveying a neighbourhood of nearby points.
#'
#' @param inds Optional \code{numeric} vector of data indices (by default, the sequence along retention time data is used, i.e. \code{seq_along(xvals)}).
#' @param xvals Retention time (x-axis) data as a \code{numeric} vector.
#' @param yvals Signal (y-axis) data as a \code{numeric} vector.
#' @param sd Second derivative of \code{yvals} as a \code{numeric} vector.
#' @param linfs,rinfs Indices of \strong{left} (\code{linfs}) and \strong{right} (\code{rinfs}) inflection points (e.g. approximated by \code{\link{chrom_detect}}).
#'
#' @return A \code{list} of two \code{data.frame} objects containing \code{left} and \code{right} inflection point coordinates (\code{"acc_x"}
#' and \code{acc_y}) as well as the data indices between which these are situated (\code{"low"} and \code{"high"}).
#' @export
#'
#' @examples
#' #Get retention time and signal data
#' rtvec <- lcqc::simlc1[,"Time"]
#' sigvec <- lcqc::simlc1[,"Signal"]
#'
#' #Get second derivatives
#' sdvec <- chrom_deriv(rtvec,sigvec)[[2]]
#'
#' #Set locations of left and right inflection points
#' lvec <- c(527,601,621,810,974,1822,2483)
#' rvec <- c(546,611,638,828,994,1850,2522)
#'
#' acc_inf(xvals = rtvec, yvals = sigvec, sd = sdvec, linfs = lvec, rinfs = rvec)
#'
#' @seealso \code{\link{acc_max}}, \code{\link{dtprep}}
acc_inf <- function(inds = seq_along(xvals), xvals, yvals, sd, linfs, rinfs) {

  if(length(linfs) != length(rinfs)) {
    stop("Left and right inflection point index vectors must be of equal length!")
  }

  #Create data.frame
  input <- cbind.data.frame(ind = inds, x = xvals, y = yvals, sd = sd)

  #Create a list of inflection points
  stinfs <- c(linfs, rinfs)

  #Retrieve Z-point (Z=9 by default) neighborhoods around the inflection point indices
  infs <- lapply(stinfs, function(x) if(is.na(x) | is.nan(x)) NA else input[input[,"ind"] %in% (x-4):(x+4),])

  linf_nbr <- infs[1:length(linfs)]
  rinf_nbr <- infs[(length(linfs)+1):length(infs)]

  #Interpolate pairs of left and right inflection points to find retention times (choose one closest to the starting point)
  #Make sure distinct left and right inflection points are found at this stage. If unavailable, revert to original inflection points
  xouts <- setNames(rep(list(NULL),2), c("left", "right"))

  for(i in seq_along(linf_nbr)) {
    lres <- rres <- c()
    if(!all(is.na(linf_nbr[[i]]))) {
      lres <- linspline(linf_nbr[[i]][,"x"], linf_nbr[[i]][,"sd"], y0 = 0, verbose = FALSE)
      #Exception: If no accurate inflection point was found, default to the original point
      if(length(lres)==0) lres <- linf_nbr[[i]][linf_nbr[[i]][,"ind"]==linfs[i], "x"]
      linfdiff <- xvals[inds == linfs[i]]-lres
    }

    if(!all(is.na(rinf_nbr[[i]]))) {
      rres <- linspline(rinf_nbr[[i]][,"x"], rinf_nbr[[i]][,"sd"], y0 = 0, verbose = FALSE)
      #Exception: If no accurate inflection point was found, default to the original point
      if(length(rres)==0) rres <- rinf_nbr[[i]][rinf_nbr[[i]][,"ind"]==rinfs[i], "x"]
    }

    reslst <- list(lres = lres, rres = rres)
    which_long <- which.max(lengths(reslst))
    which_overlap <- which(reslst[[which_long]] %in% reslst[-which_long])
    if(length(which_overlap)>0) reslst[[which_long]] <- reslst[[which_long]][-which_overlap]  #Remove any repeating inflection point times (occurs when the existing points are close together)
    lres <- reslst[["lres"]]
    rres <- reslst[["rres"]]

    if(length(lres)>0) {
      lres <- lres[which.min(abs(xvals[inds == linfs[i]]-lres))]
    } else lres <- NA

    if(length(rres)>0) {
      rres <- rres[which.min(abs(xvals[inds == rinfs[i]]-rres))]
    } else rres <- NA

    xouts[["left"]] <- append(xouts[["left"]], lres)
    xouts[["right"]] <- append(xouts[["right"]], rres)
  }
  xouts <- unname(unlist(xouts))

  #Use the interpolated retention times to find corrected signal values via approx()
  sigouts <- unlist(lapply(seq(length(xouts)), function(x) if(!is.data.frame(infs[[x]]) & all(is.na(infs[[x]]))) NA else approx(infs[[x]][,"x"], infs[[x]][,"y"], xout = xouts[x])[["y"]]))

  finres <- cbind.data.frame(acc_x = xouts, acc_y = sigouts)
  finres <- split(finres, c(rep(1, nrow(finres)/2), rep(2, nrow(finres)/2)))

  #Add point indices which enclose the inflection points
  inf_xs <- lapply(finres, function(x) {
    res <- sapply(x[,"acc_x"], function(y) if(is.na(y)) rep(NA,2) else c(inds[max(which(input[,"x"] < y))], inds[min(which(input[,"x"] > y))]))
    res <- as.data.frame(t(res))
    colnames(res) <- c("low", "high")
    return(res)
  })
  finres <- lapply(seq_along(finres), function(x) cbind.data.frame(finres[[x]], inf_xs[[x]]))
  names(finres) <- c("left", "right")

  #Check whether the function assigned the same 'accurate' inflection point to the left and right of any peaks
  #If true, change those points back to their initial values in the input data
  #same_infchk <- which(finres[["right"]][,"acc_y"]==finres[["left"]][,"acc_y"] & finres[["right"]][,"acc_x"]==finres[["left"]][,"acc_x"])
  #if(length(same_infchk)>0) {
  #  finres[["left"]][same_infchk,] <- c(xvals[inds %in% linfs[same_infchk]], yvals[inds %in% linfs[same_infchk]], rep(linfs[same_infchk],2))
  #  finres[["right"]][same_infchk,] <- c(xvals[inds %in% rinfs[same_infchk]], yvals[inds %in% rinfs[same_infchk]], rep(rinfs[same_infchk],2))
  #}
  return(finres)
}
