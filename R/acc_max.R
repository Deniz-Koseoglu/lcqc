#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate accurate peak retention times and apices from 'chrom_detect' approximation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Obtain accurate peak apex data via quadratic fitting
#'
#' @description Evaluates the shape of peak tops and, if appropriate, quadratically fits a parabola to derive accurate retention time (and signal) data.
#'
#' @param inds Optional \code{numeric} vector of data indices (by default, the sequence along retention time data is used, i.e. \code{seq_along(xvals)}).
#' @param xvals Retention time (x-axis) data as a \code{numeric} vector.
#' @param yvals Signal (y-axis) data as a \code{numeric} vector.
#' @param maxes Indices of peak maxima (apices) as a \code{numeric} vector.
#' @param linfs,rinfs Indices of \strong{left} (\code{linfs}) and \strong{right} (\code{rinfs}) inflection points (e.g. approximated by \code{\link{chrom_detect}}).
#' @param ptypes A \code{character} vector of peak apex types. There are 4 possible values: baseline-resolved (\code{"B"}), fused (\code{"F"}),
#' shoulder (\code{"S"}), and round (\code{"R"}) peaks.
#'
#' @return A 3-column \code{data.frame} containing the accurate xy-coordinates and the utilized method for each peak (one of \code{"5_point"},
#' \code{"3_point"}, or \code{"no_fit"}).
#' @export
#'
#' @details
#' The function tests various conditions to pick the appropriate method of estimating accurate peak retention time (and signal) among a
#' 5-point quadratic fit or a 3-point quadratic fit. No fit is carried out for shoulder and round peaks, or those where at least one
#' inflection point is above the maximum in terms of signal. For others, provided the inflection point width is >5 points, a 5-point
#' quadratic fit is attempted. Else, or if the estimated retention time is outside that of the 5 points used for the fit, a 3-point fit is
#' instead attempted. If the estimated retention time is again outside that of the 3 points used for the fit, no fit is carried out.
#'
#' @examples
#' #Get retention time and signal data
#' rtvec <- lcqc::simlc1[,"Time"]
#' sigvec <- lcqc::simlc1[,"Signal"]
#'
#' #Set locations of left and right inflection points as well as apices
#' lvec <- c(527,601,621,810,974,1822,2483)
#' rvec <- c(546,611,638,828,994,1850,2522)
#' mvec <- c(536,608,628,819,983,1834,2499)
#' ptype <- c("F","S","F","B","B","B","B")
#'
#' #Get accurate maxima
#' res <- acc_max(xvals = rtvec, yvals = sigvec, maxes = mvec,
#' linfs = lvec, rinfs = rvec, ptypes = ptype)
#'
#' @seealso \code{\link{dtprep}}, \code{\link{acc_inf}}
#'
#' @importFrom stats spline
acc_max <- function(inds = seq_along(xvals), xvals, yvals, maxes, linfs, rinfs, ptypes) {

  if(length(linfs) != length(rinfs)) {
    stop("Left and right inflection point index vectors must be of equal length!")
  }

  if(length(unique(lengths(list(maxes, linfs, rinfs, ptypes))))!=1) {
    stop("Lengths of 'maxes', 'linfs', 'rinfs', and 'ptypes' must be equal!")
  }

  #Create output data.frame
  acc_out <- data.frame(matrix(NA, ncol = 3, nrow = length(maxes)))
  colnames(acc_out) <- c("acc_x", "acc_y", "maxfit")

  #Match indices
  matchind <- lapply(list(maxes, linfs, rinfs), function(x) which(inds %in% x))
  maxes <- matchind[[1]]
  linfs <- matchind[[2]]
  rinfs <- matchind[[3]]

  for(i in seq_along(maxes)) {

    #Is either boundary of the peak(s) "Round"?
    cond1 <- ptypes[i] %in% c("R","S")

    #Are any inflection points above the corresponding apex?
    cond2 <-  if(!cond1 & !any(is.na(c(linfs[i],rinfs[i])))) any(yvals[c(linfs[i], rinfs[i])] > yvals[maxes[i]]) else TRUE

    #Based on cond1 and cond2, select appropriate apex estimation procedure
    if(any(c(cond1,cond2))) {

      #If the peak is either a "Round" or "Shoulder", or an inflection point signal is higher than the preliminary apex signal
      #Assign the SD minimum (or Signal Maximum) as the true peak apex
      acc_out[i,c("acc_x","acc_y")] <- c(xvals[maxes[i]], yvals[maxes[i]])
      acc_out[i,"maxfit"] <- "no_fit"

    } else if(!any(c(cond1,cond2))) {

      #Else, apply either a 5-point or 3-point parabolic spline fit to the top 5 (or 3) points by signal/height
      #Check that the inflection point width is >=5 points
      cond3 <- abs(linfs[i] - rinfs[i]) >= 5

      #If cond3 is TRUE, retrieve top 5 points by baseline-corrected signal
      if(cond3) {

        top_pts <- sort(yvals[linfs[i]:rinfs[i]], decreasing = TRUE)[1:5]
        top_inds <- which(yvals %in% top_pts)
        cond4 <- any(top_inds %in% maxes[i])

        top_pts <- cbind.data.frame(x = xvals[top_inds], y = yvals[top_inds])

        #Fit 5-point spline
        spline_5 <- spline(top_pts, method = "fmm", n = 100)
        maxind_5 <- which.max(spline_5[["y"]])
        true_top <- sapply(spline_5, function(x) x[maxind_5])

        #Check that the x-value of the spline maximum lies within the limits of the top 5 points
        cond5a <- true_top["x"] >= min(top_pts[,"x"], na.rm = TRUE) & true_top["x"] <= max(top_pts[,"x"], na.rm = TRUE)

        #If cond5a is FALSE, attempt to fit a 3-point spline and re-assess
        if(!cond5a) {

          #Find top 3 points
          top_3 <- top_pts[order(top_pts[,"y"], decreasing = TRUE),][1:3,]
          top_3 <- top_3[order(top_3[,"x"]),]

          #Fit 3-point spline
          spline_3 <- spline(top_3, method = "fmm", n = 100)
          maxind_3 <- which.max(spline_3[["y"]])
          true_top <- sapply(spline_3, function(x) x[maxind_3])

          #Check that the x-value of the spline maximum lies within the limits of the top 3 points
          cond5b <- true_top["acc_x"] >= min(top_3[,"x"], na.rm = TRUE) & true_top["x"] <= max(top_3[,"x"], na.rm = TRUE)

        } else cond5b <- FALSE

        #Finally, assign the true maximum to each peak
        if(cond4 & (cond5a | cond5b)) {

          acc_out[i,c("acc_x", "acc_y")] <- true_top
          acc_out[i,"maxfit"] <- if(cond5a) "5_point" else if(cond5b) "3_point"

        } else {
          acc_out[i,c("acc_x","acc_y")] <- c(xvals[maxes[i]], yvals[maxes[i]])
          acc_out[i,"maxfit"] <- "no_fit"
        }

      } else {
        acc_out[i,c("acc_x", "acc_y")] <- c(xvals[maxes[i]], yvals[maxes[i]])
        acc_out[i,"maxfit"] <- "no_fit"
      }
    }
  }
  return(acc_out)
}
