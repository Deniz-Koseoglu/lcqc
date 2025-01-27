#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate chromatogram baseline using the FastChrom method (Johnsen et al., 2013)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate baselines of resolved peaks using the FastChrom algorithm
#'
#' @description The function calculates baselines of well-resolved peaks and peak groups via the FastChrom algorithm of Johnsen et al. (2013).
#'
#' @param inds An \strong{optional} \code{numeric} vector of data indices equal in length to \code{sig}. Equal to \code{seq(length(sig))} by default.
#' @param sig Chromatographic signal data as a \code{numeric} vector.
#' @param starts,ends A \code{numeric} vector of peak \strong{start} and \strong{end} indices.
#' @param crit_w The critical width parameter. Usually equal to the minimum peak width at half height (see \strong{Details}).
#' @param for_plot A \code{logical} toggle that determines whether the output data should be formatted for plotting (e.g. via \code{\link{chrom_plot}}).
#'
#' @return The output changes depending on whether \code{for_plot} is \code{TRUE}. If so, a \code{list} of length 3 is returned containing \code{numeric}
#' vectors of the \code{Original_Signal}, \code{Corrected_Signal}, and \code{Baseline}. Otherwise, the same data alongside data indices is returned as a
#' 4-column \code{data.frame}.
#'
#' @export
#'
#' @details
#' Iteratively derives baselines for all resolved regions based on the FastChrom algorithm proposed by Johnsen et al. (2013).
#' First, all peak regions (e.g. determined by \code{\link{chrom_detect}}) are separated and a linear interpolated is carried out between their starting
#' and ending points to derive initial baselines.
#' Each baseline is then checked and, if necessary, adjusted iteratively as follows: if a \strong{consecutive} number of baseline points
#' equal to or greater than the critical width (\code{crit_w}) of the baseline are \strong{above the signal}, the baseline is forced through the
#' \strong{single lowest} point relative to the baseline and re-interpolated. This is repeated until there are no baseline points above the signal.
#'
#' @examples
#' sigvec <- lcqc::simlc1[,"Signal"]
#' strvec <- c(486, 763, 916, 1745, 2428)
#' endvec <- c(707, 897, 1050, 1946, 2588)
#' crw <- 10
#'
#' #Data formatted simply
#' res1 <- fastchrom_bline(sig = sigvec, starts = strvec, ends = endvec, crit_w = crw,
#' for_plot = FALSE)
#'
#' #Data formatted for plotting
#' res2 <- fastchrom_bline(sig = sigvec, starts = strvec, ends = endvec, crit_w = crw, for_plot = TRUE)
#'
#' #Create plot
#' plot(res2[["Original_Signal"]])
#' lines(res2[["Baseline"]], col = "red")
#'
#' @references Johnsen, L.G., Skov, T., Houlberg, U., Bro, R. (2013), 'An automated method for baseline correction, peak finding and peak grouping in chromatographic data', \emph{Analyst} \strong{138} (12), pp. 3502-3511, DOI: \url{https://www.doi.org/10.1039/C3AN36276K}
#'
#' @seealso \code{\link{dtprep}}
fastchrom_bline <- function(inds = NA, sig, starts, ends, crit_w = 1, for_plot = FALSE) {
  #Preliminary checks
  if(!all(sapply(list(sig, starts, ends), is.atomic)) |
     !all(sapply(list(sig, starts, ends), is.numeric))) stop("All inputs must be numeric vectors!")
  if(length(starts)!=length(ends)) stop("Peak region 'starts' and 'ends' must be of equal length!")
  if(length(crit_w)!=1 | !is.numeric(crit_w) | !is.atomic(crit_w)) stop("The critical width 'crit_w' must be a single numeric value!")
  if(any(c(starts,ends) > length(sig)) | any(c(starts,ends) <= 0)) stop("All indices given in 'starts' and 'ends' must be within the length of 'sig'!")
  if(!is.na(inds) & length(inds)!=length(sig)) stop("When custom indices are provided, their length must be equal to that of 'sig'!")
  crit_w <- unname(crit_w)

  #Filter out start-end pairs of fused peaks (ONLY APPLICABLE WHEN MORE THAN ONE PEAK IS PRESENT)
  if(all(lengths(list(starts,ends))>1)) {
    i <- 1
    repeat {
      if(ends[i]==starts[i+1]) {
        ends[i] <- ends[i+1]
        starts <- starts[-c(i+1)]
        ends <- ends[-c(i+1)]
        i <- i-1
      }
      if(i==length(starts)-1) break else i <- i+1
    }
  }

  #Define the peak region
  peak_reg <- lapply(seq_along(starts), function(x) starts[x]:ends[x])

  #Separate peak and non-peak regions
  #The baseline for non-peak regions is equal to 0, thus the original data remains
  if(is.na(inds)) inds <- 1:length(sig)
  df <- cbind.data.frame(ind = inds, orig_y = sig, bline = sig, y = rep(0,length(inds)))
  df <- df[-unlist(peak_reg),]

  #Calculate baselines to peak regions using a loop
  for(i in seq_along(peak_reg)) {
    #Use approx() to linearly interpolate between the first and last point of the peak region
    pk_x <- inds[peak_reg[[i]]]
    pk_y <- sig[peak_reg[[i]]]
    pk_bls <- approx(x = pk_x[c(1,length(pk_x))], y = pk_y[c(1,length(pk_y))], method = "linear", xout = pk_x)[["y"]]

    #Check if a consecutive number of points equal to or higher than crit_w are below the original linear baseline (i.e. INTERSECT the baseline)
    pts_rle <- rle(pk_y < pk_bls)
    blwchk <- which(pts_rle[["values"]] & pts_rle[["lengths"]] >= crit_w)

    #If yes, force the baseline through the SINGLE POINT of the raw data that is LOWEST RELATIVE to the baseline
    #Repeat the procedure until NO RAW DATA POINTS LIE BELOW THE BASELINE
    lpt_vec <- c()
    while(length(blwchk)>0) {
      #min_blw <- peaks[[i]][which(pk_y < pk_bls),]
      #min_blw <- min_blw[which.min(min_blw[,"y"]),]
      #min_x <- min_blw[,"ind"]
      #min_y <- min_blw[,"y"]
      min_ind <- which(pk_y-pk_bls == min(pk_y-pk_bls) & (!pk_y %in% lpt_vec))
      lpt_vec <- if(length(lpt_vec)==0) min_ind else append(lpt_vec, min_ind)
      pk_bls <- approx(x = pk_x[unique(sort(c(1, lpt_vec, length(pk_x))))],
                       y = pk_y[unique(sort(c(1, lpt_vec, length(pk_y))))],
                       method = "linear", xout = pk_x)[["y"]]
      pts_rle <- rle(pk_y < pk_bls)
      blwchk <- which(pts_rle[["values"]] & pts_rle[["lengths"]] >= crit_w)
    }
    df <- rbind.data.frame(df, do.call(cbind.data.frame, list(ind = pk_x, orig_y = pk_y, bline = pk_bls, y = pk_y-pk_bls)))
  }

  #Compile and return data
  if(for_plot) {
    finres <- setNames(as.list(df[,c("orig_y","y","bline")]), c("Original_Signal","Corrected_Signal","Baseline"))
  } else finres <- df[order(df[,"ind"]),]
  return(finres)
}
