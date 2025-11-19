#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#This section contains all functions necessary to execute the chrom_detect workflow
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate peak markers (minima, maxima, up- and down-crosses)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Locate initial peak markers
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Estimates peak markers from a combination of signal data as well as first and second derivatives.
#' Estimated markers include minima and maxima for signal and all derivatives. For first and second derivatives only, zero upcrosses and downcrosses are also found.
#' Both \strong{compliant} and \strong{non-compliant} markers are estimated.
#'
#' @param sig A \code{numeric} vector of signal data.
#' @param fder First derivatives corresponding to \code{sig}.
#' @param sder Second derivatives corresponding tp \code{sig}.
#' @param FD_thres A \code{numeric} vector of length 2 containing minimum and maximum \strong{first} derivative noise ranges (see \code{\link{chrom_derlims}}).
#' @param SD_thres A \code{numeric} vector of length 2 containing minimum and maximum \strong{second} derivative noise ranges (see \code{\link{chrom_derlims}}).
#' @param sig_thres A single \code{numeric} value of the signal amplitude limit used for peak detection (see \code{\link{chrom_amplim}}).
#' @param mpts Number of points surveyed on either side to confirm potential maxima and minima.
#' @param crosspts Number of points surveyed on either side to confirm zero-crossings (upcrosses and downcrosses).
#' @param sens A \code{numeric} vector of length 3 containing various sensitivity factors for fine-tuning (\code{c(0.1,1,1)} by default. Also see \strong{Details}).
#'
#' @return A \code{data.frame} containing indices of detected compliant and non-compliant (suffix \code{"_nc"}) peak markers.
#'
#' @details
#' Initially, maxima/minima and up/downcrosses are calculated taking \code{mpts} and \code{crosspts} into account, respectively.
#' From these total markers, compliant ones are extracted using several criteria. First, \strong{second derivative minima} (representing peak apices)
#' are filtered such that they are above the signal amplitude limit and below the minimum second derivative threshold multiplied by a sensitivity factor
#' (i.e. \code{> sig_thres & < sens[2]*min(SD_thres)}). Thus, a higher value of \code{sens[2]} leads to a more selective filter and decreases the number of compliant minima.
#' For detecting compliant \strong{signal} minima, only the amplitude limit is considered and \code{sens[1]} is used as the sensitivity factor.
#'
#' For maxima, a similar procedure is followed. Signal maxima are considered compliant if \code{> sig_thres & < min(SD_thres)}.
#' For first and second derivatives, this condition is revised to \code{> sens[2]*max(FD_thres) | < sens[2]*min(FD_thres)} and
#' \code{> sens[3]*max(SD_thres) | < sens[3]*min(SD_thres)}, respectively.
#'
#' For zero downcrosses and upcrosses alike, final indices are chosen such that all values within \code{±crosspts} do not constitute additional crosses,
#' while the final chosen value is closest to zero among those surveyed.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{chrom_deriv}}, \code{\link{chrom_derlims}}, \code{\link{chrom_amplim}}
#'
#' @keywords internal
peakmark <- function(sig, fder, sder, FD_thres, SD_thres, sig_thres, mpts = 3, crosspts = 2, sens = c(0.1,1,1)) {
  #Preliminary checks
  if(length(sig_thres)!=1 | !is.numeric(sig_thres)) {
    stop("The 'sig_thres' (the signal amplitude threshold) argument must be a numeric value of length 1!")
  }

  if(!all(lengths(list(FD_thres, SD_thres))==2) | !all(is.numeric(c(FD_thres, SD_thres)))) {
    stop("Both the derivative threshold arguments ('FD_thres' and 'SD_thres') must be numeric vector of length 2!")
  }

  if(!all(sapply(list(sig, fder, sder), is.numeric)) | !all(sapply(list(sig, fder, sder), is.atomic))) {
    stop("All input data vectors ('sig', 'fder', and 'sder') must be numeric!")
  }

  if(any(c(mpts, crosspts)<=0)) stop("Arguments 'mpts' and 'crosspts' must both be integers above 0!")
  if(length(sens)<3) {
    cat("\nArgument 'sens' must be a numeric vector of length 3! Duplicating the first element...")
    if(!is.numeric(sens)) stop("Argument 'sens' is not numeric! Aborting...")
    sens <- rep(sens[1],3)
  }

  #Compile necessary data
  pdata <- list(sig = sig, FD = fder, SD = sder)
  rowinds <- seq_along(sig)

  #Create output list
  reslist <- list()

  #Replace NA values
  pdata <- lapply(pdata, function(x) { x[1:min(which(!is.na(x)))-1] <- x[min(which(!is.na(x)))];
  x[max(which(!is.na(x))):length(x)] <- x[max(which(!is.na(x)))]
  return(x)})

  #Detect SD minima (those adhering to the thresholds/conditions).
  sdres <- find_peaks(-pdata[["SD"]], m = mpts)
  if(length(sdres)==0 | is.null(sdres)) {
    sdout <- NA
  } else {
    sdout <- sdres[which(sig[sdres]>sig_thres & pdata[["SD"]][sdres]<sens[2]*min(SD_thres))]
  }
  sdout <- sdout[!is.na(sdout)]

  #Calculate various maxima, minima, down- and up-crosses
  for(i in seq_along(pdata)) {

    pref <- names(pdata)[i]

    #Set up outer and inner thresholds for derivatives
    thres <- if(pref=="FD") FD_thres else if(pref=="SD") SD_thres

    #Calculate local maxima and minima
    #Minima (if SD minima, re-use previously calculated values directly)
    if(pref=="SD") {
      reslist[[paste0(pref, "_minima")]] <- sdout
    } else {
      res <- find_peaks(-pdata[[i]], m = mpts) #NOTE: An alternative to find_peaks exists in package "ggpmisc"
      if(length(res)==0 | is.null(res)) {
        reslist[[paste0(pref, "_minima")]] <- NA
      } else {
        reslist[[paste0(pref, "_minima")]] <- if(pref=="sig") res[which(pdata[[i]][res]>sens[1]*sig_thres)] else res[which(sig[res]>sig_thres & pdata[[i]][res]<sens[2]*min(thres))] #Threshold for signal minima is relaxed to 0.1*sig_thres
      }
    }

    #Maxima
    res <- find_peaks(pdata[[i]], m = mpts)
    if(length(res)==0 | is.null(res)) {
      reslist[[paste0(pref, "_maxima")]] <- NA
    } else {
      reslist[[paste0(pref, "_maxima")]] <- if(pref=="sig") res[which(pdata[[i]][res]>sig_thres & sder[res]<min(SD_thres))] else if(pref=="FD") res[which((pdata[[i]][res]>sens[2]*max(thres) | pdata[[i]][res]<sens[2]*min(thres)))] else if(pref=="SD") res[which((pdata[[i]][res]>sens[3]*max(thres) | pdata[[i]][res]<sens[3]*min(thres)))] #sig[res]>sig_thres &
    }

    #Calculate local upcrosses and downcrosses with reference to pre-calculated SD minima (ONLY FOR DERIVATIVES, NOT SIGNAL!)
    if(pref!="sig") {
      res <- c(0, diff(sign(pdata[[i]])))

      cr_pref <- c("_downcr", "_upcr")
      cr_nums <- c(-2, 2)

      for(j in seq_along(cr_pref)) {

        if(all(res==0)) {
          reslist[[paste0(pref, cr_pref[j])]] <- NA
        } else {
          crosses <- which(res==cr_nums[j])
          compchk <- which(sapply(crosses, function(x) all(res[(x+1):(x+crosspts)]==0 & res[(x-crosspts):(x-1)]==0)))
          compres <- crosses[compchk]
          compres <- sapply(compres, function(x)  { evalvec <- (x-crosspts):(x+crosspts)
          out <- which.min(abs(pdata[[i]][evalvec])) #Which is closest to zero
          out <- evalvec[out]
          return(out)})
          reslist[[paste0(pref, cr_pref[j])]] <- unique(sapply(sdout, function(x) compres[which.min(abs(x-compres))]))
        }
      }
    }
  }

  #Add non-compliant markers as well (with a static 'mpts' and 'crosspts' values of 1 and 0 respectively)
  for(pref in c("FD", "SD")) {
    #Minima and Maxima
    nc_min <- find_peaks(-pdata[[pref]], m = 1)
    nc_max <- find_peaks(pdata[[pref]], m = 1)
    reslist[[paste0(pref, "_minima_nc")]] <- nc_min[!nc_min %in% reslist[[paste0(pref, "_minima")]]]
    reslist[[paste0(pref, "_maxima_nc")]] <- nc_max[!nc_max %in% reslist[[paste0(pref, "_maxima")]]]

    #Upcrosses and downcrosses
    ncross <- c(0, diff(sign(pdata[[pref]])))
    nc_upcr <- which(ncross==2)
    nc_downcr <- which(ncross==-2)
    reslist[[paste0(pref, "_upcr_nc")]] <- nc_upcr[!nc_upcr %in% reslist[[paste0(pref, "_upcr")]]]
    reslist[[paste0(pref, "_downcr_nc")]] <- nc_downcr[!nc_downcr %in% reslist[[paste0(pref, "_downcr")]]]
  }

  #Finally, convert data to a data.frame
  for(i in seq_along(reslist)) {
    length(reslist[[i]]) <- max(lengths(reslist))
  }
  resdf <- as.data.frame(reslist)
  return(resdf)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Identify peak boundaries via ApexTrack (Empower 3 software by Waters)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Expand baseline outward using the ApexTrack algorithm
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Implements the Water Corporation ApexTrack baseline expansion algorithm to determine start and end boundaries of chromatographic peaks.
#' See \strong{Details} and \strong{References} for detailed information.
#'
#' @param end_l,end_r Vectors of \code{numeric} indices with maximum leftmost (\code{_l}) and rightmost (\code{_r}) expansion limits (as data indices).
#' @param str_l,str_r Vectors of \code{numeric} indices with leftward (\code{_l}) and rightward (\code{_r}) expansion start indices (usually peak inflection points).
#' @param rtime A \code{numeric} vector of retention time data (x-axis).
#' @param sig A \code{numeric} vector of signal data (y-axis) corresponding to \code{rtime}.
#' @param fder,sder Both \code{numeric} vectors of first (\code{fder}) and second (\code{sder}) derivatives of \code{sig}.
#' @param liftoff,touchdown Single percentages (0 to 100) used as criteria to terminate leftward (\code{liftoff}) or rightward (\code{touchdown}) baseline expansion.
#' The cornerstone of the ApexTrack algorithm. Consult \strong{References} for further details.
#' @param confirm Positive \code{integer} denoting the number of consecutive points surveyed to confirm baseline expansion termination
#' after \code{liftoff} and \code{touchdown} criteria are satisfied.
#'
#' @details
#' The ApexTrack algorithm published by Waters Corporation (updated in 2017) performs a geometric search for a baseline that best characterizes peak
#' starting and ending points. First, the inflection points of a peak apex are detected, an initial baseline is drawn between them and characterized by
#' a slope. Slope differences between this value and slopes of inflection point tangent lines are then calculated. Percentages of this difference,
#' determined by arguments \code{liftoff} and \code{touchdown} for the left and right peak boundaries, respectively, are set as the final criteria to
#' terminate baseline expansion. Once the baseline is expanded, the final slope and intercept are recorded to use for subsequent detection of crossing
#' baselines characteristic of \strong{fused peaks} (see \code{\link{find_fused}} and \code{\link{base_fused}}).
#'
#' @references Waters Corporation (2017), 'Empower Software Data Acquisition and Processing Theory Guide', document 715005481 (Rev. A), available at: \url{https://support.waters.com/KB_Inf/Empower_Breeze/WKB57375_Empower_3_-_How_to_acquire_and_process_data (accessed 19.04.2024)}.
#'
#' @return A list of length 5 with the following elements:
#' \describe{
#'  \item{initial}{A \code{data.frame} containing the starting points and maximum expansion limits as given in \code{end_l}, \code{str_l}, \code{str_r}, and \code{end_r}.}
#'  \item{starts}{A \code{numeric} vector of peak \strong{start} indices determined by ApexTrak expansion.}
#'  \item{ends}{A \code{numeric} vector of peak \strong{end} indices determined by ApexTrak expansion.}
#'  \item{slopes}{A \code{numeric} vector of final baseline slopes obtained after expansion.}
#'  \item{intercepts}{A \code{numeric} vector of y-intercepts corresponding to \code{$slopes}.}
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{base_raw}}, \code{\link{find_fused}}, \code{\link{base_fused}}
#'
#' @keywords internal
apexbnds <- function(end_l, str_l, str_r, end_r, rtime, sig, fder, sder, liftoff = 0, touchdown = 0.5, confirm = 0) {

  #Preliminary checks
  if(!is.numeric(confirm)|length(confirm)!=1|confirm<0) stop("Argument 'confirm' must be a single numeric value above or equal to zero!")
  bnds <- list(end_l = end_l, str_l = str_l, str_r = str_r, end_r = end_r)
  if(length(unique(lengths(bnds)))!=1 | any(sapply(bnds, function(x) any(is.na(x)|!is.numeric(x))))) stop("Arguments 'end_l', 'str_l', 'str_r', and 'end_r' must be numeric vectors of equal length! NA's not allowed.")
  if(!all(is.numeric(c(liftoff, touchdown))) | length(c(liftoff, touchdown)) != 2 | any(c(liftoff, touchdown) > 100) | any(c(liftoff, touchdown) < 0)) {
    stop("Parameters 'liftoff' and 'touchdown' must both be numeric values between 0 and 100!")
  }
  ulen <- unique(lengths(list(rtime, sig, fder, sder)))
  if(length(ulen)!=1) {
    stop("All input vectors ('rtime', 'sig', 'fder', 'sder') must be of equal length!")
  }

  #Obtain accurate inflection point xs and ys and save original inflection point indices
  orind_l <- str_l
  orind_r <- str_r
  infs <- acc_inf(xvals = rtime, yvals = sig, sd = sder, linfs = orind_l, rinfs = orind_r)
  rys <- infs[["right"]][,"acc_y"]
  rxs <- infs[["right"]][,"acc_x"]
  lys <- infs[["left"]][,"acc_y"]
  lxs <- infs[["left"]][,"acc_x"]

  #Obtain points that enclose the inflection points
  inf_enc <- lapply(infs, function(x) x[,c("low","high")])
  lenc <- inf_enc[["left"]]
  renc <- inf_enc[["right"]]

  #Obtain accurate first derivatives (i.e. slopes) at inflection point times
  inf_d1 <- lapply(seq_along(str_l), function(x) {
    lsub <- unlist(lenc[x,])
    rsub <- unlist(renc[x,])
    xout_l <- lxs[x]
    xout_r <- rxs[x]
    lfd <- approx(rtime[lsub], fder[lsub], xout = xout_l)[["y"]]
    rfd <- approx(rtime[rsub], fder[rsub], xout = xout_r)[["y"]]
    return(c(lfd, rfd))
  })
  inf_d1 <- setNames(Reduce(rbind.data.frame, inf_d1), c("lfd", "rfd"))
  ld1 <- inf_d1[["lfd"]] #This subsetting works with both vectors AND data.frames (retrieving elements from the former and columns from the latter)
  rd1 <- inf_d1[["rfd"]]

  #Find the slopes of baselines between inflection points
  bslopes <- (rys-lys)/(rxs-lxs)

  #Calculate expansion slope difference thresholds based on which inflection points are suitable for expansion
  loop_seq <- seq_along(bslopes)
  fthr <- tthr <- c()
  for(i in loop_seq) {
    if(ld1[i] > 0 & rd1[i] < 0) { #Left and right inflection points are both expandable
      fthr[i] <- (ld1[i] - bslopes[i]) * (liftoff/100)
      tthr[i] <- (bslopes[i] - rd1[i]) * (touchdown/100)
    } else if(ld1[i] > 0 & rd1[i] >= 0) { #Only the left inflection point is expandable
      fthr[i] <- tthr[i] <- (ld1[i] - bslopes[i]) * (liftoff/100)
    } else if(ld1[i] <= 0 & rd1[i] < 0) { #Only the right inflection point is expandable
      fthr[i] <- tthr[i] <- (bslopes[i] - rd1[i]) * (touchdown/100)
    } else fthr[i] <- tthr[i] <- NA #Neither inflection point is expandable...
  }

  #Carry out baseline expansion for each SUITABLE pair of inflection points
  starts <- ends <- slopes <- intercepts <- c()
  for(i in loop_seq) {
    #Retrieve the starting and ending points
    bex_str <- orind_l[i]
    bex_end <- orind_r[i]
    bex_lx <- lxs[i]
    bex_rx <- rxs[i]
    bex_ly <- lys[i]
    bex_ry <- rys[i]

    #Retrieve slope at starting and ending points
    bex_lfd <- fder[bex_str]
    bex_rfd <- fder[bex_end]

    #Calculate baseline slope
    bslope <- (bex_ry - bex_ly)/(bex_rx - bex_lx)
    bintcpt <- bex_ly-(bslope*bex_lx)

    #Calculate slope differences
    front_diff <- bex_lfd - bslope
    tail_diff <- bslope - bex_rfd

    #Retrieve outer limits of baseline expansion
    #IF slope (first derivative) is NA at either outer limit (due to loss of data after smoothing and other operations),
    #Select the FIRST NON-NA POINT as the new maximum expansion limit and re-assign
    fder_nachk <- which(!is.na(fder))
    bex_llim <- if(is.na(fder[end_l[i]])) min(fder_nachk) else end_l[i]
    bex_rlim <- if(is.na(fder[end_r[i]])) max(fder_nachk) else end_r[i]

    #Iteratively expand the baseline while several conditions are satisfied
    #Front and back slope differences are below their respective thresholds
    #AND the baseline has not expanded past the appropriate outer limit
    #AND the front and tail first derivatives (slopes) remain >0 and <0, respectively
    if(!any(is.na(c(fthr[i], tthr[i])))) {
      while((any(front_diff > fthr[i]) & bex_str > bex_llim & if(!all(is.na(bex_lfd))) any(bex_lfd > 0) else FALSE) |
            (any(tail_diff > tthr[i]) & bex_end < bex_rlim & if(!all(is.na(bex_rfd))) any(bex_rfd < 0) else FALSE)) {

        #Expand front
        if(any(front_diff > fthr[i]) & bex_str > bex_llim
           & if(!all(is.na(bex_lfd))) any(bex_lfd > 0) else FALSE) {
          bex_str <- bex_str - 1
        }

        #Expand tail
        if(any(tail_diff > tthr[i]) & bex_end < bex_rlim
           & if(!all(is.na(bex_rfd))) any(bex_rfd < 0) else FALSE) {
          bex_end <- bex_end + 1
        }

        #Prepare variables for updating slopes (VALUES ALSO CALCULATED FOR NEXT POINT TO AVOID SPURIOUS RESULTS)
        bex_strvec <- unique(c(bex_str, bex_str-confirm))
        bex_endvec <- unique(c(bex_end, bex_end+confirm))
        bex_lfd <- fder[bex_strvec]
        bex_rfd <- fder[bex_endvec]
        bex_ly <- sig[bex_strvec]
        bex_ry <- sig[bex_endvec]
        bex_lx <- rtime[bex_strvec]
        bex_rx <- rtime[bex_endvec]

        #Update the baseline slope
        bslope <- (bex_ry - bex_ly)/(bex_rx - bex_lx)
        bintcpt <- bex_ly-(bslope*bex_lx)

        #Update slope differences
        front_diff <- bex_lfd - bslope
        tail_diff <- bslope - bex_rfd
      }
    }

    #Add final peak start/end points and slope/intercept to results
    starts[i] <- bex_str
    ends[i] <- bex_end
    slopes[i] <- bslope
    intercepts[i] <- bintcpt
  }
  return(list(initial = as.data.frame(bnds), starts = starts, ends = ends, slopes = slopes, intercepts = intercepts))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Derive initial estimates of baseline-resolved peak boundary locations (ApexTrack)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Derive initial expanded baselines for a series of peaks via ApexTrack
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' The function first filters all second derivative minima (\code{sdmin}) which are likely to represent shoulder or round peaks.
#' These include peaks which have less than 2 proximal inflection points \strong{and} do not have corresponding signal maxima.
#' ApexTrack baseline expansion (see \code{\link{apexbnds}}) is then carried out on the remaining peaks to derive initial estimates of start and end
#' indices for baseline-resolved peaks.
#'
#' @param rtime A \code{numeric} vector of retention time (x-axis) data.
#' @param sig Signal (y-axis) data corresponding to \code{rtime}.
#' @param fder,sder First and second derivatives of \code{sig}, e.g. obtained via \code{\link{chrom_deriv}}.
#' @param infs A \code{list} of length 2 containing \strong{left} and \strong{right} inflection points as separate elements.
#' @param sdmin Indices of second derivative minima along \code{sig}.
#' @param sigmax Indices of signal maxima along \code{sig}.
#' @param fdcross,fdcross_nc Indices of compliant and non-compliant (\code{_nc}) first derivative downcrosses.
#'
#' @return Baseline-resolved peak start and end indices alongside other data structured identically to the usual output of \code{\link{apexbnds}}.
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{apexbnds}}, \code{\link{find_fused}}, \code{\link{base_fused}}
#'
#' @keywords internal
#'
#' @importFrom stats na.omit
base_raw <- function(rtime, sig, fder, sder, infs, sdmin, sigmax, fdcross, fdcross_nc) {
  #Preliminary checks
  if(length(unique(lengths(list(rtime, sig, fder, sder))))!=1) stop("Vectors 'rtime', 'sig', 'fder', and 'sder' must be of equal length!")
  if(!is.list(infs)|length(unique(lengths(infs)))!=1|!all(sapply(infs, function(x) is.numeric(x)|is.na(x)))) stop("Argument 'infs' must be a list with numeric elements of equal length!")
  if(any(c(na.omit(unlist(infs)), sdmin, sigmax, fdcross, fdcross_nc) > length(rtime))) stop("Some peak markers are out of the data range!")

  #Detect peaks which are likely shoulders or round peaks (SD minima do not have a corresponding sigmax or fdcross)
  corr_max <- lapply(list(sigmax, fdcross, fdcross_nc), function(x) corresp(sdmin, x, minsim = 3))

  #Mark pairs where either inflection point is NOT present
  inf_chk <- Reduce('|', lapply(infs, is.na))

  #Separate inflection points for the peaks
  linfs <- infs[[which.min(lapply(infs,sum,na.rm = TRUE))]]
  rinfs <- infs[[which.max(lapply(infs,sum,na.rm = TRUE))]]

  #Mark pairs where a corresponding maximum or fdcross is NOT present
  max_chk <- is.na(corr_max[[1]])|(is.na(corr_max[[1]]) & is.na(corr_max[[2]]) & is.na(corr_max[[3]]))

  #Finally, filter all inflection points and peak apices that do not fit at least one of the criteria (likely baseline-resolved and fused peaks)
  bf <- which(!(inf_chk|max_chk))

  #Carry out ApexTrack expansion on all 'bf' peaks with PRE-SET 0.0 and 0.1 liftoff and touchdown, respectively
  #Compile expansion limits
  end_l <- c(if(min(bf)==1) 1 else rinfs[min(bf)-1], rinfs[bf][-length(rinfs[bf])])
  str_l <- linfs[bf]
  str_r <- rinfs[bf]
  end_r <- c(linfs[bf][-1], if(max(bf)==length(linfs)) length(sig) else linfs[max(bf)+1])

  #Run ApexTrack (retrieves expanded peak boundaries but also slopes and intercepts of the corresponding linear baselines)
  res <- apexbnds(end_l, str_l, str_r, end_r, rtime, sig, fder, sder, liftoff = 0, touchdown = 0.1, confirm = 0)
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Detect fused boundaries from initial ApexTrack expansion results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect fused boundaries from initial ApexTrack expansion results
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' This function is intended to be used after \code{base_raw} to detect expanded baseline intersections and find fused peak boundaries
#' (only those characterised by a valley/signal minimum, not shoulder or round peaks).
#'
#' @param input The output of \code{\link{apexbnds}} containing estimates of baseline-resolved boundaries (preferably originating from \code{\link{base_raw}}).
#' @param rtime A \code{numeric} vector of retention time (x-axis) data.
#' @param sdmax Indices of second derivative maxima along \code{rtime}.
#' @param sigmin Indices of signal minima along \code{rtime}.
#'
#' @return A \code{list} of length 2 containing:
#' \describe{
#' \item{bounds}{Named \code{numeric} vector of fused peak indices.}
#' \item{peak_inds}{Peak numbers corresponding to indices in list element \code{bounds}.}
#'}
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{base_raw}}, \code{\link{apexbnds}}
#'
#' @keywords internal
find_fused <- function(input, rtime, sdmax, sigmin) {
  #Preliminary checks
  if(!is.list(input)|!all(c("initial", "starts", "ends", "slopes", "intercepts") %in% names(input))) stop("The 'input' data must originate from ApexTrack ('apexbnds' or 'base_raw' functions)!")

  #Determine which consecutive lines cross, and at which x-coordinate (retention time)
  raw_str <- input[["starts"]]
  raw_end <- input[["ends"]]
  slps <- input[["slopes"]]
  ints <- input[["intercepts"]]
  str_r <- input[["initial"]][,"str_r"]
  end_r <- input[["initial"]][,"end_r"]

  #Loop through lines to check for intersections (make sure the y-value of intersections is above zero)
  trng <- range(rtime, na.rm = TRUE)
  linecross <- sapply(seq(raw_str)[-length(raw_str)], function(x) {
    comp <- c(raw_str[x+1], raw_end[x])
    if(comp[1]<=comp[2]) {
      l1 <- c(ints[x], slps[x])
      l2 <- c(ints[x+1], slps[x+1])
      res <- insect(l1, l2)[1]
      if(res>=trng[1] & res<=trng[2] & !is.na(res)) {
        res <- which.min(abs(rtime-res))
        res <- if(res>=str_r[x] & res<=end_r[x]) res else NA
      } else res <- NA
      #} else if(abs(diff(comp))<=1) {
      ##Check if there is a signal minimum nearby
      #near_chk <- median(corresp(comp, sigmin, minsim = 1), na.rm = TRUE)
      #if(is.na(near_chk)) {
      #  #Check if there is an SD maximum instead
      #  near_chk <- median(corresp(comp, sdmax, minsim = 1), na.rm = TRUE)
      #}
      ##If still not, take the rounded median as the boundary
      #res <- if(!is.na(near_chk)) round(near_chk, 0) else round(median(comp),0)
    } else res <- NA
    names(res) <- x
    return(res)
  })
  fused_bnds <- linecross[!is.na(linecross)]
  if(length(fused_bnds)>0) {
    inds <- as.numeric(names(fused_bnds))
    names(fused_bnds) <- rep("F", length(fused_bnds))
  } else fused_bnds <- inds <- NA
  return(list(bounds = fused_bnds, peak_inds = inds))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Fuse intersecting initial ApexTrack lines to finalize baseline-resolved peak boundaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Fuse and expand intersecting baselines via ApexTrack
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' This function is designed to be applied after \code{\link{base_raw}} and \code{\link{find_fused}} to fuse intersecting initial ApexTrack
#' baselines and carry out secondary expansion (where appropriate) and derive final start/end indices of baseline-resolved peak/peak groups.
#'
#' @param fused \strong{Data} indices of fused peak boundaries
#' @param inds_fused \strong{Peak} indices of fused peaks corresponding to \code{fused} (determined by \code{\link{find_fused}}).
#' @param base_str,base_end Indices of baseline-resolved peak starts and ends as determined by \code{\link{base_raw}} or another ApexTrack-based workflow.
#' @param rtime A \code{numeric} vector of retention time (x-axis) data.
#' @param sig A \code{numeric} vector of signal (y-axis) data.
#' @param fder,sder Both \code{numeric} vectors of first (\code{fder}) and second (\code{sder}) derivatives of \code{sig}.
#' @param liftoff,touchdown Single \code{numeric} parameters of ApexTrack expansion (see \code{\link{apexbnds}}).
#'
#' @return A \code{list} with 2 elements containing the final start (element \code{starts}) and end (element \code{ends})
#' indices of baseline-resolved peaks along \code{sig}.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{apexbnds}}, \code{\link{base_raw}}, \code{\link{find_fused}}
#'
#' @keywords internal
base_fused <- function(fused, inds_fused, base_str, base_end, rtime, sig, fder, sder, liftoff, touchdown) {
  #Preliminary checks
  if(length(fused)!=length(inds_fused)|!is.numeric(inds_fused)) stop("Fused peak indices 'inds_fused' must be a numeric vector equal in length to 'fused'!")
  if(length(unique(lengths(list(rtime, sig, fder, sder))))!=1) stop("Vectors 'rtime', 'sig', 'fder', and 'sder' must be of equal length!")

  #Create copy of baseline starts and ends
  str_bkp <- cur_str <- base_str
  end_bkp <- cur_end <- base_end

  #Split into continuous chunks
  new_inds <- split(inds_fused, cumsum(c(1, diff(inds_fused) != 1))) #split(inds_fused, cummax(c(1,diff(inds_fused))))

  #Loop through the baseline groups and run ApexTrack
  endl_new <- strl_new <- strr_new <- endr_new <- c()
  for(i in seq_along(new_inds)) {
    crng <- range(new_inds[[i]], na.rm = TRUE)
    cur_str[seq(crng[1], crng[2]+1, by = 1)] <- NA #cur_str[-seq(crng[1], crng[2]+1, by = 1)]
    cur_end[seq(crng[1], crng[2]+1, by = 1)] <- NA #cur_end[-seq(crng[1], crng[2]+1, by = 1)]
    endl_new[i] <- if(crng[1]==1) 1 else end_bkp[crng[1]-1]
    strl_new[i] <- str_bkp[crng[1]]
    strr_new[i] <- end_bkp[crng[2]+1]
    endr_new[i] <- if(crng[2]==length(str_bkp)-1) length(rtime) else str_bkp[crng[2]+2]
    #Check if rightmost expansion boundaries of this peak group equal the leftmost expansion boundaries of the last peak group
    #If so, adjust the expansion limits to avoid overlapping/fusing again
    if(identical(c(endl_new[i], strl_new[i]), c(strr_new[i-1], endr_new[i-1]))) {
      closechk <- abs(strl_new[i]-endl_new[i])==1
      endr_new[i-1] <- if(closechk) strr_new[i-1] else round(median(c(strr_new[i-1], endr_new[i-1])),0)-1
      endl_new[i] <- if(closechk) strl_new[i] else round(median(c(endl_new[i], strl_new[i])),0)
    }
  }
  cur_str <- cur_str[!is.na(cur_str)]
  cur_end <- cur_end[!is.na(cur_end)]
  apex_fin <- apexbnds(endl_new, strl_new, strr_new, endr_new, rtime, sig, fder, sder, liftoff, touchdown, confirm = 0)
  cur_str <- sort(c(cur_str, apex_fin[["starts"]]))
  cur_end <- sort(c(cur_end, apex_fin[["ends"]]))
  names(cur_str) <- names(cur_end) <- rep("B", unique(length(cur_str),length(cur_end)))
  return(list(starts = cur_str, ends = cur_end))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Find shoulder and round peak boundaries (when all others are available)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect shoulder and round peak boundaries (when all others are available)
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' The function is designed to be applied after all of \code{\link{base_raw}}, \code{\link{find_fused}}, and \code{\link{base_fused}} and
#' detects shoulder and round peak boundaries.
#'
#' @param bnds \strong{Final} start and end \code{numeric} indices of baseline-resolved peaks.
#' @param infs A \code{numeric} vector of all inflection point indices.
#' @param sdmin A \code{numeric} vector of second derivative minima (representing peaks).
#' @param sdmax A \code{numeric} vector of second derivative maxima indices.
#' @param fder,sder Both \code{numeric} vectors of first (\code{fder}) and second (\code{sder}) derivatives.
#' @param fd_thres A \code{numeric} vector of first derivative peak detection thresholds (e.g. obtained via \code{\link{chrom_derlims}}).
#' @param sd_thres Second derivative thresholds similar to \code{fd_thres}.
#'
#' @return A \code{named} \code{numeric} vector of shoulder and round peak indices, named \code{"S"} and \code{"R"}, respectively.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{chrom_derlims}}
#'
#' @keywords internal
find_shld <- function(bnds, infs, sdmin, sdmax, fder, sder, fd_thres, sd_thres) {
  #Preliminary checks
  if(!all(sapply(list(bnds, infs, sdmin, sdmax, sder, fd_thres, sd_thres), is.numeric))) stop("All input data must be numeric vectors!")

  #Loop through consecutive peak pairs
  sr_bnds <- sr_nms <- c()
  for(i in seq_along(sdmin[-length(sdmin)])) {
    apex1 <- sdmin[i]
    apex2 <- sdmin[i+1]
    wrk_rng <- (apex1+1):(apex2-1)
    #Check whether there is at least 1 boundary between consecutive peaks
    if(any(bnds %in% wrk_rng)) next else {

      #Check if there are at least two inflection points
      inf_chk <- which(infs %in% wrk_rng)
      if(length(inf_chk)>1) {
        sr_nms <- append(sr_nms, "S")
        inf_rng <- sort(infs[range(inf_chk)]) #Get earlier and latest inflection points
        inf_rng <- inf_rng[1]:inf_rng[2]
        cur_bnd <- if(inf_rng[1] < min(fd_thres) & inf_rng[2] > min(fd_thres)) {
          inf_rng[which.min(fder[inf_rng] > min(fd_thres))]
        } else if(inf_rng[1] < max(fd_thres) & inf_rng[2] > max(fd_thres)) {
          inf_rng[which.max(fder[inf_rng] < max(fd_thres))]
        } else round(median(inf_rng, na.rm = TRUE),0)
        sr_bnds <- append(sr_bnds, cur_bnd)
      } else {
        sr_nms <- append(sr_nms, "R")
        #Find the most prominent SD maximum between apices (if any)
        cur_sdmax <- sdmax[which(sdmax %in% wrk_rng & sdmax < 0)]
        #Find the minimum sdmax (characteristic of round peaks)
        sr_bnds <- append(sr_bnds, if(length(cur_sdmax)>0) cur_sdmax[which.min(sder[cur_sdmax])] else round(median(wrk_rng, na.rm = TRUE),0))
      }
    }
  }
  if(length(sr_bnds)==0) sr_bnds <- NA else names(sr_bnds) <- sr_nms
  return(sr_bnds[!duplicated(sr_bnds)])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Merge data from original and smoothed derivatives and determine matches between them to remove spurious/irreproducible values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Merge peak markers from original and smoothed derivatives
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Merges the results of \code{\link{peakmark}} into a final list of peak markers based on their similarity.
#'
#' @param marks The \code{\link{peakmark}} output (i.e. compliant and non-compliant peak markers) derived from original (unsmoothed) derivatives.
#' @param smooth_marks Either \code{NA} or a \code{data.frame} output from \code{\link{peakmark}} using smoothed derivatives. When \code{NA},
#' \code{marks} are returned as a \code{list} (see \strong{Value}).
#' @param minsim The distance (in point indices) within which to look for matching markers between \code{marks} and \code{smooth_marks}.
#' Defaults to ±3 points.
#' @param max_w Additional maximum width parameter (in point indices) to use for second derivative maxima/minima when \code{minsim} fails.
#' Defaults to \code{NA}. See \strong{Details}.
#'
#' @details
#' The function tests whether a peak marker of a given type in \code{marks} has an equivalent in \code{smooth_marks} that is within ±\code{minsim}.
#' Where this is true, the rounded \code{mean} value of both markers is taken as the final value.
#' Where \code{minsim} criterion fails and the marker type is a second derivative extreme (minimum or maximum), 1/4 of \code{max_w}
#' (representing the maximum smooth width - a product of smoothing points and passes) is applied as an additional criterion.
#' If \code{max_w} is \code{NA} or <10, it is set to 10 points. Else, the presence of \strong{signal} maximum or minimum within ±\code{max_w} is checked
#' when the marker in question is a second derivative minimum or maximum, respectively.
#'
#' @return A named \code{list} where each element contains a specific type of peak marker (name suffixes \code{_minima}, \code{_maxima}, \code{_downcr},
#' \code{_upcr}) for signal, first derivatives, or second derivatives (prefixes \code{sig}, \code{FD}, or \code{SD}).
#' Non-compliant markers are additionally suffixed with \code{_nc}.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{chrom_smooth}}, \code{\link{peakmark}}, \code{\link{peakfind}}
#'
#' @keywords internal
markmerge <- function(marks, smooth_marks = NA, max_w = NA, minsim = 3) {

  #Preliminary checks
  chklist <- if(is.data.frame(smooth_marks)) list(marks, smooth_marks) else list(marks)
  if(!all(lapply(chklist, ncol)==18) | !all(sapply(chklist, function(x) colnames(x) %in% c("sig_minima", "sig_maxima", "FD_minima", "FD_maxima", "FD_downcr", "FD_upcr", "SD_minima", "SD_maxima", "SD_downcr", "SD_upcr", "FD_minima_nc", "FD_maxima_nc", "FD_upcr_nc", "FD_downcr_nc", "SD_minima_nc", "SD_maxima_nc", "SD_upcr_nc", "SD_downcr_nc")))) {
    stop("Peak marker arguments 'marks' and 'smooth_marks' must be output from function 'peakmark'!")
  }
  if(!is.data.frame(smooth_marks)) cat("\nFinal peak markers were retrieved from the 'marks' input argument alone since 'smooth_marks' was not provided!")
  if(!is.numeric(minsim) | length(minsim)!=1) stop("The minimum similary 'minsim' must be a single numeric value!")
  if(!is.na(max_w) & !is.numeric(max_w)) stop("Argument 'max_w' must either be a numeric value or NA!")

  res <- list()

  if(!is.data.frame(smooth_marks)) {
    res <- lapply(as.list(marks), function(x) x[!is.na(x)])
  } else {
    for(i in colnames(smooth_marks)) {
      res[[i]] <- sapply(seq_along(smooth_marks[,i]), function(x) { res <- abs(smooth_marks[x,i]-marks[,i])
      if(!all(is.na(res))) {
        minres <- min(res, na.rm = TRUE)
        chk <- which(res == minres)[1]
        if(minres<=minsim) {
          finres <- round(mean(c(marks[chk,i], smooth_marks[x,i]), na.rm = TRUE),0)
        } else if(any(c("SD_minima", "SD_maxima") %in% i)) {
          wd <- if(is.na(max_w) | max_w/4 <= 4) 10 else round(max_w/4,0)

          if(i == "SD_minima") {
            sdres <- abs(smooth_marks[x,i]-smooth_marks[,"sig_maxima"])
          } else if(i == "SD_maxima") {
            sdres <- abs(smooth_marks[x,i]-smooth_marks[,"sig_minima"])
          }
          if(!all(is.na(sdres))) {
            sdchk <- any(sdres < wd, na.rm = TRUE)
            if(sdchk & minres<=wd) {
              finres <- round(mean(c(marks[chk,i], smooth_marks[x,i]), na.rm = TRUE),0)
            } else return(NA)
          } else return(NA)
        } else return(NA)
      } else return(NA)
      })
      res <- lapply(res, function(x) x[!is.na(x)])
    }
  }
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Reject SD minima which don't have at least one nearby inflection point AND don't have a corresponding signal maximum (sigmax)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Reject those SD minima without proximal inflection points
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Removes SD minima which do not have any proximal inflection points, prior to peak detection.
#'
#' @param sdmin A \code{numeric} vector of SD minima indices.
#' @param sigmax A \code{numeric} vector of signal maxima indices.
#' @param input The output of \code{\link{markmerge}} containing all peak markers.
#'
#' @return A numeric vector of filtered \code{sdmin} values.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
sdmin_infchk <- function(sdmin, sigmax, input) {

  #Preliminary checks
  chkvec <- c(sdmin, sigmax)
  if(!is.numeric(chkvec) | any(is.na(chkvec))) stop("Input signal maxima 'sigmax' and SD minima 'sdmin' must be numeric vectors without NAs!")
  if(!is.list(input) | !all(c("sig_minima", "sig_maxima", "FD_minima", "FD_maxima", "FD_downcr", "FD_upcr", "SD_minima", "SD_maxima",
                              "SD_downcr", "SD_upcr", "FD_minima_nc", "FD_maxima_nc", "FD_upcr_nc", "FD_downcr_nc", "SD_minima_nc",
                              "SD_maxima_nc", "SD_upcr_nc", "SD_downcr_nc") %in% names(input))) stop("The 'input' data must be from function 'markmerge'!")
  input <- unlist(input)
  input <- input[-grep("^sig_maxima|^sig_minima|^FD_downcr|^SD_minima|^FD_minima|^FD_maxima|SD_maxima_nc", names(input))]
  if(!is.numeric(input) | !is.atomic(input) | any(is.na(input))) stop("All input data must be numeric without NAs!")
  #if(!is.numeric(minsim) | length(minsim)!=1) stop("The minimum similarity 'minsim' must be a single numeric value!")

  #Processing
  rmvec <- c()
  infchk_lax <- infchk_rigid <- smaxchk <- c()
  for(i in seq_along(sdmin)) {

    #Get right-side and left-side subsets of input data
    subs <- list(input[input<sdmin[i]], input[input>sdmin[i]])
    inf_nms <- c("SD_downcr", "SD_upcr")

    #Check for proximity of inflection points
    infchk <- sapply(seq_along(subs), function(x) {
      res <- names(subs[[x]])[which.min(abs(subs[[x]]-sdmin[i]))]
      res <- length(grep(inf_nms[x], res))>0
      return(res)
    })

    infchk_rigid <- any(infchk)
    infchk_lax <- all(infchk)

    if(!infchk_rigid) rmvec <- append(rmvec, i)

    #Check for corresponding sigmax
    #diffres <- abs(sdmin[i]-sigmax)
    #minres <- unique(min(diffres, na.rm = TRUE))
    #smaxchk <- minres<=minsim

    #if(!infchk_rigid|!any(c(infchk_lax, smaxchk))) rmvec <- append(rmvec, i)
  }

  if(length(rmvec)>0) {
    cat("\nA total of ", length(rmvec), " SD minima peak markers were removed based on absence of proximal inflection points.")
    sdmin <- sdmin[-rmvec]
  }
  return(sdmin)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Detect SD minima bunching and remove spurious SD minima
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect and mitigate bunching within second derivative minima
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Removes bunched second derivative minima prior to attempting peak detection.
#'
#' @param sdmin A \code{numeric} vector containing data indices of second derivative minima.
#' @param sigmax A \code{numeric} vector containing data indices of signal maxima.
#' @param max_w The maximum smooth width (in point indices); 1/4th of this value is used as the similarity threshold for bunching (see \strong{Details}).
#'
#' @details The function detects groups of 2 or more second derivative (SD) minima (\code{sdmin}) which are less than \code{0.25*max_w} apart.
#' Each SD minimum within these groups is tested for proximal signal maxima. If only one peak has a single proximal maximum,
#' all others within the group are removed as erroneous. If either none or more than one corresponding \code{sigmax} are present, the peak index closest
#' to the median of all those within a group is taken as the final value, and all other are removed as erroneous.
#'
#' @return A numeric vector of filtered \code{sdmin} values.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
detect_bunch <- function(sdmin, sigmax, max_w = NA) {

  #Preliminary checks
  chkvec <- c(sdmin, sigmax)
  if(!is.numeric(chkvec) | any(is.na(chkvec))) stop("Input signal maxima 'sigmax' and SD minima 'sdmin' must be numeric vectors without NAs!")

  #Processing
  rmvec <- c()
  wd <- if(is.na(max_w) | max_w/4 <= 10) 10 else round(max_w/4,0)
  sdmin_diffs <- diff(sdmin)
  which_prox <- which(sdmin_diffs < wd)

  if(length(which_prox)>0) {
    prox_grp <- split(which_prox, cumsum(c(1, diff(which_prox) != 1)))
    prox_grp <- lapply(prox_grp, function(x) append(x, max(x)+1))

    #Loop through the bunched groups
    for(i in seq_along(prox_grp)) {
      cur_bnch <- sdmin[prox_grp[[i]]]

      #Check which of the bunched sdmins have a corresponding sigmax
      bnch_sigmax <- sapply(cur_bnch, function(z) {
        diffres <- abs(z-sigmax)
        minres <- unique(min(diffres, na.rm = TRUE))
        corres <- which(diffres == minres & diffres <= 3)
        if(length(corres)>=1) round(mean(sigmax[corres], na.rm = TRUE),0) else NA
      })
      bnchcond_1 <- which(!is.na(bnch_sigmax))

      #If there is either A) No corresponding sigmax or B) More than one sigmax corresponding to the points,
      #take the median and mark the rest of the sdmins for removal.
      #Otherwise, simply select the matching sdmin and mark others for removal.
      if(length(bnchcond_1)==0 | length(bnchcond_1)>1) {
        bnch_rmark <- prox_grp[[i]][!prox_grp[[i]] %in% round(median(prox_grp[[i]], na.rm = TRUE),0)]
      } else {
        bnch_rmark <- prox_grp[[i]][!prox_grp[[i]] %in% prox_grp[[i]][bnchcond_1]]
      }
      rmvec <- append(rmvec, bnch_rmark)
    }
  }
  if(length(rmvec)>0) {
    cat("\nA total of ", length(rmvec), " SD minima peak markers were removed due to bunching.")
    sdmin <- sdmin[-rmvec]
  } else cat("\nNo bunching was detected among SD minima.")
  return(sdmin)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Filter peaks based on peak area, S/N ratio, inflection point width, and/or simple height criteria
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Filter peaks based on various chromatography-relevant criteria
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Filters detected peaks based on any combination of signal-to-noise ratio (S/N), height, as well as inflection point peak area and/or width.
#'
#' @param rtime Retention time \code{numeric} vector.
#' @param sig Signal \code{numeric} vector.
#' @param apices Indices of peak apices along \code{rtime} and \code{sig}.
#' @param linfs,rinfs Indices of left and right inflection points.
#' @param crit A \strong{named} \code{numeric} vector of criteria to filter by, including: height (\code{ht}), inflection point width (\code{wd}),
#' inflection point area (\code{pa}), and/or S/N ratio (\code{sn}).
#' @param noise The noise component of the S/N ratio, calculated by the \code{\link{noise_calc}} function.
#' @param logic The logic used to combine different criteria. One of: \code{"OR"} or \code{"AND"}.
#'
#' @return A list of length 2:
#' \describe{
#' \item{results}{Vector of \code{logical} values equal in length to \code{apices}. \code{TRUE} marks peaks for removal.}
#' \item{info}{Information about the distribution of filtered peaks based on all relevant criteria.}
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
rej_filter <- function(rtime = NA, sig, apices, linfs = NA, rinfs = NA, crit = c(ht = NA, wd = NA, pa = NA, sn = NA), noise = NA, logic = "OR") {

  #Preliminary checks
  if(!any(c("OR","AND") %in% logic)) stop("Argument 'logic' must be one of: 'AND', 'OR'!")
  if(all(is.na(crit))) {
    info <- c("\nNo peaks were marked for removal since no rejection criteria were provided.")
    cat(info)
    return(list(results = rep(FALSE, length(apices)), info = info))
  }
  if(all(is.na(c(linfs,rinfs))) & any(!is.na(crit[c("pa","wd")]))) stop("When peak top area 'pa' or peak inflection point width 'wd' are provided as criteria, inflection points must be included!")
  if(!is.numeric(sig) | !is.atomic(sig)) stop("The 'sig' input must be a numeric vector!")
  marklst <- Filter(Negate(anyNA), list(apices, linfs, rinfs))
  if(any(sapply(marklst, function(x) !all(is.numeric(x) & !is.na(x) & x>0 & max(x, na.rm = TRUE) <= length(sig))))) stop("Indices of peak apices and inflection points must be within the range of input 'sig' data!")
  if(length(unique(lengths(marklst)))!=1) stop("Peak marker vectors 'apices', 'linfs', and 'rinfs' must be of equal length!")
  #if(!all(sapply(seq_along(apices), function(x) linfs[x]<apices[x] & rinfs[x]>apices[x]))) stop("Left and right inflection points must occur prior to and after corresponding peak apices, respectively!")
  if(!any(c("AND", "OR") %in% logic)) stop("Argument 'logic' must be one of: 'AND', 'OR'!")

  critnm <- c("ht" = NA, "wd" = NA, "pa" = NA, "sn" = NA)
  if(all(is.na(crit))) {
    return("No peaks were marked for removal because no rejection criteria were provided.")
  } else if(!all(names(crit) %in% c("ht", "wd", "pa", "sn"))) {
    stop(paste0("Names of the rejection criteria vector must be one or more of: ", paste0("'", critnm, "'", collapse = ", "), "!"))
  } else {
    misnm <- critnm[-which(critnm %in% names(crit))]
    crit <- c(crit, misnm)
  }
  if(!is.na(crit["sn"]) & (is.na(noise) | !is.numeric(noise))) stop("A numeric 'noise' value must be provided when a S/N criterion is given!")
  if(!is.na(crit["pa"]) & (any(is.na(rtime)) | length(rtime)!=length(sig))) stop("Retention time 'rtime' must be provided when PA criterion is given!")

  #Begin processing
  #Check S/N ratio
  snchk <- if(any(names(crit) %in% "sn")) sig[apices]/noise < as.numeric(crit["sn"]) else rep(NA, length(apices))

  #Check PA (between inflection points)
  pachk <- if(any(names(crit) %in% "pa")) sapply(seq_along(apices), function(x) if(!any(is.na(c(linfs[x],rinfs[x])))) integ(cbind.data.frame(x = rtime[linfs[x]:rinfs[x]], y = sig[linfs[x]:rinfs[x]]), slnt = TRUE) < as.numeric(crit["pa"]) else NA) else rep(NA, length(apices))

  #Check peak width (between inflection points)
  wdchk <- if(any(names(crit) %in% "wd")) abs(rinfs-linfs) < as.numeric(crit["wd"]) else rep(NA, length(apices))

  #Check peak height (from 'apices')
  htchk <- if(any(names(crit) %in% "ht")) sig[apices] < as.numeric(crit["ht"]) else rep(NA, length(apices))

  #Compile info on rejected peaks
  chklist <- list(sn = snchk, pa = pachk, wd = wdchk, ht = htchk)

  #Check if there are any undue NAs and remove associated peaks
  chklist <- lapply(chklist, function(x) {
    if(!all(is.na(x)) & any(is.na(x))) x[is.na(x)] <- TRUE
    return(x)
  })

  labs_crit <- c(sn = "S/N ratio", pa = "apex peak area", wd = "inflection point width", ht = "peak height")
  info <- paste0("\nThe following peaks were rejected based on provided criteria and '", logic, "' logic:",
                 paste0(sapply(seq_along(chklist), function(x) {
                   if(all(is.na(chklist[[x]]))) "" #paste0("No peaks based on the ", labs_crit[x], " criterion (", ifelse(is.na(crit[names(chklist)[x]]), "no criterion provided", "all values NA"), ").\n")
                   else if(all(!chklist[[x]])) paste0("\nNo peaks based on the ", labs_crit[x], " criterion of ", crit[names(chklist)[x]], " (all values FALSE).")
                   else if(any(chklist[[x]])) paste0("\n", length(which(chklist[[x]])), " peaks based on ", labs_crit[x], " of ", crit[names(chklist)[x]], if(any(is.na(chklist[[x]]))) paste0(" (", length(which(is.na(chklist[[x]]))), " peaks could not be tested)") else "", ".") else ""
                 }), collapse = ""), collapse = "")
  cat(info)

  #Compile results based on 'AND', 'OR' logic
  chklist <- lapply(chklist, function(x) { x[is.na(x)] <- FALSE
  return(x)})
  rejres <- Reduce(ifelse(logic == "OR", '|', '&'), chklist)

  return(list(results = rejres, info = info))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculating the noise component of the S/N ratio
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Get noise value from chromatographic signal
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Calculates the noise component of the S/N ratio, e.g. for use in \code{\link{rej_filter}}.
#'
#' @param sig Signal \code{numeric} vector.
#' @param sder Second derivatives corresponding to \code{sig}, e.g. obtained via \code{\link{chrom_deriv}}.
#' @param sig_thres A single \code{numeric} value of the signal amplitude limit (from \code{\link{chrom_amplim}}).
#' @param sd_thres A \code{numeric} vector of length 2 containing \strong{second derivative} peak detection thresholds (from \code{\link{chrom_derlims}}).
#'
#' @return A single \code{numeric} noise value for \code{sig}.
#'
#' @keywords internal
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{rej_filter}}, \code{\link{chrom_derlims}},
#' \code{\link{chrom_deriv}}
#'
#' @importFrom stats ecdf
noise_calc <- function(sig, sder, sig_thres, sd_thres) {

  #Retrieve all "non-peak" regions based on sd thresholds and signal thresholds
  # Function ecdf computes the Empirical Cumulative Distribution Function
  nonpeak <- sig[sder > min(sd_thres) & sder < max(sd_thres) & sig < if(ecdf(sig)(sig_thres)>0.0001) sig_thres else quantile(sig, probs = 0.01, na.rm = TRUE)]
  if(all(is.na(nonpeak)) | length(nonpeak)/length(sig)*100<0.8) {
    nonpeak <- sig[sder > min(sd_thres) & sder < max(sd_thres)]
  }

  #Find local minima and maxima
  locs <- list(max = find_peaks(nonpeak, m = 0), min = find_peaks(-nonpeak, m = 0))

  #Calculate the average of differences between adjacent maxima and minima
  siglocs <- list(nonpeak[locs[["min"]]], nonpeak[locs[["max"]]])
  longchk <- which.max(lengths(siglocs))
  longloc <- siglocs[[longchk]]
  shortloc <- siglocs[[-longchk]]

  #Calculate the noise component of the S/N ratio
  snoise <- mean(sapply(seq_along(shortloc), function(x) unique(abs(shortloc[x] - longloc[which(abs(longloc-shortloc[x]) == min(abs(longloc-shortloc[x]), na.rm = TRUE))]))), na.rm = TRUE)
  return(snoise)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Detect the inflection points of a peak
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect closest inflection point either side of a peak
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Detects a single closest inflection point either side of a peak apex.
#'
#' @param peak_val The \code{numeric} index of a peak apex.
#' @param outlim The limiting index for detection. Inflection points are detected within \code{sort(peak_val:outlim)}.
#' @param sdcrosses,sdcrosses_nc Compliant and (optionally) non-compliant \code{numeric} indices of second derivative upcrosses and downcrosses.
#' @param which_side Which side of the peak to use for inflection point search? One of: \code{"left"} or \code{"right"}.
#'
#' @return A single \code{numeric} inflection point or \code{NA} if such a point is not found.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{det_infs}}
#'
#' @keywords internal
raw_infs <- function(peak_val, outlim = NA, sdcrosses, sdcrosses_nc, which_side) {

  #Preliminary checks
  if(!is.numeric(peak_val) | length(peak_val)!=1) stop("The peak marker 'peak_val' must be a single numeric value!")

  if(!is.na(outlim)) {
    if(!is.numeric(outlim) | length(outlim)!=1) {
      stop("The search limit 'outlim' must be a single numeric value or NA!")
    } else if(outlim < peak_val & which_side=="right") {
      stop("Where provided, 'outlim' must occur later than 'peak_val' if 'which_side'=='right'!")
    } else if(outlim > peak_val & which_side=="left") stop("Where provided, 'outlim' must occur earlier than 'peak_val' if 'which_side'=='left'!")
  }

  chkvec <- c(peak_val, sdcrosses)
  if(any(is.na(chkvec)) | !is.numeric(chkvec)) stop("All input data must be numeric and without NAs!")
  if(!any(c("left","right") %in% which_side)) stop("Argument 'which_side' must be one of: 'left', 'right'!")

  #Detect left (or right) inflection point
  #If a COMPLIANT inflection point isn't found, try the NON-COMPLIANTS
  op <- if(which_side=="left") c("<",">") else c(">","<")
  inf <- if(!any(is.na(outlim))) sdcrosses[get(op[1])(sdcrosses, peak_val) & get(op[2])(sdcrosses, outlim)] else sdcrosses[get(op[1])(sdcrosses, peak_val)]
  if(length(inf)==0) inf <- if(!any(is.na(outlim))) sdcrosses_nc[get(op[1])(sdcrosses_nc, peak_val) & get(op[2])(sdcrosses_nc, outlim)] else sdcrosses_nc[get(op[1])(sdcrosses_nc, peak_val)]
  inf <- if(length(inf)==0) NA else inf[which.min(abs(peak_val-inf))]
  return(inf)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Detect inflection points and check for crosses for a series of SD minima
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect closest inflection points for one or more peak apices
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' An extension of \code{\link{raw_infs}} used to detect both left and right inflection points for one or more peaks.
#'
#' @param sig Signal \code{numeric} vector.
#' @param sdmin Indices of second derivative minima along \code{sig}.
#' @param cross,upcross Indices of second derivative downcrosses and upcrosses along \code{sig}.
#' @param cross_nc,upcross_nc \strong{Non-compliant} equivalents to \code{cross} and \code{upcross}.
#'
#' @return A named \code{list} containing \code{left} and \code{right} inflection points for each \code{sdmin}.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{raw_infs}}, \code{\link{markcross_chk}}
#'
#' @keywords internal
det_infs <- function(sig, sdmin, cross, upcross, cross_nc = NA, upcross_nc = NA) {
  linfs <- rinfs <- c()
  for(i in seq_along(sdmin)) {
    llim <- if(i==1) 1 else sdmin[i-1]
    rlim <- if(i==length(sdmin)) length(sig) else sdmin[i+1]
    linfs[i] <- raw_infs(sdmin[i], llim, cross, cross_nc, "left")
    rinfs[i] <- raw_infs(sdmin[i], rlim, upcross, upcross_nc, "right")
    #Check for crosses
    if(i>1 & !any(is.na(c(rinfs[i-1], linfs[i])))) {
      infcross <- markcross_chk(rinfs[i-1], linfs[i], sdmin[i-1], sdmin[i])
      rinfs[i-1] <- infcross[1]
      linfs[i] <- infcross[2]
    }
  }
  #linfs <- linfs[!is.na(linfs)]
  #rinfs <- rinfs[!is.na(rinfs)]
  return(list(left = linfs, right = rinfs))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: For a peak marker (SD minimum), find corresponding signal maximum, first derivative (FD) cross etc.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Detect peak markers corresponding to another single peak marker
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' The function detects a sufficiently similar peak index among \code{compvec} (if any) to a reference value (\code{peak_val}).
#' The similarity is determined by difference in index given in \code{minsim}.
#'
#' @param peak_val A single \code{numeric} index of a peak apex.
#' @param compvec A \code{numeric} vector of indices to compare \code{peak_val} to.
#' @param minsim The difference in indices between \code{peak_val} and \code{compvec} to test for. Defaults to \code{3}.
#'
#' @return A single \code{numeric} value of a \code{compvec} index matching \code{peak_val}, or \code{NA} if no match is found.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
corresp <- function(peak_val, compvec, minsim = 3) {

  #Preliminary checks
  if(!is.atomic(peak_val) | !is.numeric(peak_val)) stop("The peak marker 'peak_val' must be a numeric vector!")
  if(!is.numeric(minsim) | length(minsim)!=1) stop("The proximity threshold 'minsim' must be a single numeric value!")
  if(any(is.na(compvec)) | !is.numeric(compvec)) stop("All input data must be numeric and without NAs!")

  #Remove NAs
  if(any(is.na(peak_val))) peak_val <- peak_val[!is.na(peak_val)]

  #Processing
  finres <- sapply(peak_val, function(x) {
    diffres <- abs(x-compvec)
    minres <- unique(min(diffres, na.rm = TRUE))
    corres <- which(diffres == minres & diffres <= minsim)
    finres <- if(length(corres)>=1) round(mean(compvec[corres], na.rm = TRUE),0) else NA
  })
  return(finres)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Check whether peak markers cross when they are not supposed to (e.g. inflection points, upslope points)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Check whether peak markers cross (when they are not supposed to)
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' The function checks whether peak markers cross each other when they are not supposed to. For example, if an inflection point \code{this_mark}
#' is farther away from its corresponding peak apex \code{this_lim} and closer to the adjacent peak \code{next_lim}, there is incorrect crossing
#' and inflection point detection should be revised (or removed).
#'
#' @param this_mark An \code{numeric} peak marker index to check against \code{next_mark} for crossing.
#' @param next_mark A \code{numeric} peak marker index to compare to \code{this_mark}.
#' @param this_lim The peak apex or another peak marker corresponding to \code{this_mark}. Thus, \code{this_mark} should be closer to \code{this_lim}
#' than \code{next_lim}.
#' @param next_lim Similarly to \code{this_lim}, represents a peak marker corresponding to \code{next_mark}.
#'
#' @return A \code{numeric} vector of \code{c(this_mark, next_mark)} where wrong markers (if any) are replaced with \code{NA}.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}, \code{\link{det_infs}}
#'
#' @keywords internal
markcross_chk <- function(this_mark, next_mark, this_lim, next_lim) {

  chklst <- list(this_mark, next_mark, this_lim, next_lim)
  if(any(lengths(chklst)!=1) | any(sapply(chklst, function(x) !is.na(x) & !is.numeric(x)))) stop("All input data must be single numeric values!")

  if(!is.na(this_mark) & !is.na(next_mark) & next_mark < this_mark) {
    #Determine which marker to delete
    #Check which marker is closer to the other peak rather than to its "proper" (assumed) peak
    closechk <- c(abs(this_mark-this_lim) > abs(this_mark-next_lim),
                  abs(next_mark-next_lim) > abs(next_mark-this_lim))
    if(closechk[1] & !closechk[2]) this_mark <- NA else if((!closechk[1] & closechk[2]) | all(closechk)) next_mark <- NA
  }
  return(c(this_mark, next_mark))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Classify peak type based on boundary classification and peak markers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Classify peak apices based on various peak markers and boundary types
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Classifies peak apices into one of four categories (see \strong{Arguments} and \strong{Value}).
#'
#' @param bnds A \strong{named} \code{numeric} vector of \strong{all} peak boundary indices. Possible types are: \code{"B"} (baseline-resolved),
#' \code{"F"} (fused), \code{"S"} (shoulder), or \code{"R"} (round).
#' @param infs A \code{numeric} vector of inflection point indices.
#' @param sdmin Indices of second derivative minima (\code{numeric}).
#' @param sigmax Indices of signal maxima (\code{numeric}).
#' @param fdcross Indices of \strong{compliant} first derivative downcrosses (\code{numeric}).
#' @param fdcross_nc \strong{Non-compliant} equivalent of \code{fdcross}.
#'
#' @return A \code{character} vector of peak apex types including any of: \code{"B"}, \code{"F"}, \code{"S"}, and/or \code{"R"}.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
ptype_class <- function(bnds, infs, sdmin, sigmax, fdcross, fdcross_nc) {
  #Preliminary checks
  master_list <- list(bnds, infs, sdmin, sigmax, fdcross, fdcross_nc)
  if(!all(sapply(master_list, is.atomic))) stop("All input arguments must be atomic vectors!")
  if(!all(unlist(sapply(master_list, function(x) is.numeric(x)|is.na(x))))) stop("Input arguments must either be numeric or NA!")
  if(!all(names(bnds) %in% c("B", "F", "S", "R"))) stop("Peak boundaries must all be named with one of: 'B', 'F', 'S', or 'R'!")

  bnds <- sort(bnds[!is.na(bnds)|!duplicated(bnds)])
  names(infs) <- rep("I", length(infs))

  #Check for corresponding sigmax and/or fdcross(_nc)
  #First, detect and omit peaks which are likely shoulders or round peaks (SD minima do not have a corresponding sigmax or fdcross)
  corr_fmax <- lapply(list(sigmax, fdcross, fdcross_nc), function(x) corresp(sdmin, x, minsim = 3))

  #Mark peaks where a corresponding maximum or fdcross IS present
  fmax_chk <- !(is.na(corr_fmax[[1]])|(is.na(corr_fmax[[1]]) & is.na(corr_fmax[[2]]) & is.na(corr_fmax[[3]])))

  #Is the closest boundary on both sides "B"?
  base_chk <- sapply(sdmin, function(x) {
    lbchk <- which(bnds < x)
    rbchk <- which(bnds > x)
    if(any(lengths(list(lbchk,rbchk))==0)) FALSE else {
      all(names(c(sort(bnds[lbchk], decreasing = TRUE)[1],
                  sort(bnds[rbchk], decreasing = FALSE)[1]))=="B")
    }
  })

  #Determine whether the closest point on both sides is an inflection point
  all_marks <- sort(c(bnds, infs))
  inf_chk <- sapply(seq_along(sdmin), function(x) if(base_chk[x]) TRUE else {
    linfchk <- which(all_marks < sdmin[x])
    rinfchk <- which(all_marks > sdmin[x])
    if(any(lengths(list(linfchk, rinfchk))==0)) FALSE else {
      all(names(c(sort(all_marks[linfchk], decreasing = TRUE)[1],
                  sort(all_marks[rinfchk], decreasing = FALSE)[1]))=="I")}
  })

  #Finally assign classes to peaks
  type_peak <- unlist(sapply(seq_along(sdmin), function(x) {
    if(fmax_chk[x] & base_chk[x]) "B"
    else if(fmax_chk[x] & !base_chk[x] & inf_chk[x]) "F"
    else if(!fmax_chk[x] & !base_chk[x] & inf_chk[x]) "S"
    else if(!fmax_chk[x] & !base_chk[x] & !inf_chk[x]) "R"
    else if(!any(c(fmax_chk[x], base_chk[x], inf_chk[x]))) "R" #NOT SURE ABOUT THIS ONE!
    else NA
  }))
  return(type_peak)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Group peaks based on whether they are baseline-resolved or not
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Divide peaks into baseline-resolved groups
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Assigns each pair of left and right peak boundaries a group number such that peaks are divided into baseline-resolved groups.
#'
#' @param lbt A \code{character} vector of \strong{left} peak boundary types. Possible values are: \code{"B"} (baseline-resolved), \code{"F"} (fused),
#' \code{"S"} (shoulder), or \code{"R"} (round).
#' @param rbt A \code{character} vector of \strong{right} peak boundary types. Must be of equal length to \code{lbt}.
#' @param reclass A \code{data.frame} An \strong{optional} \code{data.frame} containing three columns with types of peak apices (\code{"ptype"}),
#' as well as those of left (\code{"lb_type"}) and right (\code{"rb_type"}) boundaries. \strong{When provided}, the peak types are re-classified
#' and peak boundary types replaced based on \code{lbt} and \code{rbt}. Defaults to \code{NA}.
#'
#' @return A \code{list} of length 2 containing:
#' \describe{
#'  \item{group}{A \code{numeric} vector of group numbers equal in length to \code{lbt} and \code{rbt}.}
#'  \item{pclass}{The re-classified peak types (only if \code{reclass} is provided, otherwise \code{NA}).}
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
grp_pks <- function(lbt, rbt, reclass = NA) {
  class_cond <- if(all(is.na(reclass))|!is.data.frame(reclass)) FALSE else TRUE
  if(class_cond & (!all(unlist(reclass) %in% c("B","F","S","R", NA))|!all(colnames(reclass) %in% c("ptype", "lb_type", "rb_type")))) stop("When peak re-classification is required, a data.frame containing peak apex and boundary classes from 'ptype_class' must be provided!")
  if(length(lbt)!=length(rbt)|!all(is.numeric(c(lbt,rbt)))) stop("Both left and round boundaries 'lbt' and 'rbt' must be numeric vectors of equal length!")
  if(length(lbt)==1) return(group = 1, pclass = reclass) else {
    grp_vec <- cur_grp <- 1
    for(i in seq(length(rbt)-1)) {
      if(rbt[i]!=lbt[i+1]) {
        cur_grp <- cur_grp+1
        #Optionally re-classify peak apices and boundaries
        if(class_cond) {
          reclass[i,"rb_type"] <- reclass[i+1,"lb_type"] <- "B"
          if(all(reclass[i,c("lb_type", "rb_type")] %in% "B")) reclass[i,"ptype"] <- "B"
          if(i==length(rbt)-1 & all(reclass[i+1,c("lb_type", "rb_type")] %in% "B")) reclass[i+1, "ptype"] <- "B"
        }
      }
      grp_vec <- append(grp_vec, ifelse(i==length(rbt)-1, rep(cur_grp,2), cur_grp))
    }
    return(list(group = grp_vec, pclass = reclass))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Refine inflection points to see if alternatives closer to the Gaussian theoretical value are available
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Refine inflection points of peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Refines inflection points of peaks to be as close as possible to their Gaussian theoretical values.
#'
#' @param sig Signal data (\code{numeric}).
#' @param sdmin Indices of second derivative minima (\code{numeric}).
#' @param apex Indices of peak maxima (\code{numeric}).
#' @param linfmrk,rinfmrk Indices of \strong{all} second derivative downcrosses (\code{numeric}, \code{linfmrk}) and upcrosses (\code{numeric}, \code{rinfmrk}) marking inflection points.
#' @param orig_l,orig_r Indices of \strong{current} left and right inflection points. Must each be equal in length to that of \code{apex}.
#' @param bds_l,bds_r Indices of peak start (\code{bds_l}) and end (\code{bds_r}) boundaries.
#'
#' @return A \code{list} with refined \code{$left} and \code{$right} inflection points.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
#'
#' @importFrom stats na.omit
inf_refine <- function(sig, sdmin, apex, linfmrk, rinfmrk, orig_l, orig_r, bds_l, bds_r) {

  if(any(sapply(list(sig, sdmin, apex, linfmrk, rinfmrk, orig_l, orig_r), function(x) !is.numeric(x)|!is.atomic(x)))) stop("All input arguments must be atomic numeric vectors!")

  res_l <- res_r <- ind_l <- ind_r <- c()
  for(i in seq_along(sdmin)) {
    linf_test <- na.omit(linfmrk[linfmrk > bds_l[i] & linfmrk < sdmin[i]])
    rinf_test <- na.omit(rinfmrk[rinfmrk > sdmin[i] & rinfmrk < bds_r[i]])
    if(length(linf_test)>0 & !all(is.na(linf_test))) {
      res_l <- append(res_l, linf_test[which.min(abs(sig[linf_test]-(sig[apex[i]]*0.606)))])
      ind_l <- append(ind_l, i)
    }
    if(length(rinf_test)>0 & !all(is.na(rinf_test))) {
      res_r <- append(res_r, rinf_test[which.min(abs(sig[rinf_test]-(sig[apex[i]]*0.606)))])
      ind_r <- append(ind_r, i)
    }
  }
  if(length(res_l)>0) orig_l[ind_l] <- res_l
  res_l <- orig_l
  if(length(res_r)>0) orig_r[ind_r] <- res_r
  res_r <- orig_r
  return(list(left = res_l, right = res_r))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Attempt to find upslope points provided left/right inflection points and peak boundaries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find upslope points for one or more peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' Finds upslope points characterised by second derivative maxima, provided peak apices, inflection points, and start/end indices (boundaries).
#'
#' @param lbds,rbds Indices of peak start (\code{lbds}) and end (\code{rbds}) boundaries.
#' @param linfs,rinfs Indices of left and right inflection points. Must each be equal in length to that of \code{sdmin}.
#' @param sdmin Indices of second derivative minima (\code{numeric}).
#' @param upsmrk Indices of second derivative maxima (\code{numeric}) characteristic of upslope points.
#' @param sder Second derivative data of the signal (\code{numeric}).
#'
#' @return A \code{list} with \code{$left} and \code{$right} upslope points.
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{peakfind}}
#'
#' @keywords internal
#'
#' @importFrom stats na.omit
find_ups <- function(lbds, rbds, linfs, rinfs, sdmin, upsmrk, sder) {
  #Preliminary checks
  if(any(sapply(list(lbds, rbds, linfs, rinfs, sdmin, upsmrk), function(x) !is.numeric(x)|!is.atomic(x)))) stop("All input arguments must be atomic numeric vectors!")

  #Only if inflection point is present
  ups <- sapply(seq_along(sdmin), function(x) {
    res_l <- na.omit(upsmrk[upsmrk < linfs[x] & upsmrk > lbds[x]])
    res_r <- na.omit(upsmrk[upsmrk > rinfs[x] & upsmrk < rbds[x]])
    res_l <- if(length(res_l)>0) res_l[which.max(sder[res_l])] else NA #res_l[which.min(abs(sig[res_l]-(sig[true_pind[x]]*0.130)))]
    res_r <- if(length(res_r)>0) res_r[which.max(sder[res_r])] else NA #res_r[which.min(abs(sig[res_r]-(sig[true_pind[x]]*0.130)))]
    return(c(res_l, res_r))
  })
  ups <- setNames(split(ups, row(ups)), c("left","right"))
  return(ups)
}
