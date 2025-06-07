#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: A single workflow to identify peak maxima, valleys, and shoulders
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title A workflow to identify baseline-resolved, fused, shoulder, and round chromatographic peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_detect}} workflow, not intended for standalone use.
#'
#' This function forms a complete workflow for peak identification and uses various component functions (consult the \strong{See Also} section).
#'
#' @param marks Initial peak markers from \strong{original unsmoothed} data output from function \code{\link{markmerge}}.
#' @param smooth_marks (\strong{Optional}) initial peak markers from \strong{smoothed} data output from function \code{\link{markmerge}}.
#' @param rtime A \code{numeric} vector of retention time data.
#' @param sig A \code{numeric} vector of chromatographic signal data.
#' @param fder,sder First and second derivative data as \code{numeric} vectors. Must be of equal length to \code{rtime} and \code{sig}.
#' @param max_w The maximum smooth width (in point indices). Set to 10 where a lower value is given.
#' @param liftoff,touchdown Single \code{numeric} parameters (between 0 and 100) required by the ApexTrack algorithm (see \code{\link{apexbnds}}).
#' @param sig_thres Signal amplitude limit (threshold) for peak detection (see \code{chrom_amplim}).
#' @param fd_thres,sd_thres Both are \code{numeric} vectors of length 2 containing the first and second derivative peak detection thresholds
#' (see \code{\link{chrom_derlims}}).
#' @param min_ht Height peak \strong{pre-}-filtering criterion (\code{numeric}). Peaks of lower height are removed. Defaults to \code{NA} (no filter applied).
#' @param w_rej Inflection point width peak \strong{post}-filtering criterion (\code{numeric}). Defaults to \code{NA}.
#' @param pa_rej Peak \strong{post}-filtering criterion for inflection point peak area (\code{numeric}). Defaults to \code{NA}.
#' @param sn_rej Signal-to-noise ratio (S/N) peak \strong{pre}-filtering criterion. Defaults to \code{NA}.
#' @param rej_logic Logic to use for peak filtering via \code{w_rej}, \code{pa_rej}, \code{min_ht}, and/or \code{sn_rej}. One of \code{"OR"} or \code{"AND"}. See \code{\link{rej_filter}} for more information.
#' @param bnch A \code{logical} indicating whether peak bunching should be detected and mitigated (see \code{\link{detect_bunch}}).
#' @param fchrom Either \code{NA} (default) or a \code{numeric} value of critical width to use for FastChrom baseline correction.
#' Applies to resolved peaks only. See \code{\link{fastchrom_bline}} for details.
#' @param refine_infs A \code{logical} indicating whether detected inflection points should be refined to be as close as possible to their Gaussian theoretical values
#' (see \code{\link{inf_refine}}).
#'
#' @return A list of length 2 containing the following elements:
#' \describe{
#' \item{results}{A \code{data.frame} of peak groups, peak types (\code{ptype}), peak boundary types (\code{lb_type}, and \code{rb_type}),
#' and extent markers (as data indices) with their corresponding retention times. Markers include second derivative-based maxima (\code{_dermax}),
#' signal maxima (\code{_sigmax}), final maxima (\code{_finmax}), start and end boundaries (\code{_starts} and \code{_ends}),
#' inflection points (\code{_linf} and \code{_rinf}), and upslope points (\code{_lups} and \code{_rups}).
#' Various peak metrics used for filtering (e.g. via \code{\link{rej_filter}}) are also included, such as the peak area (\code{"top_pa"}),
#' inflection point width (\code{"inf_wd"}), and S/N ratio (\code{"sn_ratio"}).}
#' \item{information}{A \code{character} string summarising information about the number and type(s) of detected peaks.}
#' }
#' @seealso \code{\link{chrom_detect}}, \code{\link{markmerge}}, \code{\link{sdmin_infchk}}, \code{\link{detect_bunch}}, \code{\link{noise_calc}},
#' \code{\link{rej_filter}}, \code{\link{det_infs}}, \code{\link{base_raw}}, \code{\link{find_fused}}, \code{\link{base_fused}}, \code{\link{find_shld}},
#' \code{\link{ptype_class}}, \code{\link{corresp}}, \code{\link{grp_pks}}, \code{\link{inf_refine}}, \code{\link{find_ups}}, \code{\link{fastchrom_bline}}
#'
#' @keywords internal
#'
#' @importFrom stats na.omit
peakfind <- function(marks, smooth_marks = NA, rtime, sig, fder, sder, max_w = NA, liftoff = 0, touchdown = 0.5, sig_thres, fd_thres, sd_thres,
                     min_ht = NA, w_rej = NA, pa_rej = NA, sn_rej = NA, rej_logic = rep("OR",2), bnch = TRUE, refine_infs = TRUE, fchrom = NA) { #fder and sder denote SMOOTHED first and second derivatives

  #Preliminary checks
  if(is.na(fchrom)) fchrom <- 0

  #Merge original and smoothed peak markers (when both are available)
  fin <- markmerge(marks = marks, smooth_marks = smooth_marks, max_w = max_w, minsim = 3)

  #Extract peak marker variables (COMPLIANT)
  sdmin <- fin[["SD_minima"]]
  sdmax <- fin[["SD_maxima"]]
  sdcross <- fin[["SD_downcr"]]
  sdupcross <- fin[["SD_upcr"]]
  fdmin <- fin[["FD_minima"]]
  fdmax <- fin[["FD_maxima"]]
  fdcross <- fin[["FD_downcr"]]
  fdupcross <- fin[["FD_upcr"]]
  sigmin <- fin[["sig_minima"]]
  sigmax <- fin[["sig_maxima"]]

  #Extract peak marker variables (NON-COMPLIANT) - no further filtering operations are carried out on these
  #These markers are to be used only in a select few scenarios where standard operations fail
  sdmin_nc <- fin[["SD_minima_nc"]]
  sdmax_nc <- fin[["SD_maxima_nc"]]
  sdcross_nc <- fin[["SD_downcr_nc"]]
  sdupcross_nc <- fin[["SD_upcr_nc"]]
  fdmin_nc <- fin[["FD_minima_nc"]]
  fdmax_nc <- fin[["FD_maxima_nc"]]
  fdcross_nc <- fin[["FD_downcr_nc"]]
  fdupcross_nc <- fin[["FD_upcr_nc"]]
  sigmin_nc <- fin[["sig_minima_nc"]]
  sigmax_nc <- fin[["sig_maxima_nc"]]

  #Create vector to store information about function output
  information <- c()

  #Detect matching points that reinforce a specific conclusion
  #Carry out starting checks on SD minima (peak markers)
  #Reject sdmin which don't have at least one inflection point as their top 1 closest peak marker on either side AND don't have a corresponding sigmax (sometimes occurs with round peaks)
  sdmin <- sdmin_infchk(sdmin, sigmax, fin) #, minsim = 3

  #Detect sdmin 'bunching' and leave either only the closest rounded mean value (to an integer) or the one with a corresponding sigmax
  #Function 'max_w' used to identify bunching
  if(bnch) sdmin <- detect_bunch(sdmin, sigmax, max_w)

  #Prepare for calculation of SN ratio for all peaks (calculate the NOISE component)
  snoise <- noise_calc(sig, sder, sig_thres, sd_thres)

  #Filter peaks (ROUND 1 of 2) based on height ('ht') and signal-to-noise ratio ('sn')
  cat("\nPre-filtering peaks based on height and S/N ratio...")
  rej_vec <- rej_filter(rtime, sig, sdmin, linfs = NA, rinfs = NA, crit = c(ht = min_ht, wd = NA, pa = NA, sn = sn_rej), noise = snoise, logic = rej_logic[1])
  #information <- append(information, rej_vec[["info"]])
  sdmin <- sdmin[!rej_vec[["results"]]]
  if(length(sdmin)==0) stop("Current height and S/N ratio filtering criteria removed all the peaks! Please adjust and re-run function...")

  #Retrieve inflection points of the remaining peaks
  pinfs <- det_infs(sig, sdmin, sdcross, sdupcross, sdcross_nc, sdupcross_nc)
  linfs <- pinfs[["left"]]
  rinfs <- pinfs[["right"]]

  #Derive raw first estimates of baseline-resolved peak boundary locations (via ApexTrack)
  apex_raw <- base_raw(rtime, sig, fder, sder, pinfs, sdmin, sigmax, fdcross, fdcross_nc)
  #return(apex_raw)
  if(length(sdmin)>1) {
    #Find fused boundaries by detecting intersecting ApexTrack baselines
    fused_res <- find_fused(apex_raw, rtime, sdmax, sigmin)

    if(!any(is.na(fused_res[["bounds"]]))) {
      fused_bnds <- fused_res[["bounds"]]
      fused_inds <- fused_res[["peak_inds"]]
      #Fuse the crossing lines and expand via ApexTrack
      bline_bnds <- base_fused(fused_bnds, fused_inds, apex_raw[["starts"]], apex_raw[["ends"]], rtime, sig, fder, sder, liftoff, touchdown)
      base_str <- bline_bnds[["starts"]]
      base_end <- bline_bnds[["ends"]]
    } else {
      fused_bnds <- NA
      base_str <- apex_raw[["starts"]]
      base_end <- apex_raw[["ends"]]
      names(base_str) <- names(base_end) <- rep("B", length(base_str))
    }

    #Detect shoulder and round boundaries
    sr_bnds <- find_shld(na.omit(c(base_str,base_end, fused_bnds)), c(linfs, rinfs), sdmin, sdmax, fder, sder, fd_thres, sd_thres)
  } else {
    cat("\nNo fused or shoulder/round peak detection was attempted since only one peak is present!")
    base_str <- apex_raw[["starts"]]
    base_end <- apex_raw[["ends"]]
    names(base_str) <- names(base_end) <- rep("B", length(base_str))
    fused_bnds <- sr_bnds <- NA
  }

  #Combine all peaks boundaries into one vector
  all_bnds <- c(base_str, base_end, fused_bnds, sr_bnds)
  all_bnds <- sort(all_bnds[!is.na(all_bnds) & !duplicated(all_bnds)])

  #Classify peak apices
  type_peak <- ptype_class(all_bnds, c(linfs, rinfs), sdmin, sigmax, fdcross, fdcross_nc)

  #Create and populate results data.frame
  #Derive information for data.frame
  sig_pind <- corresp(sdmin, sigmax, minsim = 3) #Signal maximum
  true_pind <- sapply(seq_along(sdmin), function(x) if(is.na(sig_pind[x])) sdmin[x] else sig_pind[x]) #Final maximum
  bds_l <- sapply(sdmin, function(x) sort(all_bnds[which(all_bnds < x)], decreasing = TRUE)[1]) #Left boundaries
  bds_r <- sapply(sdmin, function(x) sort(all_bnds[which(all_bnds > x)], decreasing = FALSE)[1]) #Right boundaries
  type_lb <- names(bds_l) #Left boundary types
  type_rb <- names(bds_r) #Right boundary types
  inf_l <- sapply(seq_along(sdmin), function(x) {
    res <- linfs[which(linfs < sdmin[x] & linfs > bds_l[x])]
    if(length(res)>0) min(res) else NA
  }) #Left inflection points
  inf_r <- sapply(seq_along(sdmin), function(x) {
    res <-  rinfs[which(rinfs > sdmin[x] & rinfs < bds_r[x])]
    if(length(res)>0) min(res) else NA
  }) #Right inflection points

  #Get groups
  grp_vec <- grp_pks(bds_l, bds_r, NA)[["group"]] #Peak groups

  #OPTIONALLY refine inflection points to be as close as possible to their Gaussian theoretical values (0.606 peak height)
  if(refine_infs) {
    new_infs <- inf_refine(sig, sdmin, true_pind, c(sdcross, sdcross_nc), c(sdupcross, sdupcross_nc), inf_l, inf_r, bds_l, bds_r)
    inf_l <- new_infs[["left"]]
    inf_r <- new_infs[["right"]]
  }

  #Attempt to detect upslope points (NOT FOR "S" or "R" peaks) as close as possible to Gaussian theoretical values (0.130 peak height)
  fin_ups <- find_ups(bds_l, bds_r, inf_l, inf_r, sdmin, c(sdmax, sdmax_nc), sder)
  ups_l <- fin_ups[["left"]]
  ups_r <- fin_ups[["right"]]

  #Continue deriving values for the data.frame
  inf_width <- abs(inf_r - inf_l)
  top_pa <-  sapply(seq_along(sdmin), function(x) {
    str <- inf_l[x]
    end <- inf_r[x]
    res <- if(!any(is.na(c(str,end)))) integ(cbind.data.frame(x = rtime[str:end], y = sig[str:end]), slnt = TRUE) else NA
  })
  sn_ratio <- sig[true_pind]/snoise

  #Create and populate data.frame
  peak_idres <- data.frame("group" = grp_vec, "peak" = seq_along(sdmin),
                           "inf_wd" = inf_width, "top_pa" = top_pa, "sn_ratio" = sn_ratio,
                           "ptype" = type_peak,"lb_type" = type_lb, "rb_type" = type_rb,
                           "ind_dermax" = sdmin, "ind_sigmax" = sig_pind, "ind_finmax" = true_pind,
                           "ind_starts" = bds_l, "ind_ends" = bds_r,
                           "ind_lups" = ups_l, "ind_rups" = ups_r,
                           "ind_linf" = inf_l, "ind_rinf" = inf_r, row.names = NULL)

  #Filter out peaks with a negative S/N ratio regardless of 'sn_rej'
  sn_neg <- which(sn_ratio<0)
  if(length(sn_neg)>0) {
    peak_idres <- peak_idres[-sn_neg,]
    cat("\n", length(sn_neg), " peaks were removed due to a negative S/N ratio.")
  }

  #Filter out peaks (ROUND 2 of 2) based on peak area ('pa') and width ('wd')
  cat("\nPost-filtering peaks based on peak inflection point area and width...")
  rej_vec2 <- rej_filter(rtime, sig, sdmin, inf_l, inf_r, crit = c(ht = NA, wd = w_rej, pa = pa_rej, sn = NA), noise = NA, logic = rej_logic[2])
  #information <- append(information, rej_vec2[["info"]])
  peak_idres <- peak_idres[!rej_vec2[["results"]],]
  if(nrow(peak_idres)==0) stop("Current peak filtering criteria removed all the peaks! Please adjust and re-run function...")

  #Remove peaks which do not have either the left or right boundary (NA) or whose peak type could not be identified (not sure about this second one)
  if(any(is.na(peak_idres[,c("ind_starts", "ind_ends", "ptype")]))) {
    peak_idres <- peak_idres[!with(peak_idres,is.na(ind_starts)|is.na(ind_ends)|is.na(ptype)),]
    cat("\nA total of ", length(sdmin)-nrow(peak_idres), " peaks were removed due to unidentified peak type and/or absence of peak starts/ends!")
  }

  #Optionally carry out additional baseline correction using FastChrom to adjust baseline-resolved peak boundaries
  if(fchrom>0) {
    information <- paste0(information, "\nFastChrom baseline correction was carried out with a critical width of ", fchrom, ".")

    fchrom_res <- fastchrom_bline(sig = sig, starts = peak_idres[,"ind_starts"], ends = peak_idres[,"ind_ends"],
                                  crit_w = fchrom, for_plot = FALSE)[["peaklims"]]

    #Replace any adjusted peak starts or ends with new ones
    peak_idres[,"ind_starts"] <- fchrom_res[["starts"]]
    peak_idres[,"ind_ends"] <- fchrom_res[["ends"]]
  }

  #Add retention times to the output data.frame
  rt_colsub <- grep("^ind_", colnames(peak_idres))
  for(i in rt_colsub) {
    peak_idres[,paste0("rt_", gsub("^ind_","", colnames(peak_idres)[i]))] <- rtime[peak_idres[,i]]
  }

  ##Check that the left boundary of the first peak and the right boundary of the last peak is "B" and fix if not
  #peak_idres[1,"lb_type"] <- peak_idres[nrow(peak_idres),"rb_type"] <- "B"
  #if(all(unlist(peak_idres[1,c("lb_type","rb_type")])=="B")) peak_idres[1,"ptype"] <- "B"
  #if(all(unlist(peak_idres[nrow(peak_idres),c("lb_type","rb_type")])=="B")) peak_idres[nrow(peak_idres),"ptype"] <- "B"

  #Also re-group peaks
  regrp <- grp_pks(peak_idres[,"ind_starts"], peak_idres[,"ind_ends"], reclass = peak_idres[,c("ptype","lb_type","rb_type")])
  peak_idres[,"group"] <- regrp[["group"]]
  peak_idres[,c("ptype","lb_type", "rb_type")] <- regrp[["pclass"]]
  peak_idres[,"peak"] <- seq(nrow(peak_idres))

  #Add information about the total number of peaks finally detected
  ptype_tbl <- table(peak_idres[,"ptype"])
  ptype_nms <- c("B" = "baseline-resolved", "F" = "fused", "S" = "shoulder", "R" = "round")
  information <- paste0(information,"\nFollowing peak picking and removal, a total of ", nrow(peak_idres), " peaks were detected:",
                        paste0(sapply(names(ptype_nms), function(x) if(any(names(ptype_tbl)==names(ptype_nms[x]))) paste0("\n", ptype_tbl[x], " ", ptype_nms[x], " peaks.") else ""), collapse = ""), collapse = "")
  cat(information)
  return(list(results = peak_idres, information = information))
}
