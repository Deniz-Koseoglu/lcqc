#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Prepare data for integration via perpendicular drop, skim, and/or curve fitting methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Summarize and prepare peak detection data for integration
#'
#' @description This function takes peak detection results output of \code{\link{chrom_detect}} and prepares the data for integration via functions
#' \code{\link{chrom_skim}} and/or \code{\link{chrom_icf}}.
#'
#' @param input The output of function \code{\link{chrom_detect}}.
#' @param vars Either \code{"auto"} (default) or a \code{character} vector of length 3 specifying the column names of
#' retention time, signal, and second derivative data in \code{input} (in that order).
#' @param crit_w Critical width parameter to use for baseline construction via \code{\link{fastchrom_bline}}.
#'
#' @return A \code{list} of length 5 containing the following elements:
#' \describe{
#' \item{main_df}{A \code{data.frame} containing data indices, retention time, original and baseline-corrected signal (output from \code{\link{fastchrom_bline}}).}
#' \item{type_df}{A \code{list} of 5 \code{data.frame} objects summarizing types, original, baseline-corrected (\code{\link{fastchrom_bline}}),
#' and/or accurately-interpolated (\code{\link{acc_inf}}, \code{\link{acc_max}}) values of peak \code{starts}, \code{ends}, \code{linfs}, \code{rinfs}, \code{maxes}.}
#' \item{grp_df}{Data equivalent to that in \code{type_df} but organized by groups of baseline-resolved peaks in a nested \code{list}.}
#' \item{grp_blines}{A \code{data.frame} containing \strong{baseline-resolved peak group IDs}, original xy-coordinates, FastChrom baselines,
#' baseline-corrected signal, and second derivatives of all peak regions.}
#' \item{acc_tops}{A \code{data.frame} containing combined results of accurate inflection point and peak retention time determination
#' via \code{\link{acc_inf}} and \code{\link{acc_max}}, respectively.}
#' \item{peak_list}{A \code{list} of \code{data.frame} objects, each containing retention time, original signal, baselines, baseline-corrected signal,
#' second derivative, and peak ID data for an individual peak.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' dtprep(lcqc:::wf_detpeaks)
#' }
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{chrom_icf}}, \code{\link{chrom_detect}}, \code{\link{acc_inf}}, \code{\link{acc_max}}
dtprep <- function(input, vars = "auto", crit_w = "auto") {
  #Preliminary checks
  input <- chkdt(input)

  if(!any(vars %in% "auto") & !all(vars %in% colnames(input[["Chromatogram"]]))) {
    stop("Time, signal, and 2nd derivative variables given in the 'vars' argument are not present in the input data!")
  }

  #Extract elements from input data
  chrom_df <- input[["Chromatogram"]][1:8] #Isolate RT and signals (raw, baseline-corrected, smoothed only)
  peak_list <- input[["Peaks"]]
  peak_idres <- input[["Peak_Extents"]]
  amplim <- input[["Amplitude_Limit"]]

  #Retrieve RT and signal data by variable names
  if(any(vars %in% "auto")) {
    vars <- colnames(chrom_df)[c(1,3,8)]
  }

  #Retrieve group spans
  grp_inds <- peak_idres[,"group"]
  grps <- unique(grp_inds)
  grpspans <- lapply(grps, function(x) {
    peakinds <- which(grp_inds==x)
    grpspans <- c(min(peak_idres[peakinds,"ind_starts"]):max(peak_idres[peakinds,"ind_ends"]))
  })

  #Retrieve peak spans
  pks <- peak_idres[,"peak"]
  peakspans <- lapply(pks, function(x) peak_idres[x,"ind_starts"]:peak_idres[x,"ind_ends"])

  #Calculate and rbind common (group) baselines
  #Derive baselines for each peak group
  fchr_res <- fastchrom_bline(sig = chrom_df[,vars[2]], starts = peak_idres[,"ind_starts"], ends = peak_idres[,"ind_ends"],
                            crit_w = if(crit_w=="auto") min(peak_idres[,"ind_rinf"]-peak_idres[,"ind_linf"], na.rm = TRUE) else crit_w)
  blines <- fchr_res[["data"]]

  grp_col <- rep(NA, nrow(blines))
  for(i in seq_along(grpspans)) {
    grp_col[grpspans[[i]]] <- grps[i]
  }
  blines <- do.call(cbind.data.frame, list(grp_col, chrom_df[,vars[1]], blines, chrom_df[,vars[3]]))
  blines <- blines[,c(1,3,2,4:7)]
  colnames(blines) <- c("group", "ind", "x", "orig_y", "bline", "y", "sd")
  blines <- blines[!is.na(blines[,"group"]),]

  #Separate blines data.frame into peaks
  pklst <- lapply(seq_along(peakspans), function(x) {
    res <- blines[blines[,"ind"] %in% peakspans[[x]],]
    res[,"peak"] <- x
    return(res)
  })
  names(pklst) <- paste0("peak_", seq_along(peakspans))

  #Derive accurate inflection points for all peaks
  #Calculate and rbind accurate inflection points and apices
  #Derive approximate apices
  apices <- lapply(seq(nrow(peak_idres)), function(x) {
    sig_col <- "finmax" #if(!is.na(peak_idres[x,"ind_sigmax"])) "sigmax" else "dermax"
    res <- peak_idres[x,c("group", "peak", "ptype", paste0("ind_", sig_col), paste0("rt_", sig_col))]
    colnames(res)[grep("ind_|rt_", colnames(res))] <- c("ind", "x")
    return(res)
  })
  apices <- Reduce(rbind.data.frame, apices)

  #Calculate inflection points and apices
  input <- blines[,c("ind","x","y","sd")]
  markers <- setNames(cbind.data.frame(peak_idres[,c("ptype", "ind_linf", "ind_rinf")], apices[,"ind"]), c("ptype", "linf", "rinf", "max"))

  finf <- acc_inf(inds = input[,"ind"], xvals = input[,"x"], yvals = input[,"y"], sd = input[,"sd"], linfs = markers[,"linf"], rinfs = markers[,"rinf"])
  fmax <- acc_max(inds = input[,"ind"], xvals = input[,"x"], yvals = input[,"y"], maxes = markers[,"max"],
                  linfs = markers[,"linf"], rinfs = markers[,"rinf"], ptypes = markers[,"ptype"])

  #Final output data.frame for accurate inflection points and peak apices
  acc_tops <- cbind.data.frame(grp_inds, pks, finf[["left"]], finf[["right"]], fmax)
  colnames(acc_tops) <- c("group", "peak", "rt_linf", "sig_linf", "low_linf", "high_linf", "rt_rinf", "sig_rinf", "low_rinf", "high_rinf", "rt_max", "sig_max", "max_type")

  #Retrieve a list of key peak-delimiting index and XY data (inflection points, maxes, starts and ends etc.)
  exlist <- lapply(c("starts", "ends", "linf", "rinf"), function(x) peak_idres[,c("group", "peak", "ptype", paste0("ind_",x), paste0("rt_",x))])
  exlist <- append(exlist, list(apices))
  names(exlist) <- c("starts", "ends", "linfs", "rinfs", "maxes")
  exlist <- lapply(exlist, function(y) setNames(Reduce(cbind.data.frame, list(y,
                                                                              chrom_df[y[,4],vars[c(2,3)]],
                                                                              Reduce(rbind.data.frame, lapply(seq(nrow(y)), function(z) if(is.na(y[z,4])) rep(NA,2) else blines[blines[,"ind"] %in% y[z,4],c("bline", "y")])))),
                                                c("group", "peak", "ptype", "ind", "x", "orig_y", "sd", "bline", "y")))
  exlist[["starts"]] <- cbind.data.frame(exlist[["starts"]], type = peak_idres[,"lb_type"])
  exlist[["ends"]] <- cbind.data.frame(exlist[["ends"]], type = peak_idres[,"rb_type"])
  exlist[["linfs"]] <- cbind.data.frame(exlist[["linfs"]], finf[["left"]])
  exlist[["rinfs"]] <- cbind.data.frame(exlist[["rinfs"]], finf[["right"]])
  exlist[["maxes"]] <- cbind.data.frame(exlist[["maxes"]], fmax[,c("acc_x", "acc_y")])
  exlist <- lapply(exlist, function(x) { rownames(x) <- NULL; x})

  #Divide exlist by group
  dtlist <- list()
  for(i in grps) {
    dtlist[[i]] <- lapply(exlist, function(y) y[y[,"group"]==i,])
  }

  if(any(colnames(peak_idres) %in% "Compound")) acc_tops <- cbind.data.frame(Compound = peak_idres[,"Compound"], acc_tops)

  return(list(main_df = setNames(cbind.data.frame(seq(nrow(chrom_df)), chrom_df[,1:3]), c("ind","x","orig_y","y")),
              type_df = exlist, grp_df = dtlist, grp_blines = blines, acc_tops = acc_tops, peak_list = pklst))
}
