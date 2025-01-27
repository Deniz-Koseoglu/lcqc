#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Carry out tangent skim on a pair of peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Front or tail Tangent Skim on a pair of peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Carries out a Tangent Skim on a suitable parent-child pair of chromatographic peaks.
#' See \code{\link{chrom_skim}} for further details.
#'
#' @param child A \code{data.frame} containing the \code{\link{dtprep}} \code{$grp_blines} output with \strong{child} peak data.
#' @param cmax A \strong{single row} \code{data.frame} containing \strong{child} peak data output from \code{\link{dtprep}}\code{$type_df$maxes}.
#' @param skim_type Type of skim to apply. One of \code{"front"} or \code{"tail"}.
#'
#' @return A \code{data.frame} containing original data indices (\code{"ind"}), retention time (\code{"x"}), baseline-corrected signal (\code{"y"}),
#' peak group ID (\code{"group"}), those of the parent and child peak (\code{"peak1"} and \code{"peak2"}), the skim type (one of \code{"TF"} or
#' \code{"TT"} for front and tail skims, respectively), and the combined ID used for plotting via \code{\link{integ_plot}} (\code{"combo_id"}).
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{integ_plot}}
#'
#' @keywords internal
#'
#' @importFrom stats lm
tskim <- function(child, cmax, skim_type) {

  #Preliminary checks
  if(!any(c("front", "tail") %in% skim_type)) stop("Argument 'skim_type' must be one of: 'front', 'tail'!")

  #Retrieve peak H2 (child peak) x and y data
  x <- child[,"x"]
  y <- child[,"y"]

  #Retrieve group and peak indices (for output)
  markers <- c(pthis = cmax[,"peak"],
               pnext = if(skim_type=="front") cmax[,"peak"]+1 else cmax[,"peak"]-1,
               grp = unique(cmax[,"group"]))

  #Based on Skim Type, calculate straight skim lines starting at each point between the peak start and peak apex ("Tail"), or peak apex and peak end ("Front")
  #Skim ending should always be at the valley/shoulder between peaks H1 and H2.
  inds <- child[,"ind"]
  peakmax <- which(inds == cmax[,"ind"])
  peakend <- length(inds)

  vlind <- if(skim_type=="front") peakend else if(skim_type=="tail") 1
  tanlim <- if(skim_type=="front") 1:peakmax else if(skim_type=="tail") peakend:peakmax

  skimres <- lapply(tanlim, function(z) {

    skim_lims <- sort(c(z, vlind))

    xs <- c(x[skim_lims[1]], x[skim_lims[2]])
    ys <- c(y[skim_lims[1]], y[skim_lims[2]])

    tbline <- lm(cbind.data.frame(y = ys, x = xs))
    tm <- tbline[["coefficients"]]["x"]
    tc <- tbline[["coefficients"]]["(Intercept)"]

    adj_x <- x[skim_lims[1]:skim_lims[2]]
    adj_y <- y[skim_lims[1]:skim_lims[2]]

    tbline <- tm*adj_x+tc

    pen_chk <- length(which((tbline - adj_y)>cmax[,"acc_y"]*0.02))
    finres <- rep(NA, length(inds))
    finres[skim_lims[1]:skim_lims[2]] <- tbline

    finres[is.na(finres)] <- y[is.na(finres)]
    if(pen_chk==0) return(finres) else return(NA)
  })

  names(skimres) <- tanlim
  skimres <- skimres[!is.na(skimres)]
  tanlim <- tanlim[tanlim %in% as.numeric(names(skimres))]

  #Select the earliest ("Front") or latest ("Tail") skim line starting points where the signal is not below the skim line by more than the current amplitude limit (amplim).
  #Set the remaining points to be equal to the corresponding signal.
  if(length(tanlim) > 0) {

    #Check that <40% of skim_bline values are either equal or within 1% of the corresponding signal values
    skimtest <- if(skim_type=="front") skimres[[which.min(tanlim)]] else if(skim_type=="tail") skimres[[which.max(tanlim)]]
    skimchk <- length(which(skimtest == child[,"y"] | abs(1-skimtest/child[,"y"]) <= 0.01)) / length(skimtest) < 0.40

  } else skimchk <- FALSE

  if(skimchk) {
    skimline <- cbind.data.frame(child[,c("ind", "x")], y = skimtest)

    #Set the integration boundary types
    bnd_text <- paste0("T", if(skim_type == "front") "F" else if(skim_type=="tail") "T")

  } else {
    skimline <- rbind.data.frame(child[vlind, c("ind","x","y")],
                                 c(child[vlind, c("ind","x")], y = 0))
    colnames(skimline) <- c("ind","x","y")
    bnd_text <- "PD"
  }

  #Sort the current and 'next' peaks ('pthis' and 'pnext') to make sure that their IDs appear sequentially in the output
  sorted_markers <- sort(markers[c("pthis","pnext")])

  skimline <- cbind.data.frame(skimline, group = markers["grp"],
                               peak1 = sorted_markers[1], peak2 = sorted_markers[2], type = bnd_text)
  skimline[,"combo_id"] <- paste0(unique(skimline[,c("peak1","peak2", "type")]), collapse = "_")
  return(skimline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Carry out exponential skim on a pair of peaks with two different optimization options
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION A: Fit an exponential curve
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Fit an exponential curve (for exponential skimming)
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Constructs an exponential curve. Used for iterative optimization of exponential skim in \code{\link{exskim}}.
#' See \code{\link{chrom_skim}} for further details.
#'
#' @param B Growth/decay constant of the exponential curve.
#' @param input A \code{numeric} vector containing retention time (x-axis) for which to derive an exponential curve.
#' @param x0,y0 Retention time (x-axis) and baseline-corrected signal (y-axis) values at the valley (or other boundary) between parent and child peaks.
#' @param A Slope of the baseline of the parent peak.
#' @param C Offset of the baseline of the parent peak.
#'
#' @return A \code{numeric} vector of the exponential curve equal in length to \code{input}.
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{experr}}, \code{\link{exskim}}
#'
#' @keywords internal
expfit <- function(B, input, y0, x0, A, C) {
  y0 * exp(B*(input-x0)) + A*input+C
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION B: Calculates the sum of Euclidean Distances between a signal and a fit exponential curve
#Define the sum of this Euclidean Distance as the maximum error (i.e. 1 or 100%)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Error minimization function for exponential skim
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Calculates two different measures of error for the exponential curve. Intended to use with \code{\link{exskim}} for error minimization via
#' \code{\link[stats]{optim}}. See \code{\link{chrom_skim}} for further details.
#'
#' @param B_opt Exponential growth/decay constant to be optimized. Starting value is set to 0 (no growth or decay).
#' @param extra_pars A \strong{named} \code{numeric} vector of length 4 containing values for constant A (slope of the parent peak baseline, \code{"fin_A"}),
#' constant C (y-offset of the baseline of the parent peak, \code{"fin_C"}), and the x- and y-data at the valley or other boundary between the peaks
#' (\code{"x0"} and \code{"y0"}).
#' @param marks A \strong{named} \code{numeric} vector of length 3 containing data indices of the starting (\code{"infind"}) and ending (\code{"fitend"})
#' points of the exponential curve, as well as the location of the valley (or other boundary) between the parent and child peaks (\code{"vlind"}).
#' @param opt_data A \code{data.frame} containing the output of \code{\link{dtprep}}\code{$grp_blines} for the \strong{parent and child} peaks only.
#' @param opt_method One of \code{1} or \code{2}. Determines the type of error measure calculated for exponential curve.
#' When \code{1}, Euclidean distance between the exponential curve and original data in the \strong{parent peak region} is calculated.
#' When \code{2}, the difference of exponential curve \strong{endpoint} from zero is calculated.
#'
#' @return The single \code{numeric} error measure.
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{exskim}}, \code{\link{expfit}}
#'
#' @keywords internal
experr <- function(B_opt, extra_pars, marks, opt_data, opt_method = 1) { #1 or 2

  exp_res <- expfit(B = B_opt, A = extra_pars["fin_A"],
                    C = extra_pars["fin_C"], x0 = extra_pars["x0"],
                    y0 = extra_pars["y0"], input = opt_data[,"x"])

  if(opt_method == 1) {
    euc_inds <- which(opt_data[,"ind"] %in% sort(marks["infind"]:marks["vlind"]))
    euc_sig <- opt_data[euc_inds,"y"]
    euc_exp <- exp_res[euc_inds]
    err <- sum(sqrt((euc_sig-euc_exp)^2), na.rm = TRUE)
  } else if(opt_method == 2) {
    err <- abs(exp_res[which(opt_data[,"ind"] == marks["fitend"])])
  }
  return(err)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION C: Main exponential skim function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Front or tail Exponential or Gaussian Skim on a pair of peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Calculates an Exponential or Gaussian Skim for a suitable parent-child pair of chromatographic peaks.
#' For further details, see \code{\link{chrom_skim}}.
#'
#' @param peaks A \code{list} of length 2, where each element is a \code{data.frame} containing the data indices (\code{"ind"}), retention times
#' (\code{"x"}), signal (\code{"y"}), and baseline (\code{"bline"}) values for a single peak (either parent or child).
#' @param maxs A 2-row \code{data.frame} containing key data about peak apices as output from \code{\link{dtprep}}\code{$type_df$maxes}.
#' @param lin,rin Both 2-row \code{data.frame} objects containing key data about peak inflection points as output from
#' \code{\link{dtprep}}\code{$type_df$linfs} and \code{\link{dtprep}}\code{$type_df$rinfs}, respectively.
#' @param skim_type Type of skim to apply. One of \code{"front"} or \code{"tail"}.
#' @param h2 A \code{numeric} ID of the child peak. Either \code{1} or \code{2} (earlier or later peak, respectively).
#' May also be set to \code{"auto"} (default), in which case the peak with the higher signal at the apex is chosen as the parent peak.
#'
#' @return A \code{data.frame} containing original data indices (\code{"ind"}), retention time (\code{"x"}), baseline-corrected signal (\code{"y"}),
#' peak group ID (\code{"group"}), those of the parent and child peak (\code{"peak1"} and \code{"peak2"}), the skim type (one of \code{"EF"} or
#' \code{"ET"} for front and tail exponential skims; \code{"GF"} or \code{"GT"} for front and tail Gaussian skims, respectively),
#' and the combined ID used for plotting via \code{\link{integ_plot}} (\code{"combo_id"}).
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{experr}}, \code{\link{expfit}}, \code{\link{gskimerr}}, \code{\link{fit_1G}}
#'
#' @keywords internal
#'
#' @importFrom stats optim
exskim <- function(peaks, maxs, lin, rin, skim_type, h2 = "auto") {

  #Preliminary checks
  if(!any(c("front", "tail") %in% skim_type)) stop("Argument 'skim_type' must be one of: 'front', 'tail'!")
  if(unique(lapply(list(maxs, lin, rin), nrow))!=2) stop("Peak markers ('maxs', 'rin', 'lin') must be data.frames containing 2 rows!")
  if(length(peaks)!=2) stop("Peak data 'peaks' must be a list of length 2!")

  if(h2=="auto") {
    h2 <- which.min(maxs[,"acc_y"])
  } else if(!any(c("auto",1,2) %in% h2)) stop("Argument 'h2' must be one of: 1, 2, or 'auto'!")
  h1 <- if(h2==2) 1 else 2

  #Retrieve group and peak indices (for output)
  markers <- c(pthis = maxs[which.min(maxs[,"peak"]),"peak"],
               pnext = maxs[which.max(maxs[,"peak"]),"peak"],
               grp = unique(maxs[,"group"]))

  #Retrieve the parent and child peaks
  par <- peaks[[h1]]
  chd <- peaks[[h2]]

  #Combine parent and child peaks
  totreg <- Reduce(rbind.data.frame, peaks)

  #Retrieve the pivot valley
  vlind <- totreg[duplicated(totreg[,"ind"]),"ind"]
  totreg <- totreg[!duplicated(totreg),]

  #Define the pivot maximum, inflection point, and ending index of the fit region
  if(skim_type=="front") {
    pivmax <- maxs[h1, "ind"]
    infind <- lin[h1,"ind"]
    fitend <- totreg[1,"ind"]
  } else if(skim_type=="tail") {
    pivmax <- maxs[h2, "ind"]
    infind <- rin[h1,"ind"]
    fitend <- totreg[nrow(totreg),"ind"]
  }

  #Fit the initial exponential curve with a growth/decay constant of 0
  #Retrieve common baseline for the current group
  bline <- totreg[totreg[,"ind"] %in% totreg[1,"ind"]:vlind,]

  #Define initial exponential function constants
  fin_A <- slope(bline[,"x"], bline[,"bline"])
  init_B <- init_C <- 0

  #Define the starting value of the curve fit
  str_x <- totreg[totreg[,"ind"]==vlind,"x"]
  str_y <- totreg[totreg[,"ind"]==vlind,"y"]

  #Derive region for initial exponential curve fit
  fitreg <- totreg[which(totreg[,"ind"] %in% sort(infind:fitend)),]
  rownames(fitreg) <- NULL

  #Carry out the initial exponential curve fit
  #USE ONLY THE REGION BETWEEN THE CLOSEST INFLECTION POINT OF THE PARENT PEAK AND THE VALLEY/SHOULDER BOUNDARY BETWEEN THE TWO PEAKS
  exp_init <- expfit(input = fitreg[,"x"], A = fin_A, B = init_B, C = init_C, x0 = str_x, y0 = str_y)

  #Correct the initial exponential curve using constant C and derive the Euclidean Distance between it and the original signal
  #Determine the final value of constant C (y-axis offset)
  fit_C <- exp_init[which(fitreg[,"ind"] == vlind)]
  sig_C <- fitreg[which(fitreg[,"ind"] == vlind), "y"]
  fin_C <- sig_C - fit_C

  #Run an optimization function to optimize the value of the growth/decay constant B
  #Define constraint for growth/decay function based on Skim Type
  B_lims <- if(skim_type=="front") c(0, Inf) else if(skim_type=="tail") c(-Inf, 0)

  #Compile extra parameters for exponential skim optimization function
  expars <- c(fin_A = fin_A, fin_C = fin_C, y0 = str_y, x0 = str_x)
  pmarks <- c(infind = infind, vlind = vlind, fitend = fitend)

  #Carry out the optimization (begin with Euclidean as the optimization metric)
  #Switch to distance minimum difference of skimline endpoint from zero as the optimization metric if this fails with an "ERROR" in [["message"]] element
  optres <- optim(par = init_B, fn = experr, opt_data = fitreg, opt_method = 1, extra_pars = expars, marks = pmarks,
                  lower = min(B_lims), upper = max(B_lims), method = "L-BFGS-B")
  if(grepl("ERROR", optres[["message"]])) {
    optres <- optim(par = init_B, fn = experr, opt_data = fitreg, opt_method = 2, extra_pars = expars, marks = pmarks,
                    lower = min(B_lims), upper = max(B_lims), method = "L-BFGS-B")
    #if(grepl("ERROR", optres[["message"]])) stop("Both exponential skim optimization methods failed! Aborting...")
  }
  fin_B <- optres[["par"]]

  #Subset the final fit region to range from the valley separating the peak pair to the start/end of the child peak
  finreg <- fitreg[which(fitreg[,"ind"] %in% sort(vlind:fitend)),]

  #Fit the final exponential curve
  exp_fin <- expfit(input = finreg[,"x"], x0 = str_x, y0 = str_y, A = fin_A, B = fin_B, C = fin_C)

  #Test whether the lowest point of the skim is different from 0 by >1% of the parent peak height
  fitchk <- abs(min(exp_fin, na.rm = TRUE)) >= 0.01*maxs[h1,"acc_y"]

  if(!fitchk) {
    #Change values that are below 0 to 0, and those that are above the signal to the signal value
    exp_fin[exp_fin < 0] <- 0

    if(length(which(exp_fin > finreg[,"y"]))>0) {
      exp_fin[exp_fin > finreg[,"y"]] <- finreg[exp_fin > finreg[,"y"],"y"]
    }

    #Prepare the exponential curve baseline data.frame
    skimline <- cbind.data.frame(finreg[,c("ind", "x")], y = exp_fin)

    #Set the integration boundary types
    bnd_text <- paste0("E", if(skim_type == "front") "F" else if(skim_type=="tail") "T")

  } else {
    skimline <- rbind.data.frame(totreg[totreg[,"ind"]==vlind, c("ind","x","y")],
                                 c(totreg[totreg[,"ind"]==vlind, c("ind","x")], y = 0))
    colnames(skimline) <- c("ind","x","y")
    bnd_text <- "PD"
  }
  skimline <- cbind.data.frame(skimline, group = markers["grp"],
                               peak1 = markers["pthis"], peak2 = markers["pnext"], type = bnd_text)
  skimline[,"combo_id"] <- paste0(unique(skimline[,c("peak1","peak2", "type")]), collapse = "_")
  return(skimline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Carry out a Gaussian skim on a pair of peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION A: Builds a single Gaussian
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Fit a Gaussian curve (for Gaussian skimming)
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Constructs a Gaussian curve. Used for iterative optimization of Gaussian skim in \code{\link{gskim}}.
#' See \code{\link{chrom_skim}} for further details.
#'
#' @param apex Height (signal at apex) of the peak.
#' @param loc Retention time at peak apex.
#' @param sigma Constant denoting the standard deviation of the Gaussian distribution.
#' @param data A \code{numeric} vector of retention time (x-axis) values for which to construct the model.
#'
#' @return A \code{numeric} vector of the Gaussian curve equal in length to \code{data}.
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{gskimerr}}, \code{\link{gskim}}
#'
#' @keywords internal
fit_1G <- function(apex, loc, sigma, data) {
  apex*exp(-((data-loc)/(2*sigma))^2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION B: Derives optimization metric (to be minimized)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Error minimization function for exponential skim
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Calculates the Euclidean Distance between the Gaussian model and chromatographic data as error measure for
#' iterative optimization of the Gaussian skim (\code{\link{gskim}}). For further details, see \code{\link{chrom_skim}}.
#'
#' @param pars A \code{numeric} vector of parameters including, in that order, the retention time at parent peak apex,
#' the standard deviation of the Gaussian curve, and the signal (y-value) at the parent peak apex.
#' @param input A \code{data.frame} containing the output of \code{\link{dtprep}}\code{$grp_blines} for the region spanning
#' from the maximum of the \strong{parent} peak to the valley (or other boundary) between the parent and child peaks.
#'
#' @return A single \code{numeric} error measure (Euclidean Distance) between the Gaussian model and baseline-corrected chromatogram.
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{fit_1G}}, \code{\link{gskim}}
#'
#' @keywords internal
gskimerr <- function(pars, input) {

  #Build single Gaussian model
  gauss_init <- cbind.data.frame(x = input[,"x"], y = fit_1G(apex = pars[3], loc = pars[1], sigma = pars[2], data = input[,"x"]))
  metric_res <- sum(sqrt((gauss_init[,"y"]-input[,"y"])^2)) #+ (gauss_init[,"x"]-input[,"x"])^2))
  return(metric_res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION C: Main Gaussian skim function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname exskim
#' @importFrom stats optim
gskim <- function(peaks, maxs, lin, rin, skim_type, h2 = "auto") {

  #Preliminary checks
  if(!any(c("front", "tail") %in% skim_type)) stop("Argument 'skim_type' must be one of: 'front', 'tail'!")
  if(unique(lapply(list(maxs, lin, rin), nrow))!=2) stop("Peak markers ('maxs', 'rin', 'lin') must be data.frames containing 2 rows!")
  if(length(peaks)!=2) stop("Peak data 'peaks' must be a list of length 2!")

  if(h2=="auto") {
    h2 <- which.min(maxs[,"acc_y"])
  } else if(!any(c("auto",1,2) %in% h2)) stop("Argument 'h2' must be one of: 1, 2, or 'auto'!")
  h1 <- if(h2==2) 1 else 2

  #Retrieve group and peak indices (for output)
  markers <- c(pthis = maxs[which.min(maxs[,"peak"]),"peak"],
               pnext = maxs[which.max(maxs[,"peak"]),"peak"],
               grp = unique(maxs[,"group"]))

  #Retrieve the parent and child peaks
  par <- peaks[[h1]]
  chd <- peaks[[h2]]

  #Combine parent and child peaks
  totreg <- Reduce(rbind.data.frame, peaks)

  #Retrieve the pivot valley
  vlind <- totreg[duplicated(totreg[,"ind"]),"ind"]
  totreg <- totreg[!duplicated(totreg),]

  #Define the pivot maximum, inflection point, and ending index of the fit region
  if(skim_type=="front") {
    pivmax <- maxs[h1, "ind"]
    infind <- lin[h1,"ind"]
    fitend <- totreg[1,"ind"]
  } else if(skim_type=="tail") {
    pivmax <- maxs[h2, "ind"]
    infind <- rin[h1,"ind"]
    fitend <- totreg[nrow(totreg),"ind"]
  }

  #Retrieve the appropriate ACCURATE inflection point of the parent peak and its corresponding INDEX based on the Skim Type
  parinf <- if(skim_type=="front") lin[h1,] else if(skim_type=="tail") rin[h1,]

  #Retrieve the accurate peak apex (if a signal apex is not available, derive a Gaussian theoretical apex)
  #If signal apex is available but the parent peak is a shoulder, use the average of the signal apex and the theoretical apex
  apex <- if(is.na(maxs[h1,"acc_y"])) {
    cbind.data.frame(acc_x = maxs[h1,"x"], acc_y = parinf[,"y"]/0.607)
  }  else if(!is.na(maxs[h1,"acc_y"]) & maxs[h1,"ptype"] %in% c("B", "F")) {
    maxs[h1,c("acc_x", "acc_y")]
  } else if(!is.na(maxs[h1,"acc_y"]) & maxs[h1,"ptype"]=="S") {
    cbind.data.frame(acc_x = maxs[h1,"x"], acc_y = mean(c(parinf[,"y"]/0.607, maxs[h1,"acc_y"]), na.rm = TRUE))
  }

  #Estimate sigma (standard deviation) of the initial Gaussian model from inflection point times
  gsigma <- abs(parinf[,"acc_x"]-apex[,"acc_x"])

  #Retrieve input data (rtime) spanning from the parent peak maximum to the valley/shoulder boundary between parent and child peaks
  fitreg <- totreg[which(totreg[,"ind"] %in% sort(pivmax:fitend)),]
  optreg <- fitreg[fitreg[,"ind"] %in% sort(vlind:pivmax),]

  #Optimize Gaussian model using a data range from the closest parent peak apex to the start/end of the child peak
  #Minimize the Euclidean Distance between the model and the signal
  #Run optimization using 3 different algorithms and select parameters corresponding to the best results (lowest error)

  optres <- lapply(c("L-BFGS-B", "Nelder-Mead", "CG"), function(x) {
    optlims <- if(x=="L-BFGS-B") {list(c(apex[,"acc_x"]*0.90, 0, apex[,"acc_y"]*0.90), c(apex[,"acc_x"]*1.10, Inf, apex[,"acc_y"]*1.10))
    } else {list(-Inf,Inf)}
    optim(par = c(apex[,"acc_x"], gsigma, apex[,"acc_y"]), fn = gskimerr, input = optreg,
          lower = optlims[[1]], upper = optlims[[2]], method = x, control = list(maxit = 10000))
  })
  bestpars <- which.min(lapply(optres, function(x) x[["value"]]))
  optpars <- optres[[bestpars]][["par"]]

  #Build the final, optimized Gaussian model
  g_fin <- fit_1G(apex = optpars[3], loc = optpars[1], sigma = optpars[2], data = fitreg[,"x"])
  g_fin <- cbind.data.frame(fitreg[,c("ind", "x")], y = g_fin)

  #Run checks on the final model
  #Check whether the lowest point of the model is higher than >1% of parent peak height
  fitchk1 <- abs(min(g_fin[,"y"], na.rm = TRUE)) >= 0.01*maxs[h1,"acc_y"]

  #Check whether the Gaussian model is higher in value than the signal at any point(s) along the child peak (plus 3 closest points of the parent peak)
  fitspan <- which(fitreg[,"ind"] %in% sort(maxs[h2,"ind"]:vlind))
  fitchk2 <- length(which(g_fin[fitspan,"y"] > fitreg[fitspan,"y"])) > 0

  #If both checks are passed, detect the earlier/latest index (if any) where the Gaussian model is higher than the signal, based on Skim Type
  #When Skim Type is "front", detect earliest crossing point, otherwise detect the latest one
  if(!any(c(fitchk1, fitchk2))) {

    #Check for the presence of any crossing points
    crosses <- which(g_fin[g_fin[,"ind"] %in% optreg[,"ind"],"y"] > optreg[,"y"])

    if(length(crosses)>0 & !any(is.na(crosses))) {
      #Select earliest or latest crossing point and subset Gaussian skim according to Skim Type
      rel_cross <- if(skim_type=="front") min(crosses, na.rm = TRUE) else if(skim_type=="tail") max(crosses, na.rm = TRUE)
      abs_cross <- optreg[rel_cross, "ind"]
      g_fin <- if(skim_type=="front") g_fin[!g_fin[,"ind"] > abs_cross,] else if(skim_type=="tail") g_fin[!g_fin[,"ind"] < abs_cross,]

      #Check if the detected crossing point is above the inflection point
      infchk <- if(skim_type=="front") abs_cross >= parinf[,"ind"] else if(skim_type=="tail") abs_cross <= parinf[,"ind"]

      #If it is, truncate the Gaussian baseline at the inflection point.
      if(infchk) {
        g_fin <- if(skim_type=="front") g_fin[!g_fin[,"ind"] < parinf,] else if(skim_type=="tail") g_fin[!g_fin[,"ind"] > parinf,]
      }

      #Change the value at the truncation point to be equal to the signal.
      g_fin[g_fin[,"ind"] == abs_cross,"y"] <- optreg[optreg[,"ind"] == abs_cross,"y"]

      #Change any other values that are above signal to corresponding signal values
      which_higher <- which(g_fin[,"y"] > fitreg[fitreg[,"ind"] %in% g_fin[,"ind"],"y"])
      g_fin[which_higher,"y"] <- fitreg[which_higher,"y"]

      #Check for any sub-zero values and change them to zero
      #g_fin[g_fin[,"y"] < 0, "y"] <- 0
    }

    #Prepare the Gaussian curve baseline data.frame
    skimline <- g_fin

    #Set the integration boundary types
    bnd_text <- paste0("G", if(skim_type == "front") "F" else if(skim_type=="tail") "T")

  } else {
    skimline <- rbind.data.frame(totreg[totreg[,"ind"]==vlind, c("ind","x","y")],
                                 c(totreg[totreg[,"ind"]==vlind, c("ind","x")], y = 0))
    colnames(skimline) <- c("ind","x","y")
    bnd_text <- "PD"
  }
  skimline <- cbind.data.frame(skimline, group = markers["grp"],
                               peak1 = markers["pthis"], peak2 = markers["pnext"], type = bnd_text)
  skimline[,"combo_id"] <- paste0(unique(skimline[,c("peak1","peak2", "type")]), collapse = "_")
  return(skimline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Derive PDrop baseline from peak starts and ends
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Derive Perpendicular Drop line
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Compiles data necessary for plotting a Perpendicular Drop line via \code{\link{integ_plot}}.
#'
#' @param str,end A 2-row \code{data.frame} containing key information about parent and child peak \strong{start} and \code{end} boundaries as output from
#' \code{\link{dtprep}}\code{$type_df$starts} and \code{\link{dtprep}}\code{$type_df$ends}, respectively.
#'
#' @return A \code{data.frame} containing original data indices (\code{"ind"}), retention time (\code{"x"}), baseline-corrected signal (\code{"y"}),
#' peak group ID (\code{"group"}), those of the parent and child peak (\code{"peak1"} and \code{"peak2"}), the skim type (one of \code{"PD"}),
#' and the combined ID used for plotting via \code{\link{integ_plot}} (\code{"combo_id"}).
#'
#' @seealso \code{\link{chrom_skim}}, \code{\link{integ_plot}}
#'
#' @keywords internal
pdrop <- function(str, end) {
  #Preliminary checks
  if(length(unique(lapply(list(str, end), nrow)))>1) stop("Peak starts ('str') and ends ('end') must both be data.frames with 2 rows!")

  #Check that the two peaks share a boundary
  vlind <- which(str[,"ind"] %in% end[,"ind"])
  if(length(vlind)==0) stop("Peaks do not share a boundary! Aborting...") else vlind <- str[vlind, "ind"]

  #Compile perpendicular drop individual baseline (for plotting)
  dpline <- rbind.data.frame(str[str[,"ind"]==vlind, c("ind","x","y")],
                             c(str[str[,"ind"]==vlind, c("ind","x")], y = 0))
  colnames(dpline) <- c("ind","x","y")

  dpline <- cbind.data.frame(dpline, group = unique(str[,"group"]),
                             peak1 = str[which.min(str[,"peak"]),"peak"],
                             peak2 = str[which.max(str[,"peak"]),"peak"],
                             type = "PD")
  dpline[,"combo_id"] <- paste0(unique(dpline[,c("peak1","peak2", "type")]), collapse = "_")
  return(dpline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Compile final data for trapezoidal integration after integration skimming
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compile Perpendicular Drop or Tangent/Exponential/Gaussian skim data for trapezoidal integration
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_skim}} workflow, not intended for standalone use.
#'
#' Compiles data for integration via the Trapezoidal Rule as part of the workflow. See \code{\link{chrom_skim}} for further details.
#'
#' @param input A \code{data.frame} containing the row-bound results of skimming functions (any of \code{\link{tskim}}, \code{\link{exskim}},
#' \code{\link{gskim}}, and/or \code{\link{pdrop}}) for all peaks to be integrated.
#' @param integ_data A \code{list} of \code{data.frame} objects, each containing data indices (\code{"ind"}), retention time (\code{"x"}),
#' and baseline-corrected signal (\code{"y"}) for an individual (often skimmed) peak to be used for integration.
#'
#' @return A \code{list} of \code{data.frame} objects, each containing the necessary xy-coordinates (retention time and baseline-corrected signal)
#' for an individual peak to be used for trapezoidal integration via \code{\link{integ}}.
#
#' @seealso \code{\link{chrom_skim}}, \code{\link{tskim}}, \code{\link{exskim}}, \code{\link{gskim}}, \code{\link{pdrop}}, \code{\link{integ}}
#'
#' @keywords internal
skimcomp <- function(input, integ_data) {
  #Preliminary checks
  if(!all(c("ind", "x", "y", "group", "peak1", "peak2", "type", "combo_id") %in% colnames(input))) stop("Input data must be a data.frame output from one of the integration skims: 'tskim', 'exskim', or 'gskim'!")

  pks <- lapply(input[,c("peak1","peak2")], unique)
  pks <- lapply(pks, sort)
  if(length(integ_data)<=length(pks[[1]])) stop("The length of provided integration data list must be greater than the number of unique peak pairs in the 'input' data.frame!")

  #Compile skimmed baseline data and corresponding integration data
  #Loop through pairs of peaks
  for(i in pks[[1]]) {

    #Retrieve data for one peak pair
    skln <- input[input[,"peak1"]==i,]

    #Determine skim type
    skim_type <- substr(unique(skln[,"type"]),2,2)

    if(any(c("F","T") %in% skim_type)) {

      #Determine parent and child indices
      if(skim_type=="F") {h2 <- i; h1 <- i+1} else if(skim_type=="T") {h2 <- i+1; h1 <- i}

      #Retrieve peaks j and j+1 from integ_list
      par <- integ_data[[h1]]
      chd <- integ_data[[h2]]

      #Determine overlapping points between parent peak and skim line
      overlap <- par[which(par[,"ind"] %in% skln[,"ind"]),"ind"]

      #Determine duplicated index in peaks i and i+1
      dup <- par[par[,"ind"] %in% chd[,"ind"],"ind"]

      #CHILD PEAK
      #Add overlapping signal to child peak
      chd <- rbind.data.frame(chd[!chd[,"ind"]==dup,], par[par[,"ind"] %in% overlap,])
      chd <- chd[order(chd[,"ind"]),]

      #Subtract integration baseline from the "expanded" child peak
      chd_in_skln <- chd[,"ind"] %in% skln[,"ind"]
      chd[chd_in_skln,"y"] <- chd[chd_in_skln,"y"] - skln[,"y"]

      #PARENT PEAK
      #Replace the overlapping region with baseline values
      par[par[,"ind"] %in% overlap,] <- skln[skln[,"ind"] %in% overlap, colnames(par)]

      #Add the remaining baseline values (belonging to the child peak)
      par <- rbind.data.frame(par, skln[!skln[,"ind"] %in% overlap, colnames(par)])
      par <- par[order(par[,"ind"]),]

      #Append modified (or not) peaks back to their slot in integ_list
      integ_data[c(h1,h2)] <- list(par, chd)
    }
  }
  return(integ_data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Integration via the Trapezoidal Rule
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Trapezoidal Rule integration of data
#'
#' @description Carries out integration of data using the Trapezoidal Rule.
#' See \code{\link{chrom_skim}} for further details.
#'
#' @param input Either a \code{data.frame} or a \code{list} of \code{data.frame} objects containing \strong{xy} data to be integrated.
#' @param vars A \code{character} vector of length 2 specifying column names of \strong{x-} and \strong{y-}data included in \code{input}.
#' Defaults to \code{c("x","y")}.
#' @param slnt A \code{logical} (defaults to \code{FALSE}). Specifies whether information about the function output is printed in the console.
#'
#' @return A \code{numeric} vector of integrated peak areas, each element corresponding to that of \code{input}
#' (or a single \code{numeric} peak area if \code{input} is a \code{data.frame}).
#'
#' @seealso \code{\link{chrom_skim}}
#'
#' @keywords internal
integ <- function(input, vars = c("x", "y"), slnt = FALSE) {
  #Preliminary checks
  if(!is.list(input) & !is.data.frame(input)) {
    stop("Data for integration was not provided in the correct format (data.frame or list thereof)!")
  } else if(is.data.frame(input)) input <- list(input)

  #Create output vector
  integ_res <- c()

  #Integrate all results (by looping through integ_list)
  if(!slnt) cat("\nCarrying out Trapezoidal Rule integration...")
  for(i in seq_along(input)) {
    inp <- input[[i]]
    integ_res[i] <- sum(sapply(seq(nrow(inp)-1), function(x) (inp[x+1,vars[1]]-inp[x,vars[1]])*(inp[x+1,vars[2]]+inp[x,vars[2]])/2), na.rm = TRUE)
  }
  return(integ_res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Visualize integration baselines on a graph
#Visualize the results (including group/common and individual baselines)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Visualize traditional integration results
#'
#' @description Visualizes the results of traditional integration of data obtained from \code{\link{chrom_skim}}, including the peak apices,
#' group baselines, as well as perpendicular drop and/or tangent/exponential/Gaussian front or tail skim lines.
#'
#' @param input The \code{list} of data output from \code{\link{chrom_skim}}.
#' @param cols Plot colour palette. Either \code{"default"} or a \strong{named} \code{character} vector of colours.
#' Names denote the following plot elements: main chromatogram (\code{"main"}), peak group baselines (\code{"grp"}),
#' perpendicular droplines (\code{"pd"}), tangent skim lines (\code{"ts"}), exponential skim lines (\code{"es"}),
#' and Gaussian skim lines (\code{"gs"}).
#' @param plabs Plot labels. Either \code{"default"} or a \strong{named} \code{character} vector of labels for the following elements:
#' main plot title (\code{"main"}), retention time (\code{"x"}), and signal (\code{"y"}).
#' @param norm A \code{logical} specifying whether to normalize the chromatographic signal (y-axis) to a percentage scale (0 to 100).
#' Defaults to \code{FALSE}.
#' @param plot_max A \code{logical} switch specifying whether to show peak apices with shape markers. Defaults to \code{FALSE}.
#' @param txt_max A \code{logical} switch specifying whether to plot peak numbers at apices. Defaults to \code{FALSE}.
#' @param asprat Aspect ratio of the plot (defaults to \code{0.71}).
#'
#' @return A \code{ggplot}-class object containing the plot.
#'
#' @seealso \code{\link{chrom_skim}}
#'
#' @examples
#' \dontrun{
#' #Get and plot data
#' skim_res <- lcqc:::wf_ints
#' integ_plot(skim_res, plot_max = TRUE)
#'
#' #With y-axis normalisation to 0-100 and text peak labels
#' integ_plot(skim_res, plot_max = TRUE, plabs = c(main = ""), txt_max = TRUE, norm = TRUE)
#' }
integ_plot <- function(input, cols = "default", plabs = "default", norm = FALSE,
                       plot_max = FALSE, txt_max = FALSE, asprat = 0.71) { #c("red", "darkred", "blue", "darkgreen", "purple")

  #Preliminary checks
  #Check for input object
  if(!all(c("orig_data", "indiv_bln", "grp_bln", "integ_res", "max_marks", "information", "call") %in% names(input))) stop("Input data must be the output object from function 'chrom_skim'!")

  #Set up plot colours
  defcols <- c(main = "black", grp = "red", pd = "darkred", ts = "blue", es = "darkgreen", gs = "purple")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Set up plot labels
  deflabs <- c(main = "Truncated chromatogram with integration baselines", x = "Time (min)", y = "Signal")
  plabs <- supp_pars(pars = plabs, defpars = deflabs, parlb = "plabs")

  #Extract plot data from input object
  main <- input[["orig_data"]]
  grp <- input[["grp_bln"]]
  indiv <- input[["indiv_bln"]]
  maxs <- input[["max_marks"]]

  #Correct the individual integration baseline data for common baselines
  if(length(indiv)>0) {
    for(i in unique(indiv[,"ind"])) {
      orig_row <- which(indiv[,"ind"]==i)
      match_row <- which(grp[,"ind"] %in% indiv[orig_row,"ind"])
      indiv[orig_row,"y"] <- if(length(match_row)>0) indiv[orig_row,"y"]+grp[match_row,"bline"] else indiv[orig_row,"y"]
    }

    #Create a truncated version of the individual baseline data.frame for data markers
    indiv_trunc <- Reduce(rbind.data.frame, lapply(unique(indiv[,"combo_id"]), function(x) {
      sset <- indiv[indiv[,"combo_id"]==x,]
      return(sset[c(1,nrow(sset)),])
    }))
  }

  #Optionally normalize all data to a scale of 0 to 100
  if(norm) {
    #Get the maximum of y signal
    maxsig <- max(main[,"y"], na.rm = TRUE)
    main[,"y"] <- main[,"y"]/maxsig*100
    grp[,"bline"] <- grp[,"bline"]/maxsig*100
    maxs[,"orig_y"] <- maxs[,"orig_y"]/maxsig*100
    indiv[,"y"] <- indiv[,"y"]/maxsig*100
    indiv_trunc[,"y"] <- indiv_trunc[,"y"]/maxsig*100
  }

  #Create the plot
  cat("\nPlotting integration results...\n")

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_xvar <- "x"
  aes_yvar <- "y"
  aes_bline <- "bline"
  aes_grpvar <- "group"
  aes_ory <- "orig_y"

  integplot <- ggplot(data = main, aes(x = .data[[aes_xvar]], y = .data[[aes_yvar]])) +
    geom_path(colour = cols["main"]) +
    geom_line(data = grp, aes(y = .data[[aes_bline]], group = .data[[aes_grpvar]]), col = cols["grp"], lwd = 1.02) +
    { if(txt_max) geom_text(data = maxs, aes(y = .data[[aes_ory]]), label=maxs[,"peak"], col = cols["main"], nudge_x = -0.005*max(maxs[,"x"], na.rm = TRUE), nudge_y = 0.04*max(main[,"y"], na.rm = TRUE), check_overlap = TRUE) } +
    { if(plot_max) geom_point(data = maxs, aes(y = .data[[aes_ory]]), col = cols["main"], pch = 20, size = 3) } +
    scale_x_continuous(breaks = breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = breaks_pretty(n = 6)) +
    coord_cartesian(xlim = c(main[min(grp[,"ind"], na.rm = TRUE)-3,"x"],
                             main[max(grp[,"ind"], na.rm = TRUE)+3,"x"])) +
    labs(title = if(plabs["main"]=="") NULL else plabs["main"],
         x = plabs["x"],
         y = plabs["y"]) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour = "black"))

  if(length(indiv)>0) {

    #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
    aes_grpvar2 <- "combo_id"
    aes_colvar <- "type"

    integplot <- integplot +
      geom_line(data = indiv, aes(y = .data[[aes_yvar]], group = .data[[aes_grpvar2]], colour = .data[[aes_colvar]]), lwd = 1) +
      geom_point(data = indiv_trunc, aes(y = .data[[aes_yvar]]), pch = 20, size = 3) +
      scale_colour_manual(values = c("PD" = cols[["pd"]],
                                     "TF" = cols[["ts"]], "TT" = cols[["ts"]],
                                     "EF" = cols[["es"]], "ET" = cols[["es"]],
                                     "GF" = cols[["gs"]], "GT" = cols[["gs"]]))
  }
  #Return the plot
  return(integplot)
}
