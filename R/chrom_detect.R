#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Add peak names to a chromo_detect output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Add peak names to \code{\link{chrom_detect}} output
#'
#' @description A helper function that adds peak names to existing output of \code{\link{chrom_detect}}.
#'
#' @param x The output of \code{\link{chrom_detect}} to add peak names to.
#' @param nms A \code{character} vector of peak names. Must correspond to peak indices given in \code{whichpks} (if provided).
#' @param whichpks An \strong{optional} \code{numeric} vector of peak indices to include names for. Must be within the range
#' of indices included in \code{x}. Defaults to \code{NA}, in which case \code{nms} must be provided for all peaks.
#'
#' @return A list of the same structure as that output by \code{\link{chrom_detect}} but including peak names.
#' @export
#'
#' @examples
#' \dontrun{
#' dt <- lcqc:::wf_detpeaks
#' pnms <- paste0("Ex",seq(7))
#' res <- addnms(dt,pnms)
#' }
addnms <- function(x, nms, whichpks = NA) {
  chkdt(x)
  pt <- x[["results"]][["Peak_Extents"]]
  if(any(colnames(pt) %in% "Compound")) cat("\nThe input data already includes peak names! These will be updated...")
  if(all(is.na(whichpks))) whichpks <- seq(nrow(pt))
  if(max(whichpks)>nrow(pt)|min(whichpks)<1) stop("Peak indices provided in 'whichpks' must be within the input data range!")
  if(!is.character(nms)|(length(nms)!=length(whichpks))) {
    stop("The length of peak names vector 'nms' must be equal to the number of input peaks or 'whichpks' (if provided)!")
  }
  #Begin processing
  mischk <- which(!whichpks %in% seq(nrow(pt))) #Check which peak indices are missing from 'whichpks'
  if(length(mischk)>0) nms[mischk] <- "Unknown"
  x[["results"]][["Peak_Extents"]] <- cbind.data.frame(Compound = nms, pt[,!colnames(pt) %in% "Compound"])
  return(x)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Detect, filter, and classify peaks automatically
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Automatic detection and classification of chromatographic peaks
#'
#' @description This function uses a combination of various custom and industry-standard algorithms to automatically detect, filter, and classify
#' chromatographic peaks using 1-dimensional data (only retention time and signal). This is the most complex function in \pkg{lcqc}.
#'
#' @param chrom A \code{data.frame} containing the chromatogram. Must include x- and y-axis data variables as listed in \code{vars}. When \code{NA},
#' these are assumed to be contained within the first two columns of the \code{data.frame}.
#' @param vars Variable names of \code{chrom} corresponding to the retention time (x-axis) and signal (y-axis), in that order.
#' Defaults to \code{c("Time","Signal")}.
#' @param trange A \code{numeric} vector containing the retention time range of \code{chrom} to apply the workflow to. By default, the entire
#' chromatogram is processed.
#' @param det_bunch A \code{logical} switch. Should peak bunching be detected and mitigated? Defaults to \code{TRUE}.
#' @param pnms \strong{Optional} \code{character} vector of peak names. Defaults to \code{NA}.
#' The number of peaks detected by the algorithm must be known before providing this parameter.
#' Thus, a "practice run" is recommended before any names are set. Names may also be added to existing \code{chrom_detect} output
#' via the helper function \code{\link{addnms}}.
#' @param bline_method Baseline correction method to use (if any). Defaults to \code{"none"}. Possible methods are:
#' \code{"als"}, \code{"chang"}, \code{"poly"}, or \code{"isrea"}. See \code{\link{chrom_bline}} for details.
#' @param bline_pars Parameters of the baseline correction. Set to \code{"default"} (sensible defaults. Otherwise, a \strong{named} \code{numeric}
#' vector of parameters unique to the chosen baseline correction method (\code{bline_method}). See \code{\link{chrom_bline}} for further details.
#' @param smooth A \code{character} vector of length 2 specifying the smoothing method to use for signal and first/second derivatives, respectively.
#' Defaults to \code{rep("tri",2)} (triangular smoothing for both). A single \code{character} value may also be provided and the same method is then
#' applied to both signal and derivatives. For further details, see \code{\link{chrom_smooth}}.
#' @param mpts A \code{numeric} vector of length 2 specifying the number of points to survey on either side to confirm (or reject) detected
#' signal (\strong{first element}) and derivative (\strong{second element}) extremes (i.e. maxima and minima). A single \code{numeric} value
#' may also be provided and will then be used for both data types. Defaults to \code{c(3,3)}.
#' @param crosspts A \code{numeric} vector of length 2 specifying the number of points to survey on either side to confirm (or reject) detected
#' zero crossings (downcrosses and upcrosses) for signal (\strong{first element}) and derivatives (\strong{second element}).
#' @param ma_pts,ma_passes The \code{numeric} vector of length 2 specifying the number of smoothing \strong{points} (\code{ma_pts})
#' and \strong{passes} (\code{ma_passes}) to use to calculate smoothed signal and derivatives using the method(s) given in \code{smooth}.
#' The first element may also be set to \code{"auto"}, in which case \code{ma_pts} and/or \code{ma_passes} will be \strong{automatically} determined
#' via \code{\link{chrom_width}}. The second element must then be a \strong{number} specifying the starting points/passes to use in
#' \code{\link{chrom_width}}. The defaults for \code{ma_pts} and \code{ma_passes} are \code{c("auto",7)} and \code{c("auto",1)}, respectively.
#' @param sens A \code{numeric} vector of sensitivity parameters used for fine-tuning the first and second derivative peak detection thresholds
#' via function \code{\link{chrom_derlims}} (first two elements, respectively), and the amplitude limit obtained via \code{\link{chrom_amplim}}
#' (when \code{amp_thres} includes \code{"zscore"}). Defaults to \code{c(2,1,3)}.
#' @param amp_thres A \code{character} value/vector specifying method(s) to use for determination of the amplitude limit for peak detection.
#' \strong{One or more} of: \code{"quant"}, \code{"diff"}, or \code{"zscore"} (see \code{\link{chrom_amplim}}) for details).
#' @param ampfrac A parameter for \code{\link{chrom_amplim}} that helps determine the amplitude threshold via method(s) specified in \code{amp_thres}.
#' @param der_thres A \code{character} vector of length 2 specifying the methods to use for determination of derivative-based peak detection thresholds
#' via \code{\link{chrom_derlims}}. The \strong{first} element must be one of \code{"vaz"}, \code{"zscore"}, or \code{"ncore"}, while possible values
#' for the \strong{second} element are \code{"none"}, \code{"all"}, \code{"iqr"}, \code{"quant"}, or \code{"sd"}.
#' @param apex_pars The \strong{liftoff} and \strong{touchdown} parameters to use for ApexTrack baseline expansion (see \strong{Details}, \strong{References}, and \code{\link{apexbnds}} for details).
#' @param rej A \strong{named} \code{numeric} vector specifying peak filters to apply for selective peak removal (carried out in two stages, before and after boundary detection and classification).
#' Possible names are:
#' \enumerate{
#' \item For \strong{pre-}classification (filtering round 1): minimum height (\code{"ht"}), S/N ratio (\code{"sn"}).
#' \item For \strong{post-}classification (filtering round 2): inflection point width (\code{"wd"}) and peak area (\code{"pa"}).
#'}
#' All values default to \code{NA} (no filters are applied).
#' @param asprat Aspect ratio of plots (defaults to \code{0.71}).
#' @param rej_logic A \code{character} vector of length 2 specifying the logic to use for two rounds of peak filtering (\strong{pre-} and \strong{post-}classification).
#' Possible values are \code{"OR"} or \code{"AND"}. Defaults to \code{rep("OR",2)}. A single value may also be provided, in which case the same logic will
#' be applied for both rounds of peak filtering.
#' @param zscore_pars Either \code{"default"} or a \code{list} of \code{numeric} vectors providing sequences of 3 parameters to use for \code{\link{z_optim}}.
#' @param plot_corr A \code{logical} switch. Should the baseline-corrected signal be plotted?
#' When \code{FALSE} (default), the original signal is plotted instead.
#' @param plotset A \code{character} string specifying whether data is visualized/shown.
#' One of \code{"make"} (generates plots without printing; default), \code{"print"} (generates and prints plots), or \code{"none"}.
#'
#' @return A \strong{list of lists} containing named elements \code{results} and \code{plots}.
#' The latter includes 3 \code{ggplot}-class plot objects including the chromatogram with detected peak markers as well as both first and second derivative-based noise plots.
#' Element \code{results} contains the following elements in a \code{list}:
#' \describe{
#'  \item{Chromatogram}{The chromatogram \code{data.frame} containing retention times, signal, baseline-corrected and/or smoothed signals (if any),
#'  original and (if any) smoothed first/second derivatives, and peak ID assignments.}
#'  \item{Derivative_Noise}{A 2-column \code{data.frame} containing the first and second derivative noise from \code{\link{chrom_derlims}}.}
#'  \item{Peaks}{A \code{list} of \code{data.frame} objects, each containing data for an individual detected peak. This includes retention time, original and (if any) smoothed signals, and associated first/second derivatives.}
#'  \item{Peak_Extents}{A \code{data.frame} containing key information about the detected peaks as detailed in \code{\link{peakfind}} (such as peak types, boundary types, inflection and upslope points etc.).}
#'  \item{Amplitude_Limit}{The \code{numeric} peak detection amplitude limit.}
#'  \item{Derivative_Limits}{A \code{list} with lower and upper peak detection thresholds derived from first and second derivatives (elements \code{FD} and \code{SD}, respectively).}
#'  \item{Zscore_Limits}{A \code{numeric} vector of starting and ending retention times of peak regions derived from \code{\link{z_optim}}.}
#'  \item{information}{A \code{character} string containing summary information about the results.}
#'  \item{call}{The function call.}
#' }
#' @export
#'
#' @references
#' Pirttilä, K., Balgoma, D., Rainer, J., Pettersson, C., Hedeland, M., Brunius, C. (2022), 'Comprehensive Peak Characterisation (CPC) in Untargeted LC-MS Analysis', \emph{Metabolites} \strong{12} (2), article 137, DOI: \url{https://www.doi.org/10.3390/metabo12020137}.
#'
#' Waters Corporation (2017), 'Empower Software Data Acquisition and Processing Theory Guide', document 715005481 (Rev. A), available at: \url{https://support.waters.com/KB_Inf/Empower_Breeze/WKB57375_Empower_3_-_How_to_acquire_and_process_data (accessed 19.04.2024)}.
#'
#' @seealso This workflow uses a multitude of exported and \strong{un}exported functions:
#'\describe{
#' \item{Exported}{\code{\link{chrom_width}}, \code{\link{chrom_smooth}}, \code{\link{chrom_bline}}, \code{\link{chrom_deriv}}, \code{\link{z_optim}},
#' \code{\link{chrom_amplim}}, \code{\link{chrom_derlims}}}
#' \item{Unexported}{\code{\link{peakmark}}, \code{\link{peakfind}} and its constituent functions}
#'}
#'
#' @examples
#' #LC chromatogram
#' reslc1 <- chrom_detect(lcqc::simlc1, vars = c("Time","Signal"))
#'
#' #GC chromatogram
#' resgc <- chrom_detect(lcqc::exgc2, vars = c("Time","Signal"), det_bunch = FALSE)
#'
#' #GC chromatogram with baseline correction
#' resgc2 <- chrom_detect(lcqc::exgc1, vars = c("Time","Signal"),
#' bline_method = "als", det_bunch = FALSE)
#'
#' @importFrom data.table fread
chrom_detect <- function(chrom, vars = c("Time", "Signal"), trange = range(chrom[,vars[1]]), det_bunch = TRUE, pnms = NA,
                         bline_method = "none", bline_pars = "default", smooth = rep("tri",2), mpts = c(3,3), crosspts = c(2,2),
                         ma_pts = c("auto",7), ma_passes = c("auto",1), sens = c(2,1,3), amp_thres = "zscore", ampfrac = 0.05,
                         der_thres = c("ncore", "iqr"), apex_pars = c(0,0.5), rej = c(wd = "auto", pa = NA, sn = NA, ht = NA),
                         asprat = 0.71, rej_logic = rep("OR",2), zscore_pars = "default", plot_corr = FALSE, plotset = "make") {

  #Generate function call
  cl_rec <- match.call()

  #Load data
  if(!is.object(chrom) & is.character(chrom)) {
    if(file.exists(chrom) & grepl("\\.csv$", chrom)) chrom <- fread(chrom, data.table = FALSE)
  }

  #Preliminary checks
  if(any(zscore_pars %in% "default")) zscore_pars <- list(seq(0.01, 0.05, 0.005), seq(1, 6, 0.5), c(0.02, 0.005))

  if(!is.character(rej_logic)|!all(rej_logic %in% c("OR", "AND"))){
    stop("Argument 'rej_logic' must be a character vector of length 2 with possible values: 'AND', 'OR'!")
  } else if(length(rej_logic)!=2) {
    cat("Argument 'rej_logic' must be a character vector of length 2 but instead is of length ", length(rej_logic), "! Duplicating first element...")
    rej_logic <- rep(rej_logic[1],2)
  }

  if(!is.logical(plot_corr)) stop("Argument 'plot_corr' must be logical!")

  if((length(zscore_pars)!=3 & is.list(zscore_pars)) | (length(zscore_pars)!=2 & is.atomic(zscore_pars)) | (!is.atomic(zscore_pars) & !is.list(zscore_pars))) {
    stop("Z-score algorithm parameters ('zscore_pars') must be either a numeric list of length 3 or numeric vector of length 2!")
  }

  if(!all(bline_method %in% c("als", "chang", "poly", "isrea", "none"))) {
    stop("Baseline correction method not recognised! Available options are: 'als', 'chang', 'poly', 'isrea', 'none'!")
  }

  if(!all(smooth %in% c("rect", "tri", "sg_quad", "sg_quart", "none"))) {
    stop("The type of smoothing to be used for the signal is not provided in the correct format! Allowed values: 'rect', 'tri', 'sg_quad', 'sg_quart', 'none'...")
  }

  if(any(smooth[2] %in% c("sg_quad", "sg_quart"))) {
    stop("Savitsky-Golay smoothing ('sg_quad' or 'sg_quart') is not supported for derivatives! Use 'tri' or 'rect' instead!")
  }

  if(!any(c("quant", "diff", "zscore") %in% amp_thres) & !is.numeric(amp_thres)) {
    stop("Amplitude threshold method not recognised! Possible character values: 'diff', 'zscore', and/or 'quant'!")
  } else if(is.numeric(amp_thres)) {
    cat("\nA numeric value of 'amp_thres' was entered! Treating as a manual threshold...")
  } else if(any(amp_thres %in% c("diff","quant")) & !is.numeric(ampfrac)) stop("Argument 'ampfrac' must be a numeric value when 'amp_thres' includes 'diff' or 'quant'!")

  if(!any(c("vaz", "zscore", "ncore") %in% der_thres[1])) {
    stop("Derivative threshold calculation method not recognized! Available methods: 'vaz', 'zscore', 'ncore'.")
  }

  if(is.character(der_thres) & length(der_thres)==1) {
    cat("\nDerivative threshold refinement method not provided! Using the default value: 'iqr'.")
    der_thres[2] <- "iqr"
  }

  if(!any(c("all", "iqr", "quant", "sd", "none") %in% der_thres[2])) {
    stop("Derivative threshold refinement method not recognized! Available methods: 'all', 'none', 'iqr', 'quant', 'sd'.")
  }

  if(!any(vars %in% "auto") & length(vars)<2) {
    stop("Time and signal variable names are not provided in the correct format!")
  } else if(any(vars %in% "auto")) {
    vars <- colnames(chrom)[1:2]
  }
  data <- cbind.data.frame(chrom[,vars[1]], chrom[,vars[2]])
  colnames(data) <- vars

  if(!is.numeric(ma_pts) & !any(ma_pts %in% "auto")) {
    stop("Argument 'ma_pts' must be a numeric vector of length 2 or set to 'auto'!")
  } else if(is.numeric(ma_pts)) {
    if(any(is.even(ma_pts))) {
      cat("\nThe number of moving average or Savitsky-Golay smoothing points must be an odd integer! Adjusting...")
      ma_pts[which(is.even(ma_pts))] <- ma_pts[which(is.even(ma_pts))]-1
      cat("\nNumbers of points for smoothing adjusted to: ", paste0(ma_pts, collapse = ", "), "...", sep = "")
    }
  } else if(is.numeric(ma_pts) & length(ma_pts)!=2) {
    cat("\nParameter 'ma_pts' is of incorrect length. Duplicating the first element...")
    ma_pts <- rep(ma_pts[1], 2)
  } else if(any(ma_pts %in% "auto") & length(ma_pts)!=2) stop("When 'ma_pts' is set to 'auto', the second element must be a numeric start point!")

  if(!is.numeric(mpts) | length(mpts)!=2) stop("The consecutive points required to define maxima/minima peak markers ('mpts') must be a numeric vector of length 2!")
  if(!is.numeric(crosspts) | length(crosspts)!=2) stop("The consecutive points required to define derivative up-/down-crosses ('crosspts') must be a numeric vector of length 2!")

  if(!is.numeric(ma_passes) & !any(ma_passes %in% "auto")) {
    stop("The number of passes for smoothing should either be a numeric vector of length 3, or set to 'auto'!")
  } else if(is.numeric(ma_passes) & length(ma_passes)!=3) {
    cat("\nNumeric parameter 'ma_passes' is of incorrect length (must be of length 3). Duplicating the first element...")
    ma_passes <- rep(ma_passes[1], 3)
  } else if(any(ma_passes %in% "auto") & length(ma_passes)!=2) stop("When 'ma_passes' is set to 'auto', the second element must be a numeric start point!")

  if(!is.numeric(sens)) {
    stop("The sensitivity parameter 'sens' must be a numeric vector of length 3!")
  } else if(length(sens)!=3) {
    cat("\nThe senstivity parameter 'sens' is of incorrect length (must be of length 3). Duplicating the first element...")
    sens <- rep(sens[1], 3)
  }

  if(length(unique(round(diff(data[,1])),2))>2 & any(c("sg_quad", "sg_quart") %in% smooth[1])) {
    cat("\nSavitsky-Golay (SG) smoothing only possible for regularly-spaced data! Switching to Rectangular ('rect')...")
    smooth <- rep("rect", 2)
  }

  if(!is.atomic(rej) | length(rej)>4 | length(rej)<1 | !all(names(rej) %in% c("wd", "pa", "sn", "ht"))) {
    stop("List of rejection thresholds ('rej') must be a NAMED vector of length 1-4! Possible names are: 'wd', 'pa', 'sn', 'ht'.")
  } else {
    def_rej <- c(wd = "auto", pa = NA, sn = NA, ht = NA)
    rejmatch <- which(c("wd", "pa", "sn", "ht") %in% names(rej))
    def_rej <- def_rej[-rejmatch]
    rej <- c(rej[rejmatch], def_rej)
    w_rej <- if(!any(is.na(rej[["wd"]])) & rej[["wd"]]!="auto") as.numeric(rej[["wd"]]) else rej[["wd"]]
    pa_rej <- as.numeric(rej[["pa"]])
    sn_rej <- as.numeric(rej[["sn"]])
    ht_rej <- as.numeric(rej[["ht"]])
  }

  #Create a vector to store information
  information <- c()

  #Time range setting
  if(is.numeric(trange) & length(trange)==2) {
    information <- paste0(information, "The chromatogram was truncated between RT thresholds: ", paste0(round(trange,3), collapse = ", "), ".")
    data <- data[data[,1]>=trange[1] & data[,1]<=trange[2],]
    if(nrow(data)==0) stop("Incorrect retention time range! Please adjust...")
    rownames(data) <- NULL
  }

  #Preparing RT and signal data
  sig <- data[,2]
  rtime <- data[,1]

  #Automatic determination of smoothing windows and passes
  autowd <- chrom_width(rtime = rtime, sig = sig, bcorr = bline_method, bpars = bline_pars, smooth_method = if(all(smooth %in% "none")) c("tri","tri") else smooth,
                        start_smooth = c(as.numeric(ma_pts[2]), as.numeric(ma_passes[2])), ampfrac = 0.05)

  #Retrieving the inflection point width in case the peak rejection width criterion is set to "auto"
  if(!is.na(w_rej) & w_rej=="auto") {
    w_rej <- autowd[["width_inf"]]
    information <- paste0(information, paste0("\nAn average inflection point width of ", w_rej, " points was calculated and later used for width-based peak filtering."))
  }

  if(any(c(ma_pts, ma_passes) %in% "auto") & !all(smooth %in% "none")) {
    cat("\nAuto-estimating smoothing window width and/or number of smoothing passes...")
    if(any(ma_pts %in% "auto")) {
      ma_pts <- rep(autowd["points"], 2)
      pt_text <- unique(ma_pts)
    } else pt_text <- "No calculation carried out for "

    if(any(ma_passes %in% "auto")) {
      ma_passes <- rep(autowd["passes"], 3)
      pass_text <- unique(ma_passes)
    } else pass_text <- "No calculation carried out for "

    cat("\nCalculated values were: ", pt_text, " points and ", pass_text, " passes!", sep = "")
    autoest_suffix <- "(estimated automatically via 'chrom_width')."
  } else autoest_suffix <- "(set manually)."

  #Create plot output list
  plot_list <- list()

  #Baseline Correction
  if(bline_method!="none") {

    #Information about background correction
    bgtxt <- c(poly = "Modified Polynomial Fit", chang = "Chang", als = "Asymmetric Least Squares", isrea = "Iterative Smoothing-splines with Root Error Adjustment")
    information <- paste0(information, "\nInitial background correction was done via method: ", bgtxt[bline_method], " ('", bline_method, "').")

    cat("\nThe following baseline correction method was applied: '", bline_method, "'!\n", sep = "")
    bline_res <- chrom_bline(sig, method = bline_method, pars = bline_pars, asprat = asprat) #plotres = "none"
    data[,paste0(colnames(data)[2], "_", bline_method)] <- csig <- bline_res[["Corrected_Signal"]]
    if(plotset!="none") plot_list[["Baseline_Plot"]] <- bline_res[["Bline_Plot"]]
  } else {
    information <- paste0(information, "\nNo initial baseline correction was applied.")
    cat("\nNo background correction was applied to the signal!")
    data[,paste0(colnames(data)[2], "_", bline_method)] <- csig <- sig
  }

  #Chromatogram smoothing
  if(smooth[1]!="none") {

    #Information about smoothing parameters
    smetxt <- c(rect = "Rectangular", tri = "Triangular", sg_quad = "Savitsky-Golay (quadratic)", sq_quart = "Savitsky-Golay (quartic)")
    information <- paste0(information, "\nThe chromatogram signal was smoothed using method: ", smetxt[smooth[1]], " ('", smooth[1], "') with ", ma_pts[1], " points and ", ma_passes[1], " passes ", autoest_suffix)

    cat("\nSmoothing the chromatogram! Method: ", smooth[1], "...", sep = "")
    ssig <- chrom_smooth(csig, method = smooth[1], pts = ma_pts[1], passes = ma_passes[1])
    data[, paste0("Smooth_", smooth[1])] <- ssig
    cat("\nChromatogram smoothing complete!")

  } else if(smooth[1]=="none") {
    information <- paste0(information, "\nThe chromatogram signal was not smoothed.")
    cat("\nNo smoothing was applied to the raw signal.")
    data[, paste0("Smooth_", smooth)] <- ssig <- csig
  }

  #Calculating FD and SD
  cat("\nCalculating first and second derivatives...")
  der_out <- chrom_deriv(rtime, ssig)
  fder <- der_out[[1]]
  sder <- der_out[[2]]
  data[,c("FDs", "SDs")] <- cbind.data.frame(fder, sder)

  #Smoothing derivatives
  if(smooth[2]!="none") {

    #Information about smoothing parameters
    smetxt <- c(rect = "Rectangular", tri = "Triangular")
    information <- paste0(information, "\nFirst and second derivative signals were smoothed using method: ", smetxt[smooth[2]], " ('", smooth[2], "') with ", ma_pts[2], " points and ", ma_passes[2], " passes ", autoest_suffix)

    cat("\nSmoothing first and second derivatives...")
    fder_smooth <- chrom_smooth(fder, method = smooth[2], pts = ma_pts[2], passes = ma_passes[2])
    sder_smooth <- chrom_smooth(sder, method = smooth[2], pts = ma_pts[2], passes = ma_passes[3])
    data[,c("FDs_smooth", "SDs_smooth")] <- cbind.data.frame(fder_smooth, sder_smooth)
  } else if(smooth[2]=="none") {

    information <- paste0(information, "\nFirst and second derivatives were not smoothed.")

    cat("\nNo smoothing was carried out on first or second derivatives since 'smooth[2]' is 'none'...")
    fder_smooth <- fder
    sder_smooth <- sder
  }

  #Peak Detection
  #Carry out z-score peak detection runs and save optimal run results for later
  cat("\nCarrying out z-score analysis to approximate signal/peak regions...")
  if(is.list(zscore_pars) & length(zscore_pars)==3) {
    z_reslist <- z_optim(ssig, lag_rng = zscore_pars[[1]], thres_rng = zscore_pars[[2]],
                         peak_thres = zscore_pars[[3]][1], noise_thres = zscore_pars[[3]][2], mode = "percent")
    zrle_df <- z_reslist[[1]]
    opt_run <- z_reslist[[2]]
  }

  #Define the amplitude threshold (to avoid detecting false/tiny peaks)
  if(is.numeric(amp_thres)) {
    amplim <- amp_thres
    #Information about the amplitude threshold
    information <- paste0(information, "\nA signal amplitude threshold was set at ", amplim, " manually.")
  } else {
    amplim <- c()
    cat("\nDetermining amplitude threshold using method(s): '", paste0(amp_thres, collapse = ", "), "'...", sep = "")
    if(any(amp_thres %in% "quant")) {
      amplim <- c(amplim, chrom_amplim(ssig, method = "quant", pars = ampfrac))
    }
    if(any(amp_thres %in% "diff")) {
      amplim <- c(amplim, chrom_amplim(ssig, method = "diff", pars = ampfrac))
    }
    if(any(amp_thres %in% "zscore")) {
      amplim <- c(amplim, chrom_amplim(ssig, method = "zscore", pars = c(opt_run[["params"]][1:2], sens[3])))
    }
    if(length(which(c("quant","diff", "zscore") %in% amp_thres))>1) {
      cat("\n", paste0("'", amp_thres, "'", collapse = ", "), " methods selected for amplitude threshold determination! Using the mean of the results as the final threshold...", sep = "")
      amplim <- mean(amplim, na.rm = TRUE)
    }

    #Information about the amplitude threshold
    amptxt <- c(diff = "differences between quantiles", zscore = "z-scores")
    information <- paste0(information, "\nA signal amplitude threshold of ", round(amplim,2), " was determined using ", paste0(amptxt[amp_thres], " ('", amp_thres, "')", collapse = ", and "),".")
  }

  #Define First and second derivative peak detection thresholds
  #Using methods of: Vaz, ZScore, or NCore
  cat("\nDetermining first and second ORIGINAL derivative thresholds for peak detection using method: '", der_thres[1], "'...", sep = "")
  der_threslist <- chrom_derlims(x = zrle_df, fder = fder, sder = sder,
                                 method = der_thres[1], outs = der_thres[2], sens = sens[1:2])
  TFD <- der_threslist[["FD"]]
  TSD <- der_threslist[["SD"]]
  dist_list <- der_threslist[["noise"]]

  #Information about derivative peak thresholds
  dthtxt <- c(vaz = "Vaz et al. (2016)", ncore = "noise-core", zscore = "z-score")
  outxt <- c(all = "inter-quartile range, quantile, and standard deviation",
             iqr = "inter-quartile range", quant = "quantile", sd = "standard deviation")
  information <- paste0(information, "\nInferior and superior first- (", paste0(round(TFD,2), collapse = ", "),
                        ") and second- (", paste0(round(TSD,2), collapse = ", "),
                        ") derivative thresholds were determined via the ", dthtxt[der_thres[1]], " ('", der_thres[1], "') method.",
                        if(der_thres[2]!="none") paste0("\nOutliers for this procedure were handled via ", outxt[der_thres[2]], " ('", der_thres[2], "') method(s).") else
                          "Outlier handling for this procedure was not carried out.")

  #Begin Positive Peak Detection (negative peak detection not currently implemented)
  #METHOD: GLOBAL NON-SEQUENTIAL
  #Identify various shifts in data characteristic of a peak maximum, and split peaks where necessary
  cat("\nDetecting peak maxima, inflection points, upslopes, round boundaries, shoulders, and valleys...")

  #Getting general peak markers (minima, maxima, up- and down-crosses of signal and derivatives for BOTH ORIGINAL AND SMOOTHED DATA)
  orig_df <- peakmark(sig = csig, fder = fder, sder = sder, sig_thres = amplim, FD_thres = TFD, SD_thres = TSD, mpts = mpts[1], crosspts = crosspts[1])
  smooth_df <- if(all(smooth %in% "none")) NA else peakmark(sig = ssig, fder = fder_smooth, sder = sder_smooth, sig_thres = amplim, FD_thres = TFD, SD_thres = TSD, mpts = mpts[2], crosspts = crosspts[2])

  #Obtaining final peak start, end, maximum, and inflection point indices alongside peak and peak boundary classification (baseline-resolved, valley/fused, shoulder, or round)
  peak_idres <- peakfind(marks = orig_df, smooth_marks = smooth_df, rtime = rtime, sig = ssig, fder = fder_smooth, sder = sder_smooth,
                         sig_thres = amplim, fd_thres = TFD, sd_thres = TSD, max_w = if(any(c(ma_pts, ma_passes) %in% "auto")) autowd[3] else NA, liftoff = apex_pars[1],
                         touchdown = apex_pars[2], min_ht = ht_rej, w_rej = w_rej, pa_rej = pa_rej, sn_rej = sn_rej, rej_logic = rej_logic,
                         bnch = det_bunch, refine_infs = TRUE)
  #return(peak_idres)
  #Information about the peak detection
  information <- paste0(information, peak_idres[["information"]])
  peak_idres <- peak_idres[["results"]]
  if(!all(is.na(pnms))) peak_idres <- cbind.data.frame(Compound = pnms, peak_idres)

  #Add peak IDs to the main data, subset the columns to keep: RT, Signal, Smoothed Signal, Peak IDs, and append data to results list
  peak_idseq <- valley_idseq <- rep(0, nrow(data))
  #Also compile peak list
  peak_list <- list()

  for(i in seq(nrow(peak_idres))) {
    peak_idseq[peak_idres[i,"ind_starts"]:peak_idres[i,"ind_ends"]] <- peak_idres[i,"peak"]
    peak_list[[paste0("peak_",i)]] <- data[peak_idres[i,"ind_starts"]:peak_idres[i,"ind_ends"],]

    #Check if peaks are adjacent to each other (if affirmative, also add the appropriate flag to peak_idres)
    cond <- peak_idres[i-1,"ind_ends"]==peak_idres[i, "ind_starts"]
    if(length(cond)==0) cond <- FALSE else if(is.na(cond)) cond <- FALSE

    if(cond) {
      peak_idseq[peak_idres[i-1,"ind_ends"]] <- peak_idres[i-1,"peak"]
      valley_idseq[peak_idres[i-1,"ind_ends"]] <- peak_idres[i,"peak"]
    }
  }

  #Compile peak data.frame
  peak_resdf <- cbind.data.frame(data, Peak_ID = peak_idseq, Peak_IDsec = valley_idseq)

  #Combine the peak data.frame, peak start-maxima-end table, and the amplitude threshold
  res_total <- list(Chromatogram = peak_resdf, Derivative_Noise = do.call(cbind.data.frame, dist_list),
                    Peaks = peak_list, Peak_Extents = peak_idres, Amplitude_Limit = amplim,
                    Derivative_Limits = list(FD = c(TS_FD = max(TFD), TI_FD = min(TFD)), SD = c(TS_SD = max(TSD), TI_SD = min(TSD))),
                    Zscore_Limits = rtime[sort(unlist(zrle_df))], information = information, call = cl_rec)

  #Determine plotting variables
  plot_vars <- if(plot_corr) c(vars[1], paste0(colnames(peak_resdf)[2], "_", bline_method)) else vars

  if(plotset!="none") {
    #Construct plots
    plot_list[["Chromatogram"]] <- chrom_plot(chrom_df = peak_resdf, ptab = peak_idres, chrom_vars = plot_vars, norm_chrom = FALSE, asprat = asprat,
                                              id = "peak", apex = "ind_finmax", inf = c("ind_linf", "ind_rinf"),
                                              ups = c("ind_lups", "ind_rups"), bound = c("ind_starts","ind_ends"),
                                              amp_line = amplim, lablim = NA, plot_title = "Chromatogram with Peak Markers", draw = FALSE)

    plot_list[["FD_noise"]] <- noise_plot(noise = dist_list[["FD_noise"]], thres = TFD, plot_title = "First Derivative Peak Detection Thresholds", y_lab = "First Derivative (noise)", asprat = asprat)
    plot_list[["SD_noise"]] <- noise_plot(noise = dist_list[["SD_noise"]], thres = TSD, plot_title = "Second Derivative Peak Detection Thresholds", y_lab = "Second Derivative (noise)", asprat = asprat)
    if(plotset=="print") print(plot_list)
  } else if(length(plot_list)==0) plot_list <- "No plots were generated since 'plotset' was set to 'none'."

  #Return results
  return(list(results = res_total, plots = plot_list))
}
