#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimate smoothing window width, number of smoothing passes, average and minimum peak width at 5% height, minimum width at inflection points
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Auto-estimate smoothing width and number of smoothing passes
#'
#' @description Part of the \code{\link{chrom_detect}} workflow. Estimates smoothing window width, number of smoothing passes, average and minimum peak width at 5% height, minimum width at inflection points.
#'
#' @param rtime A \code{numeric} vector of retention time (x-axis) values.
#' @param sig A \code{numeric} vector of signal (y-axis) values.
#' @param start_smooth A \code{numeric} vector of length 2 containing, in that order, the \strong{odd} number of smoothing points and passes to use for \strong{initial smoothing}. Defaults to \code{c(7,1)}.
#' @param bcorr Method to use for baseline correction (defaults to \code{"none"}), see \code{\link{chrom_bline}} for available options.
#' @param bpars Parameters to use for baseline correction when \code{bcorr!="none"}.
#' @param ampfrac The \code{numeric} quantile percentage value (from 0.001 to 50) to use for amplitude limit estimation via function \code{\link{chrom_amplim}}.
#' @param smooth_method A \code{character} vector of length 2 providing smoothing methods for signal and derivatives (in that order).
#' If a vector of length 1 is given, the same method is applied to both types of data. See \code{\link{chrom_smooth}} for available methods.
#'
#' @return A named \code{numeric} vector of length 5 containing the following elements:
#' \describe{
#' \item{"points"}{Number of auto-estimated smoothing points (smooth width).}
#' \item{"passes"}{Number of auto-estimated smoothing passes.}
#' \item{"width_5_mean"}{\strong{Average} peak width at 5% of peak height (in number of points/indices).}
#' \item{"width_5_min"}{\strong{Minimum} peak width at 5% of peak height (in number of points/indices).}
#' \item{"width_inf"}{Minimum inflection point width (in number of points/indices).}
#' }
#' @export
#'
#' @examples
#' dt <- lcqc::simlc1
#' chrom_width(dt[,"Time"], dt[,"Signal"], ampfrac = 0.05)
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{acc_inf}}
#'
#' @importFrom stats optim
chrom_width <- function(rtime, sig, start_smooth = c(7,1), bcorr = "none", bpars = "default",
                        ampfrac = 1, smooth_method = rep("tri",2)) {

  #Preliminary checks
  if(!is.numeric(start_smooth) | length(start_smooth)!=2) stop("Argument 'start_smooth' must be a numeric vector of length 2!")
  if(!all(sapply(list(rtime,sig), is.numeric)) | length(rtime)!=length(sig)) stop("Arguments 'rtime' and 'sig' must be numeric vectors of equal length!")
  if(!all(smooth_method %in% c("rect", "tri", "sg_quad", "sg_quart", "none"))) {
    stop("The type of smoothing to be used for the signal is not provided in the correct format! Allowed values: 'rect', 'tri', 'sg_quad', 'sg_quart', 'none'...")
  }
  if(length(smooth_method)==1) {
    cat("\nThe smoothing method must be a character vector of length 2! Copying the first element...")
    smooth_method <- rep(smooth_method,2)
  }

  if(any(smooth_method[2] %in% c("sg_quad", "sg_quart"))) {
    stop("Savitsky-Golay smoothing ('sg_quad' or 'sg_quart') is not supported for derivatives! Use 'tri' or 'rect' instead!")
  }

  #Apply baseline correction, if any
  #Baseline Correction
  if(bcorr!="none") {
    #cat("\nThe following baseline correction method was applied: '", bcorr, "'!", sep = "")
    csig <- chrom_bline(sig, method = bcorr, pars = bpars, plotres = "none")[["Corrected_Signal"]]
  } else {
    #cat("\nNo background correction was applied to the signal!")
    csig <- sig
  }

  #Smoothing chromatogram using a standard procedure specified in 'start_smooth'
  ssig <- if(smooth_method[1]=="none") csig else chrom_smooth(csig, method = smooth_method[1], pts = start_smooth[1], passes = start_smooth[2])

  #Calculate first and second derivatives
  #cat("\nCalculating first and second derivatives...")
  der_out <- chrom_deriv(rtime, ssig)
  fder <- der_out[[1]]
  sder <- der_out[[2]]

  #Smoothing derivatives using a standard procedure specified in 'start_smooth'
  #cat("\nSmoothing second derivatives...")
  fder_smooth <- if(smooth_method[2]=="none") fder else chrom_smooth(fder, method = smooth_method[2], pts = start_smooth[1], passes = start_smooth[2])
  sder_smooth <- if(smooth_method[2]=="none") sder else chrom_smooth(sder, method = smooth_method[2], pts = start_smooth[1], passes = start_smooth[2])

  #Signal
  #Define the amplitude threshold (to avoid detecting false/tiny peaks)
  amplim <- chrom_amplim(ssig, method = "quant", pars = ampfrac)

  #Subset signal and sd data
  total_df <- Reduce(cbind.data.frame, list(seq_along(ssig), rtime, ssig, fder_smooth, sder_smooth))
  colnames(total_df) <- c("ind", "x", "y", "fd", "sd")

  #Remove NAs
  min_nacol <- names(which.min(apply(total_df, 2, function(x) length(which(!is.na(x))))))[1]
  na_rng <- which(is.na(total_df[,min_nacol]))
  total_df <- total_df[-na_rng,]

  #Determine signal maxima, FD downcrosses, SD minima, and SD (up)crosses
  allmax <- find_peaks(total_df[,"y"], 5)
  allsdmin <- find_peaks(-total_df[,"sd"], 5)
  allfdcross <- which(c(0,diff(sign(total_df[,"fd"])))==-2)
  allsdcross <- which(c(0,diff(sign(total_df[,"sd"])))==-2)
  allsdupcross <- which(c(0,diff(sign(total_df[,"sd"])))==2)

  #Filter maxes, only leaving those which have a corresponding fdcross AND sd minimum
  truemax <- sapply(allmax, function(x) {
    chk <- !any(is.na(c(corresp(x, allfdcross, minsim = 3), corresp(x, allsdmin, minsim = 3))))
    if(chk) x else NA
  })
  truepeak <- truemax[!is.na(truemax)]

  truepeak <- as.data.frame(t(sapply(truepeak, function(x) {
    failres <- setNames(rep(NA,3), c("max", "linf", "rinf"))

    #Find inflection points for the signal maximum
    linf_res <- raw_infs(x, NA, allsdcross, NA, "left")
    rinf_res <- raw_infs(x, NA, allsdupcross, NA, "right")

    finres <- c("max" = x, "linf" = linf_res, "rinf" = rinf_res)
    if(length(finres)!=3 | any(is.na(finres))) finres <- failres
    return(finres)
  })))
  truepeak <- truepeak[complete.cases(truepeak),]

  if(nrow(truepeak)>0 & ncol(truepeak)==3) {
    maxes <- truepeak[,"max"]
    linfs <- truepeak[,"linf"]
    rinfs <- truepeak[,"rinf"]

    #Retrieve the minimum inflection point width
    inf_W <- min(rinfs-linfs, na.rm = TRUE)

    #Interpolate accurate inflection points
    acc_infs <- acc_inf(xvals = rtime, yvals = ssig, sd = sder_smooth, linfs = linfs, rinfs = rinfs)
    acc_linfs <- acc_infs[["left"]][,"acc_x"]
    acc_rinfs <- acc_infs[["right"]][,"acc_x"]

    #Calculate the approximate half-width at 5% peak height in terms of retention time (assuming a Gaussian peak shape)
    hws_5 <- abs(acc_rinfs-acc_linfs)*4.89549/2

    #Calculate the retention time ranges using fdcross as the peak maximum
    max_rts <- rtime[maxes]
    rt_rngs <- cbind.data.frame(min_rt = max_rts-hws_5, max_rt = max_rts+hws_5)

    #Determine the number of data points within the peak width intervals (at 5% peak height) and calculate the global mean
    W2 <- sapply(1:nrow(rt_rngs), function(x) {
      length(which(rtime>=rt_rngs[x,"min_rt"] & rtime<=rt_rngs[x,"max_rt"]))
    })
    mean_W <- round(mean(W2, na.rm = TRUE),0)

    #Also calculate the width of the narrowest peak
    min_W <- round(min(W2, na.rm = TRUE),0)

    #Calculate the ODD NUMBER CLOSEST TO 1/10th OF THE DATA POINT WIDTH as the combined moving average window W
    tempW <- round(mean_W/10,0)
    W <- if(is.even(tempW)) tempW+1 else tempW

    #Solve the simultaneous equation W = n*d-n+1 to obtain n (number of smoothing passes) and d (number of points to use for each pass)
    #See this link: https://stackoverflow.com/questions/56195594/how-to-solve-set-of-equations-for-two-unknowns-using-r

    #Define loss function
    loss <- function(X, totalW = W) {
      d <- X[1] #Width of window
      n <- X[2] #Number of passes

      return(sum(c(n*d-n+1-totalW,
                   (d-1)*n-(totalW-1),
                   -d*n+n+(totalW-1),
                   (d-1)*n+1-totalW))^2)
    }

    #Run optimization problem for an initial rough solution
    optres <- optim(par = c(3,1),
                    fn = loss,
                    method = "L-BFGS-B",
                    lower = c(3,1),
                    upper = c(W/1.5,5))[["par"]]

    #Round each estimate both up and down to the corresponding integer and determine which solution is the closest match for W
    optd <- c(ceiling(optres[1]), floor(optres[1]))
    optn <- c(ceiling(optres[2]), floor(optres[2]))
    optd <- optd[!is.even(optd)] #Removes even smoothing windows, leaving only odd numbers
    optnd <- setNames(expand.grid(optd, optn), c("d","n"))

    fin_nd <- apply(optnd, 1, function(x) diff(c(W, as.numeric(x[1]*x[2]-x[1]+1))))
    fin_nd <- as.numeric(optnd[which.min(abs(fin_nd)),])
    finres <- c(fin_nd, mean_W, min_W, inf_W)
    names(finres) <- c("points", "passes", "width_5_mean", "width_5_min", "width_inf")
  } else stop("Automatic smoothing parameter determination failed! Enter smoothing parameters manually...")
  return(finres)
}
