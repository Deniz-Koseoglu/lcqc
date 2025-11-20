#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION COLLECTION: Baseline Correction of signal
# Includes option to plot the chromatogram before and after baseline correction
# Includes various baseline correction methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: WEIGHTED Eilers (2003) iterative smoothing (for ALS baseline correction)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Weighted (Whittaker-Henderson) smoother
#'
#' @description Smooth a signal with a finite difference penalty of order 2. For more details, see \strong{References}. Modified from package \pkg{ptw}.
#'
#' @param input Input numeric vector containing signal for smoothing.
#' @param lambda Smoothing parameter (defaults to \code{1600}). Degree of smoothing increases with value.
#' @param wts Vector of relative weights, must equal \code{input} in length. Defaults to \code{1}.
#'
#' @return Numeric vector containing the smoother signal.
#' @export
#'
#' @references
#' Eilers, P.H.C. (2003), 'A perfect smoother', \emph{Analytical Chemistry} \strong{75}, pp. 3631-3636.
#'
#' Eilers, P.H.C. (2004), 'Parametric Time Warping', \emph{Analytical Chemistry} \strong{76} (2), pp. 404-411.
#'
#' @examples
#' smooth_whit(lcqc::simlc1[,"Signal"], lambda = 1600)
#'
#' @seealso \code{\link{bline_als}}
smooth_whit <- function(input, lambda, wts = rep(1, length(input))) {
  lng_in <- length(input)
  z <- d <- c <- e <- rep(0, lng_in)
  mm <- lng_in
  m <- mm #-1
  d[1] <- wts[1] + lambda
  c[1] <- -2 * lambda/d[1]
  e[1] <- lambda/d[1]
  z[1] <- wts[1]*input[1]
  d[2] <- wts[2] + 5 * lambda - d[1] * c[1] * c[1]
  c[2] <- (-4 * lambda - d[1] * c[1] * e[1])/d[2]
  e[2] <- lambda/d[2]
  z[2] <- wts[2] * input[2] - c[1] * z[1]

  for(i in 3:(m-2)) {
    i1 <- i-1
    i2 <- i-2

    d[i] <- wts[i] + 6 * lambda - c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2]
    c[i] <- (-4 * lambda - d[i1] * c[i1] * e[i1])/d[i]
    e[i] <- lambda/d[i]
    z[i] <- wts[i] * input[i] - c[i1] * z[i1] - e[i2] * z[i2]
  }

  i1 <- m-2
  i2 <- m-3
  d[m-1] <- wts[m-1] + 5 * lambda - c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2]
  c[m-1] <- (-2 * lambda - d[i1] * c[i1] * e[i1])/d[m-1]
  z[m-1] <- wts[m-1] * input[m-1] - c[i1] * z[i1] - e[i2] * z[i2]

  i1 <- m-1
  i2 <- m-2
  d[m] <- wts[m] + lambda - c[i1] * c[i1] * d[i1] - e[i2] * e[i2] * d[i2]
  z[m] <- (wts[m] * input[m] - c[i1] * z[i1] - e[i2] * z[i2])/d[m]
  z[m-1] <- z[m-1]/d[m-1] - c[m-1]*z[m]

  for(i in (m-2):1) {
    z[i] <- z[i]/d[i] - c[i] * z[i+1] - e[i] * z[i+2]
  }
  return(z)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Asymmetric Least Squares (ALS) baseline correction
# Do not confuse with ALTERNATING Least Squares!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Iterative Asymmetric Least Squares (ALS) baseline correction
#'
#' @description Asymmetric Least Squares (ALS) baseline correction, modified from package \pkg{baseline} for iterative convergence based on weighed Eilers smoothing (see also \code{\link{smooth_whit}}).
#'
#' @param input Numeric vector of signal to be baseline-corrected.
#' @param lambda Second derivative constraint.
#' @param p Weights of residuals (positive).
#' @param prec Precision (error) tolerance required for baseline correction to reach convergence.
#' @param maxit Maximum number of iterations (defaults to \code{50}).
#' @param rm_neg A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).
#'
#' @return A list with 5 elements: \code{Original_Signal}, \code{Corrected_Signal}, \code{Baseline}, \code{Method} (always \code{"als"}), and \code{Parameters}.
#'
#' @export
#'
#' @references Peng, J., Peng, S., Jiang, A., Wei, J., Li, C., Tan, J. (2010), 'Asymmetric Least Squares for Multiple Spectra Baseline Correction', \emph{Analytica Chimica Acta} \strong{683} (1), pp. 63-68, doi: \href{10.1016/j.aca.2010.08.033}{https://www.doi.org/10.1016/j.aca.2010.08.033}.
#'
#' @examples
#' res <- bline_als(lcqc::exgc1[,"Signal"])
#' bline_plot(res)
#'
#' @seealso \code{\link{chrom_bline}}, \code{\link{smooth_whit}}
bline_als <- function(input, lambda = 6, p = 0.001, prec = 1e-08, maxit = 50, rm_neg = TRUE) {
  #Preliminary checks
  if(!is.atomic(input) | !is.numeric(input)) {
    stop("Input data must be a numeric vector!")
  }

  if(p<=0 | p>=1) {
    stop("The 'p' value must be between 0.001 and 0.999!")
  }

  #Prepare variables for smoothing
  lng_in <- length(input)
  baseline <- rep(0, lng_in)
  wts <- baseline+1
  prec_use <- max(prec, prec*diff(range(input, na.rm = TRUE)))
  lambda_use <- 10^lambda

  for (i in 1:maxit) {
    baseline_old <- baseline
    baseline <- smooth_whit(input = input, lambda = lambda_use, wts = wts)
    wts <- p*(input>baseline)+(1-p)*(input<=baseline)
    error <- max(abs(baseline - baseline_old))
    if (error < prec_use) break
  }
  if (error >= prec_use) {
    warning("Baseline correction did not reach convergence!")
  }
  #Nullify points where baseline values are below zero
  if(rm_neg) baseline[baseline > input] <- input[input < baseline]
  corrected_input <- input - baseline
  return(list("Original_Signal" = input, "Corrected_Signal" = corrected_input, "Baseline" = baseline, "Method" = "als",
              "Parameters" = c(lambda = lambda, p = p, prec = prec, maxit = maxit, rm_neg = rm_neg)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Chang et al. (2007) baseline correction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Baseline correction (Chang's method)
#'
#' @description Implementation of baseline correction according to Chang et al. (2007). Modified from package \pkg{TargetSearch}.
#'
#' @param input Input numeric vector containing signal to be baseline-corrected.
#' @param threshold Position of the baseline relative to the noise component of the signal. Must be between 0 and 1.
#' @param alpha High-pass filter parameter.
#' @param bfrac Fraction of low-intensity fragments of the filtered signal that are assumed to be baseline.
#' @param segments Number of segments to divide the filtered signal into.
#' @param sig_window Signal window size (in points).
#' @param fit Method used for baseline fitting. One of \code{"linear"} or the significantly slower \code{"spline"} (cubic).
#' @param rm_neg A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).
#'
#' @return A list with 5 elements: \code{Original_Signal}, \code{Corrected_Signal}, \code{Baseline}, \code{Method} (always \code{"chang"}), and \code{Parameters}.
#' @export
#'
#' @references Chang, D., Banack, C.D., Shah, S.L. (2007), 'Robust baseline correction algorithm for signal dense NMR spectra', \emph{Journal of Magnetic Resonance} \strong{187}, pp. 288-292.
#'
#' @examples
#' res <- bline_chang(lcqc::exgc1[,"Signal"])
#' bline_plot(res)
#'
#' @seealso \code{\link{chrom_bline}}
#'
#' @importFrom stats smooth.spline
#' @importFrom stats predict
#' @importFrom stats approx
bline_chang <- function(input, threshold = 0.5, alpha = 0.95, bfrac = 0.2, segments = 100,
                        sig_window = 10, fit = "linear", rm_neg = TRUE) {

  #Preliminary checks
  if(!is.logical(rm_neg)) stop("Argument 'rm_neg' must be a logical (TRUE/FALSE)!")
  if(!is.atomic(input) | !is.numeric(input)) stop("Input data must be a numeric vector!")
  if(sig_window>length(input)) stop("The signal window ('sig_window') cannot be wider than the input data!")
  if(!any(c("linear", "cubic") %in% fit)) stop("The 'fit' argument must be one of: 'linear' or 'cubic'!")
  if(threshold<0 | threshold>1) stop("The 'threshold' value must be between 0 and 1!")
  if(bfrac<0 | bfrac>1) stop("The 'bfrac' value must be between 0 and 1!")
  if(is.odd(sig_window)) {
    cat("\nSignal window must be an even integer! Changing to the nearest even number...")
    sig_window <- sig_window+1
  }

  if(is.atomic(input) & is.numeric(input)) {
    int <- input
  } else if(!is.atomic(input) | !is.numeric(input)) {
    stop("Input signal must be a numeric vector!")
  }

  #Function requires a single vector of intensity values
  #Apply a high pass filter
  hpfres <- c()
  for(i in seq_along(input)) {
    if(i==1) hpfres[i] <- input[i] else hpfres[i] <- alpha * (hpfres[i-1] + input[i] - input[i-1])
  }

  #Divide high-pass filtered signal in segments and compute SD of every segment
  lng_in <- length(input)
  seg_div <- ceiling(lng_in / segments)
  seg_ids  <- rep(1:segments, each = seg_div)[1:lng_in]
  seg_sd <- sapply(split(hpfres, seg_ids), sd, na.rm = TRUE)

  #Get SD of the 'bfraction' segments with the lowest standard deviation (an estimation of the noise SD).
  bfrac <- round(bfrac * segments,0)
  bfrac_sd  <- order(seg_sd)[1:bfrac] #(length(seg_sd)*bfrac)
  stdn <- sd(hpfres[seg_ids %in% bfrac_sd], na.rm = TRUE)

  #Points with absolute values higher than 2*stdn are considered signal and the center of a signal window of width 'signalwindow'
  whichsig  <- which(abs(hpfres) > 2*stdn)

  #Windowing step: apply signalwindow to the signal points obtained before
  lng_which <- length(whichsig)
  ids <- integer(lng_in)

  for(i in whichsig) {
    if(i-(sig_window/2)<1) ids[1:i] <- 1 else ids[(i-sig_window/2):i] <- 1
    if(i+(sig_window/2)>lng_in) ids[i:lng_in] <- 1 else ids[i:(i+sig_window/2)] <- 1
  }
  ids[1] <- ids[lng_in] <- FALSE

  if(fit=="cubic") {
    # Fit a cubic smoothing spline of the baseline using the points considered noise (VERY SLOW).
    out_spline <- smooth.spline(seq(lng_in)[!ids==1], input[!ids==1])
    baseline <- predict(out_spline, seq(lng_in))

  } else if (fit=="linear") {
    # Calculate baseline by linear interpolation (FAST).
    baseline <- approx(seq(lng_in)[!ids==1], input[!ids==1], seq(lng_in))
  }
  # returns the smoothed points plus offset
  baseline <- baseline[["y"]] + 2 * (threshold - 0.5) * 2 * stdn
  if(rm_neg) baseline[baseline > input] <- input[input < baseline]
  corrected_input <- input - baseline
  #if(rm_neg) corrected_input[corrected_input<0] <- 0

  return(list("Original_Signal" = input, "Corrected_Signal" = corrected_input, "Baseline" = baseline, "Method" = "chang",
              "Parameters" = c(threshold = threshold, alpha = alpha, bfrac = bfrac, segments = segments, sig_window = sig_window, fit = fit, rm_neg = rm_neg)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Modified Polynomial Fit (ModPolyFit) baseline correction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Modified Polynomial Fit (ModPolyFit) baseline correction
#'
#' @description Iterative baseline correction algorithm based on automated polynomial fitting developed by Lieber & Mahadevan-Jansen (2003). Modified from package \pkg{baseline}.
#'
#' @param input Input numeric vector containing signal to be baseline-corrected.
#' @param deg Degree of the polynomial fit.
#' @param prec Precision (error) tolerance required for baseline correction to reach convergence.
#' @param maxit Maximum number of iterations (defaults to \code{100}).
#' @param rm_neg A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).
#'
#' @return A list with 5 elements: \code{Original_Signal}, \code{Corrected_Signal}, \code{Baseline}, \code{Method} (always \code{"poly"}), and \code{Parameters}.
#' @export
#'
#' @references Lieber, C.A., Mahadevan-Jansen, A. (2003), 'Automated Method for Subtraction of Fluorescence from Biological Raman Spectra', \emph{Applied Spectroscopy} \strong{57} (11), pp. 1363-1367, doi: \href{10.1366/000370203322554518}{https://www.doi.org/10.1366/000370203322554518}.
#'
#' @examples
#' res <- bline_poly(lcqc::exgc1[,"Signal"])
#' bline_plot(res)
#'
#' @seealso \code{\link{chrom_bline}}
bline_poly <- function(input, deg = 4, prec = 0.001, maxit = 100, rm_neg = TRUE) {

  lng_in <- length(input)
  absc <- "none" #Argument 'absc' removed for now... not really needed for chromatographic processing.

  #Preliminary checks
  if(!is.atomic(input) | !is.numeric(input)) stop("Input data must be a numeric vector!")
  if (absc=="none" | !is.numeric(absc)) {
    absc <- seq(lng_in)
  }

  baseline <- rep(0, lng_in)
  polx <- cbind(1/sqrt(lng_in), stats::poly(absc, degree = deg))
  ywork <- yold <- yorig <- input
  nrep <- 0

  repeat {
    nrep <- nrep + 1
    baseline <- polx %*% crossprod(polx, yold)
    ywork <- pmin(yorig, baseline)
    crit <- sum(abs((ywork - yold)/yold), na.rm = TRUE)
    if (crit < prec || nrep > maxit) {
      cat("\nBackground correction failed to reach convergence!")
      break
    }
    yold <- ywork
  }
  if(rm_neg) baseline[baseline > input] <- input[input < baseline]
  corrected_input <- input - baseline
  return(list("Original_Signal" = input, "Corrected_Signal" = as.vector(corrected_input), "Baseline" = as.vector(baseline), "Method" = "poly",
              "Parameters" = c(deg = deg, prec = prec, maxit = maxit, rm_neg = rm_neg)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Iterative Smoothing Splines with Root Error Adjustment (ISREA) baseline correction algorithm (Xu et al., 2021)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Iterative Smoothing-Splines with Root Error Adjustment (ISREA) baseline correction
#'
#' @description Baseline correction that uses smoothing splines to estimate the baseline. Developed by Xu et al. (2021).
#'
#' @param input Input numeric vector containing signal to be baseline-corrected.
#' @param eta Convergence criterion. May take values from 0.0001 to 10 (based on Xu et al., 2021). Usually does not appreciably affect the results, but more computation time is needed for lower values.
#' @param maxit Maximum number of iterations (defaults to \code{100}).
#' @param rm_neg A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).
#'
#' @return A list with 5 elements: \code{Original_Signal}, \code{Corrected_Signal}, \code{Baseline}, \code{Method} (always \code{"isrea"}), and \code{Parameters}.
#' @export
#'
#' @references Xu, Y., Du, P., Senger, R., Robertson, J., Pirkle, J.L. (2021), 'ISREA: An Efficient Peak-Preserving Baseline Correction Algorithm for Raman Spectra', \emph{Applied Spectroscopy} \strong{75} (1), pp. 34-45, doi: \href{10.1177/0003702820955245}{https://www.doi.org/10.1177/0003702820955245}.
#'
#' @examples
#' res <- bline_isrea(lcqc::exgc1[,"Signal"])
#' bline_plot(res)
#'
#' @seealso \code{\link{chrom_bline}}
#'
#' @importFrom  stats smooth.spline
bline_isrea <- function(input, eta = 10, maxit = 100, rm_neg = TRUE) {
  #Preliminary checks
  if(!is.atomic(input) & !is.numeric(input)) stop("The 'input' data must be a numeric vector!")
  if(eta>10|eta<0.001) stop("The value of 'eta' must be between 0.001 and 10!")
  if(!is.numeric(maxit)) stop("The maximum number of iterations 'maxit' must be a numeric value!")

  #Make backup of initial data
  input_init <- input

  #Compute initial smoothing spline
  baseline <- smooth.spline(input_init, nknots = 5)[["y"]]

  #Set initial D (error) value
  D <- 2*eta

  #Start iteration counter
  iter <- 1

  #Loop until convergence
  while(D > eta) {
    delta <- input - baseline
    baseline_old <- baseline
    baseline <- sapply(seq_along(delta), function(x) if(delta[x]<=0) input[x] else baseline[x] + delta[x]^(1/4))
    baseline <- smooth.spline(baseline, nknots = 5)[["y"]]
    #Compute a new D value as the 2-norm squared of the difference between new and old baseline
    D <- sum(abs(baseline - baseline_old)^2)
    iter <- iter+1
    if(iter > maxit) {
      cat("\nBaseline correction failed to reach convergence!")
      break
    }
  }

  if(rm_neg) baseline[baseline > input_init] <- input_init[input_init < baseline]
  corrected_input <- input_init - baseline
  return(list("Original_Signal" = input_init, "Corrected_Signal" = corrected_input, "Baseline" = baseline, "Method" = "isrea",
              "Parameters" = c(eta = eta, maxit = maxit, rm_neg = rm_neg)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Wrapper for all baseline correction methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Baseline correction using various methods
#'
#' @description This is the main baseline correction function able to apply several methods, such as Asymmetric Least Squares (\code{\link{bline_als}}),
#' Modified Polynomial Fit (\code{\link{bline_poly}}), Chang's method (\code{\link{bline_chang}}), and ISREA (\code{\link{bline_isrea}}). Optionally, the results may be summarised visually using \code{\link{bline_plot}}.
#'
#' @param input Input numeric vector containing signal to be baseline-corrected.
#' @param method Baseline correction method. One of: \code{"als"}, \code{"chang"}, \code{"poly"}, \code{"isrea"}, or \code{"none"} (for convenience when incorporating function into flexible workflows).
#' @param pars A \strong{named} vector of parameters specific to each baseline correction \code{method}. Applies default parameters when set to \code{"default"} - these are listed in \strong{Details}.
#' @param plotres Should a summary plot of baseline correction be created and/or printed? One of: \code{"none"}, \code{"plot"}, or \code{"print"}.
#' @param asprat Numeric value of the summary plot aspect ratio (defaults to \code{0.71}).
#' @param slnt A \code{TRUE/FALSE logical}. Displays information about progress and results when \code{FALSE} (default).
#' @param rm_neg A \code{TRUE/FALSE logical} specifying whether points where the computed baseline is higher that the original signal should be discarded (\code{TRUE} by default).
#'
#' @details This function incorporates all 4 approaches to baseline correction included in \pkg{lcqc}, each requiring unique parameters to be passed to its respective function.
#' These can be provided in argument \code{pars} as a vector, i.e. \code{c()}, or simply set to \code{"default"} to use sensible defaults. These are listed below:
#' \describe{
#'  \item{For ALS (function \code{\link{bline_als}})}{\code{c(lambda = 6, p = 0.001, prec = 1e-08, maxit = 50)}}
#'  \item{For Chang (function \code{\link{bline_chang}})}{\code{c(threshold = 0.5, alpha = 0.95, bfrac = 0.2, segments = 100, sig_window = 10, fit = "linear")}}
#'  \item{For ModPolyFit (function \code{\link{bline_poly}})}{\code{c(deg = 4, prec = 0.001, maxit = 100)}}
#'  \item{For ISREA (function \code{\link{bline_isrea}})}{\code{c(eta = 10, maxit = 100)}}
#' }
#'
#' @return A list containing usual baseline correction results (see \code{\link{bline_poly}}, for example) and, when \code{plotres!="none"}, a \code{Bline_Plot} element containing a summary plot.
#' @export
#'
#' @examples
#' chrom_bline(lcqc::exgc1[,"Signal"], method = "als", plotres = "plot")
#'
#' @seealso \code{\link{bline_als}}, \code{\link{bline_chang}}, \code{\link{bline_poly}}, \code{\link{bline_isrea}}, \code{\link{bline_plot}}
chrom_bline <- function(input, method = "als", pars = "default", plotres = "print", asprat = 0.71, rm_neg = TRUE, slnt = FALSE) {
  #Preliminary checks
  if(!all(method %in% c("als", "chang", "poly", "isrea", "none"))) stop("Baseline correction method not recognised! Available options are: 'als', 'chang', 'poly', 'isrea', or 'none'!")
  if(!is.atomic(input)|!is.numeric(input)) stop("Input data must be a numeric vector!")
  if(!any(pars %in% "default") & length(which(nchar(names(pars))>0))!=length(pars)) stop("All baseline correction parameters provided in argument 'pars' must be named!")

  def_pars <- if(method=="als") {
    c("lambda" = 6, "p" = 0.001, "prec" = 1e-08, "maxit" = 50)
  } else if(method=="chang") {
    c("threshold" = 0.5, "alpha" = 0.95, "bfrac" = 0.2, "segments" = 100, "sig_window" = 10, "fit" = "linear")
  } else if(method=="poly") {
    c("deg" = 4, "prec" = 0.001, "maxit" = 100)
  } else if(method=="isrea") {
    c("maxit" = 100, "eta" = 10)
  }

  #Isolate the 'rm_neg' argument into a separate object
  #rm_neg <- if(any(names(pars) %in% "rm_neg")) as.logical(pars["rm_neg"]) else TRUE
  cat("The removal of baseline points lying above the signal is set to ", rm_neg, "\n")

  accept_names <- c(names(def_pars), "rm_neg")

  if(any(pars %in% "default")) {
    pars <- def_pars
    cat("All parameters were left at defaults for BG correction method '", method, "': ", paste0(names(def_pars), collapse= ", "), ".", sep = "")
  } else if(!all(names(pars) %in% accept_names)) {
    stop(paste0("The following parameter names were not recognized for method '", method, "': ", paste0(names(pars)[!names(pars) %in% accept_names], collapse = ", "), "."))
  } else {
    defchk <- names(def_pars)[which(!def_pars %in% Reduce(intersect, list(pars, def_pars)) | !names(def_pars) %in% names(pars))]
    defpaste <- if(length(defchk)==0) "none" else defchk
    if(!slnt) cat("The following parameters were left at defaults for BG correction method '", method, "': ", paste0(defpaste, collapse= ", "), ".", sep = "")
    pars[!names(def_pars) %in% names(pars)] <- def_pars[!names(def_pars) %in% names(pars)]
    names(pars) <- names(def_pars)
  }

  #Begin baseline correction
  if(method=="als") {
    bline_res <- bline_als(input = input,
                           lambda = pars["lambda"],
                           p = pars["p"],
                           prec = pars["prec"],
                           maxit = pars["maxit"],
                           rm_neg = rm_neg)
  } else if(method=="chang") {
    bline_res <- bline_chang(input = input,
                             threshold = as.numeric(pars["threshold"]),
                             alpha = as.numeric(pars["alpha"]),
                             bfrac = as.numeric(pars["bfrac"]),
                             segments = as.numeric(pars["segments"]),
                             sig_window = as.numeric(pars["sig_window"]),
                             fit = pars["fit"],
                             rm_neg = rm_neg)
  } else if(method=="poly") {
    bline_res <- bline_poly(input = input,
                            deg = pars["deg"],
                            prec = pars["prec"],
                            maxit = pars["maxit"],
                            rm_neg = rm_neg)
  } else if(method=="isrea") {
    bline_res <- bline_isrea(input = input,
                             eta = pars["eta"],
                             maxit = pars["maxit"],
                             rm_neg = rm_neg)
  }

  if(plotres!="none" & any(c("print", "plot") %in% plotres)) {
    bline_res[["Bline_Plot"]] <- bline_plot(bline_res, corrsig = TRUE, asprat = 0.71, bline = TRUE)
    if(plotres=="print") print(bline_res[["Bline_Plot"]])
  }
  return(bline_res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Plot baseline results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Plot baseline correction results
#'
#' @description Create a summary plot of baseline correction results containing the original signal, calculated baseline, and (optionally) baseline-corrected signal.
#'
#' @param input Output from one of the baseline correction functions of \pkg{lcqc}: \code{\link{bline_als}}, \code{\link{bline_chang}}, \code{\link{bline_poly}}, \code{\link{bline_isrea}}, or \code{\link{chrom_bline}}.
#' @param xvar An optional numeric vector of \code{x} values corresponding to the signal in \code{input}. If set to \code{"auto"}, uses point indices instead.
#' @param xname Title of x-axis (defaults to \code{"Index"}).
#' @param bline A \code{logical}. Should the calculated baseline be included in the plot? Defaults to \code{FALSE}.
#' @param corrsig A \code{logical}. Should be baseline-corrected signal be included in the plot? Defaults to \code{TRUE}.
#' @param asprat A numeric value setting the plot aspect ratio.
#'
#' @return A \code{ggplot}-class object containing the plot.
#' @export
#'
#' @examples
#' bline_res <- chrom_bline(lcqc::exgc1[,"Signal"], method = "als", slnt = TRUE)
#' bline_plot(bline_res, bline = TRUE, corrsig = TRUE)
#'
#' @seealso \code{\link{chrom_bline}}
#'
#' @import ggplot2
bline_plot <- function(input, xvar = "auto", xname = "Index", bline = FALSE, corrsig = TRUE, asprat = 0.71) {

  #Preliminary checks
  if(!is.list(input)|!all(c("Original_Signal", "Corrected_Signal", "Baseline") %in% names(input))) {
    stop("Input data must be output from function 'chrom_bline' or one of the baseline functions: 'bline_als', 'bline_chang', 'bline_poly', 'bline_isrea'!")
  }

  if(any(names(input)=="Bline_Plot")) warning("A baseline plot is already present in the input data!")

  if(!any(xvar=="auto") & (!is.vector(xvar) | !is.atomic(xvar) | !is.numeric(xvar))) {
    stop("The provided custom x-axis variable 'xvar' is not a suitable numeric vector!")
  } else if(!any(xvar=="auto") & is.atomic(xvar) & is.numeric(xvar) & length(xvar)!=length(input[[1]])) {
    stop("When 'xvar' is a numeric vector, its length must equal that of each of the input vectors!")
  } else if(any(xvar=="auto")) xvar <- seq(length(input[[1]]))

  bline_df <- lapply(seq_along(input)[1:3], function(x) {
    res <- cbind.data.frame("Group" = names(input)[x], "xname" = xvar, "Value" = input[[x]])
    colnames(res)[colnames(res) %in% "xname"] <- xname
    return(res)
  })
  bline_df <- do.call(rbind.data.frame, bline_df)

  cols <- c("black", "purple", "red")
  ltys <- c(1,2,1)
  names(cols) <- names(ltys) <- names(input)[1:3] #c("Original_Signal", "Corrected_Signal", "Baseline")

  if(!bline) {
    cols <- cols[-3]
    ltys <- ltys[-3]
    bline_df <- bline_df[!bline_df[,"Group"] %in% names(input)[3],]
  }

  if(!corrsig) {
    cols <- cols[-2]
    ltys <- ltys[-2]
    bline_df <- bline_df[!bline_df[,"Group"] %in% names(input)[2],]
  }

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_xvar <- "Index"
  aes_yvar <- "Value"
  aes_grpvar <- "Group"

  blineplot <- ggplot(data = bline_df, aes(x = .data[[aes_xvar]])) +
    geom_path(aes(y = .data[[aes_yvar]], group = .data[[aes_grpvar]], colour = .data[[aes_grpvar]], lty = .data[[aes_grpvar]]), lwd = 0.6) +
    scale_linetype_manual(name = "", values = ltys) +
    scale_colour_manual(name = "", values = cols) +
    labs(x = xname, y = "Signal", title = "Baseline Correction") +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 11, colour = "black"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom")

  return(blineplot)
}
