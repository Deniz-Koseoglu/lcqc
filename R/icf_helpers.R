#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Non-Linear Least Squares Iterative Curve Fitting (HELPER FUNCTIONS)
#Includes models:
#Gaussian (GS), Exponentially-Modified Gaussian (EMG), Exponential-Gaussian Hybrid Function (EGH),
#Empirically Transformed Gaussian (ETG)
#In development:
#Polynomial Modified Gaussian (PMG), Generalized Exponentially Modified Gaussian (GEMG),
#Parabolic-Lorentzian Modified Gaussian (PLMG)
#Supports groups of between 2-5 fused peaks, as well as single, baseline-resolved peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate peak half-widths at any peak apex fraction(s)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate peak half-widths at any peak height fraction(s)
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#' Calculates peak width and half-widths are specified peak height fractions (from 0 to 1).
#'
#' @param input A \code{data.frame} containing retention time and signal values (specified in \code{vars}) for the peak to be processed.
#' @param vars A \code{character} vector of retention time (x-axis) and signal (y-axis) column names. Defaults to \code{c("x","y")}.
#' @param accmax A \code{numeric} vector (or \code{data.frame}) containing retention time (x-axis) and signal (y-axis) of the peak maximum,
#' \strong{in that order}.
#' @param frac A \code{numeric} vector of peak height fractions (between 0.00 and 1.00) to calculate peak width and half-widths at.
#' @param resolved A \code{logical} indicating whether the peak to be assessed is baseline-resolved (\code{TRUE} by default).
#' @param slnt A \code{logical} switch determining whether information about results is printed in the console.
#'
#' @return A 4-column \code{data.frame} containing the peak height fraction(s) specified in \code{frac} (\code{"frac"}) with
#' associated total width (\code{"W"}), left half-width (\code{"A"}), and right half-width (\code{"B"}).
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{chrom_tplate}}, \code{\link{chrom_asym}}, \code{\link{chrom_tpa}}, \code{\link{chrom_res}}
#'
#' @keywords internal
peak_hw <- function(input, vars = c("x","y"), accmax = NULL, frac = 0.15, resolved = TRUE, slnt = FALSE) {

  #Perform checks
  accmax <- unlist(accmax)
  if(!is.null(accmax) & length(accmax)!=2) stop("Argument 'accmax' must be a numeric vector of length 2 (peak apex retention time and signal)!")
  if(any(frac<0) | any(frac>0.99)) stop("Peak height fraction 'frac' at which width is calculated must be between 0 and 0.99!")
  if(!all(vars %in% colnames(input))) stop("One or more variable names were not found in the 'input' data.frame!")
  if(any(frac==0) & !resolved & !slnt) cat("\nPeak width at base estimation is only available for baseline-resolved peaks!")

  #Define variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  rt_l <- min(rt, na.rm = TRUE)
  rt_h <- max(rt, na.rm = TRUE)
  sig_l <- sig[1]
  sig_h <- sig[length(sig)]

  #Derive maximum if 'accmax' is not given
  if(is.null(accmax)) {
    maxsig <- which.max(sig)
    accmax <- c(rt[maxsig], sig[maxsig])
    names(accmax) <- vars
  }

  #Results list
  res <- list()

  #Fraction of peak height at the beginning and end of the peak
  frac_locs <- sapply(c(sig_l, sig_h), function(x) x/accmax[2])

  #If peak width at base is to be calculated, mark this as a flag!
  frac_flag <- rep(FALSE, length(frac))
  if(any(frac==0) & resolved) frac_flag[which(frac==0)] <- TRUE

  #Peak Half-Widths at specific height fraction(s)
  for(i in seq_along(frac)) {
    #Check whether the minimum boundary is above 'frac'
    if(any(frac_locs > frac[i]) & !frac_flag[i]) {
      res[[i]] <- c(frac[i], rep(NA, 3))
    } else {
      #Find the first and last points where signal is above 'frac'
      thressig <- rep(accmax[2]*frac[i], 2)
      names(thressig) <- c("start","end")
      if(frac_flag[i]) {
        mind <- 2
        maxd <- length(sig)-1
        thressig[c("start","end")] <- c(sig_l, sig_h)
      } else {
        mind <- min(which(sig > thressig["start"]))
        maxd <- max(which(sig > thressig["end"]))
      }

      #Calculate half-widths as accurately as possible via linear interpolation
      ties_start <- ties_end <- mean
      repeat {
        rt_start <- try(approx(sig[c(mind-1,mind)], rt[c(mind-1,mind)], xout = thressig["start"], ties = ties_start)[["y"]], silent = TRUE)
        rt_end <- try(approx(sig[c(maxd,maxd+1)], rt[c(maxd,maxd+1)], xout = thressig["end"], ties = ties_end)[["y"]], silent = TRUE)
        errchk <- sapply(list(rt_start, rt_end), function(x) class(x) == "try-error")
        if(any(errchk)) {
          if(errchk[1]) ties_start <- "ordered"
          if(errchk[2]) ties_end <- "ordered"
        } else break
      }
      W <- abs(rt_end-rt_start)
      hw_A <- abs(accmax[1] - rt_start)
      hw_B <- abs(accmax[1] - rt_end)
      res[[i]] <- c(frac[i], W, hw_A, hw_B)
    }
  }
  res <- lapply(res, function(x) setNames(x, NULL))
  res <- do.call(rbind.data.frame, res)
  colnames(res) <- c("frac", "W", "A", "B")
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimate Sigma Squared and Tau for EGH, and other initial parameters for PLMG
#For EMG, ETG, and simple Gaussian models, separate functions are unnecessary...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Estimate initial Gaussian standard deviation and exponential time constant for EGH models
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' Estimates the standard deviation (sigma) of the Gaussian component and the time constant (tau) of the truncated exponential component
#' for the Exponential-Gaussian Hybrid (EGH) model from peak data. See \code{\link{chrom_icf}} for further details.
#'
#' @param A,B The \code{numeric} left and right (\code{A} and \code{B}, respectively) half-widths of the peak determined at \code{frac}.
#' @param frac The \code{numeric} peak height (signal) fraction (between 0 and 1) at which \code{A} and \code{B} were determined.
#'
#' @return A named \code{numeric} vector containing the estimated \code{"sigma"} and \code{"tau"} values.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{icf_EGH}}, \code{\link{egh_penalty}}
#'
#' @keywords internal
init_egh <- function(A = NA, B = NA, frac = NA) {

  #Perform checks
  if(any(frac<=0) | any(frac>0.99)) stop("Peak height fraction 'frac' at which width is calculated must be between 0 and 0.99!")

  if(any(is.na(c(frac,A,B)))) {
    cat("\nNo values were provided for EGH parameter estimation. Returning NA...")
    res <- NA
  } else {
    sigma <- sqrt(-1/(2*log(frac))*(B*A)) #log() calculates natural logarithm (ln) by default
    tau <- -1/log(frac)*(B-A)
    res <- c(sigma, tau)
    names(res) <- c("sigma", "tau")
  }
  return(res)
}

#For PLMG
#init_plmg <- function(A = NA, B = NA) {
#  #Perform checks
#  if(any(!is.numeric(c(A,B)))) stop("Both 'A' and 'B' must be numeric values!")
#
#  if(any(is.na(c(A,B)))) {
#    cat("\nNo values were provided for PLMG parameter estimation. Returning NA...")
#    res <- NA
#  } else {
#    #Assuming p = 1
#    sigma <- min(c(A,B), na.rm = TRUE)
#    d_est <- 2*((B*A-sigma^2)/(B-A))
#    m_est <- 1/(1+2*(d_est/(B-A)))
#    r_est <- d_est/(1-m_est*4.6) #4.6 is p10, see Caballero et al. (2002)
#    w_est <- 6*(r_est+3*sigma)
#    res <- c(sigma, d_est, m_est, r_est, w_est)
#  }
#  return(res)
#}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Builds a Gaussian function for up to 5 peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build a Gaussian model for one or more peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' Builds a Gaussian curve for up to 5 peaks to be used in conjunction with error calculation function \code{\link{gs_penalty}}.
#' See \code{\link{chrom_icf}} for details.
#'
#' @param H,H2,H3,H4,H5 The height at peak maximum for 1-5 peaks.
#' @param center,center2,center3,center4,center5 The retention time at peak maximum for 1-5 peaks.
#' @param sigma,sigma2,sigma3,sigma4,sigma5 The standard deviation of the Gaussian curve for 1-5 peaks.
#' @param t A \code{numeric} vector of peak retention times.
#'
#' @return A \code{list} of \code{numeric} vectors of Gaussian curve modeled signal corresponding to and equal
#' in length to retention time \code{t}.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{gs_penalty}}
#'
#' @keywords internal
icf_GS <- function(H, center, sigma,
                   H2 = NA, center2 = NA, sigma2 = NA,
                   H3 = NA, center3 = NA, sigma3 = NA,
                   H4 = NA, center4 = NA, sigma4 = NA,
                   H5 = NA, center5 = NA, sigma5 = NA,
                   t) {

  #Length of parameter set needed to model 1 peak
  reqlen <- 3

  #Get length of all non-NA parameters
  parvec <- c(H, center, sigma,
              H2, center2, sigma2,
              H3, center3, sigma3,
              H4, center4, sigma4,
              H5, center5, sigma5)
  parlen <- length(which(!is.na(parvec)))
  parvec <- parvec[seq(parlen)]

  #Error check for a complete parameter set
  if(length(parvec) %% reqlen != 0) stop("Incorrect number of parameters given for GS model!")

  #Number of peaks based on parameters required to model 1 peak
  npeak <- length(parvec)/reqlen

  #Create output list
  result <- rep(list(rep(0, length(t))), npeak)

  #Compile parameters into list for looping
  pars <- split(parvec, ceiling(seq_along(parvec)/reqlen))

  #Model Peaks
  for(i in seq_along(result)) {

    H_p <- pars[[i]][1]
    center_p <- pars[[i]][2]
    sigma_p <- pars[[i]][3]

    result[[i]] <- H_p*exp(-((t-center_p)/(2*sigma_p))^2)
  }
  return(result)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Builds an Exponential-Gaussian Hybrid function for up to 5 peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build an Exponential-Gaussian Hybrid (EGH) model for one or more peaks
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' Builds an Exponential-Gaussian Hybrid curve for up to 5 peaks to be used in conjunction with error calculation function \code{\link{egh_penalty}}.
#' Initial standard deviation and time constant for the curve may be estimated via \code{\link{init_egh}}.
#' See \code{\link{chrom_icf}} for details.
#'
#' @param H,H2,H3,H4,H5 The height at peak maximum for 1-5 peaks.
#' @param center,center2,center3,center4,center5 The retention time at peak maximum for 1-5 peaks.
#' @param sigma,sigma2,sigma3,sigma4,sigma5 The standard deviation of the Gaussian component of the curve for 1-5 peaks.
#' @param tau,tau2,tau3,tau4,tau5 The time constant of the truncated exponential component of the curve for 1-5 peaks.
#' @param t A \code{numeric} vector of peak retention times.
#'
#' @return A \code{list} of \code{numeric} vectors of EGH curve modelled signal corresponding to and equal
#' in length to retention time \code{t}.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{egh_penalty}}, \code{\link{init_egh}}
#'
#' @keywords internal
icf_EGH <- function(H, center, sigma, tau,
                    H2 = NA, center2 = NA, sigma2 = NA, tau2 = NA,
                    H3 = NA, center3 = NA, sigma3 = NA, tau3 = NA,
                    H4 = NA, center4 = NA, sigma4 = NA, tau4 = NA,
                    H5 = NA, center5 = NA, sigma5 = NA, tau5 = NA,
                    t) {

  #Length of parameter set needed to model 1 peak
  reqlen <- 4

  #Get length of all non-NA parameters
  parvec <- c(H, center, sigma, tau,
              H2, center2, sigma2, tau2,
              H3, center3, sigma3, tau3,
              H4, center4, sigma4, tau4,
              H5, center5, sigma5, tau5)
  parlen <- length(which(!is.na(parvec)))
  parvec <- parvec[seq(parlen)]

  #Error check for a complete parameter set
  if(length(parvec) %% reqlen != 0) stop("Incorrect number of parameters given for EGH model!")

  #Number of peaks based on parameters required to model 1 peak
  npeak <- length(parvec)/reqlen

  #Create output list
  result <- rep(list(rep(0, length(t))), npeak)

  #Compile parameters into list for looping
  pars <- split(parvec, ceiling(seq_along(parvec)/reqlen))

  #Model Peaks
  for(i in seq_along(result)) {

    H_p <- pars[[i]][1]
    center_p <- pars[[i]][2]
    sigma_p <- pars[[i]][3]
    tau_p <- pars[[i]][4]

    index <- which(2*sigma_p^2 + tau_p*(t-center_p)>0)
    result[[i]][index] <- H_p*exp(-(t[index]-center_p)^2/(2*sigma_p^2 + tau_p*(t[index]-center_p)))
  }
  return(result)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Builds an Exponentially-Modified Gaussian (EMG) function for up to 5 peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build an Exponentially-Modified Gaussian (EMG) curve
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' Builds an Exponentially-Modified Gaussian curve for up to 5 peaks to be used in conjunction with error calculation function \code{\link{emg_penalty}}.
#' See \code{\link{chrom_icf}} for details.
#'
#' @param A,A2,A3,A4,A5 The peak area for 1-5 peaks.
#' @param center,center2,center3,center4,center5 The retention time at peak maximum for 1-5 peaks.
#' @param sigma,sigma2,sigma3,sigma4,sigma5 The standard deviation of the Gaussian component of the curve for 1-5 peaks.
#' @param tau,tau2,tau3,tau4,tau5 The time constant of the truncated exponential component of the curve for 1-5 peaks.
#' @param t A \code{numeric} vector of peak retention times.
#' @param variant The chosen expression variant of the EMG function as a \code{character} string (sometimes affects optimization stability).
#' Current variants are \code{"emg1"} or \code{"emg2"}.
#'
#' @return A \code{list} of \code{numeric} vectors of EMG curve modelled signal corresponding to and equal
#' in length to retention time \code{t}.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{emg_penalty}}
#'
#' @keywords internal
#'
#' @importFrom stats pnorm
#' @importFrom stats pchisq
icf_EMG <- function(A, center, sigma, tau,
                    A2 = NA, center2 = NA, sigma2 = NA, tau2 = NA,
                    A3 = NA, center3 = NA, sigma3 = NA, tau3 = NA,
                    A4 = NA, center4 = NA, sigma4 = NA, tau4 = NA,
                    A5 = NA, center5 = NA, sigma5 = NA, tau5 = NA,
                    t, variant = "emg1") { #"emg1" "emg2"
  #Length of parameter set needed to model 1 peak
  reqlen <- 4

  #Get length of all non-NA parameters
  parvec <- c(A, center, sigma, tau,
              A2, center2, sigma2, tau2,
              A3, center3, sigma3, tau3,
              A4, center4, sigma4, tau4,
              A5, center5, sigma5, tau5)
  parlen <- length(which(!is.na(parvec)))
  parvec <- parvec[seq(parlen)]

  #Error check for a complete parameter set
  if(length(parvec) %% reqlen != 0) stop("Incorrect number of parameters given for EMG model!")

  #Number of peaks based on parameters required to model 1 peak
  npeak <- length(parvec)/reqlen

  #Create output list
  result <- rep(list(rep(0, length(t))), npeak)

  #Compile parameters into list for looping
  pars <- split(parvec, ceiling(seq_along(parvec)/reqlen))

  #Model Peaks
  for(i in seq_along(result)) {

    A_p <- pars[[i]][1]
    center_p <- pars[[i]][2]
    sigma_p <- pars[[i]][3]
    tau_p <- pars[[i]][4]

    if(variant=="emg1") {
      erf_exp <- (t-center_p)/(sqrt(2)*sigma_p) - sigma_p/(sqrt(2)*tau_p)
      erf_res <- pchisq(2 * erf_exp^2, 1) * sign(erf_exp)
      result[[i]] <- A_p/(2*tau_p)*exp(((sigma_p^2)/(2*tau_p^2))+(center_p-t)/sigma_p)*(1+erf_res)
    } else if(variant=="emg2") {
      qs <- (sigma_p/tau_p)^2/2 - (t-center_p)/tau_p
      zs <- (t-center_p)/sigma_p - sigma_p/tau_p
      Is <- pnorm(zs)
      result[[i]] <- (A_p * exp(qs)*Is)/tau_p
    }
  }
  return(result)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Builds an Empirically Transformed Gaussian (ETG) function for up to 5 peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Build an Empirically-Transformed Gaussian (ETG) curve
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' Builds an Empirically-Transformed Gaussian curve for up to 5 peaks to be used in conjunction with error calculation function \code{\link{etg_penalty}}.
#' See \code{\link{chrom_icf}} for details.
#'
#' @param H,H2,H3,H4,H5 The peak area for 1-5 peaks.
#' @param tl,tl2,tl3,tl4,tl5 The retention times of \strong{left} inflection points for 1-5 peaks. \strong{This parameter remains constant}.
#' @param tt,tt2,tt3,tt4,tt5 The retention times of \strong{right} inflection points for 1-5 peaks. \strong{This parameter remains constant}.
#' @param kl,kl2,kl3,kl4,kl5 Parameter of the \strong{leading} edge for 1-5 peaks, originally defined by a reciprocal of Gaussian standard deviation.
#' @param kt,kt2,kt3,kt4,kt5 Parameter of the \strong{trailing} edge for 1-5 peaks, originally defined by a reciprocal of Gaussian standard deviation.
#' @param laml,laml2,laml3,laml4,laml5 Parameter two of the \strong{leading} edge for 1-5 peaks.
#' @param lamr,lamr2,lamr3,lamr4,lamr5 Parameter two of the \strong{trailing} edge for 1-5 peaks.
#' @param a,a2,a3,a4,a5 Parameter three of the \strong{leading} edge for 1-5 peaks.
#' @param b,b2,b3,b4,b5 Parameter three of the \strong{trailing} edge for 1-5 peaks.
#' @param t A \code{numeric} vector of peak retention times.
#'
#' @return A \code{list} of \code{numeric} vectors of ETG curve modeled signal corresponding to and equal
#' in length to retention time \code{t}.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{etg_penalty}}
#'
#' @keywords internal
icf_ETG <- function(H, tl, tt, kl, kt, laml, lamr, a, b,
                    H2 = NA, tl2 = NA, tt2 = NA, kl2 = NA, kt2 = NA, laml2 = NA, lamr2 = NA, a2 = NA, b2 = NA,
                    H3 = NA, tl3 = NA, tt3 = NA, kl3 = NA, kt3 = NA, laml3 = NA, lamr3 = NA, a3 = NA, b3 = NA,
                    H4 = NA, tl4 = NA, tt4 = NA, kl4 = NA, kt4 = NA, laml4 = NA, lamr4 = NA, a4 = NA, b4 = NA,
                    H5 = NA, tl5 = NA, tt5 = NA, kl5 = NA, kt5 = NA, laml5 = NA, lamr5 = NA, a5 = NA, b5 = NA,
                    t) {
  #NOTE THAT tl and tt are CONSTANT VALUES (NOT OPTIMIZED!)

  #Length of parameter set needed to model 1 peak
  reqlen <- 9 #Only 7 of these are optimized!

  #Get length of all non-NA parameters
  parvec <- c(H, tl, tt, kl, kt, laml, lamr, a, b,
              H2, tl2, tt2, kl2, kt2, laml2, lamr2, a2, b2,
              H3, tl3, tt3, kl3, kt3, laml3, lamr3, a3, b3,
              H4, tl4, tt4, kl4, kt4, laml4, lamr4, a4, b4,
              H5, tl5, tt5, kl5, kt5, laml5, lamr5, a5, b5)
  parlen <- length(which(!is.na(parvec)))
  parvec <- parvec[seq(parlen)]

  #Error check for a complete parameter set
  if(length(parvec) %% reqlen != 0) stop("Incorrect number of parameters given for ETG model!")

  #Number of peaks based on parameters required to model 1 peak
  npeak <- length(parvec)/reqlen

  #Create output list
  result <- rep(list(rep(0, length(t))), npeak)

  #Compile parameters into list for looping
  pars <- split(parvec, ceiling(seq_along(parvec)/reqlen))

  #Model Peaks
  for(i in seq_along(result)) {

    H_p <- pars[[i]][1]
    tl_p <- pars[[i]][2]
    tt_p <- pars[[i]][3]
    kl_p <- pars[[i]][4]
    kt_p <- pars[[i]][5]
    laml_p <- pars[[i]][6]
    lamr_p <- pars[[i]][7]
    a_p <- pars[[i]][8]
    b_p <- pars[[i]][9]

    result[[i]] <- 2*H_p*exp(0.5)/((1+laml_p*exp(kl_p * (tl_p - t)))^a_p + (1 + lamr_p * exp(kt_p * (t - tt_p)))^b_p - 1)
  }
  return(result)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Builds a Parabolic-Lorentzian Modified Gaussian function for up to 5 peaks
#See the following work: https://aaltodoc.aalto.fi/bitstream/handle/123456789/115198/master_Kuikka_Juho_2022.pdf?sequence=1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#icf_PLMG <- function(H, center, sigma, d_par, m_par, r_par, w_par,
#                     H2 = NA, center2 = NA, sigma2 = NA, d_par2 = NA, m_par2 = NA, r_par2 = NA, w_par2 = NA,
#                     H3 = NA, center3 = NA, sigma3 = NA, d_par3 = NA, m_par3 = NA, r_par3 = NA, w_par3 = NA,
#                     H4 = NA, center4 = NA, sigma4 = NA, d_par4 = NA, m_par4 = NA, r_par4 = NA, w_par4 = NA,
#                     H5 = NA, center5 = NA, sigma5 = NA, d_par5 = NA, m_par5 = NA, r_par5 = NA, w_par5 = NA,
#                     t) {
#
#  #Length of parameter set needed to model 1 peak
#  reqlen <- 7
#
#  #Get length of all non-NA parameters
#  parvec <- c(H, center, sigma, d_par, m_par, r_par, w_par,
#              H2, center2, sigma2, d_par2, m_par2, r_par2, w_par2,
#              H3, center3, sigma3, d_par3, m_par3, r_par3, w_par3,
#              H4, center4, sigma4, d_par4, m_par4, r_par4, w_par4,
#              H5, center5, sigma5, d_par5, m_par5, r_par5, w_par5)
#  parlen <- length(which(!is.na(parvec)))
#  parvec <- parvec[seq(parlen)]
#
#  #Error check for a complete parameter set
#  if(length(parvec) %% reqlen != 0) stop("Incorrect number of parameters given for GS model!")
#
#  #Number of peaks based on parameters required to model 1 peak
#  npeak <- length(parvec)/reqlen
#
#  #Create output list
#  result <- rep(list(rep(0, length(t))), npeak)
#
#  #Compile parameters into list for looping
#  pars <- split(parvec, ceiling(seq_along(parvec)/reqlen))
#
#  #Model Peaks
#  for(i in seq_along(result)) {
#
#    H_p <- pars[[i]][1]
#    center_p <- pars[[i]][2]
#    sigma_p <- pars[[i]][3]
#    d_p <- pars[[i]][4]
#    m_p <- pars[[i]][5]
#    r_p <- pars[[i]][6]
#    w_p <- pars[[i]][7]
#
#    fsigmas <- sigma_p^2 + m_p*((t-center_p+d_p)^2/(1+((t-center_p+r_p)^2/(w_p^2))))
#    result[[i]] <- H_p * exp(-0.5*((t-center_p)^2/fsigmas))
#  }
#  return(result)
#}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTOIN: Derive an RMSE optimization metric (to be minimized)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title RMSE calculation for iterative curve fitting optimization
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_icf}} workflow, not intended for standalone use.
#'
#' The Root Mean Squared Error (RMSE) calculation function to use for iterative optimization of various Iterative Curve Fitting (ICF) models.
#' Currently available models include the simple Gaussian (GS), Expontentially-Modified Gaussian (EMG), Exponential-Gaussian Hybrid (EGH),
#' and Empricially Transformed Gaussian (ETG). See \code{\link{chrom_icf}} for further details.
#'
#' @param pars A \code{numeric} vector of parameters to pass to curve fitting function.
#' @param input A \code{data.frame} containing \strong{x} and \strong{y} data from which to model the curve.
#' @param vars A \code{character} vector of length 2 specifying the column names of \strong{x} and \strong{y} variables in \code{input}.
#' Defaults to \code{c("x","y")}.
#' @param infs \strong{Exclusive to \code{\link{etg_penalty}}}. A \code{numeric} vector of inflection point times, \strong{alternating}
#' between \strong{left} (leading) and \strong{right} (trailing) inflection points.
#' @param repr Some models may be written/represented in more than one way mathematically (some are more stable than others for optimization).
#' Current representations include: EMG (\code{"emg1"} or \code{"emg2"}).
#'
#' @return The \code{numeric} Root Mean Squared Error (RMSE) of the model derived using chosen \code{pars}.
#'
#' @seealso \code{\link{chrom_icf}}, \code{\link{icf_GS}}, \code{\link{icf_EGH}}, \code{\link{icf_EMG}}, \code{\link{icf_ETG}}
#'
#' @keywords internal
gs_penalty <- function(pars, input, vars = c("x", "y")) {
  #Perform checks
  if(!is.data.frame(input)) stop("Input data must be an XY data.frame containing retention times and the chromatographic signal!")
  if(!all(vars %in% colnames(input))) stop("Variable names not found in input data.frame! Check the 'vars' argument.")
  if(length(pars)%%3!=0) stop("The number of parameters should be a multiple of 3 for Gaussian models!")

  #Retrieve variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  n <- length(rt)

  #Pad parameters with NAs if length is less than that required for a maximum-size group
  max_l <- 15
  if(length(pars)!=max_l) pars <- append(pars, rep(NA, max_l-length(pars)))

  #Create model
  modres <-   icf_GS(H = pars[1], center = pars[2], sigma = pars[3],
                     H2 = pars[4], center2 = pars[5], sigma2 = pars[6],
                     H3 = pars[7], center3 = pars[8], sigma3 = pars[9],
                     H4 = pars[10], center4 = pars[11], sigma4 = pars[12],
                     H5 = pars[13], center5 = pars[14], sigma5 = pars[15],
                     t = rt)

  #Sum the list of modeled peaks by element
  modres <- Reduce(`+`, modres)

  #Derive RMSE
  metric_res <- sum(sqrt((modres-sig)^2)/n)
  return(metric_res)
}

#' @rdname gs_penalty
#' @keywords internal
egh_penalty <- function(pars, input, vars = c("x","y")) {

  #Perform checks
  if(!is.data.frame(input)) stop("Input data must be an XY data.frame containing retention times and the chromatographic signal!")
  if(!all(vars %in% colnames(input))) stop("Variable names not found in input data.frame! Check the 'vars' argument.")
  if(length(pars)%%4!=0) stop("The number of parameters should be a multiple of 4 for EGH!")

  #Retrieve variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  n <- length(rt)

  #Pad parameters with NAs if length is less than that required for a maximum-size group
  max_l <- 20
  if(length(pars)!=max_l) pars <- append(pars, rep(NA, max_l-length(pars)))

  modres <- icf_EGH(H = pars[1], center = pars[2], sigma = pars[3], tau = pars[4],
                    H2 = pars[5], center2 = pars[6], sigma2 = pars[7], tau2 = pars[8],
                    H3 = pars[9], center3 = pars[10], sigma3 = pars[11], tau3 = pars[12],
                    H4 = pars[13], center4 = pars[14], sigma4 = pars[15], tau4 = pars[16],
                    H5 = pars[17], center5 = pars[18], sigma5 = pars[19], tau5 = pars[20],
                    t = rt)

  #Sum the list of modeled peaks by element
  modres <- Reduce(`+`, modres)

  #Derive RMSE
  metric_res <- sum(sqrt((modres-sig)^2)/n)
  return(metric_res)
}

#' @rdname gs_penalty
#' @keywords internal
emg_penalty <- function(pars, input, vars = c("x","y"), repr = "emg1") {

  #Perform checks
  if(!is.data.frame(input)) stop("Input data must be an XY data.frame containing retention times and the chromatographic signal!")
  if(!all(vars %in% colnames(input))) stop("Variable names not found in input data.frame! Check the 'vars' argument.")
  if(length(pars)%%4!=0) stop("The number of parameters should be a multiple of 4 for EMG!")

  #Retrieve variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  n <- length(rt)

  #Pad parameters with NAs if length is less than that required for a maximum-size group
  max_l <- 20
  if(length(pars)!=max_l) pars <- append(pars, rep(NA, max_l-length(pars)))

  modres <- icf_EMG(A = pars[1], center = pars[2], sigma = pars[3], tau = pars[4],
                    A2 = pars[5], center2 = pars[6], sigma2 = pars[7], tau2 = pars[8],
                    A3 = pars[9], center3 = pars[10], sigma3 = pars[11], tau3 = pars[12],
                    A4 = pars[13], center4 = pars[14], sigma4 = pars[15], tau4 = pars[16],
                    A5 = pars[17], center5 = pars[18], sigma5 = pars[19], tau5 = pars[20],
                    t = rt, variant = repr)

  #Sum the list of modeled peaks by element
  modres <- Reduce(`+`, modres)

  #Derive RMSE
  metric_res <- sum(sqrt((modres-sig)^2)/n)
  return(metric_res)
}

#' @rdname gs_penalty
#' @keywords internal
etg_penalty <- function(pars, input, vars = c("x","y"), infs) {
  #Perform checks
  if(!is.data.frame(input)) stop("Input data must be an XY data.frame containing retention times and the chromatographic signal!")
  if(!all(vars %in% colnames(input))) stop("Variable names not found in input data.frame! Check the 'vars' argument.")
  if(length(pars)%%7!=0) stop("The number of parameters should be a multiple of 7 for ETG models!")
  if(length(infs)/2 != length(pars)/7) stop("Incorrect number of inflection points ('infs') provided for the number of parameters ('pars')!")

  #Retrieve variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  n <- length(rt)

  #Pad parameters with NAs if length is less than that required for a maximum-size group
  max_l <- 35
  if(length(pars)!=max_l) pars <- append(pars, rep(NA, max_l-length(pars)))

  #Also pad vector of inflection points
  max_linf <- 10
  if(length(infs)!=max_linf) infs <- append(infs, rep(NA, max_linf-length(infs)))

  modres <- icf_ETG(H = pars[1], kl = pars[2], kt = pars[3], laml = pars[4], lamr = pars[5], a = pars[6], b = pars[7],
                    H2 = pars[8], kl2 = pars[9], kt2 = pars[10], laml2 = pars[11], lamr2 = pars[12], a2 = pars[13], b2 = pars[14],
                    H3 = pars[15], kl3 = pars[16], kt3 = pars[17], laml3 = pars[18], lamr3 = pars[19], a3 = pars[20], b3 = pars[21],
                    H4 = pars[22], kl4 = pars[23], kt4 = pars[24], laml4 = pars[25], lamr4 = pars[26], a4 = pars[27], b4 = pars[28],
                    H5 = pars[29], kl5 = pars[30], kt5 = pars[31], laml5 = pars[32], lamr5 = pars[33], a5 = pars[34], b5 = pars[35],
                    tl = infs[1], tt = infs[2], tl2 = infs[3], tt2 = infs[4], tl3 = infs[5], tt3 = infs[6],
                    tl4 = infs[7], tt4 = infs[8], tl5 = infs[9], tt5 = infs[10],
                    t = rt)

  #Sum the list of modeled peaks by element
  modres <- Reduce(`+`, modres)

  #Derive RMSE
  metric_res <- sum(sqrt((modres-sig)^2)/n)
  return(metric_res)
}

#For PLMG (Parabolic-Lorentzian Modified Gaussian)
#plmg_penalty <- function(pars, input, vars = c("x","y")) {
#
# #Perform checks
#  if(!is.data.frame(input)) stop("Input data must be an XY data.frame containing retention times and the chromatographic signal!")
#  if(!all(vars %in% colnames(input))) stop("Variable names not found in input data.frame! Check the 'vars' argument.")
#  if(length(pars)%%7!=0) stop("The number of parameters should be a multiple of 7 for PLMG!")
#
#  #Retrieve variables
#  rt <- input[,vars[1]]
#  sig <- input[,vars[2]]
#  n <- length(rt)
#
#  #Pad parameters with NAs if length is less than that required for a maximum-size group
#  max_l <- 35
#  if(length(pars)!=max_l) pars <- append(pars, rep(NA, max_l-length(pars)))
#
#  modres <- icf_PLMG(H = pars[1], center = pars[2], sigma = pars[3], d_par = pars[4], m_par = pars[5], r_par = pars[6], w_par = pars[7],
#                     H2 = pars[8], center2 = pars[9], sigma2 = pars[10], d_par2 = pars[11], m_par2 = pars[12], r_par2 = pars[13], w_par2 = pars[14],
#                     H3 = pars[15], center3 = pars[16], sigma3 = pars[17], d_par3 = pars[18], m_par3 = pars[19], r_par3 = pars[20], w_par3 = pars[21],
#                     H4 = pars[22], center4 = pars[23], sigma4 = pars[24], d_par4 = pars[25], m_par4 = pars[26], r_par4 = pars[27], w_par4 = pars[28],
#                     H5 = pars[29], center5 = pars[30], sigma5 = pars[31], d_par5 = pars[32], m_par5 = pars[33], r_par5 = pars[34], w_par5 = pars[35],
#                     t = rt)
#
#  #Sum the list of modeled peaks by element
#  modres <- Reduce(`+`, modres)
#
#  #Derive RMSE
#  metric_res <- sum(sqrt((modres-sig)^2)/n)
#  return(metric_res)
#}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Visualize integration baselines on a graph
#Visualize the results (including group/common and individual baselines)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Visualize the results of Non-Linear Least Squares Iterative Curve Fitting (ICF)
#'
#' @description Visualizes the results of Non-Linear Least Squares Iterative Curve Fitting (ICF) of chromatographic data
#' obtained via \code{\link{chrom_icf}}.
#'
#' @param input The \code{data.frame} of data output from the \code{[["main_data"]]} element of \code{\link{chrom_skim}}.
#' @param cols Plot colour palette. Either \code{"default"} or a \strong{named} \code{character} vector of colours.
#' Names denote the following plot elements: original chromatogram (\code{"main"}), Gaussian (\code{"gs"}), EGH (\code{"egh"}),
#' EMG (\code{"emg"}), and ETG (\code{"etg"}) grouped models, unmodeled peaks (\code{"none"}),
#' and models for individual peaks rather than groups (\code{"indiv"}).
#' @param plabs Plot labels. Either \code{"default"} or a \strong{named} \code{character} vector of labels for the following elements:
#' main plot title (\code{"main"}), retention time (\code{"x"}), and signal (\code{"y"}).
#' @param plotsum A \code{logical} switch. Should summed models for peak groups be plotted? Defaults to \code{TRUE}.
#' @param plotind A \code{logical} switch. Should individual peak models be plotted? Defaults to \code{FALSE}.
#' @param fillplot A \code{logical} specifying whether peak models should be coloured/filled (\code{TRUE} by default).
#' When \code{FALSE}, only outlines of peak models are plotted.
#' @param txtmax A \code{logical} switch specifying whether to plot peak numbers at apices. Defaults to \code{FALSE}.
#' Only applies to plots of individual peak models (\code{plotind = TRUE}).
#' @param norm A \code{logical} specifying whether to normalize the chromatographic signal (y-axis) to a percentage scale (0 to 100).
#' Defaults to \code{FALSE}.
#' @param asprat Aspect ratio of the plot (defaults to \code{0.71}).
#'
#' @return A \code{ggplot}-class object containing the plot.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Get data and plot
#' icf_res <- lcqc:::wf_icf[["main_data"]]
#' icf_plot(icf_res)
#'
#' #Normalized plot with models for individual peaks included
#' icf_plot(icf_res, plotind = TRUE, norm = TRUE)
#' }
#'
#' @seealso \code{\link{chrom_icf}}
icf_plot <- function(input, cols = "default", plabs = "default", plotsum = TRUE, plotind = FALSE, fillplot = TRUE,
                     txtmax = TRUE, norm = FALSE, asprat = 0.71) { #c("red", "darkred", "blue", "darkgreen", "purple")

  #Preliminary checks
  #Check that at least one of the modeled peak plotting functions is on
  if(!any(c(plotsum, plotind))) stop("At least one of 'plotsum' and 'plotind' must be TRUE!")

  #Check for input object
  if(!is.data.frame(input) & !all(c("x", "y", "bline", "modsum_y") %in% colnames(input))) stop("Input data must be the 'main_data' output element of function 'chrom_icf'!")

  #Set up plot colours
  defcols <- c(main = "grey30", gs = "blue" , egh = "darkgreen", emg = "purple", etg = "orange", none = "darkred", indiv = "darkred")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  #Set up plot labels
  deflabs <- c(main = "Truncated chromatogram with integration baselines", x = "Time (min)", y = "Signal")
  plabs <- supp_pars(pars = plabs, defpars = deflabs, parlb = "plabs")

  #Correct modeled data for common baselines
  input[,grep("modsum_y|peak_", colnames(input))] <- lapply(grep("modsum_y|peak_", colnames(input)), function(x) {
    input[,x] <- input[,x] + input[,"bline"]
    return(input[,x])
  })

  #Convert model type to factor
  input[,"model_type"] <- factor(input[,"model_type"], levels = unique(input[,"model_type"]))

  #Optionally scale the plot signal to a percentage (0 to 100)
  if(norm) {
    #Get the maximum of y signal
    maxsig <- max(input[,"corr_y"], na.rm = TRUE)
    input[,"corr_y"] <- input[,"corr_y"]/maxsig*100
    input[,"modsum_y"] <- input[,"modsum_y"]/maxsig*100
    input[,grep("peak_",colnames(input))] <- input[,grep("peak_",colnames(input))]/maxsig*100
  }

  #Create the plot
  cat("\nPlotting ICF model results...\n")

  modplot <- list()

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_xvar <- "x"
  aes_yvar <- "corr_y"

  baseplot <- ggplot(data = input, aes(x = .data[[aes_xvar]])) +
    geom_path(aes(y = .data[[aes_yvar]]), col = cols["main"]) +
    scale_colour_manual(name = "", values = c("gs" = cols[["gs"]],
                                              "egh" = cols[["egh"]],
                                              "emg" = cols[["emg"]],
                                              "etg" = cols[["etg"]],
                                              "none" = cols[["none"]])) +
    scale_fill_manual(name = "", values = c("gs" = cols[["gs"]],
                                            "egh" = cols[["egh"]],
                                            "emg" = cols[["emg"]],
                                            "etg" = cols[["etg"]],
                                            "none" = cols[["none"]])) +
    scale_x_continuous(breaks = breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = breaks_pretty(n = 6)) +
    coord_cartesian(xlim = c(input[min(which(!is.na(input[,"bline"])))-3,"x"],
                             input[max(which(!is.na(input[,"bline"])))+3,"x"])) +
    labs(title = if(plabs["main"]=="") NULL else plabs["main"],
         x = plabs["x"],
         y = plabs["y"]) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour = "black"))

  if(plotsum) {

    #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
    aes_msum <- "modsum_y"
    aes_grpvar <- "group"
    aes_mtype <- "model_type"

    modplot[["Modeled_Groups"]] <- baseplot + geom_path(data = input[!is.na(input[,"bline"]) & !is.na(input[,"modsum_y"]),], aes(y = .data[[aes_msum]], group = .data[[aes_grpvar]], col = .data[[aes_mtype]]), lwd = 1.02) +
      {if(fillplot) geom_ribbon(data = input[!is.na(input[,"bline"]) & !is.na(input[,"modsum_y"]),], aes(ymax = .data[[aes_msum]], fill = .data[[aes_mtype]], group = .data[[aes_grpvar]]), ymin = 0, alpha = 0.15)}
  }

  if(plotind) {
    modplot[["Modeled_Peaks"]] <- baseplot
    for(i in grep("peak_", colnames(input), value = TRUE)) {
      modplot[["Modeled_Peaks"]] <- modplot[["Modeled_Peaks"]] + geom_path(aes(y = .data[[i]]), col = cols["indiv"], lwd = 1.02, na.rm = TRUE) +
        {if(fillplot) geom_ribbon(aes(ymax = .data[[i]]), ymin = 0, fill = cols[6], alpha = 0.15)}
    }

    if(txtmax) {
      #Retrieve max values of MODELED peaks
      pkcols <- grep("peak_", colnames(input))
      pkseq <- seq_along(pkcols)
      pkinds <- sapply(pkcols, function(x) which.max(input[,x]))
      pkmax <- sapply(pkcols, function(x) max(input[,x], na.rm = TRUE))
      maxs <- cbind.data.frame(ind = pkseq, x = input[pkinds,"x"], y = pkmax)

      #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
      aes_yvar2 <- "y"

      #Add text label layer to the individual peaks plot
      modplot[["Modeled_Peaks"]] <- modplot[["Modeled_Peaks"]] +
        geom_text(data = maxs, aes(y = .data[[aes_yvar2]]), label = maxs[,"ind"], col = cols["none"], nudge_x = -0.005*max(maxs[,"x"], na.rm = TRUE), nudge_y = 0.04*max(maxs[,"y"], na.rm = TRUE), check_overlap = TRUE)
    }
  }

  #Return the plot(s)
  return(modplot)
}
