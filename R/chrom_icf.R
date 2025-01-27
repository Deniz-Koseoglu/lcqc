#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Model peaks and (optionally) baseline-resolved peaks via Iterative Non-Linear Least Squares Curve Fitting
#Includes models:
#Gaussian (GS), Exponentially-Modified Gaussian (EMG), Exponential-Gaussian Hybrid Function (EGH),
#Empirically Transformed Gaussian (ETG)
#In development:
#Polynomial Modified Gaussian (PMG), Generalized Exponentially Modified Gaussian (GEMG),
#Parabolic-Lorentzian Modified Gaussian (PLMG)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Non-Linear Least Squares Iterative Curve Fitting (ICF) of chromatographic data using various models
#'
#' @description The workflow uses data from peaks detected by \code{\link{chrom_detect}} to model chromatographic peaks by
#' iteratively fitting simple Gaussian and modified Gaussian models of varying complexity.
#' See \strong{Details} for further information.
#'
#' @param input The output \code{list} of data from \code{\link{chrom_detect}}.
#' @param method The \code{character} vector of method(s) to use for curve fitting. One or more of several available models:
#' simple Gaussian (\code{"gs"}), Exponentially-Modified Gaussian (\code{"emg"}), Exponential-Gaussian Hybrid (\code{"egh"}),
#' Empirically-Transformed Gaussian (\code{"etg"}), or \code{"all"}
#' (default value which applies all available models and picks the best-performing model individually for each peak or peak group).
#' @param crit_w The critical width parameter used to calculate peak group baselines via \code{\link{fastchrom_bline}}.
#' @param optmet The \code{character} vector of method(s) to use for iterative optimization as outlined in \code{\link[stats]{optim}}.
#' One or more of: \code{"Nelder-Mead"}, \code{"BFGS"}, \code{"L-BFGS-B"}, \code{"SANN"}, or \code{"all"} (default).
#' @param reprs A \strong{named} \code{character} vector of model representations (where more than one is available).
#' Names must be present in \code{method}. Currently available representations are:
#' EMG (\code{"emg1"} or \code{"emg2"}).
#' @param modres A \code{logical} switch which determines whether baseline-resolved peaks are modeled (\code{FALSE} by default).
#' @param plotset A \code{character} string specifying whether data is visualized/shown via \code{\link{icf_plot}}.
#' One of \code{"make"} (generates plots without printing; default), \code{"print"} (generates and prints plots), or \code{"none"}.
#' @param asprat Aspect ratio of the plot (defaults to \code{0.71}).
#'
#' @return A \code{list} of length 5 containing the following elements:
#' \describe{
#'  \item{main_data}{A \code{data.frame} containing comprehensive results of ICF. Columns include data indices (\code{"ind"}),
#'  retention time (\code{"x"}), original signal (\code{"orig_y"}), that corrected for the \strong{global} baseline
#'  (\code{"corr_y"}; see also \code{\link{chrom_bline}}), the individual peak or peak group baseline obtained via the
#'  FastChrom algorithm (\code{"bline"}; see also \code{\link{fastchrom_bline}}), the final baseline-corrected signal (\code{"y"}),
#'  ICF model type used (\code{"model_type"}), the summed model curve for all peaks within a group (\code{"modsum_y"}),
#'  one column each for individual peak models (\code{paste0("peak_", peak_ID)}),
#'  and the baseline-resolved peak group ID (\code{"group"}).}
#'  \item{integ_res}{A \code{data.frame} containing the integration results for each peak, including the type of model used
#'  (\code{"model_type"}), the associated RMSE (\code{"model_rmse"}), and the integrated peak area (\code{"pa"}).}
#'  \item{information}{A \code{character} string of various statements about the results.}
#'  \item{call}{The function call.}
#'  \item{modplot}{An (optional) list of \code{ggplot} objects containing \code{\link{icf_plot}} visualization(s) of results.}
#' }
#'
#' @details
#' Iterative curve fitting (ICF) fits several Gaussian and modified Gaussian models to either a single or a group of chromatographic peaks
#' and minimizes the associated Root Mean Squared Error (RMSE) of the resulting model through iterative optimization. Thus, a
#' model that resembles actual chromatographic signal as closely as possible within the constraints of the chosen model is obtained.
#' ICF is especially useful as a peak deconvolution method for moderately or even completely fused peaks. The \code{\link{chrom_icf}}
#' workflow currently implements 3 popular ICF models and offers active selection of the most appropriate model for each examined peak
#' or a group of up to 5 fused peaks. The available models include a simple Gaussian model (GS), the Exponentially-Modified Gaussian (EMG),
#' and the Exponential-Gaussian Hybrid (EGH) that is considered a simpler, more robust alternative to EMG with faster computation time.
#'
#' The GS model is based on the following equation (Nikitas et al., 2001):
#' \deqn{H_{gs} = H_m\times e^{-((t_R-t_m)/2\sigma)^2}}
#' Where \eqn{H_m} and \eqn{t_R} are the signal and retention time at peak maximum, respectively, \eqn{t_R} is the current retention time,
#' and \eqn{\sigma} is the standard deviation of the curve, effectively approximated as half of the peak width at inflection point height.
#' The Gaussian model thus has only 3 parameters that require optimization and is therefore computationally cheap, but cannot accommodate
#' fronting or tailing phenomena often observed in chromatographic data.
#'
#' The EMG model (e.g. Li, 1995; Nikitas et al., 2001; Caballero et al., 2002; Kalambet et al., 2011) incorporates an exponential
#' component into the Gaussian model and is by far the most popular approach to modeling chromatographic peaks with tailing and fronting:
#' \deqn{H_{emg} = A\times e^{0.5\times(\sigma/\tau)^2 - ((t_R-t_0)/\tau)}\times\mathcal{P}((t_R-t_0)/\sigma - \sigma/\tau)/\tau}
#' Sometimes also written as (Li, 1997):
#' \deqn{H_{emg} = A/2\tau\times e^{(\sigma^2/2\tau^2 + (t_0-t_R)/\tau)}\times(1+erf((t_R-t_0)/\sqrt{2\sigma} - \sigma/\sqrt{2\tau}))}
#' Where new parameters \eqn{A} and \eqn{\tau} are, respectively, the peak area and the exponential time constant, which is negative
#' for fronting peaks and positive for tailing peaks. The EMG model takes longer than GS or EGH to compute and appears to be less stable.
#'
#' The EGH model (Lan & Jorgenson, 2001; Li, 2002) is a simplified empirical equation that also accommodates fronting and tailing
#' peaks by including a truncated exponential component into the equation:
#' \deqn{H_{egh} = H_m\times e^{-(t_{R2}-t_0)^2/(2\sigma^2 + \tau\times(t_{R2}-t_0))}}
#' Where \eqn{t_{R2}} are those retention times where \eqn{(2\sigma^2 + \tau\times(t_R-t_0)) > 0}. EGH converges significantly faster
#' than EMG, but somewhat lacks flexibility for modeling fronting peaks (similarly to EMG).
#'
#' Finally, the ETG model is unique in incorporating 6 parameters describing the \strong{leading} (\eqn{k_l}, \eqn{\lambda_l},
#' and \eqn{\alpha}) and \strong{trailing} (\eqn{k_r}, \eqn{\lambda_r}, and \eqn{\beta}) peak edges, respectively. Including
#' the peak height \eqn{H_m}, a total of 7 parameters are optimized. Additionally, the function requires estimates of the left
#' and right inflection point times (\eqn{t_l} and \eqn{t_r}) for each peak, which remain constant and are not optimized:
#' \deqn{H_{etg} = (2H_me^{0.5})/((1 + \lambda_l e^{k_l(t_l - t)})^\alpha + (1 + \lambda_r e^{k_r(t - t_r)})^\beta - 1)}
#' The ETG model offers a unique advantage of loose coupling between the descriptions of leading and trailing peak edges, which
#' are only related by one peak amplitude parameter (derived from peak height). Additionally, despite the iterative optimization
#' of 7 parameters, the fitting procedure converges rapidly (is not computationally expensive) - often more so than EMG.
#'
#' All of the above functions are submitted for iterative optimization via a penalty function based on the Root Mean Squared Error (RMSE),
#' which may be represented as follows:
#' \deqn{RMSE = \sum{\sqrt{(y_i - \hat{y}_i)^2/n}}}
#' Where \eqn{y_i}, \eqn{\hat{y}_i}, and \eqn{n} are the original signal, modeled curve, and sample size (number of points in the original data),
#' respectively. This error metric is used to evaluate model performance and select the best-suited model among GS, EMG, EGH, and ETG during
#' iterative optimization via the \code{\link[stats]{optim}} function.
#'
#' @references
#' Kalambet, Y., Kozmin, Y., Mikhailova, K., Nagaev, I., Tikhonov, P. (2011), 'Reconstruction of Chromatographic Peaks using the Exponentially Modified Gaussian Function', \emph{Journal of Chemometrics} \strong{25} (7), pp. 352-356, DOI: \url{https://doi.org/10.1002/cem.1343}.
#'
#' Li (1995), 'A Simplified Exponentially Modified Gaussian Function for Modeling Chromatographic Peaks', \emph{Journal of Chromatographic Science} \strong{33}, pp. 568-572, DOI: \url{https://www.doi.org/10.1093/chromsci/33.10.568}.
#'
#' Li (1997), 'Development and Evaluation of Flexible Empirical Peak Functions for Processing Chromatographic Peaks', \emph{Analytical Chemistry} \strong{69}, pp. 4452-4462, DOI: \url{https://www.doi.org/10.1021/ac970481d}.
#'
#' Li (2002), 'Comparison of the Capability of Peak Function in Describing Real Chromatographic Peaks', \emph{Journal of Chromatography A} \strong{952} (1), pp. 63-70, DOI: \url{https://doi.org/10.1016/S0021-9673(02)00090-0}.
#'
#' Lan, K., Jorgenson, J.W. (2001), 'A Hybrid of Exponential and Gaussian Functions as a Simple Model of Asymmetric Chromatographic Peaks', \emph{Journal of Chromatography A} \strong{915} (1), pp. 1-13, DOI: \url{https://doi.org/10.1016/S0021-9673(01)00594-5}.
#'
#' Nikitas, P., Pappa-Louisi, A., Papageorgiou, A. (2001), 'On the Equations Describing Chromatographic Peaks and the Problem of the Deconvolution of Overlapped Peaks', \emph{Journal of Chromatography A} \strong{912} (1), pp. 13-29, DOI: \url{https://doi.org/10.1016/S0021-9673(01)00524-6}.
#'
#' Wahab, M.F., Armstrong, D.W., Hellinghausen, G. (2019), 'The Progress Made in Peak Processing', \emph{LCGC Supplements} \strong{32} (5), pp. 22-28.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Get data
#' det_res <- lcqc:::wf_detpeaks
#' chrom_icf(det_res, method = "egh", modres = TRUE)
#' chrom_icf(det_res, method = c("egh","etg"), modres = FALSE)
#' }
#'
#' @seealso This workflow uses a multitude of exported and \strong{un}exported functions:
#'\describe{
#' \item{Exported}{\code{\link{icf_plot}}, \code{\link{dtprep}}, \code{\link{fastchrom_bline}}, \code{\link{integ}}}
#' \item{Unexported}{Model-building functions (\code{\link{icf_GS}}, \code{\link{icf_EMG}}, \code{\link{icf_EGH}}, \code{\link{icf_ETG}}),
#' error calculation functions (\code{\link{gs_penalty}}, \code{\link{emg_penalty}}, \code{\link{egh_penalty}}, \code{\link{etg_penalty}}),
#' \code{\link{init_egh}}, \code{\link{peak_hw}}}
#'}
#'
#' @importFrom stats optim
chrom_icf <- function(input, method = "all", crit_w = "auto", optmet = "all", reprs = c(emg = "emg1"),
                      modres = FALSE, plotset = "make", asprat = 0.71) {

  #Preliminary checks
  input <- chkdt(input)

  #if(!is.list(input) | !all(c("type_df", "grp_df", "grp_blines", "acc_tops") %in% names(input))) {
  #  stop("Input data must be a list output from function 'dtprep'!")
  #}
  modnms <- c("all", "gs", "emg", "egh", "etg")
  if(!all(method %in% modnms)) {
    stop(paste0("Integration skim type must be any of: ", paste0("'", modnms, "'", collapse = ", "), "!"))
  }
  if(any(method %in% "all")) method <- modnms[!modnms %in% "all"]

  if(!all(names(reprs) %in% modnms)) {
    stop("Model representations 'reprs' must be a named vector with one or more suitable method names: ",
         paste0("'", modnms, "'", collapse = ", "), "!")
  }

  optmetnms <- c("all", "Nelder-Mead", "BFGS", "L-BFGS-B", "SANN")
  if(!all(optmet %in% optmetnms)) {
    stop(paste0("Optimization method 'optmet' must be any of: ", paste0("'", optmetnms, "'", collapse = ", "), "!"))
  } else if(any(optmet %in% "all")) optmet <- optmetnms[!optmetnms %in% "all"]

  #Generate function call
  cl_rec <- match.call()

  #Pre-process input data
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  acc <- input[["acc_tops"]]
  bln <- input[["grp_blines"]]
  gdt <- input[["grp_df"]]
  tdt <- input[["type_df"]]
  main <- outdf <- input[["main_df"]]
  pklst <- input[["peak_list"]]

  #Group and peak indices
  grp_inds <- acc[,"group"]
  grps <- unique(grp_inds)
  peak_inds <- acc[,"peak"]

  #Marker types
  str <- tdt[["starts"]]
  end <- tdt[["ends"]]
  lin <- tdt[["linfs"]]
  rin <- tdt[["rinfs"]]
  maxs <- tdt[["maxes"]]

  #Data.frame for integration results (integration types and peak areas)
  integ_res <- data.frame(matrix(NA, ncol = 9, nrow = nrow(acc)))
  colnames(integ_res) <- c("group", "peak", "start_rt", "apex_rt", "acc_apex_rt", "end_rt", "model_type", "model_rmse", "pa")
  integ_res[,c("group","peak", "start_rt", "apex_rt", "acc_apex_rt", "end_rt")] <- list(grp_inds, peak_inds, str[,"x"], maxs[,"x"], maxs[,"acc_x"], end[,"x"])
  if(any(colnames(acc) %in% "Compound")) integ_res <- cbind.data.frame(Compound = acc[,"Compound"], integ_res)

  #Create initial peak signal data list
  cat("\nReading detected peak list...")
  integ_list <- lapply(grps, function(x) bln[bln[,"group"]==x, c("ind","x","y")])

  #Create a data.frame for modelled peak data output
  outdf <- cbind.data.frame(outdf, as.data.frame(matrix(NA, nrow = nrow(outdf), ncol = 5+nrow(maxs))))
  colnames(outdf)[4:ncol(outdf)] <- c("corr_y", "bline", "y", "model_type", "modsum_y", paste0("peak_",maxs[,"peak"]), "group")

  #Create list to store individual peaks FITTED BY ICF
  gs_icf <- egh_icf <- emg_icf <- etg_icf <- none_icf <- list()

  #Create dataframe to store various other signals to be added to the main output data.frame

  cat("\nProcessing peak groups and baseline-resolved peaks...")
  for(i in grps) {
    #Create vector to store RMSEs
    rmses <- setNames(rep(NA,length(method)+1), c(method,"none")) #c("gs"=NA, "egh"=NA, "emg"=NA, "etg"=NA, "none"=NA)

    #Determine whether the number of peaks belonging to the current group exceeds 1
    pks <- peak_inds[grp_inds %in% i]
    grp_cond <- length(pks)>1
    mod_df <- integ_list[[i]][,c("x", "y")]

    #Retrieve spans of each peak (in indices) as well as the peak group as a whole
    pkspans <- lapply(pklst[pks], function(x) x[,"ind"])
    grpspan <- sort(unique(unlist(pkspans)))

    #Condition denoting whether or not peak groups were modeled or not (TRUE by default)
    grpmod_cond <- TRUE

    #Optionally integrate baseline-resolved peaks via the Trapezoidal Rule
    if((!grp_cond & !modres) | length(pks)>5) {

      #Update the modeling condition
      grpmod_cond <- FALSE

      if(length(pks)>5) cat("\nThere are ", length(pks), " peaks in group ", i, ". ICF is currently limited to groups of 5 peaks!", sep = "")
      if(length(pks)<=5) cat("\nNo Iterative Curve Fitting necessary for peak group ", i, "! These will be integrated as-is...", sep = "")
      none_icf[pks] <- lapply(pkspans, function(x) bln[bln[,"ind"] %in% x, "y"])
      rmses["none"] <- 0

      #Otherwise, model all peaks within group (even if baseline-resolved)
    } else {

      #Retrieve some peak features required for estimation of initial parameters by both Gaussian and EGH models
      #These include inflection points, peak retention time (center), peak height
      tLs <- tl <- lin[grp_inds==i,"acc_x"]
      tTs <- tt <- rin[grp_inds==i,"acc_x"]
      centers <- maxs[grp_inds==i, "acc_x"]
      Hs <- maxs[grp_inds==i,"acc_y"]
      sigmas <- sapply(seq_along(Hs), function(x) mean(c(abs(centers[x]-tLs[x]), abs(centers[x]-tTs[x])), na.rm = TRUE))
      names(tLs) <- names(tTs) <- names(tl) <- names(tt) <- names(centers) <- names(Hs) <- names(sigmas) <- pks

      #Additional checks for sigmas
      sig_nas <- as.numeric(names(sigmas)[which(is.na(sigmas))])
      if(length(sig_nas)>0) {
        for(j in sig_nas) {
          sigma_ind <- as.numeric(names(sigmas)) %in% j
          chk_sum <- which(pklst[[j]][,"y"] > maxs[j,"acc_y"]*0.606)
          sigmas[sigma_ind] <- sum(pklst[[j]][chk_sum,"x"], na.rm = TRUE)/2
        }
      }
      if(any(is.na(sigmas))) stop("Sigma values could not be estimated due to the absence of at least one inflection point(s)! Aborting...")

      #FITTING GAUSSIAN MODELS
      if(any(method %in% "gs")) {

        #Determine parameter limits
        optlims_l <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*0.20, centers[x]-20*sigmas[x], 0)))
        optlims_h <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*2.00, centers[x]+20*sigmas[x], 10*sigmas[x])))
        optlims <- list(optlims_l, optlims_h)
        optlims <- lapply(optlims, unname)

        #Compile starting parameters
        initpars <- lapply(seq_along(pks), function(x) {
          c(Hs[x], centers[x], sigmas[x])
        })
        initpars <- unlist(initpars)

        #Optimize the model via the built-in optimizer and retrieve the RMSE
        optlst <- list()
        errvec <- c()

        for(j in seq_along(optmet)) {
          cat("\nOptimizing curve fitting for peak group ", i, " using optimizer: ", optmet[j], " and the Gaussian model!", sep = "")
          optlst[[optmet[j]]] <- optres <- try(optim(par = initpars, fn = gs_penalty, input = mod_df,
                                                     lower = if(j=="L-BFGS-B") optlims[[1]] else -Inf,
                                                     upper = if(j=="L-BFGS-B") optlims[[2]] else Inf,
                                                     method = optmet[j], control = list(maxit = 5000)), silent = TRUE)
          errchk <- inherits(optres, "try-error")
          if(errchk) cat("\nFitting failed for peak group ", i, " with method: ", optmet[j], ". Proceeding...", sep = "")
          errvec[j] <- if(errchk) NA else optres[["value"]]
        }

        minerr <- which.min(errvec)
        rmses["gs"] <- errvec[minerr]

        optpars <- optlst[[minerr]][["par"]]

        #If necessary, pad 'optpars' with the number of NA's required to complete the ICF parameter set for 5 peaks
        reqlen <- 15
        optpars <- if(length(optpars)!=reqlen) append(optpars, rep(NA, reqlen-length(optpars)))

        #Build the optimized model
        gs_icf[pks] <- icf_GS(H = optpars[1], center = optpars[2], sigma = optpars[3],
                              H2 = optpars[4], center2 = optpars[5], sigma2 = optpars[6],
                              H3 = optpars[7], center3 = optpars[8], sigma3 = optpars[9],
                              H4 = optpars[10], center4 = optpars[11], sigma4 = optpars[12],
                              H5 = optpars[13], center5 = optpars[14], sigma5 = optpars[15],
                              t = mod_df[,"x"])
      }

      #FITTING EGH MODELS
      if(any(method %in% "egh")) {
        #Estimate parameters required by the EGH model
        #Peak Half-Widths at 0.15 of peak height and the remaining initial parameters
        fracs <- seq(0.15, 0.60, 0.15)
        initpars <- list()

        for(j in pks) {
          nm <- which(names(centers) %in% j)
          hws <- peak_hw(pklst[[j]], accmax = maxs[j,c("acc_x", "acc_y")], frac = fracs, slnt = TRUE)
          valid_hws <- which(complete.cases(hws))
          if(length(valid_hws)>0) {
            hws <- unlist(hws[min(valid_hws,na.rm = TRUE),])
            initpars[[j]] <- c(Hs[nm], centers[nm], init_egh(frac = hws["frac"], A = hws["A"], B = hws["B"]))
          } else {
            initpars[[j]] <- c(Hs[nm], centers[nm], sigmas[nm], 0)
          }
        }
        #Retrieve sigmas and taus and compile a complete initial parameter set
        sigmas_egh <- unlist(lapply(initpars, function(x) x[3]))
        taus <- unlist(lapply(initpars, function(x) x[4]))

        initpars <- unlist(initpars)
        names(initpars) <- NULL

        #Determine parameter limits
        optlims_l <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*0.20, centers[x]-20*sigmas_egh[x], 0, -Inf))) #0 #sigmas[x]*0.5
        optlims_h <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*2.00, centers[x]+20*sigmas_egh[x], 10*sigmas_egh[x], Inf))) #10*sigmas[x] #sigmas[x]*1.5
        optlims <- list(optlims_l, optlims_h)
        optlims <- lapply(optlims, unname)

        #Optimize the model via the built-in optimizer and retrieve the RMSE
        optlst <- list()
        errvec <- c()

        for(j in seq_along(optmet)) {
          cat("\nOptimizing curve fitting for peak group ", i, " using optimizer: ", optmet[j], " and the EGH model!", sep = "")
          optlst[[optmet[j]]] <- optres <- try(optim(par = initpars, fn = egh_penalty, input = mod_df,
                                                     lower = if(j=="L-BFGS-B") optlims[[1]] else -Inf,
                                                     upper = if(j=="L-BFGS-B") optlims[[2]] else Inf,
                                                     method = optmet[j], control = list(maxit = 5000)), silent = TRUE)
          errchk <- inherits(optres, "try-error")
          if(errchk) cat("\nFitting failed for peak group ", i, " with method: ", optmet[j], ". Proceeding...", sep = "")
          errvec[j] <- if(errchk) NA else optres[["value"]]
        }

        minerr <- which.min(errvec)
        rmses["egh"] <- errvec[minerr]

        optpars <- optlst[[minerr]][["par"]]

        #If necessary, pad 'optpars' with the number of NA's required to complete the ICF parameter set for 5 peaks
        reqlen <- 20
        optpars <- if(length(optpars)!=reqlen) append(optpars, rep(NA, reqlen-length(optpars)))

        #Build the optimized model
        egh_icf[pks] <- icf_EGH(H = optpars[1], center = optpars[2], sigma = optpars[3], tau = optpars[4],
                                H2 = optpars[5], center2 = optpars[6], sigma2 = optpars[7], tau2 = optpars[8],
                                H3 = optpars[9], center3 = optpars[10], sigma3 = optpars[11], tau3 = optpars[12],
                                H4 = optpars[13], center4 = optpars[14], sigma4 = optpars[15], tau4 = optpars[16],
                                H5 = optpars[17], center5 = optpars[18], sigma5 = optpars[19], tau5 = optpars[20],
                                t = mod_df[,"x"])
      }

      #FITTING EMG MODELS
      if(any(method %in% "emg")) {
        #Estimate parameters required by the EMG model
        #Calculate peak areas
        peaks_for_integ <- lapply(pkspans, function(x) bln[bln[,"ind"] %in% x, c("x","y")])
        areas <- integ(peaks_for_integ, slnt = TRUE)

        #Other parameters (besides sigma) are 'centers' and lambda, set at 1/log(0.5) - experimentally determined value
        taus <- rep(1/log(0.5), length(pks))

        #Determine parameter limits
        optlims_l <- unlist(lapply(seq_along(pks), function(x) c(areas[x]*0.5, centers[x]-20*sigmas[x], 0, log(1e-4)))) #1/log(1e-4) #log(areas[x]*0.5)
        optlims_h <- unlist(lapply(seq_along(pks), function(x) c(areas[x]*1.5, centers[x]+20*sigmas[x], 10*sigmas[x], log(20)))) #1/log(20) #log(areas[x]*1.5)
        optlims <- list(optlims_l, optlims_h)
        optlims <- lapply(optlims, unname)

        #Compile starting parameters
        initpars <- lapply(seq_along(pks), function(x) {
          c(areas[x], centers[x], sigmas[x], taus[x])
        })
        initpars <- unlist(initpars)

        #Optimize the model via the built-in optimizer and retrieve the RMSE
        optlst <- list()
        errvec <- c()

        for(j in seq_along(optmet)) {
          cat("\nOptimizing curve fitting for peak group ", i, " using optimizer: ", optmet[j], " and the EMG model!", sep = "")
          optlst[[optmet[j]]] <- optres <- try(optim(par = initpars, fn = emg_penalty, input = mod_df, repr = reprs["emg"],
                                                     lower = if(j=="L-BFGS-B") optlims[[1]] else -Inf,
                                                     upper = if(j=="L-BFGS-B") optlims[[2]] else Inf,
                                                     method = optmet[j], control = list(maxit = 5000)), silent = TRUE)
          errchk <- inherits(optres, "try-error")
          if(errchk) cat("\nFitting failed for peak group ", i, " with method: ", optmet[j], ". Proceeding...", sep = "")
          errvec[j] <- if(errchk) NA else optres[["value"]]
        }

        minerr <- which.min(errvec)
        rmses["emg"] <- errvec[minerr]

        optpars <- optlst[[minerr]][["par"]]

        #If necessary, pad 'optpars' with the number of NA's required to complete the ICF parameter set for 5 peaks
        reqlen <- 20
        optpars <- if(length(optpars)!=reqlen) append(optpars, rep(NA, reqlen-length(optpars)))

        #Build the optimized model
        emg_icf[pks] <-icf_EMG(A = optpars[1], center = optpars[2], sigma = optpars[3], tau = optpars[4],
                               A2 = optpars[5], center2 = optpars[6], sigma2 = optpars[7], tau2 = optpars[8],
                               A3 = optpars[9], center3 = optpars[10], sigma3 = optpars[11], tau3 = optpars[12],
                               A4 = optpars[13], center4 = optpars[14], sigma4 = optpars[15], tau4 = optpars[16],
                               A5 = optpars[17], center5 = optpars[18], sigma5 = optpars[19], tau5 = optpars[20],
                               t = mod_df[,"x"])
      }

      #FITTING ETG MODELS
      if(any(method %in% "etg")) {
        #Retrieve parameters required by the ETG model
        if(any(is.na(tl))) tl[is.na(tl)] <- Hs[is.na(tl)]-sigmas[is.na(tl)]
        if(any(is.na(tt))) tt[is.na(tt)] <- Hs[is.na(tt)]+sigmas[is.na(tt)]
        lamls <- lamrs <- as <- bs <- rep(1, length(pks))
        kls <- kts <- 1/sigmas #or simply 0.25

        #Determine parameter limits
        optlims_l <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*0.20, rep(-Inf,6))))
        optlims_h <- unlist(lapply(seq_along(pks), function(x) c(Hs[x]*2.00, rep(Inf,6))))
        optlims <- list(optlims_l, optlims_h)
        optlims <- lapply(optlims, unname)

        #Compile starting parameters
        initpars <- lapply(seq_along(pks), function(x) {
          c(Hs[x], kls[x], kts[x], lamls[x], lamrs[x], as[x], bs[x])
        })
        initpars <- unlist(initpars)

        #Compile inflection points
        initinfs <- unlist(lapply(seq_along(pks), function(x) c(tl[x], tt[x])))

        #Optimize the model via the built-in optimizer and retrieve the RMSE
        optlst <- list()
        errvec <- c()

        for(j in seq_along(optmet)) {
          cat("\nOptimizing curve fitting for peak group ", i, " using optimizer: ", optmet[j], " and the ETG model!", sep = "")
          optlst[[optmet[j]]] <- optres <- try(optim(par = initpars, fn = etg_penalty, input = mod_df, infs = initinfs,
                                                     lower = if(j=="L-BFGS-B") optlims[[1]] else -Inf,
                                                     upper = if(j=="L-BFGS-B") optlims[[2]] else Inf,
                                                     method = optmet[j], control = list(maxit = 5000)), silent = TRUE)
          errchk <- inherits(optres, "try-error")
          if(errchk) cat("\nFitting failed for peak group ", i, " with method: ", optmet[j], ". Proceeding...", sep = "")
          errvec[j] <- if(errchk) NA else optres[["value"]]
        }

        minerr <- which.min(errvec)
        rmses["etg"] <- errvec[minerr]

        optpars <- optlst[[minerr]][["par"]]

        #If necessary, pad 'optpars' with the number of NA's required to complete the ICF parameter set for 5 peaks
        reqlen <- 35
        optpars <- if(length(optpars)!=reqlen) append(optpars, rep(NA, reqlen-length(optpars)))

        #Also pad inflection point times if needed
        reqinf <- 10/2
        tl <- if(length(tl)!=reqinf) append(tl, rep(NA, reqinf-length(tl)))
        tt <- if(length(tt)!=reqinf) append(tt, rep(NA, reqinf-length(tt)))

        #Build the optimized model
        etg_icf[pks] <- icf_ETG(H = optpars[1], kl = optpars[2], kt = optpars[3], laml = optpars[4], lamr = optpars[5], a = optpars[6], b = optpars[7],
                                H2 = optpars[8], kl2 = optpars[9], kt2 = optpars[10], laml2 = optpars[11], lamr2 = optpars[12], a2 = optpars[13], b2 = optpars[14],
                                H3 = optpars[15], kl3 = optpars[16], kt3 = optpars[17], laml3 = optpars[18], lamr3 = optpars[19], a3 = optpars[20], b3 = optpars[21],
                                H4 = optpars[22], kl4 = optpars[23], kt4 = optpars[24], laml4 = optpars[25], lamr4 = optpars[26], a4 = optpars[27], b4 = optpars[28],
                                H5 = optpars[29], kl5 = optpars[30], kt5 = optpars[31], laml5 = optpars[32], lamr5 = optpars[33], a5 = optpars[34], b5 = optpars[35],
                                tl = tl[1], tt = tt[1], tl2 = tl[2], tt2 = tt[2], tl3 = tl[3], tt3 = tt[3],
                                tl4 = tl[4], tt4 = tt[4], tl5 = tl[5], tt5 = tt[5],
                                t = mod_df[,"x"])
      }
    }

    #Optionally choose model for each peak based on goodness-of-fit
    whichmod <- names(rmses)[which.min(rmses)]
    fin_icf <- if(whichmod=="gs") gs_icf[pks] else if(whichmod=="egh") egh_icf[pks] else if(whichmod=="emg") emg_icf[pks] else if(whichmod=="etg") etg_icf[pks] else if(whichmod=="none") none_icf[pks]
    names(fin_icf) <- pks

    #Add baseline-corrected signal and baseline to main output data.frame
    outdf[outdf[,"ind"] %in% grpspan, c("bline", "y")] <- bln[bln[,"ind"] %in% grpspan, c("bline","y")]

    #Sum modeled signal and add to the output data.frame alongside ICF model type
    if(!grpmod_cond) {
      outdf[outdf[,"ind"] %in% grpspan, "modsum_y"] <- unlist(lapply(seq_along(fin_icf), function(x) {
        maxind <- length(fin_icf[[x]])
        res <- if(x==length(fin_icf)) fin_icf[[x]] else fin_icf[[x]][-maxind]
        return(res)
      }))
    } else {
      outdf[outdf[,"ind"] %in% grpspan, "modsum_y"] <- Reduce(`+`, fin_icf)
    }

    outdf[outdf[,"ind"] %in% grpspan, "model_type"] <- whichmod
    outdf[outdf[,"ind"] %in% grpspan, "group"] <- i

    #Add individual modeled peaks to the output data.frame
    if(!grpmod_cond) {
      for(j in pks) {
        curpeak <- pkspans[[paste0("peak_", j)]]
        outdf[outdf[,"ind"] %in% curpeak, paste0("peak_", j)] <- fin_icf[names(fin_icf) %in% j]
        rtspan <- outdf[outdf[,"ind"] %in% curpeak, "x"]
        fin_icf[[as.character(j)]] <- cbind.data.frame("x" = rtspan, "y" = fin_icf[[as.character(j)]])
      }
    } else {
      for(j in pks) {
        outdf[outdf[,"ind"] %in% grpspan, paste0("peak_", j)] <- fin_icf[names(fin_icf) %in% j]
      }
      rtspan <- outdf[outdf[,"ind"] %in% grpspan, "x"]
      fin_icf <- lapply(fin_icf, function(x) cbind.data.frame("x" = rtspan, "y" = x))
    }

    #Compile and report integration results
    integ_res[pks,"model_type"] <- whichmod
    integ_res[pks, c("model_rmse", "pa")] <- c(rep(if(whichmod=="none") NA else rmses[whichmod], length(pks)), integ(fin_icf))
  }

  #Compile information about function
  mod_nms <- c(gs = "Gaussian", emg = "Exponentially-Modified Gaussian (EMG)", egh = "Exponential-Gaussian Hybrid", etg = "Empirically-Transformed Gaussian")
  skim_nms <- c("Nelder-Mead" = "Nelder-Mead", "BFGS" = "quasi-Newton (BFGS)", #"CG" = "Conjugate Gradient (CG)",
                "L-BFGS-B" = "quasi-Newton with box constraints (L-BFGS-B)", "SANN" = "Simulated Annealing (SANN)")
  fused_pknum <- length(which(grp_inds %in% grp_inds[duplicated(grp_inds)]))
  resolved_pknum <- length(peak_inds)-fused_pknum
  information <- paste0("Baselines for ", resolved_pknum," resolved and ", fused_pknum, " fused peaks spread across ", length(which(!duplicated(grp_inds))), " groups were determined via the FastChrom algorithm.",
                        "\nThe following Non-Linear Least-Squares Curve Fitting model(s) was/were applied: ", paste0(mod_nms[method], " ('", method, "')", collapse = ", "),".",
                        "\nBaseline-resolved peak modeling was ", ifelse(modres, "", "NOT "), "carried out.",
                        "\nA critical width equal to ", ifelse(is.numeric(crit_w), crit_w, " the smallest inflection point distance"), " was used for baseline adjustment in the peak regions.",
                        "\nPeaks were integrated using the Trapezoidal Rule.")

  #Combine results
  finres <- list(main_data = outdf, integ_res = integ_res, information = information, call = cl_rec)

  #Plot results
  if(plotset!="none") {
    finres[["modplot"]] <- icf_plot(outdf, plotind = TRUE, asprat = asprat)
    if(plotset=="print") print(finres[["modplot"]])
  }

  #Compile and return final results
  return(finres)
}
