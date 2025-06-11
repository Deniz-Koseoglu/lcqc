#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculation of Asymmetry Factor (As), Tailing Factor (USP), and Total Peak Analysis (TPA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate asymmetry metrics for chromatographic peaks
#'
#' @description Calculates common asymmetry metrics for chromatographic data including the United States Pharmacopoeia (USP)/European
#' Pharmacopoeia (EP) Tailing Factor (\eqn{T_f}), and the Asymmetry Factor (\eqn{A_s}). Additionally, the Total Peak Analysis (TPA) workflow
#' proposed by Wahab et al. (2017) is also implemented for baseline-resolved peaks. See \strong{Details} for further information.
#'
#' @param input The output data from function \code{\link{chrom_detect}}.
#' @param method A \code{character} vector of method(s) to apply for asymmetry calculations. One or more of: \code{"all"} (default),
#' \code{"Tf"} (USP \eqn{T_f}), \code{"As"} (\eqn{A_s}), and/or \code{"TPA"} (Total Peak Analysis).
#' @param which_peaks Selects specific peaks from \code{input} to process. Either \code{"all"} (default) or a \code{numeric} vector
#' of peak indices included in \code{input}.
#' @param show_widths A \code{logical} specifying whether the various peak half-widths at specific heights required for the
#' calculations are included in the results (\code{TRUE} by default).
#' @param crit_w The critical width parameter to use for baseline calculation via FastChrom (\code{\link{fastchrom_bline}}).
#' Defaults to \code{"auto"} or can be set manually as a \code{numeric} (usually equal to the minimum peak width at half height).
#' @param asprat Aspect ratio of the plot(s). Defaults to \code{0.71}.
#' @param tpa_thres The \code{numeric} threshold between 0 and 0.99 to use for calculation of peak width and its relation to
#' Gaussian standard deviation for TPA. Defaults to \code{0.85}. See \strong{Details}.
#' @param optmet A \code{character} string specifying which method to use for iterative adjustment of the Gaussian model.
#' One of \code{"optim"} or \code{"nlp"} (default).
#' @param plotset A \code{character} string specifying whether results of TPA are visualized/shown via \code{\link{chrom_tpa}}.
#' One of \code{"make"} (generates plots without printing; default), \code{"print"} (generates and prints plots), or \code{"none"}.
#'
#' @return A named \code{list} of length 4 containing the \code{data.frame} of \code{results}, the function \code{call},
#' the list of TPA \code{plots} (if any), and a \code{character} string providing various \code{information} about the results.
#'
#' @export
#'
#' @details
#' Peak asymmetry may be described in different ways, with most of the classical values providing a single numeric metric.
#' All of the methods described below have been implemented in \pkg{lcqc}.
#' Among the widespread metrics is the Asymmetry Factor \eqn{A_s}, calculated as the ratio between peak half-widths at 10% peak height on
#' the \strong{trailing} (\eqn{B_{10}}) and \strong{leading} (\eqn{A_{10}}) edges of the peak. Thus, values greater than 1 indicate tailing,
#' values less than one indicate fronting, and a tailing factor of exactly 1 is characteristic of a perfectly symmetrical peak.
#' \deqn{A_s = B_{10}/A_{10}}
#' Another metric commonly calculated is the USP/EP Tailing Factor \eqn{T_f}, which uses half-widths at 5% peak height.
#' \deqn{T_f = \frac{(A_5+B_5)}{2A_5}}
#' Irrespective of whether \eqn{T_f} or \eqn{A_s} is used, acceptance criteria outlined in various pharmacopoeias list a range of \strong{0.8-1.8}
#' as acceptable. In practice, values of \strong{0.9-1.2} are routinely achieved and desirable for typical test analytes.
#' Both \eqn{T_f} and \eqn{A_s} provide information only about the \strong{relative} amount of tailing or fronting, but fail to accommodate cases
#' where both are present. For example, a widened peak with both tailing and fronting (a so-called \emph{Eiffel Tower} effect) may have \eqn{T_f}
#' and \eqn{A_s} values close to 1 despite being heavily distorted from a Gaussian profile. In order to provide separate measures of the absolute
#' and relative contributions of tailing and fronting to the total peak shape, as well as to effectively visualize the results, \pkg{lcqc}
#' also implements the Total Peak Analysis (TPA) workflow developed by Wahab et al. (2017) as a simple visual and quantitative assessment tool.
#' First, the Gaussian standard deviation \eqn{\sigma} is estimated from a peak (with a maximum signal \strong{normalized to unity}) using
#' the peak width at a chosen percentage of peak height (\eqn{W_H}):
#' \deqn{\sigma = W_H/(2\sqrt{2ln(1/H)})}
#' The estimation of \eqn{\sigma} takes advantage of the fact that the tops of chromatographic peaks (after 80-85% peak height) usually follow a
#' Gaussian distribution closely even for heavily-distorted peaks. Thus, a Gaussian model is constructed using the estimated \eqn{\sigma},
#' a peak maximum equal to 1, and the actual peak retention time. A linear constrained solver is then used to ensure the top 15% of the
#' Gaussian model values are either equal to or enclosed by the actual chromatographic peak. Finally, absolute and relative (%) residuals are
#' calculated separately for the leading and trailing edges of the peak, providing separate quantitative measures of the degree of peak fronting
#' and tailing. Currently, the TPA procedure is limited by \pkg{lcqc} to only assess baseline-resolved peaks. Each peak is also assessed for
#' its suitability for TPA based on residual sums to the left and right of the peak apex. The Gaussian model is supposed to be completely enclosed
#' by the original chromatographic peak, but this is seldom the case, resulting in negative residuals where the model is outside the retention time
#' boundaries of the peak. If \strong{>50%} of residuals are found to be negative on either side of the peak, it is considered to be unsuitable
#' for TPA.
#'
#' @references
#' Meyer, V.R. (2010), \emph{Practical High-Performance Liquid Chromatography}, John Wiley & Sons, Chichester, United Kingdom.
#'
#' The United States Pharmacopeial Convention (2021), \emph{Physical Tests/<621> Chromatography} (USP 40-NF 35), available at:
#' \url{https://www.usp.org/harmonization-standards/pdg/excipients/chromatography} (accessed 24.04.2024).
#'
#' Wahab, M.F., Patel, D.C., Armstrong, D.W. (2017), 'Total Peak Shape Analysis: Detection and Quantitation of Concurrent Fronting, Tailing, and Their Effect on Asymmetry Measurements', \emph{Journal of Chromatogpraphy A} \strong{1509}, pp. 163-170, DOI: \url{https://doi.org/10.1016/j.chroma.2017.06.031}.
#'
#' @examples
#' \dontrun{
#' #Get data
#' dt <- lcqc:::wf_detpeaks
#' res <- chrom_asym(dt, which_peaks = 3:7, show_widths = FALSE)
#' }
#'
#' @seealso \code{\link{chrom_tpa}}, \code{\link{chrom_detect}}
chrom_asym <- function(input, method = "all", which_peaks = "all", show_widths = TRUE, crit_w = "auto", asprat = 0.71, tpa_thres = 0.85,
                       optmet = "nlp", plotset = "make") {
  #Perform checks
  input <- chkdt(input)

  if(!any(c("all", "Tf", "As", "TPA") %in% method)) stop("Method must be one or more of: 'all', 'Tf', 'As', and/or 'TPA'!")
  if(any(method %in% "all")) method <- c("Tf", "As", "TPA")
  if(any(which_peaks %in% "all")) which_peaks <- seq(nrow(input[["acc_tops"]]))
  if(is.numeric(which_peaks)) {
    if(max(which_peaks)>nrow(input[["Peak_Extents"]])) stop("At least one of the chosen peak indices exceeds the number of peaks in the input data!")
  }

  #Generate function call
  cl_rec <- match.call()

  #Retrieve data for calculation of Theoretical Plate Number
  #Pre-process input data
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  peaknum <- nrow(input[["acc_tops"]])
  blinenum <- length(which(input[["type_df"]][["maxes"]][,"ptype"]=="B"))

  acc <- input[["acc_tops"]][which_peaks,]
  ptypes <- input[["type_df"]][["maxes"]][which_peaks,"ptype"] #Peak types
  pklst <- input[["peak_list"]][which_peaks]

  #Output list
  output <- plotlist <- list()
  #outdf <- as.data.frame(matrix(NA, nrow = length(ptypes), ncol = 2))
  #outdf[,c("group","peak")] <- acc[,c("group","peak")]

  for(i in seq_along(pklst)) {

    #Optionally retrieve peak name (where present)
    tpa_suffix <- if(any(colnames(acc) %in% "Compound")) paste0(i, " (", acc[i,"Compound"], ")") else acc[i,"peak"]
    trmax <- acc[i,"rt_max"]
    sigmax <- acc[i,"sig_max"]
    fracs <- if(any(method %in% "TPA")) c(0.05, 0.10, tpa_thres) else c(0.05, 0.10)

    #Calculate peak half-widths
    hws <- peak_hw(pklst[[i]], accmax = c(trmax, sigmax), frac = fracs, resolved = if(!is.na(ptypes[i]) & ptypes[i]=="B") TRUE else FALSE, slnt = TRUE)

    #Retrieve peak widths and half-widths
    wvec <- c(hws[1,"A"], hws[1,"B"], hws[2,"A"], hws[2,"B"], rep(NA, 2))
    suf <- if(nchar(tpa_thres*100)==1) "0" else ""
    if(any(method %in% "TPA") & !is.na(ptypes[i]) & ptypes[i]=="B") {
      wvec[c(5,6)] <- c(hws[3, "A"], hws[3, "B"])
    }
    wnms <- c("A05", "B05", "A10", "B10", paste0(c("A","B"), suf, tpa_thres*100))
    names(wvec) <- wnms

    #Output vector for peak i
    asyms <- c()
    #Add calculated widths if show_widths == TRUE
    if(show_widths) asyms[wnms] <- wvec

    for(j in method) {
      #Calculate asymmetry factor and/or tailing factor values
      if(j=="As") {
        asyms[j] <- wvec["B10"]/wvec["A10"]
      } else if(j=="Tf") {
        asyms[j] <- (wvec["A05"]+wvec["B05"])/(2*wvec["A05"])
      } else if(j=="TPA") {
        if(!is.na(hws[3,"W"]) & !is.na(ptypes[i]) & ptypes[i]=="B") {
          tpa_out <- chrom_tpa(input = pklst[[i]], width = hws[3,"W"], accmax = c(trmax, sigmax), thres = tpa_thres, asprat = asprat,
                               pknum = tpa_suffix, opt = optmet, print_plot = FALSE, plot_info = FALSE)
          tpa_res <- tpa_out[["results"]]
          plotlist[[paste0("Peak_",acc[i,"peak"])]] <- tpa_out[["Plot"]]
        }  else tpa_res <- NA

        asyms[c(paste0(if(nchar(tpa_thres*100)==1) "W0" else "W", tpa_thres*100), "sigma", "resid_sum",
                "resid_front", "resid_back", "percent_fronting", "percent_tailing", "tpa_suitability")] <- tpa_res
      }
    }
    output[[paste0("peak_",i)]] <- unlist(c(acc[i,c("group","peak")], asyms))
  }

  #output data.frame
  output <- do.call(rbind.data.frame, lapply(output, function(x) data.frame(as.list(x))))
  rownames(output) <- NULL

  #Add compound names (optional)
  if(any(colnames(acc) %in% "Compound")) output <- cbind.data.frame(Compound = acc[,"Compound"], output)

  #Compile plots
  fout <- list(results = output, call = cl_rec)
  if(any(method %in% "TPA") & plotset!="none") {
    fout[["plots"]] <- plotlist
    if(plotset=="print") print(plotlist)
  }

  #Compile information about function
  metnms <- c(Tf = "USP Tailing Factor", As = "Asymmetry Factor", TPA = "Total Peak Analysis")
  fout[["information"]] <- paste0("Asymmetry metric calculation was attempted for ", ifelse(is.numeric(which_peaks), paste0(length(which_peaks), " out of "), "all of "), peaknum, " peaks.",
                                  "\nThe following methods were used: ", paste0(metnms[names(metnms) %in% method], " ('", method, "')", collapse = ", "),".",
                                  ifelse(any(method=="TPA"), paste0("\nTotal Peak Analysis (TPA) was carried out on ", length(plotlist), " baseline-resolved peaks out of ", peaknum, " total peaks (or ",
<<<<<<< HEAD
                                                                    blinenum, " baseline-resolved peaks)."),""))
=======
                                                                    blinenum, " baseline-resolved peaks."),""))
>>>>>>> 17ad0a858074f62c312b79f29c2f1c67a6440373
  return(fout)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Total Peak Analysis (TPA) of baseline-resolved peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Carry out Total Peak Analysis (TPA) for a selected chromatographic peak
#'
#' @description
#' Calculates the Total Peak Analysis (TPA) metrics as described by Wahab et al. (2017) and optionally visualizes the results.
#' See \code{\link{chrom_asym}} for further information.
#'
#' @param input A \code{data.frame} containing retention time and signal data of the peak for which TPA is to be carried out.
#' @param vars A \code{character} vector of length 2 containing the column names of retention time and signal, in that order.
#' @param width Either \code{"auto"} (default) or a single \code{numeric} value of the peak width (in retention time units).
#' @param accmax Either \code{"auto"} (default) or a \code{numeric} vector of length 2 containing the retention time and signal
#' (in that order) of the peak for which TPA is to be carried out.
#' @param thres The \code{numeric} threshold between 0 and 0.99 to use for calculation of peak width and its relation to
#' Gaussian standard deviation for TPA. Defaults to \code{0.85}.
#' @param pknum A \code{character} string to append to the plot title. Usually contains information like the peak number and/or compound name.
#' @param opt A \code{character} string specifying which method to use for iterative adjustment of the Gaussian model.
#' One of \code{"optim"} or \code{"nlp"} (default).
#' @param asprat Aspect ratio of the plot (defaults to \code{0.71}).
#' @param print_plot A \code{logical} switch. Should the generated plot be printed? \code{TRUE} by default.
#' @param plot_info A \code{logical} switch. Should extra information about the suitability of the peak for TPA be included in the plot?
#' \code{FALSE} by default.
#'
#' @return A named \code{list} containing the following elements:
#' \describe{
#' \item{results}{A named \code{numeric} vector of key results including the peak width at \code{thres}, the Gaussian standard
#' deviation (\code{"sigma"}), the \strong{absolute} residual sum (\code{"resid_sum"}), its front- and back-of-peak components
#' (\code{"resid_front"} and \code{"resid_back"}), corresponding percentage contributions to fronting and tailing
#' (\code{"percent_fronting"} and \code{"percent_tailing"}), and a binary measure of the suitability of the peak
#' for TPA (\code{"tpa_suitability"}) determined from the residuals. See \code{\link{chrom_asym}}.}
#' \item{Peak_Data}{A \code{data.frame} containing the retention time (\code{"rt"}), normalized peak signal (\code{"normpeak"}),
#' its Gaussian model (\code{"gausspeak"}), residuals between the normalized peak and the Gaussian model (\code{"resids"}),
#' and their absolute counterparts (\code{"aresids"}).}
#' \item{Plot}{A \code{ggplot} object containing TPA visualization.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Get data
#' dt <- lcqc:::wf_detpeaks[["results"]][["Peaks"]][["peak_7"]]
#' chrom_tpa(dt, vars = c("Time","Signal"), pknum = "7 (Placeholder)", plot_info = TRUE)
#' }
#'
#' @seealso \code{\link{chrom_asym}}, \code{\link{peak_hw}}
#'
#' @importFrom stats optim
chrom_tpa <- function(input, vars = c("x","y"), width = "auto", accmax = "auto", thres = 0.85, pknum = "", opt = "nlp",
                      asprat = 0.71, print_plot = TRUE, plot_info = FALSE) {

  #Perform checks
  if(!any(c("optim","nlp") %in% opt)) stop("The iterative optimization method 'opt' must be one of: 'optim' or 'nlp'!")
  if(any(thres<0) | any(thres>0.99)) stop("TPA height threshold 'thres' must be between 0 and 0.99!")
  if(!all(vars %in% colnames(input))) stop("One or more variable names were not found in the 'input' data.frame!")

  #Define variables
  rt <- input[,vars[1]]
  sig <- input[,vars[2]]
  rt_l <- min(rt, na.rm = TRUE)
  rt_h <- max(rt, na.rm = TRUE)
  sig_l <- sig[1]
  sig_h <- sig[length(sig)]
  maxsig <- which.max(sig)

  #Derive maximum if 'accmax' is not given
  if(any(accmax %in% "auto")) {
    accmax <- c(rt[maxsig], sig[maxsig])
  }
  center <- accmax[1]
  sigmax <- accmax[2]

  #Determine peak width if not provided
  if(width=="auto") width <- peak_hw(input = input, vars = vars, frac = thres, slnt = TRUE)[,"W"]

  #Normalize baseline-adjusted peak signal (for direct comparison of residuals between peaks)
  norm_peak <- sig/sigmax
  norm_peak[norm_peak==0] <- NA

  #Model Gaussian peak from normalized peak
  #Calculate sigma
  sigma <- width/(2*sqrt(2*log(1/thres, base = exp(1)))) #Wahab et al. (2017) EQUATION 4

  #Calculate normalized peak maximum (should always equal 1)
  papx <- max(norm_peak, na.rm = TRUE) #max(norm_peak, na.rm = TRUE) * exp(-(0.5*((rt-center)/sigma)^2))

  #Refine peak iteratively until the upper 85% of the peak lie within the actual chromatographic peak
  #Compile data
  opt_dt <- cbind.data.frame(x = rt, y = norm_peak)

  #Get half-widths at 60% peak height
  hws <- peak_hw(input = input, vars = vars, frac = 0.60, slnt = TRUE)
  llim <- center - hws[,"A"]
  rlim <- center + hws[,"B"]

  #Define error minimization (objective) function (sum of squared differences between peaks)
  gsimerr <- function(x){
    #Fit Gaussian
    gcurve <- fit_1G(apex = papx, loc = center, sigma = x, data = opt_dt[,"x"])
    #Calculate sum of squared differences in retention times for the top 15% of the peak
    ptop <- which(opt_dt[,"y"] >= max(opt_dt[,"y"], na.rm = TRUE)*0.80 & opt_dt[,"y"] <= max(opt_dt[,"y"], na.rm = TRUE)*0.90)
    err <- sum((gcurve[ptop]-opt_dt[ptop,"y"])^2, na.rm = TRUE)
    return(err)
  }
  if(opt=="nlp") {
    #Define constraint function
    gconst <- function(x) {
      constr <- c(llim - center + x,
                  center + x - rlim)
      return(constr)
    }

    #Define initial sigma value and algorithm options
    x0 <- sigma
    local_opts <- list("algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-10)
    opts <- list("algorithm" = "NLOPT_GN_ISRES", #"NLOPT_LN_COBYLA"
                 "xtol_rel" = 1.0e-10,
                 "local_opts" = local_opts,
                 "maxeval" = 50000) #"local_opts" = local_opts,

    #Run optimizer
    optres <- nloptr::nloptr(x0 = x0,
                             eval_f = gsimerr,
                             lb = 0,
                             ub = 5,
                             eval_g_ineq = gconst,
                             opts = opts)
    fsigma <- optres[["solution"]]
  } else if(opt=="optim") {
    fsigma <- optim(par = sigma, fn = gsimerr, method = "L-BFGS-B", lower = 0, upper = Inf)[["par"]]
  }

  #Build final Gaussian peak with iteratively optimized sigma
  gauss_peak <- fit_1G(apex = papx, loc = center, sigma = fsigma, data = rt)

  #Calculate residuals and absolute residuals between normalized and Gaussian model peaks
  resids <- norm_peak - gauss_peak
  aresids <- abs(resids)
  aresid_sum <- sum(aresids, na.rm = TRUE)

  #Split total residual sum into that corresponding to peak half-widths (front and back)
  front_aresid <- sum(aresids[1:maxsig], na.rm = TRUE)
  back_aresid <- sum(aresids[maxsig:length(sig)], na.rm = TRUE)

  #Convert residual half-widths into percentages
  front_perc <- front_aresid/aresid_sum*100
  tail_perc <- back_aresid/aresid_sum*100

  #Calculate residual sums (not absolute residual sums) to DETERMINE SUITABILITY OF PEAK FOR TPA
  front_resid <- sum(resids[1:maxsig], na.rm = TRUE)
  back_resid <- sum(resids[maxsig:length(resids)], na.rm = TRUE)


  #Fraction of negative residuals on the front and back half of peak
  front_negs <- length(which(resids[1:maxsig]<0))/length(1:maxsig)
  back_negs <- length(which(resids[maxsig:length(resids)]<0))/length(maxsig:length(resids))
  front_versuf <- if(front_resid<0 & front_negs>0.50) "Unsuitable" else if(front_resid>0 & front_negs > 0.50) "Likely unsuitable" else "Suitable"
  back_versuf <- if(back_resid<0 & back_negs>0.50) "Unsuitable" else if(back_resid>0 & back_negs > 0.50) "Likely unsuitable " else "Suitable"

  if(plot_info) {
    front_verdict <- paste0("Front of peak residual sum: ", round(front_resid, 2), " (", round(front_negs*100, 0), "% of values -ve). ", front_versuf, " for TPA.")
    back_verdict <- paste0("Back of peak residual sum: ", round(back_resid, 2), " (", round(back_negs*100, 0), "% of values -ve). ", back_versuf, " for TPA.")
  } else front_verdict <- back_verdict <- ""

  #Create output vector
  outvec <- c(width, fsigma, aresid_sum, front_aresid, back_aresid, front_perc, tail_perc, if(any(c(front_versuf, back_versuf) %in% "Unsuitable")) 0 else 1)
  names(outvec) <- c(paste0(if(nchar(thres*100)==1) "W0" else "W", thres*100), "sigma", "resid_sum", "resid_front", "resid_back", "percent_fronting", "percent_tailing", "tpa_suitability")

  #Create data.frame for TPA suitability text annotations
  annot <- data.frame(xpos = rep(Inf, 2), ypos = rep(Inf, 2),
                      annotation = c(front_verdict, back_verdict),
                      hjustvar = rep(1, 2),
                      vjustvar = c(1.2, 2.9))

  #(OPTIONALLY) Plot TPA of each peak with: Normalized Peak, Gaussian Peak, Absolute Residuals, and Sum of Residuals as well as Percent Fronting and Tailing
  resdf <- data.frame(rt = rt, normpeak = norm_peak, gausspeak = gauss_peak, resids = resids, aresids = aresids)
  plotdf <- cbind.data.frame(resdf,
                             one = rep("one", length(rt)), two = rep("two", length(rt)), three = rep("three", length(rt)))

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_xvar <- "rt"
  aes_yvar <- "gausspeak"
  aes_yvar2 <- "normpeak"
  aes_yvar3 <- "aresids"
  aes_colvar <- "one"
  aes_colvar2 <- "two"
  aes_colvar3 <- "three"
  aes_xpos <- "xpos"
  aes_ypos <- "ypos"
  aes_labvar <- "annotation"
  aes_hjust <- "hjustvar"
  aes_vjust <- "vjustvar"

  tpa_plot <- ggplot(data = plotdf, aes(x = .data[[aes_xvar]])) +
    geom_path(aes(y= .data[[aes_yvar]], colour = .data[[aes_colvar]]), lty=2, lwd=1.3, na.rm = TRUE) +
    geom_path(aes(y= .data[[aes_yvar2]], colour = .data[[aes_colvar2]]), lty=1, lwd=1.3, na.rm = TRUE) +
    geom_path(aes(y= .data[[aes_yvar3]], colour= .data[[aes_colvar3]]), lty=3, lwd=1.3, na.rm = TRUE) +
    {if(plot_info) geom_text(data = annot, aes(x = .data[[aes_xpos]], y = .data[[aes_ypos]],
                                label = .data[[aes_labvar]], hjust = .data[[aes_hjust]],
                                vjust = .data[[aes_vjust]]), size = 3.4) } +
    coord_cartesian(ylim=c(0, 1.15)) +
    scale_colour_manual(name = NULL,
                        values = c("one" = "green",
                                   "two" = "darkblue",
                                   "three" = "red"),
                        labels = c("one" = "Gaussian Model",
                                   "two" = "Baseline-Adjusted Peak",
                                   "three" = "Absolute Residuals")) +
    labs(x = "Time (min)", y = "Signal", title = paste0("Peak ", pknum), subtitle = paste0("Absolute Residual Sum: ", round(aresid_sum, 2),
                                                                                                   "; Fronting: ", round(front_perc, 0)," %",
                                                                                                   "; Tailing: ", round(tail_perc, 0), " %")) +
    theme(aspect.ratio = asprat,
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"),
          legend.position = "bottom",
          legend.text = element_text(size = 11, colour = "black")
    )

  if(print_plot) print(tpa_plot)
  return(list(results = outvec, Peak_Data = resdf, Plot = tpa_plot))
}
