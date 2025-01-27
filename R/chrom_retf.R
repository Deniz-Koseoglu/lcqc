#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculation of Retention Factors
#Can be relative to one or multiple reference peaks
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculation of Retention Factors for HPLC analytes
#'
#' @description Calculates retention factors for chromatographic peaks based on column dead time.
#' See \strong{Details} for further information.
#'
#' @param input The output \code{list} from \code{\link{chrom_detect}}.
#' @param t0_mode The mode to use for calculation of dead time. One of: \code{"peak"} or \code{"manual"}, where \emph{t0} is taken
#' directly as the time of specified peak maximum or given manually in minutes, respectively.
#' @param t0 Dead time of the column, i.e. breakthrough time. Either given in \strong{minutes} if \code{t0_mode} is \code{"manual"},
#' or given as a single peak index included in \code{which_peaks}.
#' @param peaks Either \code{"all"} (default) or a \code{numeric} vector of peak indices to calculate retention factors for.
#' @param crit_w The critical width parameter to use for baseline calculation via FastChrom (\code{\link{fastchrom_bline}}).
#' Defaults to \code{"auto"} or can be set manually as a \code{numeric} (usually equal to the minimum peak width at half height).
#'
#' @return A named \code{list} of length 4 containing a \code{data.frame} of \code{results}, which includes the peak \code{id},
#' peak \code{type} (see \code{\link{chrom_detect}} for descriptions of available peak types), retention time \code{rt}, the
#' dead time \code{t0}, and the calculated retention factors \code{k}. \strong{If} the input data (from \code{\link{chrom_detect}})
#' also includes the \code{Compound} names for each peak, these are included as the first column.
#' Also included is the dead time \code{t0}, a \code{character} string of various \code{information} about the results,
#' and the function \code{call} as separate list elements.
#'
#' @details
#' The retention factor \eqn{k} is used to gauge the relative retention of an analyte compared to that of an unretained solute (such as uracil
#' for reversed-phase separations). It is calculated from dead time \eqn{t_0}, a.k.a. breakthrough time, using the following simple equation:
#' \eqn{k = (t_R-t_0)/t_0}
#' Typical RP separations include compounds with \eqn{k} ranging between 1 and 10 (up to 20 for difficult separations). In addition to providing
#' a retention time manually, it is possible to set the dead time as the retention time of one of the peaks in \pkg{input} when \code{t0_mode} is
#' set to \code{"peak"}.
#'
#' @references
#' Meyer, V.R. (2010), \emph{Practical High-Performance Liquid Chromatography}, John Wiley & Sons, Chichester, United Kingdom.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Get data and run
#' dt <- lcqc:::wf_detpeaks
#' res <- chrom_retf(input = dt, t0_mode = "peak", t0 = 1)
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{fastchrom_bline}}
chrom_retf <- function(input, t0_mode = "peak", t0, peaks = "all", crit_w = "auto") {

  #Perform checks
  input <- chkdt(input)

  #Generate function call
  cl_rec <- match.call()

  #Process data
  #Retrieve data for calculation of Theoretical Plate Number
  #Pre-process input data
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  ptypes <- input[["type_df"]][["maxes"]][,"ptype"] #Peak types
  acc <- input[["acc_tops"]]
  pids <- acc[,"peak"]
  rts <- acc[,"rt_max"]

  #Additional checks
  if(!all(t0_mode %in% c("peak", "manual"))) stop("Argument 't0_mode' must be one of 'peak' or 'manual'!")
  if(t0_mode=="manual" & t0<0) stop("When 't0_mode' is set to 'manual', dead time t0 cannot be less than zero!")
  if(length(pids)<2 & t0_mode=="peak") stop("Dead time t0 must be provided explicitly when only one peak is present!")
  if(t0%%1!=0 & t0_mode=="peak") stop("When 't0_mode' is set to 'peak', 't0' must be an integer!")
  else if(t0_mode=="peak" & (t0 > nrow(acc))) stop("The peak ID identifying t0 is outside of the input data range!")
  if(any(peaks %in% "all")) {
    peaks <- if(any(pids %in% t0) & t0_mode=="peak") pids[-which(pids %in% t0)] else pids[rts>t0]
  }
  if(!all(peaks %in% pids)) stop("At least one peak id ('peaks') was not found in the input data!")
  if(t0_mode=="peak" & any(peaks %in% t0)) {
    cat("\nt0 peak included in the list of 'peaks' for calculation! Removing...")
    peaks <- peaks[-which(peaks %in% t0)]
  }
  t_bckp <- t0
  if(t0_mode == "peak") t0 <- rts[t0]
  if(any(rts[peaks] < t0)) {
    early_chk <- peaks[rts[peaks]<t0]
    warning(paste0("The following peaks selected for retention factor calculation elute before t0: ",
                   paste0(early_chk, collapse = ", "), "! These were removed."))
    peaks <- peaks[!peaks %in% early_chk]
  }

  #Calculate retention factors
  reslst <- list(pids[peaks], ptypes[peaks], rts[peaks], rep(t0,length(peaks)), (rts[peaks]-t0)/t0)
  res <- Reduce(cbind.data.frame, reslst)
  colnames(res) <- c("id", "type", "rt", "t0", "k")

  #Add compound names (optional)
  if(any(colnames(acc) %in% "Compound")) res <- cbind.data.frame(Compound = acc[as.numeric(res[,"id"]),"Compound"], res)

  #Compile information about function
  information <- paste0("Retention factors (k) were calculated for ", nrow(res), " out of ", length(pids)," peaks", ifelse(t0_mode=="peak", " (1 peak used for t0 estimation).", "."),
                        "\nDead time was equal to ", ifelse(t0_mode=="peak", paste0("the retention time of peak ", t_bckp, " (", round(t0,3), " min)"), paste0(round(t0,3), " minute(s)")),".")
  return(list(results = res, t0 = t0, information = information, call = cl_rec))
}
