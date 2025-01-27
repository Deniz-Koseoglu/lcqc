#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculation of separation factors between any peak pair
#NOTE: Calculated such that separation factor alpha is always >1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculation of Separation Factors for HPLC analytes
#'
#' @description Calculates the retention and separation factors for any number of peak pairs.
#'
#' @param input The output \code{list} from \code{\link{chrom_detect}}.
#' @param peaks1,peaks2 Either \code{"all"} (default) or \code{numeric} vectors of \strong{start} and \strong{end} peaks for which to calculate
#' separation factors. Each element of \code{peaks1} therefore corresponds to the same element of \code{peaks2}. When set to \code{"all"},
#' separation factors are calculated for pairs of successive peaks (e.g. 1:2, 2:3 etc.).
#' @param ks Used to configure retention factor calculations via \code{\link{chrom_retf}}. Usually a \code{vector} of length 2 specifying
#' the \code{character} mode of calculating retention factors (one of \code{"peak"} or \code{"manual"}) and an accompanying
#' \code{numeric} value as a parameter. When in \code{"peak"} mode, the accompanying value is the index of a peak
#' present in \code{input} data whose retention time is used as dead time \emph{t0} to calculate retention factors from.
#' When mode is \code{"manual"}, the accompanying value must simply be the dead time itself (in minutes). Alternatively,
#' retention indices may be provided as a \code{numeric} vector of length equal to that of \code{peaks1} and \code{peaks2},
#' in which case \code{\link{chrom_retf}} is not called.
#' @param crit_w The critical width parameter to use for baseline calculation via FastChrom (\code{\link{fastchrom_bline}}).
#' Defaults to \code{"auto"} or can be set manually as a \code{numeric} (usually equal to the minimum peak width at half height).
#'
#' @return A named \code{list} of length 4 containing a \code{data.frame} of \code{results}, which includes the peak IDs
#' (\code{id1} and \code{id2}), peak types (\code{type1} and \code{type2}), retention factors \code{k1} and \code{k2},
#' and the calculated separation factors \code{sep_factor}. \strong{If} the input data (from \code{\link{chrom_detect}})
#' also includes the \code{Compound} names for each peak pair, these are included as the first two columns.
#' Also included is the dead time \code{t0}, a \code{character} string of various \code{information} about the results,
#' and the function \code{call} as separate list elements.
#'
#' @details
#' Separation factor \eqn{\alpha} (or \emph{relative retention} in some older texts) is useful to determine the relative
#' degree of separation between any two peaks in a chromatogram. It is therefore a measure of selectivity. For two peaks
#' \eqn{i} and a \strong{later-eluting} \eqn{j}, \eqn{\alpha} can be calculated as a simple ratio of retention factors
#' \eqn{k_i} and \eqn{k_j}. If \eqn{\alpha =< 1}, no separation between the components takes place.
#' \deqn{\alpha = k_j/k_i}
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
#' res <- chrom_sepf(input = dt, peaks1 = c(2:6), peaks2 = c(3:7), ks = c(1,"peak"))
#' }
#'
#' @seealso \code{\link{chrom_retf}}, \code{\link{chrom_detect}}, \code{\link{fastchrom_bline}}
chrom_sepf <- function(input, peaks1 = "all", peaks2 = "all", ks = c(1,"peak"), crit_w = "auto") {

  #Perform checks
  input <- chkdt(input)

  #Generate function call
  cl_rec <- match.call()

  #Retrieve data for calculation of Theoretical Plate Number
  #Pre-process input data
  input_bckp <- input
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  acc <- input[["acc_tops"]]
  ptypes <- input[["type_df"]][["maxes"]][,"ptype"] #Peak types
  pids <- acc[,"peak"]
  rts <- acc[,"rt_max"]

  #Check the retention factor argument 'ks'
  ks_num <- as.numeric(ks[!is.na(suppressWarnings(as.numeric(ks)))])
  if(any(ks %in% "peak") & any(ks_num>max(pids)|ks_num<1)) stop("When a peak index is used to derive dead time, it must be within the range of the input data!")

  #Combine peaks1 and peaks2, remove all peaks eluting earlier than t0, and check for duplicates
  if(any(c(peaks1, peaks2) %in% "all")) {
    if(any(c("peak","manual") %in% ks)) {
      pk_rm <- if(any(ks %in% "peak")) which(pids<=ks[ks_num]) else if(any(ks %in% "manual")) which(rts<=ks[ks_num])
      if(length(pk_rm)>0) pids <- pids[-pk_rm]
      if(length(pk_rm)>1) cat("\nA total of ", length(pk_rm), " peaks were removed due to eluting earlier than t0!", sep = "")
    }
    peaks1 <- pids[-length(pids)]
    peaks2 <- pids[-1] #Retrieve adjacent later-eluting peaks for each id in peaks1
  }
  dupchk <- c()
  for(i in seq_along(peaks1)) {
    dupchk[i] <- peaks2[i]==peaks1[i]
  }
  if(any(dupchk)) stop(paste0("The following ids in 'peaks1' are duplicated in 'peaks2': ", paste0(peaks1[which(dupchk)], collapse = ", "),"!"))
  upeaks <- unique(c(peaks1,peaks2))

  #Additional checks
  if(length(upeaks)<=1) stop("At least two peaks must be provided to calculate separation factors!")
  if(any(ks=="peak") & length(upeaks)<=2) stop("More than 2 total peaks must be present when dead time (t0) mode is set to 'peak'! Otherwise, provide t0 explicitly.")
  if(any(ks %in% c("peak", "manual")) & any(upeaks %in% ks_num)) stop("The peak used to determine t0 for retention factor calculation must not be included in 'peaks1' or 'peaks2'!")
  if(length(peaks1)!=length(peaks2)) stop("Peak id vectors 'peaks1' and 'peaks2' must be equal in length!")
  if(!all(upeaks %in% pids)) stop(paste0("The following peak ids were not found in the input data: ", paste0(!upeaks %in% pids, collapse = ", "), "!"))
  if(!any(ks %in% c("peak","manual")) & length(ks)!=length(upeaks)) stop("When retention factors ('ks') are provided manually, their length must equal the total number of peaks ('peaks1', 'peaks2')!")
  for(i in seq_along(peaks1)) {
    if(peaks2[i]<peaks1[i]) {
      cat("\nElement", i, " of 'peaks2' represents a peak that elutes earlier than the corresponding element of 'peaks1'! Switching elements...", sep = "")
      rep1 <- peaks1[i]
      rep2 <- peaks2[i]
      peaks1[i] <- rep2
      peaks2[i] <- rep1
    }
  }

  #Calculate or retrieve retention factors (ks)
  if(any(ks %in% c("peak", "manual"))) {
    kres <- chrom_retf(input = input_bckp, t0_mode = ks[which(ks %in% c("peak","manual"))], t0 = ks_num, peaks = upeaks)[["results"]]
    t0_val <- kres[1,4]
    kfs <- kres[,"k"]
  } else kfs <- ks

  #Calculate separation factors and compile results
  ks2 <- kfs[upeaks %in% peaks2]
  ks1 <- kfs[upeaks %in% peaks1]
  sfacs <- ks2/ks1
  res <- list(peaks1, peaks2, ptypes[peaks1], ptypes[peaks2], ks1, ks2, sfacs)
  cnames_res <- c("id1", "id2", "type1", "type2", "k1", "k2", "sep_factor")
  res <- Reduce(cbind.data.frame, res)
  colnames(res) <- cnames_res

  #Add compound names (optional)
  if(any(colnames(acc) %in% "Compound")) res <- do.call(cbind.data.frame, list(Compound1 = acc[as.numeric(res[,"id1"]),"Compound"],
                                                                               Compound2 = acc[as.numeric(res[,"id2"]), "Compound"],
                                                                               res))

  #Compile information about function
  information <- paste0("Dead time was equal to ", ifelse(ks[2]=="peak", paste0("the retention time of peak ", ks[1], " (", round(t0_val,3), " min)"), paste0(round(t0_val,3), " min")),".")
  return(list(results = res, t0 = round(t0_val,3), information = information, call = cl_rec))
}
