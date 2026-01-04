#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculation of Resolution between adjacent peaks only
#Four different methods are included:
#Full Peak Width Method: (tr2-tr1)/(0.5*(W1+W2))
#Half Peak Width Method #1: 1.18*((tr2-tr1)/(W50_1+W50_2))
#Half Peak Width Method #2: (tr2-tr1)/(1.7*0.5*(W50_1+W50_2))
#Theoretical Plate, Separation & Retention Factor Method: 0.25*sqrt(N)*((a-1)/a)*(k/(1+k))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate HPLC chromatographic resolution using various methods
#'
#' @description Calculates chromatographic resolution using the full width, width at 50%, and separation factor methods (see \strong{Details}).
#'
#' @param input The output \code{list} from \code{\link{chrom_detect}}.
#' @param peaks1,peaks2 Either \code{"all"} (default) or \code{numeric} vectors of \strong{start} and \strong{end} peaks for which to calculate
#' separation factors. Each element of \code{peaks1} therefore corresponds to the same element of \code{peaks2}. When set to \code{"all"},
#' resolution is calculated for pairs of successive peaks (e.g. 1:2, 2:3 etc.).
#' @param method A \code{character} vector specifying the method(s) to use for the calculations. One or more of: \code{"all"} (default),
#' \code{"W0"} (full width), \code{"W50_1"} (50% width, variant 1), \code{"W50_2"} (50% width, variant 2),
#' and/or \code{"sepret"} (separation factor and theoretical plate method).
#' @param ks Used to configure retention factor calculations. Either a \code{numeric} value of the dead time (in minutes), or a \code{vector} of
#' length 2 specifying that dead time is to be obtained from the retention time of a specific \code{"peak"}, and providing the \code{numeric}
#' index of said peak that must be present in \code{input} data.
#' @param crit_w The critical width parameter to use for baseline calculation via FastChrom (\code{\link{fastchrom_bline}}).
#' Defaults to \code{"auto"} or can be set manually as a \code{numeric} (usually equal to the minimum peak width at half height).
#' @param verbose_res A \code{logical} switch specifying whether to include intermediate results such as retention factors and
#' separation factors into the results. Defaults to \code{FALSE}.
#'
#' @return A named \code{list} of length 4 containing a \code{data.frame} of \code{results}, which includes the peak IDs
#' (\code{id1} and \code{id2}), peak types (\code{type1} and \code{type2}), and resolution values calculated via one or
#' more methods specified in \code{method} and prefixed with \code{res_}. If \code{verbose_res} is \code{TRUE}, additional
#' data from \code{\link{chrom_retf}} and \code{\link{chrom_sepf}} are included.
#' Also included is the dead time \code{t0}, a \code{character} string of various \code{information} about the results,
#' and the function \code{call} as separate list elements.
#'
#' @details
#' Chromatographic resolution \eqn{R} may be calculated between two neighbouring peaks using various approaches which incorporate, in one
#' form or another, information about both peak apices and widths. Only the peak apices are separated at \eqn{R = 1}, while near-complete
#' resolution of similarly-sized peaks is achieved at \eqn{R >= 1.5}. For peaks \eqn{1} and a \strong{later-eluting} \eqn{2}, the simpler
#' and more widespread calculation methods involve the use of retention times \eqn{t_{R1}} and \eqn{t_{R2}}, and corresponding peak widths
#' at either the base (\eqn{W_1} and \eqn{W_2}) or 50% peak height (\eqn{W_{0.5h1}} and \eqn{W_{0.5h2}}). Three different equations commonly
#' utilized with these parameters are presented below. The first of these uses the full peak width at the base and is therefore the most
#' conservative measure that is relatively more affected by non-Gaussian peak shapes, often resulting in lower values than those obtained
#' by other methods.
#' \deqn{R = (t_{R2}-t_{R1})/(0.5\times(W_1+W_2))}
#' \deqn{R = 1.176\times[(t_{R2}-t_{R1})/(W_{0.5h1}+W_{0.5h2})]}
#' \deqn{R = (t_{R2}-t_{R1})/[1.7\times 0.5\times(W_{0.5h1}+W_{0.5h2})]}
#' \strong{For isocratic separations}, where the number of theoretical plates between adjacent peaks should largely be the same, the value
#' of \eqn{R} may also be related to peak retention factors (\eqn{k_1} and \eqn{k_2}), separation factor \eqn{\alpha}, and average number of
#' theoretical plates \eqn{\overline{N}} by the following equation (sometimes referred to as the fundamental resolution equation):
#' \deqn{R = (\sqrt{\overline{N}}/4)\times[(\alpha-1)/\alpha]\times[k_2/(1+k_2)]}
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
#' res <- chrom_res(input = dt, ks = c(1,"peak"))
#' }
#'
#' @seealso \code{\link{chrom_sepf}}, \code{\link{chrom_retf}}, \code{\link{chrom_detect}}, \code{\link{fastchrom_bline}}
chrom_res <- function(input, peaks1 = "all", peaks2 = "all", method = "all", ks = c(1,"peak"),
                      crit_w = "auto", verbose_res = FALSE) {

  #Perform checks
  input <- chkdt(input)

  if(!all(method %in% c("all", "W0", "W50_1", "W50_2", "sepret"))) stop("Argument 'method' must be one or more of: 'all', 'W0', 'W50_1', 'W50_2', and/or 'sepret'!")
  if(any(method %in% "all")) method <- c("W0", "W50_1", "W50_2", "sepret")

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
  sigmax <- acc[,"sig_max"]
  pklst <- input[["peak_list"]]

  #Create peak list
  #Check the retention factor argument 'ks'
  ks_num <- as.numeric(ks[!is.na(suppressWarnings(as.numeric(ks)))])
  if(any(ks %in% "peak") & any(ks_num>max(pids)|ks_num<1)) stop("When a peak index is used to derive dead time, it must be within the range of the input data!")

  #Combine peaks1 and peaks2, remove all peaks eluting earlier than t0, and check for duplicates
  if(any(c(peaks1, peaks2) %in% "all")) {
    if(any(peaks1 %in% "all")) peaks1 <- pids[-length(pids)]
    if(any(peaks2 %in% "all")) peaks2 <- pids[-1]
  }
  if(any(c("peak","manual") %in% ks)) {
    new_pids <- pids[pids %in% peaks1]
    new_rts <- rts[pids %in% peaks1]
    pk_rm <- if(any(ks %in% "peak")) which(new_pids<=ks_num) else if(any(ks %in% "manual")) which(new_rts<=ks_num)
    if(length(pk_rm)>0) {
      new_pids <- new_pids[-pk_rm]
      cat("\nA total of ", length(pk_rm), " peaks were removed due to eluting earlier than t0!", sep = "")
    }
    peaks_sub <- peaks1 %in% new_pids
    peaks1 <- peaks1[peaks_sub]
    peaks2 <- peaks2[peaks_sub]
  }

  dupchk <- c()
  for(i in seq_along(peaks1)) {
    dupchk[i] <- peaks2[i]==peaks1[i]
  }
  if(any(dupchk)) stop(paste0("The following ids in 'peaks1' are duplicated in 'peaks2': ", paste0(peaks1[which(dupchk)], collapse = ", "),"!"))
  upeaks <- unique(c(peaks1,peaks2))

  #Additional checks
  if(length(upeaks)<=1) stop("At least two peaks must be provided to calculate resolution!")
  if(any(ks=="peak") & length(upeaks)<=2) stop("More than 2 total peaks must be present when dead time (t0) mode is set to 'peak'! Otherwise, provide t0 explicitly.")
  #if(any(upeaks %in% as.numeric(ks[1]))) stop("The peak used to determine t0 for retention factor calculation must not be included in 'peaks1' or 'peaks2'!")
  if(length(peaks1)!=length(peaks2)) stop("Peak id vectors 'peaks1' and 'peaks2' must be equal in length!")
  if(!all(upeaks %in% pids)) stop(paste0("The following peak ids were not found in the input data: ", paste0(!upeaks %in% pids, collapse = ", "), "!"))

  #Calculate resolution and all other required variables
  output <- list()

  for(i in seq_along(peaks1)) {
    #Calculate peak widths at base and half-height
    ind1 <- peaks1[i]
    ind2 <- peaks2[i]
    hws <- lapply(c(ind1,ind2), function(x) peak_hw(input = pklst[[x]], accmax = c(rts[x], sigmax[x]), frac = c(0.001,0.5), resolved = if(!is.na(ptypes[x]) & ptypes[x]=="B") TRUE else FALSE, slnt = TRUE)[,c("frac","W")])
    resvec <- c("id1" = ind1, "id2" = ind2,
                "type1" = ptypes[ind1], "type2" = ptypes[ind2],
                "rt1" = rts[ind1], "rt2" = rts[ind2],
                "W0_1" = hws[[1]][1,"W"], "W0_2" = hws[[2]][1,"W"],
                "W50_1" = hws[[1]][2,"W"], "W50_2" = hws[[2]][2,"W"])
    output[[paste0("peaks_",ind1,"_",ind2)]] <- resvec
  }
  cnms <- names(output[[1]])
  output <- if(length(output)>1) Reduce(rbind.data.frame, output) else data.frame(t(output[[1]])) #Different approach for when only 2 peaks are present
  colnames(output) <- cnms
  output[,!colnames(output) %in% c("type1","type2")] <- apply(output[,!colnames(output) %in% c("type1","type2")], 2, as.numeric)

  #Calculate separation and retention factors
  sepres <- chrom_sepf(input_bckp, peaks1 = peaks1, peaks2 = peaks2, ks = ks)
  if(any(method %in% "sepret")) {
    seps <- sepres[["results"]]
    output <- cbind.data.frame(output, seps[,c("k1", "k2", "sep_factor")])
  }

  #Calculate resolution
  #pnms <- c("rt1", "rt2", "W0_1", "W0_2", "W50_1", "W50_2")
  #if(any(method %in% "sepret")) pnms <- c(pnms, "k1", "k2", "sep_factor")
  #p <- lapply(pnms, function(x) output[,x])
  tr1 <- output[,"rt1"]
  tr2 <- output[,"rt2"]
  w1 <- output[,"W0_1"]
  w2 <- output[,"W0_2"]
  wh1 <- output[,"W50_1"]
  wh2 <- output[,"W50_2"]
  if(any(method %in% "sepret")) {
    k1 <- output[,"k1"]
    k2 <- output[,"k2"]
    sf <- output[,"sep_factor"]
  }

  for(i in method) {
    if(i=="W0") {
      output[,"res_W0"] <- (tr2-tr1)/(0.5*(w1+w2))
    } else if(i=="W50_1") {
      output[,"res_W50_1"] <- 1.176*((tr2-tr1)/(wh1+wh2))
    } else if(i=="W50_2") {
      output[,"res_W50_2"] <- (tr2-tr1)/((1.7*0.5*(wh1+wh2)))
    } else if(i=="sepret") {
      output[,"N1"] <- n1 <- 5.545*(tr1/wh1)^2
      output[,"N2"] <- n2 <- 5.545*(tr2/wh2)^2
      output[,"res_seprt"] <- if(!all(is.na(c(n1,n2)))) 0.25*sqrt(mean(c(n1,n2), na.rm = TRUE))*((sf-1)/sf)*(k2/(1+k2)) else NA
    }
  }

  #OPTIONALLY remove extra columns
  if(!verbose_res) output <- output[,-grep("^W|k1|k2|sep_factor|^N", colnames(output))]

  #Add compound names (optional)
  if(any(colnames(acc) %in% "Compound")) output <- do.call(cbind.data.frame, list(Compound1 = acc[as.numeric(output[,"id1"]),"Compound"],
                                                                                  Compound2 = acc[as.numeric(output[,"id2"]), "Compound"],
                                                                                  output))

  #Compile information about function
  metnms <- c(W0 = "Full-Width", W50_1 = "Half-Width (variant 1)", W50_2 = "Half-Width (variant 2)", sepret = "Separation/Retention Factor")
  information <- paste0("Resolution was calculated via the ", paste0(metnms[method], " ('", method, "')", collapse = ", "), " methods.",
                        "\nDead time was equal to ", ifelse(ks[2]=="peak", paste0("the retention time of peak ", ks[1], " (", round(sepres[["t0"]],3), " min)"), paste0(round(sepres[["t0"]],3), " min")),".")
  return(list(results = output, t0 = sepres[["t0"]], information = information, call = cl_rec))
}
