#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Z-score peak detection optimization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Optimization of z-score thresholding algorithm
#'
#' @description The function tests a specific range of input values for the \pkg{lcqc} z-scores peak picking algorithm (see \code{\link{z_thres}}) using a numeric vector.
#' The combination of parameters resulting in the highest number of legitimate peaks (along a numeric vector \code{x}) at the lowest values of \code{lag} and \code{threshold} is selected.
#' These parameters are returned alongside the start and end indices of corresponding detected peak regions.
#'
#' @param x Input numeric vector of equi-spaced signal values.
#' @param lag_rng Sequence of lag values to test, as outlined in \code{\link{z_thres}}.
#' @param thres_rng Sequence of threshold values to test, as outlined in \code{\link{z_thres}}.
#' @param peak_thres Threshold value. Minimum number (or fraction) of consecutive points in \code{x} required to initially accept a peak.
#' @param noise_thres Threshold value. Minimum number (or fraction) of consecutive points in \code{x} required to keep a peak in the final stage of optimization. Non-conforming peaks (which are too narrow) are removed as noise.
#' @param mode One of \code{"percent"} or \code{"absolute"}. Determines whether \code{peak_thres} and \code{noise_thres} are interpreted as fractions of total points in \code{x}, or as absolute values.
#'
#' @return A list of length two containing the following elements:
#' \describe{
#'  \item{\code{Peak_Limits}}{A 2-column \code{data.frame} containing the start and end indices of detected peaks along \code{x}.}
#'  \item{\code{Best_ZScore_Run}}{The results of the "best" z-score thresholding run, formatted as per the \code{\link{z_thres}} function.}
#' }
#' @export
#'
#' @examples
#' yvals <- lcqc::simlc1[,"Signal"]
#' z_optim(yvals)
#'
#' @seealso \code{\link{z_thres}}
#'
#' @importFrom utils head
z_optim <- function(x, lag_rng = seq(0.01, 0.05, 0.005) , thres_rng = seq(1, 6, 0.5),
                    peak_thres = 0.02, noise_thres = 0.005, mode = "percent") {
  #Preliminary checks
  if(!is.atomic(x) | !is.numeric(x)) {
    stop("A vector of numeric signal values is required!")
  }

  if(!any(c("absolute","percent") %in% mode)) stop("Argument 'mode' must be one of: 'absolute' or 'percent'!")
  if(mode=="absolute" & !all(sapply(c(noise_thres, peak_thres), function(x) x%%1==0))) {
    stop("Peak and noise thresholds ('peak_thres', 'noise_thres') must be whole numbers when 'mode' is 'absolute'!")
  }

  if(any(c(peak_thres, noise_thres) <= 0)) stop("Peak and noise thresholds ('peak_thres', 'noise_thres') must be above 0!")

  if(length(peak_thres)!=1 | length(noise_thres)!=1) {
    stop("Peak and noise thresholds must be a numeric value of length 1!")
  } else if(mode=="percent" & (peak_thres>1 | peak_thres<0 | noise_thres>1 | noise_thres<0)) {
    stop("Peak and noise thresholds must be between 0 and 1 in value when 'mode'=='percent'!")
  }

  if(any(!is.numeric(c(lag_rng, thres_rng)))) {
    stop("Lag and threshold optimization ranges for the z-score algorithm must be numeric vectors!")
  }

  point_num <- length(x)

  #Create a test grid based on the ranges of parameters to be optimized
  z_testgrid <- expand.grid(lag = round(point_num*lag_rng,0),
                            threshold = thres_rng)

  #Carry out all runs
  z_optres <- lapply(1:nrow(z_testgrid), function(y) z_thres(y = x,
                                                             lag = z_testgrid[y,1],
                                                             threshold = z_testgrid[y,2],
                                                             influence = 0))

  #Carry out run-length encoding of the positive and negative peak region identifications
  z_rle <- lapply(z_optres, function(y) rle(y[["signals"]]))

  #Set up peak width and noise thresholds (either absolute or a fraction of the total signal length)
  pthres <- if(mode=="percent") point_num*peak_thres else if(mode=="absolute") peak_thres
  nthres <- if(mode=="percent") point_num*noise_thres else if(mode=="absolute") noise_thres

  #Compile the number of peaks, noise regions, and their totals into a data.frame
  z_merit <- lapply(z_rle, function(y) { peaks <- length(which(y[["lengths"]]> pthres & y[["values"]]==1));
  noise <- length(which(y[["lengths"]]<nthres & y[["values"]]==1));
  total <- length(which(y[["values"]]==1));
  return(c(peaks = peaks, noise = noise, total = total))})
  z_merit <- Reduce(rbind.data.frame, z_merit)
  colnames(z_merit) <- c("peaks", "noise", "total")

  #Filter the run data based on thresholds and other criteria
  z_merit <- z_merit[z_merit[,"total"]>0,]
  z_merit <- z_merit[z_merit[,"noise"] <= mean(z_merit[,"noise"], na.rm = TRUE),]
  z_merit <- z_merit[which(abs(z_merit[,"peaks"]-z_merit[,"total"]) <= mean(abs(z_merit[,"peaks"]-z_merit[,"total"]),na.rm = TRUE)),]
  z_merit <- z_merit[order(z_merit[,"peaks"], decreasing = TRUE),]
  z_merit <- z_merit[z_merit[,"peaks"]==max(z_merit[,"peaks"], na.rm = TRUE),]

  z_merit[,c("lag", "threshold")] <- Reduce(rbind.data.frame,
                                            lapply(z_optres[as.numeric(rownames(z_merit))], function(y) y[["params"]][1:2]))

  opt_params <- z_merit[order(z_merit[,"lag"], z_merit[,"threshold"])[1],]
  rm(z_merit)
  opt_run <- z_optres[[as.numeric(rownames(opt_params))]]
  opt_rle <- z_rle[[as.numeric(rownames(opt_params))]]

  zrle_end <- cumsum(opt_rle[["lengths"]])
  zrle_start <- c(1, head(zrle_end, -1) + 1)

  zrle_df <- data.frame(start = zrle_start, end = zrle_end)
  zrle_df <- zrle_df[which(opt_rle[["values"]]==1),]

  #Remove regions identified as noise
  zrle_df <- zrle_df[which(zrle_df["end"]-zrle_df["start"] >= nthres),]

  return(list(Peak_Limits = zrle_df, Best_ZScore_Run = opt_run))
}
