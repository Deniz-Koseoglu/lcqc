#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: FD and SD superior and inferior peak detection limits via optimal Z-scores results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Determine First and Second Derivative Peak Detection Thresholds
#'
#' @description Calculation of inferior (low) and superior (high) first/second derivative thresholds to use for peak detection.
#'
#' @param x Vector of peak \strong{start} indices (when \code{y} is provided), or a 2-column \code{data.frame} containing peak start and end indices.
#' @param y Either \code{NULL} (if \code{x} is a \code{data.frame}) or a vector of peak \strong{end} indices equal in length to \code{x} (if the latter is also a vector).
#' @param fder A vector of first derivatives derived from a signal.
#' @param sder A vector of second derivatives derived from the same signal as \code{fder}.
#' @param method Method used to calculate derivative-based peak detection thresholds. One of: \code{"vaz"} (Vaz et al., 2016), \code{"zscore"} (Z-score), or \code{"ncore"} (noise core, \strong{Details}).
#' @param outs Outlier detection/removal method. One \strong{or more} of: \code{"none"}, \code{"all"}, \code{"iqr"} (inter-quartile range), \code{"quant"} (quantile), or \code{"sd"} (standard deviation, see \strong{Details}).
#' @param sens A vector of two \code{numeric} sensitivity values for fine adjustment of peak detection thresholds. Unique for each \code{method} - see \strong{Details}.
#'
#' @return A \code{list} with 3 elements: \code{$FD} and \code{$SD} each a containing a vector of length 2 with inferior and superior first and second derivative thresholds, respectively.
#' Finally, element \code{$noise} contains a list of \code{fder} (\code{$FD_noise}) and \code{sder} (\code{$SD_noise}) values determined to be noise.
#'
#' @details
#' As a first step, the function removes all \code{fder} and \code{sder} points within the peak regions provided in \code{x} and/or \code{y}.
#' Outliers are then removed from the resulting vector of noise using \strong{one or more} (or none) of the following criteria in any combination:
#' \describe{
#'  \item{\code{"iqr"}}{ All values outside of \eqn{Q_1-1.5\times IQR} and \eqn{Q_3+1.5\times IQR} are removed (where \eqn{Q_1}, \eqn{Q_3}, and \eqn{IQR} are the first quartile, third quartile, and inter-quartile range, respectively).}
#'  \item{\code{"quant"}}{ All values outside of the 2.5% and 97.5% quantiles are removed.}
#'  \item{\code{"sd"}}{ All values outside \eqn{\overline{X}\pm 2.24\times SD} are removed (where \eqn{\overline{x}} and \eqn{SD} are the mean and standard deviation of the derivatives, respectively).}
#' }
#' Inferior and superior (i.e. low and high) peak detection thresholds are then calculated from the remaining data using one of three methods.
#' \strong{Method \code{"vaz"}} is based on the work of Vaz et al. (2016). Differences between the global median of derivative signal \eqn{M_D}
#' and the derivative signal are calculated and a new median \eqn{M_new} calculated from these results. Finally, the inferior and superior thresholds \eqn{T} are calculated as follows:
#' \deqn{T=M_D\pm sens1\times M_new/sens2}
#' Here, \eqn{sens1} (given in \code{sens[1]}) is an empirical factor with which the threshold range widens/increases.
#' Conversely, \eqn{sens2} (given in \code{sens[2]}) is inversely related to the threshold range.
#'
#' \strong{Method \code{"zscore"}} is a slight variation reliant solely on the \code{median} of derivatives:
#' \deqn{T=M_D+sens1\times M_D/sens2}
#' Finally, \strong{method \code{"ncore"}} follows the widely-practised noise-core approach (e.g. Waters Corporation, 2016).
#' The noise core is determined as the standard deviation \eqn{SD} distance from the mean \eqn{\overline{X}} of derivatives:
#' \deqn{T=\overline{X}\pm sens1\times SD/sens2}
#' Many industry-standard approaches recommend a value of 4 for \eqn{sens1}, stating that \eqn{\pm 4SD} best defines chromatographic noise.
#'
#' @export
#'
#' @examples
#' #Calculate derivatives
#' xvals <- lcqc::simlc1[,"Time"]
#' yvals <- lcqc::simlc1[,"Signal"]
#' ders <- chrom_deriv(xvals, yvals)
#' fder <- ders[[1]]
#' sder <- ders[[2]]
#'
#' #Get optimal z-score peak regions
#' zlims <- z_optim(yvals)[[1]]
#'
#' #Get derivative peak detection thresholds
#' thres <- chrom_derlims(x = zlims, y = NULL, fder, sder, method = "ncore", sens = c(2,1))
#' noise_plot(thres[["noise"]][["FD_noise"]], thres[["FD"]])
#' noise_plot(thres[["noise"]][["SD_noise"]], thres[["SD"]])
#'
#' @references
#' Vaz, F.A.S., Neves, L.N.O., Marques, R., Sato, R.T., Oliveira, M.A.L. (2016), 'Chromophoreasy, an Excel-Based Program for Detection and Integration of Peaks from Chromatographic and Electromigration Techniques', \emph{Journal of the Brazilian Chemical Society} \strong{27} (10), pp. 1899-1911.
#'
#' Waters Corporation (2016), 'ApexTrack Integration: Theory and Application', document number 72000494EN, available at: https://www.waters.com/waters/library.htm?cid=511436&lid=1546221&locale=en_GB (accessed 17.04.2024).
#'
#' @seealso \code{\link{z_thres}}, \code{\link{chrom_deriv}}, \code{\link{noise_plot}}
#'
#' @importFrom stats median
chrom_derlims <- function(x, y=NULL, fder, sder, method="ncore", outs = "iqr", sens = c(3,1)) {
  #Preliminary checks
  if(!is.numeric(fder) | !is.numeric(sder)) {
    stop("First and second derivative inputs must both be numeric vectors!")
  }

  if(!is.data.frame(x) & !is.matrix(x)) {
    if(is.atomic(x) & is.numeric(x) & (is.null(y) | !is.atomic(y) | !is.numeric(y))) {
      stop("'x' must either be a 2-column dataframe with peak start and end indices, or a numeric vector of peak starts alongside peak ends given in 'y'!")
    } else if(length(x)!=length(y)) stop("Length of 'x' must be equal to that of 'y'!")
  }

  if(length(sens)!=2 | !is.numeric(sens)) {
    stop("The sensitivity parameters ('sens') must be a numeric vector of length 2!")
  }

  if(!any(outs %in% c("all", "none", "iqr", "quant", "sd"))) {
    stop("Outlier handling method not recognized! Available options: 'all', 'none', 'iqr', 'quant', or 'sd'!")
  }

  if(!any(method %in% c("ncore", "vaz", "zscore"))) stop("Method must be one of: 'vaz', 'zscore', 'ncore'!")

  if(is.vector(x) & is.vector(y)) zrle_df <- cbind.data.frame(start = x, end = y) else zrle_df <- x

  zrle_inds <- unlist(apply(zrle_df, 1, function(z) z[1]:z[2]), use.names = FALSE)
  dist_list <- list("FD_noise"=fder[-zrle_inds], "SD_noise"=sder[-zrle_inds])

  #Handling outliers
  if(!any(outs %in% "none")) {

    if(any(outs %in% "all")) {
      req_score <- 3
      outs <- c("iqr", "quant", "sd")
    } else req_score <- length(outs)

    for(i in seq_along(dist_list)) {

      #Create a list of distances with zeroes removed!
      zerofree_list <- dist_list[[i]][dist_list[[i]]!=0]

      outs_df <- data.frame(matrix(ncol = 3, nrow = length(dist_list[[i]])))
      colnames(outs_df) <- c("IQR", "Quant", "SDs")
      #out_df[,"Data"] <- dist_list[[i]]

      if(any(outs %in% "iqr")) { #Take care to REMOVE ZEROES AND NAs from the quantiles calculation!
        quants <- quantile(zerofree_list, na.rm = TRUE)
        quart_3 <- quants[names(quants)=="75%"]
        quart_1 <- quants[names(quants)=="25%"]
        iqr <- quart_3-quart_1
        low_lim <- quart_1 - 1.5*iqr
        high_lim <- quart_3 + 1.5*iqr
        iqr_scores <- sapply(dist_list[[i]], function(z) if(!is.na(z)) { if(z<low_lim | z>high_lim) 1 else 0 } else NA)
        outs_df[,"IQR"] <- iqr_scores
      }

      if(any(outs %in% "quant")) { #Again, remove zeroes and NAs
        quants <- quantile(zerofree_list, probs = c(0.025, 0.975), na.rm = TRUE)
        quant_scores <- sapply(dist_list[[i]], function(z) if(!is.na(z)) { if(z<min(quants) | z>max(quants)) 1 else 0 } else NA)
        outs_df[,"Quant"] <- quant_scores
      }

      if(any(outs %in% "sd")) {
        avg_val <- mean(zerofree_list, na.rm = TRUE)
        sds <- sd(zerofree_list, na.rm = TRUE)
        low_lim <- avg_val-2.24*sds
        high_lim <- avg_val+2.24*sds
        sd_scores <- sapply(dist_list[[i]], function(z) if(!is.na(z)) { if(z<low_lim | z>high_lim) 1 else 0 } else NA)
        outs_df[,"SDs"] <- sd_scores
      }

      total_scores <- rowSums(outs_df, na.rm = TRUE)
      scorecheck <- which(total_scores >= req_score)
      dist_list[[i]][scorecheck] <- NA #dist_list[[i]] <- dist_list[[i]][-scorecheck]
    }
  }

  if(method=="vaz") {

    #Calculate Medians from First and Second Derivatives
    MFD <- median(dist_list[[1]], na.rm = TRUE)
    MD_first <- median(dist_list[[1]]-MFD, na.rm = TRUE)
    MSD <- median(dist_list[[2]], na.rm = TRUE)
    MD_second <- median(dist_list[[2]]-MSD, na.rm = TRUE)

    #Calculating Superior and Inferior Thresholds for Peak Start and End
    TS_first <- MFD+(sens[1]*MD_first/sens[2])
    TI_first <- MFD-(sens[1]*MD_first/sens[2])
    TS_second <- MSD+(sens[1]*MD_second/sens[2])
    TI_second <- MSD-(sens[1]*MD_second/sens[2])

  } else if(any(c("zscore", "ncore") %in% method)) {

    if(method=="zscore") {
      dist_res <- list()

      for(i in seq_along(dist_list)) {
        dist_res[[i]] <- sapply(seq_along(dist_list[[i]]), function(z) {
          if(any(is.na(dist_list[[i]][(z-1):(z+1)]))) {
            res <- NA
          } else {
            res <- dist_list[[i]][z]-mean(dist_list[[i]][c(z-1,z+1)], na.rm = TRUE)
          }
          return(res)})
      }

      AbsMD_first <- median(abs(dist_res[[1]]), na.rm = TRUE)
      AbsMD_second <- median(abs(dist_res[[2]]), na.rm = TRUE)
      TS_first <- AbsMD_first+sens[1]*AbsMD_first/sens[2]
      TI_first <- AbsMD_first-sens[1]*AbsMD_first/sens[2]
      TS_second <- AbsMD_second+sens[1]*AbsMD_second/sens[2]
      TI_second <- AbsMD_second-sens[1]*AbsMD_second/sens[2]

    } else if(method=="ncore") {
      MN_first <- mean(dist_list[[1]], na.rm = TRUE)
      MN_second <- mean(dist_list[[2]], na.rm = TRUE)
      SD_first <- sd(dist_list[[1]], na.rm = TRUE)
      SD_second <- sd(dist_list[[2]], na.rm = TRUE)
      TS_first <- MN_first+sens[1]*SD_first/sens[2] #WATERS recommends 4xSD (e.g. sens == c(4,1)) in their Empower Software Data Acquisition and Processing Theory Guide
      TI_first <- MN_first-sens[1]*SD_first/sens[2]
      TS_second <- MN_second+sens[1]*SD_second/sens[2]
      TI_second <- MN_second-sens[1]*SD_second/sens[2]
    }
  }
  return(list(FD = c(TI_FD = TI_first, TS_FD = TS_first), SD = c(TI_SD = TI_second, TS_SD = TS_second), noise = dist_list))
}
