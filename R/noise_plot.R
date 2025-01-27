#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Plot FD or SD noise
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Visualise noise data
#'
#' @description Create a summary plot of noise data, usually derived from \code{\link{chrom_derlims}}.
#'
#' @param noise A \code{numeric} vector containing noise data.
#' @param thres A \code{numeric} vector of length 2 containing the lower and upper noise thresholds.
#' @param plot_title The \code{character} plot title.
#' @param y_lab The \code{character} y-axis title.
#' @param asprat Aspect ratio of the plot.
#'
#' @return A \code{ggplot}-class plot object.
#' @export
#'
#' @examples
#' #Get noise and threshold data
#' \dontrun{
#' dt <- lcqc:::wf_detpeaks[["results"]]
#' nsdt <- dt[["Derivative_Noise"]][["SD_noise"]]
#' thrdt <- dt[["Derivative_Limits"]][["SD"]]
#' noise_plot(nsdt, thrdt)
#' }
#'
#' @seealso \code{\link{chrom_derlims}}
#'
#' @importFrom stats complete.cases
noise_plot <- function(noise, thres, plot_title = "Peak Recognition Thresholds", y_lab = "Noise", asprat = 0.71) {

  #Preliminary checks
  if(!is.vector(noise) | !is.numeric(noise)) stop("The input 'noise' must be a numeric vector!")
  if(!is.numeric(thres) | length(thres)!=2) stop("The upper/lower noise thresholds ('thres') must be a numeric vector of length 2!")

  der_data <- cbind.data.frame(Index = seq(length(noise)), Noise = noise)

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_xvar <- "Index"
  aes_yvar <- "Noise"

  plotres <- ggplot() +
    geom_path(data = der_data[complete.cases(der_data),], aes(x = .data[[aes_xvar]], y = .data[[aes_yvar]]), colour = "black", lty = 1, lwd = 0.5) +
    geom_hline(yintercept = thres, colour = "darkred", lty = 2, lwd = 0.6) +
    labs(x = "Index", y = y_lab,
         title = plot_title,
         subtitle = paste0("Inferior Threshold: ", round(min(thres),2), "; Superior Threshold: ", round(max(thres),2))) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 11, colour = "black"))
  return(plotres)
}
