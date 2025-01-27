#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION: Create a graph showing identified peak start, end, and maxima
# Plot main chromatogram with or without peak markers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Plot chromatogram with or without peak markers
#'
#' @description Plots a chromatogram with various possible levels of complexity, from a simple plot of the signal versus retention time to inclusion of
#' peak markers (e.g. peak boundaries, inflection, upslope, and apex points) and the peak detection signal amplitude limit.
#'
#' @param chrom_df A \code{data.frame} containing time (x-axis) and signal (y-axis) variables, whose colnames are included in \code{chrom_vars}.
#' @param ptab The peak table containing peak apex/inflection/upslope/boundary point markers, provided as a \code{data.frame}. Often output from function \code{\link{chrom_detect}} or \code{\link{peakfind}}.
#' @param chrom_vars A \code{character} vector of length 2 providing column names of x- and y-axis variables present in \code{chrom_df}.
#' @param id Column name of the peak ID (index) included in \code{ptab}. If set to \code{"auto"} (default), peak indices are assigned automatically by \code{seq(1,nrow(ptab),1)}.
#' @param apex Controls plotting of peak \strong{apices}.
#' Either a \code{character} vector of length 2 with column names (present in \code{ptab}) containing indices of left and right peak boundaries, or \code{NA} (default).
#' @param inf Controls plotting of peak \strong{inflection} points.
#' Either a \code{character} vector of length 2 with column names (present in \code{ptab}) containing indices of left and right peak boundaries, or \code{NA} (default).
#' @param ups Controls plotting of peak \strong{upslope} points.
#' Either a \code{character} vector of length 2 with column names (present in \code{ptab}) containing indices of left and right upslope points, or \code{NA} (default).
#' @param bound Controls plotting of peak \strong{start/end boundaries}.
#' Either a \code{character} vector of length 2 with column names (present in \code{ptab}) containing indices of left and right peak boundaries, or \code{NA} (default).
#' @param hlt A vector of peak apex \strong{indices} present in \code{ptab} that must be highlighted.
#' @param norm_chrom A \code{logical} indicating whether the chromatogram signal (y-axis) should be normalized to a maximum value of 100. Defaults to \code{TRUE}.
#' @param cols Either \code{"default"} or a \strong{named} vector of recognized colors. Accepted names are: \code{"id"} (peak number), \code{"main"} (main plot),
#' \code{"apex"} (peak apices), \code{"hlt"} (highlight for apices set in \code{hlt}), \code{"inf"} (inflection points),
#' \code{"ups"} (upslope points), \code{"bound"} (peak start/end boundaries), \code{"ampline"} (signal amplitude threshold for peak detection).
#' @param ptab_mode A \code{character} value denoting whether peak marker data in \code{ptab} is provided as \strong{data indices} (\code{"index"}, default) or \strong{real values}, e.g. retention time (\code{"real"}).
#' @param zoom Either a vector of length 2 providing \code{c(low,high)} x-axis limits, or set to \code{"auto"} to automatically zoom into the detected peak region.
#' @param which_peaks Either \code{NA} (default) or a \code{numeric} vector of \code{ptab} peak indices to plot, discarding the rest.
#' @param lablim Either \code{NA} (default) or a single \code{numeric} value between 0 and 100. Denotes the \strong{relative} signal threshold below which peak apices are not labeled with numbers.
#' @param amp_line Amplitude limit to be plotted as a dashed line, given as a single \code{numeric} value. Defaults to \code{NA} (no line plotted).
#' @param xlab The x-axis \code{character} label.
#' @param ylab The y-axis \code{character} label.
#' @param plot_title The main title of the plot (\code{character}).
#' @param draw Should the generated plot be shown in the graphics window? \code{TRUE} by default.
#' @param asprat Aspect ratio of the plot (defaults to 0.71).
#'
#' @return A \code{ggplot}-class object containing the plot.
#' @export
#'
#' @examples
#' #Simple plot
#' chrom_plot(lcqc::simlc1)
#'
#' #Plot with peak markers
#' \dontrun{
#' dt <- lcqc:::wf_detpeaks[["results"]]
#' chrom <- dt[["Chromatogram"]] #Get chromatogram data
#' pt <- dt[["Peak_Extents"]] #Get peak table
#' alim <- dt[["Amplitude_Limit"]] #Get amplitude limit
#' chrom_plot(chrom_df = chrom, ptab = pt, norm_chrom = TRUE, id = "peak", apex = "ind_finmax",
#' inf = c("ind_linf", "ind_rinf"), ups = c("ind_lups", "ind_rups"),
#' bound = c("ind_starts", "ind_ends"), amp_line = alim)
#'}
#'
#' @importFrom scales breaks_pretty
#' @importFrom stats setNames
chrom_plot <- function(chrom_df, ptab = NA, chrom_vars="auto", id = "auto", apex = NA, inf = NA, ups = NA, bound = NA, hlt = NA, norm_chrom = TRUE,
                       cols = "default", ptab_mode = "index", zoom = "auto", which_peaks = NA, lablim = NA, amp_line = NA,
                       xlab = chrom_vars[1], ylab = chrom_vars[2], plot_title = "Chromatogram", draw = TRUE, asprat = 0.71) {

  #Preliminary checks
  if(any(chrom_vars=="auto")) {
    cat("\nMain variable names not given! Using the first two columns for X and Y axis, respectively...")
    chrom_vars <- colnames(chrom_df)[1:2]
  } else if(length(chrom_vars)!=2) stop("The length of 'chrom_vars' must be 2 (RT and Signal columns, in that order)!")

  if((length(zoom)!=2 | !is.numeric(zoom)) & !zoom %in% "auto" & !any(is.na(zoom))) stop("When provided, 'zoom' must be a numeric vector of length 2 or set to 'auto'!")
  if(!is.data.frame(chrom_df)) stop("The input chromatogram ('chrom_df') must be a data frame!")
  if(!all(is.na(ptab)) & !is.data.frame(ptab)) stop("When provided, the peak table ('ptab') must be a data frame!")
  if(!is.na(amp_line) & !is.numeric(amp_line)) stop("When provided, 'amp_line' must be a single numeric value!")
  if(!is.na(lablim) & (lablim < 0 | lablim > 100)) stop("When provided, 'lablim' must be a single value between 0 and 100!")

  #Checks specific to the peak table
  if(is.data.frame(ptab)) {
    if(all(is.na(c(apex, inf, ups, bound)))) stop("The peak table was provided but no column names specified!")
    if(!ptab_mode %in% c("real","index") & is.data.frame(ptab)) stop("Argument 'ptab_mode' must be one of: 'real' or 'index'!")

    #Check peak ID/number identifier
    if(any(id=="auto") & is.data.frame(ptab)) {
      ptab[,"ID"] <- seq(nrow(ptab))
      id <- "ID"
    }

    #Check peaks for highlighting
    #if(any(hlt < 1 | hlt > max(as.numeric(ptab[,id]), na.rm = TRUE))) stop("Peak indices for highlighting ('hlt') must be within the range of the input peak table!")

    #Check peak labels/subsets
    if(is.numeric(which_peaks) & !all(which_peaks %in% as.numeric(ptab[,id]))) stop("All peak IDs specified in 'which_peaks' must be present in the peak table!")

    #Check the lengths of peak table variable column names
    colist <- list(id = id, apex = apex, inf = inf, ups = ups, bound = bound)
    which_not_na <- which(sapply(colist, function(x) !all(is.na(x))))
    req_lens <- c(1,1,2,2,2)[which_not_na]
    wrong_lens <- which(!lengths(colist[which_not_na]) %in% req_lens)
    if(length(wrong_lens)>0) stop(paste0("The length(s) of the following peak table ('ptab') column IDs is/are wrong: ", paste0("'", names(colist)[wrong_lens], "'", collapse = ", "), "!"))
    if(!all(unlist(colist[which_not_na]) %in% colnames(ptab))) stop("All peak identifier ('id', 'apex', 'inf', 'ups', 'bound') column names must be present in the 'ptab' data frame!")
    colist <- colist[!names(colist) %in% "id"]
  }

  #Set up plot colours
  defcols <- c(id = "black", main = "grey30", apex = "navy", hlt = "darkred", inf = "darkorange", ups = "lightblue", bound = "darkgreen", ampline = "purple")
  cols <- supp_pars(pars = cols, defpars = defcols, parlb = "cols")

  cat("\nPlotting results...\n")

  #Get chromatogram
  chrom_data <- chrom_df[,chrom_vars]
  chrom_data[,"Index"] <- 1:nrow(chrom_data)

  #Optionally scale the chromatogram intensity
  if(norm_chrom) {
    chrom_max <- max(chrom_data[,chrom_vars[2]], na.rm = TRUE)
    chrom_data[,chrom_vars[2]] <- chrom_data[,chrom_vars[2]]/chrom_max*100
    if(!is.na(amp_line)) amp_line <- amp_line/chrom_max*100
  }

  #Work with peak table
  if(is.data.frame(ptab)) {

    #Remove peaks not in 'which_peaks'
    if(is.numeric(which_peaks)) ptab <- ptab[as.numeric(ptab[,id]) %in% which_peaks,]

    #Get vector of indices/RTs with corresponding group value (inflection point, apex, start, end etc.)
    for(i in seq_along(colist)) names(colist[[i]]) <- rep(names(colist)[i], length(colist[[i]]))
    colvec <- unlist(unname(colist))
    colvec <- colvec[!is.na(colvec)]
    peak_data <- setNames(as.data.frame(matrix(nrow = 0, ncol = 3)), c("Index", "Group", "Label"))
    for(i in seq_along(colvec)) {
      app_data <- do.call(cbind.data.frame, list(Index = ptab[,colvec[i]], Group = names(colvec)[i], Label = if(names(colvec)[i]=="apex") ptab[,id] else ""))

      #Optionally highlight peaks
      if(names(colvec)[i]=="apex" & !any(is.na(hlt))) app_data[which(app_data[,"Label"] %in% hlt),"Group"] <- "hlt"

      peak_data <- rbind.data.frame(peak_data, app_data)
    }

    #Append Time and Signal (X and Y) values from chromatogram data
    if(ptab_mode=="real") {
      peak_data[,chrom_vars[1]] <- peak_data[,"Index"]
      #Interpolate nearest signal from chromatogram
      peak_data[,chrom_vars[2]] <- approx(chrom_data[,chrom_vars[1]], chrom_data[,chrom_vars[2]], xout = peak_data[,"Index"])[["y"]]
      #Interpolate nearest 'Index' from chromatogram
      peak_data[,"Index"] <- round(approx(chrom_data[,chrom_vars[1]], chrom_data[,"Index"], xout = peak_data[,"Index"])[["y"]],0)
    } else if(ptab_mode=="index") peak_data <- cbind.data.frame(peak_data, chrom_data[match(peak_data[,"Index"], chrom_data[,"Index"]),chrom_vars]) #chrom_data[,"Index"] %in% peak_data[,"Index"] doesn't work since it sorts the matches!

    #Turn the 'Group' column into a factor and replace colour and group names
    #Also create a colour vector for peak label text
    oldnames <- c("apex","inf","ups","bound","hlt")
    newnames <- c("Apex", "Inflection", "Upslope", "Boundary","Highlight")
    for(i in seq_along(oldnames)) peak_data[peak_data[,"Group"]==oldnames[i],"Group"] <- names(cols)[names(cols) %in% oldnames[i]] <- newnames[i]

    #Colour vector for peak label text
    text_colvec <- peak_data[,"Group"]
    oldvals <- unique(text_colvec)
    newvals <- unname(cols[oldvals])
    text_colvec[text_colvec %in% oldvals] <- newvals[match(text_colvec, oldvals, nomatch = 0)]

    peak_data[,"Group"] <- factor(peak_data[,"Group"], levels = sort(unique(peak_data[,"Group"])))
    cols <- cols[order(names(cols))]

    #Set up peak labels threshold
    if(is.numeric(lablim)) {
      relvec <- if(norm_chrom) peak_data[,chrom_vars[2]] else chrom_data[peak_data[,"Index"],chrom_vars[2]]/max(chrom_data[,chrom_vars[2]], na.rm = TRUE)*100
      peak_data[which(relvec <= lablim),"Label"] <- ""
    }
  }

  #Set up zoom limits
  xlims <- range(chrom_data[,chrom_vars[1]], na.rm = TRUE)
  if(is.numeric(zoom)) xlims <- zoom else if(any(zoom %in% "auto") & is.data.frame(ptab)) {
    minpeak <- min(peak_data[,chrom_vars[1]])
    maxpeak <- max(peak_data[,chrom_vars[1]])
    minslip <- abs(minpeak-xlims[1])*0.25
    maxslip <- abs(maxpeak-xlims[2])*0.25
    xlims <- c(minpeak-minslip, maxpeak+maxslip)
  }

  #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
  aes_labvar <- "Index"

  #Create base plot
  plotres <- ggplot(data = chrom_data, aes(x = .data[[chrom_vars[1]]], y = .data[[chrom_vars[2]]], label1 = .data[[aes_labvar]])) +
    geom_path(col = cols["main"], lty = 1, lwd = 0.6) +
    {if(is.numeric(amp_line)) geom_hline(yintercept = amp_line, colour = cols["ampline"], lty = 2, lwd = 0.5)} +
    labs(x = chrom_vars[1], y = chrom_vars[2], title = plot_title) +
    scale_x_continuous(breaks = breaks_pretty(n = 6)) +
    scale_y_continuous(breaks = breaks_pretty(n = 6)) +
    coord_cartesian(xlim = xlims) +
    theme(aspect.ratio = asprat,
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 11),
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 11),
          legend.position = "bottom")

  #Add peaks
  if(is.data.frame(ptab)) {
    colscale_labs <- sort(levels(peak_data[,"Group"]))
    colscale_vals <- cols[order(names(cols) %in% colscale_labs)]
    colscale_vals <- cols[names(cols) %in% colscale_labs]

    #Define vars for new ggplot3.4.0 aes (after aes_string deprecation)
    aes_grpvar <- "Group"

    plotres <- plotres +
      geom_point(data = peak_data, aes(col = .data[[aes_grpvar]]), pch = 16, size = 1.7, na.rm = TRUE) +
      geom_text(data = peak_data, label = peak_data[,"Label"], col = text_colvec, size = 3, na.rm = TRUE,
                #nudge_x = -0.005*max(peak_data[,chrom_vars[1]], na.rm = TRUE),
                nudge_y = 0.03*max(peak_data[,chrom_vars[2]], na.rm = TRUE),
                check_overlap = if(is.na(lablim)) TRUE else FALSE) +
      scale_colour_manual(name = "",
                          values = colscale_vals,
                          labels = colscale_labs)
  }
  if(draw) print(plotres)
  return(plotres)
}
