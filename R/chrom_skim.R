#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Integrate data using traditional methods
#Incl. Perpendicular Drop, Tangent, Exponential, or Gaussian Skim suitability and Flat/Linear Baseline Calculation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Traditional peak integration of chromatographic data
#'
#' @description Creates baseline-resolved, perpendicular drop, and/or various skimmed baselines (tangent/exponential/Gaussian) and
#' integrates peaks detected via \code{\link{chrom_detect}} via the Trapezoidal Rule. Results may be optionally visualized.
#' Also see \strong{Details}.
#'
#' @param input The output object of function \code{\link{chrom_detect}}.
#' @param method The preferred integration/skimming method. One of: perpendicular drop (\code{"pdrop"}), tangent skim (\code{"tskim"}),
#' exponential skim (\code{"exskim"}), or Gaussian skim (\code{"gskim"}).
#' @param skim The \code{numeric} Skim Ratio criterion to determine suitability of a parent-child peak pair for skimming.
#' Defaults to \code{10} (see \strong{Details}).
#' @param dyson The \code{numeric} Dyson criterion to determine suitability of a parent-child peak pair for skimming.
#' Defaults to \code{10} (see \strong{Details}).
#' @param crit_w The critical width parameter used to calculate peak group baselines via \code{\link{fastchrom_bline}}.
#' @param asprat Aspect ratio of the plot (plotted via \code{\link{integ_plot}}). Defaults to \code{0.71}.
#' @param plotset A \code{character} string specifying whether data is visualized/shown via \code{\link{integ_plot}}.
#' One of \code{"make"} (generates plots without printing; default), \code{"print"} (generates and prints plots), or \code{"none"}.
#'
#' @return A \code{list} containing various data in the following named elements:
#' \describe{
#' \item{orig_data}{A \code{data.frame} containing the original data indices (\code{"ind"}), retention times (\code{"x"}),
#' as well as the raw and baseline-corrected signal (\code{"orig_y"} and \code{"y"}, respectively).}
#' \item{indiv_bln}{A \code{data.frame} containing perpendicular drop and/or skimmed baselines for each individual peak.
#' The data includes indices (\code{"ind"}), retention time (\code{"x"}), baseline-corrected signal (\code{"y"}),
#' peak group ID (\code{"group"}), those of parent and child peaks (\code{"peak1"} and \code{"peak2"}),
#' integration type (\code{"type"}), and a composite ID unique for each baseline (\code{"combo_id"}).}
#' \item{grp_bln}{A \code{data.frame} containing peak group baselines (one baseline per group). Data include
#' group indices (\code{"group"}), data indices (\code{"ind"}), retention times (\code{"x"}),
#' original and baseline-corrected signals (\code{"orig_y"} and \code{"y"}), baselines (\code{"bline"}),
#' and second derivatives (\code{"sd"}).}
#' \item{integ_res}{The \strong{main results} \code{data.frame} containing, among other general data such as retention time and signal,
#' the integrated peak areas (\code{"pa"}) and results of testing for skim suitability via 7 conditions outlined in \strong{Details}
#' (\code{"true_conds"} and \code{"failed_conds"}).}
#' \item{max_marks}{A \code{data.frame} containing accurate peak maxima retention time and signals (see \code{\link{acc_max}}) among
#' other general data such as retention time and signal.}
#' \item{information}{A \code{character} string of various statements about baseline calculation and integration results.}
#' \item{call}{The function call.}
#' \item{plot}{An \strong{optional} element containing the \code{ggplot} visualisation of results (see \code{\link{integ_plot}}).}
#' }
#'
#' @export
#'
#' @details
#' The workflow assesses peaks based on their resolution. First, baselines for fully-resolved peaks are calculated via the FastChrom
#' algorithm (see \code{\link{fastchrom_bline}} and Johnsen et al., 2013). Common baselines for groups of \strong{un}resolved peaks
#' are also derived in the same manner. Boundaries between each pair of unresolved peaks are then either separated by a simple
#' Perpendicular Drop baseline when \code{method = "pdrop"}. Perpendicular Drop tends to increasingly distort peak areas with
#' decreasing resolution (see Asher et al., 2009). Alternatively, peaks boundaries are tested for suitability for one of three
#' "skimmed" baseline approaches - tangent skim (\code{method = "tskim"}), exponential skim (\code{"exskim"}), or Gaussian skim
#' (\code{"gskim"}) - by assessing a series of conditions (Agilent Technologies, 2016; Water Corporation, 2017):
#' \enumerate{
#' \item Is the peak boundary classified as either fused or a shoulder?
#' \item Is at least one peak within the assessed pair classified as fused or a shoulder?
#' }
#' If either of these initial conditions are false, no skim is attempted. Otherwise, additional conditions are evaluated:
#' \enumerate{
#' \item Is the earlier-occurring peak the parent peak or a child peak (decided based on the height of peak maxima)?
#' This determines whether to apply a \strong{front} or a \strong{tail} skim.
#' \item Is the apex of the child peak lower than the closest (along the x-axis) inflection point of the parent peak?
#' If this conditions is false, \strong{exponential or Gaussian skimming are not attempted}.
#' \item Is the Skim-Valley ratio (child peak height over that of the inter-peak boundary/valley) lower than the threshold set in \code{skim}?
#' \item Is the height ratio of the parent peak to the child peak higher than the criterion set in \code{dyson}?
#' If either of the conditions dependent on \code{skim} and \code{dyson} are false, \strong{skimming is not carried out}.
#' \item Is the outer boundary of the child peak higher than the inter-peak boundary? If true, \strong{tangent skimming is not possible}.
#' }
#' These conditions also determine the identity of the parent and child peak as well as the appropriate skim type
#' (\strong{front} or \strong{tail}) for each peak pair. Thus, if a boundary is determined to be suitable based on
#' the above conditions, construction and optimization of a skimmed baseline of the type specified in \code{method} is attempted.
#'
#' For \strong{front} tangent skim off the parent peak, straight lines are drawn from the inter-peak boundary to each point between
#' the beginning and the maximum of the child peak (which occurs earlier in this case). For a \strong{tail} tangent skim, lines are
#' instead drawn between the inter-peak boundary and each point between the maximum and end of the child peak (which is now the later-occurring peak).
#' Lines whose y-values are >2% of the maximum child peak signal at any point are discarded. Among the rest, the line whose end-point
#' is closest to the outer boundary of the child peak and where <40% of the values are within 1% of the corresponding chromatographic signal
#' value is selected.
#'
#' For exponential skim, the following equation is used to build an exponential curve extending from the inflection point of the
#' parent peak closest to the inter-peak boundary towards either the start (for \strong{front} skim) or end (for \strong{tail} skim)
#' of the child peak.
#' \deqn{H_{ex} = H_0\times exp^{(-B\times(t_R-t_0))} + A\times t_R + C}
#' Where \eqn{H_{ex}} is the exponential curve value, \eqn{H_0} is the height (signal) at the inter-peak boundary (e.g. valley),
#' \eqn{B} is the exponential growth/decay function (negative for \strong{tail} skim), \eqn{A} is the slope of the parent peak baseline,
#' \eqn{C} is the baseline offset of the parent peak, and \eqn{t_R} along with \eqn{t_0} are retention times at \eqn{H_b} and the
#' inter-peak boundary, respectively. The initial exponential curve is constructed with values of \eqn{B} and \eqn{C} both set to \code{0}.
#' The offset constant \eqn{C} is then determined by the different between the result and the signal at the inter-peak boundary,
#' and constant \eqn{B} of the exponential fit is optimized for minimum Euclidean Distance between the curve and original chromatographic
#' signal \strong{in the parent peak region} (spanning from the closest inflection point to the inter-peak boundary). Finally, the
#' final exponential curve spanning from the inter-peak boundary to the outer boundary of the child peak is plotted using optimized
#' constants \eqn{B} and \eqn{C}. This is used as the skimmed baseline.
#'
#' For Gaussian skim, the following general form of the Gaussian curve is used to construct a model between the apex of the \strong{parent}
#' peak and the inter-peak boundary.
#' \deqn{H_{gs} = H_p\times exp^{-(\frac{t_R-t_0}{\sigma})^2}}
#' Where \eqn{H_{gs}} is the Gaussian curve value, \eqn{H_p} is the parent peak maximum signal, \eqn{t_0} is the corresponding retention
#' time, \eqn{t_R} is the retention time at \eqn{H_{gs}}, and \eqn{\sigma} is the standard deviation of the Gaussian curve (estimated here
#' as the half-width of the peak at inflection points). The Gaussian curve is iteratively optimized until Euclidean Distance between the
#' resulting curve and original parent peak is minimized. The final curve is then constructed using optimized parameters between the
#' \strong{parent} peak apex and the outer boundary of the \strong{child} peak. The model is then checked for two conditions:
#' \enumerate{
#' \item Is the lowest point of the Gaussian curve higher than 1% of the parent peak maximum?
#' \item Are any points of the curve in the \strong{child} peak region above the original signal (i.e. does the curve cross the chromatogram)?
#' }
#' If either of the above is true, the Gaussian curve is rejected, no further skim is attempted, and a Perpendicular Dropline is instead
#' constructed. Otherwise, the Gaussian curve is truncated from the \strong{parent} peak maximum until the point where the curve is
#' of \strong{consistently} lower signal (i.e. height) than the original chromatogram.
#'
#' Once all the baselines are constructed, the Trapezoidal Rule is used to integrate all peaks and calculate their peak areas (\eqn{PA}).
#' \deqn{PA = \sum{(x_{i+1}-x_i)\times(y_{i+1}+y_i)/2}}
#' Results are optionally visualized via \code{\link{integ_plot}}.
#'
#' @references
#' Agilent Technologies (2016), 'Agilent OpenLAB CDS Data Analysis Reference Guide', document M8410-90031, available at: \url{https://www.agilent.com/cs/library/usermanuals/public/DataAnalysisReferenceGuide.pdf}.
#'
#' Asher, B.J., D'Agostino, L.A., Way, J.D., Wong, C.S., Harynuk, J.J. (2009), 'Comparison of peak integration methods for the determination of enantiomeric fraction in environmental samples', \emph{Chemosphere} \strong{75} (8), pp. 1042-1048, DOI: \url{https://doi.org/10.1016/j.chemosphere.2009.01.041}.
#'
#' Dyson, N. (1998), \emph{Chromatographic Integration Methods}, The Royal Society of Chemistry (RSC Chromatography Monographs series), Herefordshire, United Kingdom.
#'
#' Johnsen, L.G., Skov, T., Houlberg, U., Bro, R. (2013), 'An automated method for baseline correction, peak finding and peak grouping in chromatographic data', \emph{Analyst} \strong{138} (12), pp. 3502-3511, DOI: \url{https://www.doi.org/10.1039/C3AN36276K}.
#'
#' Kalambet, Y., Kozmin, Y., Samokhin, A. (2018), 'Comparison of integration rules in the case of very narrow peaks', \emph{Chemometrics and Intelligent Laboratory Systems} \strong{179}, pp. 22-30, DOI: \url{https://doi.org/10.1016/j.chemolab.2018.06.001}.
#'
#' SERAS (2000), 'Standard Operating Procedure 1001: Chromatographic Peak Integration Procedures', U.S. EPA Contract EP-W-09-031, available at: \url{https://clu-in.org/download/ert/1001-r00.pdf} (accessed 22.04.2024).
#'
#' Waters Corporation (2017), 'Empower Software Data Acquisition and Processing Theory Guide', document 715005481 (Rev. A), available at: \url{https://support.waters.com/KB_Inf/Empower_Breeze/WKB57375_Empower_3_-_How_to_acquire_and_process_data (accessed 19.04.2024)}.
#'
#' @examples
#' \dontrun{
#' #Get data and integrate via Perpendicular Drop
#' det_peaks <- lcqc:::wf_detpeaks
#' chrom_skim(det_peaks, method = "pdrop")
#'
#' #Forced exponential skim
#' chrom_skim(det_peaks, method = "exskim", dyson = 1.5, skim = NA)
#' }
#'
#' @seealso
#' This workflow uses a multitude of exported and \strong{un}exported functions:
#'\describe{
#' \item{Exported}{\code{\link{dtprep}}, \code{\link{fastchrom_bline}}, \code{\link{integ}}, \code{\link{integ_plot}}}
#' \item{Unexported}{\code{\link{chkdt}}, skimming functions (\code{\link{tskim}}, \code{\link{exskim}}, \code{\link{gskim}}) and their helper functions, \code{\link{pdrop}}, \code{\link{skimcomp}}}
#'}
chrom_skim <- function(input, method = "gskim", skim = 10, dyson = 10, crit_w = "auto", asprat = 0.71, plotset = "make") {

  #Preliminary checks
  input <- chkdt(input)

  #if(!is.list(input) | !all(c("type_df", "grp_df", "grp_blines", "acc_tops") %in% names(input))) {
  #  stop("Input data must be a list output from function 'dtprep'!")
  #}

  if(!any(method %in% c("pdrop", "tskim", "exskim", "gskim"))) {
    stop("Integration skim type must be one of: 'pdrop', 'tskim', 'exskim', or 'gskim'!")
  }

  #Generate function call
  cl_rec <- match.call()

  #Pre-process input data
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  acc <- input[["acc_tops"]]
  bln <- input[["grp_blines"]]
  gdt <- input[["grp_df"]]
  tdt <- input[["type_df"]]
  main <- input[["main_df"]]

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
  integ_res <- data.frame(matrix(NA, ncol = 12, nrow = nrow(acc)))
  colnames(integ_res) <- c("group", "peak", "start_rt", "apex_rt", "acc_apex_rt", "end_rt", "integ_lb", "integ_rb", "integ_type", "pa", "true_conds", "failed_conds")
  integ_res[,c("group","peak", "start_rt", "apex_rt", "acc_apex_rt", "end_rt")] <- list(grp_inds, peak_inds, str[,"x"], maxs[,"x"], maxs[,"acc_x"], end[,"x"])
  #Add compound names (if any)
  if(any(colnames(acc) %in% "Compound")) {
    integ_res <- cbind.data.frame(Compound = acc[,"Compound"], integ_res)
    maxs <- cbind.data.frame(Compound = acc[,"Compound"], maxs)
  }

  #List for skim/PD baselines
  skimlns <- list()

  #Create initial peak signal data list
  cat("\nReading detected peak list...")
  integ_list <- lapply(peak_inds, function(x) bln[bln[,"ind"] %in% str[x,"ind"]:end[x,"ind"], c("ind","x","y")])

  cat("\nProcessing peak groups and baseline-resolved peaks...")
  for(i in grps) {
    #Determine whether the number of peaks belonging to the current group exceeds 1
    pks <- peak_inds[grp_inds %in% i]
    grp_cond <- length(pks)>1

    #Process groups of peaks first
    if(grp_cond) {
      #Loop through PAIRS of peaks
      for(j in pks[-length(pks)]) {

        #Create a vector for outputting condition results
        cond <- rep(FALSE, 7)

        #Conditionally set leftmost and rightmost integration boundary types within each peak group to "B" (i.e. baseline-resolved peak integration)
        if(j==pks[1]) integ_res[j, "integ_lb"] <- "B"
        if(j %in% pks[c(length(pks)-1, length(pks))]) integ_res[j+1, "integ_rb"] <- "B"

        #Define other variables (prefix 'p' denotes PAIRED values for peaks j and j+1)
        pstr <- str[c(j,j+1),]
        pend <- end[c(j,j+1),]
        plin <- lin[c(j,j+1),]
        prin <- rin[c(j,j+1),]
        pmaxs <- maxs[c(j,j+1),]

        pspan <- lapply(seq_along(pstr[,"ind"]), function(x) pstr[x,"ind"]:pend[x,"ind"])
        tspan <- pstr[1,"ind"]:pend[2,"ind"]
        ppks <- list(bln[bln[,"ind"] %in% pspan[[1]],c("ind", "x", "y", "bline")], bln[bln[,"ind"] %in% pspan[[2]],c("ind","x","y", "bline")])

        #Run an extra check to confirm peaks j and j+1 belong to the same group. Else abort the function.
        if(pstr[2,"ind"]!=pend[1,"ind"]) stop(paste0("Peaks ", j, " and ", j+1, " do not belong to the same group! Aborting..."))

        #Determine appropriateness of skim (Tangent, Exponential, or Gaussian)
        #Check that peak boundary is either "F" or "S"
        bnd_tp <- unique(c(pstr[2,"type"],pend[1,"type"]))
        cond[1] <- length(bnd_tp)==1 & any(c("F","S") %in% bnd_tp[1])

        #Check that at least one of the peaks within the j, j+1 pair is "F" or "B"
        cond[2] <- any(pstr[,"ptype"] %in% c("B", "F"))

        #If cond1 or cond2 is FALSE, abort the skim.
        if(!all(c(cond[1], cond[2])) | method == "pdrop") {

          #Create Perpendicular Drop condition, which triggers standard drop line integration when TRUE
          pd_cond <- TRUE

          #ELSE, carry on with additional checks
        } else {

          pd_cond <- FALSE

          #Determine which peak is taller among j and j+1 and assign them as parent (h1) and child (h2).
          h1 <- which.max(pmaxs[,"acc_y"])
          cond[3] <- h1==1 #If the first peak is taller (and hence is the parent peak h1), cond[3] is TRUE
          h2 <- if(cond[3]) 2 else 1

          #Assign required Skim Type
          skim_type <- if(cond[3]) "tail" else "front"

          #Check that the child peak apex is below the neighbouring peak's closest inflection point
          #If peak j is taller, check that the apex of peak h2 is below the right inflection point of peak h1
          # ELSE if peak j+1 is taller, check that the apex of peak h2 is below the left inflection points of peak h1
          cond[4] <- pmaxs[h2,"acc_y"] < if(cond[3]) prin[h1, "acc_y"] else plin[h1, "acc_y"]

          #Assess peak height criteria
          #Retrieve valley height
          valh <- pend[1,"y"]

          #Optionally assess the Skim-Valley Ratio criterion
          #(If valley is lower than threshold, that means the maximum of the child peak is sufficiently close in value to the valley height, and skimming is appropriate)
          #(thus, skimming can be forced by setting skim to a high value, while a value closer to 0 will force a pdrop integration instead)
          skim_rat <- pmaxs[h2,"acc_y"]/valh
          if(!is.na(skim)) {
            cond[5] <- skim_rat < skim
          } else cond[5] <- TRUE

          #Assess the Dyson criterion
          #(Setting a low value will force a skim, while a high value will force a pdrop)
          dyson_rat <- if(skim_rat < 1) pmaxs[h1,"acc_y"] / pmaxs[h2,"acc_y"] else (pmaxs[h1,"acc_y"]-valh) / (pmaxs[h2,"acc_y"]-valh)
          cond[6] <- dyson_rat > dyson

          #Check whether boundaries are suitable for skims
          #If Skim Type is "front", check whether the LEFT BOUNDARY is either "R" or taller than/equal to the current boundary
          #ELSE if Skim Type is "tail", check whether the RIGHT BOUNDARY is either "R" or taller than/equal to the current boundary
          cond[7] <- if(skim_type == "front") pstr[1,"y"] >= valh | pstr[1,"type"] == "R" else if(skim_type == "tail") pend[2,"y"] >= valh | pend[2,"type"] == "R"

          #Correct any NAs in 'cond' to FALSE
          cond[is.na(cond)] <- FALSE

          #Ascertain appropriateness of various skim types
          #Abort skim if:
          #A) Either cond5 or cond6 (the peak height ratio criteria) is FALSE.
          #B) If Skim Type is "front" and the LEFT INTEGRATION BOUNDARY of peak j is a tail skim ("TT", "ET", or "GT").
          #C) Tangent Skim is not carried out if cond7 is TRUE.
          #D) Exponential or Gaussian skims are not carried out if cond4 is FALSE.
          if(!all(c(cond[5], cond[6])) |
             skim_type == "front" & any(c("TT", "ET", "GT") %in% integ_res[j,"integ_lb"]) |
             (cond[7] & method=="tskim") |
             (!cond[4] & any(c("exskim", "gskim") %in% method))) {
            pd_cond <- TRUE
          }
        }

        #Check if the condition forcing perpendicular drop integration is TRUE
        if(!pd_cond) {

          cat("\nInitial conditions met! Attempting ", skim_type, " ", method, " on peaks ", j, " and ", j+1, "...", sep = "")

          #Carry out the chosen type of skim (Tangent, Exponential, or Gaussian)
          if(method=="tskim") {
            skimout <- tskim(child = ppks[[h2]], cmax = pmaxs[h2,], skim_type = skim_type)
          } else if(method=="exskim") {
            skimout <- exskim(peaks = ppks, maxs = pmaxs, lin = plin, rin = prin, skim_type = skim_type)
          } else if(method=="gskim") {
            skimout <- gskim(peaks = ppks, maxs = pmaxs, lin = plin, rin = prin, skim_type = skim_type)
          }

        } else if(pd_cond) {

          #Else, create a simple perpendicular drop baseline
          skimout <- pdrop(str = pstr, end = pend)
        }

        #Assign individual peak baseline (skim | pd) to output list
        skimlns[[j]] <- skimout

        #Update the perpendicular drop condition
        if(unique(skimout[,"type"])=="PD") pd_cond <- TRUE

        if(pd_cond) {

          cat("\nPeak pair: ", j, " and ", j+1, " was integrated using Perpendicular Drop...", sep = "")

          #Assign boundary types and integration type as "PD"
          integ_res[j, c("integ_rb","integ_type")] <- integ_res[j+1,c("integ_lb", "integ_type")] <- "PD"
          #There is no need to modify the integ_list values in cases where no skimming was performed!

          #Assign the baseline-resolved boundary type ("B") if peak is either the first or last of a group
          if(j==pks[1]) integ_res[j,"integ_lb"] <- "B" else if(j+1==pks[length(pks)]) integ_res[j+1,"integ_rb"] <- "B"

        } else if(!pd_cond) {

          cat("\nPeak pair: ", j, " and ", j+1, " was integrated using a ", skim_type, " ", method, "!", sep = "")

          #Assign integration boundary type and integration type
          integ_res[j,"integ_rb"] <- integ_res[j+1,"integ_lb"] <- unique(skimlns[[j]][,"type"])
          bnd_prefix <- substr(unique(skimlns[[j]][,"type"]),1,1)
          integ_res[j,"integ_type"] <- paste0(bnd_prefix, "S")
          if(j+1==pks[length(pks)]) integ_res[j+1,"integ_type"] <- paste0(bnd_prefix, "S")
        }

        #Assign results of conditions to output data.frame
        cond <- sapply(cond, function(x) if(x==0) FALSE else TRUE)
        integ_res[j,"true_conds"] <- if(length(which(cond))==0) "none" else paste0(which(cond), collapse = ",")
        integ_res[j,"failed_conds"] <- if(length(which(!cond))==0) "none" else paste0(which(!cond), collapse = ",")
        if(j+1==pks[length(pks)]) integ_res[j+1,c("true_conds","failed_conds")] <- "last_in_group"

      }

    } else if(!grp_cond) {
      #Process single peaks next
      #Append integration boundary type as "B"
      cat("\nPeak ", pks, " is baseline-resolved! No skim or perpendicular drop necessary...", sep = "")
      integ_res[pks, c("integ_lb", "integ_rb", "integ_type")] <- "B"
      integ_res[pks,c("true_conds", "failed_conds")] <- "resolved_peak"
    }
  }

  if(length(skimlns)>0) {
    #Compile data into a individual baselines data.frame
    skimlns <- Reduce(rbind.data.frame, skimlns)

    #Update integration data according to the calculated skim | pdrop baselines
    integ_list <- skimcomp(input = skimlns, integ_data = integ_list)
  }

  #Integrate data via the Trapezoidal Rule to calculate PA
  integ_res[,"pa"] <- integ(integ_list)

  #Compile information about function
  skim_nms <- c(pdrop = "Perpendicular Drop (PD)", tskim = "Tangent Skim (TS)", exskim = "Exponential Skim (ES)", gskim = "Gaussian Skim (GS)")
  fused_pknum <- length(which(grp_inds %in% grp_inds[duplicated(grp_inds)]))
  resolved_pknum <- length(peak_inds)-fused_pknum
  information <- paste0("Baselines for ", resolved_pknum," resolved and ", fused_pknum, " fused peaks spread across ", length(which(duplicated(grp_inds))), " groups were determined via the FastChrom algorithm.",
                        "\n", skim_nms[method], " was selected as the peak skimming method.",
                        "\nA critical width equal to ", ifelse(is.numeric(crit_w), crit_w, " the smallest inflection point distance"), " was used for baseline adjustment in the peak regions.",
                        "\nThe Skim-Valley Ratio ('skim') and Dyson (Parent-to-Child Peak Ratio; 'dyson') criteria were set at ", skim, " and ", dyson, ", respectively.",
                        "\nPeaks were integrated using the Trapezoidal Rule.")

  #Compile all results
  totres <- list(orig_data = main, indiv_bln = skimlns, grp_bln = bln, integ_res = integ_res, max_marks = maxs, information = information, call = cl_rec)

  #Visualize the results
  if(plotset!="none") {
    totres[["plot"]] <- intplot <- integ_plot(input = totres, plot_max = TRUE, txt_max = TRUE, asprat = asprat)
    if(plotset=="print") print(intplot)
  }

  #Return the results
  return(totres)
}
