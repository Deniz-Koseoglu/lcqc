#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Exporting plots and data to a directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Compile and export LCQC data and visualizations
#'
#' @description Compiles all key data and visualizations obtainable via various \pkg{lcqc} functions and exports to disk.
#'
#' @param input_list A \strong{named} \code{list} of data to be exported. Must contain \strong{one or more} of the following elements:
#' \describe{
#' \item{pks}{Peak detection data output from \code{\link{chrom_detect}}.}
#' \item{acctops}{Accurate peak apices and inflection points obtained by running \code{\link{dtprep}} on \code{\link{chrom_detect}} output.}
#' \item{int}{Traditional peak integration data output from \code{\link{chrom_skim}}.}
#' \item{icf}{Iterative Curve Fitting and integration data output from \code{\link{chrom_icf}}.}
#' \item{visc}{Dynamic viscosity data output from \code{\link{chrom_visc}}.}
#' \item{tp}{Theoretical plates and other metrics output from \code{\link{chrom_tplate}}.}
#' \item{asym}{Asymmetry metrics and Total Peak Analysis (TPA) data output from \code{\link{chrom_asym}}.}
#' \item{rf}{Retention factors output from \code{\link{chrom_retf}}.}
#' \item{sf}{Separation factors output from \code{\link{chrom_sepf}}.}
#' \item{res}{Peak resolution data output from \code{\link{chrom_res}}.}
#' \item{cperf}{Additional performance metrics output from \code{\link{chrom_addmets}}.}
#' }
#' @param expath The \code{character} string of the \strong{directory} in which to create a folder and export the results.
#' @param plotpars A \strong{named} \code{numeric} vector of plotting parameters. \strong{Only relevant when \code{plot_format = "png"}}.
#' One or more of \code{"w"} (width, in cm), \code{"h"} (height, in cm), \code{"psize"} (point size), and \code{"res"} (resolution).
#' When set to \code{"default"}, the set of parameters are \code{c(w = 10, h = 12, psize = 12, res = 300)}.
#' @param plot_format A \code{character} specifying which format to export plots in. One of \code{"png"} or \code{"pdf"}.
#' @param expfun A \code{logical} specifying whether function calls should be exported. Defaults to \code{FALSE}.
#'
#' @return A timestamped folder is created inside \code{expath} containing the compiled results in a
#' \strong{LCQC_Results.csv} file, and .png/.pdf visualizations contained in a separate \strong{Plots} directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #Export to the working directory (visualisations in .PNG)
#' chrom_export(input_list = list(pks = lcqc:::wf_detpeaks,
#' acctops = dtprep(lcqc:::wf_detpeaks),
#' int = lcqc:::wf_ints,
#' icf = lcqc:::wf_icf,
#' visc = lcqc:::wf_viscs,
#' tp = lcqc:::wf_tplate,
#' asym = lcqc:::wf_asyms,
#' rf = lcqc:::wf_kfs,
#' sf = lcqc:::wf_sfs,
#' res = lcqc:::wf_rs,
#' cperf = lcqc:::wf_perfmets))
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{chrom_skim}}, \code{\link{chrom_icf}},
#' \code{\link{chrom_tplate}}, \code{\link{chrom_asym}}, \code{\link{chrom_retf}}, \code{\link{chrom_sepf}},
#' \code{\link{chrom_res}}, \code{\link{chrom_addmets}}, \code{\link{read_shim}}, \code{\link{lcqc_render}}
#'
#' @importFrom grDevices png
#' @importFrom grDevices pdf
#' @importFrom grDevices dev.off
#' @importFrom data.table fwrite
chrom_export <- function(input_list, expath = getwd(), plotpars = "default", plot_format = "png", expfun = FALSE) {

  #Set separator value
  fwsep <- "\t"

  #Preliminary checks
  #Check input data structures
  defnms <- c("pks", "acctops", "int", "icf", "visc", "tp", "asym", "rf", "sf", "res", "cperf")
  if(!all(names(input_list) %in% defnms)) stop(paste0("The following 'input_list' names were not recognized: ", paste0(names(input_list)[-which(names(input_list) %in% defnms)], collapse = ", "), "! Accepted names are: ", paste0(defnms, collapse = ", "), "."))

  checknms <- c()
  names_to_check <- names(input_list)[-which(names(input_list)=="pks")]
  for(i in names_to_check) {
    if(i=="pks") {
      input_list[[i]] <- chkdt(input_list[["pks"]])
      checknms[i] <- FALSE
    } else {
      curnms <- if(i=="int") c("orig_data", "indiv_bln", "grp_bln", "integ_res", "max_marks", "information", "call", "plot")
      else if(i=="acctops") c("main_df", "type_df", "grp_df", "grp_blines", "acc_tops", "peak_list")
      else if(i=="icf") c("main_data", "integ_res", "modplot", "information", "call")
      else if(i=="asym") c("results", "call", "plots", "information")
      else c("results", "t0", "information", "call")
      checknms[i] <- !all(names(input_list[[i]]) %in% curnms)
    }
  }
  if(any(checknms)) stop(paste0("The following input data have incorrect structure: ", paste0("'", names_to_check[checknms], "'", collapse = ", "), "."))

  #Prepare/create export directories
  expath <- paste0(expath, "/", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"))
  invisible(dir.create(expath))

  #Set up plot parameters
  defpars <- c(w = 10, h = 12, psize = 12, res = 300)
  plotpars <- supp_pars(pars = plotpars, defpars = defpars, parlb = "plotpars")

  #Export plots
  plotchk <- any(c("pks", "int", "icf", "asym") %in% names(input_list))

  if(plotchk) {

    cat("Exporting Plots...")
    #Define/create output directory
    expath_plots <- paste0(expath, "/Plots")
    invisible(dir.create(expath_plots))

    for(i in names(input_list)[names(input_list) %in% c("pks", "int", "icf", "asym")]) {
      elname <- if(any(c("pks","asym") %in% i)) "plots" else if(i=="icf") "modplot" else if(i=="int") "plot"
      curplots <- input_list[[i]][[elname]]
      if(!any(class(curplots) %in% "list")) curplots <- setNames(list(curplots), i)

      for(j in seq_along(curplots)) {
        if(plot_format=="png") {
          png(filename=paste0(expath_plots, "/", names(curplots)[j], ".png"),
              type="cairo", units="cm",
              height = plotpars["h"], width = plotpars["w"],
              pointsize = plotpars["psize"], res = plotpars["res"])
        } else if(plot_format=="pdf") {
          pdf(file = paste0(expath_plots, "/", names(curplots)[j], ".pdf"),
              height = plotpars["h"], width = plotpars["w"], pointsize = plotpars["psize"], compress = TRUE, onefile = FALSE)
        }
        print(curplots[[j]])
        dev.off()
      }
    }
  }

  #Export data
  cat("\nExporting Data...")
  #Exporting data to a single .TAB
  fpath <- paste0(expath, "/LCQC_Results.tab")

  #BEGIN EXPORTING DATA
  #Create headings
  headings <- c(pks = "PEAK TABLE (FUNCTION 'chrom_detect')",
                acctops = "ACCURATE INFLECTION POINTS AND PEAK APICES (FUNCTION 'dtprep')",
                int = "TRADITIONAL INTEGRATION (FUNCTION 'chrom_skim')",
                icf = "ITERATIVE CURVE FITTING (FUNCTION 'chrom_icf')",
                visc = "DYNAMIC VISCOSITY (FUNCTION 'chrom_visc')",
                tp = "NUMBER OF THEORETICAL PLATES (FUNCTION 'chrom_tplate')",
                asym = "SYMMETRY METRICS (FUNCTION 'chrom_asym')",
                rf = "RETENTION FACTORS (FUNCTION 'chrom_retf')",
                sf = "SEPARATION FACTORS (FUNCTION 'chrom_retf')",
                res = "RESOLUTION (FUNCTION 'chrom_res')",
                cperf = "COLUMN-SPECIFIC PERFORMANCE METRICS (FUNCTION 'chrom_addmets')")
  #funcnames <- gsub("^.*\\(FUNCTION '(.*)\\')", "\\1", headings)

  for(i in names(input_list)) {
    #Export heading
    cat(headings[i], "\n", file = fpath, append = TRUE)
    #Export information about procedure carried out by function
    if(i!="acctops") cat(ifelse(i=="pks", input_list[[i]][["results"]][["information"]], input_list[[i]][["information"]]), "\n", file = fpath, append = TRUE)
    cat("\n", file = fpath, append = TRUE)

    #Function-specific exports
    if(i=="pks") {
      fwrite(input_list[[i]][["results"]][["Peak_Extents"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
    } else if(i=="acctops") {
      fwrite(input_list[[i]][["acc_tops"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
    } else if(i=="int") {
      cat("Peak List with Areas and Boundary Types (Traditional Integration)\n", file = fpath, append = TRUE)
      fwrite(input_list[[i]][["integ_res"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
      cat("Accurate and baseline-corrected peak maxima\n", file = fpath, append = TRUE)
      fwrite(input_list[[i]][["integ_res"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
    } else if(i=="icf") {
      cat("Peak List with Areas and Model Types (Iterative Curve Fitting)\n", file = fpath, append = TRUE)
      fwrite(input_list[[i]][["integ_res"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
    } else if(i=="visc") {
      for(j in names(input_list[[i]][["results"]])) {
        cat(j, "\n", file = fpath, append = TRUE)
        fwrite(as.data.frame(t(input_list[[i]][["results"]][[j]])), file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      }
      replicate(2, cat("\n", file = fpath, append = TRUE))
    } else {
      fwrite(input_list[[i]][["results"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
      replicate(2, cat("\n", file = fpath, append = TRUE))
    }
  }

  #EXPORT FUNCTION CALLS (OPTIONALLY)
  if(expfun) {
    cat("FUNCTION CALLS\n", file = fpath, append = TRUE)
    for(i in names(input_list)[!names(input_list) %in% "acctops"]) {
      #cat(funcnames[i], "\n", file = fpath, append = TRUE)
      curcall <- if(i=="pks") input_list[[i]][["results"]][["call"]] else input_list[[i]][["call"]]
      #Collapse the function call
      curcall <- paste0(format(curcall), "\n", collapse = "") #Can use 'deparse()' in place of 'format()'
      cat(curcall, "\n", file = fpath, append = TRUE)
    }
  }

  #FINALLY, EXPORT CHROMATOGRAM DATA (IF AVAILABLE)
  if("pks" %in% names(input_list)) {
    cat("\nCHROMATOGRAM (PROCESSED FOR PEAK DETECTION)\n", file = fpath, append = TRUE)
    fwrite(input_list[["pks"]][["results"]][["Chromatogram"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
  } else if("int" %in% names(input_list)) {
    cat("\nCHROMATOGRAM (PROCESSED FOR INTEGRATION)\n", file = fpath, append = TRUE)
    fwrite(input_list[["int"]][["orig_data"]], file = fpath, sep = fwsep, row.names = FALSE, col.names = TRUE, append = TRUE)
  }
}
