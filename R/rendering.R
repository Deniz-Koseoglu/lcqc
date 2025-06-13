#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Render a .PDF of key data from 'chrom_skim', 'chrom_tplate', and 'chrom_asym'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION A: Show definitions of text parameters to 'lcqc_render'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname lcqc_render
render_defs <- function() {
  return(setNames(data.frame(list(Parameter = c("sn", "pn", "desc", "bn", "dnum", "oper", "mp", "sp", "bp", "flow", "temp", "inj", "unit_c", "tform"),
                                  Name = c("Serial Number", "Part Number", "Description", "Batch Number", "Document Number", "Operator", "Mobile Phase",
                                           "Storage Phase", "Back Pressure", "Flow Rate", "Column Temperature", "Injection Volume", "Concentration Unit", "Time (Formatted)"),
                                  Example = c("C18-2024000001", "L-C182546510", "AC-C18 Column\n(250x4.6 mm, 5 micron, 10 nm)", "20240000001", "LC-C18-20240000001",
                                              "Deniz Can Koseoglu", "70:30 MeOH:Water", "80:20 (volumetric) IPA:Water", "126 bar", "1.0 mL/min", "30 degC", "20 microL",
                                              "ppm", "Apr 14, 2024 (01:19:04)"))), c("Parameter", "Name", "Example")))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION B: Render a .PDF report of key HPLC column QC results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Render LCQC data into a concise HPLC column performance report
#'
#' @description Compiles key data obtained from the \pkg{lcqc} workflow into a
#' modular HPLC Column Performance Report in .PDF format. The function utilizes
#' the \strong{Quarto} framework for R.
#'
#' @param tpl The output of function \code{\link{chrom_tplate}} containing theoretical plates and other metrics.
#' @param cplot A \code{ggplot} of the chromatogram to be included in the report.
#' Usually obtained via the \code{\link{chrom_skim}} or \code{\link{chrom_icf}} workflows.
#' @param tpars A \strong{named} \code{character} vector specifying various general text to be included in the report.
#' Values may include (also run \code{\link{render_defs}} for examples):
#' \enumerate{
#' \item Column serial number \code{"sn"}
#' \item Column part number \code{"pn"}
#' \item Short column description \code{"desc"}
#' \item Batch number \code{"bn"}
#' \item Document number \code{"dnum"}
#' \item Operator name and surname \code{"oper"}
#' \item Mobile phase specification \code{"mp"}
#' \item Shipment phase \code{"sp"} (which solvent is the column shipped in?)
#' \item Back pressure \code{"bp"}
#' \item Flow rate \code{"flow"}
#' \item Column temperature \code{"temp"}
#' \item Injection volume \code{"inj"}
#' \item Analyte concentration unit \code{"unit_c"}
#' \item Timestamp for the report genesis \code{"tform"} (\code{"auto"} by default)
#' }
#' @param clogo A \code{character} filepath to a custom \strong{.JPG} logo to include in the report header.
#' A placeholder is displayed if no logo is provided.
#' @param sym \strong{Optional} asymmetry metrics and Total Peak Analysis output from function \code{\link{chrom_asym}}.
#' @param ext \strong{Optional} additional column-specific metrics output from function \code{\link{chrom_addmets}}.
#' @param spec A \strong{named} \code{list} of QC specifications to display in the report. Names \strong{must match}
#' the elements of \code{mets}.
#' @param mets A \code{character} vector of theoretical plate (\emph{N}) and asymmetry metrics to display in the report.
#' Possible metrics are: European Pharmacopoeia \emph{N} (\code{"EP"}), Area-Height \emph{N} (\code{"AH"}),
#' sigma-5 \emph{N} (\code{"S5"}), Exponentially-Modified Gaussian \emph{N} (\code{"EMG"}), Asymmetry Factor (\code{"As"}),
#' and/or USP/EP Tailing Factor (\code{"Tf"}).
#' @param exmets \strong{Optional} \code{character} vector of \strong{additional} column-specific metrics to display
#' at the bottom of the first page. \strong{Only relevant if} \code{ext} is provided.
#' Possible values are \code{"Linear Velocity"}, \code{"Packing Porosity"}, \code{"Flow Resistance"}, and/or \code{"Permeability"}.
#' @param add_tpa A \code{logical} switch indicating whether to add TPA plots to the second page of the report
#' (defaults to \code{FALSE}).
#' @param expath A \code{character} directory path into which to export the .PDF report.
#' @param asprat Aspect ratio of the chromatogram plot (\code{0.4} by default).
#' @param pnms Either \code{NA} (default) or a \code{character} vector of peak names to include in the report.
#' Must be equal in length to the total number of peaks in the input data, or to \code{which_pks} if these are specified.
#' @param pconcs Either \code{NA} (default) or a vector of concentrations corresponding to each peak included in the data,
#' or to those specified in \code{which_pks}.
#' @param add_pnums A \code{logical} switch specifying whether to add peak index numbers to the PDF report \strong{when \code{pnms} are given}.
#' Defaults to \code{TRUE}.
#' @param fontsize The font size to use for main text of the report (\code{10} by default).
#' @param which_pks Either \code{"all"} (default) or a \code{numeric} vector of peak indices to include in the report.
#'
#' @return The column performance report .PDF file is exported to the directory specified in \code{expath}.
#'
#' @references
#' Allaire, J.J., Teague, C., Scheidegger, C., Xie, Y., Dervieux, C., (2024), \emph{Quarto, version 1.4}, DOI: \url{https://www.doi.org/10.5281/zenodo.5960048}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #View available definitions
#' render_defs()
#'
#' #Render .PDF report into the working directory
#' lcqc_render(tpl = lcqc:::wf_tplate,
#' cplot = lcqc:::wf_ints[["plot"]], #ALWAYS remove title and legend from plot
#' sym = lcqc:::wf_asyms,
#' ext = lcqc:::wf_perfmets,
#' which_pks = 4:7,
#' pnms = c("Naphthalene", "Anthracene", "Caffeine", "Benzene"),
#' pconcs = c(250, 500, 250, 500),
#' mets = c("EP", "AH", "Tf", "As"),
#' exmets = c("Linear Velocity", "Packing Porosity", "Flow Resistance", "Permeability"),
#' add_tpa = TRUE,
#' spec = list(EP = rep(">4000", 4),
#'            AH = rep(">4000", 4),
#'            Tf = rep("0.8-1.2",4),
#'            As = rep("0.8-1.2",4)),
#' tpars = c(sn = "C18-2024000001",
#'          pn = "L-C182546510",
#'          desc = "AC-C18 Column\n(250x4.6 mm, 5 micron, 10 nm)",
#'          bn = "20240000001",
#'          dnum = "LC-C18-20240000001",
#'          oper = "Deniz Can Koseoglu",
#'          mp = "70:30 MeOH:Water",
#'          sp = "80:20 (volumetric) IPA:Water",
#'          bp = "126 bar",
#'          flow = "1.0 mL/min",
#'          temp = "30 degC",
#'          inj = "20 microL",
#'          unit_c = "ppm",
#'          tform = "default"),
#' expath = getwd(),
#' asprat = 0.4,
#' add_pnums = TRUE,
#' fontsize = 10)
#'}
#'
#' @seealso \code{\link{render_defs}}, \code{\link{chrom_tplate}}, \code{\link{chrom_asym}}, \code{\link{chrom_addmets}},
#' \code{\link{chrom_skim}}
#'
#' @import quarto
#' @import magick
#' @importFrom ggpubr get_legend
#' @importFrom flextable flextable autofit append_chunks prepend_chunks valign align border_remove bold italic fontsize line_spacing width delete_part as_chunk as_sup as_i
#' @importFrom tibble tribble
#' @importFrom glue glue
#' @importFrom fs file_move
#' @importFrom knitr plot_crop
#'
lcqc_render <- function(tpl, cplot, tpars, spec, clogo = NA, sym = NA, ext = NA,
                        mets = c("EP", "AH", "Tf", "As"), exmets = c("Linear Velocity", "Packing Porosity", "Flow Resistance", "Permeability"),
                        add_tpa = FALSE, expath = getwd(), asprat = 0.4, pnms = NA, pconcs = NA, add_pnums = TRUE,
                        fontsize = 10, which_pks = "all") {
  #Check for dependencies
  #Preliminary checks
  #Check and compile input data
  if(any(which_pks %in% "all")) {
    which_pks <- seq(nrow(tpl[["results"]]))
  } else if(any(which_pks<1)|length(which_pks)>nrow(tpl[["results"]])) stop("Peaks for which data is to be reported ('which_pks') must be within the data range!")

  if((is.list(tpl) & !all(c("results", "information", "call") %in% names(tpl)))|
     (is.list(sym) & !all(c("results", "information", "call", "plots") %in% names(sym)))|
     (is.list(ext) & !all(c("results", "information", "call") %in% names(ext)))) {
    stop("Input data in incorrect format! Arguments 'tpl', 'sym' and 'ext' must be output from 'chrom_tplate', 'chrom_asym', and 'chrom_addmets', respectively.")
  } else input <- setNames(lapply(list(tpl, sym, ext), function(x) if(!all(is.na(x)) & "results" %in% names(x)) x[["results"]][if(all(c("metric", "value", "units") %in% colnames(x[["results"]]))) 1:nrow(x[["results"]]) else which_pks,] else NA), c("tpl", "sym", "ext"))
  if(!any(c("gg","ggplot") %in% class(cplot))) stop("The chromatogram plot ('cplot') must originate from ggplot2!")

  #Check text parameters (mobile phase, serial number etc.)
  parnms <- c("sn", "pn", "desc", "bn", "dnum", "oper", "mp", "sp", "bp", "flow", "temp", "inj", "unit_c", "tform")
  defpars <- setNames(c(rep("?", 13),"default"), parnms)
  misnms <- which(!parnms %in% names(tpars))
  if(length(misnms)>0) tpars[parnms[misnms]] <- defpars[parnms[misnms]]

  #Define temporary variables (MAYBE INCLUDE THESE AS ARGUMENTS IN THE FUTURE?)
  tmfile <- "HPLC_QC_REPORT_1"
  tmpath <- list.files(system.file("qmd_temp", package = "lcqc"), full.names = TRUE, pattern = if(is.na(tmfile)) "\\.qmd$" else paste0(tmfile, ".qmd"))
  imgpath <- paste0(dirname(tmpath), "/img")

  #Check additional parameters
  if(!is.numeric(fontsize)|length(fontsize)!=1) stop("Argument 'fontsize' must be a single numeric value!")
  if(!file.exists(tmpath)) stop("The Quarto template .QMD file does not exist! Check the filepath...")
  if(any(tpars["tform"] %in% "default")) tpars["tform"] <- format(Sys.time(), "%b %d, %Y (%H:%M:%S)")
  if(all(is.na(pnms))) pnms <- ptpa <- paste0("Peak ", which_pks) else if(add_pnums) {
    ptpa <- paste0(pnms, " (", which_pks, ")")
    pnms <- paste(which_pks, ":", pnms) #seq(nrow(input[["tpl"]]))
  }
  if(all(is.na(pconcs))) pconcs <- rep("?", length(which_pks))
  metnms <- c("EP", "AH", "S5", "EMG", "Tf", "As")
  exmetnms <- c("Linear Velocity", "Packing Porosity", "Flow Resistance", "Permeability")
  if(any(mets %in% "all")) mets <- metnms
  if(any(exmets %in% "all")) exmets <- exmetnms
  if(!all(mets %in% metnms)) stop(paste0("Argument 'mets' must be one or more of: ", paste0("'", metnms, "'", collapse = ", "), "!"))
  if(!all(exmets %in% exmetnms)) stop(paste0("Argument 'exmets' must be one or more of: ", paste0("'", exmetnms, "'", collapse = ", "), "!"))
  if(all(is.na(input[["sym"]]))) mets <- mets[!mets %in% c("Tf","As")]
  if(!identical(sort(names(spec)), sort(mets))) { #!all(names(spec) %in% mets
    stop("All elements of 'spec' must be named, with names included in 'mets'!")
  } else {
    spec <- lapply(spec, function(x) {
      if(!any(length(x) %in% c(1,length(which_pks)))) {
        stop("Argument 'spec' must be a list of character vectors containing QC specification limits, with each element possessing length of either 1 or equal to that of 'which_pks'!")
      } else if(length(x)==1) rep(x, length(which_pks)) else x
    })
  }

  #Prepare clogo
  if(is.na(clogo)) {
    clogo <- "false"
    cat("No custom logo was provided. Using sample logo for report...")
  }  else if(!file.exists(clogo)) stop("The chosen custom logo file path is invalid!") else {
    logo_ext <- strsplit(basename(clogo), split="\\.")[[1]][-1]
    if(!any(c("png", "jpg", "jpeg") %in% logo_ext)) stop("Currently only .png and .jpg formats are supported for custom logo!")
    file.copy(from = clogo, to = paste0(imgpath,"/custom_logo.",logo_ext), overwrite = TRUE)
    clogo <- "true"
  }

  #Export plot(s) - INCLUDING OPTIONALLY TPA PLOTS!
  plst <- list()

  plst[["qcplot"]] <- cplot + theme(aspect.ratio = asprat,
                         plot.margin = margin(0, 0, 0, 0, "cm"),
                         plot.title = element_blank(),
                         legend.position = "none")

  if(add_tpa & is.list(sym)) { #TPA PLOTS
    tplt <- sym[["plots"]]
    tplt_nms <- as.numeric(gsub("\\D", "", names(tplt)))
    tplt <- tplt[which(tplt_nms %in% which_pks)] #FILTER TPA PLOTS BASED ON which_pks
    ptpa <- ptpa[which(which_pks %in% tplt_nms)] #FILTER TPA PLOT NAMES BASED ON which_pks

    #Extract and export plot legend
    tpa_expl <- tplt[[1]] + theme(legend.box.background = element_blank())
    tpa_leg <- ggpubr::as_ggplot(get_legend(tplt[[1]]))
    ggsave(filename = "tpa_leg.png",
           plot = tpa_leg,
           path = imgpath,
           width = 6,
           height = 1.5,
           units = "in",
           dpi = 300)
    knitr::plot_crop(paste0(imgpath, "/tpa_leg.png"))

    #Format plots
    for(i in seq_along(tplt)) plst[[paste0("tpa_",i)]] <- tplt[[i]] + labs(title = ptpa[i]) + theme(legend.position = "none",
                                                                                                    plot.margin = margin(0,0,0,0,"cm"),
                                                                                                    text = element_text(size = 20),
                                                                                                    axis.text = element_text(size = 20),
                                                                                                    axis.title = element_text(size = 20))
  }

  #EXPORT PLOTS
  pltdims <- c(8.02, 8.02)
  tpafnms <- c()
  for(i in seq_along(plst)) {
    pltnm <- paste0(names(plst)[i], ".png")
    ggsave(filename = pltnm,
           plot = plst[[i]],
           path = imgpath,
           height = if(names(plst)[i]=="qcplot") pltdims[2]*asprat else pltdims[2],
           width = pltdims[1],
           units = "in",
           dpi = 300)
    #Crop the white space from the plot via ImageMagick
    knitr::plot_crop(paste0(imgpath, "/", pltnm))

    #Compile paths
    if(grepl("tpa_", names(plst)[i])) tpafnms <- append(tpafnms, pltnm)
  }

  #Begin processing
  tpars["tmix"] <- paste0(pnms, " (", pconcs, " ", tpars["unit_c"], ")", collapse = "\n")

  #Compile data for results tables (for Quarto, these HAVE to be lists as these are represented in YAML!)
  mainres <- c(list(pnms), lapply(mets, function(x) {
    if(x %in% c("EP", "AH", "S5", "EMG")) input[["tpl"]][,grep(paste0("N_",x), colnames(input[["tpl"]]))]
    else input[["sym"]][,grep(x, colnames(input[["sym"]]))]
  }))
  names(mainres) <-c("Name", mets)

  #Remove metrics and extra metrics which result in output of length 0 or all NAs:
  mainres_rm <- which(lengths(mainres)==0 | sapply(mainres, function(x) all(is.na(x))))
  if(length(mainres_rm)>0) {
    mainres <- mainres[-mainres_rm]
    mets <- mets[-mainres_rm]
  }

  #Optionally also compile extra column-specific parameters
  if(!all(is.na(input[["ext"]]))) {
    exmetchk <- which(input[["ext"]][,"metric"] %in% exmets)
    exres <- as.list(input[["ext"]][exmetchk,"value"])
    names(exres) <- exmets[exmetchk]
    exmets <- exmets[exmetchk]
  } else exres <- "false"

  #Run rendering function
  render_fname <- paste0("LC_Col_Report_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec"), ".pdf")
  cat("\nRendering report...")
  quarto::quarto_render(input = tmpath,
                        output_format = "pdf",
                        output_file = render_fname,
                        execute_params = list(clogo = clogo,
                                              tpath = if(length(tpafnms)==0) "false" else as.list(tpafnms),
                                              tpars = as.list(tpars),
                                              mainmets = mainres, spec = spec, extmets = exres, metfont = fontsize),
                        quiet = FALSE)

  #Move the output file to a custom directory (file is also removed from working directory)
  outdir <- paste0(getwd(), "/", render_fname) #imgpath
  fs::file_move(outdir, expath)
  cat("\nPDF report exported to: ", paste0(expath, "/", render_fname))

  #Finally, remove the main and tpa plot files
  invisible(file.remove(list.files(imgpath, pattern = "tpa_[[:digit:]]|qcplot.png$", full.names = TRUE)))
}
