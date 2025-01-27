#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate Theoretical Plate Number, Height Equivalent to a Theoretical Plate (HETP), Reduced Plate Height h, and/or Separation Impedance E
#USP, EP/BP/DAB, Area Height, and EMG (Exponentially-Modified Gaussian) methods included
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Number of Theoretical Plates and related metrics for LC columns
#'
#' @description Calculates the theoretical plate number \emph{N}, Height Equivalent to a Theoretical Plate (HETP),
#' reduced plate height \emph{h}, and separation impedance \emph{E} using various methods such as 5-sigma (S5),
#' European/British Pharmacopoeia (EP), Area-Height (AH), and Exponentially-Modified Gaussian (EMG). See \strong{Details} for further
#' information.
#'
#' @param input The output of function \code{\link{chrom_detect}}.
#' @param method The method(s) to use for the calculation of \emph{N}. One or more of: full-width (\code{"FW"}), 5-sigma (\code{"S5"}),
#' European Pharmacopoeia (\code{"EP"}), inflection point width (\code{"inf"}), Area-Height (\code{"AH"}),
#' Exponentially-Modified Gaussian (\code{"EMG"}), or \code{"all"} (default).
#' @param pa \strong{Optional} \code{numeric} vector of peak areas of length equal to the examined number of peaks. Required if \code{method}
#' includes \code{"AH"}.
#' @param len \strong{Optional} \code{numeric} column length (in mm). Defaults to \code{NA}. Required for calculation of HETP.
#' @param dp \strong{Optional} \code{numeric} column stationary phase particle diameter (in µm). Required alongside \code{len} for
#' calculation of reduced plate height \emph{h}.
#' @param show_widths A \code{logical} specifying whether the various peak half-widths at specific heights required for the
#' calculations are included in the results (\code{TRUE} by default).
#' @param crit_w The critical width parameter to use for baseline calculation via FastChrom (\code{\link{fastchrom_bline}}).
#' Defaults to \code{"auto"} or can be set manually as a \code{numeric} (usually equal to the minimum peak width at half height).
#' @param which_peaks Selects specific peaks from \code{input} to process. Either \code{"all"} (default) or a \code{numeric} vector
#' of peak indices included in \code{input}.
#' @param deltap \strong{Optional} back pressure observed for the column with a specific mobile phase (in \strong{bar}). Required for calculation of
#' separation impedance \emph{E} (see \strong{Details}).
#' @param visc \strong{Optional} dynamic viscosity of the mobile phase (in \strong{mPas}). May be calculated by \code{\link{chrom_visc}}.
#' Required for calculation of separation impedance \emph{E} (see \strong{Details}).
#' @param t0 \strong{Optional} dead time of the column, i.e. breakthrough time. Either given in \strong{minutes} if \code{t0_mode} is \code{"manual"},
#' or given as a single peak index included in \code{which_peaks}. Required for calculation of separation impedance \emph{E}
#' (see \strong{Details}).
#' @param t0_mode The mode to use for calculation of dead time. One of: \code{"peak"} or \code{"manual"}, where \emph{t0} is taken
#' directly as the time of specified peak maximum or given manually in minutes, respectively.
#' @param imped_met A single \code{character} string specifying which method to use for calculation of Separation Impedance \emph{E}.
#' One \strong{or more} of: \code{"all"} (default), \code{"indiv"} and/or \code{"univ"} . See \strong{Details}.
#'
#' @return A named \code{list} of length 3 containing the \code{data.frame} of results for each examined peak (\code{results}),
#' a \code{character} string of various information about the results (\code{information}), and the function call (\code{call}).
#'
#' @export
#'
#' @details
#' The number of theoretical plates \eqn{N} is a widely-used column performance index where a higher number of theoretical plates
#' represents better column efficiency. Several methods of calculation are implemented in \pkg{lcqc}. The full-width method
#' uses the 'base width' \eqn{W_b} of a Gaussian peak defined as the baseline intercepts of tangent lines for a perfect Gaussian
#' peak, which occur at \strong{13.4% peak height}.
#' \deqn{N_{FW} = 16(t_R/W_b)^2}
#' The \strong{5-sigma} method utilises the fact that a perfectly Gaussian peak is exactly 5-sigma (i.e. 5 standard deviations)
#' wide at a 4.4% height (Bidlingmeyer & Warren Jr., 1984; Villalon, 2023) and uses the 5-sigma measure to determine width.
#' This helps cope with tailing often encountered in chromatography, which distorts the peak from its Gaussian shape.
#' \deqn{N_{5\sigma} = 25(t_R/W_{5\sigma})^2}
#' The European Pharmacopoeia (EP) method is perhaps the most popular and uses the peak width at half-height (\eqn{W_{50}}) along
#' with retention time \eqn{t_R}. Since the coefficient used varies from 5.55 in the German (DAB), British (BP), and European (EP)
#' Pharmacopoeias to 5.54 in the Japanese Pharmacopoeia (Rev. 15, April 2006), a mean of these (5.545) was implemented herein.
#' \deqn{N_{EP} = 5.545(t_R/W_{50})^2}.
#' #' An alternative equation uses the theoretical Gaussian inflection point width at ca. 60.7% peak height.
#' \deqn{N_{inf} = 4(t_R/w_{60.7})^2}
#' True to its name, the Area-Height (AH) method utilizes peak area \eqn{A} and height \eqn{H} to calculate \eqn{N}.
#' \deqn{N_{AH} = 2\pi(t_RH/A)^2}
#' All of the above methods are based on a true Gaussian peak shape, which is almost never encountered in practical chromatography.
#' Tailing and (less prevalent) fronting phenomena distort the peak shape and cause errors in calculation. A simple equation that
#' results in "approximately accurate" (Meyer, 2010) values of \eqn{N} and based on the Exponentially-Modified Gaussian (EMG) model
#' was proposed by Foley & Dorsey (1983). The equation uses the peak width (\eqn{W_{10}}) and half-widths at 10% height
#' (\eqn{b} and \eqn{a} for trailing and leading width, respectively). The number of theoretical plates obtained via this equation
#' is usually lower than that from other methods.
#' \deqn{N_{EMG} = (41.7(t_R/W_{10})^2)/(b/a + 1.25)}
#' Also calculated is the plate height, also known as Height Equivalent to a Theoretical Plate (HETP) is the distance in mm (or µm)
#' over which chromatographic equilibrium is achieved. This is simply related to \eqn{N} and column length \eqn{L} by:
#' \deqn{HETP = L/N}
#' From HETP, the reduced plate height \eqn{h} may also be calculated provided the average particle size of the stationary phase
#' \eqn{d_p} (in µm) is known; \eqn{h} is a dimensionless parameter and may be used to compare columns of different length
#' and particle size more easily. Values of \eqn{h} should be in range of \strong{2 to 5 (lower is better)}. For example, at a value
#' of 3, complete chromatographic equilibrium is obtained over 3 layers of stationary phase (Meyer, 2010).
#' \deqn{h = HETP/d_p}
#' Finally, Separation Impedance \eqn{E} is a measure of column "quality" (efficiency) that incorporates back pressure (\eqn{\Delta p}),
#' number of theoretical plates, dynamic viscosity of the mobile phase (\eqn{\eta}, in mPas), and breakthrough time.
#' Values of \strong{<10000} are imperative for a liquid chromatography process to be considered "high performance" (Meyer, 2010).
#' In \pkg{lcqc}, \eqn{E} may either be calculated separately for each individual value of \eqn{N} (much like \eqn{HETP} and \eqn{h})
#' when \code{imped_met} includes \code{"indiv"}. In this case, the following equation is used:
#' \deqn{E = (\Delta p t_0)/(N^2\eta)}
#' Additionally, a universal equation independent of \eqn{N} may be used when \code{imped_met} includes \code{"univ"} (default).
#' \deqn{E = (10^8/5.54^2)\times(\Delta p t_0/\eta)\times(W_{50}/t_R)^4}
#'
#' @references
#' Bidlingmeyer, B.A., Warren, F.V.Jr. (1984), 'Column Efficiency Measurement', \emph{Analytical Chemistry} \strong{56} (14), pp. 1583A-1596A, DOI: \url{https://www.doi.org/10.1021/ac00278a002}.
#'
#' Foley, J.P., Dorsey, J.G. (1983), 'Equations for calculation of chromatographic figures of merit for ideal and skewed peaks', \emph{Analytical Chemistry} \strong{55} (4), pp. 730-737, DOI: \url{https://www.doi.org/10.1021/ac00255a033}.
#'
#' Ishizuka, N., Kobayashi, H., Minakuchi, H., Nakahishi, K., Hirao, K., Hosoya, K., Ikegami, T., Tanaka, N. (2002), 'Monolithic silica columns for high-efficiency separations by high-performance liquid chromatography', \emph{Journal of Chromatography A} \strong{960}, pp. 85-96, DOI: \url{https://doi.org/10.1016/S0021-9673(01)01580-1}.
#'
#' Jarmalavičienė, R., Kornyšova, O., Westerlund, D., Maruška, A. (2003), 'Non-particulate (continuous bed or monolithic) restricted-access reversed-phase media for sample clean-up and separation by capillary-format liquid chromatography', \emph{Analytical and Bioanalytical Chemistry} \strong{377} (5), pp. 902-908, DOI: \url{https://www.doi.org/10.1007/s00216-003-2244-z}.
#'
#' Meyer, V.R. (2010), \emph{Practical High-Performance Liquid Chromatography}, John Wiley & Sons, Chichester, United Kingdom.
#'
#' The United States Pharmacopeial Convention (2021), \emph{Physical Tests/<621> Chromatography} (USP 40-NF 35), available at:
#' \url{https://www.usp.org/harmonization-standards/pdg/excipients/chromatography} (accessed 24.04.2024).
#'
#' Villalon, G.C. (2023), 'Characterization of Chromatographic Peaks with Excel', \emph{Journal of Chemical Education} \strong{100}, pp. 928-932, DOI: \url{https://doi.org/10.1021/acs.jchemed.2c00588}.
#'
#' @examples
#' \dontrun{
#' #Get main data and peak areas
#' dt <- lcqc:::wf_detpeaks
#' pas <- lcqc:::wf_ints$integ_res$pa
#'
#' #Theoretical plates only
#' res <- chrom_tplate(dt, method = c("S5","EP", "EMG"))
#'
#' #With AH method, HETP, and h
#' res2 <- chrom_tplate(dt, method = "AH", pa = pas, len = 150, dp = 5)
#'
#' #With Separation Impedance E
#' res3 <- chrom_tplate(dt, method = "AH", pa = pas, len = 150, dp = 5, visc = 0.5264825,
#' t0 = 0.25, t0_mode = "manual", deltap = 85, imped_met = "all")
#' }
#'
#' @seealso \code{\link{chrom_detect}}, \code{\link{chrom_visc}}
chrom_tplate <- function(input, method = "all", pa = NA, len = NA, dp = NA, show_widths = TRUE, crit_w = "auto", which_peaks = "all",
                         deltap = NA, visc = NA, t0 = NA, t0_mode = "peak", imped_met = "all") {
  #Perform checks
  input <- chkdt(input)

  if(!any(c("all", "indiv","univ") %in% imped_met)) stop("Argument 'imped_met' must be one of: 'all', 'indiv' or 'univ'!")
  if(any(imped_met %in% "all")) imped_met <- c("indiv","univ")
  metacc <- c("all", "FW", "S5", "EP", "inf", "AH", "EMG")
  if(!any(metacc %in% method)) stop(paste0("Method must be one or more of: ", paste0("'", metacc, "'", collapse = ", "), "!"))
  if(any(method %in% "all")) method <- metacc[!metacc %in% "all"]
  if(any(method %in% "AH") & any(is.na(pa))) stop("Peak Areas ('pa') must be provided if method 'AH' is selected!")
  if(!any(is.na(pa)) & length(pa)!=nrow(input[["Peak_Extents"]])) stop("If a vector of peak areas ('pa') is provided, its length must equal the number of peaks!")
  if(is.numeric(which_peaks)) {
    if(max(which_peaks)>nrow(input[["Peak_Extents"]])) stop("At least one of the chosen peak indices exceeds the number of peaks in the input data!")
  }

  #Generate function call
  cl_rec <- match.call()

  #Retrieve data for calculation of Theoretical Plate Number
  #Pre-process input data
  input <- dtprep(input, crit_w = crit_w)

  #Unpack input data
  acc <- input[["acc_tops"]]
  ptypes <- input[["type_df"]][["maxes"]][,"ptype"] #Peak types
  pklst <- input[["peak_list"]]

  #Additional checks
  if(!is.na(t0)) {
    if(!all(t0_mode %in% c("peak", "manual"))) stop("Argument 't0_mode' must be one of 'peak' or 'manual'!")
    if(t0%%1!=0 & t0_mode=="peak") stop("When 't0_mode' is set to 'peak', 't0' must be an integer!")
    else if(t0_mode=="peak" & (t0 > nrow(acc))) stop("The peak ID identifying t0 is outside of the input data range!")
    t_bckp <- t0
    if(t0_mode == "peak") t0 <- acc[t0,"rt_max"]
    if(any(acc[,"rt_max"] < t0)) stop("At least one of the peaks elutes before t0!")

    #Convert t0 to seconds
    t0 <- t0*60
  }

  #Output list
  output <- list()
  #outdf <- as.data.frame(matrix(NA, nrow = length(ptypes), ncol = 2))
  #outdf[,c("group","peak")] <- acc[,c("group","peak")]

  for(i in seq_along(pklst)) {

    #Retrieve accurate peak maximum and corresponding retention time
    tr <- acc[i,"rt_max"]
    H <- acc[i,"sig_max"]

    #Calculate peak half-widths
    hws <- peak_hw(pklst[[i]], accmax = c(tr, H), frac = c(0.044, 0.10, 0.134, 0.50, 0.607), resolved = if(!is.na(ptypes[i]) & ptypes[i]=="B") TRUE else FALSE, slnt = TRUE)

    #Retrieve peak widths and half-widths
    W044 <- hws[1,"W"]
    W10 <- hws[2,"W"]
    W13 <- hws[3,"W"]
    W50 <- hws[4,"W"]
    W60 <- hws[5,"W"]
    A044 <- hws[1,"A"]
    B044 <- hws[1,"B"]
    A13 <- hws[2,"A"]
    B13 <- hws[2,"B"]
    A10 <- hws[3,"A"]
    B10 <- hws[3,"B"]
    A50 <- hws[4,"A"]
    B50 <- hws[4,"B"]
    A60 <- hws[5,"A"]
    B60 <- hws[5,"B"]

    #Output vector for peak i
    ns <- c()

    #Add calculated widths if show_widths == TRUE
    if(show_widths) ns[c("W044", "W10", "W13", "W50", "W60", "A044", "B044", "A10", "B10", "A13", "B13", "A50", "B50", "A60", "B60")] <-
      c(W044, W10, W13, W50, W60, A044, B044, A10, B10, A13, B13, A50, B50, A60, B60)

    for(j in method) {
      #Calculate N values
      ns[paste0("N_",j)] <- n_temp <-
        if(j=="FW") 16*(tr/W13)^2
        else if(j=="S5") 25*(tr/W044)^2
      else if(j=="EP") 5.545*(tr/W50)^2
      else if(j=="inf") 4*(tr/W60)^2
      else if(j=="AH") 2*pi*((tr*H/pa[i]))^2
      else if(j=="EMG") 41.7*((tr/W10)^2/((B10/A10)+1.25))
    }

    #If requisite values are provided, calculate HETP and reduced plate height
    for(j in method) {
      if(!is.na(len)) ns[paste0("HETP_",j)] <- h_temp <- len/ns[paste0("N_",j)] #USP RECOMMENDS 'len' in MICROMETERS HERE!
      if(!any(is.na(c(len, dp)))) ns[paste0("h_",j)] <- h_temp/(dp/1000)
    }

    #If requisite variables are provided, calculate Separation Impedance E
    if(!any(is.na(c(deltap, visc, t0)))) {
      if(any(imped_met %in% "indiv")) {
        #deltap_E <- deltap*1.01971621 #Convert from bar to kg/cm^2
        #visc_E <- visc/1000000*10.1971621 #Convert from mPas to kg/cm^2s
        visc_E <- visc/10^8 #Convert from mPa-s to bar-s
        for(j in method) {
          ns[paste0("E_",j)] <- (deltap*t0)/((ns[paste0("N_",j)]^2)*visc_E) #Pressure in kg cm^-2, t0 in seconds, visc in kg cm^-2 s
        }
      }
      if(any(imped_met %in% "univ")) {
        ns["E"] <- (10^8/5.54^2)*((deltap*t0)/visc)*(W50/tr)^4
      }
    }
    output[[paste0("peak_",i)]] <- c(acc[i,c("group","peak")], ns)
  }

  #output data.frame
  output <- Reduce(rbind.data.frame, output)
  if(is.list(output)) output <- as.data.frame(output) #For cases where only one peak is present!

  #Compile information about function
  metnms <- c(FW = "Full Width", S5 = "5-sigma", EP = "European Pharmacopoeia", inf = "Inflection Point", AH = "Area-Height", EMG = "Exponentially-Modified Gaussian") #USP = "US Pharmacopoeia"
  parnms <- c(len = "Column Length", dp = "Particle Size", deltap = "Back Pressure", visc = "Dynamic Viscosity")
  parlst <- setNames(c(len, dp, deltap, round(visc,3)), names(parnms))
  parunits <- setNames(c("mm", "microns", "bar", "mPas"), names(parnms))
  parlst <- parlst[!is.na(parlst)]
  t0_state <- if(!is.na(t0)) paste0("\nDead time was equal to ", ifelse(t0_mode=="peak", paste0("the retention time of peak ", t_bckp, " (", t0/60, "min)"), paste0(t0/60, " min")),".") else "\nDead time was not determined/provided."
  add_state <- if(length(parlst)>0) paste0("\nThe following additional parameters were provided: ", paste0(parnms[names(parlst)], " (", parlst, " ", parunits[names(parlst)], ")", collapse = ", "), ".") else ""
  information <- paste0("Theoretical Plate number calculation was attempted for ", ifelse(is.numeric(which_peaks), paste0(length(which_peaks), " out of "), "all of "), length(pklst), " peaks.",
                        "\nThe following methods were used: ", paste0(metnms[method], " (", method, ")", collapse = ", "),".",
                        t0_state,
                        add_state,
                        ifelse(is.numeric(pa), " Peak areas were also provided.", ""),
                        ifelse(!any(is.na(c(len,dp))), "\nHeight Equivalents to a Theoretical Plate (HETP) and reduced plate heights (h) were calculated.", ""),
                        ifelse(!any(is.na(c(deltap, visc, t0))), "\nSeparation Impedance (E) was calculated.", ""))

  #Add compound names (optional)
  if(any(colnames(acc) %in% "Compound")) output <- cbind.data.frame(Compound = acc[,"Compound"], output)

  #Filter unwanted peaks
  if(is.numeric(which_peaks)) output <- output[which_peaks,]
  return(list(results = output, information = information, call = cl_rec))
}
