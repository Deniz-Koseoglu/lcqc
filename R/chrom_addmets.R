#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Calculate additional column-specific metrics
#Packing porosity Epsilon, permeability (mm^2/s/bar), specific permeability (mm^2), and Flow Resistance
#If column particle size is provided, validity of specific permeability can be checked
#OMITTED: Reduced Velocity v due to the requirement for a Diffusion Coefficient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Calculate additional HPLC column-specific performance metrics
#'
#' @description This function uses various column characteristics such as length, particle size,
#' flow rate, internal diameter (among others) to calculate various column performance indicators.
#' See \strong{Details} for further information.
#'
#' @param which_mets A \code{character} vector of metrics to calculate. One or more of: \code{"all"} (default),
#' \code{"linvel"} (Linear Velocity; mm/s), \code{"porosity"} (Packing Porosity; dimensionless),
#' \code{"flowres"} (Flow Resistance; dimensionless), \code{"pabil"} (Permeability; mm^2/s/bar),
#' and/or \code{"spabil"} (Specific Permeability; mm^2).
#' @param t0 The \code{numeric} value for dead time (i.e. breakthrough time) in \strong{minutes}.
#' @param len The \code{numeric} column length (in mm).
#' @param flow Flow rate (mL/min, \code{numeric}).
#' @param id Column internal diameter (in mm, \code{numeric}).
#' @param deltap Back pressure (bar; \code{numeric}).
#' @param visc Dynamic viscosity of the mobile phase (mPas; \code{numeric}). May be calculated via \code{\link{chrom_visc}}.
#' @param dp Stationary phase particle size (in µm).
#'
#' @return A named \code{list} of length 3 containing a \code{data.frame} of \code{results}, various \code{information} about them,
#' and the function \code{call}.
#'
#' @details
#' The linear velocity \eqn{\mu} of the mobile phase is simply related to column length \eqn{L_c} and true breakthrough time \eqn{t_0}.
#' \deqn{\mu = L_c/t_0}
#' It may be beneficial to verify breakthrough time in this equation, i.e. determine whether the non-retained analyte is really passing
#' through the column at the velocity of the mobile phase, by calculating packing porosity \eqn{\epsilon}. The packing porosity of
#' \strong{bonded} silica-based stationary phases is \emph{ca.} 0.65. Using this value as reference, \eqn{\epsilon} may be calculated
#' simply from \eqn{t_0\text{ }(s)}, \eqn{L_c\text{ }(mm)}, flow rate \eqn{F\text{ }(mL\text{ }min^{-1})}, and inner column diameter
#' \eqn{d_c^2\text{ }(mm^2)}.
#' \deqn{\epsilon = 21\times[(Ft_0)/(d_c^2L_c)]}
#' Significantly higher or lower values of \eqn{\epsilon} (>1 and <0.5, respectively) indicate retention of the analyte or its exclusion
#' from the pores of the stationary phase (Meyer, 2010).
#' Another useful metric is permeability \eqn{K}, which incorporates back pressure \eqn{\Delta\rho\text{ }(bar)}. A large value of \eqn{K}
#' indicates poor column packing, while the reverse is characteristic of a leak. Values of \eqn{15\text{ }mm^2\text{ }s^{-1}\text{ }bar^{-1}}
#' are typical for bonded phases at a fast flow rate of \eqn{1.6\text{ }mL\text{ }min^{-1}}.
#' \deqn{K = L_c^2/(\Delta\rho t_0)}
#' Specific permeability \eqn{K^{\circ}\text{ }(mm^{-2})} may also be calculated (typically \eqn{4.0\times 10^{-8}\text{ }mm^2} for bonded
#' phases) by incorporating flow rate, dynamic viscosity, column length, inner diameter, and back pressure.
#' \deqn{K^{\circ} = 21\times 10^-8\times[(F\eta L_c)/(d_c^2\Delta\rho)]}
#' Finally, \eqn{K} may be represented as a dimensionless metric called Flow Resistance \eqn{\Phi}, which facilitates comparison of different columns.
#' One of the additional required parameters is particle size \eqn{d_p\text{ }(\mu m)}.
#' \deqn{\Phi = 4.7\times[(\Delta\rho d_p^2 d_c^2)/(L_c \eta F)]}.
#' A typical \eqn{\Phi} value of 1000 is observed for packed HPLC columns. Significant upward and downward deviations (e.g. >2000 or <500)
#' are indicative of a blockage or voids in the packing, respectively.
#'
#' For calculation of theoretical plates and additional dimensionless metrics such as reduced plate height and Separation Impedance
#' to assess column performance, see \code{\link{chrom_tplate}}.
#'
#' @references
#' Meyer, V.R. (2010), \emph{Practical High-Performance Liquid Chromatography}, John Wiley & Sons, Chichester, United Kingdom.
#'
#' @export
#'
#' @examples
#' chrom_addmets(t0 = 1, len = 250, id = 3.2, flow = 1.6, deltap = 70, visc = 0.33, dp = 5)
#'
#' @seealso \code{\link{chrom_visc}}, \code{\link{chrom_tplate}}
chrom_addmets <- function(which_mets = "all", t0, len,  flow = NA, id = NA, deltap = NA, visc = NA, dp = NA) {

  #Perform checks
  if(!all(which_mets %in% c("all", "linvel", "porosity", "flowres", "pabil", "spabil"))) stop("The following 'which_mets' inputs are supported: 'all', 'linvel', 'porosity', 'flowres', 'pabil', and/or 'spabil'!")
  if(any(which_mets %in% "all")) which_mets <- c("linvel", "porosity", "flowres", "pabil", "spabil")
  if(any(which_mets %in% "porosity") & any(is.na(c(flow, id)))) stop("Packing porosity calculation also requires flow rate ('flow') and internal diameter ('id')!")
  if(any(which_mets %in% "flowres") & any(is.na(c(visc, flow, deltap, dp, id)))) stop("Flow resistance calculation also requires flow rate ('flow'), eluent viscosity ('visc'), particle size ('dp'), back pressure ('deltap'), and internal diameter ('id')!")
  if(any(which_mets %in% "pabil") & is.na(deltap)) stop("Permeability calculation also requires back pressure ('deltap')!")
  if(any(which_mets %in% "spabil") & any(is.na(c(visc, flow, deltap, id)))) stop("Specific permeability calculation also requires eluenty viscosity ('visc'), flow rate ('flow'), internal diameter ('id'), and back pressure ('deltap')!")

  #Generate function call
  cl_rec <- match.call()

  #Convert t0 from minutes to seconds
  t0_sec <- t0*60

  #Perform calculations
  res <- list()
  nmvec <- c("Linear_Velocity", "Packing_Porosity", "Flow_Resistance", "Permeability", "Specific_Permeability")
  metvec <- c("linvel", "porosity", "flowres", "pabil", "spabil")
  metvec[!metvec %in% which_mets] <- NA
  unitvec <- c("mmsec^-1", "dimensionless", "dimensionless", "mm^2s^-1bar^-1", "mm^2")

  for(i in seq_along(metvec)) {
    if(!is.na(metvec[i])) {
      res[[i]] <- c(nmvec[i],
                    if(metvec[i]=="linvel") len/t0_sec
                    else if(metvec[i]=="porosity") (21*((flow*t0_sec)/(id^2*len))) #ALTERNATIVE: (4*flow*t0_sec)/(id^2*pi*len)
                    else if(metvec[i]=="flowres") 4.7*((deltap*dp^2*id^2)/(len*visc*flow)) #ALTERNATIVES: (deltap*dp^2*id^2*pi)/(4*len*visc*flow); dp^2/(permeability*visc*porosity)
                    else if(metvec[i]=="pabil") len^2/(deltap*t0_sec)
                    else if(metvec[i]=="spabil") 21*10^-8*((flow*visc*len)/(id^2*deltap)), #ALTERNATIVE: 10^-8*pabil*visc*porosity
                    unitvec[i])
    } else res[[i]] <- rep(NA, 3)
  }
  res <- Reduce(rbind.data.frame, res)
  colnames(res) <- c("metric", "value", "units")
  res <- res[complete.cases(res),]
  res[,"value"] <- as.numeric(res[,"value"])
  res[,"metric"] <- gsub("_", " ", res[,"metric"], fixed = TRUE)

  #Compile information about function
  parnms <- c(t0 = "Dead Time", len = "Column Length", flow = "Flow", id = "Internal Diameter",
              deltap = "Back Pressure", visc = "Dynamic Viscosity", dp = "Particle Size")
  parlst <- setNames(c(t0, len,  flow, id, deltap, visc, dp), names(parnms))
  parunits <- setNames(c("min", "mm", "mLmin^-1", "mm", "bar", "mPas", "microns"), names(parnms))
  parlst <- parlst[!is.na(parlst)]

  information <- paste0("The following parameters were provided for column performance calculations: ", paste0(parnms[names(parlst)], " (", parlst, " ", parunits[names(parlst)], ")", collapse = ", "),".")
  return(list(results = res, information = information, call = cl_rec))
}
