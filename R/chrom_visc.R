#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Bilinear Interpolation of IRREGULARLY-SPACED DATA
#Modified from package 'akima'. See also FORTRAN code:
#https://github.com/cran/akima/blob/master/src/bilinear.f
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Bilinear Interpolation of Irregularly-spaced Data
#'
#' @description \strong{This function is not exported}. Part of the \code{\link{chrom_visc}} workflow, not intended for standalone use.
#' Carries out bilinear interpolation of irregularly-spaced data. Modified from \code{\link[akima]{bilinear}} in package \pkg{akima}.
#'
#' @param x,y Vectors (\code{numeric}) of x- and y-coordinates.
#' @param z A vector of \code{numeric} values to populate the a \code{matrix} of dimensions \code{[x,y]}.
#' @param xout,yout Values of x- and y-coordinates to interpolate for.
#'
#' @return A \code{list} of vectors containing the interpolation coordinates (\code{"x"}, \code{"y"}) and interpolated values (\code{"z"}).
#'
#' @keywords internal
#'
#' @seealso \code{\link{chrom_tplate}}
bilin_interp <- function(x, y, z, xout, yout) {

  #Create xyz df
  df <- do.call(cbind.data.frame, list(x = x, y = y, z = z))

  #Remove duplicates and leave ONLY UNIQUE VALUES of x and y
  dups <- !duplicated(cbind(x,y))
  df <- df[dups,]
  x <- unique(x[dups])
  y <- unique(y[dups])

  #Create x, y matrix
  zmat <- matrix(nrow = length(x), ncol = length(y))
  rownames(zmat) <- x
  colnames(zmat) <- y

  #Populate the matrix with z values
  matgrid <- expand.grid(x = seq_along(x), y = seq_along(y))
  for(i in 1:nrow(matgrid)) {
    chk <- df[which(df[,"x"] == x[matgrid[i,"x"]] & df[,"y"] == y[matgrid[i,"y"]]), "z"]
    zmat[matgrid[i,"x"], matgrid[i,"y"]] <- if(length(chk)==0) NA else chk
  }
  #Bilinear interpolation
  res <- akima::bilinear(x = x, y = y, z = zmat, x0 = xout, y0 = yout)
  return(res)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Show data ranges available for viscosity calculations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname chrom_visc
browse_visc <- function() {
  return(list(dippr105_density = visc_dippr105, vogel_viscosity = visc_vogel))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Estimation of eluent (mobile phase) viscosity via the Linear Blend Rule
#Any number of mobile phase constituents supported.
#Acids/buffers assumed to NOT affect viscosity!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Estimate dynamic viscosity of common HPLC mobile phases
#'
#' @description This function estimates the temperature-dependent dynamic viscosity \eqn{\eta} (in mPas) for pure
#' organic solvents and their mixtures commonly used as mobile phases for High-Performance Liquid Chromatography (HPLC).
#' It is also possible to estimate viscosities of aqueous mobile phases containing methanol (MeOH) or acetonitrile (MeCN) based on
#' bilinear interpolation of experimental data collected from various sources. See \strong{Details} for further information.
#'
#' @param ids A \code{character} vector of mobile phase component IDs to calculate viscosity for. One or more of:
#' butanol (\code{"buoh"}), isopropanol (\code{"ipa"}), acetone (\code{"acet"}), acetonitrile (\code{"mecn"}), benzene (\code{"benz"}),
#' chloroform (\code{"chcl3"}), cyclohexane (\code{"chex"}), diethyl ether (\code{"dee"}), ethanol (\code{"etoh"}), ethyl acetate (\code{"etac"}),
#' methanol (\code{"meoh"}), tetrahydrofuran (\code{"thf"}), and water (\code{"h2o"}). If the latter is included, \code{ids} must be of length 2
#' with the only other possible components being acetonitrile (\code{"mecn"}) or methanol (\code{"meoh"}). See \strong{Details}.
#' @param fracs A \code{numeric} vector of mobile phase component fractions of equal length to \code{ids}.
#' @param frac_type The \strong{type} of fraction given in \code{fracs}. One of: volumetric (\code{"vol"}, default), mass (\code{"mass"}),
#' or mole (\code{"mol"}).
#' @param temp Temperature to calculate viscosity at. See \strong{Details} and \code{\link{browse_visc}} for acceptable temperature ranges for each solvent.
#'
#' @return A named \code{list} containing \code{results}, a \code{character} string of various \code{information} about them,
#' and the function \code{call}. The \code{results} element is a \code{list} named according to the mobile phase components
#' specified in argument \code{ids} and containing a named \code{numeric} vector with the viscosity value (\code{"visc_mPas"}),
#' temperature taken from \code{temp} (\code{"temp_degC"}), as well as mole, mass, and volume fractions of each component
#' (suffixed \code{_molfrac}, \code{_massfrac}, and \code{_volfrac}, respectively).
#'
#' @details
#' Dynamic viscosity and density of various organic solvents is calculated using data taken from the Dortmund Data Bank (DDB, 2024)
#' and packaged with \pkg{lcqc}. Specifically, dynamic viscosity \eqn{\eta} (mPas) is calculated using the Vogel Equation
#' (García-Colín et al., 1989), which uses experimentally derived empirical coefficients \eqn{A_V}, \eqn{B_V}, and \eqn{C_V}.
#' The result is also dependent on temperature, given as \eqn{T_K} in Kelvin.
#' A full list of coefficients and the temperature ranges (\strong{in °C}) within which they are valid is available upon
#' calling \code{\link{browse_visc}}.
#' \deqn{\eta\text{ }(mPas) = exp[A_V+(B_V/(C_V + T_K))]}
#' Similarly, density \eqn{\rho} (kg m^-3) is calculated via the DIPPR105 equation, which incorporates 4 empirical coefficients
#' (\eqn{A}, \eqn{B}, \eqn{C}, and \eqn{D}) alongside temperature (DDB, 2024; Silva et al., 2018).
#' \deqn{\rho\text{ }(kg\text{ }m^{-3}) = A/B^{1+(1-T_K/C)^D}}
#'
#' When a mixture of components is provided in \code{ids}, they have to be accompanied by mole (\code{"mol"}), mass (\code{"mass"}),
#' or volumetric (\code{"vol"}, default) fractions given in \code{fracs}. Whichever fraction type (\code{frac_type}) is chosen,
#' the others are calculated and included in function output. For example, conversion to \eqn{F_{mass}} and \eqn{F_{mol}} from
#' \eqn{F_{vol}} is carried out using density \eqn{\rho} and relative molecular mass (\eqn{RMM}, g/mol) as follows for each component.
#' \deqn{F_{mass} = F_{vol}\times\rho}
#' \deqn{F_{mol} = F_{mass}/RMM}
#' The overall viscosity \eqn{\eta_{total}} of the mixtures is calculated using the Linear Blend Rule \strong{for purely organic mixtures}.
#' Briefly, the contributing \strong{mole} fraction (\eqn{F_{mol}}) of each component \eqn{i} is multiplied by the corresponding viscosity
#' value of the pure component, and results for all components obtained in this manner are summed.
#' \deqn{\eta_{total} = \sum{F_{mol(i)}*\eta_i}}
#' For \strong{organic-water mixtures}, the Linear Blend Rule is not applicable due to the strong interactions between water molecules
#' (Snyder et al., 1997). For this reason, \pkg{lcqc} is limited to estimating the viscosity of such mixtures via
#' \strong{bilinear interpolation} of experimental data obtained for the two most popular aqueous mobile phases in HPLC,
#' \strong{Methanol-Water} and \strong{Acetonitrile-Water}, from various sources (Snyder et al., 2009; Thompson et al., 2006;
#' Teutenberg et al., 2009; Wohlfarth & Wohlfahrt, 2001).
#'
#' @references
#' Dortmund Data Bank (2024), 'Online Calculation', available at: \url{https://www.ddbst.com/calculation.html} (accessed 25.04.2024).
#'
#' García-Colín, L.S., del Castillo, L.F., Goldstein, P. (1989), 'Theoretical Basis for the Vogel-Fulcher-Tammann Equation', \emph{Physical Review B} \strong{40} (10), pp. 7040-7044, DOI: \url{https://www.doi.org/10.1103/PhysRevB.40.7040}.
#'
#' Silva, M., Vieira, B., Ottens, M. (2018), 'Preferential crystallization for the purification of similar hydrophobic polyphenols', \emph{Journal of Chemical Technology & Biotechnology} \strong{93} (7), pp. 1997-2010, DOI: \url{https://doi.org/10.1002/jctb.5526}.
#'
#' Snyder, L.R., Kirkland, J.J., Glajch, J.L. (1997), 'Appendix II: Properties of Solvent Used in HPLC', In: \emph{Practical HPLC Method Development, Second Edition}, John Wiley & Sons, USA, DOI: \url{https://www.doi.org/10.1002/9781118592014.app2}.
#'
#' Snyder, L.R., Kirkland, J.J., Dolan, J.W. (2009), 'Appendix I: Properties of HPLC Solvents', In: \emph{Introduction to Modern Liquid Chromatography}, pp. 879-886, DOI: \url{https://doi.org/10.1002/9780470508183.app1}.
#'
#' Teutenberg, T., Wiese, S., Wagner, P., Gmehling, J. (2009), 'High-temperature liquid chromatography. Part II: Determination of the viscosities of binary solvent mixtures - Implications for liquid chromatographic separations', \emph{Journal of Chromatography A} \strong{1216} (48), pp. 8470-8479, DOI: \url{https://doi.org/10.1016/j.chroma.2009.09.075}.
#'
#' Thompson, J.W., Kaiser, T.J., Jorgenson, J.W. (2006), 'Viscosity measurements of methanol-water and acetonitrile-water mixtures at pressures up to 3500 bar using a novel capillary time-of-flight viscometer', \emph{Journal of Chromatography A} \strong{1134} (1), pp. 201-209, DOI: \url{https://doi.org/10.1016/j.chroma.2006.09.006}.
#'
#' Wohlfarth, C., Wohlfahrt, B. (2001), '3 Mixtures of Water and Organic Compounds', In: \emph{Landolt-Börnstein - Group IV Physical Chemistry, Volume 18A: Pure Organometallic and Organononmetallic Liquids, Binary Liquid Mixtures}, Springer-Verlag Berlin Heidelberg, DOI: \url{https://www.doi.org/10.1007/10639275_6}.
#'
#' @export
#'
#' @examples
#' #Pure component (MeOH)
#' vpure <- chrom_visc(ids = "meoh", fracs = 1, frac_type = "vol", temp = 25)
#'
#' #Mixture (3:7 MeOH:EtAc)
#' vmix <- chrom_visc(ids = c("meoh", "etac"), fracs = c(0.3, 0.7), frac_type = "vol", temp = 25)
#'
#' #Aqueous mobile phase (2:8 MeOH:H2O)
#' vaq <- chrom_visc(ids = c("meoh", "h2o"), fracs = c(0.2, 0.8), frac_type = "vol", temp = 25)
#'
#' @seealso \code{\link{chrom_tplate}}
chrom_visc <- function(ids, fracs, frac_type = "vol", temp = 25) {

  #Generate function call
  cl_rec <- match.call()

  #Perform checks
  id_list <- visc_vogel[,"Short_Name"]
  if(!all(ids %in% id_list)) stop(paste0("The following mobile phase ids not recognized: ", paste0(ids[which(!ids %in% id_list)], collapse = ", ")))
  if(sum(fracs)!=1) stop("The sum of mobile phase constituent fractions must equal to 1!")
  if(!any(c("vol", "mol", "mass") %in% frac_type)) stop("Argument 'frac_type' must be one of: 'mol', 'mass', or 'vol'!")
  if(any(ids %in% "h2o") & length(ids)!=1 & (!any(c("meoh", "mecn") %in% ids) | length(ids)!=2)) stop("Currently only mixtures of MeCN or MeOH with water are supported!")
  tcond <- c()
  temp_k <- temp + 273.15
  for(i in ids) {
    #Check whether the temperature is outside of the equation prediction and/or interpolation range (the latter for binary mixtures)
    rownum <- which(id_list == i)
    tcond[i] <- any(visc_dippr105[rownum,"T_min"] > temp) | any(visc_vogel[rownum, "T_min"] > temp) | any(visc_dippr105[rownum,"T_max"] < temp) | any(visc_vogel[rownum, "T_max"] < temp)
  }
  if(any(tcond)) stop(paste0("Temperature is outside the allowed range for the following mobile phase components: ", paste0(ids[which(tcond)], collapse = ", "), "..."))

  #Process data to calculate or interpolate dynamic viscosity for Organic:Water mixtures
  if(length(ids)==2 & (all(c("meoh", "h2o") %in% ids) | all(c("mecn", "h2o") %in% ids))) {

    #Statement about calculation method
    calc_state <- paste0("\nSince the components form an organic/water mixture, bilinear interpolation of experimental data was used to calculate viscosity.")

    pref <- if(any(ids %in% "meoh")) "MeOH_" else "MeCN_"
    aqmix_df <- if(any(ids %in% "meoh")) visc_meoh_h2o else visc_mecn_h2o

    int_temps <- aqmix_df[,"T_degC"]
    trange <- range(int_temps, na.rm = TRUE)
    if(temp < trange[1] | temp > trange[2]) stop("Temperature of the binary mobile phase is outside of the permitted range!")

    int_bfracs <- if(frac_type=="vol") paste0(pref, "volfrac") else if(frac_type=="mass") paste0(pref, "massfrac") else paste0(pref, "molfrac")
    int_bfracs <- aqmix_df[,int_bfracs]
    int_viscs <- aqmix_df[,"visc_mPas"]

    known_frac <- fracs[c(which("meoh" %in% ids), which("mecn" %in% ids))]

    res_visc <- bilin_interp(x = int_temps, y = int_bfracs, z = int_viscs, xout = temp, yout = known_frac)[["z"]]
    output <- list(c(res_visc, temp, fracs))
    names(output[[1]]) <- c("visc_mPas", "temp_degC", paste0(ids, "_", frac_type, "frac"))
    names(output) <- paste0(pref,"H2O")

  } else { #FOR PURELY ORGANIC MIXTURES
    calc_state <- paste0("\nViscosities of pure components were calculated via the Vogel equation.",
                         "\nDensities were similarly calculated via the DIPPR105 equation.",
                         "\nFinally, the Linear Blend Rule was used to obtain mixture viscosity.")

    #Calculate viscosity of each pure component at temperature 'temp' via the Vogel equation
    pure_visc <- sapply(ids, function(x) { rownum <- which(id_list == x)
    cs <- unlist(visc_vogel[rownum,c("A","B","C"), drop = TRUE])
    names(cs) <- c("A","B","C")
    res <- exp(cs["A"]+(cs["B"]/(cs["C"]+temp_k)))
    return(res)
    })
    names(pure_visc) <- ids

    #Create output vectors for fractions of components
    fracs_vol <- fracs_mass <- fracs_mol <- c()

    #Retrieve RMM values
    rmms <- visc_vogel[which(id_list %in% ids),"RMM"]
    names(rmms) <- ids

    #Calculate pure substance densities using the DIPPR105 equation
    pure_dens <- sapply(ids, function(x) { rownum <- which(id_list == x)
    cs <- unlist(visc_dippr105[rownum,c("A","B","C","D"), drop = TRUE])
    names(cs) <- c("A","B","C","D")
    res <- (cs["A"]/(cs["B"]^(1+(1-temp_k/cs["C"])^cs["D"])))/1000
    return(res)
    })
    names(pure_dens) <- ids

    #Calculate dynamic viscosity based on mol fraction (convert to mol fraction if volume/mass fraction given)
    if(frac_type=="vol") {
      fracs_vol <- fracs
      fracs_mass <- sapply(seq_along(fracs_vol), function(x) (fracs_vol[x]*pure_dens[x])/sum(fracs_vol*pure_dens))
      fracs_mol <- sapply(seq_along(fracs_mass), function(x) (fracs_mass[x]/rmms[x])/sum(fracs_mass/rmms))
    } else if(frac_type == "mass") {
      fracs_mass <- fracs
      fracs_vol <- sapply(seq_along(fracs_mass), function(x) (fracs_mass[x]/pure_dens[x])/sum(fracs_mass/pure_dens))
      fracs_mol <- sapply(seq_along(fracs_mass), function(x) (fracs_mass[x]/rmms[x])/sum(fracs_mass/rmms))
    } else if(frac_type == "mol") {
      fracs_mol <- fracs
      fracs_mass <- sapply(seq_along(fracs_mol), function(x) (fracs_mol[x]*rmms[x])/sum(fracs_mol*rmms))
      fracs_vol <- sapply(seq_along(fracs_mass), function(x) (fracs_mass[x]/pure_dens[x])/sum(fracs_mass/pure_dens))
    }
    res_visc <- sum(fracs_mol*pure_visc) #LINEAR BLEND RULE
    output <- list(c(res_visc, temp, fracs_mol, fracs_mass, fracs_vol))
    names(output[[1]]) <- c("visc_mPas", "temp_degC", paste0(ids, "_molfrac"), paste0(ids, "_massfrac"), paste0(ids, "_volfrac"))
    names(output) <- paste0(ids, collapse = "_")
  }

  #Compile information about function
  compnms <- setNames(visc_vogel[,"Name"], visc_vogel[,"Short_Name"])
  fracnms <- c(vol = "Volume", mass = "Mass", mol = "Mol")
  information <- paste0("Dynamic viscosity was calculated for the ", ifelse(length(ids)==1, "single component", "mixture"), ": ", paste0(compnms[ids], " ('", ids, "')", collapse = ", "), " at ", temp, " degC.",
                        "\n", fracnms[frac_type], " fraction(s) of ", paste0(fracs, collapse = ", "), " was/were specified for ", paste0(compnms[ids], collapse=", "), ifelse(length(ids)==1, ".", " (respectively)."),
                        calc_state)

  return(list(results = output, information = information, call = cl_rec))
}
