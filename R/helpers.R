#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AUX. FUNCTIONS to check odd vs even numbers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find whether a number is even or odd
#'
#' @description Returns a logical values based on whether an input number is even or odd.
#'
#' @param x Single numeric integer value.
#'
#' @return A TRUE/FALSE depending on whether the input is an odd (for \code{is.odd}) or even (for \code{is.even}) number.
#' @export
#'
#' @examples
#' is.even(1)
#' is.odd(3)
is.even <- function(x) { x %% 2 == 0 }

#' @rdname is.even
#' @export
is.odd <- function(x) { x %% 2 != 0 }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AUX. FUNCTIONS: Finding roots in c(x,y) data via linear interpolation and finding roots in spline function output via non-linear interpolation
#From: https://stackoverflow.com/questions/52655729/get-x-value-given-y-value-general-root-finding-for-linear-non-linear-interpol
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find roots via linear or non-linear interpolation
#'
#' @description By default, function \code{linspline} finds the zero-crossing (i.e. root) of a linear interpolation function.
#' Similarly, \code{cubspline} finds the root of a cubic spline (output from \code{stats::splinefun}).
#'
#' @param x The known x values (only for \code{linspline}).
#' @param y The known y values (only for \code{linspline}).
#' @param f Cubic spline results obtained from \code{stats::splinefun}.
#' @param y0 The y value for which an x value is to be found (defaults to 0, indicating the root is to be found).
#' @param verbose A logical which determines whether the results are plotted using \code{plot()}.
#'
#' @return A numeric vector of intersection point(s) along the x-axis.
#' @author Zheyuan Li (see also \href{https://stackoverflow.com/questions/52655729/get-x-value-given-y-value-general-root-finding-for-linear-non-linear-interpol}{here})
#' @export
#'
#' @examples
#' set.seed(0)
#' x <- 1:10 + runif(10, -0.1, 0.1)
#' y <- rnorm(10, 3, 1)
#' f <- stats::splinefun(x, y, method = "fmm")
#' linspline(x, y, y0 = 2.85, verbose = FALSE)
#' cubspline(f, y0 = 2.85, verbose = FALSE)
linspline <- function (x, y, y0 = 0, verbose = TRUE) {
  if (is.unsorted(x)) {
    ind <- order(x)
    x <- x[ind]; y <- y[ind]
  }
  z <- y - y0
  ## which piecewise linear segment crosses zero?
  k <- which(z[-1] * z[-length(z)] <= 0)
  ## analytical root finding
  xr <- x[k] - z[k] * (x[k + 1] - x[k]) / (z[k + 1] - z[k])
  ## make a plot?
  if (verbose) {
    plot(x, y, "l"); abline(h = y0, lty = 2)
    points(xr, rep.int(y0, length(xr)))
  }
  ## return roots
  xr
}

#' @rdname linspline
#' @importFrom graphics abline
#' @importFrom graphics curve
#' @importFrom graphics points
#' @export
cubspline <- function (f, y0 = 0, verbose = TRUE) {
  ## extract piecewise construction info
  info <- environment(f)$z
  n_pieces <- info$n - 1L
  x <- info$x; y <- info$y
  b <- info$b; c <- info$c; d <- info$d
  ## list of roots on each piece
  xr <- vector("list", n_pieces)
  ## loop through pieces
  i <- 1L
  while (i <= n_pieces) {
    ## complex roots
    croots <- polyroot(c(y[i] - y0, b[i], c[i], d[i]))
    ## real roots (be careful when testing 0 for floating point numbers)
    rroots <- Re(croots)[round(Im(croots), 10) == 0]
    ## the parametrization is for (x - x[i]), so need to shift the roots
    rroots <- rroots + x[i]
    ## real roots in (x[i], x[i + 1])
    xr[[i]] <- rroots[(rroots >= x[i]) & (rroots <= x[i + 1])]
    ## next piece
    i <- i + 1L
  }
  ## collapse list to atomic vector
  xr <- unlist(xr)
  ## make a plot?
  if (verbose) {
    curve(f, from = x[1], to = x[n_pieces + 1], xlab = "x", ylab = "f(x)")
    abline(h = y0, lty = 2)
    points(xr, rep.int(y0, length(xr)))
  }
  ## return roots
  xr
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NOT EXPORTED!
# AUX. FUNCTION: Moore-Penrose generalized inverse of a complex matrix
# FROM PACKAGE: pracma
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Moore-Penrose Gen. Inverse of a Matrix
#' @references https://CRAN.R-project.org/package=pracma
#' @keywords internal
#' @noRd
moorepen <- function (A, tol = .Machine$double.eps^(2/3))
{
  stopifnot(is.numeric(A) || is.complex(A), is.matrix(A))
  s <- svd(A)
  if (is.complex(A))
    s$u <- Conj(s$u)
  p <- (s$d > max(tol * s$d[1], 0))
  if (all(p)) {
    mp <- s$v %*% (1/s$d * t(s$u))
  }
  else if (any(p)) {
    mp <- s$v[, p, drop = FALSE] %*% (1/s$d[p] * t(s$u[,
                                                       p, drop = FALSE]))
  }
  else {
    mp <- matrix(0, nrow = ncol(A), ncol = nrow(A))
  }
  return(mp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AUX. FUCTIONS: Calculating slope and intercept of a linear function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Slope and intercept of a linear function
#'
#' @description Functions to calculate the \code{slope} and \code{intercept} of a line.
#'
#' @param x Numeric vector of known x values.
#' @param y Numeric vector of Known y values.
#'
#' @return A numeric value of the linear gradient/slope.
#' @export
#'
#' @examples
#' with(CO2, slope(conc, uptake))
#' with(CO2, intercept(conc, uptake))
#' @seealso \url{https://stackoverflow.com/questions/66771929/intercept-and-slope-functions-in-r}
#' @importFrom stats cov
#' @importFrom stats var
slope <- function(x, y) { cov(x, y) / var(x) }

#' @rdname slope
#' @export
intercept <- function(x, y) { mean(y) - slope(x, y) * mean(x) }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AUX. FUNCTION: Calculate intersection point between two lines defined by slope and intercept
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Find intersection between two lines
#'
#' @description Locates the intersection point of two lines characterized by a slope and intercept.
#'
#' @param l1 Line 1 characterized by slope and intercept. A numeric vector of length 2 in the format: c(intercept, slope).
#' @param l2 Line 2 characterized by slope and intercept. Format is identical to that of \code{l1}.
#'
#' @return A numeric vector with \code{c(x,y)} coordinates of the intersection point (if any).
#' @export
#'
#' @seealso \code{\link{slope}}, \code{\link{intercept}}
#'
#' @examples
#' #Define two lines by slope and intercept
#' #Format: c(intercept, slope)
#' l1 <- c(10, -2)
#' l2 <- c(0, 2)
#' isect <- insect(l1, l2)
insect <- function(l1, l2) {
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(c(x = x, y = y))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AUX. FUNCTION: Check suitability of input data from chrom_detect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Check suitability of input data
#'
#' @description \strong{This function is not exported}.
#'
#' Check suitability of input data (must be output from \code{chrom_detect}).
#'
#' @param dt Data object output from \code{chrom_detect}
#'
#' @return Nothing.
#' @keywords internal
chkdt <- function(dt) {
  if((length(dt)==9 & (!is.list(dt) | !all(c("Chromatogram", "Derivative_Noise", "Peaks", "Peak_Extents", "Amplitude_Limit", "Derivative_Limits", "Zscore_Limits", "information", "call") %in% names(dt)))) |
     (length(dt)==2 & (!is.list(dt[["results"]]) | !all(c("Chromatogram", "Derivative_Noise", "Peaks", "Peak_Extents", "Amplitude_Limit", "Derivative_Limits", "Zscore_Limits", "information", "call") %in% names(dt[["results"]]))))) {
    stop("The input data must be the output of function 'chrom_detect'!")
  } else if(length(dt)==2 & all(c("results", "plots") %in% names(dt))) dt <- dt[["results"]]
  return(dt)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AUX. FUNCTION: Print information about function run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Print information about function run
#'
#' @description Prints the contents of \code{$information} contained in the output of various \pkg{lcqc} functions.
#'
#' @param x Output from one of: \code{chrom_detect}, \code{chrom_skim}, \code{chrom_icf}, \code{chrom_visc},
#' \code{chrom_tplate}, \code{chrom_asym}, \code{chrom_retf}, \code{chrom_sepf}, \code{chrom_res}, or \code{chrom_addmets}.
#'
#' @return A \code{cat} printout in the console.
#' @export
#'
#' @examples
#' \dontrun{
#' cprint(lcqc:::wf_detpeaks)
#' }
cprint <- function(x) {
  if(!any(names(x) %in% "information") & !any(names(x[["results"]]) %in% "information")) stop("There must be an element called 'information' in the input data!")
  if(any(names(x) %in% "information")) cat(x[["information"]]) else cat(x[["results"]][["information"]])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: Supplement function arguments (named vectors) with default values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Supplement function arguments (named vectors) with default values
#'
#' @description Compares an input \strong{named} vector of parameters with a default vector,
#' supplementing with default parameters where necessary.
#' \strong{This function is not exported}.
#'
#' @param defpars A \strong{named} vector of default parameters (numeric or character).
#' @param pars A \strong{named} vector of input parameters where all names must correspond to those of \code{defpars}.
#' @param parlb The label for parameters. If \code{NA} (default), defaults to \code{"pars"}.
#'
#' @return The input vector supplemented with default parameters (if necessary).
#'
supp_pars <- function(defpars, pars = "default", parlb = NA) {

  #Preliminary checks
  mlst <- list(pars, defpars)
  if(!any(pars %in% "default") & any(unlist(lapply(mlst, function(x) is.null(names(x)))))) stop("Arguments 'pars' and 'defpars' must both be named vectors!")
  if(!all(sapply(mlst, is.atomic))) stop("Arguments 'pars' and 'defpars' must both be atomic vectors!")
  if(is.na(parlb)) parlb <- "pars"

  #Processing
  posnms <- paste0("'", names(defpars), "'", collapse = ", ")
  if(!any(pars %in% "default")) {
    if(is.null(names(pars))) stop(paste0("The vector '", parlb, "' must be named! Possible names are: " , posnms, "."))
    if(!all(names(defpars) %in% names(pars))) {
      if(length(which(!names(pars) %in% names(defpars)))>0) cat("\nSome of the names provided to '", parlb, "' were not recognized. Possible names are: " , posnms, ".", sep = "")
      abs_pars <- which(!names(defpars) %in% names(pars))
      if(length(abs_pars)>0) cat("\nAdding the following missing parameters to '", parlb, "': ", paste0("'", names(defpars)[abs_pars], "'", collapse = ", "),  "...", sep = "")
      pars <- append(pars, defpars[abs_pars])[names(defpars)]
    }
  } else pars <- defpars
  return(pars)
}
