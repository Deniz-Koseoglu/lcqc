#' @title Check for and install missing packages
#'
#' @description Checks a character vector of package names, detects any CRAN packages not currently installed, installs, and loads them.
#'
#' @param packlist A list of packages to check for. Packages not found are automatically installed and called.
#'
#' @return Nothing.
#'
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @export
#'
#' @examples
#' \dontrun{
#' chk_pack(c("ggplot2","knitr"))
#' }

chk_pack <- function(packlist) {
  if(!is.character(packlist)) stop("The list of packages must be a character value/vector!")
  newpacks <- packlist[!(packlist %in% installed.packages()[,"Package"])]

  #Work on exceptions
  #Exception 1: 'depict' package
  if("depict" %in% packlist) {
    newpacks <- newpacks[!newpacks %in% "depict"]
    devtools::install_github("CDK-R/depict")
  }

  #Install packages
  if(length(newpacks)>0) install.packages(newpacks)
  #Load packages
  invisible(lapply(packlist, require, character.only = TRUE))
}
