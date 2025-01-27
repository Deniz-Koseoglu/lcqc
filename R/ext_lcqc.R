#' @title Browse external package data.
#'
#' @description Lists paths or filenames of external raw data files (such as .csv and .txt) included with the \pkg{lcqc} package.
#'
#' @param full.path A \code{logical} switch. Should full paths to external files be returned? Defaults to \code{TRUE}.
#' @param txt_only A \code{logical} switch.. Should only \strong{.txt} files be listed? When \code{FALSE}, all external files included with the package are listed.
#'
#' @return Filenames (or file paths if \code{full.path = TRUE}) of external data files included with the package.
#' @export
#'
#' @examples ext_lcqc(full.path = FALSE)

ext_lcqc <- function(full.path = TRUE, txt_only = TRUE) {
  patt <- if(txt_only) "\\.txt$" else "*"
  return(list.files(system.file("extdata", package = "lcqc"), pattern = patt, full.names = full.path))
}
