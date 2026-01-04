#' Launch the LCQC Shiny Application
#'
#' This function launches the LCQC Shiny application packaged within the 'lcqc' package.
#'
#' @return A shiny app object. The function starts the Shiny application and
#'   does not return a value directly to the R session until the app is closed.
#' @export
#'
#' @examples
#' \dontrun{
#' # To run the application after installing the package:
#' lcqc::run_app()
#' }
run_app <- function() {
  # ... diğer kurulmuş ayarlarınız ...

  shiny::runApp(
    # Uygulama dosyanızın yolu
    app_dir <- system.file("shiny-app", package = "lcqc"),

  )
}
