intro_steps_current_tab3 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='smooth']",
        intro = i18n$t("This module is used to apply smoothing methods to refine your chromatogram signal."),
        position = "bottom"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE
  )
}

intro_steps_tab3_basic <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("signal_smoothing_method_div")),
        intro = i18n$t("Select a signal smoothing method from this dropdown. Options include 'None', 'Rectangular', 'Triangular', and two types of Savitzky-Golay filters. Your selection here will determine the smoothing algorithm applied."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("deriv_smoothing_method_div")),
        intro = i18n$t("Select a derivative smoothing method from this dropdown. Options include 'None', 'Rectangular', or 'Triangular'. Your selection here will determine the smoothing algorithm applied."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("apply_smoothing_div")),
        intro = i18n$t("After selecting your desired method, click this button to apply smoothing. If 'None' is selected, this button will allow you to skip the smoothing step and proceed to peak detection."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

#CONDITIONAL TOUR
intro_steps_tab3_advanced <- function(ns, i18n, input) { #PASSING INPUT IS REQUIRED TO EVALUATE CONDITIONS

  steps <- list()

  # Always visible
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("selected_smoothing_method_div")),
      intro = i18n$t("This displays the currently selected smoothing method. You can change the method in the 'Basic' tab."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("basic_auto_smoothing_div")),
      intro = i18n$t("Toggle this on to automatically estimate the optimal number of smoothing points and passes based on average peak width and other factors."),
      position = "auto"
    )
  ))

  #Conditional
  if(!input$basic_auto_smoothing) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("manual_smoothing_div")),
        intro = i18n$t("Manually adjust smoothing points and passes."),
        position = "auto"
      )
    ))
  } else {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("auto_smoothing_div")),
        intro = i18n$t("Configure the starting parameters for auto-smoothing."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_smoothing_advanced_div")),
      intro = i18n$t("Apply or skip smoothing using these settings."),
      position = "auto"
    )
  ))

  list(
    steps = steps,
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}
