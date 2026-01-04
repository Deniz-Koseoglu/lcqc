intro_steps_current_tab6 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='dconv']",
        intro = i18n$t("This module allows you to perform peak deconvolution on your chromatographic data."),
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


intro_steps_tab6_basic <- function(ns, i18n, input) {

  steps <- list()

  # Deconvolution method
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("deconvolution_method_div")),
      intro = i18n$t(
        "Select one or more deconvolution methods. 'All Available' is recommended to automatically pick the best-performing model."
      ),
      position = "auto"
    )
  ))

  # Auto critical width toggle
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width2_div")),
      intro = i18n$t(
        "Choose whether the critical width used for deconvolution should be determined automatically or entered manually."
      ),
      position = "auto"
    )
  ))

  # Manual critical width (only if visible)
  if (isFALSE(input$auto_crit_width_decon)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("manual_width_decon_div")),
        intro = i18n$t(
          "Enter a manual value for the critical width used to define peak groups for deconvolution."
        ),
        position = "auto"
      )
    ))
  }

  # Model baseline-resolved peaks
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("deconvolution_modres_div")),
      intro = i18n$t(
        "Enable this option to apply deconvolution to peaks that are already baseline-resolved."
      ),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_deconvolution_basic_div")),
      intro = i18n$t(
        "Click this button to apply the selected deconvolution settings to your data."
      ),
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


intro_steps_tab6_advanced <- function(ns, i18n) {

  steps <- list()

  # Selected methods display
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("selected_deconvolution_method_display_div")),
      intro = i18n$t(
        "This displays the deconvolution method(s) currently selected in the Basic tab."
      ),
      position = "auto"
    )
  ))

  # Optimization method
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("deconvolution_optmet_div")),
      intro = i18n$t(
        "Choose the optimization method(s) used for iterative curve fitting during deconvolution."
      ),
      position = "auto"
    )
  ))

  # EMG representation
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("deconvolution_reprs_emg_div")),
      intro = i18n$t(
        "Select how Exponentially-Modified Gaussian (EMG) peaks are represented in the deconvolution model."
      ),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_deconvolution_advanced_div")),
      intro = i18n$t(
        "Apply the advanced deconvolution parameters to process your data."
      ),
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
