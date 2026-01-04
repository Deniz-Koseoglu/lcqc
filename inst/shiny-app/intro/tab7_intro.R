intro_steps_current_tab7 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='psym']",
        intro = i18n$t("This module allows you to calculate various peak symmetry metrics for your chromatographic data."),
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

intro_steps_tab7_basic <- function(ns, i18n, input) {

  steps <- list()

  # Symmetry method
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("symmetry_method_div")),
      intro = i18n$t(
        "Select the method(s) for calculating peak asymmetry, such as USP Tailing Factor (Tf), Asymmetry Factor (As), or Total Peak Analysis (TPA)."
      ),
      position = "auto"
    )
  ))

  # Peaks to model
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("peaks_to_model_div")),
      intro = i18n$t(
        "Specify which detected peaks should be included in the symmetry analysis, using ranges or comma-separated indices."
      ),
      position = "auto"
    )
  ))

  # Auto critical width toggle
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width3_div")),
      intro = i18n$t(
        "Choose whether the critical width used to define peak boundaries should be determined automatically or entered manually."
      ),
      position = "auto"
    )
  ))

  # Manual critical width (only if visible)
  if (isFALSE(input$auto_crit_width_symm)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("symmetry_crit_w_div")),
        intro = i18n$t(
          "Enter a manual value for the critical width used to determine peak boundaries."
        ),
        position = "auto"
      )
    ))
  }

  # Show widths
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("show_widths_div")),
      intro = i18n$t(
        "Enable this option to include peak half-widths and related measurements in the results table."
      ),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_symmetry_basic_div")),
      intro = i18n$t(
        "Click this button to apply the selected peak symmetry calculations to your data."
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

intro_steps_tab7_advanced <- function(ns, i18n) {

  steps <- list()

  # Optimization method
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("symmetry_optmet_div")),
      intro = i18n$t(
        "Select the optimization method used for iterative Gaussian fitting, particularly relevant for Total Peak Analysis (TPA)."
      ),
      position = "auto"
    )
  ))

  # Representation resolution
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("symmetry_reprs_div")),
      intro = i18n$t(
        "Adjust the resolution used when rendering peak models and plots. Higher values produce smoother curves."
      ),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_symmetry_advanced_div")),
      intro = i18n$t(
        "Apply the advanced peak symmetry settings to model your data."
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
