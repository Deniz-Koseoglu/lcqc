intro_steps_current_tab4 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='pdet']",
        intro = i18n$t("This module is used for flexible, derivative-based peak detection using the optionally pre-processed chromatographic signal.
                       Accurate peak apices, inflection points, upslope points, and peak boundaries are detected using various algorithms."),
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

intro_steps_tab4_basic <- function(ns, i18n, input) {

  steps <- list()

  # Amplitude threshold selection
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("amp_thres_selection_div")),
      intro = i18n$t("Choose whether to use an automatic or manual amplitude threshold for peak detection."),
      position = "auto"
    )
  ))

  # Automatic amplitude threshold method
  if (isFALSE(input$use_manual_amp_thres)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("amp_thres_method_div")),
        intro = i18n$t("Select the automatic method used to determine the amplitude threshold."),
        position = "auto"
      )
    ))
  }

  # Manual amplitude threshold
  if (isTRUE(input$use_manual_amp_thres)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("manual_amp_thres_div")),
        intro = i18n$t("Manually specify a fixed amplitude threshold for peak detection."),
        position = "auto"
      )
    ))
  }

  # Amplitude fraction (quant / diff only)
  if (!is.null(input$amp_thres_method) &&
      grepl("quant|diff", input$amp_thres_method)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("ampfrac_advanced_div")),
        intro = i18n$t("Adjust the amplitude fraction used by the selected thresholding method."),
        position = "auto"
      )
    ))
  }

  # Peak bunching
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("det_bunch_div")),
      intro = i18n$t("Enable detection and mitigation of closely eluting (bunched) peaks."),
      position = "auto"
    )
  ))

  # Derivative threshold methods
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("der_thres_method1_div")),
      intro = i18n$t("Select the primary derivative-based method for detecting peak boundaries."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("der_thres_method2_div")),
      intro = i18n$t("Optionally combine a second derivative-based method to improve robustness."),
      position = "auto"
    )
  ))

  # Peak rejection filters
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("peak_rejection_filters_div")),
      intro = i18n$t("Define filters to reject peaks based on signal-to-noise ratio, height, width, or area."),
      position = "auto"
    )
  ))

  # Peak rejection logic
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("peak_rejection_logic_div")),
      intro = i18n$t("Specify how the rejection filters are combined logically when
                     determining which peaks to discard. Height and S/N ratio filters are
                     applied before peak classification, while peak width and area filters
                     are applied afterwards."),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_peak_detection_basic_div")),
      intro = i18n$t("Apply the basic peak detection parameters and visualize the detected peaks."),
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

intro_steps_tab4_advanced <- function(ns, i18n, input) {

  steps <- list()

  # Sensitivity parameters (section title is meaningful)
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("sensitivity_parameters_div")),
      intro = i18n$t("Adjust sensitivity parameters that control how aggressively peaks are detected."),
      position = "auto"
    )
  ))

  # Z-score parameters (conditional)
  if (!is.null(input$amp_thres_method) &&
      grepl("zscore", input$amp_thres_method)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("zscore_parameters_div")),
        intro = i18n$t("Configure Z-score specific parameters used for amplitude thresholding."),
        position = "auto"
      )
    ))
  }

  # FastChrom toggle
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("enable_fast_chrom_div")),
      intro = i18n$t("Enable or disable FastChrom baseline correction for complex chromatograms."),
      position = "auto"
    )
  ))

  # FastChrom critical width
  if (isTRUE(input$enable_fast_chrom)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("fast_chrom_critical_width_div")),
        intro = i18n$t("Specify the critical width used by FastChrom for baseline correction."),
        position = "auto"
      )
    ))
  }

  # Boundary verification (use section header div)
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("Boundary_verification_parameters_div")),
      intro = i18n$t("Configure parameters that verify peak boundaries using signal and derivative confirmation."),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_peak_detection_advanced_div")),
      intro = i18n$t("Apply all advanced peak detection parameters and update the results."),
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
