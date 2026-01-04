intro_steps_current_tab8 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='perf']",
        intro = i18n$t("This module allows you to calculate various performance metrics for your HPLC column, including theoretical plates,
                       resolution, retention factor, separation factor, and additional column-specific metrics."),
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

intro_steps_tab8_tplate <- function(ns, i18n, input) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_method_div")),
      intro = i18n$t("Select the method(s) for calculating the number of theoretical plates (N)."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_len_div")),
      intro = i18n$t("Enter the column length in millimeters."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_dp_div")),
      intro = i18n$t("Enter the stationary phase particle size in micrometers."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_show_widths_div")),
      intro = i18n$t("Include peak width measurements in the results table."),
      position = "auto"
    )
  ))

  # Auto crit width toggle
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width4_div")),
      intro = i18n$t("Choose whether critical width should be determined automatically."),
      position = "auto"
    )
  ))

  # Manual crit width
  if (isFALSE(input$auto_crit_width_tplate)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("tplate_crit_w_div")),
        intro = i18n$t("Enter a manual critical width value."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_deltap_div")),
      intro = i18n$t("Enter the observed back pressure."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_visc_div")),
      intro = i18n$t("Enter the mobile phase viscosity."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_t0_mode_div")),
      intro = i18n$t("Select how dead time (t₀) will be determined."),
      position = "auto"
    )
  ))

  if (input$tplate_t0_mode == "manual") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("tplate_t0_manual_div")),
        intro = i18n$t("Enter the column dead time manually."),
        position = "auto"
      )
    ))
  }

  if (input$tplate_t0_mode == "peak") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("tplate_t0_peak_index_div")),
        intro = i18n$t("Specify which peak represents the dead time."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_imped_met_div")),
      intro = i18n$t("Select methods for calculating separation impedance."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_tplate_div")),
      intro = i18n$t("Calculate theoretical plates and related metrics."),
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


intro_steps_tab8_res <- function(ns, i18n, input) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("res_peaks1_manual_div")),
      intro = i18n$t("Select the first set of peaks."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("res_peaks2_manual_div")),
      intro = i18n$t("Select the second set of peaks."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("res_peaks_modal_btn_div")),
      intro = i18n$t("Interactively select peaks to use for the calculation."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("res_method_div")),
      intro = i18n$t("Choose the resolution calculation method."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("res_t0_mode_div")),
      intro = i18n$t("Choose how dead time (t₀) is determined."),
      position = "auto"
    )
  ))

  if (input$res_t0_mode == "manual") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("res_ks_manual_div")),
        intro = i18n$t("Enter dead time manually."),
        position = "auto"
      )
    ))
  }

  if (input$res_t0_mode == "peak") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("res_ks_peak_index_div")),
        intro = i18n$t("Select the peak representing dead time."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width5_div")),
      intro = i18n$t("Toggle automatic critical width."),
      position = "auto"
    )
  ))

  if (isFALSE(input$auto_crit_width_res)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("res_crit_w_div")),
        intro = i18n$t("Enter a manual critical width."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_resolution_div")),
      intro = i18n$t("Calculate chromatographic resolution."),
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


intro_steps_tab8_retf <- function(ns, i18n, input) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("retf_t0_mode_div")),
      intro = i18n$t("Choose how dead time (t₀) is determined."),
      position = "auto"
    )
  ))

  if (input$retf_t0_mode == "manual") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("retf_t0_manual_div")),
        intro = i18n$t("Enter the dead time manually."),
        position = "auto"
      )
    ))
  }

  if (input$retf_t0_mode == "peak") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("retf_t0_peak_index_div")),
        intro = i18n$t("Select the peak used to estimate dead time."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("retf_peaks_manual_div")),
      intro = i18n$t("Select the peaks used to calculate the retention factor (k′)."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("retf_peaks_modal_btn_div")),
      intro = i18n$t("Interactively select peaks to use for the calculation."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width6_div")),
      intro = i18n$t(
        "Choose whether the critical width should be determined automatically or entered manually."
      ),
      position = "auto"
    )
  ))

  # Manual critical width (only if visible)
  if (isFALSE(input$auto_crit_width_retf)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("retf_crit_w_div")),
        intro = i18n$t(
          "Enter a manual value for the critical width."
        ),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_retention_factor_div")),
      intro = i18n$t("Calculate the retention factor."),
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


intro_steps_tab8_sepf <- function(ns, i18n, input) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("sepf_peaks1_manual_div")),
      intro = i18n$t("Select the first peak(s) for separation factor calculation."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("sepf_peaks2_manual_div")),
      intro = i18n$t("Select the second peak(s) for separation factor calculation."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("sepf_peaks_modal_btn_div")),
      intro = i18n$t("Interactively select peaks to use for the calculation."),
      position = "auto"
    )
  ))

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("sepf_t0_mode_div")),
      intro = i18n$t("Choose how dead time (t₀) is determined."),
      position = "auto"
    )
  ))

  if (input$sepf_t0_mode == "manual") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("sepf_ks_manual_div")),
        intro = i18n$t("Enter the dead time manually."),
        position = "auto"
      )
    ))
  }

  if (input$sepf_t0_mode == "peak") {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("sepf_ks_peak_index_div")),
        intro = i18n$t("Select the peak representing dead time."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width7_div")),
      intro = i18n$t(
        "Choose whether the critical width should be determined automatically or entered manually."
      ),
      position = "auto"
    )
  ))

  # Manual critical width (only if visible)
  if (isFALSE(input$auto_crit_width_sepf)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("sepf_crit_w_div")),
        intro = i18n$t(
          "Enter a manual value for the critical width."
        ),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_separation_factor_div")),
      intro = i18n$t("Calculate the separation factor."),
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


intro_steps_tab8_addmets <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("addmets_which_mets_div")),
        intro = i18n$t("Select which additional column-specific performance metrics to calculate."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_t0_div")),
        intro = i18n$t("Enter the dead time (t0) of the column in minutes. This is required for linear velocity and permeability calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_len_div")),
        intro = i18n$t("Enter the length of your chromatographic column in millimeters (mm). This is required for linear velocity and permeability calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_flow_div")),
        intro = i18n$t("Enter the flow rate of the mobile phase in mL/min. This is required for packing porosity, specific permeability, and flow resistance calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_id_div")),
        intro = i18n$t("Enter the internal diameter of the column in millimeters (mm). This is required for packing porosity, specific permeability, and flow resistance calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_deltap_div")),
        intro = i18n$t("Enter the back pressure observed for the column in bar. This is required for permeability, specific permeability, and flow resistance calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_visc_div")),
        intro = i18n$t("Enter the dynamic viscosity of the mobile phase in mPas. This is required for specific permeability and flow resistance calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("addmets_dp_div")),
        intro = i18n$t("Enter the stationary phase particle size in micrometers (µm). This is required for flow resistance calculations."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("apply_addmets_div")),
        intro = i18n$t("Click this button to calculate the selected additional column performance metrics."),
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


intro_steps_tab8_visc <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("visc_which_solv_div")),
        intro = i18n$t("Select mobile phase mixture components to calculate viscosity for."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("viscfrac_inputs_div")),
        intro = i18n$t("Enter the fraction(s) of specified mobile phase components within the mixture."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("visc_fractype_div")),
        intro = i18n$t("Specify the type of fraction to use. Choices include volumetric, mass, or mole fractions."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("visc_temp_div")),
        intro = i18n$t("Enter the temperature at which to calculate dynamic viscosity."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("visc_calc_div")),
        intro = i18n$t("Click this button to calculate dynamic viscosity."),
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
