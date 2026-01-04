intro_steps_current_tab9 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='rpr']",
        intro = i18n$t("This module allows you to generate a comprehensive HPLC column performance report in PDF format."),
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

intro_steps_tab9_report_options <- function(ns, i18n) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("which_chart_div")),
      intro = i18n$t("Select which type of chromatogram chart to include in the report."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("peak_indices_div")),
      intro = i18n$t("Manually or interactively select specific peaks to include in the report."),
      position = "auto"
    ),

    list(
      element = paste0("#", ns("pnms_div")),
      intro = i18n$t("Enter the names of the peaks to be displayed."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("pconcs_div")),
      intro = i18n$t("Enter the concentrations corresponding to the selected peaks."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("unitconc_div")),
      intro = i18n$t("Enter the concentration units to display in the report."),
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

intro_steps_tab9_metrics_specifications <- function(ns, i18n) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("tplate_inputs_div")),
      intro = i18n$t("Select which theoretical plate metrics to use and specify their QC limits."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("asymmetry_inputs_div")),
      intro = i18n$t("Select which asymmetry metrics to use and specify their QC limits.
                     If available, the inclusion of Total Peak Analysis (TPA) plots may also be toggled here."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("add_addmets_div")),
      intro = i18n$t("Select which additional performance metrics to include."),
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


intro_steps_tab9_report_content <- function(ns, i18n) {

  list(
    steps = list(
      list(element = paste0("#", ns("document_number_div")),
           intro = i18n$t("Enter the document number for the report."),
           position = "auto"),
      list(element = paste0("#", ns("operator_div")),
           intro = i18n$t("Enter the name of the operator."),
           position = "auto"),
      list(element = paste0("#", ns("column_serial_number_div")),
           intro = i18n$t("Enter the column serial number."),
           position = "auto"),
      list(element = paste0("#", ns("column_part_number_div")),
           intro = i18n$t("Enter the column part number."),
           position = "auto"),
      list(element = paste0("#", ns("column_description_div")),
           intro = i18n$t("Provide a brief description of the column."),
           position = "auto"),
      list(element = paste0("#", ns("batch_number_div")),
           intro = i18n$t("Enter the batch number."),
           position = "auto"),
      list(element = paste0("#", ns("mobile_phase_div")),
           intro = i18n$t("Enter the mobile phase composition."),
           position = "auto"),
      list(element = paste0("#", ns("shipment_phase_div")),
           intro = i18n$t("Enter the shipment solvent composition."),
           position = "auto"),
      list(element = paste0("#", ns("back_pressure_div")),
           intro = i18n$t("Enter the back pressure."),
           position = "auto"),
      list(element = paste0("#", ns("flow_rate_div")),
           intro = i18n$t("Enter the flow rate."),
           position = "auto"),
      list(element = paste0("#", ns("column_temperature_div")),
           intro = i18n$t("Enter the column temperature."),
           position = "auto"),
      list(element = paste0("#", ns("injection_volume_div")),
           intro = i18n$t("Enter the injection volume."),
           position = "auto")
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


intro_steps_tab9_output_settings <- function(ns, i18n, input) {

  steps <- list()

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("asprat_div")),
      intro = i18n$t("Adjust the chromatogram aspect ratio."),
      position = "auto"
    )
  ))

  if (isTRUE(input$add_tpa)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("asprat_tpa_div")),
        intro = i18n$t("Adjust the aspect ratio for TPA plots."),
        position = "auto"
      )
    ))
  }

  steps <- append(steps, list(
    list(
      element = paste0("#", ns("fontsize_div")),
      intro = i18n$t("Set the report font size."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("clogo_div")),
      intro = i18n$t("Upload a custom logo."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("expath_div")),
      intro = i18n$t("Choose the export directory."),
      position = "auto"
    ),
    list(
      element = paste0("#", ns("generate_report_div")),
      intro = i18n$t("Generate the PDF report."),
      position = "auto"
    )#,
    # list(
    #   element = paste0("#", ns("render_log_div")),
    #   intro = i18n$t("View report generation messages."),
    #   position = "auto"
    # )
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
