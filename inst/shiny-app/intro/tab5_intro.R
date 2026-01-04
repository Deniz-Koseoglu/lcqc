intro_steps_current_tab5 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='integ']",
        intro = i18n$t("This module allows you to perform peak integration on your chromatographic data."),
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

intro_steps_tab5_basic <- function(ns, i18n, input) {

  steps <- list()

  # Integration method
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("integration_method_div")),
      intro = i18n$t("Select the desired integration method from the dropdown. Options include Gaussian skim, perpendicular drop, tangent skim, and exponential skim."),
      position = "auto"
    )
  ))

  # Automatic critical width toggle
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("auto_crit_width_div")),
      intro = i18n$t("Choose whether the critical width should be determined automatically or specified manually."),
      position = "auto"
    )
  ))

  # Manual critical width (only when visible)
  if (isFALSE(input$auto_crit_width_int)) {
    steps <- append(steps, list(
      list(
        element = paste0("#", ns("manual_width_int_div")),
        intro = i18n$t("Enter a manual value for the critical width used during integration."),
        position = "auto"
      )
    ))
  }

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_integration_basic_div")),
      intro = i18n$t("Click this button to apply the selected integration settings to your data."),
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

intro_steps_tab5_advanced <- function(ns, i18n) {

  steps <- list()

  # Selected method display
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("selected_integration_method_div")),
      intro = i18n$t("This displays the currently selected integration method from the Basic tab."),
      position = "auto"
    )
  ))

  # Skim ratio
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("integration_skim_div")),
      intro = i18n$t("Adjust the Skim Ratio criterion to control how aggressively parent-child peak pairs are skimmed."),
      position = "auto"
    )
  ))

  # Dyson criterion
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("integration_dyson_div")),
      intro = i18n$t("Set the Dyson Criterion used to resolve overlapping peaks during integration."),
      position = "auto"
    )
  ))

  # Apply button
  steps <- append(steps, list(
    list(
      element = paste0("#", ns("apply_integration_advanced_div")),
      intro = i18n$t("Apply the advanced integration parameters to process your data."),
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
