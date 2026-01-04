# inst/shiny-app/ui_modules/export_modal_ui.R
export_modal_ui <- function(id,i18n) {
  ns <- NS(id) 
  modalDialog(
    title = i18n$t("Export LCQC Data and Visualizations"),
    size = "l",
    easyClose = TRUE,
    fluidPage(
      fluidRow(
        column(
          width = 6,
          wellPanel(
            tags$h4(i18n$t("Select Data to Export")),
            uiOutput(ns("export_data_selection_ui")) # Dynamic UI for checkboxes
          )
        ),
        column(
          width = 6,
          wellPanel(
            tags$h4(i18n$t("Output Settings")),
            radioButtons(
              ns("plot_format"),
              i18n$t("Plot Format:"),
              choices = c("PNG" = "png", "PDF" = "pdf"),
              selected = "png"
            ),
            textInput(ns("export_path"), i18n$t("Export Directory"), value = getwd()),
            shinyDirButton(
              ns("browse_export_path"), i18n$t("Browse"),
              i18n$t("Select export directory")
            )
          )
        )
      )
    ),
    footer = tagList(
      modalButton(i18n$t("Cancel")),
      actionButton(ns("confirm_export"), i18n$t("Export"), class = "btn-primary")
    )
  )
}
