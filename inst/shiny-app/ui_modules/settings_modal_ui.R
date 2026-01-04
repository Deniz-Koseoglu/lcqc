# inst/shiny-app/ui_modules/settings_modal_ui.R
settings_modal_ui <- function(id, i18n) {
  ns <- NS(id)
  modalDialog(
    title = i18n$t("Manage Settings"),
    size = "m",
    easyClose = TRUE,
    fluidPage(
      tabsetPanel(
        id = ns("settings_tabs"),
        #tabPanel(i18n$t("Save"),
        #         wellPanel(
        #           tags$h4(i18n$t("Save Current Settings")),
        #           textInput(ns("save_settings_name"), i18n$t("Settings Name"), placeholder = i18n$t("e.g., my_hplc_method")),
        #           actionButton(ns("confirm_save"), i18n$t("Save"), class = "btn-primary", icon = icon("save"))
        #         )
        #),
        #tabPanel(i18n$t("Load"),
        #         wellPanel(
        #           tags$h4(i18n$t("Load Saved Settings")),
        #           uiOutput(ns("load_settings_selection_ui")), # Dynamic UI for dropdown
        #           actionButton(ns("confirm_load"), i18n$t("Load"), class = "btn-primary", icon = icon("folder-open"))
        #         )
        #),
        tabPanel(i18n$t("Import"),
                 wellPanel(
                   tags$h4(i18n$t("Import Settings from File")),
                   uiOutput(ns("import_file")), # For dynamic translation of button labels
                   #tags$hr(),
                   tags$h4(i18n$t("Select Modules to Import or Load Defaults")),
                   uiOutput(ns("module_checkboxes")), # Dynamic checkboxes for module selection
                   #tags$hr(),
                   uiOutput(ns("import_buttons")) # Dynamic buttons for language reactivity
                 )
        ),
        tabPanel(i18n$t("Export"),
                 wellPanel(
                   tags$h4(i18n$t("Export Current Settings")),
                   textInput(ns("export_filename"), i18n$t("Filename"), value = "lcqc_settings.json"),
                   downloadButton(ns("confirm_export"), i18n$t("Export"), class = "btn-primary", icon = icon("file-export"))
                 )
        )
      )
    ),
    footer = tagList(
      modalButton(i18n$t("Cancel"))
    )
  )
}
