# ui_modules/status_bar_ui.R
status_bar_ui <- function(id,i18n) {
  ns <- NS(id) # Create a namespace function

  div(
    class = "status-bar", # Add a CSS class for styling
    fluidRow(align = "left",
      column(12,
             div(class = "status-item",
                 icon("file-alt"), # Icon
                 tags$span(style="margin-left:5px", class = "status-subtitle", i18n$t("Current File:")), # Subtitle
                 tags$span(id = ns("current_file_name"), class = "status-value",style="font-weight: bold;color:orange", uiOutput(ns("current_file_name"), inline = TRUE)) # Value
             )
      ),
      column(12,
             div(class = "status-item",
                 icon("cogs"), # Icon
                 tags$span(style="margin-left:5px", class = "status-subtitle", i18n$t("Loaded Settings:")), # Subtitle
                 tags$span(id = ns("current_settings_name") ,style="font-weight: bold;color:orange", class = "status-value", uiOutput(ns("current_settings_name"), inline = TRUE)) # Value
             )
      )
    )
  )
}
