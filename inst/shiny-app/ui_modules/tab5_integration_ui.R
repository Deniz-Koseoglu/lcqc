# ui_modules/tab5_integration_ui.R
integration_ui <- function(id, config, i18n) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab5_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Integration"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab5_HELP"))))),

      tabsetPanel(
        id = ns("integration_options"),
        tabPanel(
          value = "tab5_panel_basic",
          title = i18n$t("Basic"),
          div(

            actionButton(ns("integration_basic_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("integration_method_div"),
            uiOutput(ns("integration_method"))
          ),
          div(
            id = ns("auto_crit_width_div"),
            checkboxInput(ns("auto_crit_width_int"), i18n$t("Determine critical width automatically"), value = config$integ$manual_crit$value)
          ),
          conditionalPanel(
            condition = "!input['integration-auto_crit_width_int']",
            div(
              id = ns("manual_width_int_div"),
              textInput(ns("manual_width_int"), i18n$t("Critical Width"), value = config$integ$crit_w$value)
            )
          ),
          div(
            id = ns("apply_integration_basic_div"),
            actionButton(ns("apply_integration"), i18n$t("Apply Integration"), icon = icon("play"))
          )
        ),
        tabPanel(
          value = "tab5_panel_advanced",
          title = i18n$t("Advanced"),
          div(

            actionButton(ns("integration_advanced_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("selected_integration_method_div"),
            fluidRow(
              column(width = 12, tags$b(i18n$t("Current Method:"))),
              column(width = 12, textOutput(ns("selected_integration_method")))
            )
          ),
          hr(),
          div(
            id = ns("integration_skim_div"),
            numericInput(ns("integration_skim"), i18n$t("Skim Ratio"), value = config$integ$skim$value, step = 1)
          ),
          div(
            id = ns("integration_dyson_div"),
            numericInput(ns("integration_dyson"), i18n$t("Dyson Criterion"), value = config$integ$dyson$value, step = 1)
          ),
          div(
            id = ns("apply_integration_advanced_div"),
            actionButton(ns("apply_integration"), i18n$t("Apply Integration"), icon = icon("play"))
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("integration_output_display"),
        tabPanel(i18n$t("Integration Plot"), plotlyOutput(ns("integration_plot"))),
        tabPanel(i18n$t("Results Table"), DT::dataTableOutput(ns("integration_results_table"))),
        tabPanel(i18n$t("Summary"), htmlOutput(ns("integration_information_text")))
      )
    )
  )
}
