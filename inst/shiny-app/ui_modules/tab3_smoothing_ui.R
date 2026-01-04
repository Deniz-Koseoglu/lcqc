# ui/tab3_smoothing_ui.R
smoothing_ui <- function(id, config, i18n) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab3_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Smoothing"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab3_HELP"))))),

      tabsetPanel(
        id = "smooting_options",
        tabPanel(
          value = "tab3_panel_basic",
          title = i18n$t("Basic"),
          div(

            actionButton(ns("smoothing_basic_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("signal_smoothing_method_div"),
            uiOutput(ns("signal_smoothing_method"))
          ),
          div(
            id = ns("deriv_smoothing_method_div"),
            uiOutput(ns("deriv_smoothing_method"))
          ),
          div(
            id = ns("apply_smoothing_div"),
            # Button for "None" method
            conditionalPanel(
              condition = "input['smoothing-signal_smoothing_method'] == 'None'",
              actionButton(ns("apply_smoothing"), i18n$t("Skip"))
            ),
            # Button for other methods
            conditionalPanel(
              condition = "input['smoothing-signal_smoothing_method'] != 'None'",
              actionButton(ns("apply_smoothing"), i18n$t("Apply Smoothing"), icon = icon("play"))
            )
          )
        ),
        tabPanel(
          value = "tab3_panel_advanced",
          title = i18n$t("Advanced"),
          div(

            actionButton(ns("smoothing_advanced_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("selected_smoothing_method_div"),
            fluidRow(
              column(width = 12, tags$b(i18n$t("Current Signal Smoothing Method:"))),
              column(width = 12, textOutput(ns("selected_signal_smoothing_method")))
            ),
            fluidRow(
              column(width = 12, tags$b(i18n$t("Current Derivative Smoothing Method:"))),
              column(width = 12, textOutput(ns("selected_deriv_smoothing_method")))
            )
          ),
          div(id = ns("basic_auto_smoothing_div"),
              checkboxInput(ns("basic_auto_smoothing"), i18n$t("Automatically Estimate Smoothing Points and Passes"), value = config$smooth$autosmooth)
          ),
          conditionalPanel(
            condition = "!input['smoothing-basic_auto_smoothing']",
            div(
              id = ns("manual_smoothing_div"),
              numericInput(ns("smoothing_pts"), i18n$t("Points"), value = config$smooth$smoothing_pts$value, step = 1),
              numericInput(ns("smoothing_passes"), i18n$t("Passes"), value = config$smooth$smoothing_passes$value, step = 1)
            )
          ),
          conditionalPanel(
            condition = "input['smoothing-basic_auto_smoothing']",
            div(
              id = ns("auto_smoothing_div"),
              numericInput(ns("start_smooth_pts"), i18n$t("Starting Points for Auto-Smoothing"), value = config$smooth$start_smooth$pts, step = 1),
              numericInput(ns("start_smooth_passes"), i18n$t("Starting Passes for Auto-Smoothing"), value = config$smooth$start_smooth$passes, step = 1)
            )
          ),
          div(
            id = ns("apply_smoothing_advanced_div"),
            # Button for "None" method
            conditionalPanel(
              condition = "input['smoothing-signal_smoothing_method'] == 'None'",
              actionButton(ns("apply_smoothing"), i18n$t("Skip"))
            ),
            # Button for other methods
            conditionalPanel(
              condition = "input['smoothing-signal_smoothing_method'] != 'None'",
              actionButton(ns("apply_smoothing"), i18n$t("Apply Smoothing"), icon = icon("play"))
            )
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      plotlyOutput(ns("smoothing_plot"))
    )
  )
}
