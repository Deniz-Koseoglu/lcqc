# ui_modules/tab6_deconvolution_ui.R

deconvolution_ui <- function(id, config,i18n) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab6_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Deconvolution"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab6_HELP"))))),

      tabsetPanel(
        id = ns("deconvolution_options"),
        tabPanel(
          value = "tab6_panel_basic",
          title = i18n$t("Basics"),
          div(

            actionButton(ns("deconvolution_basic_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("deconvolution_method_div"),
            uiOutput(ns("deconvolution_method"))
          ),
          div(
            id = ns("auto_crit_width2_div"),
            checkboxInput(ns("auto_crit_width_decon"), i18n$t("Determine critical width automatically"), value = config$dconv$manual_crit_d$value)
          ),
          conditionalPanel(
            condition = "!input['deconvolution-auto_crit_width_decon']",
            div(
              id = ns("manual_width_decon_div"),
              textInput(ns("manual_width_decon"), i18n$t("Critical Width"), value = config$dconv$crit_w_d$value)
            )
          ),
          div(
            id = ns("deconvolution_modres_div"),
            checkboxInput(ns("deconvolution_modres"), i18n$t("Model baseline-resolved peaks"), value = config$dconv$modres$value)
          ),
          div(
            id = ns("apply_deconvolution_basic_div"),
            actionButton(ns("apply_deconvolution"), i18n$t("Apply Deconvolution"), icon = icon("play"))
          )
        ),
        tabPanel(
          value = "tab6_panel_advanced",
          title = i18n$t("Advanced"),
          div(

            actionButton(ns("deconvolution_advanced_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("selected_deconvolution_method_display_div"),
            fluidRow(
              column(width = 12, tags$b(i18n$t("Selected Method:"))),
              column(width = 12, textOutput(ns("selected_deconvolution_method_display")))
            )
          ),
          hr(),
          div(
            id = ns("deconvolution_optmet_div"),
            uiOutput(ns("deconvolution_optmet"))
          ),
          div(
            id = ns("deconvolution_reprs_emg_div"),
            selectInput(ns("deconvolution_reprs_emg"), i18n$t("EMG representation"),
                        choices = c("EMG1" = "emg1", "EMG2" = "emg2"), selected = config$dconv$reprs_emg$value)
          ),
          div(
            id = ns("apply_deconvolution_advanced_div"),
            actionButton(ns("apply_deconvolution"), i18n$t("Apply Deconvolution"), icon = icon("play"))
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("deconvolution_output_display"),
        tabPanel(i18n$t("Modeled Groups"), plotlyOutput(ns("deconvolution_plot1"))),
        tabPanel(i18n$t("Modeled Peaks"), plotlyOutput(ns("deconvolution_plot2"))),
        tabPanel(i18n$t("Results Table"), DT::dataTableOutput(ns("deconvolution_results_table"))),
        tabPanel(i18n$t("Summary"), htmlOutput(ns("deconvolution_information_text")))
      )
    )
  )
}
