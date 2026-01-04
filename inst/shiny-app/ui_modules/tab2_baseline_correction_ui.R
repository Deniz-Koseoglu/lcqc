baseline_correction_ui <- function(id, config, i18n) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab2_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Baseline Correction"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab2_HELP"))))),

      tabsetPanel(
        id = "baseline_options",
        tabPanel(
          value = "tab1_panel_basic",
          title = i18n$t("Basics"),
          div(

            actionButton(ns("baseline_basic_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("baseline_method_div"),
            uiOutput(ns("baseline_method"))
          ),
          div(
            id = ns("calculate_baseline_div"),
            # Button for "None" method
            conditionalPanel(
              condition = "input['baseline-baseline_method'] == 'None'",
              actionButton(ns("calculate_baseline"), i18n$t("Skip"))
            ),
            # Button for other methods
            conditionalPanel(
              condition = "input['baseline-baseline_method'] != 'None'",
              actionButton(ns("calculate_baseline"), i18n$t("Calculate Baseline"), icon = icon("play"))
            )
          )
        ),
        tabPanel(
          id = "tab_panel_advanced",
          title = i18n$t("Advanced"),
          div(

            actionButton(ns("baseline_advanced_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("selected_baseline_method_div"),
            fluidRow(
              column(width = 12, tags$b(i18n$t("Current Method:"))),
              column(width = 12, textOutput(ns("selected_baseline_method")))
            )
          ),
          hr(),
          div(
            id = ns("als_params_div"),
            conditionalPanel(
              # condition = "input.baseline_method == 'als'",
              condition = "input['baseline-baseline_method'] == 'als'",
              div(id = ns("als_lambda_div"), numericInput(ns("als_lambda"), i18n$t("Lambda"), value = config$bline$als$lambda)),
              div(id = ns("als_p_div"), numericInput(ns("als_p"), i18n$t("p"), value = config$bline$als$p, step = 0.001)),
              div(id = ns("als_prec_div"), numericInput(ns("als_prec"), i18n$t("Precision"), value = config$bline$als$prec, step = 0.00000001)),
              div(id = ns("als_maxit_div"), numericInput(ns("als_maxit"), i18n$t("Max Iterations"), value = config$bline$als$maxit, step = 10)),
              div(id = ns("als_rm_neg_div"), checkboxInput(ns("als_rm_neg"), i18n$t("Remove Negatives"), value = config$bline$als$rm_neg))
            )
          ),
          div(
            id = ns("chang_params_div"),
            conditionalPanel(
              condition = "input['baseline-baseline_method'] == 'chang'",
              div(id = ns("chang_threshold_div"), numericInput(ns("chang_threshold"), i18n$t("Threshold"), value = config$bline$chang$threshold, min = 0, max = 1, step = 0.05)),
              div(id = ns("chang_alpha_div"), numericInput(ns("chang_alpha"), i18n$t("Alpha"), value = config$bline$chang$alpha, min = 0, max = 1, step = 0.05)),
              div(id = ns("chang_bfrac_div"), numericInput(ns("chang_bfrac"), i18n$t("Baseline Fraction"), value = config$bline$chang$bfrac, min = 0, max = 1, step = 0.05)),
              div(id = ns("chang_segments_div"), numericInput(ns("chang_segments"), i18n$t("Segments"), value = config$bline$chang$segments, step = 10)),
              div(id = ns("chang_sig_window_div"), numericInput(ns("chang_sig_window"), i18n$t("Signal Window"), value = config$bline$chang$sig_window, step = 1)),
              div(id = ns("chang_fit_div"),
                  uiOutput(ns("chang_fit"))),
              div(id = ns("chang_rm_neg_div"), checkboxInput(ns("chang_rm_neg"), i18n$t("Remove Negatives"), value = config$bline$chang$rm_neg))
            )
          ),
          div(
            id = ns("isrea_params_div"),
            conditionalPanel(
              condition = "input['baseline-baseline_method'] == 'isrea'",
              div(id = ns("isrea_eta_div"), numericInput(ns("isrea_eta"), i18n$t("Eta"), value = config$bline$isrea$eta, step = 1)),
              div(id = ns("isrea_maxit_div"), numericInput(ns("isrea_maxit"), i18n$t("Max Iterations"), value = config$bline$isrea$maxit, step = 10)),
              div(id = ns("isrea_rm_neg_div"), checkboxInput(ns("isrea_rm_neg"), i18n$t("Remove Negatives"), value = config$bline$isrea$rm_neg))
            )
          ),
          div(
            id = ns("poly_params_div"),
            conditionalPanel(
              condition = "input['baseline-baseline_method'] == 'poly'",
              div(id = ns("poly_deg_div"), numericInput(ns("poly_deg"), i18n$t("Degree"), value = config$bline$poly$deg, step = 1)),
              div(id = ns("poly_prec_div"), numericInput(ns("poly_prec"), i18n$t("Precision"), value = config$bline$poly$prec, step = 0.0001)),
              div(id = ns("poly_maxit_div"), numericInput(ns("poly_maxit"), i18n$t("Max Iterations"), value = config$bline$poly$maxit, step = 10)),
              div(id = ns("poly_rm_neg_div"), checkboxInput(ns("poly_rm_neg"), i18n$t("Remove Negatives"), value = config$bline$poly$rm_neg))
            )
          ),
          div(
            id = ns("calculate_baseline_advanced_div"),
            # Button for "None" method
            conditionalPanel(
              condition = "input['baseline-baseline_method'] == 'None'",
              actionButton(ns("calculate_baseline"), i18n$t("Skip"))
            ),
            # Button for other methods
            conditionalPanel(
              condition = "input['baseline-baseline_method'] != 'None'",
              actionButton(ns("calculate_baseline"), i18n$t("Calculate Baseline"), icon = icon("play"))
            )
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      plotlyOutput(ns("baseline_plot"))
    )
  )
}
