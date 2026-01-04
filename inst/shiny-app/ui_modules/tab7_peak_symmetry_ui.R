peak_symmetry_ui <- function(id, config,i18n) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab7_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Peak Symmetry"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab7_HELP"))))),

      tabsetPanel(
        id = ns("peak_symmetry_options"),
        tabPanel(
          value="tab7_panel_basic",
          title=i18n$t("Basic"),
          div(

            actionButton(ns("symmetry_basic_intro"),
                         "",
                         style = "float: right; margin-top: -2px; margin-right:15px;",
                         icon = icon("info-circle"),
                         class = "btn-light btn-xs"
            )
          ),
          div(
            id = ns("symmetry_method_div"),
            uiOutput(ns("symmetry_method"))
          ),

          div(
            id = ns("peaks_to_model_div"),
            #h5(i18n$t("Peaks to Model")),
            fluidRow(
              column(12, textInput(ns("symmetry_peaks_manual"), i18n$t("Peaks (e.g. 1:3 or comma-separated)"), value = config$psym$peaks_psym$value))
            )
          ),
          div(
            id = ns("symmetry_peaks_modal_btn_div"),
            fluidRow(
              column(12, actionButton(ns("symmetry_peaks_modal_btn"), i18n$t("Visual Select"), icon = icon("list-ul"), class = "btn-sm btn-info"))
            )
          ),
          hr(),

          div(
            id = ns("auto_crit_width3_div"),
            checkboxInput(ns("auto_crit_width_symm"), i18n$t("Determine critical width automatically"), value = config$psym$manual_crit_s$value)
          ),
          conditionalPanel(
            condition = "!input['symmetry-auto_crit_width_symm']",
            div(
              id = ns("symmetry_crit_w_div"),
              textInput(ns("symmetry_crit_w"), i18n$t("Critical Width"), value = config$psym$crit_w_s$value)
            )
          ),
          div(
            id = ns("show_widths_div"),
            checkboxInput(ns("show_widths"), i18n$t("Show widths"), value = config$psym$show_widths$value)
          ),

          tags$hr(),
          div(
            id = ns("apply_symmetry_basic_div"),
            actionButton(ns("apply_symmetry"), i18n$t("Apply Modelling"), icon = icon("play"))
          )
        ),
        tabPanel(
          value="tab7_panel_advanced",
          title=i18n$t("Advanced"),
          div(

            actionButton(ns("symmetry_advanced_intro"),
                         "",
                         style = "float: right; margin-top: -2px; margin-right:15px;",
                         icon = icon("info-circle"),
                         class = "btn-light btn-xs"
            )
          ),
          tags$br(),
          div(
            id = ns("symmetry_optmet_div"),
            uiOutput(ns("symmetry_optmet"))
          ),
          div(
            id = ns("symmetry_reprs_div"),
            numericInput(ns("symmetry_reprs"), i18n$t("Resolution"), value = config$psym$reprs$value, min = 10, step = 10)
          ),
          div(
            id = ns("apply_symmetry_advanced_div"),
            actionButton(ns("apply_symmetry"), i18n$t("Apply Modelling"), icon = icon("play"))
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("deconvolution_asym_output_display"),
        tabPanel(i18n$t("Results Table"), DT::dataTableOutput(ns("asym_results_table"))),
        tabPanel(i18n$t("Summary"), htmlOutput(ns("asym_information_text"))),
        tabPanel(i18n$t("Modelled Peaks Plot"),
                 uiOutput(ns("plot_slider")),
                 plotlyOutput(ns("asym_modelled_peaks_plot"), height = "auto"),
        )
      )
    )
  )
}
