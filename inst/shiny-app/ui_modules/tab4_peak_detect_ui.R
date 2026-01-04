peak_detect_ui <- function(id, config,i18n) {
  ns <- NS(id)

  fluidPage(
    sidebarPanel(
      width = 3,

      # Display the tab title and HELP icon (green)
      div(id = "tab4_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Peak Detection"), style = "margin-top:0;")),
                                                                            column(2, uiOutput(ns("tab4_HELP"))))),

      tabsetPanel(
        id = "peak_detect_parameters",
        tabPanel(
          value="tab4_panel_basic",
          title=i18n$t("Basic"),
          div(

            actionButton(ns("peak_detect_basic_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          br(),
          div(
            id = ns("amp_thres_selection_div"),
            checkboxInput(ns("use_manual_amp_thres"), i18n$t("Use Manual Amplitude Threshold"), value = config$pdet$amp_thres_manual$value)
          ),
          conditionalPanel(
            condition = "!input['peak_detect-use_manual_amp_thres']",
            div(
              id = ns("amp_thres_method_div"),
              uiOutput(ns("amp_thres_method"))
            )
          ),
          conditionalPanel(
            condition = "input['peak_detect-use_manual_amp_thres']",
            div(
              id = ns("manual_amp_thres_div"),
              numericInput(ns("manual_amp_thres"), i18n$t("Manual Amplitude Threshold"), value = config$pdet$amp_manual$value, min = 0, step = 0.01)
            )
          ),
          div(
            id = ns("ampfrac_advanced_div"),
            conditionalPanel(
              condition = "input['peak_detect-amp_thres_method'].includes('quant') || input['peak_detect-amp_thres_method'].includes('diff')",
              numericInput(ns("ampfrac_advanced"), i18n$t("Amplitude Fraction (0.001-50%)"), value = ifelse(is.null(config$pdet$ampfrac$value), 5, config$pdet$ampfrac$value), min = 0.001, max = 50, step = 0.1)
            )
          ),
          hr(style = "border-top:1px solid #000000;"),
          div(
            id = ns("det_bunch_div"),
            checkboxInput(
              inputId = ns("det_bunch"),
              label = i18n$t("Detect and mitigate peak bunching"),
              value = config$pdet$det_bunch$value
            )
          ),
          div(
            id = ns("der_thres_method1_div"),
            uiOutput(ns("der_thres_method1"))
          ),
          div(
            id = ns("der_thres_method2_div"),
            uiOutput(ns("der_thres_method2"))
          ),

          hr(style = "border-top:1px solid #000000;"),
          div(
            id = ns("peak_rejection_filters_div"),
            fluidRow(
              column(8, h4(i18n$t("Peak Rejection Filters"))),
              div(
                style = "text-align: center;",
                actionButton(ns("reset_peak_filters"), i18n$t("Reset Filters"), icon = icon("refresh"))
              ),
              column(
                6,
                numericInput(
                  inputId = ns("rej_sn"),
                  label = i18n$t("Signal/Noise"),
                  value = config$pdet$rej$sn$value,
                  min = 0
                )
              ),
              column(
                6,
                numericInput(
                  inputId = ns("rej_ht"),
                  label = i18n$t("Minimum Height"),
                  value = config$pdet$rej$ht$value,
                  min = 0
                )
              ),
              column(
                6,
                numericInput(
                  inputId = ns("rej_wd"),
                  label = i18n$t("Peak Width"),
                  value = config$pdet$rej$wd$value,
                  min = 0
                )
              ),
              column(
                6,
                numericInput(
                  inputId = ns("rej_pa"),
                  label = i18n$t("Peak Area"),
                  value = config$pdet$rej$pa$value,
                  min = 0
                )
              )
            )
          ),
          div(
            id = ns("peak_rejection_logic_div"),
            fluidRow(
              column(
                6,
                uiOutput(ns("rej_logic_pre"))
              ),
              column(
                6,
                uiOutput(ns("rej_logic_post"))
              )
            )
          ),

          div(
            id = ns("apply_peak_detection_basic_div"),
            actionButton(ns("apply_peak_detection"), i18n$t("Apply Peak Detection"), icon = icon("play"))
          )
        ),
        tabPanel(
          value="tab4_panel_advanced",
          title=i18n$t("Advanced"),
          div(

            actionButton(ns("peak_detect_advanced_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          fluidRow(
            column(width = 12, textOutput(ns("amp_thres_method_text")))
          ),



          fluidRow(
            column(width = 12, textOutput(ns("ampfrac_text")))
          ),
          # fluidRow(
          #   column(width = 12, textOutput(ns("sens_text")))
          # ),
          fluidRow(
            column(width = 12, textOutput(ns("det_bunch_text")))
          ),
          fluidRow(
            column(width = 12, textOutput(ns("der_thres_text")))
          ),
          hr(style = "border-top:1px solid #000000;"),
          div(
            id = ns("sensitivity_parameters_div"),
            fluidRow(
              column(12, h4(i18n$t("Sensitivity Parameters"))),
              column(
                6,
                numericInput(ns("sens_fd"),
                  i18n$t("First Derivative Sensitivity"),
                  value = config$pdet$sens_fd$value, min = 0, step = 0.1
                )
              ),
              column(
                6,
                numericInput(ns("sens_sd"),
                  i18n$t("Second Derivative Sensitivity"),
                  value = config$pdet$sens_sd$value, min = 0, step = 0.1
                )
              )
            ),
            fluidRow(
              column(
                12,
                conditionalPanel(
                  condition = "input['peak_detect-amp_thres_method'].includes('zscore')",
                  numericInput(ns("sens_amp"),
                    i18n$t("Amplitude Limit Sensitivity (Z-score)"),
                    value = config$pdet$sens_amp$value, min = 0, step = 0.1
                  )
                )
              )
            )
          ),
          hr(style = "border-top:1px solid #000000;"),
          div(
            id = ns("zscore_parameters_div"),
            conditionalPanel(
              condition = "input['peak_detect-amp_thres_method'].includes('zscore')",
              h4(i18n$t("Z-score Parameters")),
              numericInput(ns("amp_thres_pars_zscore_lag"), i18n$t("Moving Average/SD Lag (integer)"), value = config$pdet$amp_thres_pars$zscore$lag$value, min = 1, step = 1), # lag in z_thres
              numericInput(ns("amp_thres_pars_zscore_thres"), i18n$t("Threshold (factor of standard deviation)"), value = config$pdet$amp_thres_pars$zscore$thres$value, min = 0.1, step = 0.1), # threshold in z_thres
              numericInput(ns("amp_thres_pars_zscore_sens"), i18n$t("Sensitivity to average and SD"), value = config$pdet$amp_thres_pars$zscore$sens$value, min = 0, max = 1, step = 0.05) # sensitivity
            )
          ),

          div(
            id = ns("enable_fast_chrom_div"),
            checkboxInput(
              inputId = ns("enable_fast_chrom"),
              label = i18n$t("Enable FastChrom Baseline Correction"),
              value = config$pdet$enable_fast_chrom$value
            )
          ),
          conditionalPanel(
            condition = "input['peak_detect-enable_fast_chrom'] == true",
            div(
              id = ns("fast_chrom_critical_width_div"),
              numericInput(ns("fast_chrom_critical_width"), i18n$t("FastChrom critical width"), value = config$pdet$fast_chrom_critical_width$value, step = 1)
            )
          ),
          hr(style = "border-top:1px solid #000000;"),
            div(
            id = ns("Boundary_verification_parameters_div"),
            fluidRow(
              column(12, h4(i18n$t("Boundary Verification Parameters"))),
              column(
                6,
                numericInput(
                  inputId = ns("mpts_signal"),
                  label = i18n$t("Signal Confirmation Points"),
                  value = config$pdet$mpts$signal$value,
                  min = 1,
                  max = 10
                )
              ),
            column(
              6,
              numericInput(
                inputId = ns("mpts_deriv"),
                label = i18n$t("Derivative Confirmation Points"),
                value = config$pdet$mpts$deriv$value,
                min = 1,
                max = 10
              )
            )
          ), # This closes the fluidRow for Boundary Verification Parameters
          fluidRow(
            column(12, h4(i18n$t("ApexTrack Baseline Expansion Parameters"))),
            column(
              6,
              numericInput(
                inputId = ns("apex_pars_liftoff"),
                label = i18n$t("Liftoff (%)"),
                value = config$pdet$apex_pars$liftoff$value,
                min = 0,
                max = 100,
                step = 0.1
              )
            ),
            column(
              6,
              numericInput(
                inputId = ns("apex_pars_touchdown"),
                label = i18n$t("Touchdown (%)"),
                value = config$pdet$apex_pars$touchdown$value,
                min = 0,
                max = 100,
                step = 0.1
              )
            )
          ),
          fluidRow(
            column(12, h4(i18n$t("Zero Crossing Parameters"))),

            column(
              6,
              numericInput(
                inputId = ns("crosspts_signal"),
                label = i18n$t("Signal Zero Crossing Points"),
                value = config$pdet$crosspts$signal$value,
                min = 1,
                max = 10
              ),
            ),
            column(
              6,
              numericInput(
                inputId = ns("crosspts_deriv"),
                label = i18n$t("Derivative Zero Crossing Points"),
                value = config$pdet$crosspts$deriv$value,
                min = 1,
                max = 10
              ),
            )
          ) # This closes the fluidRow for Zero Crossing Parameters
          ), # This closes the div for Boundary_verification_parameters_div
          hr(style = "border-top:1px solid #000000;"),
          div(
            id = ns("apply_peak_detection_advanced_div"),
            actionButton(ns("apply_peak_detection"), i18n$t("Apply Peak Detection"), icon = icon("play"))
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "peak_detection_tabs",
        tabPanel(i18n$t("Chromatogram Plot"), plotlyOutput(ns("peak_plot"))), # Plot using the 'Chromatogram' element
        tabPanel(i18n$t("FD Noise Plot"), plotlyOutput(ns("fd_noise_plot"))), # Plot using the 'sd_noise' element

        tabPanel(i18n$t("SD Noise Plot"), plotlyOutput(ns("sd_noise_plot"))), # Plot using the 'sd_noise' element
        tabPanel(i18n$t("Peak Table"), DTOutput(ns("peak_extents_table"))), # Table using the 'Peak_Extents' element
        tabPanel(i18n$t("Information"), htmlOutput(ns("peak_info_text"))), # Output for information string
        tabPanel(i18n$t("Detection Thresholds"), htmlOutput(ns("peak_summary"))) # For summary or log
      )
    )
  )
}
