# ui_modules/tab8_performance_metrics_ui.R
performance_metrics_ui <- function(id, config, i18n) {
  ns <- NS(id)

  fluidPage(
    sidebarPanel(
      width = 3,

      # input for chrom_tplate
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Theoretical Plates"), "'"),

        # Display the tab title and HELP icon (green)
        div(id = "tab8_title_tplate_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Theoretical Plates"), style = "margin-top:0;")),
                                                                              column(2, uiOutput(ns("tab8_tplate_HELP"))))),

        div(

          actionButton(ns("tplate_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("tplate_method_div"),
          uiOutput(ns("tplate_method"))
        ),
        div(
          id = ns("tplate_len_div"),
          numericInput(ns("tplate_len"), i18n$t("Column Length (mm)"), value = config$perf$tplate$len$value, min = 0)
        ),
        div(
          id = ns("tplate_dp_div"),
          numericInput(ns("tplate_dp"), i18n$t("Particle Size (µm)"), value = config$perf$tplate$dp$value, min = 0)
        ),
        div(
          id = ns("tplate_show_widths_div"),
          checkboxInput(ns("tplate_show_widths"), i18n$t("Show Widths"), value = config$perf$tplate$show_widths$value)
        ),

        div(
          id = ns("auto_crit_width4_div"),
          checkboxInput(ns("auto_crit_width_tplate"), i18n$t("Determine critical width automatically"), value = config$perf$tplate$manual_crit_tplate$value)
        ),
        conditionalPanel(
          condition = "!input['performance-auto_crit_width_tplate']",
          div(
            id = ns("tplate_crit_w_div"),
            textInput(ns("tplate_crit_w"), i18n$t("Critical Width"), value = config$perf$tplate$crit_w_tplate$value)
          )
        ),
        div(
          id = ns("tplate_deltap_div"),
          numericInput(ns("tplate_deltap"), i18n$t("Back Pressure (bar)"), value = config$perf$tplate$deltap$value, min = 0)
        ),
        div(
          id = ns("tplate_visc_div"),
          numericInput(ns("tplate_visc"), i18n$t("Mobile Phase Viscosity (mPas)"), value = config$perf$tplate$visc$value, min = 0)
        ),
        div(
          id = ns("tplate_t0_mode_div"),
          uiOutput(ns("tplate_t0_mode"))
        ),
        div(
          id = ns("tplate_t0_manual_div"),
          conditionalPanel(
            condition = paste0("input['", ns("tplate_t0_mode"), "'] == 'manual'"),
            numericInput(ns("tplate_t0_manual"), i18n$t("Dead Time (minutes)"), value = config$perf$tplate$t0_manual$value)
          )
        ),
        div(
          id = ns("tplate_t0_peak_index_div"),
          conditionalPanel(
            condition = paste0("input['", ns("tplate_t0_mode"), "'] == 'peak'"),
            numericInput(ns("tplate_t0_peak_index"), i18n$t("Dead Time Peak Index"), value = config$perf$tplate$t0_peak_index$value)
          )
        ),
        div(
          id = ns("tplate_imped_met_div"),
          uiOutput(ns("tplate_imped_met"))
        ),
        div(
          id = ns("apply_tplate_div"),
          actionButton(ns("apply_tplate"), i18n$t("Calculate"))
        )
      ),

      # Inputs for chrom_res
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Resolution"), "'"),

        # Display the tab title and HELP icon (green)
        div(id = "tab8_title_res_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Resolution"), style = "margin-top:0;")),
                                                                                     column(2, uiOutput(ns("tab8_res_HELP"))))),

        div(

          actionButton(ns("res_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("res_peaks1_manual_div"),
          fluidRow(
            column(12, textInput(ns("res_peaks1_manual"), i18n$t("Peaks 1 (e.g. 1:3 or comma-separated)"), value = config$perf$res$peaks1_manual$value))
          )
        ),
        div(
          id = ns("res_peaks2_manual_div"),
          fluidRow(
            column(12, textInput(ns("res_peaks2_manual"), i18n$t("Peaks 2 (e.g. 1:3 or comma-separated)"), value = config$perf$res$peaks2_manual$value))
          )
        ),
        div(
          id = ns("res_peaks_modal_btn_div"),
          fluidRow(
            column(12, actionButton(ns("res_peaks_modal_btn"), i18n$t("Visual Select"), icon = icon("list-ul"), class = "btn-sm btn-info"))
          )
        ),
        hr(),
        div(
          id = ns("res_method_div"),
          uiOutput(ns("res_method"))
        ),
        div(
          id = ns("res_t0_mode_div"),
          uiOutput(ns("res_t0_mode"))
        ),
        div(
          id = ns("res_ks_manual_div"),
          conditionalPanel(
            condition = paste0("input['", ns("res_t0_mode"), "'] == 'manual'"),
            numericInput(ns("res_ks_manual"), i18n$t("Dead Time (minutes)"), value = config$perf$res$ks_manual$value)
          )
        ),
        div(
          id = ns("res_ks_peak_index_div"),
          conditionalPanel(
            condition = paste0("input['", ns("res_t0_mode"), "'] == 'peak'"),
            numericInput(ns("res_ks_peak_index"), i18n$t("Dead Time Peak Index"), value = config$perf$res$ks_peak_index$value)
          )
        ),
        div(
          id = ns("auto_crit_width5_div"),
          checkboxInput(ns("auto_crit_width_res"), i18n$t("Determine critical width automatically"), value = config$perf$res$manual_crit_res$value)
        ),
        conditionalPanel(
          condition = "!input['performance-auto_crit_width_res']",
          div(
            id = ns("res_crit_w_div"),
            textInput(ns("res_crit_w"), i18n$t("Critical Width"), value = config$perf$res$crit_w_res$value)
          )
        ),
        div(
          id = ns("apply_resolution_div"),
          actionButton(ns("apply_resolution"), i18n$t("Calculate"))
        )
      ),

      # Inputs for chrom_retf
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Retention Factor (k)"), "'"),
        div(

          div(id = "tab8_title_retf_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Retention Factor (k)"), style = "margin-top:0;")),
                                                                                    column(2, uiOutput(ns("tab8_retf_HELP"))))),

          actionButton(ns("retf_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("retf_t0_mode_div"),
          uiOutput(ns("retf_t0_mode"))
        ),
        div(
          id = ns("retf_t0_manual_div"),
          conditionalPanel(
            condition = paste0("input['", ns("retf_t0_mode"), "'] == 'manual'"),
            numericInput(ns("retf_t0_manual"), i18n$t("Dead Time (minutes)"), value = config$perf$retf$t0_manual$value)
          )
        ),
        div(
          id = ns("retf_t0_peak_index_div"),
          conditionalPanel(
            condition = paste0("input['", ns("retf_t0_mode"), "'] == 'peak'"),
            numericInput(ns("retf_t0_peak_index"), i18n$t("Dead Time Peak Index"), value = config$perf$retf$t0_peak_index$value)
          )
        ),
        div(
          id = ns("retf_peaks_manual_div"),
          fluidRow(
            column(12, textInput(ns("retf_peaks_manual"), i18n$t("Peaks (e.g. 1:3 or comma-separated)"), value = config$perf$retf$peaks_manual$value))
          )
        ),
        div(
          id = ns("retf_peaks_modal_btn_div"),
          fluidRow(
            column(12, actionButton(ns("retf_peaks_modal_btn"), i18n$t("Visual Select"), icon = icon("list-ul"), class = "btn-sm btn-info"))
          )
        ),
        hr(),
        div(
          id = ns("auto_crit_width6_div"),
          checkboxInput(ns("auto_crit_width_retf"), i18n$t("Determine critical width automatically"), value = config$perf$retf$manual_crit_retf$value)
        ),
        conditionalPanel(
          condition = "!input['performance-auto_crit_width_retf']",
          div(
            id = ns("retf_crit_w_div"),
            textInput(ns("retf_crit_w"), i18n$t("Critical Width"), value = config$perf$retf$crit_w_retf$value)
          )
        ),
        div(
          id = ns("apply_retention_factor_div"),
          actionButton(ns("apply_retention_factor"), i18n$t("Calculate"))
        )
      ),

      # Inputs for chrom_sepf
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Separation Factor"), "'"),
        div(

          div(id = "tab8_title_sepf_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Separation Factor"), style = "margin-top:0;")),
                                                                                    column(2, uiOutput(ns("tab8_sepf_HELP"))))),

          actionButton(ns("sepf_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("sepf_peaks1_manual_div"),
          fluidRow(
            column(12, textInput(ns("sepf_peaks1_manual"), i18n$t("Peaks 1 (e.g. 1:3 or comma-separated)"), value = config$perf$sepf$peaks1_manual$value))
          )
        ),
        div(
          id = ns("sepf_peaks2_manual_div"),
          fluidRow(
            column(12, textInput(ns("sepf_peaks2_manual"), i18n$t("Peaks 2 (e.g. 1:3 or comma-separated)"), value = config$perf$sepf$peaks2_manual$value))
          )
        ),
        div(
          id = ns("sepf_peaks_modal_btn_div"),
          fluidRow(
            column(12, actionButton(ns("sepf_peaks_modal_btn"), i18n$t("Visual Select"), icon = icon("list-ul"), class = "btn-sm btn-info"))
          )
        ),
        hr(),
        div(
          id = ns("sepf_t0_mode_div"),
          uiOutput(ns("sepf_t0_mode"))
        ),
        div(
          id = ns("sepf_ks_manual_div"),
          conditionalPanel(
            condition = paste0("input['", ns("sepf_t0_mode"), "'] == 'manual'"),
            numericInput(ns("sepf_ks_manual"), i18n$t("Dead Time (minutes)"), value = config$perf$sepf$ks_manual$value)
          )
        ),
        div(
          id = ns("sepf_ks_peak_index_div"),
          conditionalPanel(
            condition = paste0("input['", ns("sepf_t0_mode"), "'] == 'peak'"),
            numericInput(ns("sepf_ks_peak_index"), i18n$t("Dead Time Peak Index"), value = config$perf$sepf$ks_peak_index$value)
          )
        ),
        div(
          id = ns("auto_crit_width7_div"),
          checkboxInput(ns("auto_crit_width_sepf"), i18n$t("Determine critical width automatically"), value = config$perf$sepf$manual_crit_sepf$value)
        ),
        conditionalPanel(
          condition = "!input['performance-auto_crit_width_sepf']",
          div(
            id = ns("sepf_crit_w_div"),
            textInput(ns("sepf_crit_w"), i18n$t("Critical Width"), value = config$perf$sepf$crit_w_sepf$value)
          )
        ),
        div(
          id = ns("apply_separation_factor_div"),
          actionButton(ns("apply_separation_factor"), i18n$t("Calculate"))
        )
      ),

      # input for chrom_addmets
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Column-Specific Metrics"), "'"),
        div(

          div(id = "tab8_title_addmets_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Column-Specific Metrics"), style = "margin-top:0;")),
                                                                                    column(2, uiOutput(ns("tab8_addmets_HELP"))))),

          actionButton(ns("addmets_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("addmets_which_mets_div"),
          uiOutput(ns("addmets_which_mets"))
        ),
        div(
          id = ns("addmets_t0_div"),
          numericInput(ns("addmets_t0"), i18n$t("Dead Time (minutes)"), value = config$perf$addmets$t0$value)
        ),
        div(
          id = ns("addmets_len_div"),
          numericInput(ns("addmets_len"), i18n$t("Column Length (mm)"), value = config$perf$addmets$len$value)
        ),
        div(
          id = ns("addmets_flow_div"),
          numericInput(ns("addmets_flow"), i18n$t("Flow Rate (mL/min)"), value = config$perf$addmets$flow$value)
        ),
        div(
          id = ns("addmets_id_div"),
          numericInput(ns("addmets_id"), i18n$t("Column Internal Diameter (mm)"), value = config$perf$addmets$id$value)
        ),
        div(
          id = ns("addmets_deltap_div"),
          numericInput(ns("addmets_deltap"), i18n$t("Back Pressure (bar)"), value = config$perf$addmets$deltap$value)
        ),
        div(
          id = ns("addmets_visc_div"),
          numericInput(ns("addmets_visc"), i18n$t("Mobile Phase Viscosity (mPas)"), value = config$perf$addmets$visc$value)
        ),
        div(
          id = ns("addmets_dp_div"),
          numericInput(ns("addmets_dp"), i18n$t("Particle Size (µm)"), value = config$perf$addmets$dp$value) #\u00B5
        ),
        div(
          id = ns("apply_addmets_div"),
          actionButton(ns("apply_addmets"), i18n$t("Calculate"))
        )
      ),

      #Viscosity Panel
      conditionalPanel(
        condition = paste0("input['", ns("navlist"), "'] == '", i18n$t("Viscosity Calculation"), "'"),
        div(

          div(id = "tab8_title_visc_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Viscosity Calculation"), style = "margin-top:0;")),
                                                                                        column(2, uiOutput(ns("tab8_visc_HELP"))))),

          actionButton(ns("visc_intro"),
                       "",
                       style = "float: right; margin-top: -2px; margin-right:15px;",
                       icon = icon("info-circle"),
                       class = "btn-light btn-xs"
          )
        ),
        h4(""),
        div(
          id = ns("visc_which_solv_div"),
          uiOutput(ns("visc_which_solv"))
        ),
        div(id = ns("viscfrac_inputs_div"),
            uiOutput(ns("viscfrac_inputs"))),
        div(
          id = ns("visc_fractype_div"),
          uiOutput(ns("visc_fractype"))
        ),
        div(id = ns("visc_temp_div"),
          numericInput(
            inputId = ns("visc_temp"),
            label   = i18n$t("Temperature (°C)"),
            value   = 25,
            min     = -20,
            max     = 150,
            step    = 1
          )
        ),
        div(
          id = ns("visc_calc_div"),
          actionButton(ns("visc_calc"), i18n$t("Calculate Viscosity"))
          )
      )
    ), # End sidebarPanel

    mainPanel(
      width = 9,
      navlistPanel(
        id = ns("navlist"),
        widths = c(2, 10),
        header = i18n$t("Performance Metrics"),
        tabPanel(
          i18n$t("Theoretical Plates"),
          htmlOutput(ns("tplate_information_text")),
          dataTableOutput(ns("tplate_results_table"))
        ),
        tabPanel(
          i18n$t("Resolution"),
          htmlOutput(ns("res_information_text")),
          dataTableOutput(ns("res_results_table"))
        ),
        tabPanel(
          i18n$t("Retention Factor (k)"),
          htmlOutput(ns("retf_information_text")),
          dataTableOutput(ns("retf_results_table"))
        ),
        tabPanel(
          i18n$t("Separation Factor"),
          htmlOutput(ns("sepf_information_text")),
          dataTableOutput(ns("sepf_results_table"))
        ),
        tabPanel(
          i18n$t("Column-Specific Metrics"),
          htmlOutput(ns("addmets_information_text")),
          dataTableOutput(ns("addmets_results_table"))
        ),
        tabPanel(
          i18n$t("Viscosity Calculation"),
          htmlOutput(ns("visc_information_text")),
          dataTableOutput(ns("visc_results_table"))
        )
      ) # End navlistPanel
    ) # End mainPanel
  ) # End fluidPage
} # End performance_metrics_ui
