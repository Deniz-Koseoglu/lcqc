report_ui <- function(id, config, i18n) {
  ns <- NS(id)

  fluidPage(includeCSS("www/fileInput.css"),
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        width = 3, # First Column

        shiny::wellPanel(

          # Display the tab title and HELP icon (green)
          div(id = "tab9_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("Reporting"), style = "margin-top:0;")),
                                                                                column(2, uiOutput(ns("tab9_HELP"))))),

          div(

            actionButton(ns("report_options_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          tags$h3(i18n$t("Report Options")),
          div(
            id = ns("which_chart_div"),
            uiOutput(ns("which_chart"))
          ),
          div(
            id = ns("peak_indices_div"),
            uiOutput(ns("peak_indices")),
            actionButton(ns("visual_select_peaks"), i18n$t("Visual Select"), icon = icon("list-ul"), class = "btn-sm btn-info")
          ),
          hr(),
          div(
            id = ns("pnms_div"),
            textInput(ns("pnms"), i18n$t("Enter Peak Names"), value = config$rpr$pnms,placeholder = "Naphthalene,Anthracene,Caffeine,Benzene")
          ),
          div(
            id = ns("pconcs_div"),
            textInput(ns("pconcs"), i18n$t("Enter Peak Concentrations"), value = config$rpr$pcons, placeholder = "250,500,200,400")
          ),
          div(
            id = ns("unitconc_div"),
            textInput(ns("tpars_unitc"), i18n$t("Concentration units"), value = config$rpr$tpars_unitc, placeholder = "ppm")
          )
        )
      ),
      column(
        width = 3, # Second Column
        shiny::wellPanel(
          div(

            actionButton(ns("metrics_specifications_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          tags$h3(i18n$t("Theoretical Plates")),
          div(
            id = ns("tplate_inputs_div"),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_EP"), "EP", value = config$rpr$metric_EP)
              ),
              column(
                width = 10,
                textInput(ns("spec_EP"), label = "", value = config$rpr$spec_EP, placeholder = ">10000")
              )
            ),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_AH"), "AH", value = config$rpr$metric_AH)
              ),
              column(
                width = 10,
                textInput(ns("spec_AH"), label = "", value = config$rpr$spec_AH, placeholder = ">10000")
              )
            ),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_S5"), "S5", value = config$rpr$metric_S5)
              ),
              column(
                width = 10,
                textInput(ns("spec_S5"), label = "", value = config$rpr$spec_S5, placeholder = ">10000")
              )
            ),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_EMG"), "EMG", value = config$rpr$metric_EMG)
              ),
              column(
                width = 10,
                textInput(ns("spec_EMG"), label = "", value = config$rpr$spec_EMG, placeholder = ">10000")
              )
            )
          ),
          hr(),
          tags$h3(i18n$t("Asymmetry Metrics")),
          div(
            id = ns("asymmetry_inputs_div"),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_As"), "As", value = config$rpr$metric_As)
              ),
              column(
                width = 10,
                textInput(ns("spec_As"), label = "", value = config$rpr$spec_As, placeholder = "0.8-1.2")
              )
            ),
            fluidRow(
              column(
                width = 2,
                checkboxInput(ns("metric_Tf"), "Tf", value = config$rpr$metric_Tf)
              ),
              column(
                width = 10,
                textInput(ns("spec_Tf"), label = "", value = config$rpr$spec_Tf, placeholder = "0.8-1.2")
              )
            ),
            div(
              id = ns("add_tpa_div"),
              checkboxInput(ns("add_tpa"), i18n$t("Include TPA Plots"), value = config$rpr$add_tpa),
            )
          ),
          hr(),
          div(
            tags$h3(i18n$t("Column-Specific Metrics")),
            div(
              id = ns("add_addmets_div"),
              checkboxInput(ns("add_addmets"), i18n$t("Add Additional Performance Metrics "), value = config$rpr$add_addmets)
            )
          )
        )
      ),
      column(
        width = 4, # Third Column
        shiny::wellPanel(
          div(

            actionButton(ns("report_content_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          tags$h3(i18n$t("Report Content")),
          div(
            id = ns("document_number_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Document Number"))),
              column(6, textAreaInput(ns("tpars_dnum"), NULL, rows = 1, value = config$rpr$tpars_dnum))
            )
          ),
          div(
            id = ns("operator_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Operator"))),
              column(6, textAreaInput(ns("tpars_oper"), NULL, rows = 1, value = config$rpr$tpars_oper))
            )
          ),
          tags$h4(i18n$t("Product Information")),
          div(
            id = ns("column_serial_number_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Column Serial Number"))),
              column(6, textAreaInput(ns("tpars_sn"), NULL, rows = 1, value = config$rpr$tpars_sn))
            )
          ),
          div(
            id = ns("column_part_number_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Column Part Number"))),
              column(6, textAreaInput(ns("tpars_pn"), NULL, rows = 1, value = config$rpr$tpars_pn))
            )
          ),
          div(
            id = ns("column_description_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Column Description"))),
              column(6, textAreaInput(ns("tpars_desc"), NULL, rows = 3, value = config$rpr$tpars_desc))
            )
          ),
          div(
            id = ns("batch_number_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Batch Number"))),
              column(6, textAreaInput(ns("tpars_bn"), NULL, rows = 1, value = config$rpr$tpars_bn))
            )
          ),
          tags$h4(i18n$t("Analytical Method")),
          div(
            id = ns("mobile_phase_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Mobile Phase"))),
              column(6, textAreaInput(ns("tpars_mp"), NULL, rows = 1, value = config$rpr$tpars_mp))
            )
          ),
          div(
            id = ns("shipment_phase_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Shipment Phase"))),
              column(6, textAreaInput(ns("tpars_sp"), NULL, rows = 3, value = config$rpr$tpars_sp))
            )
          ),
          div(
            id = ns("back_pressure_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Back Pressure (bar)"))),
              column(6, textAreaInput(ns("tpars_bp"), NULL, rows = 1, value = config$rpr$tpars_bp))
            )
          ),
          div(
            id = ns("flow_rate_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Flow Rate (mL/min)"))),
              column(6, textAreaInput(ns("tpars_flow"), NULL, rows = 1, value = config$rpr$tpars_flow))
            )
          ),
          div(
            id = ns("column_temperature_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Column Temperature (°C)"))),
              column(6, textAreaInput(ns("tpars_temp"), NULL, rows = 1, value = config$rpr$tpars_temp))
            )
          ),
          div(
            id = ns("injection_volume_div"),
            fluidRow(
              column(6, tags$label(i18n$t("Injection Volume (µL)"))),
              column(6, textAreaInput(ns("tpars_inj"), NULL, rows = 1, value = config$rpr$tpars_inj))
            )
          )
        )
      ),
      column(
        width = 2, # Fourth Column
        shiny::wellPanel(
          div(

            actionButton(ns("output_settings_intro"),
              "",
              style = "float: right; margin-top: -2px; margin-right:15px;",
              icon = icon("info-circle"),
              class = "btn-light btn-xs"
            )
          ),
          tags$h3(i18n$t("Output Settings")),
          div(
            id = ns("asprat_div"),
            sliderInput(ns("asprat"), i18n$t("Chromatogram Aspect Ratio"),
              min = 0.1, max = 1.0, value = config$rpr$asprat, step = 0.05
            )
          ),
          div(id = ns("asprat_tpa_div"),
              sliderInput(ns("asprat_tpa"), i18n$t("TPA Plot Aspect Ratio"),
                          min = 0.1, max = 1.0, value = config$rpr$asprat_tpa, step = 0.05)
              ),
          div(
            id = ns("fontsize_div"),
            numericInput(ns("fontsize"), i18n$t("Report Font Size"), value = config$rpr$fontsize, min = 8, max = 14, step = 1)
          ),
          uiOutput(ns("clogo_div")),
          tags$br(),
          tags$h3(i18n$t("Report Generation")),
          div(
            id = ns("expath_div"),
            textInput(ns("expath"), i18n$t("Export Directory"), value = config$rpr$expath)
          ),
          uiOutput(ns("browse_expath_div")),
          tags$br(""),
          div(
            id = ns("generate_report_div"),
            actionButton(ns("generate_report"), i18n$t("Generate Report"), class = "btn-primary")
          ),
          div(
            id = ns("render_log_div"),
            htmlOutput(ns("render_log"))
          )
        )
      )
    )
  )
}
