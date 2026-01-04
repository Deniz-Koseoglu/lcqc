# Sys.setlocale("LC_ALL","English")
# inst/shiny-app/app.R
reqlibs <- c(
  "shiny", "shinyjs", "shinyalert", "rintrojs", "V8", "ggplot2",
  "shinythemes", "lcqc", "bslib", "DT", "plotly", "xts"
)
library(lcqc)
library(shiny)
library(shiny.i18n)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(rintrojs)
library(V8)
library(ggplot2)
library(shinythemes)
library(plotly)
library(jsonlite)
library(shinyFiles)
library(fs)
library(shinyhelper)
library(waiter)

# Source server and UI module files
source(system.file("shiny-app", "server_modules", "tab1_infile_read_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab2_baseline_correction_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab3_smoothing_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab4_peak_detect_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab5_integration_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab6_deconvolution_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab7_peak_symmetry_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab8_performance_metrics_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "tab9_report_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "export_server.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "status_bar_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "settings_modal_ui.R", package = "lcqc"))


source(system.file("shiny-app", "ui_modules", "tab1_infile_read_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab2_baseline_correction_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab3_smoothing_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab4_peak_detect_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab5_integration_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab6_deconvolution_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab7_peak_symmetry_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab8_performance_metrics_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "tab9_report_ui.R", package = "lcqc"))
source(system.file("shiny-app", "ui_modules", "export_modal_ui.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "status_bar_server.R", package = "lcqc"))
source(system.file("shiny-app", "server_modules", "settings_server.R", package = "lcqc"))


source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "info_translator.R", package = "lcqc"))

source(system.file("shiny-app", "intro", "general_intro.R", package = "lcqc"))
sapply(paste0("tab", 1:9, "_intro.R"), function(file) {
  source(system.file("shiny-app", "intro", file, package = "lcqc"))
})



version <- as.character(packageVersion("lcqc"))
lib_name <- "LCQC v"
html_title <- paste(lib_name, version, sep = "")
merge_translation_jsons(dir_path = "./www/i18n/", output_filename = "translation.json")

i18n <- Translator$new(translation_json_path = "./www/i18n/translation.json")
i18n$use_js() #This needs to be called in order for live translation to work in the dashboard header (or anywhere else)

# i18n <- Translator$new(translation_json_path = system.file("shiny-app", "www","i18n","translation.json", package = "lcqc"))
i18n$set_translation_language("en")

jsext <- distabs()

# Read the configuration file
app_config_path <- system.file("shiny-app", "config", "default_settings.json", package = "lcqc")

config <- tryCatch(
  {
    jsonlite::read_json(app_config_path)
  },
  error = function(e) {
    message("An error occurred reading config: ", e$message)
    NULL
  }
)

# Set default export path to user's home directory for cross-platform compatibility
if (!is.null(config)) {
  config$rpr$expath <- normalizePath("~")
}

options(shiny.reactlog = TRUE)
ui <- fluidPage(
  class = "lcqc-app-container", # Add a class for styling
  usei18n(i18n),
  useShinyjs(),

  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      div(style="text-align:center;color:white",
          tags$h2(i18n$t("LCQC is loading...")),
          waiter::spin_ellipsis()
      )
    ),
    color = "darkgrey"
  ),

  extendShinyjs(text = jsext$jscode, functions = c("disableTab", "enableTab")),
  inlineCSS(jsext$css),
  introjsUI(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
  ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "shared/introjs/introjs.min.css")),
  fluidRow(
    id = "navbuts",
    style = 'margin-left: 0%; margin-right: 0%;',
    div(tags$h1(paste0(html_title, "    "), style = "display: inline-block; float: left;"),
        tags$a(href = "https://www.altraflora.com", tags$img(src = "custom_logo.jpg", style = "max-height:300px; max-width:300px;
        display: inline-block;vertical-align: middle; horizontal-align: left; float: left; margin-left: 5px; margin-top: 2px;"))),
    div(style = ".stripe width: 100%; display: inline-block; vertical-align: middle; horizontal-align: right; float: right",
        div(
        dropdownButton(
          label=i18n$t("Intro"),
          actionButton("intro_general",
                       i18n$t("General Tour"),
                       icon = icon("play-circle"),
                       class = "btn-block"
          ),
          actionButton("intro_current_tab",
                       i18n$t("Current Tab"),
                       icon = icon("info-circle"),
                       class = "btn-block"
          ),
          circle = FALSE,
          status = "info",
          icon = icon("circle-info")
          #tooltip = tooltipOptions(title = i18n$t("Yardım"))
        ),
        style = "display: flex; gap: 10px; align-items: center;",
        actionButton(
          inputId = "export_button",
          label = i18n$t("Export"),
          icon = icon("download")
        ),
        actionButton(
          inputId = "settings_button",
          label = i18n$t("Save/load method"),
          icon = icon("cog")
        ),
        div(id = "lang_select",
          style = "display: flex; align-items: center;margin-top:15px;margin-right:0px;",
          pickerInput(
            inputId = "selected_language",
            label = NULL,
            choices = setNames(i18n$get_languages(), c("English", "Русский", "Türkçe")),
            selected = i18n$get_key_translation(),
            width = "100%",
            options = list(
              style = "btn-default",
              size = 5
            )
          )
        )
      )
    )
  ),
  div(id = "status_bar_div",
      status_bar_ui("status_bar", i18n = i18n)),
  tags$br(),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(i18n$t("1. File Data Import"), infile_read_ui("infile", config, i18n = i18n), value = "dimp", id = "dimp"),
    tabPanel(i18n$t("2. Baseline Correction"), baseline_correction_ui("baseline", config, i18n = i18n), value = "bline", id = "bline"),
    tabPanel(i18n$t("3. Smoothing"), smoothing_ui("smoothing", config, i18n = i18n), value = "smooth", id = "smooth"),
    tabPanel(i18n$t("4. Peak Detection"), peak_detect_ui("peak_detect", config, i18n = i18n), value = "pdet", id = "pdet"),
    tabPanel(i18n$t("5. Integration"), integration_ui("integration", config, i18n = i18n), value = "integ", id = "integ"),
    tabPanel(i18n$t("6. Deconvolution"), deconvolution_ui("deconvolution", config, i18n = i18n), value = "dconv", id = "dconv"),
    tabPanel(i18n$t("7. Peak Symmetry"), peak_symmetry_ui("symmetry", config, i18n = i18n), value = "psym", id = "psym"),
    tabPanel(i18n$t("8. Performance Metrics"), performance_metrics_ui("performance", config, i18n = i18n), value = "perf", id = "perf"),
    tabPanel(i18n$t("9. Reporting"), report_ui("rep", config, i18n = i18n), value = "rpr", id = "rpr")
  )
)


server <- function(input, output, session) {

  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
  })

  observe_helpers(help_dir = "help_mds", withMathJax = TRUE)
  sync <- reactiveValues(
    infile = reactiveValues(
      chromatogram = NULL,
      metadata = NULL,
      peak_table = NULL,
      completed = FALSE
    ),
    baseline = reactiveValues(
      chromatogram = NULL,
      method = NULL,
      settings = list(),
      completed = FALSE
    ),
    smoothing = reactiveValues(
      smoothing_data = NULL,
      completed = FALSE
    ),
    peak_detection = reactiveValues(
      peak_data = NULL,
      completed = FALSE
    ),
    integration = reactiveValues(
      completed = FALSE
    ),
    deconvolution = reactiveValues(
      completed = FALSE
    ),
    symmetry = reactiveValues(
      completed = FALSE
    ),
    metrics = reactiveValues(
      completed = FALSE
    ),
    reporting = reactiveValues(
      completed = FALSE
    ),
    status_bar = reactiveValues(
      file_name = i18n$t("-")
    ),
    settings = reactiveValues(
      name = i18n$t("-")
    )
  )

  output$status_bar <- renderUI({
    status_bar_ui
  })

  # Disable tabs on page load
  distab_vec <- c("bline", "smooth", "pdet", "integ", "dconv", "psym", "perf", "rpr")
  for(i in distab_vec) js$disableTab(i)

  # Disable export button initially
  shinyjs::disable("export_button")


  observe({
    req(input$main_tabs)
    tab_dependencies <- list(
      infile = "bline",
      baseline = "smooth",
      smoothing = "pdet",
      peak_detection = "integ",
      integration = c("dconv", "psym", "perf"),
      metrics = "rpr"
    )

    # Iterate through the dependencies and enable tabs if the task is completed
    for (task in names(tab_dependencies)) {
      if (sync[[task]]$completed) {
        tabs_to_enable <- tab_dependencies[[task]]
        if (!is.vector(tabs_to_enable)) {
          tabs_to_enable <- c(tabs_to_enable)
        }
        for (tab in tabs_to_enable) {
          js$enableTab(tab)
        }
      }
    }

    # Enable export button if peak detection is completed (Tab 4)
    if (sync$peak_detection$completed) {
      shinyjs::enable("export_button")
    } else {
      shinyjs::disable("export_button")
    }
  })

  # Call module server functions
  infile_read_server("infile", sync, config, i18n = i18n)
  baseline_correction_server("baseline", sync, config, i18n = i18n)
  smoothing_server("smoothing", sync, config, i18n = i18n)
  peak_detect_server("peak_detect", sync, input, config, i18n = i18n)
  integration_server("integration", sync, config, i18n = i18n)
  deconvolution_server("deconvolution", sync, config, i18n = i18n)
  peak_symmetry_server("symmetry", sync, config, i18n = i18n)
  performance_metrics_server("performance", sync, config, i18n = i18n)
  report_server("rep", sync, config, i18n = i18n)
  export_server("export", sync, export_button_input = reactive(input$export_button),i18n = i18n) # Pass reactive input
  status_bar_server("status_bar", input, output, session, sync, i18n = i18n)

  # Call settings_server if it's intended to be active
  settings_server("settings", sync, config, input, session, i18n = i18n) # Pass the main input and session objects

  # Hide waiter
  waiter_hide()

  observeEvent(input$settings_button, {
    showModal(settings_modal_ui("settings", i18n = i18n))
  })

  # General Introduction
  observeEvent(input$intro_general, {
    introjs(session, options = intro_steps_general(i18n))
  })

  # Current tab specific intro
  observeEvent(input$intro_current_tab, {
    current_tab <- input$main_tabs


    if (current_tab == "dimp") {
      introjs(session, options = intro_steps_current_tab1(i18n))
    } else if (current_tab == "bline") {
      introjs(session, options = intro_steps_current_tab2(i18n))
    } else if (current_tab == "smooth") {
      introjs(session, options = intro_steps_current_tab3(i18n))
    } else if (current_tab == "pdet") {
      introjs(session, options = intro_steps_current_tab4(i18n))
    } else if (current_tab == "integ") {
      introjs(session, options = intro_steps_current_tab5(i18n))
    } else if (current_tab == "dconv") {
      introjs(session, options = intro_steps_current_tab6(i18n))
    } else if (current_tab == "psym") {
      introjs(session, options = intro_steps_current_tab7(i18n))
    } else if (current_tab == "perf") {
      introjs(session, options = intro_steps_current_tab8(i18n))
    } else if (current_tab == "rpr") {
      introjs(session, options = intro_steps_current_tab9(i18n))
    }
  })
}

shinyApp(ui = ui, server = server)
