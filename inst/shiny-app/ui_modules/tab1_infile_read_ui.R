library(shiny)
library(plotly)
library(DT)
library(tibble)
library(shinyjs)


shimadzuInputPanelUI <- function(id, ns, config, i18n) {

  tagList(
    tabsetPanel(includeCSS("www/fileInput.css"),
      id = ns("shimadzuOptionsTabs"),
      tabPanel(
        value = "tab1_panel_basic",
        title = i18n$t("Basic"),
        div(

          actionButton(ns("shimadzu_basic_intro"),
            "",
            style = "float: right; margin-top: -2px; margin-right:15px;",
            icon = icon("info-circle"),
            class = "btn-light btn-xs"
          )#, #%>% helper(content = i18n$t("tab1_help_en"), type = "markdown", size = "m", style = "color:green;")
          #uiOutput(ns("shimadzu_basic_intro_HELP"))
        ),
         uiOutput(ns('file1_div')), #For uploading Shimadzu TXT file (server-side translation used)
        div(
          id = ns("mode_div"),
          uiOutput(ns("mode"))
        ),
        div(
          id = ns("chromatogramChoice_div"),
          selectInput(ns("chromatogramChoice"), i18n$t("Choose Chromatogram"), choices = NULL)
        ),
        div(
          id = ns("peakTableChoice_div"),
          selectInput(ns("peakTableChoice"), i18n$t("Choose Peak Table"), choices = NULL)),
        div(
          id = ns("shim_trange_div"),
          fluidRow(
            column(
              6,
              tags$div(
                title = "Retention Time Lower Limit",
                numericInput(ns("shim_trange_lower"), i18n$t("RT Lower"), value = config$dimp$shim_trange_lower$value)
              )
            ),
            column(
              6,
              tags$div(
                title = "Retention Time Upper Limit",
                numericInput(ns("shim_trange_upper"), i18n$t("RT Upper"), value = config$dimp$shim_trange_upper$value)
              )
            )
          )
        ),
        div(
        id=ns("shim_column_div"),
        selectInput(ns("shimTimeColumn"), i18n$t("Time Column"), choices = NULL),
        selectInput(ns("shimIntensityColumn"), i18n$t("Intensity Column"), choices = NULL)
        )
      ),
      tabPanel(
        value = "tab1_panel_advanced",
        title = i18n$t("Advanced"),
        div(

          actionButton(ns("shimadzu_advanced_intro"),
            "",
            style = "float: right; margin-top: -2px; margin-right:15px;",
            icon = icon("info-circle"),
            class = "btn-light btn-xs"
          )
        ),
        br(),
        div(
          id = ns("simtable_div"),
          checkboxInput(ns("simtable"), i18n$t("Extract Similarity Table (GC-MS only)"), value = config$dimp$simtable$value)
        ),
        div(
          id = ns("ptable_div"),
          checkboxInput(ns("ptable"), i18n$t("Extract Peak Table"), value = config$dimp$ptable$value)
        ),
        div(id = ns("pnames_div"),
            checkboxInput(ns("pnames"), i18n$t("Retrieve Peak Names"), value = config$dimp$ptable$value)
            ),
        div(id = ns("pcas_div"),
            checkboxInput(ns("pcas"), i18n$t("Retrieve CAS Numbers (GC-MS only)"), value = config$dimp$pnames$value)
            ),
        div(
          id = ns("metadata_div"),
          checkboxInput(ns("metadata"), i18n$t("Retrieve Metadata"), value = config$dimp$metadata$value)
        ),
        div(id = ns("separators_div"),
            selectInput(ns("sep"), i18n$t("Column Separator"), choices = c("auto", ",", ";", "\\t"), selected = config$dimp$sep$selected),
            selectInput(ns("decsep"), i18n$t("Decimal Separator"), choices = c("auto", ".", ","), selected = config$dimp$descep$selected)
            ),
        div(id = ns("fix_names_div"),
            checkboxInput(ns("fix_names"), i18n$t("Fix Column Names"), value = config$dimp$fix_names$value)
            ),
        div(id = ns("fil_cols_div"),
            checkboxInput(ns("fil_cols"), i18n$t("Filter Columns"), value = config$dimp$fil_cols$value)
            )
      )
    )
  )
}


csvInputPanelUI <- function(ns, config, i18n) {
  tagList(
    tabsetPanel(
      id = ns("csvOptionsTabs"),
      tabPanel(
        i18n$t("Basic"),
        div(

          actionButton(ns("csv_basic_intro"),
            "",
            style = "float: right; margin-top: -2px; margin-right:15px;",
            icon = icon("info-circle"),
            class = "btn-light btn-xs"
          )
        ),
         uiOutput(ns('csvFile_div')), #For uploading CSV file (server-side translation used)
        div(
          id = ns("csv_trange_div"),
          fluidRow(
            column(
              6,
              tags$div(
                title = i18n$t("Retention Time Lower Limit"),
                numericInput(ns("csv_trange_lower"), i18n$t("RT Lower"), value = config$dimp$csv_trange_upper$value)
              )
            ),
            column(
              6,
              tags$div(
                title = i18n$t("Retention Time Upper Limit"),
                numericInput(ns("csv_trange_upper"), i18n$t("RT Upper"), value = config$dimp$csv_trange_upper$value)
              )
            )
          )
        ),
        div(
          id = ns("timeColumn_div"),
          selectInput(ns("timeColumn"), i18n$t("Time Column"), choices = NULL)
        ),
        div(
          id = ns("intensityColumn_div"),
          selectInput(ns("intensityColumn"), i18n$t("Intensity Column"), choices = NULL)
        ),
      ),
      tabPanel(
        title = i18n$t("Advanced"),
        div(

          actionButton(ns("csv_advanced_intro"),
            "",
            style = "float: right; margin-top: -2px; margin-right:15px;",
            icon = icon("info-circle"),
            class = "btn-light btn-xs"
          )
        ),
        # Advanced Options

        div(
          id = ns("csvSep_div"),
          selectInput(ns("csvSep"), i18n$t("Column Separator"), choices = c(",", ";", "\\t"), selected = config$dimp$csvSep$selected)
        ),
        div(
          id = ns("csvDecsep_div"),
          selectInput(ns("csvDecsep"), i18n$t("Decimal Separator"), choices = c(".", ","), selected = config$dimp$csvDecSep$value)
        ),
        div(
          id = ns("csvHeader_div"),
          checkboxInput(ns("csvHeader"), i18n$t("Header Row"), value = config$dimp$csvHeader$value)
        ),
      )
    )
  )
}


shimadzuMainPanelContentUI <- function(ns, i18n) {
  tagList(
    tabsetPanel(
      id = ns("shimadzuMainTabs"),
      tabPanel(
        i18n$t("Chromatogram"), plotlyOutput(ns("infile_chromatogramPlot")),
        fluidRow(
          column(
            12,
            checkboxInput(ns("showAnnotations"), i18n$t("Show Peak Markers and Names"),
              value = TRUE,
            )
          )
        ),
      ),
      tabPanel(i18n$t("Chromatogram Data"), value = ns("chromatogram_tab"), dataTableOutput(ns("infile_chromatogram_data"))),
      tabPanel(i18n$t("Peak Table"), value = ns("peaktable_tab"), dataTableOutput(ns("infile_peak_table"))),
      tabPanel(i18n$t("Similarity Table"), value = ns("similaritytable_tab"), htmlOutput(ns("infile_simtable"))),
      tabPanel(i18n$t("Peak Names"), value = ns("peaknames_tab"), htmlOutput(ns("infile_pnames"))),
      tabPanel(i18n$t("Metadata"), value = ns("metadata_tab"), dataTableOutput(ns("infile_metadataTable")))
    )
  )
}

csvMainPanelContentUI <- function(ns,i18n) {
  tagList(

    tabsetPanel(
      id = ns("csvMainTabs"),
      tabPanel(i18n$t("Chromatogram"),

               plotlyOutput(ns("csvChromatogramPlot"))
      )
    )
  )
}
# Main UI function for the module
infile_read_ui <- function(id, config, i18n) {

  ns <- NS(id)

  # Update condition to only show panels when a data source is selected
  shimadzu_condition <- paste0("input['", ns("dataSource"), "'] == 'Shimadzu TXT'")
  csv_condition <- paste0("input['", ns("dataSource"), "'] == 'CSV File'")

  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Add a default "Choose Data" option and make it selected
        # Display the tab title and HELP icon (green)
        div(id = "tab1_title_div", style = "vertical-align:center;", fluidRow(column(10, h3(i18n$t("File Data Import"), style = "margin-top:0;")),
                 column(2, uiOutput(ns("tab1_HELP"))))),
        uiOutput(ns("dataSource")),



        # Wrap input panels in conditionalPanel using the namespaced condition string
        conditionalPanel(
          condition = shimadzu_condition,
          shimadzuInputPanelUI(id, ns, config, i18n)
        ),
        conditionalPanel(
          condition = csv_condition,
          csvInputPanelUI(ns, config, i18n)
        )
      ),
      mainPanel(
        width = 9, # Adjust width as needed for the main panel
        # uiOutput if the title is dynamic based on selection
        uiOutput(ns("data_preview_title")),

        # Wrap main panel content in conditionalPanel using the namespaced condition string
        conditionalPanel(
          condition = shimadzu_condition,
          shimadzuMainPanelContentUI(ns, i18n)
        ),
        conditionalPanel(
          condition = csv_condition,
          csvMainPanelContentUI(ns, i18n)
        )
      )
    )
  )
}
