# server_modules/data_import_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "lcqc_adapter.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "messages.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "subscription.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab1_intro.R", package = "lcqc"))

infile_read_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Observers that dynamically hide output tabs
    # Metadata tab
    tabvec <- setNames(c("metadata_tab", "similaritytable_tab", "peaktable_tab", "peaknames_tab"),
                       c("metadata", "simtable", "ptable", "pnames"))

    lapply(names(tabvec), function(input_name) {
      observeEvent(input[[input_name]], {
        toggle_tab_enabled(
          session      = session,
          tabset_id    = "shimadzuMainTabs",
          tab_value    = ns(tabvec[[input_name]]),
          enabled      = isTRUE(input[[input_name]]),
          fallback_tab = ns("chromatogram_tab")
        )
      }, ignoreInit = TRUE)
    })

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab1_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab1_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

    #Render UI elements
    #CSV upload
    output$csvFile_div <- renderUI({
      div(
        id = ns("csvFile_div"),
        fileInput(ns("csvFile"), i18n_r()$t("Choose CSV File"), accept = c(
          ".csv"),buttonLabel = i18n$t("Browse"), placeholder = i18n_r()$t("No file selected"))
      )
    })

    #TXT upload
    output$file1_div <- renderUI({
      div(
        id = ns("file1_div"),
        fileInput(ns("file1"), i18n_r()$t("Choose TXT File"), accept = c(
          ".txt"),buttonLabel = i18n$t("Browse"), placeholder = i18n_r()$t("No file selected"))
      )
    })

    #For table translation
    tablang <- reactive({
      list(
        sDecimal = i18n$t("."),
        sInfoThousands = i18n$t(","),
        sProcessing = i18n$t("Processing..."),
        sSearch = i18n$t("Search:"),
        sLengthMenu = i18n$t("Show _MENU_ entries"),
        sInfo = i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
        sInfoEmpty = i18n$t("Showing 0 to 0 of 0 entries"),
        sInfoFiltered = i18n$t("(filtered from _MAX_ total entries)"),
        sInfoPostFix = "",
        sLoadingRecords = i18n$t("Loading..."),
        sZeroRecords = i18n$t("No matching records found"),
        sEmptyTable = i18n$t("No data available in table"),
        oPaginate = list(
          sFirst = i18n$t("First"), sPrevious = i18n$t("Previous"),
          sNext = i18n$t("Next"), sLast = i18n$t("Last")
        ),
        oAria = list(
          sSortAscending = i18n$t(": activate to sort column ascending"),
          sSortDescending = i18n$t(": activate to sort column descending")
        ))
    })

    #Update Dropdown lists
    output$dataSource <- renderUI({
      selectInput(ns("dataSource"), label = i18n$t("Data Source"),
                  choices = setNames(c("Shimadzu TXT", "CSV File"
                  ), i18n_r()$t(c("Shimadzu TXT","CSV File"))),
                  selected = config$dimp$dataSource$selected)
    })

    output$mode <- renderUI({
      selectInput(ns("mode"), label = i18n$t("Chromatogram Mode"),
                  choices = setNames(c("fid", "gcms", "lc"), i18n_r()$t(c("GC-FID","GC-MS","HPLC"))),
                  selected = config$dimp$mode$selected)
    })


    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab1_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "csvFile_div", suspendWhenHidden = FALSE)
    outputOptions(output, "file1_div", suspendWhenHidden = FALSE)
    outputOptions(output, "dataSource", suspendWhenHidden = FALSE)
    outputOptions(output, "mode", suspendWhenHidden = FALSE)

    # CSV File Logic
    csv_data <- reactive({
      req(input$csvFile)

      sync$infile$completed <- FALSE
      sync$smoothing$completed <- FALSE
      sync$baseline$completed <- FALSE
      sync$peak_dtection$completed <- FALSE

      sync$status_bar$file_name <- input$csvFile$name
      csv_trange <- c(input$csv_trange_lower, input$csv_trange_upper)
      csv_trange <- csv_trange[!is.na(csv_trange)]

      df <- read_data(input$csvFile$datapath, input$csvHeader, input$csvSep, input$csvDecsep, session)

      if (!is.null(df) && length(csv_trange) == 2) {
        time_col <- input$timeColumn
        if (time_col %in% names(df)) {
          df <- df[df[[time_col]] >= csv_trange[1] & df[[time_col]] <= csv_trange[2], ]
        } else {
          show_error(session, i18n_r()$t("Time range cannot be constrainted since the corresponding column was not found in the data."))
        }
      }
      return(df)
    })

    observe({
      df <- csv_data()
      if (!is.null(df)) {
        updateSelectInput(session, "timeColumn", choices = names(df), selected = names(df)[1])
        updateSelectInput(session, "intensityColumn", choices = names(df), selected = names(df)[2])
      } else {
        updateSelectInput(session, "timeColumn", choices = NULL, selected = NULL)
        updateSelectInput(session, "intensityColumn", choices = NULL, selected = NULL)
      }
    })


    csv_chromatogram_data <- reactive({
      csv_data()
    })


    output$csvChromatogramPlot <- renderPlotly({
      chromatogram_data <- csv_chromatogram_data()


      time_col <- input$timeColumn
      intensity_col <- input$intensityColumn

      req(nrow(chromatogram_data) > 0 & all(c(time_col, intensity_col) %in% colnames(chromatogram_data)))


      if(!all(c(time_col,intensity_col) %in% colnames(chromatogram_data))) {
        show_error(session, "Selected time or intensity columns not found in the data.")
        sync$infile$completed <- FALSE
        return(NULL)
      }

      tryCatch(
        {
          p <- plot_ly(data = chromatogram_data, x = as.formula(paste0("~", time_col)), y = as.formula(paste0("~", intensity_col)), type = "scatter", mode = "lines") %>%
            layout(
              title = i18n_r()$t("Chromatogram"),
              xaxis = list(title = i18n_r()$t("Retention Time (min)")),
              yaxis = list(title = i18n_r()$t("Signal Intensity"))
            )

          sync$infile$chromatogram <- chromatogram_data
          sync$infile$completed <- TRUE
          return(p)
        },
        error = function(e) {
          show_error(session, paste(i18n_r()$t("Error creating plot:"), e$message))
          sync$infile$completed <- FALSE
          return(NULL)
        }
      )

    })

    observeEvent(input$file1,
                 {
                   inFile <- input$file1
                   sync$status_bar$file_name <- input$file1$name
                   if (!is.null(inFile) && !is.null(inFile$datapath)) {
                     # Assuming detect_shimadzu_mode() reads the first few lines and returns
                     # "lc", "gcms", "fid", or NULL
                     detected_mode <- detect_shimadzu_mode(inFile$datapath)
                     if (!is.null(detected_mode)) {

                       # Use updateSelectInput with the namespaced input ID
                       updateSelectInput(session, "mode", selected = detected_mode)
                     }
                   }
                 },
                 ignoreInit = TRUE
    ) # ignoreInit = TRUE, prevents it from running on app start



    # Shimadzu TXT file logic
    shim_data <- reactive({
      inFile <- input$file1
      sync$infile$completed <- FALSE
      sync$smoothing$completed <- FALSE
      sync$baseline$completed <- FALSE
      sync$peak_dtection$completed <- FALSE

      if (is.null(inFile)) {
        return(NULL)
      }

      shim_trange <- c(input$shim_trange_lower, input$shim_trange_upper)
      shim_trange <- shim_trange[!is.na(shim_trange)]

      if (length(shim_trange) == 0) {
        shim_trange <- NA
      }


      ad_read_shim(
        file = inFile$datapath,
        ptable = input$ptable,
        simtable = input$simtable,
        pnames = input$pnames,
        pcas = input$pcas,
        metadata = input$metadata,
        mode = input$mode,
        sep = input$sep,
        decsep = input$decsep,
        fix_names = input$fix_names,
        fil_cols = input$fil_cols,
        trange = shim_trange,
        session = session
      )
    })

    observe({
      data <- shim_data()
      chromatogram_name <- input$chromatogramChoice

      if (!is.null(data) && !is.null(data$chromatogram) && !is.null(chromatogram_name) && chromatogram_name %in% names(data$chromatogram)) {
        selected_chromatogram <- data$chromatogram[[chromatogram_name]]
        all_chromatogram_names <- names(selected_chromatogram)

        available_intensity_columns <- setdiff(all_chromatogram_names, input$shimTimeColumn)

        updateSelectInput(session, "shimTimeColumn", choices = all_chromatogram_names, selected = if (length(all_chromatogram_names) >= 1) all_chromatogram_names[1] else "")
        updateSelectInput(session, "shimIntensityColumn", choices = available_intensity_columns, selected = if (length(available_intensity_columns) >= 1) available_intensity_columns[1] else "")
      } else {
        updateSelectInput(session, "shimTimeColumn", choices = NULL, selected = NULL)
        updateSelectInput(session, "shimIntensityColumn", choices = NULL, selected = NULL)
      }
    })


    observe({
      data <- shim_data()
      if (!is.null(data)) {
        chromatogram_names <- names(data$chromatogram)
        updateSelectInput(session, "chromatogramChoice", choices = chromatogram_names, selected = if (length(chromatogram_names) > 0) chromatogram_names[1] else NULL)

        peak_table_names <- names(data$ptable)
        updateSelectInput(session, "peakTableChoice", choices = peak_table_names)
      } else {
        updateSelectInput(session, "chromatogramChoice", choices = NULL, selected = NULL)
        updateSelectInput(session, "peakTableChoice", choices = NULL, selected = NULL)
      }
    })

    observeEvent(selected_chromatogram_data(), {
      chromatogram_data <- selected_chromatogram_data()
      if (!is.null(chromatogram_data)) {
        time_col_name <- colnames(chromatogram_data)[grep("time", colnames(chromatogram_data), ignore.case = TRUE)[1]]

        if (!is.na(time_col_name)) {
          min_rt <- min(chromatogram_data[[time_col_name]], na.rm = TRUE)
          max_rt <- max(chromatogram_data[[time_col_name]], na.rm = TRUE)

          # Update lower limit only if config value is empty or NA
          if (is.null(config$dimp$shim_trange_lower$value) || config$dimp$shim_trange_lower$value == "" || is.na(config$dimp$shim_trange_lower$value)) {
            updateNumericInput(session, "shim_trange_lower", value = min_rt)
          }

          # Update upper limit only if config value is empty or NA
          if (is.null(config$dimp$shim_trange_upper$value) || config$dimp$shim_trange_upper$value == "" || is.na(config$dimp$shim_trange_upper$value)) {
            updateNumericInput(session, "shim_trange_upper", value = max_rt)
          }
        }
      }
    })

    selected_chromatogram_data <- reactive({
      data <- shim_data()
      chromatogram_name <- input$chromatogramChoice

      if (is.null(data) || is.null(chromatogram_name) ||
          !chromatogram_name %in% names(data$chromatogram)) {
        return(NULL)
      }

      data$chromatogram[[chromatogram_name]]
    })

    all_chromatogram_data <- reactive({
      data_source <- input$dataSource

      if (data_source == "Shimadzu TXT") {
        data <- shim_data()
        if (!is.null(data)) {
          chromatogram_name <- input$chromatogramChoice
          if (!is.null(chromatogram_name) && chromatogram_name %in% names(data$chromatogram)) {
            return(data$chromatogram[[chromatogram_name]])
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      } else if (data_source == "CSV File") {
        return(csv_chromatogram_data())
      } else {
        return(NULL)
      }
    })

    #Chromatogram table
    #Monitor selected language

    output$infile_chromatogram_data <- renderDataTable({

      #Set translation language
      #langs <- c("en" = "English", "ru" = "Russian", "tr" = "Turkish")
      #langstring <- langs[names(langs) %in% i18n$get_translation_language()]
      #tablang_new <- tablang()
      #print(tablang_new)
      data <- shim_data()
      chromatogram_name <- input$chromatogramChoice

      if (!is.null(data) && !is.null(chromatogram_name) && chromatogram_name %in% names(data$chromatogram)) {
        page_length <- 100
        istenen_satir <- as.numeric(input$satirNumarasi)

        istenen_sayfa <- ceiling(istenen_satir / page_length) - 1

        data$chromatogram[[chromatogram_name]] |>
          DT::datatable(rownames = FALSE,
                        extensions = "Buttons",
                        options = list(
                          dom = "Bfrtip",
                          pageLength = page_length,
                          searching = FALSE,
                          start = istenen_sayfa * page_length,
                          language = tablang(), #paste0("DT_", langstring,".json"

                        buttons = list(
                          list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                               action = copy_button_no_popup(
                                 copy_label = i18n_r()$t("Copy"),
                                 copied_label = i18n_r()$t("Copied!")
                               )),
                          list(extend = "csv", filename = generate_filename_with_timestamp("chromatogram")),
                          list(extend = "excel", filename = generate_filename_with_timestamp("chromatogram")),
                          list(extend = "pdf", filename = generate_filename_with_timestamp("chromatogram"))
                        )),
          )
      } else {
        NULL
      }
    })
    #})
    selected_peak_table_data <- reactive({
      data <- shim_data()
      peak_table_name <- input$peakTableChoice

      if (is.null(data) || is.null(peak_table_name) ||
          !peak_table_name %in% names(data$ptable)) {
        return(NULL)
      }

      data$ptable[[peak_table_name]]
    })

    #Metadata table
    output$infile_metadataTable <- renderDataTable(
      {
        data <- shim_data()
        if (!is.null(data)) {
          metadata_df <- data.frame(
            Property = names(data$metadata),
            Value = unlist(data$metadata),
            stringsAsFactors = FALSE
          )

          metadata_df$Property <- make.names(metadata_df$Property, unique = TRUE)

          metadata_df |>
            DT::datatable(
              extensions = "Buttons",
              rownames = FALSE,
              options = list(
                dom = "Bfrtip",
                pageLength = min(nrow(metadata_df), 100),
                searching = FALSE,
                language = tablang(), #paste0("DT_", langstring,".json"

              buttons = list(
                list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                     action = copy_button_no_popup(
                       copy_label = i18n_r()$t("Copy"),
                       copied_label = i18n_r()$t("Copied!")
                     )),
                list(extend = "csv", filename = generate_filename_with_timestamp("metadata")),
                list(extend = "excel", filename = generate_filename_with_timestamp("metadata")),
                list(extend = "pdf", filename = generate_filename_with_timestamp("metadata"))
              )))
        } else {
          NULL
        }
      },
      options = list(pageLength = 20, searching = FALSE, language = tablang)
    )

    output$data_preview_title <- renderUI({
      req(input[[ns("dataSource")]])
      if (input[[ns("dataSource")]] %in% c("Shimadzu TXT", "CSV File")) {
        h4(paste(i18n_r()$t("Data Preview for: "), input[[ns("dataSource")]]))
      } else {
        h4(i18n_r()$t("Select a Data Source"))
      }
    })


    output$infile_chromatogramPlot <- renderPlotly({
      chromatogram_data <- selected_chromatogram_data()
      peak_table_data <- selected_peak_table_data()

      req(chromatogram_data)
      req(input$shimTimeColumn)
      req(input$shimIntensityColumn)

      if (is.null(chromatogram_data) || !is.data.frame(chromatogram_data) || ncol(chromatogram_data) < 2) {
        return(NULL)
      }

      time_col <- colnames(chromatogram_data)[grep("time", colnames(chromatogram_data), ignore.case = TRUE)[1]]
      intensity_col <- colnames(chromatogram_data)[grep("intensity", colnames(chromatogram_data), ignore.case = TRUE)[1]]

      if (is.na(time_col)) {
        return(NULL)
      }

      time_col <- input$shimTimeColumn
      intensity_col <- input$shimIntensityColumn


      if (is.na(intensity_col)) {
        return(NULL)
      }
      sync$infile$completed <- TRUE


      sync$infile$chromatogram <- chromatogram_data



      if (!is.null(peak_table_data) && is.data.frame(peak_table_data)) {
        rtime_col <- colnames(peak_table_data)[grep("time", colnames(peak_table_data), ignore.case = TRUE)[1]]
        name_col <- colnames(peak_table_data)[grep("name", colnames(peak_table_data), ignore.case = TRUE)[1]]

        if (is.na(rtime_col)) {
          peak_table_data <- NULL
        } else {
          peak_table_data[[rtime_col]] <- as.numeric(peak_table_data[[rtime_col]]) # Use dynamic column name

          if (anyNA(peak_table_data[[rtime_col]])) {
            peak_table_data <- peak_table_data[!is.na(peak_table_data[[rtime_col]]), ]
          }
        }
      } else {
        peak_table_data <- NULL
      }


      p <- plot_ly(data = chromatogram_data, x = as.formula(paste0("~`", time_col, "`")), y = as.formula(paste0("~`", intensity_col, "`")), type = "scatter", mode = "lines", name = i18n_r()$t("Signal")) %>% layout(
        title = i18n_r()$t("Chromatogram"),
        xaxis = list(title = i18n_r()$t("Retention Time (min)")),
        yaxis = list(title = i18n_r()$t("Signal Intensity"), autorange = "max"),
        legend = list(
          x = 1, # X position (0: left, 1: right)
          y = 1, # Y position (0: bottom, 1: top)
          xanchor = "right", # X-axis reference (right edge of legend aligns with x=1)
          yanchor = "top" # Y-axis reference (top edge of legend aligns with y=1)
        )
      )


      p <- add_annotations_from_table(
        p,
        input$showAnnotations,
        peak_table_data,
        chromatogram_data,
        rtime_col,
        name_col,
        time_col,
        intensity_col,
        tolerance = 0.01,
        custom_peakname = i18n_r()$t("Peaks")
      )
      return(p)
    })

    #Peak Table
    output$infile_peak_table <- renderDataTable({
      peak_table_data <- selected_peak_table_data()
      if (!is.null(peak_table_data)) {
        peak_table_data |>
          DT::datatable(rownames = FALSE,
                        extensions = "Buttons",
                        options = list(
                          dom = "Bfrtip",
                          pageLength = min(nrow(peak_table_data), 100),
                          searching = FALSE,
                          language = tablang(), #paste0("DT_", langstring,".json"

                        buttons = list(
                          list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                               action = copy_button_no_popup(
                                 copy_label = i18n_r()$t("Copy"),
                                 copied_label = i18n_r()$t("Copied!")
                               )),
                          list(extend = "csv", filename = generate_filename_with_timestamp("peaktable")),
                          list(extend = "excel", filename = generate_filename_with_timestamp("peaktable")),
                          list(extend = "pdf", filename = generate_filename_with_timestamp("peaktable"))
                        ))
          ) %>%
          formatRound(columns = "Area_Percent", digits = 2)
      } else {
        NULL
      }
    })

    #SIM table
    output$infile_simtable <- renderUI({
      data <- safe_return(shim_data(), "simtable")
      if (!is.null(data)) {
        if (is.list(data) || is.vector(data)) {
          items <- paste("<li>", data, "</li>", collapse = "")
          HTML(paste0("<h4>", i18n_r()$t("Similarity Table"), "</h4><ul>", items, "</ul>"))
        } else {
          HTML(paste0("<p>", i18n_r()$t("No similarity table data available."), "</p>"))
        }
      } else {
        HTML(paste0("<p>", i18n_r()$t("No similarity table data available."), "</p>"))
      }
    })

    #Peak names
    output$infile_pnames <- renderUI({
      data <- safe_return(shim_data(), "pnames")
      if (!is.null(data)) {
        if (is.list(data) || is.vector(data)) {
          items <- paste("<li>", data, "</li>", collapse = "")
          HTML(paste0("<h4>", i18n_r()$t("Peak Names"), "</h4><ul>", items, "</ul>"))
        } else {
          HTML(paste0("<p>", i18n_r()$t("No peak names data available."), "</p>"))
        }
      } else {
        HTML(paste0("<p>", i18n_r()$t("No peak names data available."), "</p>"))
      }
    })

    #CAS numbers
    output$infile_pcas <- renderPrint({
      safe_return(shim_data(), "pcas")
    })

    #INTRO POP-UP PANELS
    # Shimadzu Basic Intro
    observeEvent(input$shimadzu_basic_intro, {
      updateSelectInput(session, "dataSource", selected = "Shimadzu TXT")
      shinyjs::runjs(paste0("$('#", ns("shimadzuOptionsTabs"), " a[data-value=\"tab1_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab1_shimadzu_basic(ns, i18n))
      })
    })

    # Shimadzu Advanced Intro
    observeEvent(input$shimadzu_advanced_intro, {
      updateSelectInput(session, "dataSource", selected = "Shimadzu TXT")
      shinyjs::runjs(paste0("$('#", ns("shimadzuOptionsTabs"), " a[data-value=\"tab1_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab1_shimadzu_advanced(ns, i18n))
      })
    })

    # CSV Basic Intro
    observeEvent(input$csv_basic_intro, {
      updateSelectInput(session, "dataSource", selected = "CSV File")
      shinyjs::runjs(paste0("$('#", ns("csvOptionsTabs"), " a[data-value=\"tab1_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab1_csv_basic(ns, i18n))
      })
    })

    # CSV Advanced Intro
    observeEvent(input$csv_advanced_intro, {
      updateSelectInput(session, "dataSource", selected = "CSV File")
      shinyjs::runjs(paste0("$('#", ns("csvOptionsTabs"), " a[data-value=\"tab1_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab1_csv_advanced(ns, i18n))
      })
    })
  })
}
