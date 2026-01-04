# server_modules/tab9_report_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab9_intro.R", package = "lcqc"))

report_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab9_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab9_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

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

    #Render UI elements
    #Custom Logo upload
    output$clogo_div <- renderUI({
      div(
        id = ns("clogo_div"),
        fileInput(ns("clogo"), i18n_r()$t("Upload Custom Logo"), accept = c(".jpg", ".jpeg", ".png"),
                  ,buttonLabel = i18n_r()$t("Browse"), placeholder = i18n_r()$t("No file selected"))
      )
    })

    output$peak_indices <- renderUI({
      textInput(ns("peak_indices"), i18n$t("Enter Peak Indices"), value = NULL, placeholder = i18n_r()$t("2:5 or comma-separated")) #
    })

    #Output directory selection
    output$browse_expath_div <- renderUI({div(
      id = ns("browse_expath_div"),
      shinyDirButton(
        ns("browse_expath"), i18n_r()$t("Browse"),
        title = i18n_r()$t("Select export directory"),
        icon = icon("folder-open")
      )
    )})

    #Update dropdown lists
    chart_names <- reactive({
      if(sync$deconvolution$completed) {
        setNames(c("Peak Integration", "Peak Deconvolution"),
                 i18n_r()$t(c("Peak Integration", "Peak Deconvolution")))
      }  else setNames("Peak Integration", i18n_r()$t("Peak Integration"))
    })

    output$which_chart <- renderUI({
      selectInput(ns("which_chart"), i18n$t("Chart Selection"),
                  choices = chart_names(), selected = config$rpr$which_chart)
    })

    #Add "Peak Deconvolution" if respective data processing is completed
    observeEvent(sync$deconvolution$completed, {
      if (sync$deconvolution$completed) {
        current_choices <- i18n_r()$t(c("Peak Integration", "Peak Deconvolution"))
        updateSelectInput(session, "which_chart", choices = current_choices)
      }
    })

    #Add real-time conditional panel for TPA plot aspect ratio
    #Start hidden if checkbox is FALSE
    observe({
      if (!input$add_tpa) {
        shinyjs::hide("asprat_tpa_div")
      }
    })

    #Show/hide via ShinyJS
    observeEvent(input$add_tpa, {
      if (input$add_tpa) {
        shinyjs::show("asprat_tpa_div")
      } else shinyjs::hide("asprat_tpa_div")
    }, ignoreInit = FALSE)

    #Change value dynamically
    observe({
      req(config$rpr$asprat_tpa)
      updateSliderInput(
        session,
        ns("asprat_tpa"),
        value = config$rpr$asprat_tpa
      )
    })

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab9_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "clogo_div", suspendWhenHidden = FALSE)
    outputOptions(output, "peak_indices", suspendWhenHidden = FALSE)
    outputOptions(output, "browse_expath_div", suspendWhenHidden = FALSE)
    outputOptions(output, "which_chart", suspendWhenHidden = FALSE)

    #Continue processing
    volumes <- c(Home = normalizePath("~"), shinyFiles::getVolumes()())

    shinyDirChoose(input, "browse_expath", roots = volumes, session = session)

    observeEvent(input$browse_expath, {
      path <- parseDirPath(volumes, input$browse_expath) # flat character path



      updateTextInput(session, "expath", value = path)
    })



    # Reactive for selected peaks (indices) - using helper.R parse_peak_indices

    selected_peak_indices <- reactive({
      req(sync$integration$outputs) # Need integrated data to get peak indices

      all_peaks_indices <- sync$integration$outputs$integ_res[["peak"]]

      # Parse comma-separated numeric input for manual selection, handling ranges and "all"
      indices_str <- input$peak_indices
      if (nzchar(indices_str)) {
        # Handle "all" case
        if (tolower(trimws(indices_str)) == "all") {
          return(all_peaks_indices)
        }

        manual_indices <- tryCatch(
          {
            # Use helper.R parse_peak_indices function
            parsed_indices <- parse_peak_indices(indices_str, length(all_peaks_indices))
            if (length(parsed_indices) == 0 && nzchar(indices_str)) {
              stop(i18n_r()$t("Invalid format for peak indices. Please use comma-separated numbers or ranges (e.g., '1,3:5,8') or 'all'."))
            }
            parsed_indices
          },
          error = function(e) {
            showNotification(paste(i18n_r()$t("Error parsing peak indices:"), e$message), type = "error")
            NULL
          }
        )

        if (!is.null(manual_indices) && length(manual_indices) > 0) {
          # Validate indices are within the range of detected peaks
          valid_indices <- manual_indices[manual_indices %in% all_peaks_indices]
          if (length(valid_indices) != length(manual_indices)) {
            invalid_indices <- manual_indices[!manual_indices %in% all_peaks_indices]
            showNotification(
              paste(
                i18n_r()$t("Warning: Invalid peak indices entered:"), paste(invalid_indices, collapse = ", "),
                i18n_r()$t(". Only valid indices will be used.")
              ),
              type = "warning"
            )
          }
          valid_indices
        } else {
          NULL # Return NULL if parsing failed or resulted in empty vector
        }
      } else {
        all_peaks_indices # Return all peaks if input is empty
      }
    })

    # Reactive for peak names
    selected_peak_names <- reactive({
      pnms_str <- input$pnms
      if (nzchar(pnms_str)) {
        # Split by comma and trim whitespace
        names_vec <- sapply(strsplit(pnms_str, ",", fixed = TRUE)[[1]], trimws)
        names_vec
      } else {
        NULL # Return NULL if input is empty
      }
    })

    # Reactive for peak concentrations
    selected_peak_concs <- reactive({
      pconcs_str <- input$pconcs
      if (nzchar(pconcs_str)) {
        # Use base R scan for robust parsing of delimited numbers
        concs_vec <- tryCatch(
          {
            scan(text = pconcs_str, what = numeric(), sep = ",", quiet = TRUE, na.strings = "")
          },
          error = function(e) {
            showNotification(i18n_r()$t("Error parsing peak concentrations. Please use comma-separated numbers."), type = "error")
            NULL
          }
        )
        concs_vec
      } else {
        NULL # Return NULL if input is empty
      }
    })



    # Reactive for selected theoretical plate and asymmetry metrics (`mets`)
    selected_metrics <- reactive({
      c("EP", "AH", "S5", "EMG", "As", "Tf")[
        c(
          input$metric_EP, input$metric_AH, input$metric_S5,
          input$metric_EMG, input$metric_As, input$metric_Tf
        )
      ]
    })

    # Reactive for metrics specifications (`spec`)
    metrics_spec <- reactive({
      mets <- selected_metrics()
      specs <- list() # Initialize an empty list for specifications

      # Populate the list with specifications for selected metrics
      # Only include if the metric is selected and the specification text input is not empty
      process_metric_input <- function(input_value) {
        if (nzchar(input_value)) {
          parsed_values <- as.numeric(unlist(strsplit(input_value, ",")))
          n_peaks <- length(selected_peak_indices()) # Get the number of selected peaks
          if (length(parsed_values) == 1 && n_peaks > 1) {
            return(rep(input_value, n_peaks)) # Keep as character for 'spec' argument
          } else if (length(parsed_values) == n_peaks) {
            # If the number of values matches the number of peaks
            return(unlist(strsplit(input_value, ","))) # Keep as character for 'spec' argument
          } else {
            # Mismatch, return NULL and show a warning
            showNotification(
              paste0(
                i18n_r()$t("Warning: Number of specifications ("), length(parsed_values),
                i18n_r()$t(") for this metric does not match the number of selected peaks ("), n_peaks,
                i18n_r()$t("). Please provide one value or one value per peak.")
              ),
              type = "warning",
              duration = 5
            )
            return(NULL)
          }
        } else {
          return(NULL)
        }
      }

      if ("EP" %in% mets) specs$EP <- process_metric_input(input$spec_EP)
      if ("AH" %in% mets) specs$AH <- process_metric_input(input$spec_AH)
      if ("S5" %in% mets) specs$S5 <- process_metric_input(input$spec_S5)
      if ("EMG" %in% mets) specs$EMG <- process_metric_input(input$spec_EMG)
      if ("As" %in% mets) specs$As <- process_metric_input(input$spec_As)
      if ("Tf" %in% mets) specs$Tf <- process_metric_input(input$spec_Tf)

      if (length(specs) == 0) {
        return(NULL)
      } # Return NULL if no specs are provided for selected metrics
      specs
    })


    # Reactive for text parameters (`tpars`)
    text_parameters <- reactive({
      tpars_list <- list()

      # Populate the list, only including non-empty values
      if (!is.na(input$tpars_dnum) && nzchar(as.character(input$tpars_dnum))) tpars_list$dnum <- input$tpars_dnum
      if (!is.na(input$tpars_oper) && nzchar(as.character(input$tpars_oper))) tpars_list$oper <- input$tpars_oper
      if (!is.na(input$tpars_sn) && nzchar(as.character(input$tpars_sn))) tpars_list$sn <- input$tpars_sn
      if (!is.na(input$tpars_pn) && nzchar(as.character(input$tpars_pn))) tpars_list$pn <- input$tpars_pn
      if (!is.na(input$tpars_desc) && nzchar(as.character(input$tpars_desc))) tpars_list$desc <- input$tpars_desc
      if (!is.na(input$tpars_bn) && nzchar(as.character(input$tpars_bn))) tpars_list$bn <- input$tpars_bn
      if (!is.na(input$tpars_mp) && nzchar(as.character(input$tpars_mp))) tpars_list$mp <- input$tpars_mp
      if (!is.na(input$tpars_sp) && nzchar(as.character(input$tpars_sp))) tpars_list$sp <- input$tpars_sp
      if (!is.na(input$tpars_bp) && nzchar(as.character(input$tpars_bp))) tpars_list$bp <- paste(input$tpars_bp, i18n_r()$t("bar")) # Add unit
      if (!is.na(input$tpars_flow) && nzchar(as.character(input$tpars_flow))) tpars_list$flow <- paste(input$tpars_flow, i18n_r()$t("mL/min")) # Add unit
      if (!is.na(input$tpars_temp) && nzchar(as.character(input$tpars_temp))) tpars_list$temp <- paste(input$tpars_temp, i18n_r()$t("°C")) # Add unit
      if (!is.na(input$tpars_inj) && nzchar(as.character(input$tpars_inj))) tpars_list$inj <- paste(input$tpars_inj, i18n_r()$t("µL")) # Add unit
      if (!is.na(input$tpars_unitc) && nzchar(as.character(input$tpars_unitc))) tpars_list$unitc <- input$tpars_unitc

      # tpars_list$unitc <- "ppm" # Example if static
      tpars_list$tform <- "default" # Always use default timestamp format

      if (length(tpars_list) == 0) {
        return(NULL)
      } # Return NULL if no text parameters are provided
      tpars_list
    })

    # Reactive for additional column metrics (`exmets`)
    selected_additional_metrics <- reactive({
      # Only return selected metrics if additional_metrics (ext) is available
      if (is.null(sync$metrics$addmets_outputs) || !input$add_addmets) {
        return(NULL) # If ext is not available or checkbox is unchecked, no exmets
      }

       selected_metrics <- c()
       if (input$add_addmets) selected_metrics <- c("Linear Velocity", "Packing Porosity", "Flow Resistance", "Permeability")
       if (length(selected_metrics) == 0) {
         NULL # If no metrics are selected, return NULL
       } else {
         selected_metrics # Otherwise return the vector of selected metrics
       }
    })

    # Reactive for asymmetry and TPA (`sym`)
    asym_data <- reactive({ sync$symmetry$outputs})


    # --- Report Generation Logic ---

    observeEvent(input$generate_report, {
      showNotification("Calculation start", type = "warning", duration = 10)

      chrom_plot <- if(input$which_chart == "Peak Integration") sync$integration$outputs$plot else if(input$which_chart == "Peak Deconvolution") sync$deconvolution$outputs$modplot$Modeled_Peaks
      if (is.null(chrom_plot)) {
        showNotification(i18n_r()$t("Integration data is missing. Please carry out integration first!"), type = "error", duration = 15)
        message(i18n_r()$t("DEBUG: ERROR - The chromatogram plotting data (chrom_plot) is NULL."))
        return()
      }

      tpl_data <- sync$metrics$tplate_outputs
      if (is.null(tpl_data) || length(tpl_data) == 0) {
        showNotification(i18n_r()$t("Theoretical plate data is missing."), type = "error", duration = 15)
        message("DEBUG: ERROR - Theoretical plate data (tpl_data) is NULL.")
        return()
      }


      if (is.null(chrom_plot) || is.null(tpl_data)) {
        showNotification(i18n_r()$t("Missing required analytical results (theoretical plates or chromatogram plot)."), type = "error")
        return()
      }

      current_expath <- input$expath
      if (!nzchar(current_expath)) {
        showNotification(i18n_r()$t("Please specify an export directory."), type = "error")
        return()
      }

      # Check if export directory exists and is writable
      if (!dir.exists(current_expath)) {
        tryCatch(
          {
            dir.create(current_expath, recursive = TRUE)
          },
          error = function(e) {
            showNotification(paste(i18n_r()$t("Error creating export directory:"), e$message), type = "error")
            return()
          }
        )
      }

      if (file.access(current_expath, 2) != 0) { # 2 = write permission
        showNotification(i18n_r()$t("Export directory is not writable. Please choose a different location."), type = "error")
        return()
      }

      # --- Prepare Arguments for lcqc_render ---
      render_args <- list(
        tpl = tpl_data,
        cplot = chrom_plot,
        pnms = if (is.null(selected_peak_names())) NA else selected_peak_names(),
        tpars = text_parameters(), # Use the reactive value
        spec = metrics_spec(), # Use the reactive value
        clogo = if (is.null(input$clogo)) NA else input$clogo$datapath, # Handle uploaded logo
        sym = asym_data(),
        ext = if (input$add_addmets && !is.null(sync$metrics$addmets_outputs)) {
          sync$metrics$addmets_outputs
        } else {
          NA # Pass NA if not available or unchecked
        },
        mets = selected_metrics(), # Use the reactive value
        extmets = selected_additional_metrics(), # Use the reactive value
        add_tpa = input$add_tpa,
        expath = current_expath, # Use the validated path
        asprat = input$asprat,
        asprat_tpa = input$asprat_tpa,
        which_pks = selected_peak_indices(), # Use the reactive value
        unit_c = "ppm",
        tform = "default",
        addpars = list(replabel = i18n_r()$t("HPLC_QC_REPORT_1_en"),
                       nametag = "Peak",
                       chrom_labs = c(title = "", x = i18n_r()$t("Time (min)"), y = i18n_r()$t("Signal")),
                       tpa_labs = c(resid = i18n_r()$t("Absolute Residual Sum"),
                                    front = i18n_r()$t("Fronting"),
                                    tail = i18n_r()$t("Tailing"),
                                    peak = i18n_r()$t("Baseline-Adjusted Peak"),
                                    abs_resid = i18n_r()$t("Absolute Residuals"),
                                    gauss = i18n_r()$t("Gaussian Model"),
                                    x = i18n_r()$t("Time (min)"),
                                    y = i18n_r()$t("Signal")))
      )

      # ---- Add peak names / concentrations ---------------------------------------
      pnames <- selected_peak_names()
      pconcs <- selected_peak_concs()
      n_peaks <- length(render_args$which_pks)

      ## ---- names -----------------------------------------------------------------
      if (!is.null(pnames) && length(pnames) != n_peaks) {
        showNotification(
          paste0(
            i18n_r()$t("Warning: Number of peak names ("), length(pnames),
            i18n_r()$t(") does not match the number of selected peaks ("), n_peaks,
            i18n_r()$t("). Peak names will be ignored.")
          ),
          type = "warning"
        )
        pnames <- NULL
      }

      if (!is.null(pnames)) {
        render_args$pnms <- pnames
        render_args$add_pnums <- input$add_pnums
      } else {
        render_args$add_pnums <- FALSE
      }

      ## ---- concentrations --------------------------------------------------------
      if (!is.null(pconcs) && length(pconcs) != n_peaks) {
        showNotification(
          paste0(
            i18n_r()$t("Warning: Number of peak concentrations ("), length(pconcs),
            i18n_r()$t(") does not match the number of selected peaks ("), n_peaks,
            i18n_r()$t("). Peak concentrations will be ignored.")
          ),
          type = "warning"
        )
        pconcs <- NULL
      }

      if (!is.null(pconcs)) {
        render_args$pconcs <- pconcs
      }

      render_args$fontsize <- input$fontsize

      # --- Call lcqc_render ---
      output$render_log <- renderUI({
        HTML(paste0("<p>", i18n_r()$t("Generating report..."), "</p>")) # Initial message
      })

      withProgress(message = i18n_r()$t("Generating Performance Report"), value = 0, {
        incProgress(0.1, detail = i18n_r()$t("Preparing data for render..."))

        tryCatch(
          {
            # Dynamically construct the call expression to avoid passing NULLs explicitly
            # when the corresponding inputs are unchecked or empty.
            # Build the arguments list carefully, excluding NULL values where NA is required by lcqc_render.
            final_render_args <- list(
              tpl = render_args$tpl,
              cplot = render_args$cplot,
              tpars = render_args$tpars
            )

            # Add optional arguments only if they are not NULL/NA as required by lcqc_render
            if (!is.null(render_args$spec)) final_render_args$spec <- render_args$spec
            if (!is.na(render_args$clogo)) final_render_args$clogo <- render_args$clogo
            # lcqc_render expects NA, not NULL, for sym and ext if not used
            final_render_args$sym <- if (is.null(render_args$sym)) NA else render_args$sym
            final_render_args$ext <- if (is.null(render_args$ext)) NA else render_args$ext
            final_render_args$mets <- render_args$mets
            if (!is.null(render_args$exmets)) final_render_args$exmets <- render_args$exmets
            final_render_args$add_tpa <- render_args$add_tpa
            final_render_args$expath <- render_args$expath
            final_render_args$asprat <- render_args$asprat
            final_render_args$asprat_tpa <- render_args$asprat_tpa
            final_render_args$which_pks <- render_args$which_pks
            final_render_args$addpars <- render_args$addpars #ADDITIONAL TEXT PARAMETERS (FOR TRANSLATION)

            if (!is.null(render_args$pnms)) {
              final_render_args$pnms <- render_args$pnms
              final_render_args$add_pnums <- render_args$add_pnums
            }
            if (!is.null(render_args$pconcs)) {
              final_render_args$pconcs <- render_args$pconcs
            }


            final_render_args$fontsize <- render_args$fontsize

            incProgress(0.4, detail = i18n_r()$t("Rendering..."))

            # Use do.call to call lcqc_render with the list of arguments
            report_file_path <- do.call(lcqc::lcqc_render, final_render_args)

            incProgress(0.8, detail = "Finalizing...")
            Sys.sleep(1) # Simulate finalization time

            output$render_log <- renderUI({
              HTML(paste0("<p>", i18n_r()$t("Report successfully generated at: "), render_args$expath, "</p>"))
            })
            showNotification(paste0(i18n_r()$t("Report generated successfully in "), current_expath), type = "message")
          },
          error = function(e) {
            output$render_log <- renderUI({
              HTML(paste0("<p style='color:red;'>", i18n_r()$t("Error generating report: "), e$message, "</p>"))
            })
            showNotification(paste0(i18n_r()$t("Report generation failed: "), e$message), type = "error", duration = NULL)
          }
        )
      })
    })

    # Report Options Intro
    observeEvent(input$report_options_intro, {
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab9_report_options(ns, i18n))
      })
    })

    # Metrics and Specifications Intro
    observeEvent(input$metrics_specifications_intro, {
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab9_metrics_specifications(ns, i18n))
      })
    })

    # Report Content Intro
    observeEvent(input$report_content_intro, {
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab9_report_content(ns, i18n))
      })
    })

    # --- Visual Select Peaks Modal Logic ---

    # Modal Observer (3-column layout, inspired by Tab 7)
    observeEvent(input$visual_select_peaks, {
      req(sync$peak_detection$outputs$results$Peak_Extents)

      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      if (is.null(peak_data) || nrow(peak_data) == 0) {
        showNotification(i18n_r()$t("No peaks detected. Please perform peak detection first."), type = "warning")
        return()
      }

      max_peaks <- nrow(peak_data)

      # Parse current selections
      current_indices <- tryCatch(parse_peak_indices(input$peak_indices, max_peaks), error = function(e) integer(0))
      current_names <- if(nzchar(input$pnms)) strsplit(input$pnms, ",", fixed = TRUE)[[1]] else character(0)
      current_concs <- if(nzchar(input$pconcs)) {
        scan(text = input$pconcs, what = numeric(), sep = ",", quiet = TRUE)
      } else numeric(0)

      showModal(modalDialog(
        title = i18n$t("Visual Peak Selection with Names & Concentrations"),
        size = "l", # Extra large for 3 columns
        fluidRow(
          # Column 1: Peak Information Table
          column(6,
                 h4(i18n$t("Peak Information")),
                 DT::dataTableOutput(ns("modal_report_peak_table"))
          ),
          # Column 2: Checkboxes
          column(2,
                 h4(i18n$t("Select Peaks")),
                 div(style = "max-height: 500px; overflow-y: scroll;",
                     uiOutput(ns("modal_report_peaks_checkboxes"))
                 )
          ),
          # Column 3: Dynamic Name/Conc Inputs
          column(4,
                 h4(i18n$t("Names & Concentrations")),
                 div(style = "max-height: 500px; overflow-y: scroll;",
                     uiOutput(ns("modal_report_dynamic_inputs"))
                 )
          )
        ),
        uiOutput(ns("modal_report_validation_message")),
        footer = tagList(
          modalButton(i18n$t("Cancel")),
          actionButton(ns("modal_report_apply"), i18n$t("Apply"), class = "btn-primary")
        )
      ))

      # Render peak table (with RT, Area, Height, S/N columns)
      #Prepare column names for modal data tables
      modal_prettynames <- reactive({
        c(i18n_r()$t("Peak"),
          i18n_r()$t("RT (min)"),
          i18n_r()$t("Area"),
          i18n_r()$t("Signal/Noise"),
          i18n_r()$t("Width"))
      })

      output$modal_report_peak_table <- DT::renderDataTable({
        req(peak_data)

        # Check if required columns exist
        if (is.null(peak_data$peak) || is.null(peak_data$rt_sigmax)) {
          return(DT::datatable(data.frame(Error = "Peak data columns not found"),
                               options = list(language = tablang()),
                               rownames = FALSE))
        }

        display_data <-  data.frame(
          Peak = peak_data$peak,
          RT = peak_data$rt_sigmax,
          Area = peak_data$top_pa,
          SN = peak_data$sn_ratio,
          Width = peak_data$inf_wd
        )

        DT::datatable(display_data, colnames = modal_prettynames(),
                      options = list(scrollY = "400px", paging = FALSE, searching = FALSE, language = tablang()),
                      rownames = FALSE) %>%
          formatRound(columns = c("RT", "Area", "SN", "Width"), digits = 2)
      })

      # Render checkboxes
      output$modal_report_peaks_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak"), " %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_report_peaks_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = if(length(current_indices) > 0) current_indices else NULL)
      })
    })

    # Dynamic Inputs Observer - shows name/conc inputs only for selected peaks
    observe({
      req(input$modal_report_peaks_sel)

      selected <- sort(as.numeric(input$modal_report_peaks_sel))

      output$modal_report_dynamic_inputs <- renderUI({
        if(length(selected) == 0) {
          div(class = "alert alert-info",
              icon("info-circle"),
              i18n$t("Select peaks to enter names and concentrations"))
        } else {
          # Get current values for prepopulation (if any)
          current_names <- if(!nzchar(input$pnms)) character(length(selected)) else {
            existing <- trimws(strsplit(input$pnms, ",", fixed = TRUE)[[1]])
            if(length(existing) >= length(selected)) existing[1:length(selected)] else existing
          }
          current_concs <- if(!nzchar(input$pconcs)) numeric(length(selected)) else {
            existing <- scan(text = input$pconcs, what = numeric(), sep = ",", quiet = TRUE)
            if(length(existing) >= length(selected)) existing[1:length(selected)] else existing
          }

          # Create inputs for each selected peak
          inputs_list <- lapply(seq_along(selected), function(i) {
            pk_idx <- selected[i]
            name_val <- if(i <= length(current_names) && nzchar(current_names[i])) current_names[i] else ""
            conc_val <- if(i <= length(current_concs) && !is.na(current_concs[i])) current_concs[i] else NA

            div(style = "margin-bottom: 5px; padding: 5px; background-color: #f8f9fa; border-radius: 5px;",
                tags$strong(sprintf(paste0(i18n_r()$t("Peak")," %d:"), pk_idx)),
                fluidRow(
                  column(6, textInput(ns(paste0("modal_name_", pk_idx)),
                                      i18n_r()$t("Name"), value = name_val,
                                      placeholder = "e.g., Caffeine")),
                  column(6, numericInput(ns(paste0("modal_conc_", pk_idx)),
                                         i18n_r()$t("Conc."), value = conc_val,
                                         min = 0, step = 1))
                )
            )
          })
          do.call(tagList, inputs_list)
        }
      })
    })

    # Validation Observer
    observe({
      req(!is.null(input$modal_report_peaks_sel))
      len <- length(input$modal_report_peaks_sel)

      output$modal_report_validation_message <- renderUI({
        if (len == 0) {
          div(class = "alert alert-info",
              icon("info-circle"),
              i18n$t("Select at least one peak"))
        } else {
          div(class = "alert alert-success",
              icon("check-circle"),
              sprintf(i18n$t("Ready: %d peaks selected"), len))
        }
      })

      valid <- len > 0
      shinyjs::toggleState("modal_report_apply", condition = valid)
    })

    # Apply Observer - updates main inputs with selected indices, names and concentrations
    observeEvent(input$modal_report_apply, {
      selected <- sort(as.numeric(input$modal_report_peaks_sel))

      # 1. Update peak indices
      indices_text <- paste(selected, collapse = ",")
      updateTextInput(session, "peak_indices", value = indices_text)

      # 2. Collect and set names (only if at least one is provided and non-empty)
      names_vals <- sapply(selected, function(pk_idx) {
        val <- input[[paste0("modal_name_", pk_idx)]]
        if(is.null(val) || !nzchar(trimws(val))) "" else trimws(val)
      })
      names_text <- paste(names_vals, collapse = ",")
      if(any(nzchar(names_vals))) {
        updateTextInput(session, "pnms", value = names_text)
      }

      # 3. Collect and set concentrations (only if at least one is provided and valid)
      concs_vals <- sapply(selected, function(pk_idx) {
        val <- input[[paste0("modal_conc_", pk_idx)]]
        if(is.null(val) || is.na(val)) NA else val
      })
      if(any(!is.na(concs_vals))) {
        # Replace NA with empty for display
        concs_text <- paste(ifelse(is.na(concs_vals), "", concs_vals), collapse = ",")
        updateTextInput(session, "pconcs", value = concs_text)
      }

      removeModal()
      showNotification(i18n_r()$t("Peak selection with names and concentrations applied successfully"), type = "message")
    })

    # Disable Visual Select button if no peaks detected
    observe({
      has_peaks <- !is.null(sync$peak_detection$outputs$results$Peak_Extents) &&
        nrow(sync$peak_detection$outputs$results$Peak_Extents) > 0

      if(has_peaks) {
        shinyjs::enable("visual_select_peaks")
      } else {
        shinyjs::disable("visual_select_peaks")
      }
    })

    # Disable report output checkboxes if respective results are not available
    observe({
      metlst <- c(metrics = "EP", metrics = "AH", metrics = "S5", metrics = "EMG",
                  metrics = "linvel", metrics = "porosity", metrics = "flowres", metrics = "pabil", metrics = "spabil",
                  symmetry = "As", symmetry = "Tf", symmetry = "tpa")

      for(curind in seq_along(metlst)) {
        curname <- names(metlst)[curind]
        curmet <- metlst[curind]
        catcond <- any(c("linvel", "porosity", "flowres", "pabil", "spabil", "tpa") %in% curmet)
        curcat <- if(catcond) "add" else c("metric", "spec")
        cond1 <- is.null(sync[[curname]][[paste0("is_",curmet)]])
        cond2 <- is.null(sync[[curname]][["is_all"]])
        fin_suffix <- if(catcond & curmet != "tpa") "addmets" else curmet
        if(cond1 & cond2) {
          shinyjs::disable(paste0(curcat[1],"_",fin_suffix))
          if(!catcond) shinyjs::disable(paste0(curcat[2],"_",fin_suffix))
        } else {
          shinyjs::enable(paste0(curcat[1],"_",fin_suffix))
          if(!catcond) shinyjs::enable(paste0(curcat[2],"_",fin_suffix))
        }
      }
    })

    # Output Settings Intro
    observeEvent(input$output_settings_intro, {
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab9_output_settings(ns, i18n, input))
      })
    })
  }) # Close moduleServer
} # Close report_output_server
