# server_modules/tab7_peak_symmetry_server.R
library(shinyjs)
source(system.file("shiny-app", "intro", "tab7_intro.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc"))

peak_symmetry_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab7_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab7_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

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
    symm_choices <- reactive({
      setNames(c("all", "Tf", "As", "tpa"),
               i18n_r()$t(c("All Available",
               "USP Tailing Factor (Tf)",
               "Asymmetry Factor (As)",
               "Total Peak Analysis (TPA)")))
    })

    output$symmetry_method <- renderUI({
      selectInput(ns("symmetry_method"), i18n$t("Modelling Method"),
                  choices = symm_choices(), selected = config$psym$method$selected, multiple = TRUE)
    })

    symmopt_choices <- reactive({
      setNames(c("nlp", "optim"),
               i18n_r()$t(c("Non-Linear Programming",
                            "Nelder-Mead")))
    })

    output$symmetry_optmet <- renderUI({
      selectInput(ns("symmetry_optmet"), i18n$t("Optimization Method"),
                  choices = symmopt_choices(),
                  selected = config$psym$optmet$selected)
    })

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab7_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "symmetry_method", suspendWhenHidden = FALSE)
    outputOptions(output, "symmetry_optmet", suspendWhenHidden = FALSE)

    #TURN OFF OUTPUT SUSPENSION (Avoids errors for objects Shiny sees as HIDDEN)
    outputOptions(output, "symmetry_optmet", suspendWhenHidden = FALSE)

    #Provide numeric peak values when peak selector input is set to "all"
    observe({
      req(sync$peak_detection$outputs)
      peaknum <- nrow(sync$peak_detection$outputs$results$Peak_Extents)
      if("all" %in% input$symmetry_peaks_manual) updateTextInput(session, "symmetry_peaks_manual", value = paste0("1:",peaknum))
    })

    # Parser Function (Secure Implementation)
    parse_peak_indices <- function(text, max_peaks) {
      if (is.null(text) || trimws(text) == "") {
        return(integer(0))
      }

      text <- tolower(gsub("\\s+", "", text)) # Remove whitespace and convert to lowercase

      if (text == "all") {
        return("all") # Return "all" string directly
      }

      parts <- unlist(strsplit(text, ","))

      indices <- integer(0)
      for (part in parts) {
        if (grepl("^\\d+:\\d+$", part)) { # Range validation
          range_parts <- as.integer(strsplit(part, ":")[[1]])
          if (length(range_parts) == 2 && !anyNA(range_parts)) {
            indices <- c(indices, seq(range_parts[1], range_parts[2]))
          } else {
            stop(i18n_r()$t("Invalid range format: "), part)
          }
        } else if (grepl("^\\d+$", part)) { # Single number
          idx <- as.integer(part)
          if (!is.na(idx)) indices <- c(indices, idx)
        } else {
          stop(i18n_r()$t("Invalid format: '"), part, i18n_r()$t("'. Use numbers and ranges (e.g., 1,2,5:7) or 'all'"))
        }
      }

      indices <- sort(unique(indices))

      # Bounds validation
      if (any(indices < 1 | indices > max_peaks)) {
        invalid <- indices[indices < 1 | indices > max_peaks]
        stop(
          i18n_r()$t("Peak indices out of range ("), paste(invalid, collapse = ", "),
          i18n_r()$t("). Valid range: 1-"), max_peaks
        )
      }

      indices
    }

    # Reactive value to store the results from chrom_asym
    symmetry_results <- reactiveVal(NULL)
    plotly_plots_list <- reactiveVal(list())


    # Logic to handle "all" selection exclusivity for Modelling Method
    observe({
      current_selection <- input$symmetry_method

      if ("all" %in% current_selection && length(current_selection) > 1) {
        # If "all" is selected along with other methods, deselect others
        updateSelectizeInput(session, "symmetry_method", selected = "all")
      }
      # } else if (!("all" %in% current_selection) && length(current_selection) == 0) {
      #   # If nothing is selected, default to "all"
      #   updateSelectizeInput(session, "symmetry_method", selected = "all")
      # }
    })


    # Event to trigger the chrom_asym calculation
    observeEvent(input$apply_symmetry, {
      req(sync$peak_detection$outputs)

      input_data <- sync$peak_detection$outputs

      # Use the current state of input$symmetry_method after exclusivity logic has run
      selected_methods <- input$symmetry_method
      # The exclusivity logic is now handled by the observe block above, so no need to re-check here.
      # Just ensure that if "all" is selected, it's passed as a single string "all" to chrom_asym
      if ("all" %in% selected_methods) {
        selected_methods <- "all"
      }

      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      max_peaks <- nrow(peak_data)

      selected_peaks <- tryCatch(
        parse_peak_indices(input$symmetry_peaks_manual, max_peaks),
        error = function(e) {
          showNotification(paste(i18n_r()$t("Peaks error:"), e$message), type = "error")
          req(FALSE)
        }
      )

      # Convert "all" to numeric indices
      if (identical(selected_peaks, "all")) {
        selected_peaks <- seq_len(max_peaks)
      }

      critical_width <- if (input$symmetry_crit_w == "auto" | isTRUE(input$auto_crit_width_symm)) "auto" else as.numeric(input$symmetry_crit_w)
      show_widths <- input$show_widths
      optimization_method <- input$symmetry_optmet



      withProgress(message = incProgress_styling(i18n_r()$t("Calculating Peak Symmetry")), value = 0, {
        incProgress(0.2, detail = incProgress_styling(i18n_r()$t("Processing data")))


        tryCatch(
          {
            print(selected_peaks)
            results <- chrom_asym(
              input = input_data,
              method = selected_methods,
              which_peaks = selected_peaks,
              crit_w = critical_width,
              show_widths = show_widths,
              optmet = optimization_method,
              plotset = "make"
            )
            incProgress(0.8, detail = incProgress_styling(i18n_r()$t("Creating output")))
            Sys.sleep(1)
            symmetry_results(results)
            sync$symmetry$completed <- TRUE
          },
          error = function(e) {
            showNotification(paste(i18n_r()$t("Error calculating peak symmetry:"), e$message), type = "error")
            symmetry_results(NULL)
            sync$symmetry$completed <- FALSE
          }
        )
      })
    })

    #Add flag for each symmetry method
    observe({
      for(symet in c("As", "Tf", "tpa", "all")) {
        if(symet %in% input$symmetry_method & sync$symmetry$completed) {
          sync$symmetry[[paste0("is_",symet)]] <- TRUE
        }
      }
    })

    # Store results in the sync object
    observe({
      sync$symmetry$outputs <- symmetry_results()
    })


    observeEvent(symmetry_results(), {
      results <- symmetry_results()
      if (!is.null(results$plots) && length(results$plots) > 0) {
        plotly_list <- lapply(results$plots, function(p) {
          # Access the data used by the layers
          plot_data <- tryCatch(
            {
              ggplot_build(p)$data[[1]] # Try to access the data from the first layer
            },
            error = function(e) {
              NULL # Return NULL if data access fails
            }
          )

          if (!is.null(plot_data)) {
            # Find the column names likely representing time and intensity
            time_col <- names(plot_data)[grep("time|Time|scan", names(plot_data), ignore.case = TRUE)][1]
            intensity_col <- names(plot_data)[grep("intensity|Intensity|absorbance", names(plot_data), ignore.case = TRUE)][1]

            if (!is.na(time_col) && !is.na(intensity_col)) {
              # Create the tooltip string using the found column names
              tooltip_text <- paste(
                paste0(time_col, ":"), plot_data[[time_col]], "<br>",
                paste0(intensity_col, ":"), plot_data[[intensity_col]]
                # Add other relevant information here if available in plot_data
              )

              # Add the tooltip aesthetic if it's not already present
              if (!("tooltip" %in% names(p$mapping))) {
                p <- p + aes(tooltip = tooltip_text)
              } else {
                # If tooltip is already mapped, update its value
                p$mapping$tooltip <- tooltip_text
              }
            } else {
              # Default tooltip or no tooltip if columns not found
              tooltip_text <- NULL
            }
          } else {
            tooltip_text <- NULL
          }



          if (!is.null(tooltip_text)) {
            ggplotly(p, tooltip = "tooltip")
          } else {
            ggplotly(p, tooltip = NULL) # Plot without specific tooltip if data missing
          }
        })
        test_plot <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
          geom_point()
        test_plotly <- ggplotly(test_plot)
        if (length(plotly_list) > 0 && all(sapply(plotly_list, inherits, "plotly"))) {
          plotly_plots_list(plotly_list)
        } else {
          plotly_plots_list(list())
        }
      } else {
        plotly_plots_list(list())
      }
    })


    output$plot_slider <- renderUI({
      total_plots <- length(plotly_plots_list())
      if (total_plots > 0) {
        sliderInput(
          session$ns("plot_index"),
          i18n_r()$t("Choose Plots:"),
          min = 1,
          max = total_plots,
          value = 1,
          step = 1,
          ticks = FALSE
        )
      } else {
        NULL
      }
    })

    #Render peaks plot
    # Direct access to the element created within renderUI
    output$asym_modelled_peaks_plot <- renderPlotly({
      plots <- plotly_plots_list()
      index <- input$plot_index

      if (is.null(index) || length(plots) == 0) {
        if (length(plots) == 0) {
        } else {
        }
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(title = i18n_r()$t("No TPA plots generated (check peak suitability)"), showlegend = FALSE)
      } else if (index > length(plots)) {
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(title = i18n_r()$t("Invalid plot index selected."), showlegend = FALSE)
      } else {
        selected_plot <- plots[[index]]

        if (!is.null(selected_plot) && inherits(selected_plot, "plotly")) {
          original_ggplot <- symmetry_results()$plots[[index]]

          # Get the original subtitle text
          original_subtitle <- if (!is.null(original_ggplot) && !is.null(original_ggplot$labels$subtitle)) {
            original_ggplot$labels$subtitle
          } else {
            ""
          }

          # Translate the subtitle dynamically
          # Extract numeric values using regex
          resid_sum <- sub(".*Absolute Residual Sum: ([0-9.]+).*", "\\1", original_subtitle)
          fronting <- sub(".*Fronting: ([0-9.]+).*", "\\1", original_subtitle)
          tailing <- sub(".*Tailing: ([0-9.]+).*", "\\1", original_subtitle)

          # Build translated subtitle with dynamic values
          translated_subtitle <- paste0(
            i18n_r()$t("Absolute Residual Sum"), ": ", resid_sum, "; ",
            i18n_r()$t("Fronting"), ": ", fronting, " %; ",
            i18n_r()$t("Tailing"), ": ", tailing, " %"
          )

          # Get and translate the title
          title_text <- if (!is.null(original_ggplot) && !is.null(original_ggplot$labels$title)) {
            gsub("Peak", i18n_r()$t("Peak"), original_ggplot$labels$title)
          } else {
            ""
          }

          # Combine title and subtitle using HTML line break
          combined_title <- paste0(
            title_text,
            "<br><span style='font-size:12px; color:gray;'>",
            translated_subtitle,
            "</span>"
          )

          plotly_plot <- selected_plot %>%
            layout(
              title = list(
                text = combined_title,
                y = 0.95
              ),
              yaxis = list(title = i18n_r()$t("Signal")),
              xaxis = list(title = i18n_r()$t("Time (min)"))
            )

          # Update the legend names and modes for each trace
          for (i in seq_along(plotly_plot$x$data)) {
            plotly_plot$x$data[[i]]$name <- i18n_r()$t(c(
              "Gaussian Model",
              "Baseline-Adjusted Peak",
              "Absolute Residuals"
            ))[i]
            plotly_plot$x$data[[i]]$mode <- "lines"
          }
          plotly_plot
        } else {
          plot_ly(type = "scatter", mode = "lines") %>%
            layout(title = i18n_r()$t("Error displaying plot."), showlegend = FALSE)
        }
      }
    })

    # Populate the Results Table tab
    #Create pretty column names
    asym_prettynames <- reactive({
      c(i18n_r()$t("Group ID"),
        i18n_r()$t("Peak ID"),
        i18n_r()$t("Width A at 5%"),
        i18n_r()$t("Width B at 5%"),
        i18n_r()$t("Width A at 10%"),
        i18n_r()$t("Width B at 10%"),
        i18n_r()$t("Width A at 85%"),
        i18n_r()$t("Width B at 85%"),
        i18n_r()$t("USP Tailing Factor"),
        i18n_r()$t("Asymmetry Factor"),
        i18n_r()$t("Full Width at 85%"),
        i18n_r()$t("Sigma (Width at 10%)"),
        i18n_r()$t("TPA Residual Sum (Total)"),
        i18n_r()$t("TPA Residual Sum (Front)"),
        i18n_r()$t("TPA Residual Sum (Back)"),
        i18n_r()$t("Percent Fronting"),
        i18n_r()$t("Percent Tailing"),
        i18n_r()$t("TPA Suitability"))
    })

    #Render table
    output$asym_results_table <- DT::renderDataTable({
      results <- symmetry_results()
      req(results)

      cols_to_exclude <- c("group", "peak", "tpa_suitability")

      # Get the names of the columns we DO want to check for data
      cols_to_check <- setdiff(names(results$results), cols_to_exclude)

      # Apply the condition ONLY to the 'cols_to_check' subset of the data frame
      rows_to_keep <- apply(results$results[, cols_to_check, drop = FALSE], 1, function(row_vals) any(!is.na(row_vals)))

      # Filter the original data frame using this logical vector
      cleaned_results <- results$results[rows_to_keep, ]
      cols_all <- c("group", "peak", "A05", "B05", "A10", "B10", "A85", "B85", "Tf", "As", "W85", "sigma", "resid_sum", "resid_front", "resid_back", "percent_fronting", "percent_tailing", "tpa_suitability")
      names(cols_all) <- asym_prettynames()
      cols_present <- cols_all[which(cols_all %in% colnames(cleaned_results))]
      new_colnames <- names(cols_present)
      # Return the cleaned data frame for DT::renderDataTable
      dt <- DT::datatable(cleaned_results, extensions = "Buttons",
                          options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                         buttons = list(
                                           list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                action = copy_button_no_popup(
                                                  copy_label = i18n_r()$t("Copy"),
                                                  copied_label = i18n_r()$t("Copied!")
                                                )),
                                           list(extend = "csv", filename = generate_filename_with_timestamp("symmetry")),
                                           list(extend = "excel", filename = generate_filename_with_timestamp("symmetry")),
                                           list(extend = "pdf", filename = generate_filename_with_timestamp("symmetry"))
                                         )),
                          colnames = new_colnames, rownames = FALSE
                          )
      dt %>% formatRound(columns = cols_present[!cols_present %in% cols_to_exclude], digits = 2)
    })

    # Populate the Summary Information tab
    output$asym_information_text <- renderUI({
      results <- symmetry_results()
      req(results)

      # Get the display names of selected methods from the selectInput
      selected_method_names <- get_selected_names(input$symmetry_method, symm_choices())

      # Pass methods to translate_info
      html_content <- translate_info(
        results$information,
        i18n_r(),
        methods = list(symmetry = selected_method_names)
      )

      HTML(paste0("<h4>", i18n_r()$t("Peak Symmetry Summary Information"), "</h4><hr>", html_content))
    })


    # Peak Symmetry Modal Observer
    observeEvent(input$symmetry_peaks_modal_btn, {
      req(sync$peak_detection$outputs)
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      if (is.null(peak_data) || nrow(peak_data) == 0) {
        showNotification(i18n_r()$t("No peaks detected. Please perform peak detection first."), type = "warning")
        return(NULL)
      }

      max_peaks <- nrow(peak_data)
      current_peaks_selection <- tryCatch(
        parse_peak_indices(input$symmetry_peaks_manual, max_peaks),
        error = function(e) integer(0)
      )

      showModal(modalDialog(
        title = i18n$t("Select Peaks for Peak Symmetry"),
        size = "l",
        fluidRow(
          column(
            7,
            h4(i18n$t("Peak Information")),
            DT::dataTableOutput(ns("modal_symmetry_peak_table"))
          ),
          column(
            5,
            h4(i18n$t("Peak Selection")),
            div(
              style = "max-height: 400px; overflow-y: scroll;",
              uiOutput(ns("modal_symmetry_peaks_checkboxes"))
            )
          )
        ),
        uiOutput(ns("modal_symmetry_validation_message")),
        footer = tagList(
          modalButton(i18n$t("Cancel")),
          actionButton(ns("modal_symmetry_apply"), i18n$t("Apply"), class = "btn-primary")
        )
      ))


      #Prepare column names for modal data table
      modal_prettynames <- reactive({
        c(i18n_r()$t("Peak"),
          i18n_r()$t("RT (min)"),
          i18n_r()$t("Area"),
          i18n_r()$t("Signal/Noise"),
          i18n_r()$t("Width"))
      })

      output$modal_symmetry_peak_table <- DT::renderDataTable({
        req(peak_data)
        modal_cols <- data.frame(
          Peak = peak_data$peak,
          RT = peak_data$rt_sigmax,
          Area = peak_data$top_pa,
          SN = peak_data$sn_ratio,
          Width = peak_data$inf_wd
        )
        DT::datatable(modal_cols, colnames = modal_prettynames(),
                      options = list(scrollY = "400px", paging = FALSE, searching = FALSE, language = tablang()),
                      rownames = FALSE) %>%
          formatRound(columns = c("RT", "Area", "SN", "Width"), digits = 2)
      })

      output$modal_symmetry_peaks_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak"), " %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_symmetry_peaks_sel"), NULL,
          choices = setNames(1:max_peaks, labels),
          selected = current_peaks_selection
        )
      })
    })

    # Live validation for Peak Symmetry modal
    observe({
      req(input$modal_symmetry_peaks_sel)
      len <- length(input$modal_symmetry_peaks_sel)

      output$modal_symmetry_validation_message <- renderUI({
        if (len == 0) {
          div(
            class = "alert alert-info",
            icon("info-circle"), i18n$t("Select at least one peak")
          )
        } else {
          div(
            class = "alert alert-success",
            icon("check-circle"),
            sprintf(i18n$t("Ready: %d peaks selected"), len)
          )
        }
      })

      valid <- len > 0
      shinyjs::toggleState("modal_symmetry_apply", condition = valid)
    })

    # Apply logic for Peak Symmetry modal
    observeEvent(input$modal_symmetry_apply, {
      new_text <- paste(input$modal_symmetry_peaks_sel, collapse = ",")
      updateTextInput(session, "symmetry_peaks_manual", value = new_text)
      removeModal()
    })

    # Disable modal button if no peaks
    observe({
      if (is.null(sync$peak_detection$outputs) || nrow(sync$peak_detection$outputs$results$Peak_Extents) == 0) {
        disable("symmetry_peaks_modal_btn")
      } else {
        enable("symmetry_peaks_modal_btn")
      }
    })

    incProgress_styling <- function(stage) {
      stage
    }

    # Symmetry Basic Intro
    observeEvent(input$symmetry_basic_intro, {
      shinyjs::runjs(paste0("$('#", ns("peak_symmetry_options"), " a[data-value=\"tab7_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab7_basic(ns, i18n, input))
      })
    })

    # Symmetry Advanced Intro
    observeEvent(input$symmetry_advanced_intro, {
      shinyjs::runjs(paste0("$('#", ns("peak_symmetry_options"), " a[data-value=\"tab7_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab7_advanced(ns, i18n))
      })
    })
  })
}
