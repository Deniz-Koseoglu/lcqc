# server_modules/tab6_deconvolution_server.R
library(shinyjs)
source(system.file("shiny-app", "intro", "tab6_intro.R", package = "lcqc"))

deconvolution_server <- function(id, sync, config, i18n) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #Reactive version of i18n
      i18n_r <- reactive({i18n})

      #Help buttons render
      observe_helpers(withMathJax = TRUE)
      output$tab6_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab6_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

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
      deconv_choices <- reactive({
        setNames(c("all", "gs", "emg", "egh", "etg"),
                 i18n_r()$t(c("All Available",
                              "Simple Gaussian",
                              "Exponentially-Modified Gaussian (EMG)",
                              "Exponential-Gaussian Hybrid (EGH)",
                              "Empirically-Transformed Gaussian (ETG)")))
      })

      output$deconvolution_method <- renderUI({
        selectInput(ns("deconvolution_method"), i18n$t("Deconvolution Method"),
                    choices = deconv_choices(), selected = config$dconv$method$selected, multiple = TRUE)
      })


      doptmet_choices <- reactive({
        setNames(c("all", "Nelder-Mead", "BFGS", "L-BFGS-B", "SANN"),
                 i18n_r()$t(c("All Available",
                              "Nelder-Mead",
                              "Broyden-Fletcher-Goldfarb-Shanno (BFGS)",
                              "Box-Constrained BFGS",
                              "Simulated Annealing")))
      })

      output$deconvolution_optmet <- renderUI({
        selectizeInput(ns("deconvolution_optmet"), i18n$t("Optimization Method"),
                       choices = doptmet_choices(),
                       selected = config$dconv$optmet$selected,
                       multiple = TRUE
        )
      })

      #Prevent Lazy-Loading (needed for saved settings to load correctly)
      outputOptions(output, "tab6_HELP", suspendWhenHidden = FALSE)
      outputOptions(output, "deconvolution_method", suspendWhenHidden = FALSE)
      outputOptions(output, "deconvolution_optmet", suspendWhenHidden = FALSE)

      #TURN OFF OUTPUT SUSPENSION (Avoids errors for objects Shiny sees as HIDDEN)
      outputOptions(output, "deconvolution_optmet", suspendWhenHidden = FALSE)

      process_deconvolution <- function(data, method, crit_w, modres, optmet, reprs_emg) {
        if (is.null(data)) {
          showNotification(i18n_r()$t("No integration data available. Please perform integration first."), type = "warning")
          return(NULL)
        }
        reprs_param <- c()
        if ("emg" %in% method && !is.null(reprs_emg) && nchar(reprs_emg) > 0) {
          reprs_param <- c(emg = reprs_emg)
        } else if ("all" %in% method && !is.null(reprs_emg) && nchar(reprs_emg) > 0) {
          reprs_param <- c(emg = reprs_emg)
        }

        # Use tryCatch to handle potential errors during the deconvolution process.

        processing_results <- tryCatch(
          {
            crit_w_val <- if (input$manual_width_decon == "auto" | isTRUE(input$auto_crit_width_decon)) "auto" else as.numeric(input$manual_width_decon)

            chrom_icf(
              input = data,
              method = method,
              crit_w = crit_w_val,
              optmet = optmet,
              reprs = reprs_param, # Pass the constructed reprs_param
              modres = modres,
              plotset = "make", # Always make the plot for display

            )
          },
          error = function(e) {
            error_msg <- paste("Error during deconvolution:", e$message)
            showNotification(error_msg, type = "error", duration = NULL) # Show persistent error
            NULL
          },
          warning = function(w) {
            invokeRestart("muffleWarning") # Stops the warning from propagating further
          }
        )

        processing_results
      }

      # Function for styling progress messages
      incProgress_styling <- function(stage) {
        stage
      }

      # Event reactive to trigger deconvolution when apply_deconvolution is clicked
      deconvoluted_data <- eventReactive(input$apply_deconvolution, {
        input_data_for_deconvolution <- sync$peak_detection$outputs

        withProgress(message = incProgress_styling(i18n_r()$t("Applying Deconvolution")), value = 0, {
          incProgress(0.2, detail = incProgress_styling(i18n_r()$t("Preparing data")))

          method <- input$deconvolution_method
          crit_w <- if (input$manual_width_decon == "auto" | isTRUE(input$auto_crit_width_decon)) "auto" else as.numeric(input$manual_width_decon)
          modres <- input$deconvolution_modres
          optmet <- input$deconvolution_optmet
          reprs_emg <- input$deconvolution_reprs_emg


          incProgress(0.4, detail = incProgress_styling(i18n_r()$t("Performing deconvolution")))
          result <- process_deconvolution(
            data = input_data_for_deconvolution,
            method = method,
            crit_w = crit_w,
            modres = modres,
            optmet = optmet,
            reprs_emg = reprs_emg

          )
          incProgress(0.8, detail = incProgress_styling(i18n_r()$t("Generating outputs")))
          Sys.sleep(1)

          if (!is.null(result)) {
            # Mark deconvolution as completed in sync
            sync$deconvolution$completed <- TRUE
            showNotification(i18n_r()$t("Deconvolution completed successfully."), type = "message")
          } else {
            sync$deconvolution$completed <- FALSE # Mark as failed
            # Error notification already handled by process_deconvolution
          }

          result
        })
      })

      # Store the deconvoluted data in the sync object
      observe({
        # Store the full results object, which should contain plot, table data, and info
        sync$deconvolution$outputs <- deconvoluted_data()
      })




      output$deconvolution_plot1 <- renderPlotly({
        req(sync$deconvolution$outputs) # Require outputs from deconvolution
        results <- sync$deconvolution$outputs
        # Check if 'modplot' exists and contains at least one plot
        if ("modplot" %in% names(results) && length(results$modplot) > 0) {
          plot_object <- results$modplot[[1]] # Take the first plot
          if (inherits(plot_object, "ggplot")) {
            p <- ggplotly(plot_object)

            # Translation mapping for deconvolution model types
            legend_translations <- list(
              "none" = i18n_r()$t("None"),
              "gs" = i18n_r()$t("Simple Gaussian"),
              "emg" = i18n_r()$t("Exponentially-Modified Gaussian (EMG)"),
              "egh" = i18n_r()$t("Exponential-Gaussian Hybrid (EGH)"),
              "etg" = i18n_r()$t("Empirically-Transformed Gaussian (ETG)")
            )

            # Clean and translate trace names
            for (i in seq_along(p$x$data)) {
              if (!is.null(p$x$data[[i]]$name)) {
                # Transform "(model,1)" to "model"
                cleaned_name <- sub("^\\(\"?([^\",]+)\"?,\\s*\\d+\\)$", "\\1", p$x$data[[i]]$name)
                cleaned_name <- gsub("\"", "", cleaned_name)

                # Apply translation if available
                if (cleaned_name %in% names(legend_translations)) {
                  p$x$data[[i]]$name <- legend_translations[[cleaned_name]]
                } else {
                  p$x$data[[i]]$name <- cleaned_name
                }
              }
            }

            p %>%
              layout(
                title = list(text = i18n_r()$t("Chromatogram with integration baselines")),
                xaxis = list(title = i18n_r()$t("Time (min)")),
                yaxis = list(title = i18n_r()$t("Signal (pre-processed)")),
                legend = list(
                  x = 1,
                  y = 1,
                  xanchor = "right",
                  yanchor = "top",
                  title = list(text = "")
                )
              )
          } else {
            print(i18n_r()$t("Warning: Expected ggplot object for plot 1, but 'modplot' element is not a ggplot object."))
            NULL # Return NULL if no valid plot object
          }
        } else {
          # If 'modplot' not found or empty, return NULL.
          NULL
        }
      })


      output$deconvolution_plot2 <- renderPlotly({
        req(sync$deconvolution$outputs) # Require outputs from deconvolution
        results <- sync$deconvolution$outputs

        # Check if 'modplot' exists and contains at least two plots
        if ("modplot" %in% names(results) && length(results$modplot) > 1) {
          plot_object <- results$modplot[[2]] # Take the second plot
          if (inherits(plot_object, "ggplot")) {
            #ggplotly()
                p <- ggplotly(plot_object)
                for (i in seq_along(p$x$data)) {
                  if (!is.null(p$x$data[[i]]$name)) {
                    # Transform "(model,1)" to "model"
                    cleaned_name <- sub("^\\(\"?([^\",]+)\"?,\\s*\\d+\\)$", "\\1", p$x$data[[i]]$name)
                    # Remove any lingering quotes, if present
                    p$x$data[[i]]$name <- gsub("\"", "", cleaned_name)
                  }
                }
                p %>%
                    layout(title = list(text = i18n_r()$t("Chromatogram with integration baselines")),
                           xaxis = list(title = i18n_r()$t("Time (min)")),
                           yaxis = list(title = i18n_r()$t("Signal (pre-processed)")),
                        legend = list(
                            x = 0, # X position (0: left, 1: right)
                            y = 1, # Y position (0: bottom, 1: top)
                            xanchor = "right", # X-axis reference (right edge of legend aligns with x=1)
                            yanchor = "top" # Y-axis reference (top edge of legend aligns with y=1)
                        )
                    )
          } else {
            print(i18n_r()$t("Warning: Expected ggplot object for plot 2, but 'modplot' element is not a ggplot object."))
            NULL # Return NULL if no valid plot object
          }
        } else {
          # If 'modplot' doesn't have a second plot, return NULL.
          NULL
        }
      })


      # Render the deconvolution results table.
      #Create column names
      deconv_prettynames <- reactive({c(
        i18n_r()$t("Group ID"),
        i18n_r()$t("Peak ID"),
        i18n_r()$t("Retention Time (Start)"),
        i18n_r()$t("Retention Time (Apex)"),
        i18n_r()$t("Accurate Retention Time (Apex)"),
        i18n_r()$t("Retention Time (End)"),
        i18n_r()$t("Model Type"),
        i18n_r()$t("RMSE (Error)"),
        i18n_r()$t("Peak Area")
      )
      })

      #Render table
      output$deconvolution_results_table <- DT::renderDataTable({
        req(sync$deconvolution$outputs) # Require outputs from deconvolution

        results <- sync$deconvolution$outputs

        # Check if the results object contains the 'integ_res' element (most likely where deconvolution stores results)
        if ("result" %in% names(results)) { # Assuming chrom_icf stores main results in 'result'
          DT::datatable(results$result, extensions = "Buttons",
                        options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                       buttons = list(
                                         list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                              action = copy_button_no_popup(
                                                copy_label = i18n_r()$t("Copy"),
                                                copied_label = i18n_r()$t("Copied!")
                                              )),
                                         list(extend = "csv", filename = generate_filename_with_timestamp("deconvolution")),
                                         list(extend = "excel", filename = generate_filename_with_timestamp("deconvolution")),
                                         list(extend = "pdf", filename = generate_filename_with_timestamp("deconvolution"))
                                       )), rownames = FALSE,
                        ) # Use 'result' element
        } else if ("integ_res" %in% names(results)) {

          dt <- DT::datatable(results$integ_res, extensions = "Buttons", colnames = deconv_prettynames(),
                              options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                             buttons = list(
                                               list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                    action = copy_button_no_popup(
                                                      copy_label = i18n_r()$t("Copy"),
                                                      copied_label = i18n_r()$t("Copied!")
                                                    )),
                                               list(extend = "csv", filename = generate_filename_with_timestamp("deconvolution")),
                                               list(extend = "excel", filename = generate_filename_with_timestamp("deconvolution")),
                                               list(extend = "pdf", filename = generate_filename_with_timestamp("deconvolution"))
                                             )),
                              rownames = FALSE) # Added scrollX for wide tables
           dt %>%  formatRound(columns = c("start_rt", "apex_rt", "acc_apex_rt", "end_rt", "pa", "model_rmse"), digits = 3)
        } else {
          print(i18n_r()$t("Warning: Main results table element ('result' or 'integ_res') not found in deconvolution results."))
          DT::datatable(
            data.frame("Info" = i18n_r()$t("Main deconvolution results table not found in output.")),
            rownames = FALSE,
            options = list(dom = "t", pagin = FALSE, searching = FALSE, language = tablang()) # Simple table message
          )
        }
      })

      # Render the summary information text.
      output$deconvolution_information_text <- renderUI({
        req(sync$deconvolution$outputs) # Require outputs from deconvolution

        results <- sync$deconvolution$outputs

        # Check if the results object contains the 'information' element
        if ("information" %in% names(results)) {

          # Get selected deconvolution method names (NAMES, not values)
          selected_icf_names <- get_selected_names(input$deconvolution_method, deconv_choices())

          html_content <- translate_info(
            results$information,
            i18n_r(),
            methods = list(icf = selected_icf_names)
          )

          HTML(paste0("<h4>", i18n_r()$t("Deconvolution Summary Information"), "</h4><hr>", html_content))
        } else {
          HTML(paste0("<p>", i18n_r()$t("Deconvolution completed, but no detailed summary information was provided in the results."), "</p>"))
        }
      })

      #SELECT INPUT CONTEXTUAL "ALL" OPTION REMOVER(S)
      observe({
        if(!any(input$deconvolution_method %in% "all") & length(input$deconvolution_method) >= 1) {
          observe({
            if("all" %in% input$deconvolution_method) updateSelectizeInput(session, "deconvolution_method", selected = "all")
          })
        }
      })

      observe({
        if("all" %in% input$deconvolution_method & length(input$deconvolution_method) == 1) {
          observe({
            if(length(input$deconvolution_method) > 1) updateSelectizeInput(session, "deconvolution_method",
                                                                            selected = input$deconvolution_method[!input$deconvolution_method %in% "all"])
          })
        }
      })

      observe({
        if(!any(input$deconvolution_optmet %in% "all") & length(input$deconvolution_optmet) >= 1) {
          observe({
            if("all" %in% input$deconvolution_optmet) updateSelectizeInput(session, "deconvolution_optmet", selected = "all")
          })
        }
      })

      observe({
        if("all" %in% input$deconvolution_optmet & length(input$deconvolution_optmet) == 1) {
          observe({
            if(length(input$deconvolution_optmet) > 1) updateSelectizeInput(session, "deconvolution_optmet",
                                                                            selected = input$deconvolution_optmet[!input$deconvolution_optmet %in% "all"])
          })
        }
      })

      # Display the selected deconvolution method in the Advanced tab
      output$selected_deconvolution_method_display <- renderText({
        deconvnms <- deconv_choices()
        paste0(names(deconvnms)[deconvnms %in% input$deconvolution_method], collapse = ", ")
      })

      #Prevent Lazy-Loading (needed for saved settings to load correctly)
      outputOptions(output, "selected_deconvolution_method_display", suspendWhenHidden = FALSE)

      # Deconvolution Basic Intro
      observeEvent(input$deconvolution_basic_intro, {
        shinyjs::runjs(paste0("$('#", ns("deconvolution_options"), " a[data-value=\"tab6_panel_basic\"]').tab('show');"))
        shinyjs::delay(500, {
          introjs(session, options = intro_steps_tab6_basic(ns, i18n, input))
        })
      })

      # Deconvolution Advanced Intro
      observeEvent(input$deconvolution_advanced_intro, {
        shinyjs::runjs(paste0("$('#", ns("deconvolution_options"), " a[data-value=\"tab6_panel_advanced\"]').tab('show');"))
        shinyjs::delay(500, {
          introjs(session, options = intro_steps_tab6_advanced(ns, i18n))
        })
      })
    }
  )
}
