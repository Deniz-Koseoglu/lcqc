# server_modules/tab5_integration_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab5_intro.R", package = "lcqc"))

integration_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab5_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab5_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

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
    integ_choices <- reactive({
      setNames(c("pdrop", "tskim", "exskim", "gskim"),
               i18n_r()$t(c("Perpendicular Drop",
                            "Tangent Skim",
                            "Exponential Skim",
                            "Gaussian Skim")))
    })

    output$integration_method <- renderUI({
      selectInput(ns("integration_method"), i18n$t("Integration Method"),
                  choices = integ_choices(), selected = config$integ$method$selected) #config$integ$method$selected
    })

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab5_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "integration_method", suspendWhenHidden = FALSE)

    process_integration <- function(data, method, skim, dyson, crit_w) {
      if (is.null(data)) {
        showNotification(i18n_r()$t("No peak detection data available. Please perform peak detection first."), type = "warning")
        return(NULL)
      }

      tryCatch(
        {
          integration_results <- chrom_skim(
            input = data, # prepared_data
            method = method,
            skim = skim,
            dyson = dyson,
            crit_w = crit_w,
            asprat = 0.75,
            plotset = "make" # Always make the plot for display in Shiny
          )


          return(integration_results)
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error in integration:"), e$message), type = "error")
          return(NULL)
        }
      )
    }

    # Function for styling progress messages
    incProgress_styling <- function(stage) {
      stage
    }

    integrated_data <- eventReactive(input$apply_integration, {
      detected_peaks <- sync$peak_detection$outputs

      withProgress(message = incProgress_styling(i18n_r()$t("Applying Integration")), value = 0, {
        incProgress(0.2, detail = incProgress_styling(i18n_r()$t("Preparing data")))
        crit_w_val <- if (input$manual_width_int == "auto" | isTRUE(input$auto_crit_width_int)) "auto" else as.numeric(input$manual_width_int)
        incProgress(0.4, detail = incProgress_styling(i18n_r()$t("Performing integration")))
        result <- process_integration(
          data = detected_peaks,
          method = input$integration_method,
          skim = input$integration_skim,
          dyson = input$integration_dyson,
          crit_w = crit_w_val
        )
        incProgress(0.8, detail = incProgress_styling(i18n_r()$t("Generating outputs")))
        Sys.sleep(1)
        result
      })
    })


    observe({
      sync$integration$outputs <- integrated_data()
    })


    # Render the integration plot
    output$integration_plot <- renderPlotly({
      req(integrated_data())
      if (!is.null(integrated_data()$plot)) {
        sync$integration$completed <- TRUE
        plot_object <- integrated_data()$plot
        # Marker layer'ını bul ve güncelle (sabit değerler)
        for (i in seq_along(plot_object$layers)) {
          if (inherits(plot_object$layers[[i]]$geom, "GeomPoint")) {
            # Marker boyutunu küçült ve rengini değiştir
            plot_object$layers[[i]]$aes_params$colour <- "#51cd3b"
            plot_object$layers[[i]]$aes_params$size <- 1.5
          }
        }

        p <- ggplotly(plot_object)

        # Translation mapping for integration methods
        legend_translations <- list(
          "PD" = i18n_r()$t("Perpendicular Drop"),
          "TF" = i18n_r()$t("Tangent Skim"),
          "EF" = i18n_r()$t("Exponential Skim"),
          "GF" = i18n_r()$t("Gaussian Skim"),
          "TT" = i18n_r()$t("Tangent Skim"),
          "ET" = i18n_r()$t("Exponential Skim"),
          "GT" = i18n_r()$t("Gaussian Skim")
        )

        # Update trace names for legend
        for (i in seq_along(p$x$data)) {
          trace_name <- p$x$data[[i]]$name
          if (!is.null(trace_name) && trace_name %in% names(legend_translations)) {
            p$x$data[[i]]$name <- legend_translations[[trace_name]]
          }
        }

        p %>%
          layout(
            title = list(text = i18n_r()$t("Chromatogram with integration baselines")),
            xaxis = list(title = i18n_r()$t("Time (min)")),
            yaxis = list(title = i18n_r()$t("Signal (pre-processed)")),
            legend = list(x = 1, y = 1, xanchor = "right", yanchor = "top", title = list(text = ""))
          )
      } else {
        showNotification(i18n_r()$t("Integration plot could not be generated."), type = "warning")
        return(NULL)
      }
    })


    # Render the results table
    #Table colnames
    integ_prettynames <- reactive({c(
      i18n_r()$t("Group ID"),
      i18n_r()$t("Peak ID"),
      i18n_r()$t("Left Boundary Retention Time"),
      i18n_r()$t("Initial Peak Apex Retention Time"),
      i18n_r()$t("Accurate Peak Apex Retention Time"),
      i18n_r()$t("Right Boundary Retention Time"),
      i18n_r()$t("Left Boundary Integration Type"),
      i18n_r()$t("Right Boundary Integration Type"),
      i18n_r()$t("Peak Integration Type"),
      i18n_r()$t("Peak Area")
    )
    })

    #Render table
      output$integration_results_table <- DT::renderDataTable({
      req(integrated_data())
      if (!is.null(integrated_data()$integ_res)) {
        tabdata <- integrated_data()$integ_res
        tabdata <- tabdata[,!colnames(tabdata) %in% c("true_conds", "failed_conds")]
        dt <- DT::datatable(tabdata, extensions = "Buttons", rownames = FALSE, colnames = integ_prettynames(),
                            options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                           buttons = list(
                                             list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                  action = copy_button_no_popup(
                                                    copy_label = i18n_r()$t("Copy"),
                                                    copied_label = i18n_r()$t("Copied!")
                                                  )),
                                             list(extend = "csv", filename = generate_filename_with_timestamp("integration")),
                                             list(extend = "excel", filename = generate_filename_with_timestamp("integration")),
                                             list(extend = "pdf", filename = generate_filename_with_timestamp("integration"))
                                           )))
        dt %>%
          formatRound(columns = c("start_rt", "end_rt", "apex_rt", "acc_apex_rt", "pa"), digits = 3)
      } else {
        return(NULL)
      }
    })

    # Render the summary information text
    output$integration_information_text <- renderUI({
      req(integrated_data())
      if (!is.null(integrated_data()$information)) {

        # Get selected skim method name
        selected_skim_names <- get_selected_names(input$skim_method, skim_choices())

        html_content <- translate_info(
          integrated_data()$information,
          i18n_r(),
          methods = list(skim = selected_skim_names)
        )

        HTML(paste0("<h4>", i18n_r()$t("Integration Summary Information"), "</h4><hr>", html_content))
      } else {
        HTML(paste0("<p>", i18n_r()$t("Integration was not yet run. No summary information available."), "</p>"))
      }
    })

    # Display the selected integration method in the Advanced tab
    output$selected_integration_method <- reactive({
      #Output the value for use in UI (include the NAME of the choice in selectInput or other module, rather than the value)
      integnms <- integ_choices()
      names(integnms)[integnms %in% input$integration_method]
    })

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "selected_integration_method", suspendWhenHidden = FALSE)

    # Integration Basic Intro
    observeEvent(input$integration_basic_intro, {
      shinyjs::runjs(paste0("$('#", ns("integration_options"), " a[data-value=\"tab5_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab5_basic(ns, i18n, input))
      })
    })

    # Integration Advanced Intro
    observeEvent(input$integration_advanced_intro, {
      shinyjs::runjs(paste0("$('#", ns("integration_options"), " a[data-value=\"tab5_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab5_advanced(ns, i18n))
      })
    })
  })
}
