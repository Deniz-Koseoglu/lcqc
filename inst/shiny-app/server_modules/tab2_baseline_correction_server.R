# server_modules/tab2_baseline_correction_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab2_intro.R", package = "lcqc"))

baseline_correction_server <- function(id, sync, config, i18n) { #config
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab2_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab2_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

    #Update Dropdown lists
    bline_choices <- reactive({
      setNames(c("None", "als", "chang", "isrea", "poly"),
               i18n_r()$t(c("None",
                            "Iterative Asymmetric Least Squares (ALS)",
                            "Chang's method",
                            "Iterative Smoothing-Splines with Root Error Adjustment (ISREA)",
                            "Modified Polynomial Fit (ModPolyFit)")))
    })

    output$baseline_method <- renderUI({
      selectInput(ns("baseline_method"), i18n_r()$t("Baseline Correction Method"),
                  choices = bline_choices(), selected = config$bline$method$selected)
    })

    chang_choices <- reactive({
      setNames(c("linear", "cubic"), i18n_r()$t(c("Linear", "Cubic")))
    })

    output$chang_fit <- renderUI({
      selectInput(ns("chang_fit"),
                  i18n$t("Fit Method"),
                  choices = chang_choices(), selected = config$bline$chang$fit)
    })


    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab2_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "baseline_method", suspendWhenHidden = FALSE)
    outputOptions(output, "chang_fit", suspendWhenHidden = FALSE)

    process_baseline_correction <- function(data, method, params) {
      if (is.null(data)) {
        return(NULL)
      }

      intensity_values <- data[[2]]

      if (method == "None") {
        corrected_data <- data.frame(
          Time = data[[1]],
          Original_Signal = intensity_values,
          Corrected_Signal = intensity_values,
          Baseline = rep(0, length(intensity_values)) # Baseline is zero
        )
        return(corrected_data)
      }


      tryCatch(
        {
          res <- switch(method,
                        "als" = bline_als(input = intensity_values, lambda = params$lambda, p = params$p, prec = params$prec, maxit = params$maxit, rm_neg = params$rm_neg),
                        "chang" = lcqc::bline_chang(input = intensity_values, threshold = params$threshold, alpha = params$alpha, bfrac = params$bfrac, segments = params$segments, sig_window = params$sig_window, fit = params$fit, rm_neg = params$rm_neg),
                        "isrea" = bline_isrea(input = intensity_values, eta = params$eta, maxit = params$maxit, rm_neg = params$rm_neg),
                        "poly" = bline_poly(input = intensity_values, deg = params$deg, prec = params$prec, maxit = params$maxit, rm_neg = params$rm_neg),
                        stop(i18n_r()$t("Invalid baseline correction method"))
          )

          corrected_data <- data.frame(
            Time = data[[1]],
            Original_Signal = res$Original_Signal,
            Corrected_Signal = res$Corrected_Signal,
            Baseline = res$Baseline
          )
          return(corrected_data)
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error in"), method, ":", e$message), type = "error")
          return(NULL)
        }
      )
    }

    incProgress_styling <- function(stage) {
      stage
    }

    baseline_data <- eventReactive(input$calculate_baseline, {
      chromatogram_data <- sync$infile$chromatogram
      if (is.null(chromatogram_data)) {
        showNotification(i18n_r()$t("No chromatogram data loaded. Please load a file first."), type = "warning")
        return(NULL)
      }

      method <- input$baseline_method
      params <- switch(method,
                       "als" = list(
                         lambda = input$als_lambda, p = input$als_p, prec = input$als_prec,
                         maxit = input$als_maxit, rm_neg = input$als_rm_neg
                       ),
                       "chang" = list(
                         threshold = input$chang_threshold, alpha = input$chang_alpha, bfrac = input$chang_bfrac,
                         segments = input$chang_segments, sig_window = input$chang_sig_window, fit = input$chang_fit,
                         rm_neg = input$chang_rm_neg
                       ),
                       "isrea" = list(eta = input$isrea_eta, maxit = input$isrea_maxit, rm_neg = input$isrea_rm_neg),
                       "poly" = list(deg = input$poly_deg, prec = input$poly_prec, maxit = input$poly_maxit, rm_neg = input$poly_rm_neg),
                       NULL
      )

      withProgress(message = incProgress_styling(i18n_r()$t("Applying Baseline Correction")), value = 0, {

        incProgress(0.2, detail = incProgress_styling(i18n_r()$t("Processing data")))
        result <- process_baseline_correction(
          data = chromatogram_data,
          method = method,
          params = params
        )
        incProgress(0.8, detail = incProgress_styling(i18n_r()$t("Creating output")))
        sync$baseline$corrected_data <- result
        Sys.sleep(1)
        result
      })
    })


    output$baseline_plot <- renderPlotly({
      req(baseline_data())
      baseline_data <- baseline_data()

      if (is.null(baseline_data)) {
        return(NULL)
      }

      if (!("Time" %in% colnames(baseline_data))) {
        return(NULL)
      }
      sync$baseline$completed <- TRUE

      original_signal <- as.numeric(baseline_data[, "Original_Signal"])
      corrected_signal <- as.numeric(baseline_data[, "Corrected_Signal"])
      baseline_signal <- as.numeric(baseline_data[, "Baseline"])


      p <- plot_ly(data = baseline_data, x = ~Time, y = ~Original_Signal, type = "scatter", mode = "lines", name = i18n_r()$t("Original Signal")) %>%
        add_trace(y = ~Corrected_Signal, name = i18n_r()$t("Corrected Signal")) %>%
        add_trace(y = ~Baseline, name = i18n_r()$t("Baseline")) %>%
        layout(
          title = i18n_r()$t("Baseline Correction"),
          xaxis = list(title = i18n_r()$t("Time (min)")),
          yaxis = list(title = i18n_r()$t("Intensity"), autorange = TRUE),
          legend = list(
            x = 1, # X position (0: left, 1: right)
            y = 1, # Y position (0: bottom, 1: top)
            xanchor = "right", # X-axis reference (right edge of legend aligns with x=1)
            yanchor = "top" # Y-axis reference (top edge of legend aligns with y=1)
          )
        )
      return(p)
    })

    #Output the value for use in UI (include the NAME of the choice in selectInput or other module, rather than the value)
    output$selected_baseline_method <- renderText({
      bline_ref <- bline_choices()
      cur_bline <- input$baseline_method
      cur_bline <- names(bline_ref)[which(bline_ref %in% cur_bline)]
      cur_bline
    })

    # Baseline Basic Intro
    observeEvent(input$baseline_basic_intro, {
      shinyjs::runjs(paste0("$('#baseline_options a[data-value=\"tab1_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab2_basic(ns, i18n))
      })
    })

    # Baseline Advanced Intro
    observeEvent(input$baseline_advanced_intro, {
      shinyjs::runjs(paste0("$('#baseline_options a[data-value=\"tab_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        # Determine which intro steps to use based on the current method selected
        intro_steps <- switch(input$baseline_method,
                              "none" = intro_steps_tab2_advanced_none(ns, i18n),
                              "als" = intro_steps_tab2_advanced_als(ns, i18n),
                              "chang" = intro_steps_tab2_advanced_chang(ns, i18n),
                              "isrea" = intro_steps_tab2_advanced_isrea(ns, i18n),
                              "poly" = intro_steps_tab2_advanced_poly(ns, i18n)
        )

        introjs(session, options = intro_steps)
      })
    })
  })
}
