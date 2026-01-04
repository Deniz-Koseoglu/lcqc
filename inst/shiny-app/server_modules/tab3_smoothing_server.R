# server_modules/tab3_smoothing_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab3_intro.R", package = "lcqc"))

smoothing_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab3_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab3_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

    #Update Dropdown lists
    sigsmooth_choices <- reactive({
      setNames(c("None", "rect", "tri", "sg_quad", "sg_quart"),
               i18n_r()$t(c("None",
                            "Rectangular",
                            "Triangular",
                            "Savitsky-Golay (Quadratic)",
                            "Savitsky-Golay (Quartic)")))
    })

    output$signal_smoothing_method <- renderUI({
      selectInput(ns("signal_smoothing_method"), i18n$t("Signal Smoothing Method"),
                  choices = sigsmooth_choices(), selected = config$smooth$signal_method)
    })

    dersmooth_choices <- reactive({
      setNames(c("None", "rect", "tri"), i18n_r()$t(c("None", "Rectangular", "Triangular")))
    })

    output$deriv_smoothing_method <- renderUI({
      selectInput(ns("deriv_smoothing_method"), i18n$t("Derivative Smoothing Method"),
                  choices = dersmooth_choices(), selected = config$smooth$deriv_method)
    })

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab3_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "signal_smoothing_method", suspendWhenHidden = FALSE)
    outputOptions(output, "deriv_smoothing_method", suspendWhenHidden = FALSE)

    process_smoothing <- function(data, signal_method, deriv_method, pts, passes, start_smooth_pts, start_smooth_passes, baseline_params, basic_auto = FALSE, basic_start_pts = 7, basic_start_passes = 1) {
      if (is.null(data)) {
        showNotification(i18n_r()$t("No baseline corrected data available. Please perform baseline correction first."), type = "warning")
        return(NULL)
      }

      if (signal_method == "None") {
        no_smoothing <- data.frame(
          Time = data$Time,
          Original_Signal = data$Original_Signal,
          Corrected_Signal = data$Corrected_Signal,
          Smoothed_Signal = data$Corrected_Signal
        )
        return(no_smoothing)
      }

      auto_determine_pts <- FALSE
      auto_determine_passes <- FALSE

      if (basic_auto) {
        auto_determine_pts <- TRUE
        auto_determine_passes <- TRUE
        start_smooth_pts <- basic_start_pts
        start_smooth_passes <- basic_start_passes
      }

      # Convert "None" to "none" for chrom_width compatibility
      signal_method_for_chrom_width <- if(signal_method == "None") "none" else signal_method
      deriv_method_for_chrom_width <- if(deriv_method == "None") "none" else deriv_method

      # Validate derivative smoothing method
      if (deriv_method %in% c("sg_quad", "sg_quart")) {
        showNotification(i18n_r()$t("Savitsky-Golay smoothing is not supported for derivatives! Use methods 'Triangular' or 'Rectangular' instead."), type = "error")
        return(NULL)
      }

      # Automatic determination of smoothing points and passes
      if (auto_determine_pts || auto_determine_passes) {
        tryCatch(
          {
            # Prepare baseline correction parameters for chrom_width
            bcorr_method_selected <- if (!is.null(baseline_params) && !is.null(baseline_params$method) && !is.null(baseline_params$method$selected)) {
              baseline_params$method$selected
            } else {
              "None" # Default to "None" if not found
            }
            bcorr_method <- if (bcorr_method_selected == "None") "none" else as.character(bcorr_method_selected)

            bpars <- if (bcorr_method == "none") {
              "default"
            } else {
              switch(bcorr_method,
                "als" = {
                  if (!is.null(baseline_params$als)) {
                    list(lambda = baseline_params$als$lambda, p = baseline_params$als$p, prec = baseline_params$als$prec, maxit = baseline_params$als$maxit, rm_neg = baseline_params$als$rm_neg)
                  } else {
                    NULL
                  }
                },
                "chang" = {
                  if (!is.null(baseline_params$chang)) {
                    list(threshold = baseline_params$chang$threshold, alpha = baseline_params$chang$alpha, bfrac = baseline_params$chang$bfrac, segments = baseline_params$chang$segments, sig_window = baseline_params$chang$sig_window, fit = baseline_params$chang$fit, rm_neg = baseline_params$chang$rm_neg)
                  } else {
                    NULL
                  }
                },
                "isrea" = {
                  if (!is.null(baseline_params$isrea)) {
                    list(eta = baseline_params$isrea$eta, maxit = baseline_params$isrea$maxit, rm_neg = baseline_params$isrea$rm_neg)
                  } else {
                    NULL
                  }
                },
                "poly" = {
                  if (!is.null(baseline_params$poly)) {
                    list(deg = baseline_params$poly$deg, prec = baseline_params$poly$prec, maxit = baseline_params$poly$maxit, rm_neg = baseline_params$poly$rm_neg)
                  } else {
                    NULL
                  }
                },
                NULL
              )
            }

            auto_params <- chrom_width(
              rtime = data$Time,
              sig = data$Corrected_Signal,
              start_smooth = c(start_smooth_pts, start_smooth_passes),
              bcorr = bcorr_method,
              bpars = bpars,
              ampfrac = 0.05, # Default ampfrac
              smooth_method = c(signal_method_for_chrom_width, deriv_method_for_chrom_width)
            )

            if (auto_determine_pts) pts <- as.numeric(auto_params["points"])
            if (auto_determine_passes) passes <- as.numeric(auto_params["passes"])
          },
          error = function(e) {
            showNotification(paste(i18n_r()$t("Error in automatic smoothing parameter determination:"), e$message), type = "error")
            return(NULL)
          }
        )
      }


      tryCatch(
        {
          smoothed_signal <- if (signal_method == "None") {
            data$Corrected_Signal
          } else {
            chrom_smooth(
              x = data$Corrected_Signal,
              method = signal_method,
              pts = pts,
              passes = passes
            )
          }

          data.frame(
            Time = data$Time,
            Original_Signal = data$Original_Signal,
            Corrected_Signal = data$Corrected_Signal,
            Smoothed_Signal = smoothed_signal
          )
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error in smoothing:"), e$message), type = "error")
          return(NULL)
        }
      )
    }

    incProgress_styling <- function(stage) {

      stage
    }

    smoothed_data <- eventReactive(input$apply_smoothing, {
      corrected_data <- sync$baseline$corrected_data
      baseline_params <- sync$config$bline # Access baseline parameters from sync

      withProgress(message = incProgress_styling(i18n_r()$t("Applying Smoothing")), value = 0, {

        incProgress(0.2, detail = incProgress_styling("Processing data"))
        result <- process_smoothing(
          data = corrected_data,
          signal_method = input$signal_smoothing_method,
          deriv_method = input$deriv_smoothing_method,
          pts = input$smoothing_pts,
          passes = input$smoothing_passes,
          start_smooth_pts = input$start_smooth_pts,
          start_smooth_passes = input$start_smooth_passes,
          baseline_params = baseline_params,
          basic_auto = isTruthy(input$basic_auto_smoothing),
          basic_start_pts = input$basic_start_smooth_pts %||% 7,
          basic_start_passes = input$basic_start_smooth_passes %||% 1
        )
        incProgress(0.8, detail = incProgress_styling(i18n_r()$t("Creating output")))
        Sys.sleep(1)
        result
      })
    })

    observe({
      sync$smoothing$smoothed_data <- smoothed_data()

      # YENİ: Smoothing parametrelerini de kaydet
      sync$smoothing$params <- list(
        signal_method = input$signal_smoothing_method,
        deriv_method = input$deriv_smoothing_method,
        is_auto = isTruthy(input$basic_auto_smoothing),
        ma_pts = if(isTruthy(input$basic_auto_smoothing)) {
          input$start_smooth_pts %||% 7
        } else {
          input$smoothing_pts
        },
        ma_passes = if(isTruthy(input$basic_auto_smoothing)) {
          input$start_smooth_passes %||% 1
        } else {
          input$smoothing_passes
        }
      )
    })

    output$smoothing_plot <- renderPlotly({
      req(smoothed_data())

      smoothed_data <- smoothed_data()
      sync$smoothing$completed <- TRUE

      p <- plot_ly(data = smoothed_data, x = ~Time, y = ~Original_Signal, type = "scatter", mode = "lines", name = i18n_r()$t("Original Signal")) %>%
        add_trace(y = ~Corrected_Signal, name = i18n_r()$t("Corrected Signal")) %>%
        add_trace(y = ~Smoothed_Signal, name = i18n_r()$t("Smoothed Signal")) %>%
        layout(
          title = i18n_r()$t("Smoothing"),
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

    #Output the values for use in UI (include the NAME of the choice in selectInput or other module, rather than the value)
    output$selected_signal_smoothing_method <- renderText({
      ssmooth_ref <- sigsmooth_choices()
      cur_smet <- input$signal_smoothing_method
      cur_smet <- names(ssmooth_ref)[which(ssmooth_ref %in% cur_smet)]
      cur_smet
    })

    output$selected_deriv_smoothing_method <- renderText({
      dsmooth_ref <- dersmooth_choices()
      cur_dmet <- input$deriv_smoothing_method
      cur_dmet <- names(dsmooth_ref)[which(dsmooth_ref %in% cur_dmet)]
      cur_dmet
    })

    outputOptions(output, "selected_signal_smoothing_method", suspendWhenHidden = FALSE)
    outputOptions(output, "selected_deriv_smoothing_method", suspendWhenHidden = FALSE)

    # Smoothing Basic Intro
    observeEvent(input$smoothing_basic_intro, {
      shinyjs::runjs(paste0("$('#smooting_options a[data-value=\"tab3_panel_basic\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab3_basic(ns, i18n))
      })
    })

    # Smoothing Advanced Intro
    observeEvent(input$smoothing_advanced_intro, {
      shinyjs::runjs(paste0("$('#smooting_options a[data-value=\"tab3_panel_advanced\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab3_advanced(ns, i18n, input))
      })
    })
  })
}
