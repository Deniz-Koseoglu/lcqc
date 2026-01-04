# server_modules/tab4_peak_detect_server.R
library(shinyjs)
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc"))
source(system.file("shiny-app", "intro", "tab4_intro.R", package = "lcqc"))

peak_detect_server <- function(id, sync, main_input, config, i18n) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        #Reactive version of i18n
        i18n_r <- reactive({i18n})

        #Help buttons render
        observe_helpers(withMathJax = TRUE)
        output$tab4_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab4_help_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

        #Update Dropdown lists
        amp_choices <- reactive({
          setNames(c("quant", "diff", "zscore"),
                   i18n_r()$t(c("Simple Quantiles",
                                "Relative Differences",
                                "Z-scores")))
        })

        output$amp_thres_method <- renderUI({
          selectInput(ns("amp_thres_method"), i18n$t("Amplitude Threshold Method"),
                      choices = amp_choices(),
                      selected = config$pdet$amp_thres_method$selected, multiple = TRUE)
        })

        der1_choices <- reactive({
          setNames(c("ncore", "zscore", "vaz"),
                   i18n_r()$t(c("Noise core estimator", "Z-score based", "Vaz et al. (2016) method")))
        })

        output$der_thres_method1 <- renderUI({
          selectInput(
            inputId = ns("der_thres_method1"),
            label = i18n$t("Derivative-based peak detection method"),
            choices = der1_choices(),
            selected = config$pdet$der_thres_method1$selected
          )
        })

        der2_choices <- reactive({
          setNames(c("none", "all", "iqr", "quant", "sd"),
                   i18n_r()$t(c("No filtering", "All methods", "Interquartile range",
                                "Quantile-based", "Standard deviation-based")))
        })

        output$der_thres_method2 <- renderUI({
          selectInput(
            inputId = ns("der_thres_method2"),
            label = i18n$t("Outlier detection method"),
            choices = der2_choices(),
            selected = config$pdet$der_thres_method2$selected
          )
        })

        logic_choices <- reactive({
          setNames(c("OR", "AND"),
                   i18n_r()$t(c("Or (Disjunction)","And (Conjunction)")))
        })

        output$rej_logic_pre <- renderUI({
          selectInput(
            inputId = ns("rej_logic_pre"),
            label = i18n$t("Pre-Classification Logic"),
            choices = logic_choices(),
            selected = config$pdet$rej_logic$pre$selected
          )
        })

        output$rej_logic_post <- renderUI({
          selectInput(
            inputId = ns("rej_logic_post"),
            label = i18n$t("Post-Classification Logic"),
            choices = logic_choices(),
            selected = config$pdet$rej_logic$post$selected
          )
        })

        #Prevent Lazy-Loading (needed for saved settings to load correctly)
        outputOptions(output, "tab4_HELP", suspendWhenHidden = FALSE)
        outputOptions(output, "amp_thres_method", suspendWhenHidden = FALSE)
        outputOptions(output, "der_thres_method1", suspendWhenHidden = FALSE)
        outputOptions(output, "der_thres_method2", suspendWhenHidden = FALSE)
        outputOptions(output, "rej_logic_pre", suspendWhenHidden = FALSE)
        outputOptions(output, "rej_logic_post", suspendWhenHidden = FALSE)

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

        # Reactive value to trigger peak detection
        peak_detect_trigger <- reactiveVal(0)

        peak_detection_outputs <- eventReactive(peak_detect_trigger(), {
            # Ensure trigger is not 0 on initial load if no data
            if (peak_detect_trigger() == 0 && is.null(sync$infile$chromatogram)) {
                return(NULL)
            }

            source_data <- sync$infile$chromatogram


            if (is.null(source_data)) {
                showNotification("No original chromatogram data available for peak detection. Please import data in the first tab.", type = "warning")
                return(NULL)
            }


            withProgress(message = i18n_r()$t("Performing Peak Detection..."), value = 0, {
                incProgress(0.1, detail = i18n_r()$t("Preparing parameters..."))

                params <- list() # Initialize params list here

                signal_col <- "Signal" # Always start with original signal

                get_safe_main_input <- function(key, default = NULL) {
                    if (is.null(main_input) || !key %in% names(main_input)) {
                        return(default)
                    }
                    value <- main_input[[key]]
                    if (is.null(value) || (is.atomic(value) && length(value) == 1 && is.na(value))) {
                        return(default)
                    }
                    return(value)
                }
                # browser()

                source_data <- sync$infile$chromatogram

                # source_data  <- sync$smoothing$smoothed_data[, c("Time", "Smoothed_Signal")]


                if (is.null(source_data)) {
                    showNotification(i18n_r()$t("No original chromatogram data available for peak detection. Please import data in the first tab."), type = "warning")
                    return(NULL)
                }



                # Determine amp_thres based on user selection
                amp_thres_param <- if (isTRUE(input$use_manual_amp_thres)) {
                    as.numeric(input$manual_amp_thres)
                } else {
                    input$amp_thres_method
                }

                # ampfrac parameter for chrom_detect (only applicable if not using manual amp_thres and method includes quant or diff)
                ampfrac_param <- 0.05 # Default value
                if (!isTRUE(input$use_manual_amp_thres)) {
                    if (!is.null(input$ampfrac_advanced) && (("quant" %in% input$amp_thres_method) || ("diff" %in% input$amp_thres_method))) {
                        ampfrac_param <- as.numeric(input$ampfrac_advanced) # It's already a percentage
                    } else if ("quant" %in% input$amp_thres_method) {
                        ampfrac_param <- as.numeric(input$amp_thres_pars_quant) # It's already a percentage
                    } else if ("diff" %in% input$amp_thres_method) {
                        ampfrac_param <- as.numeric(input$amp_thres_pars_diff) # It's already a percentage
                    }
                }


                # zscore_pars parameter for chrom_detect (only used when amp_thres includes "zscore" AND not using manual threshold)
                zscore_pars_param <- if (!isTRUE(input$use_manual_amp_thres) && "zscore" %in% input$amp_thres_method) {
                    lag_val <- if (is.na(input$amp_thres_pars_zscore_lag)) 30 else as.numeric(input$amp_thres_pars_zscore_lag)
                    sens_val <- if (is.na(input$amp_thres_pars_zscore_sens)) 10 else as.numeric(input$amp_thres_pars_zscore_sens)
                    thres_val <- if (is.na(input$amp_thres_pars_zscore_thres)) 5 else as.numeric(input$amp_thres_pars_zscore_thres)
                    # Note: zscore_pars must be a list of 3 elements for z_optim: lag_rng, thres_rng, c(peak_thres, noise_thres)
                    lag_fraction <- lag_val / 1000 # örnek: 10 / 1000 = 0.01

                    list(
                        seq(lag_fraction * 0.8, lag_fraction * 1.2, length.out = 3),
                        seq(thres_val * 0.8, thres_val * 1.2, length.out = 3),
                        c(0.02, 0.005)
                    )
                } else {
                    "default"
                }

                fast_chrom <- NA
                if (!is.null(input$enable_fast_chrom) && input$enable_fast_chrom) {
                    if (!is.na(input$fast_chrom_critical_width) && input$fast_chrom_critical_width != 0) {
                        fast_chrom <- as.numeric(input$fast_chrom_critical_width)
                    }
                }



                # Baseline correction parameters
                bline_method <- get_safe_main_input("baseline-baseline_method", default = "none") # The ID from bline_ui.R
                bline_pars <- "default" # Start with default, then override if specific params are chosen

                if (bline_method != "none") {
                    if (bline_method == "als") {
                        bline_pars <- c(
                            lambda = get_safe_main_input("baseline-als_lambda"),
                            p = get_safe_main_input("baseline-als_p"),
                            prec = get_safe_main_input("baseline-als_prec"),
                            maxit = get_safe_main_input("baseline-als_maxit")
                        )
                    } else if (bline_method == "chang") {
                        bline_pars <- c(
                            threshold = get_safe_main_input("baseline-chang_threshold"),
                            alpha = get_safe_main_input("baseline-chang_alpha"),
                            bfrac = get_safe_main_input("baseline-chang_bfrac"),
                            segments = get_safe_main_input("baseline-chang_segments"),
                            sig_window = get_safe_main_input("baseline-chang_sig_window"),
                            fit= get_safe_main_input("baseline-chang_fit")
                        )
                        attr(bline_pars, "fit") <- get_safe_main_input("baseline-chang_fit")
                    } else if (bline_method == "poly") {
                        bline_pars <- c(
                            deg = get_safe_main_input("baseline-poly_deg"),
                            prec = get_safe_main_input("baseline-poly_prec"),
                            maxit = get_safe_main_input("baseline-poly_maxit")
                        )
                    } else if (bline_method == "isrea") {
                        bline_pars <- c(
                            eta = get_safe_main_input("baseline-isrea_eta", default = 4), # main_input[['baseline-isrea_eta']],
                            maxit = get_safe_main_input("baseline-isrea_maxit")
                        )
                    }
                }

                # Smoothing parameters from Tab 3
                smoothing_params <- sync$smoothing$params

                smoothing_temp <- list()
                if (smoothing_params$signal_method != "None") {
                    smoothing_temp$smooth <- c(smoothing_params$signal_method, smoothing_params$deriv_method)

                    if (smoothing_params$is_auto) {
                        smoothing_temp$ma_pts <- c("auto", smoothing_params$ma_pts)
                        smoothing_temp$ma_passes <- c("auto", smoothing_params$ma_passes)
                    } else {
                        smoothing_temp$ma_pts <- c(smoothing_params$ma_pts, smoothing_params$ma_pts)
                        smoothing_temp$ma_passes <- c(smoothing_params$ma_passes, smoothing_params$ma_passes)
                    }
                } else {
                    smoothing_temp$smooth <- rep("none", 2)
                    smoothing_temp$ma_pts <- c(3, 3)
                    smoothing_temp$ma_passes <- c(3, 3)
                }


                wd_value <- if (is.na(input$rej_wd)) NA else if (input$rej_wd == "auto") "auto" else input$rej_wd

                rej_param_list <- list()
                if (!is.na(wd_value)) rej_param_list$wd <- wd_value
                if (!is.na(input$rej_pa)) rej_param_list$pa <- input$rej_pa
                if (!is.na(input$rej_sn)) rej_param_list$sn <- input$rej_sn
                if (!is.na(input$rej_ht)) rej_param_list$ht <- input$rej_ht

                rej_param <- c(
                    wd = if (!is.na(input$rej_wd) && input$rej_wd != "") input$rej_wd else "auto",
                    pa = if (!is.na(input$rej_pa) && input$rej_pa != "") as.numeric(input$rej_pa) else NA,
                    sn = if (!is.na(input$rej_sn) && input$rej_sn != "") as.numeric(input$rej_sn) else NA,
                    ht = if (!is.na(input$rej_ht) && input$rej_ht != "") as.numeric(input$rej_ht) else NA
                )
                if (is.na(rej_param["wd"]) || rej_param["wd"] == "") {
                    rej_param["wd"] <- "auto"
                }
                rej_param["pa"] <- as.numeric(rej_param["pa"])
                rej_param["sn"] <- as.numeric(rej_param["sn"])
                rej_param["ht"] <- as.numeric(rej_param["ht"])



                params <- list(
                    chrom = source_data,
                    vars = c(names(source_data)[1], names(source_data)[2]),
                    sens = c(input$sens_fd, input$sens_sd, if (!isTRUE(input$use_manual_amp_thres) && "zscore" %in% input$amp_thres_method) input$sens_amp else 3),
                    amp_thres = amp_thres_param,
                    ampfrac = ampfrac_param,
                    det_bunch = input$det_bunch,
                    der_thres = c(input$der_thres_method1, input$der_thres_method2),
                    apex_pars = c(input$apex_pars_liftoff / 100, input$apex_pars_touchdown / 100),
                    rej = rej_param,
                    mpts = c(input$mpts_signal, input$mpts_deriv),
                    crosspts = c(input$crosspts_signal, input$crosspts_deriv),
                    fchrom = fast_chrom,
                    trange = range(source_data[, 1]),
                    pnms = NA,
                    asprat = 0.71,
                    rej_logic = c(input$rej_logic_pre, input$rej_logic_post),
                    zscore_pars = zscore_pars_param,
                    plot_corr = TRUE,
                    plotset = "make"
                )

                # Smoothing params'ı sadece "None" değilse ekle
                if (smoothing_params$signal_method != "None") {
                    params$smooth <- smoothing_temp$smooth
                    params$ma_pts <- smoothing_temp$ma_pts
                    params$ma_passes <- smoothing_temp$ma_passes
                }

                if (bline_method != "None") {
                    params$bline_method <- bline_method
                    params$bline_pars <- bline_pars
                }





                incProgress(0.3, detail = i18n_r()$t("Running chrom_detect..."))


                tryCatch(
                    {
                        # browser()
                        #print(params)

                        all_outputs <- chrom_detect_call(params)

                        incProgress(0.9, detail = i18n_r()$t("Processing results..."))


                        # Check if the output structure is as expected (list with 'results' and 'plots')
                        if (!is.null(all_outputs) &&
                            is.list(all_outputs) &&
                            "results" %in% names(all_outputs) &&
                            "plots" %in% names(all_outputs) &&
                            is.list(all_outputs$results) &&
                            is.list(all_outputs$plots)) {
                            incProgress(1, detail = "Done.")
                            sync$peak_detection$completed <- TRUE
                            all_outputs # Return the entire list of outputs
                        } else {
                            showNotification(i18n_r()$t("Peak detection returned unexpected results structure."), type = "error")
                            incProgress(1, detail = i18n_r()$t("Error."))
                            return(NULL)
                        }
                    },
                    error = function(e) {
                        showNotification(paste("Error in peak detection:", e$message), type = "error")
                        cat(i18n_r()$t("Error details:\n"))
                        cat(i18n_r()$t("Message:"), e$message, "\n")
                        cat(i18n_r()$t("Call:"), deparse(e$call), "\n")
                        traceback()
                        incProgress(1, detail = i18n_r()$t("Error."))
                        return(NULL)
                    }
                )
            }) # End of withProgress
        })

        # Observe event to store results in sync
        observe({
            sync$peak_detection$outputs <- peak_detection_outputs()
        })

        # Trigger peak detection when apply button is clicked
        observeEvent(input$apply_peak_detection, {
            peak_detect_trigger(peak_detect_trigger() + 1)
        })

        # Update slider input range based on original data
        observeEvent(sync$infile$chromatogram, {
            data <- sync$infile$chromatogram
            req(data)
        })


        # Render Plots - Use the 'Chromatogram' element from the results for plotting
        output$peak_plot <- renderPlotly({
          peak_outputs <- sync$peak_detection$outputs
          if (!is.null(peak_outputs) && !is.null(peak_outputs$plots) && !is.null(peak_outputs$plots$Chromatogram)) {
            p <- ggplotly(peak_outputs$plots$Chromatogram)

            # Translation mapping for peak markers
            legend_translations <- list(
              "Apex" = i18n_r()$t("Apex"),
              "Inflection" = i18n_r()$t("Inflection"),
              "Upslope" = i18n_r()$t("Upslope"),
              "Boundary" = i18n_r()$t("Boundary")
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
                title = list(text = i18n_r()$t("Peak detection chromatogram (with peak markers)")),
                xaxis = list(title = i18n_r()$t("Time (min)")),
                yaxis = list(title = i18n_r()$t("Signal")),
                legend = list(
                  x = 1,
                  y = 1,
                  xanchor = "right",
                  yanchor = "top",
                  title = list(text = "")
                )
              )
          } else {
            req(sync$infile$chromatogram)
            plot_dat <- sync$infile$chromatogram
            time_col <- names(plot_dat)[1]
            signal_col <- names(plot_dat)[2]
            if (is.null(time_col) || is.null(signal_col)) {
              showNotification(i18n_r()$t("Could not identify Time or Signal column for fallback plot."), type = "error")
              return(NULL)
            }

            plot_ly(data = plot_dat, x = ~ get(time_col), y = ~ get(signal_col), type = "scatter", mode = "lines", name = i18n_r()$t("Original Signal")) %>%
              layout(
                title = list(text = i18n_r()$t("Chromatogram (No Peak Detection Results Yet)")),
                xaxis = list(title = i18n_r()$t("Time (min)")),
                yaxis = list(title = i18n_r()$t("Signal (pre-processed)"), autorange = TRUE),
                legend = list(
                  x = 1,
                  y = 1,
                  xanchor = "right",
                  yanchor = "top",
                  title = list(text = "")
                )
              )
          }
        })

        #FD Noise Plot
        output$fd_noise_plot <- renderPlotly({
          peak_outputs <- sync$peak_detection$outputs
          if (!is.null(peak_outputs) && !is.null(peak_outputs$plots) && !is.null(peak_outputs$plots$FD_noise)) {
            p <- peak_outputs$plots$FD_noise
            ggplotly(p) %>%
              layout(
                title = list(text = i18n_r()$t("First Derivative Peak Detection Threshold")),
                xaxis = list(title = i18n_r()$t("Index")),
                yaxis = list(title = i18n_r()$t("First Derivative (Noise)")),
                legend = list(title = list(text = ""))
              )
          } else {
            # Create an empty plot with specific title and axis labels
            plot_ly() %>%
              add_trace(type = "scatter", mode = "markers") %>%
              layout(
                title = list(text = i18n_r()$t("First Derivative Peak Detection Threshold")),
                xaxis = list(title = i18n_r()$t("Index")),
                yaxis = list(title = i18n_r()$t("First Derivative (Noise)"))
              )
          }
        })

        #SD Noise Plot
        output$sd_noise_plot <- renderPlotly({
          peak_outputs <- sync$peak_detection$outputs
          if (!is.null(peak_outputs) && !is.null(peak_outputs$plots) && !is.null(peak_outputs$plots$SD_noise)) {
            p <- peak_outputs$plots$SD_noise
            ggplotly(p) %>%
              layout(
                title = list(text = i18n_r()$t("Second Derivative Peak Detection Threshold")),
                xaxis = list(title = i18n_r()$t("Index")),
                yaxis = list(title = i18n_r()$t("Second Derivative (Noise)")),
                legend = list(title = list(text = ""))
              )
          } else {
            # Create an empty plot with specific title and axis labels
            plot_ly() %>%
              add_trace(type = "scatter", mode = "markers") %>%
              layout(
                title = list(text = i18n_r()$t("Second Derivative Peak Detection Threshold")),
                xaxis = list(title = i18n_r()$t("Index")),
                yaxis = list(title = i18n_r()$t("Second Derivative (Noise)"))
              )
          }
        })


        # Table for Chromatogram data (can be large)
        output$chrom_table <- renderDT({
            req(sync$peak_detection$outputs$results$Chromatogram)
            datatable(sync$peak_detection$outputs$results$Chromatogram, options = list(pageLength = 50, language = tablang()))
        })

        # Table for Derivative Noise
        output$derivative_noise_table <- renderDT({
            req(sync$peak_detection$outputs$results$Derivative_Noise)
            datatable(sync$peak_detection$outputs$results$Derivative_Noise, options = list(pageLength = 50, language = tablang()))
        })


        # Table for Peak Extents (summary table)

        #Replacement column names
        #Pretty column names
        peak_extents_prettynames <- reactive({c(
          i18n_r()$t("Group ID"),
          i18n_r()$t("Peak ID"),
          i18n_r()$t("Inflection width"),
          i18n_r()$t("Area of peak top"),
          i18n_r()$t("Signal-to-Noise Ratio"),
          i18n_r()$t("Peak type"),
          i18n_r()$t("Left boundary type"),
          i18n_r()$t("Right boundary type"),
          i18n_r()$t("Index of derivative maximum"),
          i18n_r()$t("Index of signal maximum"),
          i18n_r()$t("Index of final maximum"),
          i18n_r()$t("Index of peak start"),
          i18n_r()$t("Index of peak end"),
          i18n_r()$t("Index of left upslope"),
          i18n_r()$t("Index of right upslope"),
          i18n_r()$t("Index of left inflection point"),
          i18n_r()$t("Index of right inflection point"),
          i18n_r()$t("RT at derivative maximum"),
          i18n_r()$t("RT at signal maximum"),
          i18n_r()$t("RT at final maximum"),
          i18n_r()$t("RT at peak start"),
          i18n_r()$t("RT at peak end"),
          i18n_r()$t("RT at left upslope"),
          i18n_r()$t("RT at right upslope"),
          i18n_r()$t("RT at left inflection point"),
          i18n_r()$t("RT at right inflection point")
        )})

        #Table rendering
        output$peak_extents_table <- renderDT({
            req(sync$peak_detection$outputs$results$Peak_Extents)

            dt <- datatable(sync$peak_detection$outputs$results$Peak_Extents,
                            extensions = "Buttons",
                options = list(dom = "Bfrtip", pageLength = 50, language = tablang(),
                               buttons = list(
                                 list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                      action = copy_button_no_popup(
                                        copy_label = i18n_r()$t("Copy"),
                                        copied_label = i18n_r()$t("Copied!")
                                      )),
                                 list(extend = "csv", filename = generate_filename_with_timestamp("peakdetect")),
                                 list(extend = "excel", filename = generate_filename_with_timestamp("peakdetect")),
                                 list(extend = "pdf", filename = generate_filename_with_timestamp("peakdetect"))
                               )),
                class = "display compact table table-bordered table-striped",
                colnames = peak_extents_prettynames(),
                rownames = FALSE,
            )

            dt %>%
                formatRound(columns = c("top_pa", "sn_ratio", "rt_dermax", "rt_sigmax", "rt_finmax",
                                        "rt_starts", "rt_ends", "rt_lups", "rt_rups", "rt_linf", "rt_rinf"), digits = 2) #grep("^rt_", colnames(), value = TRUE)
        })

        # Display peak parameters (if available)
        output$peak_info_text <- renderUI({
            outputs <- sync$peak_detection$outputs
            if (!is.null(outputs) && !is.null(outputs$results$information)) {

              # Gather all method selections - adjust input names as needed
              method_selections <- list()

              if (!is.null(input$bline_method)) {
                method_selections$baseline <- get_selected_names(input$bline_method, bline_choices())
              }
              if (!is.null(input$smooth_method)) {
                method_selections$smooth_signal <- get_selected_names(input$smooth_method, smooth_choices())
                method_selections$smooth_deriv <- get_selected_names(input$smooth_method_deriv, smooth_choices())
              }
              if (!is.null(input$amp_thres)) {
                method_selections$amp_thres <- get_selected_names(input$amp_thres, amp_choices())
              }
              if (!is.null(input$der_thres)) {
                method_selections$der_thres <- get_selected_names(input$der_thres, der_choices())
              }

              html_content <- translate_info(
                outputs$results$information,
                i18n_r(),
                methods = method_selections
              )

              HTML(paste0("<h4>", i18n_r()$t("Peak Detection Summary Information"), "</h4><hr>", html_content))
            } else {
              HTML(paste0("<p>", i18n_r()$t("No peak detection information available yet."), "</p>"))
            }
        })


        # Display Amplitude Limit
        output$amplitude_limit_text <- renderPrint({
            outputs <- sync$peak_detection$outputs
            if (!is.null(outputs) && !is.null(outputs$results$Amplitude_Limit)) {
                cat(i18n_r()$t("Amplitude Limit:"), "\n")
                cat("------------------\n")
                if (is.list(outputs$results$Amplitude_Limit)) {
                    print(unlist(outputs$results$Amplitude_Limit))
                } else {
                    print(outputs$results$Amplitude_Limit) # Display the numeric value
                }
            } else {
                cat(i18n_r()$t("No amplitude limit data available yet."))
            }
        })

        # Display Derivative Limits
        output$derivative_limits_text <- renderPrint({
            outputs <- sync$peak_detection$outputs
            if (!is.null(outputs) && !is.null(outputs$results$Derivative_Limits)) {
                cat(i18n_r()$t("Derivative Limits:"), "\n")

                cat("--------------------\n")
                print(outputs$results$Derivative_Limits)
            } else {
                cat(i18n_r()$t("No derivative limits data available yet."))
            }
        })

        # Display Z-score Limits
        output$zscore_limits_text <- renderPrint({
            outputs <- sync$peak_detection$outputs
            if (!is.null(outputs) && !is.null(outputs$results$Zscore_Limits)) {
                cat(i18n_r()$t("Z-score Limits:"), "\n")
                cat("-----------------\n")
                if (is.list(outputs$results$Zscore_Limits)) {
                    print(unlist(outputs$results$Zscore_Limits))
                } else {
                    print(outputs$results$Zscore_Limits) # Display the numeric value
                }
            } else {
                cat(i18n_r()$t("No Z-score limits data available yet."))
            }
        })




        # Peak summary - Combine relevant info from $results
        output$peak_summary <- renderUI({
            results <- sync$peak_detection$outputs$results
            if (!is.null(results)) {

                # Amplitude Limit
                amp_content <- if (!is.null(results$Amplitude_Limit)) {
                    value <- if (is.list(results$Amplitude_Limit)) {
                        unlist(results$Amplitude_Limit)
                    } else {
                        results$Amplitude_Limit
                    }
                    div(
                        h5(i18n_r()$t("Amplitude Limit")),
                        p(strong(formatC(value, format = "f", digits = 2, big.mark = ",")))
                    )
                } else {
                    NULL
                }

                # Derivative Limits (Scientific Notation)
                deriv_content <- if (!is.null(results$Derivative_Limits)) {
                    fd <- results$Derivative_Limits$FD
                    sd <- results$Derivative_Limits$SD

                    tagList(
                        h5(i18n_r()$t("Derivative Limits")),
                        h5(i18n_r()$t("First Derivative (FD):")),
                        tags$ul(
                            tags$li(paste0(i18n_r()$t("Superior:"), " ", formatC(fd["TS_FD"], format = "e", digits = 2))),
                            tags$li(paste0(i18n_r()$t("Inferior:"), " ", formatC(fd["TI_FD"], format = "e", digits = 2)))
                        ),
                        h5(i18n_r()$t("Second Derivative (SD):")),
                        tags$ul(
                            tags$li(paste0(i18n_r()$t("Superior:"), " ", formatC(sd["TS_SD"], format = "e", digits = 2))),
                            tags$li(paste0(i18n_r()$t("Inferior:"), " ", formatC(sd["TI_SD"], format = "e", digits = 2)))
                        )
                    )
                } else {
                    NULL
                }

                # Z-score Limits
                zscore_content <- if (!is.null(results$Zscore_Limits)) {
                    values <- if (is.list(results$Zscore_Limits)) {
                        unlist(results$Zscore_Limits)
                    } else {
                        results$Zscore_Limits
                    }
                    formatted_vals <- paste(formatC(values, format = "f", digits = 2), collapse = ", ")

                    div(
                        h5(i18n_r()$t("Z-score Limits")),
                        p(formatted_vals)
                    )
                } else {
                    NULL
                }

                # Combine all sections
                tagList(
                    h4(i18n_r()$t("Detection Thresholds")),
                    hr(),
                    amp_content,
                    hr(),
                    deriv_content,
                    hr(),
                    zscore_content
                )
            } else {
                p(i18n_r()$t("No peak detection results available yet."))
            }
        })

        # Reactive expression to combine the selected derivative threshold methods
        output$der_thres_text <- reactive({
          dermet_nms1 <- der1_choices()
          dermet_nms2 <- der2_choices()
          paste0(i18n_r()$t("Methods for derivative threshold:"), " ",
                 names(dermet_nms1)[dermet_nms1 %in% input$der_thres_method1],
                 ", ", names(dermet_nms2)[dermet_nms2 %in% input$der_thres_method2])
        })

        # Reactive expression for the selected amplitude threshold method
        output$amp_thres_method_text <- reactive({

          #Output the value for use in UI (include the NAME of the choice in selectInput or other module, rather than the value)
          ampnms <- amp_choices()

            if (isTRUE(input$use_manual_amp_thres)) {
                paste0(i18n_r()$t("Amplitude Threshold Method:"), " ", i18n_r()$t("Manual"), " (",
                       input$manual_amp_thres, ")")
            } else {
                paste0(i18n_r()$t("Amplitude Threshold Method:"), " ",
                       paste0(names(ampnms)[ampnms %in% input$amp_thres_method], collapse = ", "))
            }
        })

        # Reactive expressions for the amplitude threshold parameters based on the selected method
        output$amp_thres_pars_text <- reactive({
            if (isTRUE(input$use_manual_amp_thres)) {
              i18n_r()$t("Manual amplitude threshold is active.")
            } else {
                method <- input$amp_thres_method
                texts <- c()
                if ("quant" %in% method) {
                    texts <- c(texts, paste0(i18n_r()$t("Quantile Probability:"), " ", input$amp_thres_pars_quant, "%"))
                }
                if ("diff" %in% method) {
                    texts <- c(texts, paste0(i18n_r()$t("Max Difference Percentage:"), " ", input$amp_thres_pars_diff, "%"))
                }
                if ("zscore" %in% method) {
                    texts <- c(texts, paste0(
                      i18n_r()$t("Z-score Lag:"), " ", input$amp_thres_pars_zscore_lag,
                        ", ", i18n_r()$t("Threshold:"), " ", input$amp_thres_pars_zscore_thres,
                        ", ", i18n_r()$t("Sensitivity:"), " ", input$amp_thres_pars_zscore_sens
                    ))
                }
                paste(texts, collapse = "; ")
            }
        })

        # Reactive expression for the amplitude fraction parameter
        output$ampfrac_text <- reactive({
            if (isTRUE(input$use_manual_amp_thres)) {
              i18n_r()$t("Amplitude Fraction: Not applicable when using manual threshold.")
            } else if (!is.null(input$ampfrac_advanced) && (("quant" %in% input$amp_thres_method) || ("diff" %in% input$amp_thres_method))) {
                paste0(i18n_r()$t("Amplitude Fraction:"), " ", input$ampfrac_advanced, "%")
            } else if ("quant" %in% input$amp_thres_method) {
                paste0(i18n_r()$t("Amplitude Fraction (Quantile):"), " ", input$amp_thres_pars_quant, "%")
            } else if ("diff" %in% input$amp_thres_method) {
                paste0(i18n_r()$t("Amplitude Fraction (Max Difference):"), " ", input$amp_thres_pars_diff, "%")
            } else {
              i18n_r()$t("Amplitude Fraction: Not applicable or not set")
            }
        })

        # Reactive expression for the sensitivity parameter
        output$sens_text <- reactive({
            if (isTRUE(input$use_manual_amp_thres)) {
                paste0(
                  i18n_r()$t("First Derivative Sensitivity:"), " ", input$sens_fd,
                    ", ", i18n_r()$t("Second Derivative Sensitivity:"), " ", input$sens_sd
                )
            } else {
                paste0(
                  i18n_r()$t("First Derivative Sensitivity:"), " ", input$sens_fd,
                    ", ", i18n_r()$t("Second Derivative Sensitivity:"), " ", input$sens_sd,
                    if ("zscore" %in% input$amp_thres_method) paste0(", ", i18n_r()$t("Amplitude Limit Sensitivity (Z-score):"), " ", input$sens_amp) else ""
                )
            }
        })

        # Reactive expression for the peak bunching detection setting
        output$det_bunch_text <- reactive({
            paste0(i18n_r()$t("Detect and mitigate peak bunching:"), " ", input$det_bunch)
        })

        # Peak Detect Basic Intro
        observeEvent(input$peak_detect_basic_intro, {
            shinyjs::runjs(paste0("$('#peak_detect_parameters a[data-value=\"tab4_panel_basic\"]').tab('show');"))
            shinyjs::delay(500, {
                introjs(session, options = intro_steps_tab4_basic(ns, i18n, input))
            })
        })

        # Peak Detect Advanced Intro
        observeEvent(input$peak_detect_advanced_intro, {
            shinyjs::runjs(paste0("$('#peak_detect_parameters a[data-value=\"tab4_panel_advanced\"]').tab('show');"))
            shinyjs::delay(500, {
                introjs(session, options = intro_steps_tab4_advanced(ns, i18n, input))
            })
        })

        #Reset peak filters
        observeEvent(input$reset_peak_filters, {
            updateNumericInput(session, "rej_sn", value = NA)
            updateNumericInput(session, "rej_ht", value = NA)
            updateNumericInput(session, "rej_wd", value = NA)
            updateNumericInput(session, "rej_pa", value = NA)
            # Add a small delay to ensure input values are updated before triggering recalculation
            shinyjs::delay(100, {
                peak_detect_trigger(peak_detect_trigger() + 1) # Trigger recalculation
            })
        })
    })
}
