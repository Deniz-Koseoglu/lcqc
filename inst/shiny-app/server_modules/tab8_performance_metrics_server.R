library(shinyjs)
source(system.file("shiny-app", "intro", "tab8_intro.R", package = "lcqc"))

# Parser Function (Secure Implementation)
parse_peak_indices <- function(text, max_peaks) {
  if (is.null(text) || trimws(text) == "") return(integer(0))

  text <- tolower(gsub("\\s+", "", text))  # Remove whitespace and convert to lowercase

  if (text == "all") {
    return("all") # Return "all" string directly
  }

  parts <- unlist(strsplit(text, ","))

  indices <- integer(0)
  for (part in parts) {
    if (grepl("^\\d+:\\d+$", part)) {  # Range validation
      range_parts <- as.integer(strsplit(part, ":")[[1]])
      if (length(range_parts) == 2 && !anyNA(range_parts)) {
        indices <- c(indices, seq(range_parts[1], range_parts[2]))
      } else {
        stop(i18n_r()$t("Invalid range format: "), part)
      }
    } else if (grepl("^\\d+$", part)) {  # Single number
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
    stop(i18n_r()$t("Peak indices out of range ("), paste(invalid, collapse=", "),
         i18n_r()$t("). Valid range: 1-"), max_peaks)
  }

  indices
}

#Main Server Function
performance_metrics_server <- function(id, sync, config, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Reactive version of i18n
    i18n_r <- reactive({i18n})

    #Help buttons render
    observe_helpers(withMathJax = TRUE)
    output$tab8_tplate_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_tplate_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })
    output$tab8_res_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_res_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })
    output$tab8_retf_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_retf_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })
    output$tab8_sepf_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_sepf_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })
    output$tab8_addmets_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_addmets_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })
    output$tab8_visc_HELP <-  renderUI({ helper(shiny_tag = "", buttonLabel = i18n_r()$t("Okay"), content = i18n_r()$t("tab8_help_visc_en"), type = "markdown", size = "l", style = "color:green; font-size:20px; vertical-align:top; margin-top: 0; margin-right: 3%;") })

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
    #Theoretical plate method
    tplate_choices <- reactive({
      setNames(c("all", "FW", "S5", "EP", "inf", "AH", "EMG"),
               i18n_r()$t(c("All",
                            "Full Width (FW)",
                            "5-Sigma (S5)",
                            "European Pharmacopoeia (EP)",
                            "Inflection Point Width (inf)",
                            "Area-Height (AH)",
                            "Exponentially-Modified Gaussian")))
    })

    output$tplate_method <- renderUI({
      selectInput(ns("tplate_method"), i18n$t("Calculation Method"),
                  choices = tplate_choices(), selected = config$perf$tplate$method$selected, multiple = TRUE)
    })

    #Impedance method
    imped_choices <- reactive({
      setNames(c("all", "indiv", "univ"),
               i18n_r()$t(c("All",
                            "Individual",
                            "Universal")))
    })

    output$tplate_imped_met <- renderUI({
      selectInput(ns("tplate_imped_met"), i18n$t("Impedance Calculation Method"),
                  choices = imped_choices(), selected = config$perf$tplate$imped_met$selected, multiple = TRUE)
    })

    #Resolution method
    res_choices <- reactive({
      setNames(c("all", "W0", "W50_1", "W50_2", "sepret"),
               i18n_r()$t(c("All",
                            "Full Width",
                            "Half-Width (variant 1)",
                            "Half-Width (variant 2)",
                            "Fundamental Resolution Equation")))
    })

    output$res_method <- renderUI({
      selectInput(ns("res_method"), i18n$t("Calculation Method"),
                  choices = res_choices(), selected = config$perf$res$method$selected, multiple = TRUE)
    })

    #Additional metrics selection
    addmet_choices <- reactive({
      setNames(c("all", "linvel", "porosity", "flowres", "pabil", "spabil"),
               i18n_r()$t(c("All",
                            "Linear Velocity",
                            "Packing Porosity",
                            "Flow Resistance",
                            "Permeability",
                            "Specific Permeability")))
    })

    output$addmets_which_mets <- renderUI({
      selectInput(ns("addmets_which_mets"), i18n$t("Metrics to Calculate"),
                  choices = addmet_choices(), selected = config$perf$addmets$which_mets$selected, multiple = TRUE)
    })

    #Viscosity solvents selection
    visc_choices <- reactive({
      setNames(c("meoh", "mecn", "h2o", "etoh", "ipa", "acet", "etac", "thf", "chcl3", "benz", "chex", "dee", "buoh"),
               i18n_r()$t(c("Methanol",
                            "Acetonitrile",
                            "Water",
                            "Ethanol",
                            "Isopropanol",
                            "Acetone",
                            "Ethyl Acetate",
                            "Tetrahydrofuran",
                            "Chloroform",
                            "Benzene",
                            "Cyclohexane",
                            "Diethyl Ether",
                            "Butanol")))
    })

    output$visc_which_solv <- renderUI({
      selectInput(ns("visc_which_solv"), i18n$t("Mobile phase components"),
                  choices = visc_choices(), selected = config$perf$visc$which_solv$selected, multiple = TRUE)

    })

    #Fraction type selection
    frac_choices <- reactive({
      setNames(c("vol", "mass", "mol"),
               i18n_r()$t(c("Volume fraction",
                            "Mass fraction",
                            "Mole fraction")))
    })

    output$visc_fractype <- renderUI({
      selectInput(ns("visc_fractype"), i18n$t("Fraction Type"),
                  choices = frac_choices(), selected = config$perf$visc$fractype$selected, multiple = FALSE)
    })

    #Dead time mode selection
    dtmode_choices <- reactive({
      setNames(c("manual", "peak"),
               i18n_r()$t(c("Manual",
                            "Based on Peak")))
    })

    output$tplate_t0_mode <- renderUI({selectInput(ns("tplate_t0_mode"), i18n$t("Dead Time (t0) Mode"), choices = dtmode_choices(), selected = config$perf$tplate$t0_mode$selected)})
    output$res_t0_mode <- renderUI({selectInput(ns("res_t0_mode"), i18n$t("Dead Time (t0) Mode"), choices = dtmode_choices(), selected = config$perf$res$t0_mode$selected)})
    output$retf_t0_mode <- renderUI({selectInput(ns("retf_t0_mode"), i18n$t("Dead Time (t0) Mode"), choices = dtmode_choices(), selected = config$perf$retf$t0_mode$selected)})
    output$sepf_t0_mode <- renderUI({selectInput(ns("sepf_t0_mode"), i18n$t("Dead Time (t0) Mode"), choices = dtmode_choices(), selected = config$perf$sepf$t0_mode$selected)})

    #Prevent Lazy-Loading (needed for saved settings to load correctly)
    outputOptions(output, "tab8_tplate_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "tab8_res_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "tab8_retf_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "tab8_sepf_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "tab8_addmets_HELP", suspendWhenHidden = FALSE)
    outputOptions(output, "tab8_visc_HELP", suspendWhenHidden = FALSE)

    outputOptions(output, "tplate_method", suspendWhenHidden = FALSE)
    outputOptions(output, "tplate_imped_met", suspendWhenHidden = FALSE)
    outputOptions(output, "res_method", suspendWhenHidden = FALSE)
    outputOptions(output, "addmets_which_mets", suspendWhenHidden = FALSE)
    outputOptions(output, "visc_which_solv", suspendWhenHidden = FALSE)
    outputOptions(output, "visc_fractype", suspendWhenHidden = FALSE)

    outputOptions(output, "tplate_t0_mode", suspendWhenHidden = FALSE)
    outputOptions(output, "res_t0_mode", suspendWhenHidden = FALSE)
    outputOptions(output, "retf_t0_mode", suspendWhenHidden = FALSE)
    outputOptions(output, "sepf_t0_mode", suspendWhenHidden = FALSE)


    #Provide numeric peak values when peak selector input is set to "all"
    observe({
      req(sync$peak_detection$outputs)
      peakdata <- sync$peak_detection$outputs$results$Peak_Extents
      peaknum <- nrow(peakdata)
      #peak_rts <- peakdata[,"rt_finmax"]
      for(i in c("retf_peaks_manual", "sepf_peaks1_manual", "sepf_peaks2_manual", "res_peaks1_manual", "res_peaks2_manual")) {
        peaks_def <- if(grepl("2",i)) paste0("2:",peaknum) else if(grepl("1",i)) paste0("1:", peaknum-1) else paste0("1:",peaknum)
        if("all" %in% input[[i]]) {
          updateTextInput(session, i, value = peaks_def)
        }
      }
    })

      # observe({
      #   req(sync$peak_detection$outputs)
      #   for(i in c("retf_peaks_manual", "sepf_peaks1_manual", "sepf_peaks2_manual", "res_peaks1_manual", "res_peaks2_manual")) {
      #     which_metric <- if(grepl("sepf|res",i) & grepl("sepf",i)) "sepf" else "res"
      #     if(length(input[[paste(which_metric, "_t0_mode")]]) > 0) {
      #       evaluated_inds <- eval(parse(text = input[[i]]))
      #       mode_chk <- input[[paste(which_metric, "_t0_mode")]]=="peak"
      #       comp_ind <- if(mode_chk) input[[paste0(which_metric, "_ks_peak_index")]] else input[[paste0(which_metric, "_ks_manual")]]
      #       cur_inds <- if(mode_chk) evaluated_inds else peak_rts[evaluated_inds]
      #       incorrect_peak_chk <- which(cur_inds <= comp_ind)
      #       if(length(incorrect_peak_chk) > 0) {
      #         new_inds <- evaluated_inds[-incorrect_peak_chk]
      #         updateTextInput(session, i, value = paste0(min(new_inds), ":", max(new_inds)))
      #       }
      #     }
      #   }
      # })

    #SELECT INPUT CONTEXTUAL "ALL" OPTION REMOVER(S)
    #Theoretical Plate method
    observe({
      if(!any(input$tplate_method %in% "all") & length(input$tplate_method) >= 1) {
        observe({
          if("all" %in% input$tplate_method) updateSelectizeInput(session, "tplate_method", selected = "all")
        })
      }
    })

    observe({
      if("all" %in% input$tplate_method & length(input$tplate_method) == 1) {
        observe({
          if(length(input$tplate_method) > 1) updateSelectizeInput(session, "tplate_method",
                                                                          selected = input$tplate_method[!input$tplate_method %in% "all"])
        })
      }
    })

    #Impedance method
    observe({
      if(!any(input$tplate_imped_met %in% "all") & length(input$tplate_imped_met) >= 1) {
        observe({
          if("all" %in% input$tplate_imped_met) updateSelectizeInput(session, "tplate_imped_met", selected = "all")
        })
      }
    })

    observe({
      if("all" %in% input$tplate_imped_met & length(input$tplate_imped_met) == 1) {
        observe({
          if(length(input$tplate_imped_met) > 1) updateSelectizeInput(session, "tplate_imped_met",
                                                                   selected = input$tplate_imped_met[!input$tplate_imped_met %in% "all"])
        })
      }
    })

    #Resolution method
    observe({
      if(!any(input$res_method %in% "all") & length(input$res_method) >= 1) {
        observe({
          if("all" %in% input$res_method) updateSelectizeInput(session, "res_method", selected = "all")
        })
      }
    })

    observe({
      if("all" %in% input$res_method & length(input$res_method) == 1) {
        observe({
          if(length(input$res_method) > 1) updateSelectizeInput(session, "res_method",
                                                                      selected = input$res_method[!input$res_method %in% "all"])
        })
      }
    })

    #Additional metrics selection
    observe({
      if(!any(input$addmets_which_mets %in% "all") & length(input$addmets_which_mets) >= 1) {
        observe({
          if("all" %in% input$addmets_which_mets) updateSelectizeInput(session, "addmets_which_mets", selected = "all")
        })
      }
    })

    observe({
      if("all" %in% input$addmets_which_mets & length(input$addmets_which_mets) == 1) {
        observe({
          if(length(input$addmets_which_mets) > 1) updateSelectizeInput(session, "addmets_which_mets",
                                                                selected = input$addmets_which_mets[!input$addmets_which_mets %in% "all"])
        })
      }
    })

    #Contextual remover for viscosity
    observe({
      notechk <- FALSE
      if(!any(input$visc_which_solv %in% "h2o") & length(input$visc_which_solv) >= 1) {

          if("h2o" %in% input$visc_which_solv) {
            if(!(length(input$visc_which_solv) == 2 && (all(c("h2o","meoh") %in% input$visc_which_solv) || all(c("h2o","mecn") %in% input$visc_which_solv)))) {
              notechk <- TRUE
              updateSelectizeInput(session, "visc_which_solv", selected = "h2o")
              #renderText("❌ Water mixtures currently supported only for methanol and acetonitrile!")
            }
          }
          if(notechk) showNotification(i18n_r()$t("Only MeOH-H2O and MeCN-H2O aqueous mixtures are presently allowed!"), type = "warning", duration = 4)

      }
    })

    observe({
      notechk <- FALSE
      if("h2o" %in% input$visc_which_solv & length(input$visc_which_solv) >= 1) {

          if(length(input$visc_which_solv) > 1 & "h2o" %in% input$visc_which_solv) {
            if(!(length(input$visc_which_solv) == 2 && (all(c("h2o","meoh") %in% input$visc_which_solv) || all(c("h2o","mecn") %in% input$visc_which_solv)))) {
              notechk <- TRUE
              updateSelectizeInput(session, "visc_which_solv", selected = "h2o")
              #renderText("❌ Water mixtures currently supported only for methanol and acetonitrile!")
            }
          }
          if(notechk) showNotification(i18n_r()$t("Only MeOH-H2O and MeCN-H2O aqueous mixtures are presently allowed!"), type = "warning", duration = 4)

      }
    })

    #Begin main processing
    rv <- reactiveValues(
      tplate_calculated = FALSE,
      addmets_calculated = FALSE
    )
    res_results <- eventReactive(input$apply_resolution, {
      req(sync$peak_detection$outputs)

      input_data <- sync$peak_detection$outputs
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      max_peaks <- nrow(peak_data)

      # Prepare peaks1 input
      res_peaks1_val <- tryCatch(
        parse_peak_indices(input$res_peaks1_manual, max_peaks),
        error = function(e) {
          showNotification(paste(i18n_r()$t("Peaks 1 error:", e$message)), type = "error")
          req(FALSE)
        }
      )

      # Prepare peaks2 input
      res_peaks2_val <- tryCatch(
        parse_peak_indices(input$res_peaks2_manual, max_peaks),
        error = function(e) {
          showNotification(paste(i18n_r()$t("Peaks 2 error:"), e$message), type = "error")
          req(FALSE)
        }
      )

      # Validate equal lengths
      if (length(res_peaks1_val) != length(res_peaks2_val)) {
        showNotification(i18n_r()$t("Peaks 1 and Peaks 2 must have equal length!"), type = "error")
        req(FALSE)
      }

      # Prepare ks input
      res_ks_val <- if (input$res_t0_mode == "manual") {
        req(input$res_ks_manual)
        c(input$res_ks_manual, "manual")
      } else {
        req(input$res_ks_peak_index)
        c(input$res_ks_peak_index, "peak") # Format as c(1, "peak") for Peak mode
      }

      # Prepare crit_w input
      res_crit_w_val <- if (input$res_crit_w == "auto") "auto" else as.numeric(input$res_crit_w)

        tryCatch(
          {

            withProgress(message = i18n_r()$t("Calculating Resolution"), value = 0, {
            incProgress(0.2, detail = i18n_r()$t("Preparing inputs"))
            incProgress(0.5, detail = i18n_r()$t("Performing calculation"))
            result <- chrom_res(
              input = input_data,
              peaks1 = res_peaks1_val,
              peaks2 = res_peaks2_val,
              method = input$res_method,
              ks = res_ks_val,
              crit_w = res_crit_w_val,
              verbose_res = FALSE
            )
            incProgress(0.8, detail = i18n_r()$t("Formatting results"))
            sync$metrics$res_outputs <- result
            result
          })
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error calculating resolution:"), e$message), type = "error")
          NULL
        }
      )
    })

    output$res_information_text <- renderUI({
      req(res_results())
      selected_res_names <- get_selected_names(input$res_method, res_choices())

      html_content <- translate_info(
        res_results()$information,
        i18n_r(),
        methods = list(resolution = selected_res_names)
      )

      HTML(paste0("<h4>", i18n_r()$t("Resolution Summary Information"), "</h4><hr>", html_content))
    })

    #Output results table for Resolution
    #Create pretty column names
    res_prettynames <- reactive({
      c(i18n_r()$t("Peak ID 1"),
        i18n_r()$t("Peak ID 2"),
        i18n_r()$t("Peak Type 1"),
        i18n_r()$t("Peak Type 2"),
        i18n_r()$t("Ret. Time 1 (min)"),
        i18n_r()$t("Ret. Time 2 (min)"),
        i18n_r()$t("Full Width (FW)"),
        i18n_r()$t("Half Width 1 (HW1)"),
        i18n_r()$t("Half Width 2 (HW2)"),
        i18n_r()$t("Fundamental Resolution (FRE)"))
    })


    output$res_results_table <- DT::renderDataTable({
      req(res_results())
      dt_data <- res_results()$results

      cols_to_round <- c("id1", "id2")
      cols_to_exclude <- c(cols_to_round, "type1", "type2")
      cols_all <- c("id1", "id2", "type1", "type2", "rt1", "rt2", "res_W0", "res_W50_1", "res_W50_2", "res_seprt")
      names(cols_all) <- res_prettynames()
      cols_present <- cols_all[which(cols_all %in% colnames(dt_data))]
      new_colnames <- names(cols_present)
      # Return the cleaned data frame for DT::renderDataTable
      dt <- DT::datatable(dt_data, extensions = "Buttons",
                          options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                         buttons = list(
                                           list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                action = copy_button_no_popup(
                                                  copy_label = i18n_r()$t("Copy"),
                                                  copied_label = i18n_r()$t("Copied!")
                                                )),
                                           list(extend = "csv", filename = generate_filename_with_timestamp("resolution")),
                                           list(extend = "excel", filename = generate_filename_with_timestamp("resolution")),
                                           list(extend = "pdf", filename = generate_filename_with_timestamp("resolution"))
                                         )),
                          colnames = new_colnames, rownames = FALSE
                          ) %>%
      formatRound(columns = cols_present[!cols_present %in% cols_to_exclude], digits = 2) %>%
      formatRound(columns = cols_to_round, digits = 0)
    })


    retf_results <- eventReactive(input$apply_retention_factor, {
      req(sync$peak_detection$outputs)

      input_data <- sync$peak_detection$outputs
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      max_peaks <- nrow(peak_data)

      # Prepare t0 input
      retf_t0_val <- if (input$retf_t0_mode == "manual") {
        req(input$retf_t0_manual)
        input$retf_t0_manual
      } else {
        req(input$retf_t0_peak_index)
        input$retf_t0_peak_index # Pass peak index directly for peak mode
      }


      # Prepare peaks input
      retf_peaks_val <- tryCatch(
        parse_peak_indices(input$retf_peaks_manual, max_peaks),
        error = function(e) {
          showNotification(paste(i18n_r()$t("Peaks error:"), e$message), type = "error")
          req(FALSE)
        }
      )

      # Prepare crit_w input
      retf_crit_w_val <- if (input$retf_crit_w == "auto") "auto" else as.numeric(input$retf_crit_w)

      tryCatch(
        {

          withProgress(message = i18n_r()$t("Calculating Retention Factors"), value = 0, {
            incProgress(0.2, detail = i18n_r()$t("Preparing inputs"))
            incProgress(0.5, detail = i18n_r()$t("Performing calculation"))
            result <- chrom_retf(
              input = input_data,
              t0_mode = input$retf_t0_mode,
              t0 = retf_t0_val,
              peaks = retf_peaks_val,
              crit_w = retf_crit_w_val
            )
            incProgress(0.8, detail = i18n_r()$t("Formatting results"))
            sync$metrics$retf_outputs <- result
            result
          })
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error calculating retention factors:"), e$message), type = "error")
          NULL
        }
      )
    })

    output$retf_information_text <- renderUI({
      req(retf_results())
      html_content <- translate_info(retf_results()$information, i18n_r())
      HTML(paste0("<h4>", i18n_r()$t("Retention Factor Summary Information"), "</h4><hr>", html_content))
    })

    #Output results table for Retention Factors
    #Create pretty column names
    retf_prettynames <- reactive({
      c(i18n_r()$t("Peak ID"),
        i18n_r()$t("Peak Type"),
        i18n_r()$t("Ret. Time (min)"),
        i18n_r()$t("Dead Time t0 (min)"),
        i18n_r()$t("Ret. Factor (k)"))
    })

    #Render table
    output$retf_results_table <- DT::renderDataTable({
      req(retf_results())
      dt_data <- retf_results()$results

      cols_to_round <- c("id")
      cols_to_exclude <- c(cols_to_round, "type")
      cols_all <- c("id", "type", "rt", "t0", "k")
      names(cols_all) <- retf_prettynames()
      cols_present <- cols_all[which(cols_all %in% colnames(dt_data))]
      new_colnames <- names(cols_present)
      # Return the cleaned data frame for DT::renderDataTable
      dt <- DT::datatable(dt_data, extensions = "Buttons",
                          options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                         buttons = list(
                                           list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                action = copy_button_no_popup(
                                                  copy_label = i18n_r()$t("Copy"),
                                                  copied_label = i18n_r()$t("Copied!")
                                                )),
                                           list(extend = "csv", filename = generate_filename_with_timestamp("retention-factors")),
                                           list(extend = "excel", filename = generate_filename_with_timestamp("retention-factors")),
                                           list(extend = "pdf", filename = generate_filename_with_timestamp("retention-factors"))
                                         )),
                          colnames = new_colnames, rownames = FALSE
                          ) %>%
        formatRound(columns = cols_present[!cols_present %in% cols_to_exclude], digits = 3) %>%
        formatRound(columns = cols_to_round, digits = 0)
    })


    sepf_results <- eventReactive(input$apply_separation_factor, {
      req(sync$peak_detection$outputs)

      input_data <- sync$peak_detection$outputs
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      max_peaks <- nrow(peak_data)

      # Prepare peaks1 input
      sepf_peaks1_val <- tryCatch(
        parse_peak_indices(input$sepf_peaks1_manual, max_peaks),
        error = function(e) {
          showNotification(paste("Peaks 1 error:", e$message), type = "error")
          req(FALSE)
        }
      )

      # Prepare peaks2 input
      sepf_peaks2_val <- tryCatch(
        parse_peak_indices(input$sepf_peaks2_manual, max_peaks),
        error = function(e) {
          showNotification(paste(i18n_r()$t("Peaks 2 error:"), e$message), type = "error")
          req(FALSE)
        }
      )

      # Validate equal lengths
      if (length(sepf_peaks1_val) != length(sepf_peaks2_val)) {
        showNotification("Peaks 1 and Peaks 2 must have equal length!", type = "error")
        req(FALSE)
      }

      # Prepare ks input
      sepf_ks_val <- if (input$sepf_t0_mode == "manual") {
        req(input$sepf_ks_manual)
        c(input$sepf_ks_manual, "manual")
      } else {
        req(input$sepf_ks_peak_index)
        c(input$sepf_ks_peak_index, "peak") # Format as c(1, peak_index) for Peak mode
      }

      # Prepare crit_w input
      sepf_crit_w_val <- if (input$sepf_crit_w == "auto") "auto" else as.numeric(input$sepf_crit_w)


      tryCatch(
        {
          withProgress(message = i18n_r()$t("Calculating Separation Factors"), value = 0, {
            incProgress(0.2, detail = i18n_r()$t("Preparing inputs"))
            incProgress(0.5, detail = i18n_r()$t("Performing calculation"))
            result <- chrom_sepf(
              input = input_data,
              peaks1 = sepf_peaks1_val,
              peaks2 = sepf_peaks2_val,
              ks = sepf_ks_val,
              crit_w = sepf_crit_w_val
            )
            incProgress(0.8, detail = "Formatting results")
            sync$metrics$sepf_outputs <- result
            result
          })
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error calculating separation factors:"), e$message), type = "error")
          NULL
        }
      )
    })

    output$sepf_information_text <- renderUI({
      req(sepf_results())
      html_content <- translate_info(sepf_results()$information, i18n_r())
      HTML(paste0("<h4>", i18n_r()$t("Separation Factor Summary Information"), "</h4><hr>", html_content))
    })

    #Output results table for Separation Factors
    #Create pretty column names
    sepf_prettynames <- reactive({
      c(i18n_r()$t("Peak ID 1"),
        i18n_r()$t("Peak ID 2"),
        i18n_r()$t("Peak Type 1"),
        i18n_r()$t("Peak Type 2"),
        i18n_r()$t("Ret. Factor k (Peak 1)"),
        i18n_r()$t("Ret. Factor k (Peak 2)"),
        i18n_r()$t("Separation Factor"))
    })


    output$sepf_results_table <- DT::renderDataTable({
      req(sepf_results())
      dt_data <- sepf_results()$results

      cols_to_round <- c("id1", "id2")
      cols_to_exclude <- c(cols_to_round, "type1", "type2")
      cols_all <- c("id1", "id2", "type1", "type2", "k1", "k2", "sep_factor")
      names(cols_all) <- sepf_prettynames()
      cols_present <- cols_all[which(cols_all %in% colnames(dt_data))]
      new_colnames <- names(cols_present)
      # Return the cleaned data frame for DT::renderDataTable
      dt <- DT::datatable(dt_data, extensions = "Buttons",
                          options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                         buttons = list(
                                           list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                                action = copy_button_no_popup(
                                                  copy_label = i18n_r()$t("Copy"),
                                                  copied_label = i18n_r()$t("Copied!")
                                                )),
                                           list(extend = "csv", filename = generate_filename_with_timestamp("separation-factors")),
                                           list(extend = "excel", filename = generate_filename_with_timestamp("separation-factors")),
                                           list(extend = "pdf", filename = generate_filename_with_timestamp("separation-factors"))
                                         )),
                          colnames = new_colnames, rownames = FALSE
                          ) %>%
        formatRound(columns = cols_present[!cols_present %in% cols_to_exclude], digits = 2) %>%
        formatRound(columns = cols_to_round, digits = 0)
    })


    observeEvent(input$apply_tplate, {
      req(sync$peak_detection$outputs)
      req(sync$integration$outputs)

      current_peak_data <- sync$peak_detection$outputs
      current_peak_areas <- sync$integration$outputs

      # Prepare which_peaks - ALWAYS "all" now that the UI input is removed
      which_peaks_arg <- "all"

      # Prepare pa argument, required if 'AH' method is selected
      pa_arg <- NA
      # Check for "all" case BEFORE which_peaks_arg is potentially converted to a numeric vector
      if ("AH" %in% input$tplate_method || "all" %in% input$tplate_method) {
        req(current_peak_areas) # pa required for AH method
        # Since which_peaks_arg is always "all", pa_arg will be the areas of all peaks
        pa_arg <- current_peak_areas$integ_res$pa
      }


      # Prepare t0 argument
      t0_arg <- NA
      if (input$tplate_t0_mode == "manual") {
        t0_arg <- input$tplate_t0_manual
      } else { # t0_mode == "peak"
        # Check if t0_peak_index is a valid peak index
        t0_peak_index <- input$tplate_t0_peak_index
        if (t0_peak_index > nrow(current_peak_data$results$Peak_Extents) || t0_peak_index < 1) {
          showNotification(i18n_r()$t("Invalid dead time peak index for Theoretical Plates."), type = "error")
          return(NULL)
        }
        # The function expects the peak index for t0 when t0_mode is "peak"
        t0_arg <- t0_peak_index
      }


      # Check for required inputs for impedance calculation if 'indiv' or 'univ' is selected
      imped_met_selected <- input$tplate_imped_met
      calculate_impedance <- "all" %in% imped_met_selected || "indiv" %in% imped_met_selected || "univ" %in% imped_met_selected

      if (calculate_impedance) {
        req(input$tplate_deltap, input$tplate_visc)
        if (is.na(input$tplate_deltap) || is.na(input$tplate_visc)) {
          showNotification(i18n_r()$t("Back pressure and mobile phase viscosity are required for Separation Impedance calculation."), type = "warning")
          return(NULL)
        }
        if (input$tplate_t0_mode == "manual") {
          req(input$tplate_t0_manual)
          if (is.na(input$tplate_t0_manual)) {
            showNotification(i18n_r()$t("Dead time (t0) is required for Separation Impedance calculation when in manual mode."), type = "warning")
            return(NULL)
          }
        } else { # t0_mode == "peak"
          req(input$tplate_t0_peak_index)
          if (is.na(input$tplate_t0_peak_index)) {
            showNotification(i18n_r()$t("Dead time (t0) peak index is required for Separation Impedance calculation when in peak mode."), type = "warning")
            return(NULL)
          }
        }
      }


      # Check for required inputs for HETP and h calculation if len or dp are provided
      calculate_h_h <- !is.na(input$tplate_len) || !is.na(input$tplate_dp)
      if (calculate_h_h) {
        req(input$tplate_len, input$tplate_dp)
        if (is.na(input$tplate_len)) {
          showNotification(i18n_r()$t("Column length is required for HETP and h calculation."), type = "warning")
          return(NULL)
        }
        if (is.na(input$tplate_dp)) {
          showNotification(i18n_r()$t("Particle diameter is required for HETP calculation."), type = "warning")
          return(NULL)
        }
      }


      tplate_results <- chrom_tplate(
        input = current_peak_data,
        method = input$tplate_method,
        pa = pa_arg,
        len = input$tplate_len,
        dp = input$tplate_dp,
        show_widths = input$tplate_show_widths,
        crit_w = ifelse(input$tplate_crit_w == "auto", "auto", as.numeric(input$tplate_crit_w)),
        which_peaks = which_peaks_arg, # which_peaks is now always "all"
        deltap = input$tplate_deltap,
        visc = input$tplate_visc,
        t0 = t0_arg,
        t0_mode = input$tplate_t0_mode,
        imped_met = input$tplate_imped_met
      )

      output$tplate_information_text <- renderUI({
        sync$metrics$tplate_outputs <- tplate_results
        rv$tplate_calculated <- TRUE

        req(tplate_results)

        selected_tplate_names <- get_selected_names(input$tplate_method, tplate_choices())

        html_content <- translate_info(
          tplate_results$information,
          i18n_r(),
          methods = list(tplate = selected_tplate_names)
        )

        HTML(paste0("<h4>", i18n_r()$t("Theoretical Plates Summary Information"), "</h4><hr>", html_content))
      })

      #Output results table for Theoretical Plates
      #Create pretty column names
      tplate_prettynames <- reactive({
        c(i18n_r()$t("Group ID"),
          i18n_r()$t("Peak ID"),
          i18n_r()$t("W (44%)"),
          i18n_r()$t("W (10%)"),
          i18n_r()$t("W (13%)"),
          i18n_r()$t("W (50%)"),
          i18n_r()$t("W (60%)"),
          i18n_r()$t("A (44%)"),
          i18n_r()$t("B (44%)"),
          i18n_r()$t("A (10%)"),
          i18n_r()$t("B (10%)"),
          i18n_r()$t("A (13%)"),
          i18n_r()$t("B (13%)"),
          i18n_r()$t("A (50%)"),
          i18n_r()$t("B (50%)"),
          i18n_r()$t("A (60%)"),
          i18n_r()$t("B (60%)"),
          i18n_r()$t("N (FW)"),
          i18n_r()$t("N (S5)"),
          i18n_r()$t("N (EP)"),
          i18n_r()$t("N (Inf.)"),
          i18n_r()$t("N (AH)"),
          i18n_r()$t("N (EMG)"),
          i18n_r()$t("HETP (FW)"),
          i18n_r()$t("HETP (S5)"),
          i18n_r()$t("HETP (EP)"),
          i18n_r()$t("HETP (Inf.)"),
          i18n_r()$t("HETP (AH)"),
          i18n_r()$t("HETP (EMG)"),
          i18n_r()$t("h (FW)"),
          i18n_r()$t("h (S5)"),
          i18n_r()$t("h (EP)"),
          i18n_r()$t("h (Inf.)"),
          i18n_r()$t("h (AH)"),
          i18n_r()$t("h (EMG)"),
          i18n_r()$t("E (FW)"),
          i18n_r()$t("E (S5)"),
          i18n_r()$t("E (EP)"),
          i18n_r()$t("E (Inf.)"),
          i18n_r()$t("E (AH)"),
          i18n_r()$t("E (EMG)"),
          i18n_r()$t("E (Universal)"))
      })

      output$tplate_results_table <- renderDataTable({
        dt_data <- tplate_results$results

        #cols_to_exclude <- c("group", "peak")
        colgroup1 <- grep("^h_*", colnames(dt_data), value = TRUE)
        colgroup2 <- grep("^h_*|^W*|^A*|^B*", colnames(dt_data), value = TRUE)
        colgroup3 <- grep("^N_*|group|peak", colnames(dt_data), value = TRUE)
        cols_all <- c("group", "peak", "W044", "W10", "W13", "W50", "W60", "A044", "B044", "A10", "B10", "A13", "B13", "A50", "B50", "A60", "B60",
                      "N_FW", "N_S5", "N_EP", "N_inf", "N_AH", "N_EMG",
                      "HETP_FW", "HETP_S5", "HETP_EP", "HETP_inf", "HETP_AH", "HETP_EMG",
                      "h_FW", "h_S5", "h_EP", "h_inf", "h_AH", "h_EMG",
                      "E_FW", "E_S5", "E_EP", "E_inf", "E_AH", "E_EMG", "E")
        names(cols_all) <- tplate_prettynames()
        cols_present <- cols_all[which(cols_all %in% colnames(dt_data))]
        new_colnames <- names(cols_present)
        all_other_cols <- colnames(dt_data)[!colnames(dt_data) %in% c(colgroup1, colgroup2, colgroup3)] #cols_to_exclude
        #cols_to_format <- setdiff(all_numeric_cols, cols_to_exclude)

        # DT::datatable'ı oluştur ve formatla
        DT::datatable(dt_data, extensions = "Buttons",
                      options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                     buttons = list(
                                       list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                            action = copy_button_no_popup(
                                              copy_label = i18n_r()$t("Copy"),
                                              copied_label = i18n_r()$t("Copied!")
                                            )),
                                       list(extend = "csv", filename = generate_filename_with_timestamp("theoretical-plates")),
                                       list(extend = "excel", filename = generate_filename_with_timestamp("theoretical-plates")),
                                       list(extend = "pdf", filename = generate_filename_with_timestamp("theoretical-plates"))
                                     )),
                      colnames = new_colnames, rownames = FALSE
                      ) %>%
          formatRound(columns = colgroup1, digits = 3) %>%
          formatRound(columns = colgroup2, digits = 2) %>%
          formatRound(columns = colgroup3, digits = 0) %>%
          formatRound(columns = all_other_cols, digits = 0)
      })
    })

    addmets_results <- eventReactive(input$apply_addmets, {
      req(sync$peak_detection$outputs)

      input_data <- sync$peak_detection$outputs

      # Validate required inputs based on selected metrics
      selected_metrics <- input$addmets_which_mets

      # Check for inputs required by any selected metric (excluding 'all')
      calculate_any <- "all" %in% selected_metrics || any(selected_metrics %in% c("linvel", "porosity", "flowres", "pabil", "spabil"))

      if (calculate_any) {
        req(input$addmets_t0, input$addmets_len)
        if (is.na(input$addmets_t0) || is.na(input$addmets_len)) {
          showNotification(i18n_r()$t("Dead time and Column length are required."), type = "warning")
          return(NULL)
        }

        if ("porosity" %in% selected_metrics || "flowres" %in% selected_metrics || "pabil" %in% selected_metrics || "spabil" %in% selected_metrics || "all" %in% selected_metrics) {
          req(input$addmets_flow, input$addmets_id, input$addmets_deltap, input$addmets_visc, input$addmets_dp)
          if (is.na(input$addmets_flow) || is.na(input$addmets_id) || is.na(input$addmets_deltap) || is.na(input$addmets_visc) || is.na(input$addmets_dp)) {
            showNotification(i18n_r()$t("Flow rate, Column Internal Diameter, Back Pressure, Mobile Phase Viscosity, and Particle Size are required for the selected metrics."), type = "warning")
            return(NULL)
          }
        }
      }


      tryCatch(
        {
          withProgress(message = i18n_r()$t("Calculating Additional Metrics"), value = 0, {
            incProgress(0.2, detail = i18n_r()$t("Preparing inputs"))
            incProgress(0.5, detail = i18n_r()$t("Performing calculation"))
            result <- chrom_addmets(
              which_mets = selected_metrics,
              t0 = input$addmets_t0,
              len = input$addmets_len,
              flow = input$addmets_flow,
              id = input$addmets_id,
              deltap = input$addmets_deltap,
              visc = input$addmets_visc,
              dp = input$addmets_dp
            )
            incProgress(0.8, detail = i18n_r()$t("Formatting results"))
            rv$addmets_calculated <- TRUE
            sync$metrics$addmets_outputs <- result

            result
          })
        },
        error = function(e) {
          showNotification(paste(i18n_r()$t("Error calculating additional metrics:"), e$message), type = "error")
          NULL
        }
      )
    })

    output$addmets_information_text <- renderUI({
      req(addmets_results())
      selected_addmet_names <- get_selected_names(input$addmets_which_mets, addmet_choices())

      html_content <- translate_info(
        addmets_results()$information,
        i18n_r(),
        methods = list(addmets = selected_addmet_names)
      )

      HTML(paste0("<h4>", i18n_r()$t("Additional Metrics Summary Information"), "</h4><hr>", html_content))
    })

    #Output results table for Additional Metrics
    add_prettynames <- reactive({
      c(i18n_r()$t("Metric"),
        i18n_r()$t("Value"),
        i18n_r()$t("Units"))
    })

    add_values_to_change <- reactive({
      c("Linear Velocity" = i18n_r()$t("Linear Velocity"),
        "Packing Porosity" = i18n_r()$t("Packing Porosity"),
        "Flow Resistance" = i18n_r()$t("Flow Resistance"),
        "Permeability" = i18n_r()$t("Permeability"),
        "Specific Permeability" = i18n_r()$t("Specific Permeability"),
        "mmsec^-1" = i18n_r()$t("mm sec<sup>-1</sup>"),
        "mm^2s^-1bar^-1" = i18n_r()$t("mm<sup>2</sup> sec<sup>-1</sup> bar<sup>-1</sup>"),
        "mm^2" = i18n_r()$t("mm<sup>2</sup>"),
        "dimensionless" = i18n_r()$t("dimensionless")
        )
    })

    output$addmets_results_table <- DT::renderDataTable({
      req(addmets_results())
      dt_data <- addmets_results()$results
      if ("value" %in% names(dt_data)) {
        dt_data$"value" <- round(as.numeric(dt_data$"value"), 3)
      }
      new_vals <- add_values_to_change()
      dt_data <- as.data.frame(apply(dt_data, 2, my_mapvalues, names(new_vals), new_vals))
      DT::datatable(dt_data, extensions = "Buttons",
                    options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                   buttons = list(
                                     list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                          action = copy_button_no_popup(
                                            copy_label = i18n_r()$t("Copy"),
                                            copied_label = i18n_r()$t("Copied!")
                                          )),
                                     list(extend = "csv", filename = generate_filename_with_timestamp("column-metrics")),
                                     list(extend = "excel", filename = generate_filename_with_timestamp("column-metrics")),
                                     list(extend = "pdf", filename = generate_filename_with_timestamp("column-metrics"))
                                   )),
                    colnames = add_prettynames(), rownames = FALSE, escape = FALSE
                    )
    })

    # DYNAMIC VOSCOSITY
    ## Dynamically create fraction inputs
    output$viscfrac_inputs <- renderUI({
      req(input$visc_which_solv)
      solvs <- visc_choices()
      solvnames <- names(solvs)
      lapply(seq_along(input$visc_which_solv), function(i) {
        numericInput(
          inputId = paste0(ns("frac_"), i),
          label   = paste0(i18n_r()$t("Fraction"), " (", solvnames[solvs %in% input$visc_which_solv[i]], ")"),
          value   = 1/length(input$visc_which_solv),
          min     = 0,
          max     = 1,
          step    = 0.01
        )
      })
    })

    ## Perform calculation
    visc_res <- eventReactive(input$visc_calc, {
      req(input$visc_temp)
      req(input$visc_which_solv)
      req(input$frac_1)
      #req(output$viscfrac_inputs)

      # Collect fractions
      fracs <- sapply(seq_along(input$visc_which_solv), function(x) input[[paste0("frac_", x)]])
      #cat(fracs)
       shiny::validate(
         need(ifelse(length(fracs) != 3, abs(sum(fracs) - 1) < 1e-6, abs(sum(fracs) - 1) < 0.01),
              i18n_r()$t("Fractions must sum to exactly 1.")),
         need(length(fracs) == length(input$visc_which_solv),
              i18n_r()$t("Each component must have a fraction."))
       )

      chrom_visc(
        ids       = input$visc_which_solv,
        fracs     = fracs,
        frac_type = input$visc_fractype,
        temp      = input$visc_temp
      )
    })

    # Viscosity Results table
    visc_prettynames <- reactive({
      i18n_r()$t(c("Parameter", "Value"))
    })

    visc_values_to_change <- reactive({
      solv_list <- browse_visc()$vogel_viscosity[,"Short_Name"]
      frac_list <- c("_volfrac", "_massfrac", "_molfrac", "visc_mPas", "temp_degC")
      setNames(i18n_r()$t(
        c("Butanol",
          "Isopropanol",
          "Acetone",
          "Acetonitrile",
          "Benzene",
          "Chloroform",
          "Cyclohexane",
          "Diethyl Ether",
          "Ethanol",
          "Ethyl Acetate",
          "Methanol",
          "Tetrahydrofuran",
          "Water",
          " (vol)",
          " (mass)",
          " (mol)",
          "Dynamic Viscosity (mPas)",
          "Temperature (&deg;C)")), c(solv_list, frac_list))
    })

    output$visc_results_table <- DT::renderDataTable({
       req(visc_res()$results[[1]])
       dt_data <- visc_res()$results[[1]]
       dt_data <- data.frame(
         Parameter = names(dt_data),
         Value     = round(as.numeric(dt_data), 3),
         row.names = NULL
       )

       newvals <- visc_values_to_change()

       dt_data[,1] <- stringr::str_replace_all(dt_data[,1], newvals)
       DT::datatable(dt_data, extensions = "Buttons",
                     options = list(dom = "Bfrtip", scrollX = TRUE, language = tablang(),
                                    buttons = list(
                                      list(extend = "copy", text = i18n_r()$t("Copy"), titleAttr = i18n_r()$t("Copy"),
                                           action = copy_button_no_popup(
                                             copy_label = i18n_r()$t("Copy"),
                                             copied_label = i18n_r()$t("Copied!")
                                           )),
                                      list(extend = "csv", filename = generate_filename_with_timestamp("viscosity")),
                                      list(extend = "excel", filename = generate_filename_with_timestamp("viscosity")),
                                      list(extend = "pdf", filename = generate_filename_with_timestamp("viscosity"))
                                    )),
                     colnames = visc_prettynames(), rownames = FALSE, escape = FALSE
                     )
     })

    #Viscosity info text
    output$visc_information_text <- renderUI({
      req(visc_res()$information)
      selected_solvent_names <- get_selected_names(input$visc_which_solv, visc_choices())
      selected_frac_name <- get_selected_names(input$visc_fractype, frac_choices())

      html_content <- translate_info(
        visc_res()$information,
        i18n_r(),
        methods = list(
          visc_solvents = selected_solvent_names,
          visc_frac = selected_frac_name
        )
      )

      HTML(paste0("<h4>", i18n_r()$t("Viscosity Summary Information"), "</h4><hr>", html_content))
    })

    #Prepare column names for modal data tables
    modal_prettynames <- reactive({
      c(i18n_r()$t("Peak"),
        i18n_r()$t("RT (min)"),
        i18n_r()$t("Area"),
        i18n_r()$t("Signal/Noise"),
        i18n_r()$t("Width"))
    })

    # Resolution Modal Observers
    observeEvent(input$res_peaks_modal_btn, {
      req(sync$peak_detection$outputs)
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      if (is.null(peak_data) || nrow(peak_data) == 0) {
        showNotification(i18n_r()$t("No peaks detected. Please perform peak detection first."), type = "warning")
        return(NULL)
      }

      max_peaks <- nrow(peak_data)
      current_peaks1_selection <- tryCatch(
        parse_peak_indices(input$res_peaks1_manual, max_peaks),
        error = function(e) integer(0) # Handle parsing errors for initial selection
      )
      current_peaks2_selection <- tryCatch(
        parse_peak_indices(input$res_peaks2_manual, max_peaks),
        error = function(e) integer(0)
      )

      showModal(modalDialog(
        title = i18n$t("Select Peaks for Resolution"),
        size = "l",
        fluidRow(
          column(8,
            h4(i18n$t("Peak Information")),
            DT::dataTableOutput(ns("modal_res_peak_table"))
          ),
          column(4,
            h4(i18n$t("Peaks 1 (Earlier-Eluting)")),
            div(style = "max-height: 200px; overflow-y: scroll;",
                uiOutput(ns("modal_res_peaks1_checkboxes"))
            ),
            h4(i18n$t("Peaks 2 (Later-Eluting)")),
            div(style = "max-height: 200px; overflow-y: scroll;",
                uiOutput(ns("modal_res_peaks2_checkboxes"))
            )
          )
        ),
        uiOutput(ns("modal_res_validation_message")),
        footer = tagList(
          modalButton(i18n$t("Cancel")),
          actionButton(ns("modal_res_apply"), i18n$t("Apply"), class = "btn-primary")
        )
      ))

      #Output results table for Modal Results

      #Render table for Resolution
      output$modal_res_peak_table <- DT::renderDataTable({
        req(peak_data)
        # Select and rename important columns for modal display
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

      output$modal_res_peaks1_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak")," %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_res_peaks1_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = current_peaks1_selection)
      })

      output$modal_res_peaks2_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak")," %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_res_peaks2_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = current_peaks2_selection)
      })
    })

    # Live validation for Resolution modal
    observe({
      req(input$modal_res_peaks1_sel, input$modal_res_peaks2_sel)
      len1 <- length(input$modal_res_peaks1_sel)
      len2 <- length(input$modal_res_peaks2_sel)

      output$modal_res_validation_message <- renderUI({
        if (len1 == 0 || len2 == 0) {
          div(class = "alert alert-info",
              icon("info-circle"), i18n$t("Select at least one peak from each group"))
        } else if (len1 != len2) {
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              sprintf(i18n$t("Unequal selection: %d vs %d peaks. Must be equal!"), len1, len2))
        } else {
          div(class = "alert alert-success",
              icon("check-circle"),
              sprintf(i18n$t("Ready: Will create %d peak pairs"), len1))
        }
      })

      # Toggle Apply button state
      valid <- len1 > 0 && len1 == len2
      shinyjs::toggleState("modal_res_apply", condition = valid)
    })

    # Apply logic for Resolution modal
    observeEvent(input$modal_res_apply, {
      new_text1 <- paste(input$modal_res_peaks1_sel, collapse = ",")
      new_text2 <- paste(input$modal_res_peaks2_sel, collapse = ",")

      updateTextInput(session, "res_peaks1_manual", value = new_text1)
      updateTextInput(session, "res_peaks2_manual", value = new_text2)

      removeModal()
    })


    # Separation Factor Modal Observers
    observeEvent(input$sepf_peaks_modal_btn, {
      req(sync$peak_detection$outputs)
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      if (is.null(peak_data) || nrow(peak_data) == 0) {
        showNotification(i18n_r()$t("No peaks detected. Please perform peak detection first."), type = "warning")
        return(NULL)
      }

      max_peaks <- nrow(peak_data)
      current_peaks1_selection <- tryCatch(
        parse_peak_indices(input$sepf_peaks1_manual, max_peaks),
        error = function(e) integer(0)
      )
      current_peaks2_selection <- tryCatch(
        parse_peak_indices(input$sepf_peaks2_manual, max_peaks),
        error = function(e) integer(0)
      )

      showModal(modalDialog(
        title = i18n$t("Select Peaks for Separation Factor"),
        size = "l",
        fluidRow(
          column(8,
            h4(i18n$t("Peak Information")),
            DT::dataTableOutput(ns("modal_sepf_peak_table"))
          ),
          column(4,
            h4(i18n$t("Peaks 1 (Earlier-Eluting)")),
            div(style = "max-height: 200px; overflow-y: scroll;",
                uiOutput(ns("modal_sepf_peaks1_checkboxes"))
            ),
            h4(i18n$t("Peaks 2 (Later-Eluting)")),
            div(style = "max-height: 200px; overflow-y: scroll;",
                uiOutput(ns("modal_sepf_peaks2_checkboxes"))
            )
          )
        ),
        uiOutput(ns("modal_sepf_validation_message")),
        footer = tagList(
          modalButton(i18n$t("Cancel")),
          actionButton(ns("modal_sepf_apply"), i18n$t("Apply"), class = "btn-primary")
        )
      ))

      #Output results table for Modal Results
      output$modal_sepf_peak_table <- DT::renderDataTable({
        req(peak_data)
        # Select and rename important columns for modal display
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

      output$modal_sepf_peaks1_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak")," %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_sepf_peaks1_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = current_peaks1_selection)
      })

      output$modal_sepf_peaks2_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak")," %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_sepf_peaks2_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = current_peaks2_selection)
      })
    })

    # Live validation for Separation Factor modal
    observe({
      req(input$modal_sepf_peaks1_sel, input$modal_sepf_peaks2_sel)
      len1 <- length(input$modal_sepf_peaks1_sel)
      len2 <- length(input$modal_sepf_peaks2_sel)

      output$modal_sepf_validation_message <- renderUI({
        if (len1 == 0 || len2 == 0) {
          div(class = "alert alert-info",
              icon("info-circle"), i18n$t("Select at least one peak from each group"))
        } else if (len1 != len2) {
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              sprintf(i18n$t("Unequal selection: %d vs %d peaks. Must be equal!"), len1, len2))
        } else {
          div(class = "alert alert-success",
              icon("check-circle"),
              sprintf(i18n$t("Ready: Will create %d peak pairs"), len1))
        }
      })

      valid <- len1 > 0 && len1 == len2
      shinyjs::toggleState("modal_sepf_apply", condition = valid)
    })

    # Apply logic for Separation Factor modal
    observeEvent(input$modal_sepf_apply, {
      new_text1 <- paste(input$modal_sepf_peaks1_sel, collapse = ",")
      new_text2 <- paste(input$modal_sepf_peaks2_sel, collapse = ",")

      updateTextInput(session, "sepf_peaks1_manual", value = new_text1)
      updateTextInput(session, "sepf_peaks2_manual", value = new_text2)

      removeModal()
    })


    # Retention Factor Modal Observers
    observeEvent(input$retf_peaks_modal_btn, {
      req(sync$peak_detection$outputs)
      peak_data <- sync$peak_detection$outputs$results$Peak_Extents
      if (is.null(peak_data) || nrow(peak_data) == 0) {
        showNotification(i18n_r()$t("No peaks detected. Please perform peak detection first."), type = "warning")
        return(NULL)
      }

      max_peaks <- nrow(peak_data)
      current_peaks_selection <- tryCatch(
        parse_peak_indices(input$retf_peaks_manual, max_peaks),
        error = function(e) integer(0)
      )

      showModal(modalDialog(
        title = i18n$t("Select Peaks for Retention Factor"),
        size = "l",
        fluidRow(
          column(8,
            h4(i18n$t("Peak Information")),
            DT::dataTableOutput(ns("modal_retf_peak_table"))
          ),
          column(4,
            h4(i18n$t("Peak Selection")),
            div(style = "max-height: 400px; overflow-y: scroll;",
                uiOutput(ns("modal_retf_peaks_checkboxes"))
            )
          )
        ),
        uiOutput(ns("modal_retf_validation_message")),
        footer = tagList(
          modalButton(i18n$t("Cancel")),
          actionButton(ns("modal_retf_apply"), i18n$t("Apply"), class = "btn-primary")
        )
      ))

      #Output results table for Modal Results
      output$modal_retf_peak_table <- DT::renderDataTable({
        req(peak_data)
        # Select and rename important columns for modal display
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

      output$modal_retf_peaks_checkboxes <- renderUI({
        labels <- sapply(1:max_peaks, function(i) {
          sprintf(paste0(i18n_r()$t("Peak")," %d"), peak_data$peak[i])
        })
        checkboxGroupInput(ns("modal_retf_peaks_sel"), NULL,
                           choices = setNames(1:max_peaks, labels),
                           selected = current_peaks_selection)
      })
    })

    # Live validation for Retention Factor modal
    observe({
      req(input$modal_retf_peaks_sel)
      len <- length(input$modal_retf_peaks_sel)

      output$modal_retf_validation_message <- renderUI({
        if (len == 0) {
          div(class = "alert alert-info",
              icon("info-circle"), i18n$t("Select at least one peak"))
        } else {
          div(class = "alert alert-success",
              icon("check-circle"),
              sprintf(i18n$t("Ready: %d peaks selected"), len))
        }
      })

      valid <- len > 0
      shinyjs::toggleState("modal_retf_apply", condition = valid)
    })

    # Apply logic for Retention Factor modal
    observeEvent(input$modal_retf_apply, {
      new_text <- paste(input$modal_retf_peaks_sel, collapse = ",")
      updateTextInput(session, "retf_peaks_manual", value = new_text)
      removeModal()
    })


    # Edge case: No peaks detected
    observe({
      if (is.null(sync$peak_detection$outputs) || nrow(sync$peak_detection$outputs$results$Peak_Extents) == 0) {
        # Disable modal buttons if no peaks are detected
        disable("res_peaks_modal_btn")
        disable("retf_peaks_modal_btn")
        disable("sepf_peaks_modal_btn")
      } else {
        enable("res_peaks_modal_btn")
        enable("retf_peaks_modal_btn")
        enable("sepf_peaks_modal_btn")
      }
    })

    observe({
      selected_tplate_methods <- input$tplate_method
      if ("all" %in% selected_tplate_methods && length(selected_tplate_methods) > 1) {
        updateSelectizeInput(session, "tplate_method", selected = "all")
      }
    })

    observe({
      if (rv$tplate_calculated) { #&& rv$addmets_calculated
        sync$metrics$completed <- TRUE
      } else {
        sync$metrics$completed <- FALSE # Reset if one becomes uncalculated
      }
    })

    #Add flag for each performance metrics method
    observe({
      for(tpmet in c("FW", "S5", "EP", "inf", "AH", "EMG", "all")) {
        if(tpmet %in% input$tplate_method & rv$tplate_calculated) {
          sync$metrics[[paste0("is_",tpmet)]] <- TRUE
        }
      }
    })

    #Add flag for additional metrics
    observe({
      for(addmet in c("linvel", "porosity", "flowres", "pabil", "spabil", "all")) {
        if(addmet %in% input$addmets_which_mets & rv$addmets_calculated) {
          sync$metrics[[paste0("is_",addmet)]] <- TRUE
        }
      }
    })

    # Theoretical Plates Intro
    observeEvent(input$tplate_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Theoretical Plates"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_tplate(ns, i18n, input))
      })
    })

    # Resolution Intro
    observeEvent(input$res_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Resolution"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_res(ns, i18n, input))
      })
    })

    # Retention Factor Intro
    observeEvent(input$retf_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Retention Factor (k)"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_retf(ns, i18n, input))
      })
    })

    # Separation Factor Intro
    observeEvent(input$sepf_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Separation Factor"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_sepf(ns, i18n, input))
      })
    })

    # Additional Metrics Intro
    observeEvent(input$addmets_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Column-Specific Metrics"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_addmets(ns, i18n))
      })
    })

    # Viscosity Calculation Intro
    observeEvent(input$visc_intro, {
      shinyjs::runjs(paste0("$('#", ns("navlist"), " a[data-value=\"", i18n$t("Viscosity Calculation"), "\"]').tab('show');"))
      shinyjs::delay(500, {
        introjs(session, options = intro_steps_tab8_visc(ns, i18n))
      })
    })

  }) # End moduleServer
}
