# ============================================================
# UPDATED SERVER MODULE EXAMPLES (v2)
# ============================================================
#
# This file shows how to update the existing server modules to use
# the translate_info() helper function with method names passed
# directly from Shiny input$ objects.
#
# Add the following line to the top of each server module file:
# source(system.file("shiny-app", "utils", "info_translator.R", package = "lcqc"))
#
# KEY CONCEPT: Use get_selected_names() to get the display names (which are
# the translation keys) from selectInput choices, then pass them to translate_info()
# ============================================================

# ============================================================
# TAB 7: PEAK SYMMETRY - Updated information text rendering
# ============================================================
# File: tab7_peak_symmetry_server.R

# The choices reactive already exists:
# symm_choices <- reactive({
#   setNames(c("all", "Tf", "As", "tpa"),
#            i18n_r()$t(c("All Available",
#                         "USP Tailing Factor (Tf)",
#                         "Asymmetry Factor (As)",
#                         "Total Peak Analysis (TPA)")))
# })

# UPDATED renderUI:
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


# ============================================================
# TAB 5: INTEGRATION - Updated information text rendering
# ============================================================
# File: tab5_integration_server.R

# For chrom_skim, you have skim_choices reactive:
# skim_choices <- reactive({
#   setNames(c("pdrop", "tskim", "exskim", "gskim"),
#            i18n_r()$t(c("Perpendicular Drop (PD)",
#                         "Tangent Skim (TS)",
#                         "Exponential Skim (ES)",
#                         "Gaussian Skim (GS)")))
# })

# UPDATED renderUI:
output$integration_information_text <- renderUI({
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


# ============================================================
# TAB 6: DECONVOLUTION - Updated information text rendering
# ============================================================
# File: tab6_deconvolution_server.R

# For chrom_icf, you have deconv_choices reactive:
# deconv_choices <- reactive({
#   setNames(c("all", "gs", "emg", "egh", "etg"),
#            i18n_r()$t(c("All Available",
#                         "Simple Gaussian",
#                         "Exponentially-Modified Gaussian (EMG)",
#                         "Exponential-Gaussian Hybrid (EGH)",
#                         "Empirically-Transformed Gaussian (ETG)")))
# })

# UPDATED renderUI:
output$deconvolution_information_text <- renderUI({
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


# ============================================================
# TAB 8: PERFORMANCE METRICS - Updated information text rendering
# ============================================================
# File: tab8_performance_metrics_server.R

# --- Resolution ---
# res_choices <- reactive({
#   setNames(c("all", "W0", "W50_1", "W50_2", "sepret"),
#            i18n_r()$t(c("All",
#                         "Full Width",
#                         "Half-Width (variant 1)",
#                         "Half-Width (variant 2)",
#                         "Fundamental Resolution Equation")))
# })

output$res_information_text <- renderUI({
  req(res_results()$information)
  
  selected_res_names <- get_selected_names(input$res_method, res_choices())
  
  html_content <- translate_info(
    res_results()$information, 
    i18n_r(),
    methods = list(resolution = selected_res_names)
  )
  
  HTML(paste0("<h4>", i18n_r()$t("Resolution Summary Information"), "</h4><hr>", html_content))
})


# --- Retention Factor ---
# No method selection for retention factor, just peaks
output$retf_information_text <- renderUI({
  req(retf_results()$information)
  
  html_content <- translate_info(retf_results()$information, i18n_r())
  
  HTML(paste0("<h4>", i18n_r()$t("Retention Factor Summary Information"), "</h4><hr>", html_content))
})


# --- Separation Factor ---
# No method selection for separation factor
output$sepf_information_text <- renderUI({
  req(sepf_results()$information)
  
  html_content <- translate_info(sepf_results()$information, i18n_r())
  
  HTML(paste0("<h4>", i18n_r()$t("Separation Factor Summary Information"), "</h4><hr>", html_content))
})


# --- Theoretical Plates ---
# tplate_choices <- reactive({
#   setNames(c("all", "FW", "S5", "EP", "inf", "AH", "EMG"),
#            i18n_r()$t(c("All",
#                         "Full Width (FW)",
#                         "5-Sigma (S5)",
#                         "European Pharmacopoeia (EP)",
#                         "Inflection Point Width (inf)",
#                         "Area-Height (AH)",
#                         "Exponentially-Modified Gaussian")))
# })

output$tplate_information_text <- renderUI({
  req(tplate_results)
  
  selected_tplate_names <- get_selected_names(input$tplate_method, tplate_choices())
  
  html_content <- translate_info(
    tplate_results$information, 
    i18n_r(),
    methods = list(tplate = selected_tplate_names)
  )
  
  HTML(paste0("<h4>", i18n_r()$t("Theoretical Plates Summary Information"), "</h4><hr>", html_content))
})


# --- Additional Metrics ---
# addmet_choices <- reactive({
#   setNames(c("all", "linvel", "porosity", "flowres", "pabil", "spabil"),
#            i18n_r()$t(c("All",
#                         "Linear Velocity",
#                         "Packing Porosity",
#                         "Flow Resistance",
#                         "Permeability",
#                         "Specific Permeability")))
# })

output$addmets_information_text <- renderUI({
  req(addmets_results()$information)
  
  selected_addmet_names <- get_selected_names(input$addmets_which_mets, addmet_choices())
  
  html_content <- translate_info(
    addmets_results()$information, 
    i18n_r(),
    methods = list(addmets = selected_addmet_names)
  )
  
  HTML(paste0("<h4>", i18n_r()$t("Additional Metrics Summary Information"), "</h4><hr>", html_content))
})


# --- Viscosity ---
# visc_choices <- reactive({
#   setNames(c("meoh", "mecn", "h2o", "etoh", "ipa", "acet", "etac", "thf", "chcl3", "benz", "chex", "dee", "buoh"),
#            i18n_r()$t(c("Methanol", "Acetonitrile", "Water", "Ethanol", "Isopropanol",
#                         "Acetone", "Ethyl Acetate", "Tetrahydrofuran", "Chloroform",
#                         "Benzene", "Cyclohexane", "Diethyl Ether", "Butanol")))
# })
#
# frac_choices <- reactive({
#   setNames(c("vol", "mass", "mol"),
#            i18n_r()$t(c("Volume fraction", "Mass fraction", "Mole fraction")))
# })

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


# ============================================================
# TAB 4: PEAK DETECTION - Updated information text rendering
# ============================================================
# File: tab4_peak_detect_server.R

# Peak detection has multiple method inputs:
# - baseline correction method (bline_method)
# - smoothing method (smooth_method)
# - amplitude threshold method (amp_thres)
# - derivative threshold method (der_thres)

# You would need to have the corresponding choices reactives available.
# Example (you may need to adjust based on actual input names):

output$detection_info <- renderUI({
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


# ============================================================
# SUMMARY: The pattern for all modules is:
# ============================================================
#
# 1. Get selected method display names using get_selected_names():
#    selected_names <- get_selected_names(input$method_input, method_choices())
#
# 2. Pass to translate_info() in the methods list:
#    html_content <- translate_info(
#      results$information,
#      i18n_r(),
#      methods = list(method_type = selected_names)
#    )
#
# 3. Wrap in HTML with translated header:
#    HTML(paste0("<h4>", i18n_r()$t("Header Text"), "</h4><hr>", html_content))
#
# The key insight is that selectInput choices are named vectors where:
# - names = display text (translation keys)
# - values = internal codes
#
# So get_selected_names() reverses this to get the display names from codes.
# ============================================================
