# inst/shiny-app/server_modules/settings_server.R

# Define module names mapping (internal key -> pretty display name translation key)
get_module_names <- function() {
  list(
    dimp = "Data Import",
    bline = "Baseline Correction",
    smooth = "Smoothing",
    pdet = "Peak Detection",
    integ = "Integration",
    dconv = "Deconvolution",
    psym = "Peak Symmetry",
    perf = "Performance Metrics",
    rpr = "Reporting"
  )
}

# Helper function to collect current settings from the UI
get_current_settings <- function(main_input, i18n) {
  config <- list()

  # dimp
  config$dimp <- list(
    dataSource = list(selected = main_input[['infile-dataSource']]),
    mode = list(selected = main_input[['infile-mode']]),
    shim_trange_lower = list(value = main_input[['infile-shim_trange_lower']]),
    shim_trange_upper = list(value = main_input[['infile-shim_trange_upper']]),
    simtable = list(value = main_input[['infile-simtable']]),
    ptable = list(value = main_input[['infile-ptable']]),
    pnames = list(value = main_input[['infile-pnames']]),
    pcas = list(value = main_input[['infile-pcas']]),
    metadata = list(value = main_input[['infile-metadata']]),
    sep = list(selected = main_input[['infile-sep']]),
    decsep = list(selected = main_input[['infile-decsep']]),
    fix_names = list(value = main_input[['infile-fix_names']]),
    fil_cols = list(value = main_input[['infile-fil_cols']]),
    csv_trange_lower = list(value = main_input[['infile-csv_trange_lower']]),
    csv_trange_upper = list(value = main_input[['infile-csv_trange_upper']]),
    csvSep = list(selected = main_input[['infile-csvSep']]),
    csvDecsep = list(selected = main_input[['infile-csvDecsep']]),
    csvHeader = list(value = main_input[['infile-csvHeader']])
  )

  # bline
  config$bline <- list(
    method = list(selected = main_input[['baseline-baseline_method']]),
    als = list(
      lambda = main_input[['baseline-als_lambda']],
      p = main_input[['baseline-als_p']],
      prec = main_input[['baseline-als_prec']],
      maxit = main_input[['baseline-als_maxit']],
      rm_neg = main_input[['baseline-als_rm_neg']]
    ),
    chang = list(
      threshold = main_input[['baseline-chang_threshold']],
      alpha = main_input[['baseline-chang_alpha']],
      bfrac = main_input[['baseline-chang_bfrac']],
      segments = main_input[['baseline-chang_segments']],
      sig_window = main_input[['baseline-chang_sig_window']],
      fit = main_input[['baseline-chang_fit']],
      rm_neg = main_input[['baseline-chang_rm_neg']]
    ),
    isrea = list(
      eta = main_input[['baseline-isrea_eta']],
      maxit = main_input[['baseline-isrea_maxit']],
      rm_neg = main_input[['baseline-isrea_rm_neg']]
    ),
    poly = list(
      deg = main_input[['baseline-poly_deg']],
      prec = main_input[['baseline-poly_prec']],
      maxit = main_input[['baseline-poly_maxit']],
      rm_neg = main_input[['baseline-poly_rm_neg']]
    )
  )

  # smooth
  config$smooth <- list(
    signal_method = main_input[['smoothing-signal_smoothing_method']],
    deriv_method = main_input[['smoothing-deriv_smoothing_method']],
    autosmooth = main_input[['smoothing-basic_auto_smoothing']],
    smoothing_pts = list(value = main_input[['smoothing-smoothing_pts']]),
    smoothing_passes = list(value = main_input[['smoothing-smoothing_passes']]),
    start_smooth = list(pts = main_input[['smoothing-start_smooth_pts']], passes = main_input[['smoothing-start_smooth_passes']])
  )

  # pdet
  config$pdet <- list(
    amp_thres_method = list(selected = main_input[['peak_detect-amp_thres_method']]),
    amp_thres_manual = list(value = main_input[['peak_detect-use_manual_amp_thres']]),
    amp_manual = list(value = main_input[['peak_detect-manual_amp_thres']]),
    ampfrac = list(value = main_input[['peak_detect-ampfrac_advanced']]),
    amp_thres_pars = list(
      quant = list(value = main_input[['peak_detect-amp_thres_pars_quant']]),
      diff = list(value = main_input[['peak_detect-amp_thres_pars_diff']]),
      zscore = list(
        lag = list(value = main_input[['peak_detect-amp_thres_pars_zscore_lag']]),
        thres = list(value = main_input[['peak_detect-amp_thres_pars_zscore_thres']]),
        sens = list(value = main_input[['peak_detect-amp_thres_pars_zscore_sens']])
      )
    ),
    sens_fd = list(value = main_input[['peak_detect-sens_fd']]),
    sens_sd = list(value = main_input[['peak_detect-sens_sd']]),
    sens_amp = list(value = main_input[['peak_detect-sens_amp']]),
    det_bunch = list(value = main_input[['peak_detect-det_bunch']]),
    der_thres_method1 = list(selected = main_input[['peak_detect-der_thres_method1']]),
    der_thres_method2 = list(selected = main_input[['peak_detect-der_thres_method2']]),
    fast_chrom_critical_width = list(value = main_input[['peak_detect-fast_chrom_critical_width']]),
    enable_fast_chrom = list(value = main_input[['peak_detect-enable_fast_chrom']]),
    rej = list(
      sn = list(value = main_input[['peak_detect-rej_sn']]),
      ht = list(value = main_input[['peak_detect-rej_ht']]),
      wd = list(value = main_input[['peak_detect-rej_wd']]),
      pa = list(value = main_input[['peak_detect-rej_pa']])
    ),
    ma = list(
      pts_first = list(selected = main_input[['peak_detect-ma_pts_first']]),
      pts_second = list(value = main_input[['peak_detect-ma_pts_second']]),
      passes_first = list(selected = main_input[['peak_detect-ma_passes_first']]),
      passes_second = list(value = main_input[['peak_detect-ma_passes_second']])
    ),
    mpts = list(
      signal = list(value = main_input[['peak_detect-mpts_signal']]),
      deriv = list(value = main_input[['peak_detect-mpts_deriv']])
    ),
    crosspts = list(
      signal = list(value = main_input[['peak_detect-crosspts_signal']]),
      deriv = list(value = main_input[['peak_detect-crosspts_deriv']])
    ),
    rej_logic = list(pre = list(selected = main_input[['peak_detect-rej_logic_pre']]),
                     post = list(selected = main_input[['peak_detect-rej_logic_post']])),
    apex_pars = list(liftoff = list(value = main_input[['peak_detect-apex_pars_liftoff']]),
                     touchdown = list(value = main_input[['peak_detect-apex_pars_touchdown']]))
  )

  # integ
  config$integ <- list(
    method = list(selected = main_input[['integration-integration_method']]),
    skim = list(value = main_input[['integration-integration_skim']]),
    dyson = list(value = main_input[['integration-integration_dyson']]),
    crit_w = list(value = main_input[['integration-manual_width_int']]),
    manual_crit = list(value = main_input[['integration-auto_crit_width_int']])
  )

  # dconv
  config$dconv <- list(
    method = list(selected = main_input[['deconvolution-deconvolution_method']]),
    manual_crit_d = list(value = main_input[['deconvolution-auto_crit_width_decon']]),
    crit_w_d = list(value = main_input[['deconvolution-manual_width_decon']]),
    modres = list(value = main_input[['deconvolution-deconvolution_modres']]),
    optmet = list(selected = main_input[['deconvolution-deconvolution_optmet']]),
    reprs_emg = list(value = main_input[['deconvolution-deconvolution_reprs_emg']])
  )

  # psym
  config$psym <- list(
    method = list(selected = main_input[['symmetry-symmetry_method']]),
    start_peak = list(value = main_input[['symmetry-symmetry_start_peak']]),
    end_peak = list(value = main_input[['symmetry-symmetry_end_peak']]),
    manual_crit_s = list(value = main_input[['symmetry-auto_crit_width_symm']]),
    crit_w_s = list(value = main_input[['symmetry-symmetry_crit_w']]),
    show_widths = list(value = main_input[['symmetry-show_widths']]),
    optmet = list(selected = main_input[['symmetry-symmetry_optmet']]),
    reprs = list(value = main_input[['symmetry-symmetry_reprs']])
  )

  # perf
  config$perf <- list(
    res = list(
      peaks1_manual = list(value = main_input[['performance-res_peaks1_manual']]),
      peaks2_manual = list(value = main_input[['performance-res_peaks2_manual']]),
      method = list(selected = main_input[['performance-res_method']]),
      t0_mode = list(selected = main_input[['performance-res_t0_mode']]),
      ks_manual = list(value = main_input[['performance-res_ks_manual']]),
      ks_peak_index = list(value = main_input[['performance-res_ks_peak_index']]),
      manual_crit_res = list(value = main_input[['performance-auto_crit_width_res']]),
      crit_w_res = list(value = main_input[['performance-res_crit_w']])
    ),
    retf = list(
      t0_mode = list(selected = main_input[['performance-retf_t0_mode']]),
      t0_manual = list(value = main_input[['performance-retf_t0_manual']]),
      t0_peak_index = list(value = main_input[['performance-retf_t0_peak_index']]),
      peaks_manual = list(value = main_input[['performance-retf_peaks_manual']]),
      manual_crit_retf = list(value = main_input[['performance-auto_crit_width_retf']]),
      crit_w_retf = list(value = main_input[['performance-retf_crit_w']])
    ),
    sepf = list(
      peaks1_manual = list(value = main_input[['performance-sepf_peaks1_manual']]),
      peaks2_manual = list(value = main_input[['performance-sepf_peaks2_manual']]),
      t0_mode = list(selected = main_input[['performance-sepf_t0_mode']]),
      ks_manual = list(value = main_input[['performance-sepf_ks_manual']]),
      ks_peak_index = list(value = main_input[['performance-sepf_ks_peak_index']]),
      manual_crit_sepf = list(value = main_input[['performance-auto_crit_width_sepf']]),
      crit_w_sepf = list(value = main_input[['performance-sepf_crit_w']])
    ),
    tplate = list(
      method = list(selected = main_input[['performance-tplate_method']]),
      len = list(value = main_input[['performance-tplate_len']]),
      dp = list(value = main_input[['performance-tplate_dp']]),
      show_widths = list(value = main_input[['performance-tplate_show_widths']]),
      manual_crit_tplate = list(value = main_input[['performance-auto_crit_width_tplate']]),
      crit_w_tplate = list(value = main_input[['performance-tplate_crit_w']]),
      deltap = list(value = main_input[['performance-tplate_deltap']]),
      visc = list(value = main_input[['performance-tplate_visc']]),
      t0_mode = list(selected = main_input[['performance-tplate_t0_mode']]),
      t0_manual = list(value = main_input[['performance-tplate_t0_manual']]),
      t0_peak_index = list(value = main_input[['performance-tplate_t0_peak_index']]),
      imped_met = list(selected = main_input[['performance-tplate_imped_met']])
    ),
    addmets = list(
      which_mets = list(selected = main_input[['performance-addmets_which_mets']]),
      t0 = list(value = main_input[['performance-addmets_t0']]),
      len = list(value = main_input[['performance-addmets_len']]),
      flow = list(value = main_input[['performance-addmets_flow']]),
      id = list(value = main_input[['performance-addmets_id']]),
      deltap = list(value = main_input[['performance-addmets_deltap']]),
      visc = list(value = main_input[['performance-addmets_visc']]),
      dp = list(value = main_input[['performance-addmets_dp']])
    ),
    visc = list(
      which_solv = list(selected = main_input[['performance-visc_which_solv']]),
      fractype = list(selected = main_input[['performance-visc_fractype']]),
      #frac_1 = list(value = main_input[['performance-frac_1']]),
      #frac_2 = list(value = main_input[['performance-frac_2']]),
      #frac_3 = list(value = main_input[['performance-frac_3']]),
      #frac_4 = list(value = main_input[['performance-frac_4']]),
      #frac_5 = list(value = main_input[['performance-frac_5']]),
      temp = list(value = main_input[['performance-visc_temp']])
    )
  )

  # rpr
  config$rpr <- list(
    which_chart = list(selected = main_input[['rep-which_chart']]),
    which_pks = main_input[['rep-which_pks']],
    pnms = main_input[['rep-pnms']],
    pcons = main_input[['rep-pcons']],
    add_tpa = main_input[['rep-add_tpa']],
    add_addmets = main_input[['rep-add_addmets']],
    add_asym = main_input[['rep-add_asym']],
    metric_EP = main_input[['rep-metric_EP']],
    metric_AH = main_input[['rep-metric_AH']],
    metric_S5 = main_input[['rep-metric_S5']],
    metric_EMG = main_input[['rep-metric_EMG']],
    metric_As = main_input[['rep-metric_As']],
    metric_Tf = main_input[['rep-metric_Tf']],
    spec_EP = main_input[['rep-spec_EP']],
    spec_AH = main_input[['rep-spec_AH']],
    spec_S5 = main_input[['rep-spec_S5']],
    spec_EMG = main_input[['rep-spec_EMG']],
    spec_As = main_input[['rep-spec_As']],
    spec_Tf = main_input[['rep-spec_Tf']],
    peak_indices = main_input[['rep-peak_indices']],

    tpars_dnum = main_input[['rep-tpars_dnum']],
    tpars_oper = main_input[['rep-tpars_oper']],
    tpars_sn = main_input[['rep-tpars_sn']],
    tpars_pn = main_input[['rep-tpars_pn']],
    tpars_desc = main_input[['rep-tpars_desc']],
    tpars_bn = main_input[['rep-tpars_bn']],
    tpars_mp = main_input[['rep-tpars_mp']],
    tpars_sp = main_input[['rep-tpars_sp']],
    tpars_bp = main_input[['rep-tpars_bp']],
    tpars_flow = main_input[['rep-tpars_flow']],
    tpars_temp = main_input[['rep-tpars_temp']],
    tpars_inj = main_input[['rep-tpars_inj']],
    tpars_unitc = main_input[['rep-tpars_unitc']],
    asprat = main_input[['rep-asprat']],
    asprat_tpa = main_input[['rep-asprat_tpa']],
    fontsize = main_input[['rep-fontsize']],
    expath = main_input[['rep-expath']]
  )
  return(config)
}

# Helper function to update inputs for dimp module
update_dimp_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "infile-dataSource", selected = config$dimp$dataSource$selected)
    updateSelectInput(session, "infile-mode", selected = config$dimp$mode$selected)
    updateNumericInput(session, "infile-shim_trange_lower", value = config$dimp$shim_trange_lower$value)
    updateNumericInput(session, "infile-shim_trange_upper", value = config$dimp$shim_trange_upper$value)
    updateCheckboxInput(session, "infile-simtable", value = config$dimp$simtable$value)
    updateCheckboxInput(session, "infile-ptable", value = config$dimp$ptable$value)
    updateCheckboxInput(session, "infile-pnames", value = config$dimp$pnames$value)
    updateCheckboxInput(session, "infile-pcas", value = config$dimp$pcas$value)
    updateCheckboxInput(session, "infile-metadata", value = config$dimp$metadata$value)
    updateSelectInput(session, "infile-sep", selected = config$dimp$sep$selected)
    updateSelectInput(session, "infile-decsep", selected = config$dimp$decsep$selected)
    updateCheckboxInput(session, "infile-fix_names", value = config$dimp$fix_names$value)
    updateCheckboxInput(session, "infile-fil_cols", value = config$dimp$fil_cols$value)
    updateNumericInput(session, "infile-csv_trange_lower", value = config$dimp$csv_trange_lower$value)
    updateNumericInput(session, "infile-csv_trange_upper", value = config$dimp$csv_trange_upper$value)
    updateSelectInput(session, "infile-csvSep", selected = config$dimp$csvSep$selected)
    updateSelectInput(session, "infile-csvDecsep", selected = config$dimp$csvDecsep$selected)
    updateCheckboxInput(session, "infile-csvHeader", value = config$dimp$csvHeader$value)
  }, error = function(e) {
    warning(paste("Error updating dimp inputs:", e$message))
  })
}

# Helper function to update inputs for bline module
update_bline_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "baseline-baseline_method", selected = config$bline$method$selected)
    updateNumericInput(session, "baseline-als_lambda", value = config$bline$als$lambda)
    updateNumericInput(session, "baseline-als_p", value = config$bline$als$p)
    updateNumericInput(session, "baseline-als_prec", value = config$bline$als$prec)
    updateNumericInput(session, "baseline-als_maxit", value = config$bline$als$maxit)
    updateCheckboxInput(session, "baseline-als_rm_neg", value = config$bline$als$rm_neg)
    updateNumericInput(session, "baseline-chang_threshold", value = config$bline$chang$threshold)
    updateNumericInput(session, "baseline-chang_alpha", value = config$bline$chang$alpha)
    updateNumericInput(session, "baseline-chang_bfrac", value = config$bline$chang$bfrac)
    updateNumericInput(session, "baseline-chang_segments", value = config$bline$chang$segments)
    updateNumericInput(session, "baseline-chang_sig_window", value = config$bline$chang$sig_window)
    updateSelectInput(session, "baseline-chang_fit", selected = config$bline$chang$fit)
    updateCheckboxInput(session, "baseline-chang_rm_neg", value = config$bline$chang$rm_neg)
    updateNumericInput(session, "baseline-isrea_eta", value = config$bline$isrea$eta)
    updateNumericInput(session, "baseline-isrea_maxit", value = config$bline$isrea$maxit)
    updateCheckboxInput(session, "baseline-isrea_rm_neg", value = config$bline$isrea$rm_neg)
    updateNumericInput(session, "baseline-poly_deg", value = config$bline$poly$deg)
    updateNumericInput(session, "baseline-poly_prec", value = config$bline$poly$prec)
    updateNumericInput(session, "baseline-poly_maxit", value = config$bline$poly$maxit)
    updateCheckboxInput(session, "baseline-poly_rm_neg", value = config$bline$poly$rm_neg)
  }, error = function(e) {
    warning(paste("Error updating bline inputs:", e$message))
  })
}

# Helper function to update inputs for smooth module
update_smooth_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "smoothing-signal_smoothing_method", selected = config$smooth$signal_method)
    updateSelectInput(session, "smoothing-deriv_smoothing_method", selected = config$smooth$deriv_method)
    updateCheckboxInput(session, "smoothing-basic_auto_smoothing", value = config$smooth$autosmooth)
    updateNumericInput(session, "smoothing-smoothing_pts", value = config$smooth$smoothing_pts$value)
    updateNumericInput(session, "smoothing-smoothing_passes", value = config$smooth$smoothing_passes$value)
    updateNumericInput(session, "smoothing-start_smooth_pts", value = config$smooth$start_smooth$pts)
    updateNumericInput(session, "smoothing-start_smooth_passes", value = config$smooth$start_smooth$passes)
  }, error = function(e) {
    warning(paste("Error updating smooth inputs:", e$message))
  })
}

# Helper function to update inputs for pdet module
update_pdet_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "peak_detect-amp_thres_method", selected = config$pdet$amp_thres_method$selected)
    updateCheckboxInput(session, "peak_detect-use_manual_amp_thres", value = config$pdet$amp_thres_manual$value)
    updateNumericInput(session, "peak_detect-amp_thres_pars_quant", value = config$pdet$amp_thres_pars$quant$value)
    updateNumericInput(session, "peak_detect-amp_thres_pars_diff", value = config$pdet$amp_thres_pars$diff$value)
    updateNumericInput(session, "peak_detect-amp_thres_pars_zscore_lag", value = config$pdet$amp_thres_pars$zscore$lag$value)
    updateNumericInput(session, "peak_detect-amp_thres_pars_zscore_thres", value = config$pdet$amp_thres_pars$zscore$thres$value)
    updateNumericInput(session, "peak_detect-amp_thres_pars_zscore_sens", value = config$pdet$amp_thres_pars$zscore$sens$value)
    updateNumericInput(session, "peak_detect-sens_fd", value = config$pdet$sens_fd$value)
    updateNumericInput(session, "peak_detect-sens_sd", value = config$pdet$sens_sd$value)
    updateNumericInput(session, "peak_detect-sens_amp", value = config$pdet$sens_amp$value)
    updateCheckboxInput(session, "peak_detect-det_bunch", value = config$pdet$det_bunch$value)
    updateSelectInput(session, "peak_detect-der_thres_method1", selected = config$pdet$der_thres_method1$selected)
    updateSelectInput(session, "peak_detect-der_thres_method2", selected = config$pdet$der_thres_method2$selected)
    updateNumericInput(session, "peak_detect-fast_chrom_critical_width", value = config$pdet$fast_chrom_critical_width$value)
    updateNumericInput(session, "peak_detect-rej_sn", value = config$pdet$rej$sn$value)
    updateNumericInput(session, "peak_detect-rej_ht", value = config$pdet$rej$ht$value)
    updateNumericInput(session, "peak_detect-rej_wd", value = config$pdet$rej$wd$value)
    updateNumericInput(session, "peak_detect-rej_pa", value = config$pdet$rej$pa$value)
    updateSelectInput(session, "peak_detect-ma_pts_first", selected = config$pdet$ma$pts_first$selected)
    updateNumericInput(session, "peak_detect-ma_pts_second", value = config$pdet$ma$pts_second$value)
    updateSelectInput(session, "peak_detect-ma_passes_first", selected = config$pdet$ma$passes_first$selected)
    updateNumericInput(session, "peak_detect-ma_passes_second", value = config$pdet$ma$passes_second$value)
    updateNumericInput(session, "peak_detect-mpts_signal", value = config$pdet$mpts$signal$value)
    updateNumericInput(session, "peak_detect-mpts_deriv", value = config$pdet$mpts$deriv$value)
    updateNumericInput(session, "peak_detect-crosspts_signal", value = config$pdet$crosspts$signal$value)
    updateNumericInput(session, "peak_detect-crosspts_deriv", value = config$pdet$crosspts$deriv$value)
    updateCheckboxInput(session, "peak_detect-enable_fast_chrom", value = config$pdet$enable_fast_chrom$value)
    updateSelectInput(session, "peak_detect-rej_logic_pre", selected = config$pdet$rej_logic$pre$selected)
    updateSelectInput(session, "peak_detect-rej_logic_post", selected = config$pdet$rej_logic$post$selected)
    updateNumericInput(session, "peak_detect-apex_pars_liftoff", value = config$pdet$apex_pars$liftoff$value)
    updateNumericInput(session, "peak_detect-apex_pars_touchdown", value = config$pdet$apex_pars$touchdown$value)
  }, error = function(e) {
    warning(paste("Error updating pdet inputs:", e$message))
  })
}

# Helper function to update inputs for integ module
update_integ_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "integration-integration_method", selected = config$integ$method$selected)
    updateNumericInput(session, "integration-integration_skim", value = config$integ$skim$value)
    updateNumericInput(session, "integration-integration_dyson", value = config$integ$dyson$value)
    updateNumericInput(session, "integration-manual_width_int", value = config$integ$crit_w$value)
    updateCheckboxInput(session, "integration-auto_crit_width_int", value = config$integ$manual_crit$value)
  }, error = function(e) {
    warning(paste("Error updating integ inputs:", e$message))
  })
}

# Helper function to update inputs for dconv module
update_dconv_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "deconvolution-deconvolution_method", selected = config$dconv$method$selected)
    updateNumericInput(session, "deconvolution-manual_width_decon", value = config$dconv$crit_w_d$value)
    updateCheckboxInput(session, "deconvolution-auto_crit_width_decon", value = config$dconv$manual_crit_d$value)
    updateCheckboxInput(session, "deconvolution-deconvolution_modres", value = config$dconv$modres$value)
    updateCheckboxInput(session, "deconvolution-deconvolution_optmet", value = config$dconv$optmet$selected)
    updateCheckboxInput(session, "deconvolution-deconvolution_reprs_emg", value = config$dconv$reprs_emg$value)
  }, error = function(e) {
    warning(paste("Error updating dconv inputs:", e$message))
  })
}

# Helper function to update inputs for psym module
update_psym_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "symmetry-symmetry_method", selected = config$psym$method$selected)
    updateNumericInput(session, "symmetry-symmetry_start_peak", value = config$psym$start_peak$value)
    updateNumericInput(session, "symmetry-symmetry_end_peak", value = config$psym$end_peak$value)
    updateNumericInput(session, "symmetry-symmetry_crit_w", value = config$psym$crit_w_s$value)
    updateCheckboxInput(session, "symmetry-auto_crit_width_symm", value = config$psym$manual_crit_s$value)
    updateCheckboxInput(session, "symmetry-show_widths", value = config$psym$show_widths$value)
    updateSelectInput(session, "symmetry-symmetry_optmet", selected = config$psym$optmet$selected)
    updateNumericInput(session, "symmetry-symmetry_reprs", value = config$psym$reprs$value)
  }, error = function(e) {
    warning(paste("Error updating psym inputs:", e$message))
  })
}

# Helper function to update inputs for perf module
update_perf_inputs <- function(session, config, i18n) {
  tryCatch({
    updateTextInput(session, "performance-res_peaks1_manual", value = config$perf$res$peaks1_manual$value)
    updateTextInput(session, "performance-res_peaks2_manual", value = config$perf$res$peaks2_manual$value)
    updateSelectInput(session, "performance-res_method", selected = config$perf$res$method$selected)
    updateSelectInput(session, "performance-res_t0_mode", selected = config$perf$res$t0_mode$selected)
    updateNumericInput(session, "performance-res_ks_manual", value = config$perf$res$ks_manual$value)
    updateNumericInput(session, "performance-res_ks_peak_index", value = config$perf$res$ks_peak_index$value)
    updateNumericInput(session, "performance-res_crit_w", value = config$perf$res$crit_w_res$value)
    updateCheckboxInput(session, "performance-auto_crit_width_res", value = config$perf$res$manual_crit_res$value)

    updateSelectInput(session, "performance-retf_t0_mode", selected = config$perf$retf$t0_mode$selected)
    updateNumericInput(session, "performance-retf_t0_manual", value = config$perf$retf$t0_manual$value)
    updateNumericInput(session, "performance-retf_t0_peak_index", value = config$perf$retf$t0_peak_index$value)
    updateTextInput(session, "performance-retf_peaks_manual", value = config$perf$retf$peaks_manual$value)
    updateNumericInput(session, "performance-retf_crit_w", value = config$perf$retf$crit_w_retf$value)
    updateCheckboxInput(session, "performance-auto_crit_width_retf", value = config$perf$retf$manual_crit_retf$value)

    updateTextInput(session, "performance-sepf_peaks1_manual", value = config$perf$sepf$peaks1_manual$value)
    updateTextInput(session, "performance-sepf_peaks2_manual", value = config$perf$sepf$peaks2_manual$value)
    updateSelectInput(session, "performance-sepf_t0_mode", selected = config$perf$sepf$t0_mode$selected)
    updateNumericInput(session, "performance-sepf_ks_manual", value = config$perf$sepf$ks_manual$value)
    updateNumericInput(session, "performance-sepf_ks_peak_index", value = config$perf$sepf$ks_peak_index$value)
    updateNumericInput(session, "performance-sepf_crit_w", value = config$perf$sepf$crit_w_sepf$value)
    updateCheckboxInput(session, "performance-auto_crit_width_sepf", value = config$perf$sepf$manual_crit_sepf$value)

    updateSelectInput(session, "performance-tplate_method", selected = config$perf$tplate$method$selected)
    updateNumericInput(session, "performance-tplate_len", value = config$perf$tplate$len$value)
    updateNumericInput(session, "performance-tplate_dp", value = config$perf$tplate$dp$value)
    updateCheckboxInput(session, "performance-tplate_show_widths", value = config$perf$tplate$show_widths$value)
    updateNumericInput(session, "performance-tplate_crit_w", value = config$perf$tplate$crit_w_tplate$value)
    updateCheckboxInput(session, "performance-auto_crit_width_tplate", value = config$perf$tplate$manual_crit_tplate$value)
    updateNumericInput(session, "performance-tplate_deltap", value = config$perf$tplate$deltap$value)
    updateNumericInput(session, "performance-tplate_visc", value = config$perf$tplate$visc$value)
    updateSelectInput(session, "performance-tplate_t0_mode", selected = config$perf$tplate$t0_mode$selected)
    updateNumericInput(session, "performance-tplate_t0_manual", value = config$perf$tplate$t0_manual$value)
    updateNumericInput(session, "performance-tplate_t0_peak_index", value = config$perf$tplate$t0_peak_index$value)
    updateSelectInput(session, "performance-tplate_imped_met", selected = config$perf$tplate$imped_met$selected)

    updateSelectInput(session, "performance-addmets_which_mets", selected = config$perf$addmets$which_mets$selected)
    updateNumericInput(session, "performance-addmets_t0", value = config$perf$addmets$t0$value)
    updateNumericInput(session, "performance-addmets_len", value = config$perf$addmets$len$value)
    updateNumericInput(session, "performance-addmets_flow", value = config$perf$addmets$flow$value)
    updateNumericInput(session, "performance-addmets_id", value = config$perf$addmets$id$value)
    updateNumericInput(session, "performance-addmets_deltap", value = config$perf$addmets$deltap$value)
    updateNumericInput(session, "performance-addmets_visc", value = config$perf$addmets$visc$value)
    updateNumericInput(session, "performance-addmets_dp", value = config$perf$addmets$dp$value)

    updateSelectInput(session, "performance-visc_which_solv", selected = config$perf$visc$which_solv$selected)
    updateSelectInput(session, "performance-visc_fractype", selected = config$perf$visc$fractype$selected)
    updateNumericInput(session, "performance-visc_temp", value = config$perf$visc$temp$value)
  }, error = function(e) {
    warning(paste("Error updating perf inputs:", e$message))
  })
}

# Helper function to update inputs for rpr module
update_rpr_inputs <- function(session, config, i18n) {
  tryCatch({
    updateSelectInput(session, "rep-which_pks", selected = config$rpr$which_pks)
    updateSelectInput(session, "rep-which_chart", selected = config$rpr$which_chart$selected)
    updateTextInput(session, "rep-pnms", value = config$rpr$pnms)
    updateTextInput(session, "rep-pcons", value = config$rpr$pcons)
    updateCheckboxInput(session, "rep-add_tpa", value = config$rpr$add_tpa)
    updateCheckboxInput(session, "rep-add_addmets", value = config$rpr$add_addmets)
    updateCheckboxInput(session, "rep-add_asym", value = config$rpr$add_asym)
    updateCheckboxInput(session, "rep-metric_EP", value = config$rpr$metric_EP)
    updateCheckboxInput(session, "rep-metric_AH", value = config$rpr$metric_AH)
    updateCheckboxInput(session, "rep-metric_S5", value = config$rpr$metric_S5)
    updateCheckboxInput(session, "rep-metric_EMG", value = config$rpr$metric_EMG)
    updateCheckboxInput(session, "rep-metric_As", value = config$rpr$metric_As)
    updateCheckboxInput(session, "rep-metric_Tf", value = config$rpr$metric_Tf)
    updateTextInput(session, "rep-spec_EP", value = config$rpr$spec_EP)
    updateTextInput(session, "rep-spec_AH", value = config$rpr$spec_AH)
    updateTextInput(session, "rep-spec_S5", value = config$rpr$spec_S5)
    updateTextInput(session, "rep-spec_EMG", value = config$rpr$spec_EMG)
    updateTextInput(session, "rep-spec_As", value = config$rpr$spec_As)
    updateTextInput(session, "rep-spec_Tf", value = config$rpr$spec_Tf)
    updateTextInput(session, "rep-peak_indices", value = config$rpr$peak_indices)
    updateTextInput(session, "rep-tpars_dnum", value = config$rpr$tpars_dnum)
    updateTextInput(session, "rep-tpars_oper", value = config$rpr$tpars_oper)
    updateTextInput(session, "rep-tpars_sn", value = config$rpr$tpars_sn)
    updateTextInput(session, "rep-tpars_pn", value = config$rpr$tpars_pn)
    updateTextInput(session, "rep-tpars_desc", value = config$rpr$tpars_desc)
    updateTextInput(session, "rep-tpars_bn", value = config$rpr$tpars_bn)
    updateTextInput(session, "rep-tpars_mp", value = config$rpr$tpars_mp)
    updateTextInput(session, "rep-tpars_sp", value = config$rpr$tpars_sp)
    updateTextInput(session, "rep-tpars_bp", value = config$rpr$tpars_bp)
    updateTextInput(session, "rep-tpars_flow", value = config$rpr$tpars_flow)
    updateTextInput(session, "rep-tpars_temp", value = config$rpr$tpars_temp)
    updateTextInput(session, "rep-tpars_inj", value = config$rpr$tpars_inj)
    updateTextInput(session, "rep-tpars_unitc", value = config$rpr$tpars_unitc)
    updateNumericInput(session, "rep-asprat", value = config$rpr$asprat)
    updateNumericInput(session, "rep-asprat_tpa", value = config$rpr$asprat_tpa)
    updateNumericInput(session, "rep-fontsize", value = config$rpr$fontsize)
    updateTextInput(session, "rep-expath", value = config$rpr$expath)
  }, error = function(e) {
    warning(paste("Error updating rpr inputs:", e$message))
  })
}

# Helper function to update selected module inputs from a loaded config object
update_selected_inputs <- function(session, config, selected_modules, i18n) {
  tryCatch({
    # Update only the selected modules
    if ("dimp" %in% selected_modules && !is.null(config$dimp)) {
      update_dimp_inputs(session, config, i18n)
    }

    if ("bline" %in% selected_modules && !is.null(config$bline)) {
      update_bline_inputs(session, config, i18n)
    }

    if ("smooth" %in% selected_modules && !is.null(config$smooth)) {
      update_smooth_inputs(session, config, i18n)
    }

    if ("pdet" %in% selected_modules && !is.null(config$pdet)) {
      update_pdet_inputs(session, config, i18n)
    }

    if ("integ" %in% selected_modules && !is.null(config$integ)) {
      update_integ_inputs(session, config, i18n)
    }

    if ("dconv" %in% selected_modules && !is.null(config$dconv)) {
      update_dconv_inputs(session, config, i18n)
    }

    if ("psym" %in% selected_modules && !is.null(config$psym)) {
      update_psym_inputs(session, config, i18n)
    }

    if ("perf" %in% selected_modules && !is.null(config$perf)) {
      update_perf_inputs(session, config, i18n)
    }

    if ("rpr" %in% selected_modules && !is.null(config$rpr)) {
      update_rpr_inputs(session, config, i18n)
    }

  }, error = function(e) {
    shinyalert("Warning", paste(i18n$t("Could not update all settings. The settings file might be outdated or corrupted. Error:"), e$message), type = "warning")
  })
}

# Legacy function for backward compatibility - updates all inputs
update_all_inputs <- function(session, config, i18n) {
  all_modules <- names(get_module_names())
  update_selected_inputs(session, config, all_modules, i18n)
}


settings_server <- function(id, sync, config, main_input, main_session, i18n) { # Added main_session
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive version of i18n
    i18n_r <- reactive({i18n})

    # Get module names with translations
    module_names <- get_module_names()

    # Store selected modules state (initialize with all selected)
    selected_modules_state <- reactiveVal(names(get_module_names()))

    # Observe checkbox changes and save state (only when not NULL)
    observeEvent(input$import_modules, {
      selected_modules_state(input$import_modules)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Render UI elements
    # JSON upload
    output$import_file <- renderUI({
      fileInput(ns("import_file"), i18n_r()$t("Choose JSON File"),
                multiple = FALSE,
                accept = c("application/json", ".json"), buttonLabel = i18n_r()$t("Browse"), placeholder = i18n_r()$t("No file selected"))
    })

    # Render module checkboxes with translated labels in two columns
    output$module_checkboxes <- renderUI({
      # Create choices with translated pretty names
      choices <- setNames(
        names(module_names),
        sapply(module_names, function(name) i18n_r()$t(name))
      )

      tagList(
        tags$style(HTML(paste0(
          "#", ns("import_modules"), " { column-count: 2; column-gap: 20px; }"
        ))),
        checkboxGroupInput(
          ns("import_modules"),
          label = NULL,
          choices = choices,
          selected = selected_modules_state() # Use stored state
        )
      )
    })

    # Render import buttons with dynamic translation
    output$import_buttons <- renderUI({
      tags$div(
        style = "display: flex; gap: 10px;",
        actionButton(ns("confirm_import"), i18n_r()$t("Import"), class = "btn-primary", icon = icon("upload")),
        actionButton(ns("load_defaults"), i18n_r()$t("Load Defaults"), class = "btn-secondary", icon = icon("rotate-left"))
      )
    })

    user_settings_dir <- file.path(system.file("shiny-app", "config", package = "lcqc"), "user_settings")
    if (!dir.exists(user_settings_dir)) {
      dir.create(user_settings_dir, recursive = TRUE)
    }

    # observeEvent(input$confirm_save, {
    #   req(input$save_settings_name)
    #   settings_name <- trimws(input$save_settings_name)
    #   if (settings_name == "") {
    #     shinyalert(i18n$t("Error", "Please provide a name for the settings."), type = "error")
    #     return()
    #   }
    #   filename <- paste0(gsub("[^a-zA-Z0-9_\\-]", "_", settings_name), ".json")
    #   filepath <- file.path(user_settings_dir, filename)
    #   current_settings <- get_current_settings(main_input)
    #
    #   tryCatch({
    #     jsonlite::write_json(current_settings, filepath, pretty = TRUE, auto_unbox = TRUE)
    #     shinyalert(i18n$t("Success!"), paste(i18n$t("Settings saved as"), filename), type = "success")
    #     removeModal()
    #   }, error = function(e) {
    #     shinyalert("Error", paste(i18n$t("Failed to save settings:"), e$message), type = "error")
    #   })
    # })
    #
    # saved_settings_files <- reactive({
    #   list.files(path = user_settings_dir, pattern = "\\.json$", full.names = FALSE)
    # })
    #
    # output$load_settings_selection_ui <- renderUI({
    #   files <- saved_settings_files()
    #   if (length(files) > 0) {
    #     selectInput(session$ns("load_settings_file"), i18n$t("Select Settings"), choices = files)
    #   } else {
    #     p(i18n$t("No saved settings found."))
    #   }
    # })
    #
    # observeEvent(input$confirm_load, {
    #   req(input$load_settings_file)
    #   filepath <- file.path(user_settings_dir, input$load_settings_file)
    #   tryCatch({
    #     loaded_config <- jsonlite::read_json(filepath)
    #     update_all_inputs(main_session, loaded_config) # Use main_session here
    #
    #     # Update status bar
    #     settings_name <- tools::file_path_sans_ext(input$load_settings_file)
    #     sync$settings$name <- settings_name
    #
    #     shinyalert(i18n$t("Success!"), i18n$t("Settings loaded successfully."), type = "success")
    #     removeModal()
    #   }, error = function(e) {
    #     shinyalert("Error", paste(i18n$t("Failed to load settings:"), e$message), type = "error")
    #   })
    # })

    observeEvent(input$confirm_import, {
      # Check if a JSON file is selected
      if (is.null(input$import_file)) {
        shinyalert(
          i18n$t("Warning"),
          i18n$t("Please select a JSON file to import."),
          type = "warning"
        )
        return()
      }

      # Check if at least one module is selected
      selected_modules <- input$import_modules
      if (is.null(selected_modules) || length(selected_modules) == 0) {
        shinyalert(
          i18n$t("Warning"),
          i18n$t("Please select at least one module to import settings for."),
          type = "warning"
        )
        return()
      }

      filepath <- input$import_file$datapath
      tryCatch({
        imported_config <- jsonlite::read_json(filepath)

        # Update only selected modules using main_session
        update_selected_inputs(main_session, imported_config, selected_modules, i18n)

        # Update status bar
        settings_name <- tools::file_path_sans_ext(basename(input$import_file$name))
        sync$settings$name <- settings_name

        # Create message showing which modules were imported
        imported_module_names <- sapply(selected_modules, function(m) {
          i18n$t(module_names[[m]])
        })
        import_message <- paste(
          i18n$t("Settings imported successfully for:"),
          paste(imported_module_names, collapse = ", ")
        )

        shinyalert(i18n$t("Success!"), import_message, type = "success",
                   timer = 3000, showConfirmButton = FALSE)
        removeModal()
      }, error = function(e) {
        shinyalert("Error", paste(i18n$t("Failed to import file:"), e$message), type = "error")
      })
    })

    # Load Defaults button observer
    observeEvent(input$load_defaults, {
      # Check if at least one module is selected
      selected_modules <- input$import_modules
      if (is.null(selected_modules) || length(selected_modules) == 0) {
        shinyalert(
          i18n$t("Warning"),
          i18n$t("Please select at least one module to load defaults for."),
          type = "warning"
        )
        return()
      }

      # Path to default settings file
      defaults_filepath <- file.path(system.file("shiny-app", "config", package = "lcqc"), "default_settings.json")

      # Check if defaults file exists
      if (!file.exists(defaults_filepath)) {
        shinyalert(
          i18n$t("Error"),
          i18n$t("Default settings file not found."),
          type = "error"
        )
        return()
      }

      tryCatch({
        default_config <- jsonlite::read_json(defaults_filepath)

        # Update only selected modules using main_session
        update_selected_inputs(main_session, default_config, selected_modules, i18n)

        # Update status bar
        sync$settings$name <- i18n$t("Defaults")

        # Create message showing which modules were reset
        reset_module_names <- sapply(selected_modules, function(m) {
          i18n$t(module_names[[m]])
        })
        reset_message <- paste(
          i18n$t("Default settings loaded for:"),
          paste(reset_module_names, collapse = ", ")
        )

        shinyalert(i18n$t("Success!"), reset_message, type = "success",
                   timer = 3000, showConfirmButton = FALSE)
        removeModal()
      }, error = function(e) {
        shinyalert("Error", paste(i18n$t("Failed to load default settings:"), e$message), type = "error")
      })
    })

    output$confirm_export <- downloadHandler(
      filename = function() {
        req(input$export_filename)
        trimws(input$export_filename)
      },
      content = function(file) {
        current_settings <- get_current_settings(main_input, i18n)
        jsonlite::write_json(current_settings, file, pretty = TRUE, auto_unbox = TRUE)
      }
    )

  })
}
