intro_steps_current_tab2 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='bline']",
        intro = i18n$t("This module allows you to apply various baseline correction methods to your chromatographic data."),
        position = "bottom"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE
  )
}

intro_steps_tab2_basic <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("baseline_method_div")),
        intro = i18n$t("Select the desired baseline correction method from the dropdown. Choose 'None' to skip baseline correction."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

intro_steps_tab2_advanced_none <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("selected_baseline_method_div")),
        intro = i18n$t("This displays the currently selected baseline correction method from the Basic tab."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_advanced_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

intro_steps_tab2_advanced_als <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("selected_baseline_method_div")),
        intro = i18n$t("This displays the currently selected baseline correction method from the Basic tab."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("als_lambda_div")),
        intro = i18n$t("Adjust the 'Lambda' parameter, which controls the smoothness of the baseline. Higher values result in a smoother baseline."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("als_p_div")),
        intro = i18n$t("Adjust the 'p' parameter, which controls the asymmetry of the weighting. Values closer to 0.001 are suitable for positive peaks, and values closer to 0.999 for negative peaks."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("als_prec_div")),
        intro = i18n$t("Set the 'Precision' (error tolerance) required for the baseline correction algorithm to reach convergence. Smaller values lead to higher precision but longer computation times."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("als_maxit_div")),
        intro = i18n$t("Set the maximum number of iterations for the algorithm to run. Increase this if the baseline is not converging properly."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("als_rm_neg_div")),
        intro = i18n$t("Check this box to remove negative values from the corrected signal, ensuring the baseline does not go above the original signal."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_advanced_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

intro_steps_tab2_advanced_chang <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("selected_baseline_method_div")),
        intro = i18n$t("This displays the currently selected baseline correction method from the Basic tab."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_threshold_div")),
        intro = i18n$t("Adjust the 'Threshold' parameter, which defines the position of the baseline relative to the noise component of the signal (between 0 and 1)."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_alpha_div")),
        intro = i18n$t("Adjust the 'Alpha' parameter, which is a high-pass filter parameter controlling the smoothness of the baseline."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_bfrac_div")),
        intro = i18n$t("Adjust the 'bfrac' parameter, representing the fraction of low-intensity fragments assumed to be part of the baseline."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_segments_div")),
        intro = i18n$t("Set the number of segments to divide the filtered signal into for baseline calculation."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_sig_window_div")),
        intro = i18n$t("Define the signal window size in data points. This influences how local features are considered."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_fit_div")),
        intro = i18n$t("Select the fitting method for the baseline: 'linear' for faster computation or 'cubic' for a more flexible fit."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chang_rm_neg_div")),
        intro = i18n$t("Check this box to remove negative values from the corrected signal, ensuring the baseline does not go above the original signal."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_advanced_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

intro_steps_tab2_advanced_isrea <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("selected_baseline_method_div")),
        intro = i18n$t("This displays the currently selected baseline correction method from the Basic tab."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("isrea_eta_div")),
        intro = i18n$t("Adjust the 'Eta' parameter, which is a convergence criterion. Lower values lead to higher precision but longer computation times."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("isrea_maxit_div")),
        intro = i18n$t("Set the maximum number of iterations for the algorithm to run. Increase this if the baseline is not converging properly."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("isrea_rm_neg_div")),
        intro = i18n$t("Check this box to remove negative values from the corrected signal, ensuring the baseline does not go above the original signal."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_advanced_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}

intro_steps_tab2_advanced_poly <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("selected_baseline_method_div")),
        intro = i18n$t("This displays the currently selected baseline correction method from the Basic tab."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("poly_deg_div")),
        intro = i18n$t("Set the 'Degree' of the polynomial fit. A higher degree allows for more complex baseline shapes but can also lead to overfitting."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("poly_prec_div")),
        intro = i18n$t("Set the 'Precision' (error tolerance) required for the baseline correction algorithm to reach convergence. Smaller values lead to higher precision but longer computation times."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("poly_maxit_div")),
        intro = i18n$t("Set the maximum number of iterations for the algorithm to run. Increase this if the baseline is not converging properly."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("poly_rm_neg_div")),
        intro = i18n$t("Check this box to remove negative values from the corrected signal, ensuring the baseline does not go above the original signal."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("calculate_baseline_advanced_div")),
        intro = i18n$t("Click this button to apply the selected baseline correction method or to skip it if 'None' is chosen."),
        position = "auto"
      )
    ),
    nextLabel = i18n$t("Next"),
    prevLabel = i18n$t("Back"),
    doneLabel = i18n$t("Done"),
    skipLabel = i18n$t("Skip Tour"),
    exitOnOverlayClick = FALSE,
    showProgress = TRUE,
    showBullets = FALSE
  )
}
