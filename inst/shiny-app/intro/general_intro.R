intro_steps_general <- function(i18n) {
  list(
    steps = list(
      list(
        element = "#navbuts",
        intro = i18n$t("LCQC is designed to pre-process, detect, integrate, model, and calculate domain-specific performance metrics from simple chromatographic data.
                       The application includes modules for data import, baseline correction, smoothing, peak detection, integration, modeling,
                       peak symmetry and performance metrics calculation, and reporting of all data in .PDF format."),
        position = "bottom"
      ),
      list(
        element = "#status_bar_div",
        intro = i18n$t("This field shows the currently imported data (.CSV/.TXT) and/or settings (.JSON) file."),
        position = "bottom"
      ),
      list(
        element = "#export_button",
        intro = i18n$t("This module allows for modular export of all data (currently only in English)."),
        position = "bottom"
      ),
      list(
        element = "#settings_button",
        intro = i18n$t("This module saves or loads processing settings (i.e. methods)."),
        position = "bottom"
      ),
      list(
        element = "#lang_select",
        intro = i18n$t("Use this to select the language of the interface and the .PDF report."),
        position = "bottom"
      ),
      list(
        element = "#main_tabs",
        intro = i18n$t("The functionality of LCQC is encompassed in these 9 tabs."),
        position = "top"
      ),
      list(
        element = "a[data-value='dimp']",
        intro = i18n$t("Use this to import chromatographic data in .CSV or .TXT format."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='bline']",
        intro = i18n$t("This module allows you to apply various baseline correction methods to your chromatographic data."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='smooth']",
        intro = i18n$t("This module is used to apply smoothing methods to refine your chromatogram signal."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='pdet']",
        intro = i18n$t("This module is used for flexible, derivative-based peak detection using the optionally pre-processed chromatographic signal.
                       Accurate peak apices, inflection points, upslope points, and peak boundaries are detected using various algorithms."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='integ']",
        intro = i18n$t("This module allows you to perform peak integration on your chromatographic data."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='dconv']",
        intro = i18n$t("This module allows you to perform peak deconvolution on your chromatographic data."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='psym']",
        intro = i18n$t("This module allows you to calculate various peak symmetry metrics for your chromatographic data."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='perf']",
        intro = i18n$t("This module allows you to calculate various performance metrics for your HPLC column, including theoretical plates,
                       resolution, retention factor, separation factor, and additional column-specific metrics."),
        position = "bottom"
      ),
      list(
        element = "a[data-value='rpr']",
        intro = i18n$t("This module allows you to generate a comprehensive HPLC column performance report in PDF format and several languages."),
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
