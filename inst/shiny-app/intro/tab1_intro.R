intro_steps_current_tab1 <- function(i18n) {
  list(
    steps = list(
      list(
        element = "a[data-value='dimp']",
        intro = i18n$t("This module is used for importing chromatographic data either as a simple two-column .CSV file or as part of a vendor-specific ASCII file (e.g. Shimadzu)."),
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

intro_steps_tab1_shimadzu_basic <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("file1_div")),
        intro = i18n$t("Click here to upload your Shimadzu ASCII text file containing chromatography data."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("mode_div")),
        intro = i18n$t("Select the type of chromatogram acquisition mode (e.g., GC-FID, GC-MS, or HPLC) from this dropdown. This helps the application correctly parse your data."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("chromatogramChoice_div")),
        intro = i18n$t("After uploading your file, select the specific chromatogram you wish to analyze from this list. For GC-MS, you might see TIC or SIM chromatograms here."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("peakTableChoice_div")),
        intro = i18n$t("Once a Gc-MS chromatogram is processed, all detected Peak Tables will be listed here for selection."),
        position = "auto"
      ),
      list(
        element = paste0("#", ns("shim_trange_div")),
        intro = i18n$t("Optionally, enter the lower and/or upper retention time limits to focus your analysis on a specific range of the chromatogram. Data outside this range will be excluded."),
        position = "auto"
      ),
       list(
        element = paste0("#", ns("shim_column_div")),
        intro = i18n$t("Select Columns for 'Time' and 'Intensity"),
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

intro_steps_tab1_shimadzu_advanced <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("simtable_div")),
        intro = i18n$t("Check this box to extract the Similarity Table, which is relevant for GC-MS data and provides information on compound identification based on spectral similarity."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("ptable_div")),
        intro = i18n$t("Enable this option to extract the peak table from your Shimadzu file, which contains details about detected peaks like retention time, area, and height."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("pnames_div")),
        intro = i18n$t("Choose whether to retrieve and list peak names (GC-MS only)."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("pcas_div")),
        intro = i18n$t("Choose whether to retrieve CAS numbers for identified peaks."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("metadata_div")),
        intro = i18n$t("Select this to retrieve additional experimental metadata embedded within your Shimadzu file, such as sample names, injection volume, or acquisition parameters."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("separators_div")),
        intro = i18n$t("Select column and decimal separators here. These are normally detected automatically where set to 'auto'."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("fix_names_div")),
        intro = i18n$t("Activate this checkbox to make all column names syntactically correct (for easier plotting and further processing outside of LCQC)."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("fil_cols_div")),
        intro = i18n$t("Removes all analytically unimportant columns from input. Use with caution as the definition of 'unimportant' is subjective."),
        position = "right"
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

intro_steps_tab1_csv_basic <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("csvFile_div")),
        intro = i18n$t("Upload your chromatography data here in CSV format. Ensure your file contains columns for both time and intensity."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("csv_trange_div")),
        intro = i18n$t("Define a retention time range to display or process only a specific portion of your chromatogram. Enter the lower and/or upper limits."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("timeColumn_div")),
        intro = i18n$t("Select the column from your CSV file that represents the time (or retention time) values for your chromatogram."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("intensityColumn_div")),
        intro = i18n$t("Choose the column from your CSV file that contains the intensity (or abundance) values corresponding to your chromatogram signal."),
        position = "right"
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

intro_steps_tab1_csv_advanced <- function(ns, i18n) {
  list(
    steps = list(
      list(
        element = paste0("#", ns("csvSep_div")),
        intro = i18n$t("Specify the character used to separate columns in your CSV file (e.g., comma, semicolon, or tab)."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("csvDecsep_div")),
        intro = i18n$t("Select the character used as the decimal point in your CSV file (e.g., period or comma)."),
        position = "right"
      ),
      list(
        element = paste0("#", ns("csvHeader_div")),
        intro = i18n$t("Check this box if the first row of your CSV file contains column headers. This helps the application correctly identify your data."),
        position = "right"
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
