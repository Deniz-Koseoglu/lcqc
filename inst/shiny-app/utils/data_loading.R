# utils/data_loading.R


read_data <- function(file_path, header, sep, dec, session) {
  tryCatch({
    read.csv(file_path, header = header, sep = sep, dec = dec)
  }, error = function(e) {
    show_error(session, paste("Error reading file:", e$message))
    return(NULL)
  })
}


detect_shimadzu_mode <- function(filepath) {

  lines <- tryCatch(
    readLines(filepath, n = 10, warn = FALSE),
    error = function(e) {
      warning("Could not read file head:", e$message)
      return(NULL)
    }
  )

  if (is.null(lines)) {
    return(NULL)
  }

  # Check for patterns and return the corresponding mode
  if (any(grepl("\\.lcd", lines, ignore.case = TRUE))) {
    return("lc")
  } else if (any(grepl("\\.qgd", lines, ignore.case = TRUE))) {
    return("gcms")
  } else if (any(grepl("\\.gcd", lines, ignore.case = TRUE))){
    return("fid")
  }
  # If no specific pattern is found, return NULL to keep the current selection
  return(NULL)
}
