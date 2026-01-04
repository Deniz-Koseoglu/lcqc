# utils/helper.R


safe_return <- function(data, element = NULL) {
  if (is.null(data)) {
    return(NULL)
  }

  if (!is.null(element)) {
    if (is.null(data[[element]])) {
      return(NULL)
    }
    return(data[[element]])
  }

  return(data)
}


check_columns_exist <- function(data, cols, session) {
  missing_cols <- cols[!cols %in% names(data)]
  if (length(missing_cols) > 0) {
    show_error(session, paste("Required columns not found:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }
  return(TRUE)
}

try_create_plot <- function(data, plot_func, session) {
  tryCatch({
    plot_func(data)
  }, error = function(e) {
    show_error(session, paste("Error creating plot:", e$message))
    return(NULL)
  })
}