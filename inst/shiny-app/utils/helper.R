# helper.R
library(jsonlite)
add_annotations_from_table <- function(p, showAnnotations, peak_table_data, chromatogram_data, rtime_col, name_col, time_col, intensity_col, tolerance = 0.01, custom_peakname = "Peaks") {
  if (showAnnotations) {
    if (!is.null(peak_table_data) && nrow(peak_table_data) > 0) {
      if (is.na(name_col)) {
        name_col <- "Name"
        peak_table_data$Name <- ""
      }
      peak_times <- numeric(nrow(peak_table_data))
      peak_intensities <- numeric(nrow(peak_table_data))
      hover_texts <- character(nrow(peak_table_data))
      for (i in seq_len(nrow(peak_table_data))) {
        retention_time <- peak_table_data[[rtime_col]][i]
        peak_name <- as.character(peak_table_data[[name_col]][i])
        if (!is.na(retention_time) && is.numeric(retention_time)) {
          time_diff <- abs(chromatogram_data[[time_col]] - retention_time)
          closest_index <- which.min(time_diff)
          closest_time <- chromatogram_data[[time_col]][closest_index]
          peak_intensity <- chromatogram_data[[intensity_col]][closest_index]
          if (time_diff[closest_index] <= tolerance) {
            peak_times[i] <- closest_time
            peak_intensities[i] <- peak_intensity
            hover_texts[i] <- peak_name
          } else {
            peak_times[i] <- NA
            peak_intensities[i] <- NA
            hover_texts[i] <- ""
          }
        } else {
          peak_times[i] <- NA
          peak_intensities[i] <- NA
          hover_texts[i] <- ""
        }
      }
      p <- p %>% add_trace(
        x = peak_times,
        y = peak_intensities,
        type = "scatter",
        mode = "markers",
        marker = list(color = "red", symbol = "inverted-triangle"),
        text = hover_texts,
        hoverinfo = "text",
        name = custom_peakname
      )
    }
  }
  return(p)
}


safe_return <- function(data, element = NULL) {
  # Get the actual data to check
  actual_data <- if (!is.null(element)) {
    data[[element]]
  } else {
    data
  }

  # Check for NULL
  if (is.null(actual_data)) {
    return(NULL)
  }

  # Check for empty data structures (data.frame, tibble, vector)
  if (is.data.frame(actual_data) || is.list(actual_data)) {
    if (nrow(actual_data) == 0) {
      return(NULL)
    }
  } else if (is.vector(actual_data)) {
    if (length(actual_data) == 0 || all(is.na(actual_data)) || all(actual_data %in% c("NA", "0-00-0"))) {
      return(NULL)
    }
  }

  # If none of the above conditions are met, return the actual data
  return(actual_data)
}




get_num_or_auto <- function(value) {

  if (is.character(value) && value == "auto") {
    return("auto")
  } else {

    numeric_value <- suppressWarnings(as.numeric(value))


    if (!is.na(numeric_value)) {
      return(numeric_value)
    } else {

      return("auto")
    }
  }
}

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
        stop("Invalid range format: ", part)
      }
    } else if (grepl("^\\d+$", part)) {  # Single number
      idx <- as.integer(part)
      if (!is.na(idx)) indices <- c(indices, idx)
    } else {
      stop("Invalid format: '", part, "'. Use numbers and ranges (e.g., 1,2,5:7) or 'all'")
    }
  }

  indices <- sort(unique(indices))

  # Bounds validation
  if (any(indices < 1 | indices > max_peaks)) {
    invalid <- indices[indices < 1 | indices > max_peaks]
    stop("Peak indices out of range (", paste(invalid, collapse=", "),
         "). Valid range: 1-", max_peaks)
  }

  indices
}

merge_translation_jsons <- function(
    dir_path = "./www/i18n/",
    output_filename = "translation.json",
    exclude_files = output_filename,
    special_keys = c("cultural_date_format", "languages"),
    verbose = FALSE # New parameter: to show outputs
) {
    # Ensure dir_path ends with a slash
    if (!endsWith(dir_path, "/")) {
        dir_path <- paste0(dir_path, "/")
    }
    output_filepath <- paste0(dir_path, output_filename)
    if (verbose) {
        print(paste0("merge_translation_jsons is running. Output file: ", output_filepath)) # Debug print
    }

    # 1. Read existing translation.json to extract special keys and current translations
    existing_translations <- list()
    special_key_values <- list()
    if (file.exists(output_filepath)) {
        tryCatch({
            current_main_json <- jsonlite::read_json(output_filepath, simplifyVector = FALSE)
            if (!is.null(current_main_json$translation)) {
                existing_translations <- current_main_json$translation
            }
            for (key in special_keys) {
                if (!is.null(current_main_json[[key]])) {
                    special_key_values[[key]] <- current_main_json[[key]]
                }
            }
            if (verbose) {
                print(paste0("Read ", length(existing_translations), " translation entries from existing ", output_filename, " file.")) # Debug print
            }
        }, error = function(e) {
            message("Warning: Could not read existing ", output_filename, " file. Error: ", e$message)
        })
    }

    # Create a lookup map for existing translations using the English text as key
    # This assumes English text is unique for each translation entry
    existing_lookup <- list()
    for (item in existing_translations) {
        if (!is.null(item$en)) {
            existing_lookup[[item$en]] <- item
        }
    }

    # 2. Discover and read all other JSON files
    all_files <- list.files(dir_path, pattern = "\\.json$", full.names = TRUE)
    files_to_merge <- setdiff(all_files, output_filepath)
    merged_translation_list <- existing_translations
    if (verbose) {
        print(paste0("Number of files to merge: ", length(files_to_merge))) # Debug print
    }

    for (file_path in files_to_merge) {
        if (basename(file_path) %in% exclude_files) {
            next
        }
        if (verbose) {
            print(paste0("Processing file: ", basename(file_path))) # Debug print
        }
        tryCatch({
            json_data <- jsonlite::read_json(file_path, simplifyVector = FALSE)
            if (!is.null(json_data$translation)) {
                for (item in json_data$translation) {
                    if (!is.null(item$en) && is.null(existing_lookup[[item$en]])) {
                        # Only add if the English translation does not already exist in the main file
                        merged_translation_list <- c(merged_translation_list, list(item))
                        existing_lookup[[item$en]] <- item # Add to lookup to prevent future duplicates
                        if (verbose) {
                            print(paste0("New translation added (EN): ", item$en)) # Debug print for new translations
                        }
                    } else if (!is.null(item$en)) {
                        if (verbose) {
                            print(paste0("Translation already exists (EN): ", item$en)) # Debug print for existing translations
                        }
                    }
                }
            }
        }, error = function(e) {
            message("Warning: Could not read or parse ", basename(file_path), ". Error: ", e$message)
        })
    }

    # 3. Reconstruct the final JSON with preserved special keys and merged translations
    final_json_content <- list(translation = merged_translation_list)
    for (key in names(special_key_values)) {
        final_json_content[[key]] <- special_key_values[[key]]
    }

    # Reorder elements to ensure special keys are at the top if they were originally
    ordered_final_json_content <- list()
    for (key in special_keys) {
        if (!is.null(final_json_content[[key]])) {
            ordered_final_json_content[[key]] <- final_json_content[[key]]
        }
    }
    ordered_final_json_content[["translation"]] <- final_json_content[["translation"]]
    for (key in names(final_json_content)) {
        if (!(key %in% names(ordered_final_json_content))) {
            ordered_final_json_content[[key]] <- final_json_content[[key]]
        }
    }

    # 4. Write the result back to translation.json
    tryCatch({
        jsonlite::write_json(ordered_final_json_content, output_filepath, pretty = TRUE, auto_unbox = TRUE)
        message("Translation files successfully merged into ", output_filename, ".")
        if (verbose) {
            print(paste0("Total number of translation entries: ", length(ordered_final_json_content$translation))) # Debug print final count
        }
    }, error = function(e) {
        message("Error: Could not write to ", output_filename, " file. Error: ", e$message)
    })
}

#FUNCTION TO CUSTOMIZE THE COPY BUTTON OF THE BUTTONS EXTENSION (DT PACKAGE)
copy_button_no_popup <- function(copy_label = "Copy", copied_label = "Copied!") {
  JS(sprintf("
      function (e, dt, node, config) {
        // Temporarily disable the info popup
        var oldInfo = dt.buttons.info;
        dt.buttons.info = function() {};

        // Perform copy without popup
        $.fn.dataTable.ext.buttons.copyHtml5.action.call(this, e, dt, node, config);

        // Restore the original info function
        dt.buttons.info = oldInfo;

        // Change button text to a green check mark
        $(node).html('<span style=\\\"color:green;\\\">✔ %s</span>');

        // Revert after 2 seconds
        setTimeout(function(){
          $(node).html('%s');
        }, 2000);
      }
    ",
             copied_label, copy_label
  ))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION TO TOGGLE UNUSED OUTPUT TABS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
toggle_tab_enabled <- function(
    session,
    tabset_id,
    tab_value,
    enabled,
    fallback_tab = NULL
) {
  tab_selector <- sprintf("a[data-value='%s']", tab_value)

  if (isTRUE(enabled)) {

    # Enable the tab
    shinyjs::enable(selector = tab_selector)

  } else {

    # If currently active, move away first
    if (!is.null(fallback_tab) &&
        identical(session$input[[tabset_id]], tab_value)) {

      updateTabsetPanel(
        session,
        inputId = tabset_id,
        selected = fallback_tab
      )
    }

    # Disable the tab
    shinyjs::disable(selector = tab_selector)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FUNCTION: ShinyJs disableTab extension
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
distabs <- function() {
  jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('tab-disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('tab-disabled');
}
"

# .nav li a.disabled {
#   background-color: #eee !important;
#     opacity: 0.2;
# };

css <- "
.tab-disabled {
  opacity: 0.5;
  cursor: not-allowed;
}
"
return(list(jscode = jscode, css = css))
}
