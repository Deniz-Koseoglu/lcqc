#' @title Translate information strings dynamically
#' @description Helper function to translate information strings from lcqc functions
#' while preserving dynamic numeric values. Method names are passed directly from
#' Shiny input objects rather than being extracted from text.
#' @param info_text The original information text string
#' @param i18n The i18n translation object
#' @param methods Named list of method selections from Shiny inputs (optional)
#'        Example: list(symmetry = c("USP Tailing Factor (Tf)", "Asymmetry Factor (As)"),
#'                      tplate = c("Full Width (FW)", "European Pharmacopoeia (EP)"))
#' @return Translated HTML content
#' @keywords internal

translate_info <- function(info_text, i18n, methods = NULL) {

  if (is.null(info_text) || info_text == "") return("")


  # Split into lines
  lines <- strsplit(info_text, "\n")[[1]]
  
  translated_lines <- sapply(lines, function(line) {
    translate_info_line(line, i18n, methods)
  }, USE.NAMES = FALSE)
  
  # Return as HTML paragraphs
  paste0("<p>", translated_lines, "</p>", collapse = "")
}

#' @title Translate a single information line
#' @description Translates individual lines from information strings
#' @param line Single line of text
#' @param i18n The i18n translation object
#' @param methods Named list of method selections from Shiny inputs
#' @return Translated line
#' @keywords internal

translate_info_line <- function(line, i18n, methods = NULL) {
  if (is.null(line) || trimws(line) == "") return("")
 
  # ============================================================
  # CHROM_DETECT / PEAKFIND patterns
  # ============================================================
  
  # "The chromatogram was truncated between RT thresholds: X, Y."
  if (grepl("^The chromatogram was truncated between RT thresholds:", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("The chromatogram was truncated between RT thresholds: %s, %s."), nums[1], nums[2]))
  }
  
  # "An average inflection point width of X points was calculated..."
  if (grepl("^An average inflection point width of", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("An average inflection point width of %s points was calculated and later used for width-based peak filtering."), nums[1]))
  }
  
  # "Initial background correction was done via method: X ('Y')."
  if (grepl("^Initial background correction was done via method:", line)) {
    if (!is.null(methods$baseline) && length(methods$baseline) > 0) {
      method_name <- get_method_display_string(methods$baseline, i18n)
    } else {
      method_name <- extract_method_name_before_paren(line)
    }
    return(sprintf(i18n$t("Initial background correction was done via method: %s."), i18n$t(method_name)))
  }
  
  # "No initial baseline correction was applied."
  if (grepl("^No initial baseline correction was applied", line)) {
    return(i18n$t("No initial baseline correction was applied."))
  }
  
  # "The chromatogram signal was smoothed using method: X ('Y') with Z points and W passes (auto-estimated)."
  if (grepl("^The chromatogram signal was smoothed using method:", line)) {
    nums <- extract_numbers(line)
    auto_suffix <- if (grepl("auto-estimated", line)) i18n$t("(auto-estimated)") else ""
    if (!is.null(methods$smooth_signal) && length(methods$smooth_signal) > 0) {
      method_name <- get_method_display_string(methods$smooth_signal, i18n)
    } else {
      method_name <- extract_method_name_before_paren(line)
    }
    return(sprintf(i18n$t("The chromatogram signal was smoothed using method: %s with %s points and %s passes."),
                   i18n$t(method_name), nums[1], nums[2]))
  }
  
  # "The chromatogram signal was not smoothed."
  if (grepl("^The chromatogram signal was not smoothed", line)) {
    return(i18n$t("The chromatogram signal was not smoothed."))
  }
  
  # "First and second derivative signals were smoothed using method: X ('Y') with Z points and W passes"
  if (grepl("^First and second derivative signals were smoothed using method:", line)) {
    nums <- extract_numbers(line)
    auto_suffix <- if (grepl("auto-estimated", line)) i18n$t("(auto-estimated)") else ""
    if (!is.null(methods$smooth_deriv) && length(methods$smooth_deriv) > 0) {
      method_name <- get_method_display_string(methods$smooth_deriv, i18n)
    } else {
      method_name <- extract_method_name_before_paren(line)
    }
    return(sprintf(i18n$t("First and second derivative signals were smoothed using method: %s with %s points and %s passes."),
                   i18n$t(method_name), nums[1], nums[2]))
  }
  
  # "First and second derivatives were not smoothed."
  if (grepl("^First and second derivatives were not smoothed", line)) {
    return(i18n$t("First and second derivatives were not smoothed."))
  }
  
  # "A signal amplitude threshold was set at X manually."
  if (grepl("^A signal amplitude threshold was set at .* manually", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("A signal amplitude threshold was set at %s manually."), nums[1]))
  }
  
  # "A signal amplitude threshold of X was determined using Y ('Z')."
  if (grepl("^A signal amplitude threshold of .* was determined using", line)) {
    nums <- extract_numbers(line)
    if (!is.null(methods$amp_thres) && length(methods$amp_thres) > 0) {
      methods_str <- get_method_display_string(methods$amp_thres, i18n)
    } else {
      # Fallback: extract codes and translate
      codes <- extract_all_method_codes(line)
      methods_str <- paste(codes, collapse = i18n$t(", and "))
    }
    return(sprintf(i18n$t("A signal amplitude threshold of %s was determined using %s."), nums[1], methods_str))
  }
  
  # "Inferior and superior first- (X, Y) and second- (Z, W) derivative thresholds were determined via..."
  if (grepl("^Inferior and superior first-", line)) {
    nums <- extract_numbers(line)
    if (!is.null(methods$der_thres) && length(methods$der_thres) > 0) {
      method_name <- get_method_display_string(methods$der_thres, i18n)
    } else {
      method_name <- extract_method_name_via(line)
    }
    return(sprintf(i18n$t("Inferior and superior first- (%s, %s) and second- (%s, %s) derivative thresholds were determined via the %s method."),
                   nums[1], nums[2], nums[3], nums[4], i18n$t(method_name)))
  }
  
  # "Outliers for this procedure were handled via X ('Y') method(s)."
  if (grepl("^Outliers for this procedure were handled via", line)) {
    if (!is.null(methods$outlier) && length(methods$outlier) > 0) {
      method_name <- get_method_display_string(methods$outlier, i18n)
    } else {
      method_name <- extract_method_name_via(line)
    }
    return(sprintf(i18n$t("Outliers for this procedure were handled via %s method(s)."), i18n$t(method_name)))
  }
  
  # "Outlier handling for this procedure was not carried out."
  if (grepl("^Outlier handling for this procedure was not carried out", line)) {
    return(i18n$t("Outlier handling for this procedure was not carried out."))
  }
  
  # "FastChrom baseline correction was carried out with a critical width of X."
  if (grepl("^FastChrom baseline correction was carried out", line)) {
    nums <- extract_numbers(line)
    # Remove any trailing periods from the number (in case of "1." being extracted)
    crit_width <- sub("\\.$", "", nums[1])
    return(sprintf(i18n$t("FastChrom baseline correction was carried out with a critical width of %s."), crit_width))
  }
  
  # "Following peak picking and removal, a total of X peaks were detected:"
  if (grepl("^Following peak picking and removal", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("Following peak picking and removal, a total of %s peaks were detected:"), nums[1]))
  }
  
  # "X baseline-resolved peaks."
  if (grepl("baseline-resolved peaks\\.$", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("%s baseline-resolved peaks."), nums[1]))
  }
  
  # "X fused peaks."
  if (grepl("fused peaks\\.$", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("%s fused peaks."), nums[1]))
  }
  
  # "X shoulder peaks."
  if (grepl("shoulder peaks\\.$", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("%s shoulder peaks."), nums[1]))
  }
  
  # "X round peaks."
  if (grepl("round peaks\\.$", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("%s round peaks."), nums[1]))
  }
  
  # ============================================================
  # CHROM_ASYM patterns
  # ============================================================
  
  # "Asymmetry metric calculation was attempted for X out of Y peaks."
  if (grepl("^Asymmetry metric calculation was attempted for", line)) {
    nums <- extract_numbers(line)
    if (grepl("all of", line)) {
      return(sprintf(i18n$t("Asymmetry metric calculation was attempted for all of %s peaks."), nums[1]))
    } else {
      return(sprintf(i18n$t("Asymmetry metric calculation was attempted for %s out of %s peaks."), nums[1], nums[2]))
    }
  }
  
  # "The following methods were used: X ('Y'), Z ('W')."
  if (grepl("^The following methods were used:", line)) {
    # Determine which method type based on available methods
    if (!is.null(methods$symmetry) && length(methods$symmetry) > 0) {
      methods_str <- get_method_display_string(methods$symmetry, i18n, for_sentence = "methods_used")
    } else if (!is.null(methods$tplate) && length(methods$tplate) > 0) {
      methods_str <- get_method_display_string(methods$tplate, i18n, for_sentence = "methods_used")
    } else if (!is.null(methods$resolution) && length(methods$resolution) > 0) {
      methods_str <- get_method_display_string(methods$resolution, i18n, for_sentence = "methods_used")
    } else if (!is.null(methods$icf) && length(methods$icf) > 0) {
      methods_str <- get_method_display_string(methods$icf, i18n, for_sentence = "methods_used")
    } else {
      # Fallback: just extract codes
      codes <- extract_all_method_codes(line)
      methods_str <- paste(codes, collapse = ", ")
    }
    return(sprintf(i18n$t("The following methods were used: %s."), methods_str))
  }
  
  # "Total Peak Analysis (TPA) was carried out on X baseline-resolved peaks out of Y total peaks (or Z baseline-resolved peaks)."
  if (grepl("^Total Peak Analysis \\(TPA\\) was carried out on", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("Total Peak Analysis (TPA) was carried out on %s baseline-resolved peaks out of %s total peaks (or %s baseline-resolved peaks)."),
                   nums[1], nums[2], nums[3]))
  }
  
  # ============================================================
  # CHROM_SKIM / CHROM_ICF patterns
  # ============================================================
  
  # "Baselines for X resolved and Y fused peaks spread across Z groups were determined via the FastChrom algorithm."
  if (grepl("^Baselines for .* resolved and .* fused peaks spread across", line)) {
    nums <- extract_numbers(line)
    return(sprintf(i18n$t("Baselines for %s resolved and %s fused peaks spread across %s groups were determined via the FastChrom algorithm."),
                   nums[1], nums[2], nums[3]))
  }
  
  # "X was selected as the peak skimming method." (for chrom_skim)
  if (grepl("was selected as the peak skimming method\\.$", line)) {
    if (!is.null(methods$skim) && length(methods$skim) > 0) {
      method_name <- get_method_display_string(methods$skim, i18n)
    } else {
      method_name <- sub(" was selected as the peak skimming method\\.$", "", line)
    }
    return(sprintf(i18n$t("%s was selected as the peak skimming method."), i18n$t(method_name)))
  }
  
  # "The following Non-Linear Least-Squares Curve Fitting model(s) was/were applied: X ('Y')."
  if (grepl("^The following Non-Linear Least-Squares Curve Fitting model", line)) {
    if (!is.null(methods$icf) && length(methods$icf) > 0) {
      methods_str <- get_method_display_string(methods$icf, i18n, for_sentence = "models_applied")
    } else {
      codes <- extract_all_method_codes(line)
      methods_str <- paste(codes, collapse = ", ")
    }
    return(sprintf(i18n$t("The following Non-Linear Least-Squares Curve Fitting model(s) was/were applied: %s."), methods_str))
  }
  
  # "Baseline-resolved peak modeling was carried out." / "Baseline-resolved peak modeling was NOT carried out."
  if (grepl("^Baseline-resolved peak modeling was", line)) {
    if (grepl("NOT", line)) {
      return(i18n$t("Baseline-resolved peak modeling was NOT carried out."))
    } else {
      return(i18n$t("Baseline-resolved peak modeling was carried out."))
    }
  }
  
  # "A critical width equal to X was used for baseline adjustment in the peak regions."
  if (grepl("^A critical width equal to", line)) {
    if (grepl("smallest inflection point distance", line)) {
      return(i18n$t("A critical width equal to the smallest inflection point distance was used for baseline adjustment in the peak regions."))
    } else {
      nums <- extract_numbers(line)
      return(sprintf(i18n$t("A critical width equal to %s was used for baseline adjustment in the peak regions."), nums[1]))
    }
  }
  
  # "The Skim-Valley Ratio ('skim') and Dyson (Parent-to-Child Peak Ratio; 'dyson') criteria were set at X and Y, respectively."
  if (grepl("^The Skim-Valley Ratio", line)) {
    nums <- extract_numbers(line)
    # Remove code references in parentheses for cleaner output
    return(sprintf(i18n$t("The Skim-Valley Ratio and Dyson (Parent-to-Child Peak Ratio) criteria were set at %s and %s, respectively."),
                   nums[1], nums[2]))
  }
  
  # "Peaks were integrated using the Trapezoidal Rule."
  if (grepl("^Peaks were integrated using the Trapezoidal Rule", line)) {
    return(i18n$t("Peaks were integrated using the Trapezoidal Rule."))
  }
  
  # ============================================================
  # CHROM_TPLATE patterns
  # ============================================================
  
  # "Theoretical Plate number calculation was attempted for X out of Y peaks."
  if (grepl("^Theoretical Plate number calculation was attempted for", line)) {
    nums <- extract_numbers(line)
    if (grepl("all of", line)) {
      return(sprintf(i18n$t("Theoretical Plate number calculation was attempted for all of %s peaks."), nums[1]))
    } else {
      return(sprintf(i18n$t("Theoretical Plate number calculation was attempted for %s out of %s peaks."), nums[1], nums[2]))
    }
  }
  
  # "Dead time was equal to the retention time of peak X (Y min)." / "Dead time was equal to X min."
  if (grepl("^Dead time was equal to", line)) {
    if (grepl("retention time of peak", line)) {
      nums <- extract_numbers(line)
      return(sprintf(i18n$t("Dead time was equal to the retention time of peak %s (%s %s)."), 
                     nums[1], nums[2], i18n$t("min")))
    } else if (grepl("not determined", line)) {
      return(i18n$t("Dead time was not determined/provided."))
    } else {
      nums <- extract_numbers(line)
      return(sprintf(i18n$t("Dead time was equal to %s %s."), nums[1], i18n$t("min")))
    }
  }
  
  # "The following additional parameters were provided: X (Y unit), Z (W unit)."
  # May also have " Peak areas were also provided." appended
  if (grepl("^The following additional parameters were provided:", line)) {
    # Check if "Peak areas were also provided" is appended
    has_peak_areas <- grepl("Peak areas were also provided", line)
    
    # Extract just the parameters part
    params_line <- line
    if (has_peak_areas) {
      params_line <- sub("\\.?\\s*Peak areas were also provided\\.?", "", line)
    }
    
    params_str <- sub("^The following additional parameters were provided: ", "", params_line)
    params_str <- sub("\\.$", "", params_str)
    params <- strsplit(params_str, ", ")[[1]]
    
    translated_params <- sapply(params, function(p) {
      match <- regmatches(p, regexec("^([^(]+) \\(([0-9.]+) ([^)]+)\\)$", p))[[1]]
      if (length(match) == 4) {
        # Translate both parameter name and unit
        sprintf("%s (%s %s)", i18n$t(trimws(match[2])), match[3], i18n$t(trimws(match[4])))
      } else {
        # Try simpler pattern without value
        match2 <- regmatches(p, regexec("^([^(]+) \\(([^)]+)\\)$", p))[[1]]
        if (length(match2) == 3) {
          sprintf("%s (%s)", i18n$t(trimws(match2[2])), i18n$t(trimws(match2[3])))
        } else {
          p
        }
      }
    })
    
    result <- sprintf(i18n$t("The following additional parameters were provided: %s."),
                   paste(translated_params, collapse = ", "))
    
    # Append translated "Peak areas were also provided" if it was in original
    if (has_peak_areas) {
      result <- paste(result, i18n$t("Peak areas were also provided."))
    }
    
    return(result)
  }
  
  # "Peak areas were also provided." - can appear alone or appended to parameter line
  if (grepl("Peak areas were also provided", line)) {
    # Check if this is standalone or part of a larger line
    if (trimws(line) == "Peak areas were also provided.") {
      return(i18n$t("Peak areas were also provided."))
    }
    # If it's part of another line (e.g., after parameters), it will be handled
    # by the parameter parsing below
  }
  
  # "Height Equivalents to a Theoretical Plate (HETP) and reduced plate heights (h) were calculated."
  if (grepl("^Height Equivalents to a Theoretical Plate", line)) {
    return(i18n$t("Height Equivalents to a Theoretical Plate (HETP) and reduced plate heights (h) were calculated."))
  }
  
  # "Separation Impedance (E) was calculated."
  if (grepl("^Separation Impedance \\(E\\) was calculated", line)) {
    return(i18n$t("Separation Impedance (E) was calculated."))
  }
  
  # ============================================================
  # CHROM_RES patterns
  # ============================================================
  
  # "Resolution was calculated via the X ('Y') methods."
  if (grepl("^Resolution was calculated via the", line)) {
    if (!is.null(methods$resolution) && length(methods$resolution) > 0) {
      # Check if "All" is selected
      all_patterns <- c("All", "All Available", "all", "all available")
      has_all <- any(tolower(methods$resolution) %in% tolower(all_patterns))
      
      if (has_all) {
        # Use sentence without "the" for grammatical correctness
        return(i18n$t("Resolution was calculated via all available methods."))
      } else {
        methods_str <- get_method_display_string(methods$resolution, i18n)
        return(sprintf(i18n$t("Resolution was calculated via the %s methods."), methods_str))
      }
    } else {
      codes <- extract_all_method_codes(line)
      methods_str <- paste(codes, collapse = ", ")
      return(sprintf(i18n$t("Resolution was calculated via the %s methods."), methods_str))
    }
  }
  
  # ============================================================
  # CHROM_RETF patterns
  # ============================================================
  
  # "Retention factors (k) were calculated for X out of Y peaks (1 peak used for t0 estimation)."
  if (grepl("^Retention factors \\(k\\) were calculated for", line)) {
    nums <- extract_numbers(line)
    if (grepl("1 peak used for t0 estimation", line)) {
      return(sprintf(i18n$t("Retention factors (k) were calculated for %s out of %s peaks (1 peak used for t0 estimation)."),
                     nums[1], nums[2]))
    } else {
      return(sprintf(i18n$t("Retention factors (k) were calculated for %s out of %s peaks."), nums[1], nums[2]))
    }
  }
  
  # ============================================================
  # CHROM_ADDMETS patterns
  # ============================================================
  
  # "The following parameters were provided for column performance calculations: X (Y unit), Z (W unit)."
  if (grepl("^The following parameters were provided for column performance calculations:", line)) {
    params_str <- sub("^The following parameters were provided for column performance calculations: ", "", line)
    params_str <- sub("\\.$", "", params_str)
    params <- strsplit(params_str, ", ")[[1]]
    
    translated_params <- sapply(params, function(p) {
      # Pattern: "Parameter Name (value unit)"
      match <- regmatches(p, regexec("^([^(]+) \\(([0-9.]+) ([^)]+)\\)$", p))[[1]]
      if (length(match) == 4) {
        sprintf("%s (%s %s)", i18n$t(trimws(match[2])), match[3], i18n$t(trimws(match[4])))
      } else {
        # Try pattern without numeric value
        match2 <- regmatches(p, regexec("^([^(]+) \\(([^)]+)\\)$", p))[[1]]
        if (length(match2) == 3) {
          sprintf("%s (%s)", i18n$t(trimws(match2[2])), i18n$t(trimws(match2[3])))
        } else {
          p
        }
      }
    })
    
    return(sprintf(i18n$t("The following parameters were provided for column performance calculations: %s."),
                   paste(translated_params, collapse = ", ")))
  }
  
  # ============================================================
  # CHROM_VISC patterns
  # ============================================================
  
  # "Dynamic viscosity was calculated for the single component/mixture: X ('Y'), Z ('W') at T degC."
  if (grepl("^Dynamic viscosity was calculated for the", line)) {
    temp <- extract_numbers(line)
    temp <- temp[length(temp)] # Last number is temperature
    
    if (grepl("single component", line)) {
      if (!is.null(methods$visc_solvents) && length(methods$visc_solvents) > 0) {
        comp_name <- i18n$t(methods$visc_solvents[1])
      } else {
        comp_name <- extract_component_name(line)
      }
      return(sprintf(i18n$t("Dynamic viscosity was calculated for the single component: %s at %s degC."),
                     comp_name, temp))
    } else {
      if (!is.null(methods$visc_solvents) && length(methods$visc_solvents) > 0) {
        comp_translated <- paste(sapply(methods$visc_solvents, function(c) i18n$t(c)), collapse = ", ")
      } else {
        codes <- extract_all_method_codes(line)
        comp_translated <- paste(codes, collapse = ", ")
      }
      return(sprintf(i18n$t("Dynamic viscosity was calculated for the mixture: %s at %s degC."),
                     comp_translated, temp))
    }
  }
  
  # "Viscosities of pure components were calculated via the Vogel equation."
  if (grepl("^Viscosities of pure components were calculated via the Vogel equation", line)) {
    return(i18n$t("Viscosities of pure components were calculated via the Vogel equation."))
  }
  
  # "Densities were similarly calculated via the DIPPR105 equation."
  if (grepl("^Densities were similarly calculated via the DIPPR105 equation", line)) {
    return(i18n$t("Densities were similarly calculated via the DIPPR105 equation."))
  }
  
  # "Finally, the Linear Blend Rule was used to obtain mixture viscosity."
  if (grepl("^Finally, the Linear Blend Rule was used to obtain mixture viscosity", line)) {
    return(i18n$t("Finally, the Linear Blend Rule was used to obtain mixture viscosity."))
  }
  
  # "Volume/Mass/Mol fraction(s) of X, Y was/were specified for Z (respectively)."
  if (grepl("fraction\\(s\\) of .* was/were specified for", line)) {
    frac_type <- if (grepl("^Volume", line)) "Volume" else if (grepl("^Mass", line)) "Mass" else "Mol"
    
    # Extract fractions (numbers between "of" and "was/were")
    frac_part <- sub(".*fraction\\(s\\) of\\s+", "", line)
    frac_part <- sub("\\s+was/were.*", "", frac_part)
    fracs <- strsplit(frac_part, ",\\s*")[[1]]
    
    # Get component names from input or extract from line
    if (!is.null(methods$visc_solvents) && length(methods$visc_solvents) > 0) {
      comp_names <- paste(sapply(methods$visc_solvents, function(c) i18n$t(c)), collapse = ", ")
      num_components <- length(methods$visc_solvents)
    } else {
      comp_part <- sub(".*specified for\\s+", "", line)
      comp_part <- sub("\\s*\\(respectively\\).*", "", comp_part)
      comp_part <- sub("\\.$", "", comp_part)
      comp_names <- comp_part
      num_components <- length(strsplit(comp_names, ",\\s*")[[1]])
    }
    
    # Only add "respectively" if more than one component
    if (num_components > 1) {
      return(sprintf(i18n$t("%s fraction(s) of %s was/were specified for %s, respectively."),
                     i18n$t(frac_type), paste(fracs, collapse = ", "), comp_names))
    } else {
      return(sprintf(i18n$t("%s fraction of %s was specified for %s."),
                     i18n$t(frac_type), paste(fracs, collapse = ", "), comp_names))
    }
  }
  
  # ============================================================
  # Fallback: return original line if no pattern matches
  # ============================================================
  return(line)
}

# ============================================================
# Helper extraction functions (simplified - just extract codes/numbers)
# ============================================================

#' Extract all numbers from a string
extract_numbers <- function(text) {
  nums <- regmatches(text, gregexpr("-?[0-9]+\\.?[0-9]*", text))[[1]]
  return(nums)
}

#' Extract method code from parentheses like ('code')
extract_method_code <- function(text) {
  match <- regmatches(text, regexec("\\('([^']+)'\\)", text))[[1]]
  if (length(match) >= 2) {
    return(match[2])
  }
  return("")
}

#' Extract all method codes from a line (for lines with multiple methods)
extract_all_method_codes <- function(text) {
  matches <- gregexpr("\\('([^']+)'\\)", text)
  matched_strings <- regmatches(text, matches)[[1]]
  
  sapply(matched_strings, function(m) {
    code <- sub("\\('", "", m)
    code <- sub("'\\)", "", code)
    code
  }, USE.NAMES = FALSE)
}

#' Extract method name that appears before ('code') pattern after "method:"
extract_method_name_before_paren <- function(text) {
  # Match text between "method: " and " ('"
  match <- regmatches(text, regexec("method:\\s*([^(]+)\\s*\\('", text))[[1]]
  if (length(match) >= 2) {
    return(trimws(match[2]))
  }
  return("")
}

#' Extract method name that appears after "via" or "via the"
extract_method_name_via <- function(text) {
  match <- regmatches(text, regexec("via\\s+(?:the\\s+)?([^(]+)\\s*\\('", text))[[1]]
  if (length(match) >= 2) {
    return(trimws(match[2]))
  }
  return("")
}

#' Extract component name for viscosity (single component)
extract_component_name <- function(text) {
  match <- regmatches(text, regexec("component:\\s*([^(]+)\\s*\\('", text))[[1]]
  if (length(match) >= 2) {
    return(trimws(match[2]))
  }
  return("")
}

# ============================================================
# Helper function to get method display names from Shiny selectInput
# ============================================================

#' Get selected method names from a selectInput choices reactive
#' @param input_value The current value(s) from input$selectInput (the codes)
#' @param choices_reactive The reactive that returns the named choices vector
#' @return Character vector of display names for the selected values
#' @examples
#' # In server:
#' # selected_names <- get_selected_names(input$symmetry_method, symm_choices())
get_selected_names <- function(input_value, choices_reactive) {
  if (is.null(input_value) || length(input_value) == 0) return(character(0))
  
  # choices_reactive is a named vector where names = display names, values = codes
  # We need to reverse lookup: find names where values match input_value
  selected_names <- names(choices_reactive)[choices_reactive %in% input_value]
  return(selected_names)
}

#' Format method names for display in sentences
#' @param method_names Character vector of method display names
#' @param i18n The i18n translation object
#' @param for_sentence Type of sentence context for handling "All" cases
#' @return Formatted string for insertion into translated sentence
get_method_display_string <- function(method_names, i18n, for_sentence = NULL) {
  if (is.null(method_names) || length(method_names) == 0) return("")
  
  # Check for "All" or "All Available" selections
  all_patterns <- c("All", "All Available", "all", "all available",
                    i18n$t("All"), i18n$t("All Available"))
  
  has_all <- any(tolower(method_names) %in% tolower(all_patterns))
  
  if (has_all) {
    # Return appropriate phrase based on context
    if (!is.null(for_sentence)) {
      if (for_sentence == "methods_used") {
        return(i18n$t("all available methods"))
      } else if (for_sentence == "models_applied") {
        return(i18n$t("all available models"))
      } else if (for_sentence == "resolution") {
        return(i18n$t("all available"))
      }
    }
    return(i18n$t("all available methods"))
  }
  
  # Translate each method name and join with comma
  translated <- sapply(method_names, function(m) i18n$t(m), USE.NAMES = FALSE)
  paste(translated, collapse = ", ")
}
