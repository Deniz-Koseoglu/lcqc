# utils/lcqc_adapter.R


source(system.file("shiny-app", "utils", "messages.R", package = "lcqc"))


ad_read_shim <- function(file, ptable, simtable, pnames, pcas, metadata, mode, sep, decsep, fix_names, fil_cols, trange, session) {
  tryCatch({
    read_shim(
      file = file,
      ptable = ptable,
      simtable = simtable,
      pnames = pnames,
      pcas = pcas,
      metadata = metadata,
      mode = mode,
      sep = sep,
      decsep = decsep,
      fix_names = fix_names,
      fil_cols = fil_cols,
      trange = trange
    )
  }, error = function(e) {
    show_error(session, paste("Error reading Shimadzu file:", e$message))
    return(NULL)
  })
}

chrom_detect_call <- function(params) {
  # Directly call chrom_detect using do.call
  # This avoids issues with deparse/parse and ensures correct handling of complex R objects
  do.call(chrom_detect, params)
}
