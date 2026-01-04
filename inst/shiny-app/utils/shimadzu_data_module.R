# server_modules/shimadzu_data_module.R

source(system.file("shiny-app", "utils", "lcqc_adapter.R", package = "lcqc")) # ad_read_shim
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc")) # updateSelectInput 

shimadzu_data_module <- function(input, output, session, sync) {

  # Shimadzu TXT file logic
  shim_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }

    shim_trange <- c(input$shim_trange_lower, input$shim_trange_upper)
    shim_trange <- shim_trange[!is.na(shim_trange)]

    if (length(shim_trange) == 0) {
      shim_trange <- NA
    }

    ad_read_shim(
      file = inFile$datapath,
      ptable = input$ptable,
      simtable = input$simtable,
      pnames = input$pnames,
      pcas = input$pcas,
      metadata = input$metadata,
      mode = input$mode,
      sep = input$sep,
      decsep = input$decsep,
      fix_names = input$fix_names,
      fil_cols = input$fil_cols,
      trange = shim_trange,
      session = session
    )
  })

  observe({
    data <- shim_data()
    chromatogram_name <- input$chromatogramChoice

    if (!is.null(data) && !is.null(data$chromatogram) && !is.null(chromatogram_name) && chromatogram_name %in% names(data$chromatogram)) {
      selected_chromatogram <- data$chromatogram[[chromatogram_name]]
      all_chromatogram_names <- names(selected_chromatogram)

      available_intensity_columns <- setdiff(all_chromatogram_names, input$shimTimeColumn)

      # Sadece boş değilse güncelle
      if (length(all_chromatogram_names) > 0) {
        updateSelectInput(session, "shimTimeColumn", choices = all_chromatogram_names, selected = all_chromatogram_names[1])
      } else {
         updateSelectInput(session, "shimTimeColumn", choices = NULL, selected = NULL)
      }

      if (length(available_intensity_columns) > 0) {
        updateSelectInput(session, "shimIntensityColumn", choices = available_intensity_columns, selected = available_intensity_columns[1])
      } else {
        updateSelectInput(session, "shimIntensityColumn", choices = NULL, selected = NULL)
      }

    } else {
      updateSelectInput(session, "shimTimeColumn", choices = NULL, selected = NULL)
      updateSelectInput(session, "shimIntensityColumn", choices = NULL, selected = NULL)
    }
  })

  observe({
    data <- shim_data()
    if (!is.null(data)) {
      chromatogram_names <- names(data$chromatogram)
      updateSelectInput(session, "chromatogramChoice", choices = chromatogram_names)

      peak_table_names <- names(data$ptable)
      updateSelectInput(session, "peakTableChoice", choices = peak_table_names)
    } else {
      updateSelectInput(session, "chromatogramChoice", choices = NULL)
      updateSelectInput(session, "peakTableChoice", choices = NULL)
    }
  })

  selected_chromatogram_data <- reactive({
    data <- shim_data()
    chromatogram_name <- input$chromatogramChoice

    if (is.null(data) || is.null(chromatogram_name) ||
      !chromatogram_name %in% names(data$chromatogram)) {
      return(NULL)
    }

    data$chromatogram[[chromatogram_name]]
  })

  selected_peak_table_data <- reactive({
    data <- shim_data()
    peak_table_name <- input$peakTableChoice

    if (is.null(data) || is.null(peak_table_name) ||
      !peak_table_name %in% names(data$ptable)) {
      return(NULL)
    }

    data$ptable[[peak_table_name]]
  })

 
  list(
    shim_data = shim_data,
    selected_chromatogram_data = selected_chromatogram_data,
    selected_peak_table_data = selected_peak_table_data
  )
}