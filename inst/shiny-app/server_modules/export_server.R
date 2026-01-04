export_server <- function(id, sync, export_button_input, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_expath <- reactiveVal(getwd()) # Initialize reactive value for export path

    # Handle directory selection for export path
    volumes <- c(Home = normalizePath("~"), shinyFiles::getVolumes()())
    shinyDirChoose(input, "browse_export_path", roots = volumes, session = session)

    observeEvent(input$browse_export_path, {
      path <- parseDirPath(volumes, input$browse_export_path)
      # Ensure path is not 0-length (i.e., user canceled)
      if (!is.null(path) && length(path) > 0) {
        current_expath(path) # Update reactive value
        updateTextInput(session, "export_path", value = path)
      }
    })

    # Update text input when export_path_rv changes
    observe({
      # req(current_expath()) # This req might be problematic if path is not yet set
      updateTextInput(session, "export_path", value = current_expath())
    })


    # Observe the export button click from the main app to show the modal
    observeEvent(export_button_input(), { # Use the passed reactive input
      showModal(export_modal_ui("export", i18n)) # Pass namespaced ID for modal content
    })

    # Reactive expression to determine which data types are available
    available_data <- reactive({
      list(
        pks = !is.null(sync$peak_detection$outputs),
        int = !is.null(sync$integration$outputs),
        icf = !is.null(sync$deconvolution$outputs),
        asym = !is.null(sync$symmetry$outputs),
        tp = !is.null(sync$metrics$tplate_outputs),
        rf = !is.null(sync$metrics$retf_outputs),
        sf = !is.null(sync$metrics$sepf_outputs),
        res = !is.null(sync$metrics$res_outputs),
        cperf = !is.null(sync$metrics$addmets_outputs)
      )
    })

    # Render the dynamic checkbox group


    # Render the dynamic checkbox group
    output$export_data_selection_ui <- renderUI({
      data_status <- available_data()

      # Map display names to the internal keys used in available_data()
      all_choices_map <- list(
      "Peak Detection" = "pks",
        "Integration" = "int",
        "Iterative Curve Fitting" = "icf",
        "Peak Symmetry" = "asym",
        "Theoretical Plates" = "tp",
        "Resolution" = "res",
        "Retention Factors" = "rf",
        "Separation Factors" = "sf",
        "Additional Performance Metrics" = "cperf"
      )

      # Create checkboxes with JavaScript to disable unavailable ones
      checkbox_html <- lapply(names(all_choices_map), function(display_name) {
        key <- all_choices_map[[display_name]] # Get the internal key
        is_available <- data_status[[key]] # Check availability using the internal key

        # Create individual checkbox
        tags$div(
          class = "checkbox",
          tags$label(
            style = if (!is_available) "color: #999; opacity: 0.6;" else "",
            tags$input(
              type = "checkbox",
              name = ns("export_data_selection"),
              # Use the internal key as the value for the input,
              # as this is what's checked in input$export_data_selection later
              value = key,
              disabled = if (!is_available) NA else NULL,
              style = "margin-right: 5px;"
            ),
            i18n$t(display_name), # Display the user-friendly name
            if (!is_available) tags$span(i18n$t(" (not available)"), style = "font-style: italic; color: #999;") else NULL
          )
        )
      })

      tags$div(
        id = ns("export_data_selection"),
        class = "form-group shiny-input-checkboxgroup",
        checkbox_html
      )
    })




    # Observe the confirm export button in the modal
    observeEvent(input$confirm_export, {
      # Debugging: Add a print statement to confirm button click is registered
      print(i18n$t("Export button inside modal clicked!"))

      if (is.null(input$export_data_selection) || length(input$export_data_selection) == 0) {
        showNotification(i18n$t("Please select at least one data type to export."), type = "warning", duration = 5)
        return() # Stop execution if no data type is selected
      }

      selected_data_keys <- input$export_data_selection
      export_list <- list()

      # Map selected keys to sync reactive values
      if ("pks" %in% selected_data_keys) export_list$pks <- sync$peak_detection$outputs
      if ("int" %in% selected_data_keys) export_list$int <- sync$integration$outputs
      if ("icf" %in% selected_data_keys) export_list$icf <- sync$deconvolution$outputs
      if ("asym" %in% selected_data_keys) export_list$asym <- sync$symmetry$outputs
      if ("tp" %in% selected_data_keys) export_list$tp <- sync$metrics$tplate_outputs
      if ("rf" %in% selected_data_keys) export_list$rf <- sync$metrics$retf_outputs
      if ("sf" %in% selected_data_keys) export_list$sf <- sync$metrics$sepf_outputs
      if ("res" %in% selected_data_keys) export_list$res <- sync$metrics$res_outputs
      if ("cperf" %in% selected_data_keys) export_list$cperf <- sync$metrics$addmets_outputs

      if (length(export_list) == 0) {
        showNotification(i18n$t("No valid data selected for export."), type = "warning", duration = 5)
        return()
      }

      # Use the dynamically selected path
      export_path_final <- current_expath()
      # Ensure path is valid and accessible
      if (is.null(export_path_final) || export_path_final == "" || !dir.exists(export_path_final)) {
        showNotification(i18n$t("Please select a valid export directory."), type = "error", duration = NULL)
        return()
      }


      removeModal() # Dismiss modal before starting long operation

      withProgress(message = i18n$t("Exporting Data"), value = 0, {
        incProgress(0.1, detail = i18n$t("Preparing data..."))

        tryCatch(
          {
            export_list_filtered <- export_list[!sapply(export_list, is.null)]

            if (length(export_list_filtered) == 0) {
              # This case should ideally be caught by the earlier check, but good to have.
              showNotification(i18n$t("No valid data for selected types to export."), type = "warning", duration = 5)
              return()
            }

            incProgress(0.5, detail = i18n$t("Running export function..."))


            chrom_export(
              input_list = export_list_filtered,
              expath = export_path_final, # Use the actual path for export
              plot_format = input$plot_format
            )

            incProgress(1, detail = i18n$t("Export complete."))
            showNotification(paste0(i18n$t("Data exported successfully to "), export_path_final), type = "message", duration = 5)
          },
          error = function(e) {
            showNotification(paste(i18n$t("Export failed:"), e$message), type = "error", duration = NULL)
            # Print full error to console for debugging
            print(paste(i18n$t("Export Error:"), e$message))
          }
        )
      })
    })
  })
}
