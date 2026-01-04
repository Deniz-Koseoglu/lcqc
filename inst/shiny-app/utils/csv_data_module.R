# server_modules/csv_data_module.R

source(system.file("shiny-app", "utils", "data_loading.R", package = "lcqc"))
source(system.file("shiny-app", "utils", "helper.R", package = "lcqc")) 

csv_data_module <- function(input, output, session, sync) {

  # CSV File Logic
  csv_data <- reactive({
    req(input$csvFile)
    sync$status_bar$file_name <- input$csvFile$name
    csv_trange <- c(input$csv_trange_lower, input$csv_trange_upper)
    csv_trange <- csv_trange[!is.na(csv_trange)]

    df <- read_data(input$csvFile$datapath, input$csvHeader, input$csvSep, input$csvDecsep, session)

    if (!is.null(df) && length(csv_trange) == 2) {
      time_col <- input$timeColumn
      if (time_col %in% names(df)) {
        df <- df[df[[time_col]] >= csv_trange[1] & df[[time_col]] <= csv_trange[2], ]
      } else {
        show_error(session, "Time column not found in the data for applying time range.")
      }
    }
    return(df)
  })

  observe({
    df <- csv_data()
    if (!is.null(df)) {
      updateSelectInput(session, "timeColumn", choices = names(df), selected = names(df)[1])
      updateSelectInput(session, "intensityColumn", choices = names(df), selected = names(df)[2])
    } else {
      updateSelectInput(session, "timeColumn", choices = NULL, selected = NULL)
      updateSelectInput(session, "intensityColumn", choices = NULL, selected = NULL)
    }
  })

  csv_chromatogram_data <- reactive({
    csv_data()
  })

  list(
    csv_data = csv_data,
    csv_chromatogram_data = csv_chromatogram_data
  )
}