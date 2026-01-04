# server_modules/status_bar_server.R
status_bar_server <- function(id, input, output, session, sync, i18n) {

  moduleServer(
    id,
    function(input, output, session) {
      output$current_file_name <- renderText({
        req(sync$status_bar$file_name) # Ensure reactive value is available
        sync$status_bar$file_name
      })

      output$current_settings_name <- renderText({
        req(sync$settings$name) # Ensure reactive value is available
        sync$settings$name
      })
    }
  )
}
