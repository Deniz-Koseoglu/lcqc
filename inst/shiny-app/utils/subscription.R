 subscription_check_active <- TRUE  
  user_groups <- Sys.getenv("SHINYPROXY_USERGROUPS")

  subscription_check_active <- function(){
    return(FALSE)
  }

   # Demo file handling
  demo_data <- reactive({
    req(input$demoFile)
   
    if (input$demoFile == "demo1") {
      filepath <- "data/demo1.txt" 
      # Assuming demo1.txt is a Shimadzu TXT file
      read_shimadzu(filepath, metadata = TRUE, ptable = TRUE, simtable = TRUE, pnames = TRUE, pcas = TRUE)
      #read.table
      }
      })