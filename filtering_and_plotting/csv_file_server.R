# Module server function

csv_file_UI <- function(id, label = "CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}


csv_file_server <- function(id, stringsAsFactors) {
  
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      #csv_file = dataframe in example
      csv_file <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$Select_Indicator != "All", message = FALSE))
        
        indicator_number <- gsub("\\.", "-", input$Select_Indicator)
        csv_filepath <- paste0("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_", indicator_number, ".csv")
        read.csv(csv_filepath,
                 na.strings=c("","NA")) %>%
          mutate_if(is.factor, as.character)
      })
      
      
      
      # # We can run observers in here if we want to
      # observe({
      #   msg <- sprintf("File %s was uploaded", userFile()$name)
      #   cat(msg, "\n")
      # })
      
      # Return the reactive that yields the data frame
      return(csv_file)
    }
  )    
  
}