library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)

# data("mpg", package = "ggplot2")

ui <- fluidPage(
  tags$img(src = "SDG_logo.png",
           align = "left",
           height = 100, width = 300),
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with selectize group"),
      panel(
        useShinyjs(),
        checkboxInput("chk", label = "Hide Table ", value = F
        ),
        uiOutput("Select Indicator"),
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            variable1 = list(inputId = "variable1", title = "characteristic 1:"),
            variable2 = list(inputId = "variable2", title = "characteristic 2:"),
            variable3 = list(inputId = "variable3", title = "characteristic 3:"),
            variable4 = list(inputId = "variable4", title = "characteristic 4:")
          ),
          downloadBttn(
            outputId = "downloadData",
            style = "bordered",
            color = "primary",
            size = "sm",
          ),
        ), status = "primary"
      ),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {
  control_sheet <- read.csv("https://raw.githubusercontent.com/EmmaWoodONS/SDG_Data_Explorer_app/main/Control_sheet/control_sheet.csv") %>% 
    mutate(across(where(is.factor), as.character))
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = control_sheet,
    vars = c("variable1", "variable2", "variable3", "variable4")
  )
  
  
  
  output$`Select Indicator` <- renderUI({
    
      selectInput("Select Indicator", "Select Indicator", choices = c("All", unique(res_mod()$Indicator)))
    
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(res_mod(), con, row.names = FALSE)
    }
  )
  
  observeEvent(input$chk, {
    if (input$chk) hide("table") else show("table")
  })
  
  output$table <- DT::renderDataTable(res_mod(), rownames = FALSE)
}

shinyApp(ui, server)