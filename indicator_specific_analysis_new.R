#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)

library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
# 


not_all_na <- function(x) any(!is.na(x)) # only remove column if all values are NA
not_all_na2 <- function(x) any(!x == "") # only remove column if all values are ""
not_any_na <- function(x) all(!is.na(x)) # remove column if any values are NA

undesired <- c("Observation.status", "Unit.multiplier", "Unit.measure", "GeoCode")





# The code must be able to read the dataset and be reactive to it, it should know how mnay dissagregations the dataset selcted has and then
# in response to that should react and have the same number of dropdown selctions - this cannot be hardcoded

ui <- fluidPage(
  
  
  titlePanel("SDG specific indicator analysis"),
  
  
  sidebarPanel(
    
    fileInput("file1", "Choose csv file",
              multiple = F,
              accept = c(".csv")),
    # selectInput("plot1C1", "First disaggregation", choices = names(indicator_csv)),
    # SelectInput("var2", "Disaggregation choice", choices = indicator_csv$Region),
    uiOutput("Choice1"),
    uiOutput("Choice2"),
    uiOutput("Choice3"),
    dataTableOutput('table')
    
  )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  library(dplyr)
  
  # 
  Indicator <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
    
  }) # Takes the input file -- Please use 3.a.1 from CSV folder as example
  
  
  Indicator1 <- reactive({
    Indicator() %>% 
      select(-one_of(undesired))
  })
  
  # This will allow the choice of the first disaggregtaion
  output$Choice1 <- renderUI({
    req(input$file1)
    selectInput("Choice1", "Select first disaggregation", colnames(Indicator1())) 
  })
  
  #observeEvent(input$Choice2)
  # Allows user to select which value from chosen column - e.g. England from Country
  output$Choice2 <- renderUI({
    req(input$file1)
    selectInput("Choice2", "Select choice for this disaggregation", Indicator1()[,input$Choice1])
  })
  
  # output$Choice3 <- renderUI({
  # 
  #   x <-  Indicator() %>%
  #     filter(get(input$Choice1) == input$Choice2) %>%
  #     select(where(not_all_na2))
  #   selectInput("Choice3", "Select choices",names(x))
  # })
  
  
  test <- reactive({
    df <- Indicator1()
    if(!is.null(input$Choice1) & !is.null(input$Choice2))
    {
      df = df[df[[input$Choice1]] %in% input$Choice2, ]
    }
    df %>%
      select(where(not_all_na2))
      
  })
  
  output$Choice3 <- renderUI({
      
    selectInput("Choice3", "Select choices", choices = test()[,-which(names(test()) == "Value")])
  })
  
  
  
  
  
  
  #  test()[,-which(names(test()) == "Value")])
  # output$table <- renderDataTable({
  #   df <- Indicator1()
  #   if(!is.null(input$Choice1) & !is.null(input$Choice2))
  #   {
  #     df = df[df[[input$Choice1]] %in% input$Choice2, ]
  #   }
  #   df %>%
  #     select(where(not_all_na2))
  #   df  
  # })
  
  
  
  
  
  ## testing of reactive and observe to filter dataframe - not working
  
  # tab <- reactive({
  # filter(Indicator(), input$Choice1 == input$Choice2) %>%
  #     select(where(not_all_na2))
  #  })
  # tab<- observe({
  #   Indicator() %>% filter(input$Choice1== input$Choice2)
  # })
# 
#   output$Choice3 <- renderUI({
#     req(input$file1)
#     test = input$Choice1
#     filter(Indicator(), test == input$Choice2)
# 
#     selectInput("Choice3", "Select second disaggregation", names(Indicator()))
# 
#   })
  
  # first1 <- reactive(input$Choice1)

  
}

# Run the application 
shinyApp(ui = ui, server = server)

