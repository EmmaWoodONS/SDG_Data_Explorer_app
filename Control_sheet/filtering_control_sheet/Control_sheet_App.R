#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)


# control_sheet <- read_csv("D:\\Coding_repos\\SDG_Data_Explorer_app\\Control_sheet\\control_sheet.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # control_sheet <- read_csv("D:\\Coding_repos\\SDG_Data_Explorer_app\\Control_sheet\\control_sheet.csv"),
    # Application title

    titlePanel("SDG data explorer app"),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(
        
        uiOutput("Choice1"),
        uiOutput("Choice2"),
        uiOutput("Choice3"),
        uiOutput("Choice4")
        
        # dataTableOutput('table')
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(dplyr)
    
    control_sheet <- read.csv("D:\\Coding_repos\\SDG_Data_Explorer_app\\Control_sheet\\control_sheet_no_NAs.csv")
    
    output$Choice1 <- renderUI({
        selectInput("Choice1", "Characteristic 1", choices = c(unique(control_sheet$variable1)))
        
    })
    
    # make it wait for choice 1
    
    filtered1 <- reactive({
        control_sheet %>%
            filter(variable1 == input$Choice1)
        
    })

    output$Choice2 <- renderUI({
        req(input$Choice1)

    selectInput("Choice2", "Characteristic 2", choices = c(unique(filtered1()$variable2)))

    })


    filtered2 <- reactive ({
        filtered1() %>%
        filter(variable2 == input$Choice2)
    })

    output$Choice3 <- renderUI({
        req(input$Choice2)

        selectInput("Choice3", "Characteristic 3", choices = c(unique(filtered2()$variable3)))

    })
    
    filtered3 <- reactive ({
        filtered2() %>%
            filter(variable3 == input$Choice3)
    })
    
    output$Choice4 <- renderUI({
        req(input$Choice3)
        
        selectInput("Choice4", "Characteristic 4", choices = c(unique(filtered3()$variable4)))
        
    })
    

}



# output$Choice3 <- renderUI({
# 
#     x <-  Indicator() %>%
#         filter(get(input$Choice1) == input$Choice2) %>%
#         select(where(not_all_na2))
#     selectInput("Choice3", "Select choices",names(x))
# })
# 
# # This will allow the choice of the first disaggregtaion
# output$Choice1 <- renderUI({
#     req(input$file1)
#     selectInput("Choice1", "Select first disaggregation", colnames(Indicator1())) 
#     
# })
# 
# #observeEvent(input$Choice2)
# # Allows user to select which value from chosen column - e.g. England from Country
# output$Choice2 <- renderUI({
#     req(input$file1)
#     selectInput("Choice2", "Select choice for this disaggregation", Indicator1()[,input$Choice1])
# })

# Run the application 
shinyApp(ui = ui, server = server)
