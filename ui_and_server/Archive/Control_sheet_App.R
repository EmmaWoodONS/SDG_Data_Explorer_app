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
        conditionalPanel(
            condition = "input.Choice2" != "All",
            uiOutput("Choice3")
        ),
        # uiOutput("Choice3"),
        uiOutput("Choice4"), 
        uiOutput("Select Indicator"),
        downloadButton('download',"Download the data")
    ),
    mainPanel(
        fluidRow(column(7,dataTableOutput('dto')))
    )
    
    
        
        
        # dataTableOutput('table')
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

    selectInput("Choice2", "Characteristic 2", choices = c("All", unique(filtered1()$variable2)))

    })


    filtered2 <- reactive ({
        
        if (input$Choice2 == "All") {
            filtered1()
        } else {
            filtered1() %>%
                filter(variable2 == input$Choice2)
        }

    })
    
    output$Choice3 <- renderUI({
      req(input$Choice2)
      
      if(input$Choice2 == "All") {
        
        NULL
        # selectInput("Choice3", "Characterstic 3", choices = c("All"))
      } else if(length(unique(filtered2()$variable3)) == 1) {
          if(unique(filtered2()$variable3) == ""){
            NULL
          }
        } else {
          selectInput("Choice3", "Characteristic 3", choices = c("All", unique(filtered2()$variable3)))
        }
      }) 
    
    
    
    filtered3 <- reactive ({
        
        if(input$Choice3 == "All"){
            filtered2()
        } else
        filtered2() %>%
            filter(variable3 == input$Choice3)
    })
    
    output$Choice4 <- renderUI({
            req(input$Choice3)
        
        if(input$Choice3 == "All"){
          NULL
            
        }  else if(length(unique(filtered3()$variable4)) == 1) {
          if(unique(filtered3()$variable4) == ""){
            NULL
          }
        } else {
          (selectInput("Choice4", "Characteristic 4", choices = c("All", unique(filtered3()$variable4))))
        
            
        }

    })
    filtered4 <- reactive ({
        if(input$Choice4 == "All"){
            filtered3()
        } else
        filtered3() %>%
            filter(variable4 == input$Choice4)
    })
    
    
    
    output$`Select Indicator` <- renderUI({
        req(input$Choice4)
        if(input$Choice4 == "All"){
            selectInput("Select Indicator", "Select Indicator", choices = c("All", unique(filtered1()$indicator_title)))
        }
        else {
            selectInput("Select Indicator", "Select Indicator", choices = c("All", unique(filtered4()$indicator_title)))
        }
        
    })
    
    output$dto <- renderDataTable({
        req(input$`Select Indicator`)
        filtered4()
        })
    output$download <- downloadHandler(
        filename = function(){"thename.csv"}, 
        content = function(fname){
            write.csv(filtered4(), fname, row.names = FALSE)
        }
    )

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
