ui <- fluidPage(
  
  tags$img(src = "SDG_logo.png",
           align = "left",
           height = 100, width = 300),
  
  fluidRow(
    
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with selectize group"),
      
      panel(
        uiOutput("Select Indicator"),
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            variable1 = list(inputId = "variable1", title = "characteristic 1:"),
            variable2 = list(inputId = "variable2", title = "characteristic 2:"),
            variable3 = list(inputId = "variable3", title = "characteristic 3:"),
            variable4 = list(inputId = "variable4", title = "characteristic 4:")),
          
          downloadBttn(
            outputId = "downloadData",
            style = "bordered",
            color = "primary",
            size = "sm")
        ),
        
        status = "primary"),
      
      textOutput("csvlink"),
      
      conditionalPanel(
        condition = "input.Select_Indicator == 'All'",
        DT::dataTableOutput(outputId = "table")),
      conditionalPanel(
        condition = "input.Select_Indicator != 'All'",
        plotOutput("plot"),
        textOutput("selections"))
    )
  )
)