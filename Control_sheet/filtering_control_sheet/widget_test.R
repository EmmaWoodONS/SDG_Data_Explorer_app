library(shiny)
library(shinyWidgets)

# data("mpg", package = "ggplot2")

ui <- fluidPage(
  fluidRow(
    column(
      width = 10, offset = 1,
      tags$h3("Filter data with selectize group"),
      panel(
        selectizeGroupUI(
          id = "my-filters",
          params = list(
            variable1 = list(inputId = "variable_group_1", title = "characteristic 1:"),
            variable2 = list(inputId = "variable_group_2", title = "characteristic 2:"),
            variable3 = list(inputId = "variable_group_3", title = "characteristic 3:"),
            variable4 = list(inputId = "variable_group_4", title = "characteristic 4:")
          )
        ), status = "primary"
      ),
      DT::dataTableOutput(outputId = "table")
    )
  )
)

server <- function(input, output, session) {
  control_sheet <- read.csv("D:\\Coding_repos\\SDG_Data_Explorer_app\\Control_sheet\\control_sheet_no_NAs.csv") %>% 
    mutate(across(where(is.factor), as.character))
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = control_sheet,
    vars = c("variable_group_1", "variable_group_2", "variable_group_3", "variable_group_4")
  )
  output$table <- DT::renderDataTable(res_mod())
}

shinyApp(ui, server)