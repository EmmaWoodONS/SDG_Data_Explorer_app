install.packages("shiny")
install.packages('dplyr', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)

library(shiny)
library(dplyr)
library(ggplot2)

#set working directory & read in test csv to play with
setwd('Y:\\Data Collection and Reporting\\Jemalex\\CSV')
csv_3_a_1 <- read.csv('indicator_3-a-1.csv')

ui <- fluidPage(
  titlePanel("SDG Cross-disaggregations"),
  selectInput('disaggs1', 'Select characteristic 1', choices = c("", "Country", "Sex")),
  selectInput('disaggs2', 'Select characteristic 2', choices = c("", "Region", "Age")),
  selectInput('disaggs3', 'Select characteristic 3', choices = c("", "Ethnicity")),
  selectInput('indicator1', 'Select indicator', choices = c("", "3.a.1")),
  plotOutput("layering"),
)

server <- function(input, output, session){
  output$layering <- renderPlot({
    data <- subset(csv_3_a_1, Age == input$Age
                   )
    ggplot(data) + 
      geom_point(aes(x = Year, y = Value, colour = Sex)
        
      )
    })
}

shinyApp(ui = ui, server = server)
