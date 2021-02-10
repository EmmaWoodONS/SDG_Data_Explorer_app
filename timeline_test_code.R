
#Create key events timeline for indicator 8.1.1 - Annual growth rate of real GDP per capita
#Cretae data frame with dates
data <- data.frame(
    id      = 1:5,
    content = c("Foot and Mouth & 9/11", "2008 Global Financial Crisis",
                "2012 London Olympics", "EU Referendum", "Covid-19 Pandemic"),
    start   = c("2001-01-10", "2008-01-11",
                "2012-01-20", "2016-02-14", "2020-03-30"),
    #Must figure out hw to position above and below line
    #position = c(-1, 1, -1, 1, -1)
  )


library(shiny)
library(timevis)

if (interactive()) {
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Indicator 8.1.1: Key GDP events 2000 - 2020"),
      timevisOutput("timeline"),
    ),
    server = function(input, output) {
      output$timeline <- renderTimevis(
        timevis(data.frame(start = data$start, content = data$content))
      )
      observeEvent(input$btn, {
        setItems("timeline",
                 data.frame(start = data$start))
      })
    }
  )
}