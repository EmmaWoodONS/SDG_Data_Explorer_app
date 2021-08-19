library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)

source('UI.R', local = TRUE)
source('Server.R')


shinyApp(
  ui = UI,
  server = server
)