# app.R --> Shiny App

library(shiny)
source('ui.R')
source('server.R')

library(shiny)

shinyApp(ui = ui, server = server)