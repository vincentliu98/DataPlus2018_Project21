# app.R --> Shiny App

library(shiny)
library(rsconnect)

options(shiny.sanitize.errors = FALSE)
options(httr_oob_default = TRUE)

source('ui.R')
source('server.R')

shinyApp(ui = ui, server = server)