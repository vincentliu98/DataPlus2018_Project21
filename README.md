# DataPlus2018_Project21: Co-Curricular Pathways (E-Advisor)

## Creation of R Shiny App
app.R --> main R script that simply creates and runs the shiny app
  - uses ui.R and server.R as sources for final Shiny App

ui.R --> user interface R script that handles front-end appearance of Shiny app
  - uses Shiny dashboard package

server.R --> server R script that handles back end data management of Shiny app
  - user profile data saved into a new row of a google sheet
  - content based recommender widget managed here (malfunctioning at the moment)

## Recommendation System
ContentBasedRec.R --> our initial content-based recommender

CollaborativeRec.R --> our initial collaborative filtering recommender

RecSim.R --> R script to run and test our recommendation system
