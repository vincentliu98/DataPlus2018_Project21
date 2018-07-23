# DataPlus2018_Project21: Co-Curricular Pathways (E-Advisor)

## Final R Shiny App --> eAdvisor Folder
### https://brookezkeene.shinyapps.io/eAdvisor
ui.R --> user interface of final Shiny app

server.R --> data analysis and management for final Shiny app

## Initial Testing of R Shiny App
app.R --> R script that simply creates and runs the shiny app
  - testing ui.R and server.R as sources for Shiny App

ui.R --> user interface R script that handles front-end appearance
  - testing Shiny dashboard package and the use of tabs and widgets

server.R --> server R script that handles back-end data management
  - testing saving and receiving data from google sheets
  - testing hybrid and jaccard recommender widgets

## Recommendation System
ContentBasedRec.R --> our initial content-based recommender

CollaborativeRec.R --> our initial collaborative filtering recommender
  - includes user vs. user and item vs. item collaborative filtering

JaccardRec.R --> our initial testing with Jaccard similarity
  - can be used to find similar programs or students
  - may use to improve collaborative or content based filtering

RecSim.R --> R script to run and test our recommendation system
