# DataPlus2018_Project21: Co-Curricular Pathways (E-Advisor)

## Creation of R Shiny App
app.R --> main R script that simply creates and runs the shiny app
  - uses ui.R and server.R as sources for final Shiny App

ui.R --> user interface R script that handles front-end appearance of Shiny app
  - uses Shiny dashboard package to manage tabs and widgets

server.R --> server R script that handles back end data management of Shiny app
  - user profile data saved into a new row of a google sheet
  - content based and jaccard recommender widget housed here

## Recommendation System
ContentBasedRec.R --> our initial content-based recommender

CollaborativeRec.R --> our initial collaborative filtering recommender
  - includes user vs. user and item vs. item collaborative filtering

JaccardRec.R --> our initial testing with Jaccard similarity
  - can be used to find similar programs or students
  - may use to improve collaborative or content based filtering

RecSim.R --> R script to run and test our recommendation system
