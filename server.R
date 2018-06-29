# server.R --> server file for Shiny App

#install.packages("shiny")
#install.packages("shinyjs")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("tidyverse")
#install.packages("tm")
#install.packages("googlesheets")

library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(tidyverse)
library(tm)
library(googlesheets)

# Load Data from Google Sheets
# List of Tags
gs_tags <- gs_title("Tag_Words")
programs_tags <- gs_read_csv(gs_tags, col_names = TRUE)

server <- function(input, output) {
  # Save User Profile
  gs_eadvisor <- gs_title("E-Advisor Database")
  observeEvent(
    input$submit,
    {
    # Pre-process variables with multiple entries
    majs <- paste(input$major, collapse = ", ")
    yr1 <- paste(input$yr1prog, collapse = ", ")
    yr2 <- paste(input$yr2prog, collapse = ", ")
    yr3 <- paste(input$yr3prog, collapse = ", ")
    yr4 <- paste(input$yr4prog, collapse = ", ")
    
    # Add row to google sheet
    gs_add_row(gs_eadvisor, 
               input = c(input$netid, majs, input$year, yr1, yr2, yr3, yr4))
    
    # Clear input cells
    reset("netid")
    reset("major")
    reset("year")
    reset("yr1prog")
    reset("yr2prog")
    reset("yr3prog")
    reset("yr4prog")
    }
  )
  
  # Send User a Message?
  #observeEvent(
  #  input$submit,
  #  {
  #    session$sendCustomMessage(type = 'testmessage',
  #                              message = 'Thank you for submitting your profile')
  #  }
  #)
  
  # Access activities with NetID
  rec_progs <- c()
  
  observeEvent(
    input$recGo,
    {
      # Find row of given netID
      gs_eadvisor <- gs_title("E-Advisor Database")
      id_data <- gs_read_csv(gs_eadvisor, col_names = TRUE)
      ids = id_data[c(1)]
      id_row = which(ids==input$recID, arr.ind = TRUE)
      id_row <- id_row[1,1]
      
      # Isolate activities of given netID
      id_progs <- id_data[id_row,]
      id_progs <- id_progs[-c(1,2,3)]
      
      for(i in id_progs) {
        if(is.integer(i) && i != 2 && !(i %in% rec_progs)) {
          rec_progs <- append(rec_progs, i)
        }
        else {
          temp_list <- as.numeric(unlist(strsplit(as.character(i), ", ")))
          for(j in temp_list) {
            if(j != 2 && !(j %in% rec_progs)) {
              rec_progs <- append(rec_progs, j)
            }
          }
        }
      }
      
      output$table <- DT::renderDataTable({
        # ContentBasedRec.R
        # Load data from Google Sheet
        gs_tags <- gs_title("DukeGroups_Tech")
        programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
        
        program_names = programs_df[c(1)]              # Column of Program Names
        program_names <- program_names[-1,]
        programs_df <- programs_df[-1,-1]              # Remove column of program names
        
        # Determine DF and IDF vectors
        DF = colSums(programs_df)                      # Document Frequency 
        total_tags = rowSums(programs_df)              # Total number of tags for a program
        N = NROW(program_names)                        # Total number of documents
        IDF = log10(N/DF)                              # Inverse Document Frequency
        
        # Create a data frame for a student's participation using their input
        students_df = data.frame(matrix(0,N,1))
        row.names(students_df) <- program_names
        student_progs <- rec_progs
        for(i in student_progs) {
          print(i)
          students_df[i-2,1] = 1
        }
        
        # Normalize Data
        norm_prog = sweep(programs_df, 1, sqrt(total_tags), "/")
        
        # Create matrix of student profiles
        stud_profiles = matrix(NA, nrow = ncol(students_df), ncol = ncol(norm_prog))
        for(i in 1:ncol(students_df)) {
          stud_profiles[i,] = apply(norm_prog, 2, function(x){t(x)%*%students_df[,i]})
        }
        
        # Create matrix of weighted scores of tags for each program
        weighted_scores = t(apply(norm_prog, 1, function(x){x*IDF}))
        
        # Create matrix of student predictions based on weighted scores
        stud_predictions = matrix(NA, nrow = nrow(programs_df), ncol = ncol(students_df))
        row.names(stud_predictions) <- program_names
        for(c in 1:ncol(students_df)) {
          for(r in 1:nrow(programs_df)) {
            stud_predictions[r,c] = sum(stud_profiles[c,]*weighted_scores[r,])
          }
        }
        
        # Factor in Programs that Students Have Already Participated in
        for(i in 1:N) {
          if(students_df[i,] == 1) {
            stud_predictions[i] = -1 # change to -1
          }
        }
        
        DT::datatable(stud_predictions)
      })
    }
  )
}

shinyServer(server)