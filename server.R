# server.R --> server file for Shiny App

#install.packages --> ?

library(rsconnect)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(tidyverse)
library(tm)
library(googlesheets)

server <- function(input, output, session) {
  # Save User Profile
  gs_eadvisor <- gs_title("E-Advisor Database")
  observeEvent(
    input$submit,
    {
      # Check if user filled all fields
      if(is.null(input$netid)||is_empty(input$major)||is.null(input$year)
         ||is_empty(input$yr1prog)||is_empty(input$yr2prog)||is_empty(input$yr3prog)||is_empty(input$yr4prog)) {
        session$sendCustomMessage("check", "Please fill out your information for all fields!")
        return()
      }
      
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
      
      # Send user a message
      session$sendCustomMessage("thanks", "Thank you for submitting your profile!")
    }
  )
  
  # Content-Based Recommender Widget
  observeEvent(
    input$recGo,
    {
      # Access a person's co-curriculars with their NetID
      # Find row of given netID
      gs_eadvisor <- gs_title("E-Advisor Database")
      id_data <- gs_read_csv(gs_eadvisor, col_names = TRUE)
      ids = id_data[c(1)]
      id_row = which(ids==input$recID, arr.ind = TRUE)
      id_row <- id_row[1,1]
      
      # Isolate co-curricular codes of given netID
      id_progs <- id_data[id_row,]
      id_progs <- id_progs[-c(1,2,3)]
      
      # Add all co-curricular codes to a vector, rec_progs
      rec_progs <- c()
      for(i in id_progs) {
        if(is.integer(i) && i != 1000 && !(i %in% rec_progs)) {
          rec_progs <- append(rec_progs, i)
        }
        else {
          temp_list <- as.numeric(unlist(strsplit(as.character(i), ", ")))
          for(j in temp_list) {
            if(j != 1000 && !(j %in% rec_progs)) {
              rec_progs <- append(rec_progs, j)
            }
          }
        }
      }
      
      # Match codes to actual co-curricular names
      gs_prog <- gs_title("Co-Curriculars")
      prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
      
      gs_tags <- gs_title("DukeGroups_Edited")
      programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
      
      program_names = programs_df[c(1)]              # Column of Program Names
      program_names <- program_names[-1,]
      programs_df <- programs_df[-1,-1]              # Remove column of program names
      
      progs <- c()
      for(i in rec_progs) {
        index <- which(prog_list$Code %in% i)
        temp_name <- prog_list$CoCurriculars[index]
        new_index <- which(program_names %in% temp_name)
        progs <- append(progs, new_index)
      }
      rec_progs <- progs                             # Vector of programs with appropriate indexes
      
      # ContentBasedRec.R
      # Determine DF and IDF vectors
      DF = colSums(programs_df)                    # Document Frequency 
      total_tags = rowSums(programs_df)            # Total number of tags for a program
      N = NROW(program_names)                      # Total number of documents
      IDF = log10(N/DF)                            # Inverse Document Frequency
        
      # Create a data frame for a student's participation using their input
      students_df = data.frame(matrix(0,N,1))
      rownames(students_df) <- program_names
      student_progs <- rec_progs
      for(i in student_progs) {
        students_df[i,1] = 1
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
      rownames(stud_predictions) <- program_names
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
      # Render Output Table
      output$table <- renderTable(
        sort(stud_predictions[,1], decreasing = TRUE),
        rownames = TRUE,
       colnames = FALSE
      )
      
    }
  )
  
  # Jaccard Similarity Recommendation Widget
  observeEvent(
    input$recGo2,
    {
      # Match code to actual co-curricular names --> EDIT
      gs_prog <- gs_title("Co-Curriculars")
      prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
      
      gs_tags <- gs_title("DukeGroups_Edited")
      programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
      
      program_names = programs_df[c(1)]         # Column of Program Names
      program_names <- program_names[-1,]
      programs_df <- programs_df[-1,-1]         # Remove column of program names
      prog_num <- nrow(programs_df)             # Number of total programs
      tag_num <- ncol(programs_df)              # Number of total tags
      
      index <- which(prog_list$Code %in% strtoi(input$recProg))
      name <- prog_list$CoCurriculars[index]
      prog_index <- which(program_names %in% name)
      
      # JaccardRec.R
      # Jaccard Function
      jaccard <- function(x, y) {
        inter_cardinality <- length(intersect(x, y))
        union_cardinality <- length(union(x, y))
        return(inter_cardinality/union_cardinality)
      }
        
      # Change 1s to a New Number, Unique to Tag
      for(r in 1:prog_num) {
        for(c in 1:tag_num) {
          if(programs_df[r,c] == 1) {
            programs_df[r,c] <- c               # Sets column number as unique tag number
          }
        }
      }
        
      # Create New List of Vectors Containing Programs' Unique Tag Numbers
      prog_vecs <- list()
      for(r in 1:prog_num) {
        temp_vec <- vector(mode = 'numeric', length = 0)
        for(c in 1:tag_num) {
          if(programs_df[r,c] != 0) {
            temp_vec <- append(temp_vec, programs_df[r,c])
          }
        }
        prog_vecs[[r]] <- temp_vec              # Adds vector to list
      }
        
      # Create New Data Frame Containing Jaccard Similarity for Any 2 Programs
      prog_sim <- matrix(NA, nrow = prog_num, ncol = prog_num)
      rownames(prog_sim) <- program_names
      for(r in 1:prog_num) {
        for(c in 1:prog_num) {
          prog_sim[r,c] <- jaccard(prog_vecs[[r]], prog_vecs[[c]])
        }
      }
      
      # Render Output Table
      output$table2 <- renderTable (
        sort(prog_sim[-1, prog_index], decreasing = TRUE),
        rownames = TRUE,
        colnames = FALSE
      )
      
    }
  )
}

shinyServer(server)