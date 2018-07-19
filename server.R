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

# Content-Based Filtering
content_filter <- function(netID, progress)
{
  ## Access a Student's Co-Curriculars with their NetID
  # Find row of given netID
  gs_eadvisor <- gs_title("E-Advisor Database")
  id_data <- gs_read_csv(gs_eadvisor, col_names = TRUE)
  
  progress$inc(0.1) # Increment progress bar
  
  ids = id_data[c(1)]
  id_row = which(ids==netID, arr.ind = TRUE)
  if(length(id_row) == 0) {
    return()
  }
  else {
    id_row <- id_row[1,1]
  }
  
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
  
  progress$inc(0.1) # Increment progress bar
  
  gs_tags <- gs_title("DukeGroups_Edited")
  programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
  
  progress$inc(0.1) # Increment progress bar
  
  program_names = programs_df[c(1)]             # Column of Program Names
  program_names <- program_names[-1,]
  programs_df <- programs_df[-1,-1]             # Remove column of program names
  
  progs <- c()
  for(i in rec_progs) {
    index <- which(prog_list$Code %in% i)
    temp_name <- prog_list$CoCurriculars[index]
    new_index <- which(program_names %in% temp_name)
    progs <- append(progs, new_index)
  }
  rec_progs <- progs                            # Vector of programs with appropriate indexes
  
  ## ContentBasedRec.R
  # Determine DF and IDF vectors
  DF = colSums(programs_df)                     # Document Frequency 
  total_tags = rowSums(programs_df)             # Total number of tags for a program
  N = NROW(program_names)                       # Total number of documents
  IDF = log10(N/DF)                             # Inverse Document Frequency
  
  # Create a data frame for a student's participation using their input
  students_df = data.frame(matrix(0,N,1))
  rownames(students_df) <- program_names
  student_progs <- rec_progs
  for(i in student_progs) {
    students_df[i,1] = 1
  }
  
  # Normalize data
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
  
  # Change scores to ranks and delete programs already participated in
  participated <- c()
  for(i in 1:N) {
    if(students_df[i,] == 1) {
      stud_predictions[i,] <- -1
      participated <- append(participated, i)    }
  }
  
  stud_predictions <- as.data.frame(rank(stud_predictions))
  rownames(stud_predictions) <- program_names
  
  stud_predictions <- as.data.frame(stud_predictions[-participated,, drop = FALSE])
  
  return(stud_predictions)
}

# Collaborative Filtering
collaborative_filter <- function(netID, progress)
{
  ## Function to Calculate Cosine Similarity -- Item vs. Item
  getCosine <- function(x,y) 
  {
    this_cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this_cosine)
  }
  
  ## Function to Calculate Similarity Scores -- User vs. User
  getScore <- function(history, similarities)
  {
    x <- sum(history*similarities)/sum(similarities)
    x
  }
  
  ## CollaborativeRec.R
  # Load data from csv file
  pathways <- read_csv("/Users/brookekeene/Documents/Duke University/Data+/ShinyPathways.csv")
  
  # Create and label a document term matrix
  # Documents = Student IDs
  # Terms = Programs
  corpus <- Corpus(VectorSource(pathways$`programs`))
  ids_programs <- as.data.frame(as.matrix(DocumentTermMatrix(corpus)))
  
  stud_ids = pathways[c(1)]                     # Column of Student Net IDs
  ids_programs$student <- stud_ids
  #row.names(ids_programs) <- apply(stud_ids, MARGIN = 1, FUN = paste0)
  num_studs <- nrow(ids_programs)
  num_progs <- ncol(ids_programs)-1
  
  ## Item vs. Item Collaborative Filtering (beginning only)
  # Create a placeholder item vs. item data frame
  ids_programs.similarity  <- as.data.frame(matrix(NA, nrow=num_progs,ncol=num_progs, dimnames = list(colnames(ids_programs[,1:num_progs]),colnames(ids_programs[,1:num_progs]))))
  
  # Fill data frame with cosine similarities
  for(i in 1:num_progs) {
    for(j in 1:num_progs) {
      # Fill in placeholder with cosine similarities
      ids_programs.similarity[i,j] <- getCosine(as.matrix(ids_programs[i]),as.matrix(ids_programs[j]))
    }
  }
  ids_programs.similarity <- as.data.frame(ids_programs.similarity)
  
  progress$inc(0.1) # Increment progress bar
  
  ## User vs. User Collaborative Filtering
  # Create a placeholder user vs. user matrix
  holder <- matrix(NA, nrow = num_studs, ncol = num_progs, dimnames = list((stud_ids$student),colnames(ids_programs[,1:num_progs])))
  
  # Loop through students (rows) and programs (cols)
  for(i in 1:num_studs) {
    for(j in 1:num_progs) {
      # get student's and program's name
      stud <- rownames(holder)[i]
      prog <- colnames(holder)[j]
      
      # filter out programs student has already participated in by storing an empty string
      if(as.integer(ids_programs[ids_programs$student == stud,prog]) == 1) {
        holder[i,j] <- -1
      }
      else {
        # get program's top N neighbors sorted by similarity
        topN <- ((head(n=5, (ids_programs.similarity[order(ids_programs.similarity[,prog],decreasing = TRUE),][prog]))))
        topN_names <- as.character(rownames(topN))
        topN_similarities <- as.numeric(topN[,1])
        
        # get student's participation history for those N programs
        topN_participation <- ids_programs[,c("student", topN_names)]
        topN_studPart <- topN_participation[topN_participation$student == stud,]
        topN_studPart <- as.numeric(topN_studPart[!(names(topN_studPart) %in% c("student"))])
        
        # calculate the score for that program and student
        holder[i,j] <- getScore(similarities = topN_similarities, history = topN_studPart)
      }
    }
  }
  ids_programs.scores <- holder
  
  progress$inc(0.1) # Increment progress bar
  
  # Find specific scores associated with netID provided
  stud_scores <- ids_programs.scores[(which(rownames(ids_programs.scores) %in% netID)),]
  stud_scores <- as.data.frame(stud_scores)
  
  # Convert program codes to indexes
  gs_prog <- gs_title("Co-Curriculars")
  prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
  
  progress$inc(0.1) # Increment progress bar
  
  gs_tags <- gs_title("DukeGroups_Edited")
  programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
  
  program_names = programs_df[c(1)]              # Column of Program Names
  program_names <- program_names[-1,]
  programs_df <- programs_df[-1,-1]              # Remove column of program names
  
  stud_predictions = as.data.frame(matrix(0, nrow = nrow(programs_df), ncol = 1))
  rownames(stud_predictions) <- program_names
  
  for(i in 1:nrow(stud_scores)) {
    prog_code <- rownames(stud_scores)[i]
    index <- which(prog_list$Code %in% strtoi(prog_code))
    prog_name <- prog_list$CoCurriculars[index]
    new_index <- which(program_names %in% prog_name)
    
    stud_predictions[new_index,1] <- stud_scores[i,1]
  }
  
  # Change Scores to Ranks and Delete Programs Already Participated in
  participated <- c()
  for(i in 1:nrow(stud_predictions)) {
    if(stud_predictions[i,] < 0) {
      participated <- append(participated, i)
    }
  }
  
  stud_predictions <- as.data.frame(rank(stud_predictions))
  rownames(stud_predictions) <- program_names
  
  stud_predictions <- as.data.frame(stud_predictions[-participated,, drop = FALSE])
  
  progress$inc(0.1) # Increment progress bar
  Sys.sleep(0.25)
  
  return(stud_predictions)
}

server <- function(input, output, session) {
  # Save User Profile
  gs_eadvisor <- gs_title("E-Advisor Database")
  observeEvent(
    input$submit,
    {
      # Progress Bar
      prof_progress <- shiny::Progress$new()
      on.exit(prof_progress$close())
      
      prof_progress$set(message = "Loading Recommendations...", value = 0)
      
      # Check if user filled all fields
      if(is.null(input$netid)||is_empty(input$major)||is.null(input$year)
         ||is_empty(input$yr1prog)||is_empty(input$yr2prog)||is_empty(input$yr3prog)||is_empty(input$yr4prog)) {
        session$sendCustomMessage("check", "Please fill out your information for all fields!")
        return()
      }
      
      prof_progress$inc(0.25)
      Sys.sleep(0.1)
      
      # Pre-process variables with multiple entries
      majs <- paste(input$major, collapse = ", ")
      yr1 <- paste(input$yr1prog, collapse = ", ")
      yr2 <- paste(input$yr2prog, collapse = ", ")
      yr3 <- paste(input$yr3prog, collapse = ", ")
      yr4 <- paste(input$yr4prog, collapse = ", ")
      
      prof_progress$inc(0.25)
      Sys.sleep(0.1)
      
      # Add row to google sheet
      gs_add_row(gs_eadvisor, 
                 input = c(input$netid, majs, input$year, yr1, yr2, yr3, yr4))
      
      prof_progress$inc(0.25)
      Sys.sleep(0.1)
      
      # Clear input cells
      reset("netid")
      reset("major")
      reset("year")
      reset("yr1prog")
      reset("yr2prog")
      reset("yr3prog")
      reset("yr4prog")
      
      prof_progress$inc(0.25)
      Sys.sleep(0.1)
      
      # Send user a message
      session$sendCustomMessage("thanks", "Thank you for submitting your profile!")
    }
  )
  
  # Recommender Widget - Hybrid Recommender 
    # Combination of ContentBasedRec.R and CollaborativeRec.R
  observeEvent(
    input$recGo,
    {
      # Progress Bar
      rec_progress <- shiny::Progress$new()
      on.exit(rec_progress$close())
      
      rec_progress$set(message = "Loading Recommendations...", value = 0)
        
      # Run Functions and Store Prediction Data
      netID = input$recID
      
      content_scores <- content_filter(netID, rec_progress)
      # Check if netID exists in our system
      if(is.null(content_scores)) {
        reset("recID")
        session$sendCustomMessage("noRecord", "We do not seem to have your information. Please fill out your user profile before using this widget.")
        return()
      }
      
      rec_progress$inc(0.1) # Increment progress bar
      Sys.sleep(0.25)
      
      collaborative_scores <- collaborative_filter(netID, rec_progress)
      
      final_scores <- as.data.frame(matrix(NA, nrow = nrow(content_scores), ncol = 2, dimnames = list(rownames(content_scores),c('Score','Description'))))
      
      # Calculate weights of recommendation systems
      content_percentage = 0.75
      collaborative_percentage = 0.25
      for(i in 1:nrow(content_scores)) {
        final_scores[i,1] <- (content_scores[i,1]*content_percentage) + (collaborative_scores[i,1]*collaborative_percentage)
      }
      
      rec_progress$inc(0.1) # Increment progress bar
      Sys.sleep(0.25)
      
      # Organize Final Scores
      final_scores <- final_scores[order(final_scores[,'Score'], decreasing = TRUE),]
      final_scores <- head(final_scores, n = 10)  # display only top 10 programs
      
      # Get Descriptions
      gs_prog <- gs_title("Co-Curriculars")
      prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
      
      for(i in 1:nrow(final_scores)){
        prog_name <- rownames(final_scores)[i]
        index <- which(prog_list$CoCurriculars %in% prog_name)
        final_scores[i,2] <- prog_list[index,3]
      }
      
      rec_progress$inc(0.1) # Increment progress bar
      Sys.sleep(0.25)
      
      # Render Output Table
      output$table <- renderTable(final_scores,
                                  rownames = TRUE,
                                  colnames = TRUE)
      
      
    }
  )
  
  # Recommendation Widget - Similar Program Recommender
    # Jaccard similarity calculated using code from JaccardRec.R
  observeEvent(
    input$recGo2,
    {
      # Progress Bar
      rec2_progress <- shiny::Progress$new()
      on.exit(rec2_progress$close())
      
      rec2_progress$set(message = "Loading Recommendations...", value = 0)
      
      # Match code to actual co-curricular names --> EDIT
      gs_prog <- gs_title("Co-Curriculars")
      prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
      
      rec2_progress$inc(0.25)
      
      gs_tags <- gs_title("DukeGroups_Edited")
      programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))
      
      rec2_progress$inc(0.25)
      
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
        
      rec2_progress$inc(0.25)
      
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
      
      rec2_progress$inc(0.25)
      
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