# Load libraries ----------------------------------------------------------
library(shiny)
library(shinyjs)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(shinydashboard)
library(shinydashboardPlus)
library(tm)
library(DT)

# Load and Modify Data from Googlesheets ---------------------------------------------
# E-Advisor Database
gs_eadvisor <- gs_key("1lnZaPj22rIo0WYfKNAWerEpRDuh4ByI9tZSVbtMT4iw")
id_data <- gs_read_csv(gs_eadvisor, col_names = TRUE)
# Co-Curriculars
gs_prog <- gs_key("1HkqN1ISgHevSjYQw5XJlkAMQao61GMktFl9CO4PqMJY")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
# DukeGroups_Edited
gs_tags <- gs_key("1Zs8ELNUlX5A1pYOkyrJ38nvK2ZSl_vuh7DAdWhnQ30k")
programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))

## Access Data from Googlesheets
# Create list of majors
gs_maj <- gs_key("1LqaHYZixDr4aMYq64wojMF5AYDVOsD83IWlX-ayDc0c")
maj_list <- gs_read_csv(gs_maj, col_names = TRUE)
maj_choice <- as.list(maj_list$Abbreviations)
names(maj_choice) <- maj_list$Majors

# Create list of activities
gs_prog <- gs_key("1HkqN1ISgHevSjYQw5XJlkAMQao61GMktFl9CO4PqMJY")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
prog_choice <- as.list(prog_list$Code)
names(prog_choice) <- prog_list$CoCurriculars

# Pop-up Messages
js_thanks <- 'Shiny.addCustomMessageHandler("thanks", function(message) {alert(message);});'
js_check <- 'Shiny.addCustomMessageHandler("check", function(message) {alert(message);});'
js_exists <- 'Shiny.addCustomMessageHandler("exists", function(message) {alert(message);});'
js_record <- 'Shiny.addCustomMessageHandler("noRecord", function(message) {alert(message);});'

# Functions for data wrangling-----------------------------------------
# getAdmit <- function(stringYear)===Create separate dataframes for id_data from each year
getAdmit <- function(stringYear){
  return(id_data[id_data['Admit Year'] == stringYear, ])
}

# countActs <- function(id_data)---Create a function to create a dataframe of num of activities
countActs <- function(id_data){
  
  # loop through each year
  year = 'Year 1'
  i = 1;
  acts1 = list(); # ((name1, name2),(name3),...) 52 rows
  counts1 = c(); # (1, 1, 2...) 52 elements
  # loop through each row
  for (stud in 1:nrow(id_data[year])){
    program_names = c(); #(name1, name2)
    # convert string to int
    act = pull(id_data[year][stud, 1]); 
    act = unlist(strsplit(act, ', '))
    act = as.numeric(act); # (code1, code2)
    count = 0;
    # change the code to program names
    for (n in act){
      if (n == 1000) {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
      }
      else {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
        count = count +1;
      }
    }
    # count the num of activities
    counts1[stud] <- count;
    # append the program names of a specific year to 
    acts1[[i]] <- program_names;
    i= i + 1;
  }
  
  year = 'Year 2'
  i = 1;
  acts2 = list(); # ((name1, name2),(name3),...) 52 rows
  counts2 = c(); # (1, 1, 2...) 52 elements
  # loop through each row
  for (stud in 1:nrow(id_data[year])){
    program_names = c(); #(name1, name2)
    # convert string to int
    act = pull(id_data[year][stud, 1]); 
    act = unlist(strsplit(act, ', '))
    act = as.numeric(act); # (code1, code2)
    count = 0;
    # change the code to program names
    for (n in act){
      if (n == 1000) {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
      }
      else {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
        count = count +1;
      }
    }
    # count the num of activities
    counts2[stud] <- count;    
    # append the program names of a specific year to 
    acts2[[i]] <- program_names;
    i= i + 1;
  }
  
  year = 'Year 3'
  i = 1;
  acts3 = list(); # ((name1, name2),(name3),...) 52 rows
  counts3 = c(); # (1, 1, 2...) 52 elements
  # loop through each row
  for (stud in 1:nrow(id_data[year])){
    program_names = c(); #(name1, name2)
    # convert string to int
    act = pull(id_data[year][stud, 1]); 
    act = unlist(strsplit(act, ', '))
    act = as.numeric(act); # (code1, code2)
    count = 0;
    # change the code to program names
    for (n in act){
      if (n == 1000) {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
      }
      else {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
        count = count +1;
      }
    }
    # count the num of activities
    counts3[stud] <- count;
    # append the program names of a specific year to 
    acts3[[i]] <- program_names;
    i= i + 1;
  }
  
  ### NOTE!! There no comma in the rows right now, but when there are multiple activities need to add strsplit
  year = 'Year 4'
  i = 1;
  acts4 = list(); # ((name1, name2),(name3),...) 52 rows
  counts4 = c(); # (1, 1, 2...) 52 elements
  # loop through each row
  for (stud in 1:nrow(id_data[year])){
    program_names = c(); #(name1, name2)
    # convert string to int
    act = pull(id_data[year][stud, 1]); 
    act = as.numeric(act); # (code1, code2)
    count = 0;
    # change the code to program names
    for (n in act){
      if (n == 1000) {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
      }
      else {
        program_name = codeToName(n);
        program_names <- c(program_names, program_name); 
        count = count +1;
      }
    }
    # count the num of activities
    counts4[stud] <- count;
    # append the program names of a specific year to 
    acts4[[i]] <- program_names;
    i= i + 1;
  }
  
  allCounts = do.call(rbind, Map(data.frame, Year1=counts1, Year2=counts2,Year3=counts3,Year4=counts4))
  #  allActs = list(acts1,acts2,acts3,acts4); # (4 * ((name1, name2),(name3),...))
  return(allCounts);
}
# functions to create df of each year's activities
numFreshmanYear <- function(allCounts){
  return(as.data.frame(table(unlist(allCounts[, 1]))))
}

numSophomoreYear <- function(allCounts){
  return(as.data.frame(table(unlist(allCounts[, 2]))))
}

numJuniorYear <- function(allCounts){
  return(as.data.frame(table(unlist(allCounts[, 3]))))
}

numSeniorYear <- function(allCounts){
  return(as.data.frame(table(unlist(allCounts[, 4]))))
}

# codeToName <- function(code)---Convert code to program name
codeToName <- function(code){
  # find the corresponding name of the code
  program_name = prog_list[prog_list$Code == code, ]$CoCurriculars;
  return(program_name);
}

# Create some dataframes ---------------
# Create separate dataframes for id_data from each year
Admit2015 <- getAdmit('2015')
Admit2016 <- getAdmit('2016')
Admit2017 <- getAdmit('2017')

# Create dataframes of num of activities
allCounts2015 <- countActs(Admit2015)
allCounts2016 <- countActs(Admit2016)
allCounts2017 <- countActs(Admit2017)

#Df of number of appearances for num of activities
Fresh2015 <- numFreshmanYear(allCounts2015)
Sopho2015 <- numSophomoreYear(allCounts2015)
Junior2015 <- numJuniorYear(allCounts2015)
Senior2015 <- numSeniorYear(allCounts2015)

Fresh2016 <- numFreshmanYear(allCounts2016)
Sopho2016 <- numSophomoreYear(allCounts2016)
Junior2016 <- numJuniorYear(allCounts2016)
Senior2016 <- numSeniorYear(allCounts2016)

Fresh2017 <- numFreshmanYear(allCounts2017)
Sopho2017 <- numSophomoreYear(allCounts2017)
Junior2017 <- numJuniorYear(allCounts2017)
Senior2017 <- numSeniorYear(allCounts2017)

# Create a df to get all the program names of id_data and then rank by popularity----------------
# convert the code to program names

# Content-Based Filtering--------------------------------
content_filter <- function(netID, progress)
{
  # Access a Student's Co-Curriculars with their NetID
  # Find row of given netID
  ids <- id_data[c(1)]
  id_row <- which(ids==netID, arr.ind = TRUE)
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
  
  progress$inc(0.1)                              # Progress Bar - 10%
  
  # Match codes to actual co-curricular names
  program_names = programs_df[c(1)]              # Column of Program Names
  program_names <- program_names[-1,]
  new_programs_df <- programs_df[-1,-1]          # Remove NA row & program names
  
  progs <- c()
  for(i in rec_progs) {
    index <- which(prog_list$Code %in% i)
    temp_name <- prog_list$CoCurriculars[index]
    new_index <- which(program_names %in% temp_name)
    progs <- append(progs, new_index)
  }
  rec_progs <- progs                             # Vector of programs with appropriate indexes
  
  ## ContentBasedRec.R
  # Determine DF and IDF vectors
  DF = colSums(new_programs_df)                  # Document Frequency 
  total_tags = rowSums(new_programs_df)          # Total number of tags for a program
  N = NROW(program_names)                        # Total number of documents
  IDF = log10(N/DF)                              # Inverse Document Frequency
  
  # Create a data frame for a student's participation using their input
  students_df = data.frame(matrix(0,N,1))
  rownames(students_df) <- program_names
  student_progs <- rec_progs
  for(i in student_progs) {
    students_df[i,1] = 1
  }
  
  progress$inc(0.1)                              # Progress Bar - 20%
  
  # Normalize data
  norm_prog <- sweep(new_programs_df, 1, sqrt(total_tags), "/")
  
  # Create matrix of student profiles
  stud_profiles = matrix(NA, nrow = ncol(students_df), ncol = ncol(norm_prog))
  for(i in 1:ncol(students_df)) {
    stud_profiles[i,] = apply(norm_prog, 2, function(x){t(x)%*%students_df[,i]})
  }
  
  # Create matrix of weighted scores of tags for each program
  weighted_scores = t(apply(norm_prog, 1, function(x){x*IDF}))
  
  # Create matrix of student predictions based on weighted scores
  stud_predictions = matrix(NA, nrow = nrow(new_programs_df), ncol = ncol(students_df))
  rownames(stud_predictions) <- program_names
  for(c in 1:ncol(students_df)) {
    for(r in 1:nrow(new_programs_df)) {
      stud_predictions[r,c] = sum(stud_profiles[c,]*weighted_scores[r,])
    }
  }
  
  progress$inc(0.1)                              # Progress Bar - 30%
  
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

# Collaborative Filtering-------------------------------------
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
  # Create pathways data frame to store netIDs and list of programs
  pathways <- as.data.frame(matrix(NA, nrow = nrow(id_data), ncol = 2))
  colnames(pathways) <- c('netID', 'programs')
  for(i in 1:nrow(id_data)) {
    pathways[i,1] <- id_data[i,1]
    
    id_progs <- id_data[i,]
    id_progs <- id_progs[-c(1,2,3)]
    
    temp_progs <- c()
    for(c in id_progs) {
      if(is.integer(c) && c != 1000 && !(c %in% temp_progs)) {
        temp_progs <- append(temp_progs, c)
      }
      else {
        temp_list <- as.numeric(unlist(strsplit(as.character(c), ", ")))
        for(j in temp_list) {
          if(j != 1000 && !(j %in% temp_progs)) {
            temp_progs <- append(temp_progs, j)
          }
        }
      }
    }
    
    pathways[i,2] <- paste(temp_progs, collapse = ", ")
    
  }
  
  progress$inc(0.1)                              # Progress Bar - 50%
  
  # Create and label a document term matrix
  # Documents = Student IDs
  # Terms = Programs
  corpus <- Corpus(VectorSource(pathways$`programs`))
  ids_programs <- as.data.frame(as.matrix(DocumentTermMatrix(corpus)))
  
  stud_ids = pathways[c(1)]                      # Column of Student Net IDs
  ids_programs$student <- stud_ids
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
  
  progress$inc(0.1)                              # Progress Bar - 60%
  
  ## User vs. User Collaborative Filtering
  # Create a placeholder user vs. user matrix
  holder <- matrix(NA, nrow = num_studs, ncol = num_progs, dimnames = list((stud_ids$netID),colnames(ids_programs[,1:num_progs])))
  
  # Loop through students (rows) and programs (cols)
  for(i in 1:num_studs) {
    if(i == num_studs/2) {
      progress$inc(0.1)                              # Progress Bar - 70%
    }
    
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
  
  progress$inc(0.1)                              # Progress Bar - 80%
  
  # Find specific scores associated with netID provided
  stud_scores <- ids_programs.scores[(which(rownames(ids_programs.scores) %in% netID)),]
  stud_scores <- as.data.frame(stud_scores)
  
  # Convert program codes to indexes
  program_names = programs_df[c(1)]              # Column of Program Names
  program_names <- program_names[-1,]
  new_programs_df <- programs_df[-1,-1]          # Remove column of program names
  
  stud_predictions = as.data.frame(matrix(0, nrow = nrow(new_programs_df), ncol = 1))
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
  
  return(stud_predictions)
}
# Server & Hybrid Rec & Plot--------------------------------------------
server <- function(input, output, session) {
  # Plot number of activites=====================
  
  # Plot number of activities categorized with admit year
  
  # yearPlot
  output$yearPlot <- renderPlot({
    # Render a barplot
    ggplot(data=allCounts2016, aes(x=Year1, y=get(input$year))) +
      geom_bar(stat="identity") 
    
  })
  
  # Save User Profile
  observeEvent(
    input$submit,
    {
      # Progress Bar
      prof_progress <- shiny::Progress$new()
      on.exit(prof_progress$close())
      
      prof_progress$set(message = "Saving Profile...", value = 0)
      
      # Check if user filled all fields
      if(is.null(input$netid)||is_empty(input$major)||is.null(input$year)
         ||is_empty(input$yr1prog)||is_empty(input$yr2prog)||is_empty(input$yr3prog)||is_empty(input$yr4prog)) {
        session$sendCustomMessage("check", "Please fill out your information for all fields!")
        return()
      }
      # Check if user already has a profile
      ids <- id_data[c(1)]
      id_row <- which(ids==tolower(input$netid), arr.ind = TRUE)
      if(length(id_row) != 0) {
        session$sendCustomMessage("exists", "It appears that you are already in our system.")
        # Clear input cells
        reset("netid")
        reset("major")
        reset("year")
        reset("yr1prog")
        reset("yr2prog")
        reset("yr3prog")
        reset("yr4prog")
        return()
      }
      
      prof_progress$inc(0.25)                    # Progress Bar - 25%
      Sys.sleep(0.1)
      
      # Pre-process variables with multiple entries
      majs <- paste(input$major, collapse = ", ")
      yr1 <- paste(input$yr1prog, collapse = ", ")
      yr2 <- paste(input$yr2prog, collapse = ", ")
      yr3 <- paste(input$yr3prog, collapse = ", ")
      yr4 <- paste(input$yr4prog, collapse = ", ")
      
      prof_progress$inc(0.25)                    # Progress Bar - 50%
      Sys.sleep(0.1)
      
      # Add row to google sheet
      gs_add_row(gs_eadvisor, 
                 input = c(tolower(input$netid), majs, input$year, yr1, yr2, yr3, yr4))
      
      prof_progress$inc(0.25)                    # Progress Bar - 75%
      Sys.sleep(0.1)
      
      # Clear input cells
      reset("netid")
      reset("major")
      reset("year")
      reset("yr1prog")
      reset("yr2prog")
      reset("yr3prog")
      reset("yr4prog")
      
      prof_progress$inc(0.25)                    # Progress Bar - 100%
      Sys.sleep(0.1)
      
      # Send user a message
      session$sendCustomMessage("thanks", "Thank you for submitting your profile!")
    }
  )
  
  ## Widget - Co-Curricular Recommender
  # Hybrid Recommender - Combination of ContentBasedRec.R and CollaborativeRec.R
  observeEvent(
    input$recGo,
    {
      # Load E-Advisor Database Googlesheet
      gs_eadvisor <- gs_key("1lnZaPj22rIo0WYfKNAWerEpRDuh4ByI9tZSVbtMT4iw")
      id_data <- gs_read_csv(gs_eadvisor, col_names = TRUE)
      
      # Progress bar
      rec_progress <- shiny::Progress$new()
      on.exit(rec_progress$close())
      
      rec_progress$set(message = "Loading Recommendations...", value = 0)
      
      # Run Functions and Store Prediction Data
      netID = tolower(input$recID)
      
      content_scores <- content_filter(netID, rec_progress)
      # Check if netID exists in our system
      if(is.null(content_scores)) {
        reset("recID")
        session$sendCustomMessage("noRecord", "We do not seem to have your information. Please fill out your user profile before using this widget.")
        return()
      }
      
      rec_progress$inc(0.1)                      # Progress Bar - 40%
      
      collaborative_scores <- collaborative_filter(netID, rec_progress)
      
      rec_progress$inc(0.1)                      # Progress Bar - 90%
      
      final_scores <- as.data.frame(matrix(NA, nrow = nrow(content_scores), ncol = 2, dimnames = list(rownames(content_scores),c('Score','Description'))))
      
      # Calculate weights of recommendation systems
      content_percentage = 0.75
      collaborative_percentage = 0.25
      for(i in 1:nrow(content_scores)) {
        final_scores[i,1] <- (content_scores[i,1]*content_percentage) + (collaborative_scores[i,1]*collaborative_percentage)
      }
      
      # Organize final scores
      final_scores <- final_scores[order(final_scores[,'Score'], decreasing = TRUE),]
      final_scores <- head(final_scores, n = 10)  # display only top 10 programs
      
      final_rows <- rownames(final_scores)
      
      # Get descriptions
      
      for(i in 1:nrow(final_scores)){
        prog_name <- rownames(final_scores)[i]
        index <- which(prog_list$CoCurriculars %in% prog_name)
        final_scores[i,2] <- prog_list[index,3]
      }
      
      final_scores <- as.data.frame(final_scores[,-1])
      row.names(final_scores) <- final_rows
      
      rec_progress$inc(0.1)                      # Progress Bar - 100%
      
      # Render output table
      output$table <- renderTable(final_scores,
                                  rownames = TRUE,
                                  colnames = FALSE)
    }
  )
  
  ## Widget - Find Similar Programs
  # Jaccard similarity calculated with JaccardRec.R
  observeEvent(
    input$recGo2,
    {
      # Progress bar
      rec2_progress <- shiny::Progress$new()
      on.exit(rec2_progress$close())
      
      rec2_progress$set(message = "Loading Recommendations...", value = 0)
      
      # Match program code to actual co-curricular name
      program_names = programs_df[c(1)]          # Column of Program Names
      program_names <- program_names[-1,]
      new_programs_df <- programs_df[-1,-1]      # Remove column of program names
      prog_num <- nrow(new_programs_df)          # Number of total programs
      tag_num <- ncol(new_programs_df)           # Number of total tags
      
      # Find index of input program
      index <- which(prog_list$Code %in% strtoi(input$recProg))
      name <- prog_list$CoCurriculars[index]
      prog_index <- which(program_names %in% name)
      
      ## JaccardRec.R
      # Jaccard Function
      jaccard <- function(x, y) {
        inter_cardinality <- length(intersect(x, y))
        union_cardinality <- length(union(x, y))
        return(inter_cardinality/union_cardinality)
      }
      
      # Change 1s to a new number, unique to tag
      for(r in 1:prog_num) {
        for(c in 1:tag_num) {
          if(new_programs_df[r,c] == 1) {
            new_programs_df[r,c] <- c            # Sets column number as unique tag number
          }
        }
      }
      
      rec2_progress$inc(0.25)                    # Progress Bar - 25%
      
      # Create new list of vectors containing programs' unique tag numbers
      prog_vecs <- list()
      for(r in 1:prog_num) {
        temp_vec <- vector(mode = 'numeric', length = 0)
        for(c in 1:tag_num) {
          if(new_programs_df[r,c] != 0) {
            temp_vec <- append(temp_vec, new_programs_df[r,c])
          }
        }
        prog_vecs[[r]] <- temp_vec               # Adds vector to list
      }
      
      rec2_progress$inc(0.25)                    # Progress Bar - 50%
      
      # Create new data frame containing jaccard similarity for any 2 programs
      prog_sim <- matrix(NA, nrow = prog_num, ncol = prog_num)
      rownames(prog_sim) <- program_names
      for(r in 1:prog_num) {
        for(c in 1:prog_num) {
          prog_sim[r,c] <- jaccard(prog_vecs[[r]], prog_vecs[[c]])
        }
      }
      
      rec2_progress$inc(0.25)                    # Progress Bar - 75%
      
      prog_sim <- as.data.frame(prog_sim[,prog_index])
      
      # Organize final similarities
      final_sim <- as.data.frame(matrix(NA, nrow = nrow(prog_sim), ncol = 2, dimnames = list(program_names,c('Score','Description'))))
      
      for(i in 1:nrow(prog_sim)) {
        final_sim[i,1] <- prog_sim[i,1]
      }
      
      final_sim <- final_sim[order(final_sim[,1], decreasing = TRUE),]
      final_sim <- final_sim[-1,]
      final_sim <- head(final_sim, n = 10) # display only top 10 programs
      
      final_rows <- rownames(final_sim)
      
      # Get descriptions
      
      for(i in 1:nrow(final_sim)) {
        prog_name <- rownames(final_sim)[i]
        index <- which(prog_list$CoCurriculars %in% prog_name)
        final_sim[i,2] <- prog_list[index,3]
        print(prog_list[index,3])
      }
      
      final_sim <- as.data.frame(final_sim[,-1])
      row.names(final_sim) <- final_rows
      
      rec2_progress$inc(0.25)                    # Progress Bar - 100%
      
      # Render output table
      output$table2 <- renderTable (final_sim,
                                    rownames = TRUE,
                                    colnames = FALSE
      )
    }
  )
  ## Thumbs up/down
  observeEvent(
    input$up, {
      print('hi')
    }
  )
  
  observeEvent(
    input$down, {
      print('bye')
    }
  )
}
# UI--------------------------
header <-  
  dashboardHeader(
    title = "Duke Co-Curricular E-Advisor",
    titleWidth = 300
  )

sidebar <- 
  dashboardSidebar(
    width = 300,
    sidebarMenuOutput("menu"),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      #menuItem("Favorites", tabName = "favorites", icon= icon("star")),
      menuItem("Co-Curricular Recommender", tabName = "hybrid", icon = icon("list"), 
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Find Similar Co-Curriculars", tabName = "jaccard", icon = icon("th"), 
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Statistics", tabName = "stats", icon = icon("table"), 
               badgeLabel = "new", badgeColor = "green"),
      menuItem("Feedback", tabName = "feedback", icon = icon("comment")),
      menuItem("About Us", tabName = "about", icon = icon("address-card"))
    )
  )

body <- 
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",                   
              h2("Dashboard"),
              h3("Welcome!"),
              p("As we all know, Duke University can be a difficult environment to navigate, especially 
                with its countless opportunities and resources. Therefore, we have compiled a list of over 
                150 different co-curricular programs, although we hope to eventually include all Duke activities. 
                With the information we have gathered regarding these organizations, we have deveoloped
                various algorithms that will recommend certain co-curricular programs based on a student's
                interests and previous participation. Feel free to explore our website and test the tools
                shown in the menu on the left"),
              p("If you would like to help us improve our system, please fill out the User Profile
                below. Additionally, if you would like to add your organization to our database or provide
                leave us a comment, please refer to our \"Feedback\" section. If you would like to learn more 
                about our project, please visit the \"About Us\" section."),
              tags$head(tags$script(HTML(js_thanks))),
              tags$head(tags$script(HTML(js_check))),
              tags$head(tags$script(HTML(js_exists))),
              useShinyjs(),
              div(
                box(title = "User Profile", status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    # Include clarifying text
                    helpText("Please enter your information into all of the following fields."),
                    textOutput("check"),
                    column(width = 4,
                           textInput("netid", label = h4("Net ID"), placeholder = "Ex. abc123"),
                           selectInput("major", label = h4("Major(s)"),
                                       choices = maj_choice,
                                       multiple = TRUE),
                           helpText("*Please select up to 3 majors. If you are unsure, simply select \"Undeclared\"."),
                           textInput("year", label = h4("Admit Year"), placeholder = "Ex. 2016, if Class of 2020"),
                           helpText("*Please type the year in which you matriculated at Duke.")
                    ),
                    column(width = 8,
                           selectInput("yr1prog", label = h4("Programs - Year 1"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr2prog", label = h4("Programs - Year 2"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr3prog", label = h4("Programs - Year 3"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr4prog", label = h4("Programs - Year 4+"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           helpText("*Please select the programs that you have participated in during each year 
                                    you have been at Duke. Summer programs are counted under the academic year
                                    you finished directly prior to the program. If you have not completed a specific
                                    year yet, please select \"NA\".")
                    ),
                    actionButton("submit", "Submit")
                )
              )
              ),
      tabItem(tabName = "hybrid",
              h2("Co-Curricular Recommender"),
              p("Discover new Duke co-curricular activities with the tool below! 
                If you have already completed your user profile, you are able to use
                our \"Co-Curricular Recommender\" by simply entering your Duke netID and 
                pressing \"Recommend!\"."),
              p("If you have participated in only one or two co-curricular activities 
                here at Duke, we would recommend that you initially try the 
                \"Find Similar Co-Curriculars\" tab."),
              
              ## Hybrid Recommender
              tags$head(tags$script(HTML(js_record))),
              useShinyjs(),
              div(
                box(title = "Co-Curricular Recommender", status = "primary",
                    solidHeader = TRUE, width = 12, collapsible = TRUE, 
                    column(width = 4,
                           # Include clarifying text
                           helpText("If you have already completed your user profile, you are ready
                                    to receive recommendations! Please enter your Duke NetID 
                                    below which you used to create your profile."),
                           textInput("recID", label = h3("Enter your NetID"), placeholder = "Ex. abc123"),
                           actionButton("recGo", "Recommend!")
                           ),
                    column(width = 8,
                           tableOutput("table")
                    ),
                    collapsed = FALSE
                    )
              )
              ),
      tabItem(tabName = "jaccard",
              h2("Find Similar Co-Curriculars"),
              p("With this tool, you can discover Duke co-curricular activities that 
                are similar to each other! Just select the activity that you are interested
                in from the drop down menu and press \"Recommend!\"."),
              p("If you have already completed your user profile, you are also able to use
                our Co-Curricular Recommender."),
              
              ## Jaccard Similarity Recommender
              box(title = "Find Similar Co-Curriculars", status = "primary",
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                  column(width = 4,
                         helpText("Enter the co-curricular program for which you would like to
                                  see similar activities."),
                         selectInput("recProg", label = h3("Enter a Program"),
                                     choices = prog_choice[-1]),
                         actionButton("recGo2", "Recommend!")
                         ),
                  column(width = 8,
                         tableOutput("table2")
                  ),
                  collapsed = FALSE
              )
              ),
      tabItem(tabName = "stats",
              p("I will modify this section later."),
              # Use a fluid Bootstrap layout
              fluidPage(    
                # Give the page a title
                titlePanel("Statistics & Insights"),
                # Generate a row with a sidebar
                sidebarLayout(      
                  
                  # Define the sidebar with one input
                  sidebarPanel(
                    selectInput("year", "Year:", 
                                choices=colnames(allCounts2016)),
                    hr(),
                    helpText("Choose the Year.")
                  ),
                  
                  # Create a spot for the barplot
                  mainPanel(
                    plotOutput("yearPlot")  
                  )
                )
              )
      ),
      tabItem(tabName = "feedback",
              h2("Feedback"),
              p("Please let us know what you thought of our website! We are currently in the testing stage 
                of this project and would appreciate any and all feedback. If you believe we are missing a 
                co-curricular program or activity, please fill out the box below. If you would like to
                help us by providing more information about an activity, please fill out the following",
                a(" survey", href = "https://goo.gl/forms/BB34EWQfGJofHkyo1"), "."),
              p("Also, please indicate whether you like or dislike our website using the thumbs up and down
                voting method below! Thank you!"),
              textInput('newCo', "Suggest a New Co-Curricular", width = '400px'),
              textAreaInput('comment', "Leave a Comment",
                            width = '400px',
                            height = '250px',
                            resize = "both"),
              fluidRow(
                actionButton("up", label = icon("thumbs-up"),
                             style = 'color: green;
                             position: relative;
                             left: 20px;
                             display:block;
                             height: 50px;
                             width: 50px;
                             border-radius: 50%;
                             border: 2px solid green;'),
                actionButton("down", label = icon("thumbs-down"),
                             style = 'color: red;
                             position: relative;
                             top: -50px;
                             left: 80px;
                             display:block;
                             height: 50px;
                             width: 50px;
                             border-radius: 50%;
                             border: 2px solid red;')
                )
                ),
      tabItem(tabName = "about",
              h2("About Us"),
              p("We are a team of Duke undergraduate students currently working on a", 
                a("Data+", href = "https://bigdata.duke.edu/data"), "project in collaboration 
                with Duke's ",
                a("Office of Information Technology", href = "https://oit.duke.edu/"),
                "(O.I.T.). Duke University is an exciting and ever-changing institution, but as 
                Duke's massive co-curricular environment grows it can become more difficult for 
                undergraduates to navigate this complicated academic and extracurricular landscape. Therefore, 
                our goal is to create an 'e-advisor' tool that will help students determine which 
                co-curricular activities are well-suited for their interests. In order to accomplish 
                this, we hope to gather data that will allow us to track a student's co-curricular 
                'pathway,' a map of the various activities the student was involved in during their 
                time at Duke. With this data, we plan to improve the current recommendation system running
                behind this initial version of our 'e-advisor.'"),
              p("Thank you for helping us with this task! 
                If you have any questions, please visit our ", 
                a("website", href = "https://bigdata.duke.edu/projects/co-curricular-technology-pathways-e-advisor"), 
                " or feel free to contact us at ", 
                a("eadvisordukeoit@gmail.com", href = "mailto:eadvisordukeoit@gmail.com"), "."),
              h3("Contact Information"),
              p("Data+ Director: Paul Bendich"),
              p("Project Manager: Lindsay Berry"),
              p("Project Team Members: Alec Ashforth, Brooke Keene, Vincent Liu, Dezmanique Martin"),
              p("Project Clients: Michael Faber, Evan Levine")
              )
                )
              )

ui <- dashboardPage(header, sidebar, body)


shinyApp(ui = ui, server = server)
