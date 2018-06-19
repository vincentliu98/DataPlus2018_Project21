# Model of a Content-Based Recommendation Algorithm
library(tidyverse)
library(tm)

# Load Data from Excel (in csv format)
programs_tags <- read_csv("/Users/brookekeene/Documents/Duke University/Data+/Project_21/Tag_Words.csv")

# Create and Label a Document Term Matrix
  # Documents = Programs
  # Terms = Tags
corpus <- Corpus(VectorSource(programs_tags$`Tag Words`))
programs_df <- as.matrix(DocumentTermMatrix(corpus))

program_names = programs_tags[c(1)]            # Column of Program Names
row.names(programs_df) <- apply(program_names, MARGIN = 1, FUN = paste0)

# Determine DF and IDF vectors
DF = colSums(programs_df)                      # Document Frequency 
total_tags = rowSums(programs_df)              # Total number of tags for a program
N = nrow(program_names)                        # Total number of documents
IDF = log10(N/DF)                              # Inverse Document Frequency

# Create a data frame of a student and their participation
students_df = data.frame(matrix(0,N,1))
  # To randomly generate student activities us either of the 2 below:
  # Student1 = sample(c(0,1), size = N, replace = TRUE)) OR rbinom(N, 1, 0.5))
row.names(students_df) <- apply(program_names, MARGIN = 1, FUN = paste0)
student_progs = c("Bass Connections", "Data+", "DataFest")
for(j in student_progs) {
  for(i in 1:N) {
    if(program_names[i,] == j) {
      students_df[i,1] = 1
    }
  }
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
row.names(stud_predictions) <- apply(program_names, MARGIN = 1, FUN = paste0)
for(c in 1:ncol(students_df)) {
  for(r in 1:nrow(programs_df)) {
    stud_predictions[r,c] = sum(stud_profiles[c,]*weighted_scores[r,])
  }
}
stud_predictions

# Factor in Programs that Students Have Already Participated in
for(i in 1:N) {
  if(students_df[i,] == 1) {
    stud_predictions[i] = -1 # change to -1
  }
}


# Analyze Data
  #hist(stud_predictions)
  #qqnorm(stud_predictions)
  #qqline(stud_predictions)

