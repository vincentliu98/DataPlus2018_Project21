# Model of a Collaborative Filtering Recommendation Algorithm
library(tidyverse)
library(tm)

# Function to Calculate Cosine Similarity -- Item vs. Item
getCosine <- function(x,y) 
{
  this_cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this_cosine)
}

# Function to Calculate Similarity Scores -- User vs. User
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

# Load Data from csv File
pathways <- read_csv("/Users/brookekeene/Documents/Duke University/Data+/ShinyPathways.csv")

# Create and Label a Document Term Matrix
  # Documents = Student IDs
  # Terms = Programs
corpus <- Corpus(VectorSource(pathways$`programs`))
ids_programs <- as.data.frame(as.matrix(DocumentTermMatrix(corpus)))

stud_ids = pathways[c(1)]           # Column of Student Net IDs
ids_programs$student <- stud_ids
#row.names(ids_programs) <- apply(stud_ids, MARGIN = 1, FUN = paste0)
num_studs <- nrow(ids_programs)
num_progs <- ncol(ids_programs)-1

# Item vs. Item Collaborative Filtering

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

# Get the top N neighbours for each
N = 5
ids_programs.neighbours <- matrix(NA, nrow=ncol(ids_programs.similarity),ncol=N,
                                  dimnames=list(colnames(ids_programs.similarity)))

for(i in 1:num_progs) 
{
  ids_programs.neighbours[i,] <- (t(head(n=N,rownames(ids_programs.similarity[order(ids_programs.similarity[,i],decreasing=TRUE),][i]))))
}
# Final Result of Item vs. Item
ids_programs.neighbours

# User vs. User Collaborative Filtering

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
      holder[i,j] <- ""
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

# Make recommendations pretty
ids_programs.scores.ordered <- matrix(NA, nrow = nrow(ids_programs.scores), ncol = ncol(ids_programs.scores), dimnames = list(rownames(ids_programs.scores)))
for(i in 1:nrow(ids_programs.scores)) {
  ids_programs.scores.ordered[i,] <- names(head(n=100, (ids_programs.scores[,order(ids_programs.scores[i,], decreasing = TRUE)])[i,]))
}

# Final Result of User vs. User
ids_programs.scores.ordered

