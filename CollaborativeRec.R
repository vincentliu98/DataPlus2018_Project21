# Model of a Collaborative Filtering Recommendation Algorithm
library(tidyverse)
library(tm)

# Function to Calculate Cosine Similarity
getCosine <- function(x,y) 
{
  this_cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this_cosine)
}

# Load Data from Excel (in csv format)
pathways <- read_csv("/Users/brookekeene/Documents/Duke University/Data+/pathways.csv")

# Create and Label a Document Term Matrix
# Documents = Student IDs
# Terms = Programs
corpus <- Corpus(VectorSource(pathways$`programs`))
ids_programs <- as.data.frame(as.matrix(DocumentTermMatrix(corpus)))

stud_ids = pathways[c(1)]           # Column of Program Names
row.names(ids_programs) <- apply(stud_ids, MARGIN = 1, FUN = paste0)

# Create a Placeholder Item vs. Item Data Frame
ids_programs.similarity  <- as.data.frame(matrix(NA, nrow=ncol(ids_programs),ncol=ncol(ids_programs),
                                                 dimnames=list(colnames(ids_programs),colnames(ids_programs))))

# Fill data frame with cosine similarities
for(i in 1:ncol(ids_programs)) {
  for(j in 1:ncol(ids_programs)) {
    # Fill in placeholder with cosine similarities
    ids_programs.similarity[i,j] <- getCosine(as.matrix(ids_programs[i]),as.matrix(ids_programs[j]))
  }
}
ids_programs.similarity <- as.data.frame(ids_programs.similarity)

# Get the top N neighbours for each
N = 3
ids_programs.neighbours <- matrix(NA, nrow=ncol(ids_programs.similarity),ncol=N,
                                  dimnames=list(colnames(ids_programs.similarity)))

for(i in 1:ncol(ids_programs)) 
{
  ids_programs.neighbours[i,] <- (t(head(n=N,rownames(ids_programs.similarity[order(ids_programs.similarity[,i],decreasing=TRUE),][i]))))
}

ids_programs.neighbours