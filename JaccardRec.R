# Model of Jaccard Similarity Index Calculations
library(tidyverse)
library(googlesheets)

# Jaccard Function
jaccard <- function(x, y) {
  inter_cardinality <- length(intersect(x, y))
  union_cardinality <- length(union(x, y))
  return(inter_cardinality/union_cardinality)
}

# Load Data from GoogleSheets (in csv format)
gs_progdf <- gs_title("DukeGroups_Edited")
progdf <- data.frame(gs_read_csv(gs_progdf, col_names = TRUE))

program_names = progdf[c(1)]              # Column of program names
program_names <- program_names[-1,]
progdf <- progdf[-1,-1]                   # Remove column of program names
prog_num <- nrow(progdf)                  # Number of total programs
tag_num <- ncol(progdf)                   # Number of total tags

# Change 1s to a New Number, Unique to Tag
for(r in 1:prog_num) {
  for(c in 1:tag_num) {
    if(progdf[r,c] == 1) {
      progdf[r,c] <- c                    # Sets column number as unique tag number
    }
  }
}

# Create New List of Vectors Containing Programs' Unique Tag Numbers
prog_vecs <- list()
for(r in 1:prog_num) {
  temp_vec <- vector(mode = 'numeric', length = 0)
  for(c in 1:tag_num) {
    if(progdf[r,c] != 0) {
      temp_vec <- append(temp_vec, progdf[r,c])
    }
  }
  prog_vecs[[r]] <- temp_vec              # Adds vector to list
}

# Create New Data Frame Containing Jaccard Similarity for Any 2 Programs
prog_sim <- as.data.frame(matrix(NA, nrow = nrow(progdf), ncol = nrow(progdf)))
rownames(prog_sim) <- program_names
colnames(prog_sim) <- program_names
for(r in 1:prog_num) {
  for(c in 1:prog_num) {
    prog_sim[r,c] <- jaccard(prog_vecs[[r]], prog_vecs[[c]])
  }
}
