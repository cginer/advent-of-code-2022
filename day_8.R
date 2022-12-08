library(dplyr)
library(readr)


forest <- strsplit(readLines("day_8_input.txt"), split = "") %>%
  sapply(as.integer)


check_visible_trees <- function(tree_line){
  last_max <- 0L
  visible_tree_line <- rep(0L, times = length(tree_line))
  for (i in 1:length(tree_line)) {
    if(tree_line[i] > last_max){
      visible_tree_line[i] <- 1L
      last_max <- tree_line[i]
    }
  }
  return(visible_tree_line)
}


visible_trees_in_forest <- function(forest){
  visible_forest <- matrix(nrow = nrow(forest), ncol = ncol(forest), 0L)

  # Left
  for (i in 1:nrow(forest)) {
    visible_forest[i,] <- visible_forest[i,] + check_visible_trees(forest[i,])
  }
  # Top
  for (i in 1:ncol(forest)) {
    visible_forest[,i] <- visible_forest[,i] + check_visible_trees(forest[,i])
  }

  # Rigth
  for (i in 1:nrow(forest)) {
    visible_forest[i,] <- visible_forest[i,] + rev(check_visible_trees(rev(forest[i,])))
  }
  # Bottom
  for (i in 1:ncol(forest)) {
    visible_forest[,i] <- visible_forest[,i] + rev(check_visible_trees(rev(forest[,i])))
  }

  # Edges
  visible_forest[1,] <- visible_forest[1,] + 1L
  visible_forest[nrow(forest),] <- visible_forest[nrow(forest),] + 1L
  visible_forest[,1] <- visible_forest[,1] + 1L
  visible_forest[,nrow(forest)] <- visible_forest[,nrow(forest)] + 1L


  return(visible_forest)
}

visible_forest <- visible_trees_in_forest(forest)
sum(visible_forest > 0)
