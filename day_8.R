library(dplyr)

# Part 1
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


visible_trees_in_forest <- function(forest_filename){

  forest <- strsplit(readLines(forest_filename), split = "") %>%
    sapply(as.integer) %>% t()

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

  total_visible_trees <- sum(visible_forest > 0)

  return(total_visible_trees)
}


visible_trees_in_forest("day_8_input.txt")


# Part 2
get_scene <- function(tree_line, tree_size){
  n_visible_trees <- 0L
  visible_tree_line <- rep(0L, times = length(tree_line))
  for (i in 1:length(tree_line)) {
    if(tree_line[i] < tree_size){
      n_visible_trees <- n_visible_trees + 1L
    }else if(tree_line[i] >= tree_size){
      n_visible_trees <- n_visible_trees + 1L
      break
    }
  }
  return(n_visible_trees)
}

tree_scenic_score <- function(row, col, forest){
  forest_width <- ncol(forest)
  forest_height <- nrow(forest)
  current_tree <- forest[row, col]

  right_score <- ifelse(col == forest_width, 0L, get_scene(forest[row, (col+1):forest_width], current_tree))
  left_score <- ifelse(col == 1L, 0L, get_scene(forest[row, (col-1):1], current_tree))
  bottom_score <- ifelse(row == forest_height, 0L, get_scene(forest[(row+1):forest_height, col], current_tree))
  top_score <- ifelse(row == 1L, 0L, get_scene(forest[(row-1):1, col], current_tree))

  scenic_score <- right_score * left_score * bottom_score * top_score
  return(scenic_score)
}

max_scenic_score <- function(forest_filename){

  forest <- strsplit(readLines(forest_filename), split = "") %>%
    sapply(as.integer) %>% t()

  scenic_forest <- matrix(nrow = nrow(forest), ncol = ncol(forest), NA)

  for (i in 1:nrow(forest)) {
    for (j in 1:ncol(forest)) {
      scenic_forest[i, j] <- tree_scenic_score(i, j, forest)
    }
  }

  best_scenic_view <- max(scenic_forest)
  return(best_scenic_view)
}

max_scenic_score("day_8_input.txt")
