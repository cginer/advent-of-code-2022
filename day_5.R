library(dplyr)
library(readr)

# Not easy with tidyverse. Making a copy of the rearrangement with one stack per
# line to help.

# Helper functions
read_procedure <- function(input_filename){

  input_content <- read_lines(input_filename)
  separation_line <- c(1:length(input_content))[input_content %in% ""]

  procedure <- read_table(input_filename, skip = separation_line, col_names = FALSE) %>%
    select(n_crates = X2, from_stack = X4, to_stack = X6)

  return(procedure)
}

read_arrangement <- function(stack_arrangement_filename){

    arrangement <- readLines(con = stack_arrangement_filename, n = -1) %>%
        gsub(pattern = "\\[", replacement = "") %>%
        gsub(pattern = "\\]", replacement = "") %>%
        strsplit(split = "\\t")

  return(arrangement)
}

add_to_stack <- function(stack_vector, crate){

  len <- length(stack_vector)
  if (len %in% 1 & stack_vector[1] %in% ""){
    updated_stack <- crate
  } else {
    updated_stack <- c(stack_vector, crate)
  }

  return(updated_stack)
}

remove_n_from_stack <- function(stack_vector, n){
  len <- length(stack_vector)
  if (len > n){
    updated_stack_vector <- stack_vector[1:(len-n)]
  } else if (len %in% n) {
    updated_stack_vector <- ""
  } else {
    print("The stack doesn't have enough crates")
  }

  return(updated_stack_vector)
}

# Main function
get_top_word <- function(stack_arrangement_filename, input_filename, crane = "CrateMover 9000"){

  arrangement <- read_arrangement(stack_arrangement_filename)
  procedure <- read_procedure(input_filename)

  for (i in 1:nrow(procedure)) {
    n_crates <- procedure$n_crates[i]
    from_stack <- procedure$from_stack[i]
    to_stack <- procedure$to_stack[i]
    if(crane %in% "CrateMover 9000"){
      for (crate in 1:n_crates) {
        crate_to_move <- arrangement[[from_stack]] %>% last()
        arrangement[[from_stack]] <- remove_n_from_stack(arrangement[[from_stack]], 1)
        arrangement[[to_stack]] <- add_to_stack(arrangement[[to_stack]], crate_to_move)
      }
    }else if(crane %in% "CrateMover 9001"){
      crates_to_move <- arrangement[[from_stack]] %>% tail(n_crates)
      arrangement[[from_stack]] <- remove_n_from_stack(arrangement[[from_stack]], n_crates)
      arrangement[[to_stack]] <- add_to_stack(arrangement[[to_stack]], crates_to_move)
    }

  }

  top_word <- sapply(arrangement, last) %>%
    paste(collapse = "")

  return(top_word)
}

get_top_word("day_5_input_part1.txt", "day_5_input.txt")
get_top_word("day_5_input_part1.txt", "day_5_input.txt", crane = "CrateMover 9001")
