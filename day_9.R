library(dplyr)
library(readr)

# ( 3,-1) ( 3,0) ( 3,1) ( 3,2) ( 3,3)
# ( 2,-1) ( 2,0) ( 2,1) ( 2,2) ( 2,3)
# ( 1,-1) ( 1,0) ( 1,1) ( 1,2) ( 1,3)
# ( 0,-1) ( 0,0) ( 0,1) ( 0,2) ( 0,3)
# (-1,-1) (-1,0) (-1,1) (-1,2) (-1,3)

get_H_locations <- function(series_of_motions){

  series_of_steps <- series_of_motions %>%
    mutate(block = 1:n()) %>%
    group_by(block, direction) %>%
    summarise(direction = rep(direction, times = moves),
              .groups = "drop") %>%
    mutate(H_row_move = case_when(direction %in% "U" ~ 1L,
                                  direction %in% "D" ~ -1L,
                                  direction %in% c("R", "L") ~ 0L),
           H_col_move = case_when(direction %in% "R" ~ 1L,
                                  direction %in% "L" ~ -1L,
                                  direction %in% c("U", "D") ~ 0L),
           H_row_pos = cumsum(H_row_move),
           H_col_pos = cumsum(H_col_move)) %>%
    mutate(step = 1:n()) %>%
    select(block, step, direction, H_row_pos, H_col_pos)

  return(series_of_steps)
}

get_next_step <- function(H_row, H_col, T_row, T_col){

  row_diff <- H_row - T_row
  col_diff <- H_col - T_col
  row_distance <- abs(row_diff)
  col_distance <- abs(col_diff)
  row_sign <- sign(row_diff)
  col_sign <- sign(col_diff)

  if(row_distance <= 1L && col_distance <= 1L){
    # Close enough, stay put
    T_row_to <- T_row
    T_col_to <- T_col
  }else if(row_distance == 0L && col_distance == 2L){
    # Move horizontally
    T_row_to <- T_row
    T_col_to <- T_col + col_sign
  }else if(row_distance == 2L && col_distance == 0L){
    # Move vertically
    T_row_to <- T_row + row_sign
    T_col_to <- T_col
  }else if(row_distance == 2L || col_distance == 2L){
    # Move diagonally
    T_row_to <- T_row + row_sign
    T_col_to <- T_col + col_sign
  }else{
    print("Tail is too far!")
    T_row_to <- NA_integer_
    T_col_to <- NA_integer_
  }

  return(c(T_row_to, T_col_to))
}

positions_visited <- function(motions_filename){
  series_of_motions <- read_table(motions_filename, col_names = c("direction", "moves"))

  series_of_steps <- get_H_locations(series_of_motions) %>%
    mutate(T_row_pos = 0L, T_col_pos = 0L)

  for (i in 2:nrow(series_of_steps)) {
    series_of_steps[i, c("T_row_pos", "T_col_pos")] <- get_next_step(
      series_of_steps[i, "H_row_pos"],
      series_of_steps[i, "H_col_pos"],
      series_of_steps[i-1, "T_row_pos"],
      series_of_steps[i-1, "T_col_pos"]
    )
  }

  total_positions <- series_of_steps %>%
    distinct(T_row_pos, T_col_pos) %>%
    nrow()

  return(total_positions)
}

positions_visited("day_9_input.txt")
