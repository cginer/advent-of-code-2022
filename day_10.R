library(dplyr)
library(tidyr)

get_signal_strengths <- function(program_filename){
  program <- read.table(program_filename, header = FALSE, sep = " ",
                        fill = TRUE) %>%
    rename(instruction = V1, V_value = V2)

  cycle_signal_strength <- program %>%
    mutate(V_value = if_else(instruction %in% "addx", paste(0, V_value, sep = ","),
                             "0"),
           n_cycles = if_else(instruction %in% "addx", "1,1", "1")) %>%
    separate_rows(V_value, n_cycles, sep = ",") %>%
    select(-n_cycles) %>%
    mutate(cycle = 1L + 1L:n(),
           X_value = 1L + cumsum(V_value),
           effective_strength = cycle * X_value)
  return(cycle_signal_strength)
}

total_interesting_signal_strengths <- function(cycle_signal_strength){
  total_signal <- cycle_signal_strength %>%
    filter((cycle + 20) %% 40 %in% 0) %>%
    pull(effective_strength) %>%
    sum()
  return(total_signal)
}

get_screen <- function(cycle_signal_strength){
  CRT_sprite_position <- bind_rows(tibble(cycle = 1L, X_value = 1L),
                               cycle_signal_strength %>%
                                 select(cycle, X_value)) %>%
    mutate(CRT_row = ceiling(cycle/40),
           CRT_pixel = (cycle - 1) %% 40) %>%
    group_by(cycle) %>%
    mutate(result = if_else(CRT_pixel %in% c(X_value - 1, X_value, X_value + 1),
                            "#",
                            ".")) %>%
    ungroup()

  message <- CRT_sprite_position %>%
    select(CRT_row, CRT_pixel, result) %>%
    group_by(CRT_row) %>%
    summarise(paste(result, collapse = ""))

  return(message)
}

cycle_signal_strength <- get_signal_strengths("day_10_input.txt")

# Part 1
total_interesting_signal_strengths(cycle_signal_strength)

# Part 2
get_screen(cycle_signal_strength)
