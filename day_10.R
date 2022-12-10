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

cycle_signal_strength <- get_signal_strengths("day_10_input.txt")

total_interesting_signal_strengths(cycle_signal_strength)
