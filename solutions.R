library(dplyr)
library(tidyr)
library(readr)

################################################################################
# Advent of code - Day 1
################################################################################

elf_calories <- function(filename){

  data <- read.table(filename, blank.lines.skip = FALSE) %>%
    rename(calory_list = V1)

  n_elves <- data %>% filter(is.na(calory_list)) %>% nrow() + 1

  tidy_data <- data %>%
    summarise(list = paste(calory_list, collapse = ",")) %>%
    separate(list, sep = ",NA,", into = paste("elf", 1:n_elves, sep = "_")) %>%
    pivot_longer(starts_with("elf"), names_to = "elf", values_to = "calories") %>%
    separate_rows(calories, sep = ",", convert = TRUE) %>%
    group_by(elf) %>%
    summarise(total_calories = sum(calories))

  the_most_calories <- tidy_data %>%
    arrange(total_calories) %>%
    tail(1) %>%
    pull(total_calories)

  total_top_three <- tidy_data %>%
    arrange(total_calories) %>%
    tail(3) %>%
    pull(total_calories) %>%
    sum()

  return(list(most = the_most_calories, top_three = total_top_three))

}


elf_calories("day_one_input.txt")


################################################################################
# Advent of code - Day 2
################################################################################

total_score <- function(guide_filename, opponent_dictionary, your_dictionary,
                        shape_combinations){

  guide <- read_delim(guide_filename, delim = " ", col_names = FALSE) %>%
    rename(opponent_key = X1, your_key = X2)

  guide_scores <- guide %>%
    left_join(opponent_dictionary) %>%
    left_join(your_dictionary) %>%
    left_join(shape_combinations) %>%
    mutate(round_points = shape_points + outcome_points)

  total_points <- sum(guide_scores$round_points)

  return(list(guide_scores, total_points))
}

# Part one dictionaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opponent_dictionary <- tibble(
  opponent_key = c("A", "B", "C"),
  opponent_shape = c("rock", "paper", "scissors")
)

your_dictionary <- tibble(
  your_key = c("X", "Y", "Z"),
  your_shape = c("rock", "paper", "scissors"),
  shape_points = c(1L, 2L, 3L)
)

shape_combinations <- tibble(
  opponent_shape = c("rock", "rock", "rock",
                      "paper", "paper", "paper",
                      "scissors", "scissors", "scissors"),
  your_shape = c("rock", "paper", "scissors",
                  "rock", "paper", "scissors",
                  "rock", "paper", "scissors"),
  outcome_points = c(3L, 6L, 0L,
                    0L, 3L, 6L,
                    6L, 0L, 3L)
)

total_score("day_two_input.txt", opponent_dictionary, your_dictionary,
            shape_combinations)[[2]]

# Part two dictionaries
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opponents_new_dictionary <- tibble(
  opponent_key = c("A", "B", "C"),
  opponent_shape = c("rock", "paper", "scissors")
)

your_new_dictionary <- tibble(
  your_key = c("X", "Y", "Z"),
  outcome_points = c(0L, 3L, 6L)
)

new_shape_combinations <- tibble(
  opponent_shape = c("rock", "rock", "rock",
                      "paper", "paper", "paper",
                      "scissors", "scissors", "scissors"),
  your_shape = c("rock", "paper", "scissors",
                  "rock", "paper", "scissors",
                  "rock", "paper", "scissors"),
  outcome_points = c(3L, 6L, 0L,
                    0L, 3L, 6L,
                    6L, 0L, 3L),
  shape_points = c(1L, 2L, 3L,
                   1L, 2L, 3L,
                   1L, 2L, 3L)
)

total_score("day_two_input.txt", opponent_dictionary = opponents_new_dictionary,
            your_dictionary = your_new_dictionary,
            shape_combinations = new_shape_combinations)[[2]]
