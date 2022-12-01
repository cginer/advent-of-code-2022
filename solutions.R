library(dplyr)
library(tidyr)

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

  return(the_most_calories)

}


elf_calories("day_one_input.txt")
