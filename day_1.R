library(dplyr)
library(tidyr)

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
