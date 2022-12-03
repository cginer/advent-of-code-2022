library(dplyr)
library(tidyr)
library(readr)


priority_dictionary <- tibble(
  item = c(letters, LETTERS),
  priority = 1:52
)

sum_of_priorities <- function(rucksack_contents_filename, priority_dictionary){
  rucksack_contents <- read_tsv(rucksack_contents_filename, col_name = FALSE) %>%
    rename(contents = X1)

  rucksack_problems <- rucksack_contents %>%
    mutate(rucksack = 1:n(),
           contents_length = nchar(contents),
           compartment_1 = substring(contents, 1, contents_length/2),
           compartment_2 = substring(contents, contents_length/2 + 1, contents_length)) %>%
    pivot_longer(cols = c("compartment_1", "compartment_2"), names_to = "compartment",
                 values_to = "item") %>%
    separate_rows(item, sep = "") %>%
    distinct(rucksack, compartment, item) %>%
    filter(!item %in% "") %>%
    count(rucksack, item) %>%
    filter(n > 1) %>%
    left_join(priority_dictionary)

  total_priorities <- sum(rucksack_problems$priority)

  return(total_priorities)

}

sum_of_priorities("day_3_input.txt", priority_dictionary)
