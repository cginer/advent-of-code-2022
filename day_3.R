library(dplyr)
library(tidyr)
library(readr)


priority_dictionary <- tibble(
  item = c(letters, LETTERS),
  priority = 1:52
)

tidy_rucksacks_content <- function(rucksack_contents){

  tidy_rucksacks <- rucksack_contents %>%
    mutate(rucksack = 1:n(),
           elf_group = rep(1:(n()/3), each = 3),
           contents_length = nchar(contents),
           compartment_1 = substring(contents, 1, contents_length/2),
           compartment_2 = substring(contents, contents_length/2 + 1, contents_length)) %>%
    pivot_longer(cols = c("compartment_1", "compartment_2"), names_to = "compartment",
                 values_to = "item") %>%
    separate_rows(item, sep = "") %>%
    filter(!item %in% "")

  return(tidy_rucksacks)
}

# Part one
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sum_of_priorities <- function(rucksack_contents_filename, priority_dictionary){
  rucksack_contents <- read_tsv(rucksack_contents_filename, col_name = FALSE) %>%
    rename(contents = X1)

  tidy_rucksacks <- tidy_rucksacks_content(rucksack_contents)

  rucksack_problems <- tidy_rucksacks %>%
    distinct(rucksack, compartment, item) %>%
    count(rucksack, item) %>%
    filter(n > 1) %>%
    left_join(priority_dictionary)

  total_priorities <- sum(rucksack_problems$priority)

  return(total_priorities)

}

sum_of_priorities("day_3_input.txt", priority_dictionary)


# Part two
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
badge_priorities <- function(rucksack_contents_filename, priority_dictionary){
  rucksack_contents <- read_tsv(rucksack_contents_filename, col_name = FALSE) %>%
    rename(contents = X1)

  tidy_rucksacks <- tidy_rucksacks_content(rucksack_contents)

  rucksack_badges <- tidy_rucksacks %>%
    distinct(rucksack, elf_group, compartment, item) %>%
    count(elf_group, item) %>%
    filter(n > 2) %>%
    left_join(priority_dictionary)

  total_priorities <- sum(rucksack_badges$priority)

  return(total_priorities)
}

badge_priorities("day_3_input.txt", priority_dictionary)
