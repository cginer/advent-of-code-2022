library(dplyr)
library(tidyr)
library(readr)


get_id_list <- function(id_range){
  lapply(id_range, function(x){
    values <- strsplit(x, split = "-")[[1]]
    values[1]:values[2]
  })
}

number_of_full_overlaps <- function(list_filename){

  cleaning_list <- read_csv(list_filename, col_names = FALSE) %>%
    rename(elf_1 = X1, elf_2 = X2)

  explicit_cleaning_list <- cleaning_list %>%
    mutate(pair = 1:n()) %>%
    pivot_longer(starts_with("elf"), names_to = "elf", values_to = "id_range") %>%
    mutate(id_list = get_id_list(id_range)) %>%
    unnest(cols = c(id_list)) %>%
    group_by(pair, id_list) %>%
    summarise(elves = paste(elf, collapse = ","),
              .groups = "drop") %>%
    group_by(pair, elves) %>%
    summarise(ids = paste(id_list, collapse = ","),
              .groups = "drop") %>%
    pivot_wider(names_from = "elves", values_from = "ids") %>%
    mutate(overlap = case_when(!is.na(`elf_1,elf_2`) & (is.na(elf_1) | is.na(elf_2)) ~ "full_overlap",
                               !is.na(`elf_1,elf_2`) ~ "partial_overlap",
                               TRUE ~ "none"))

  fully_overlapping_pairs <- explicit_cleaning_list %>%
    filter(overlap %in% "full_overlap") %>%
    nrow()

  return(fully_overlapping_pairs)

}

number_of_full_overlaps("day_4_input.txt")
