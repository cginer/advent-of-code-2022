library(dplyr)
library(tidyr)

read_monkey_notes <- function(notes_filename){
  monkey_notes <- readLines(notes_filename)

  monkey_attributes <- tibble(
    monkey = grep(monkey_notes, pattern = "Monkey", value = TRUE),
    operation = grep(monkey_notes, pattern = "Operation:", value = TRUE),
    test_divisible = grep(monkey_notes, pattern = "Test:", value = TRUE),
    monkey_if_true = grep(monkey_notes, pattern = "If true:", value = TRUE),
    monkey_if_false = grep(monkey_notes, pattern = "If false:", value = TRUE)
  ) %>%
    extract(monkey, into = "monkey", regex = "Monkey ([0-9]):") %>%
    extract(operation, into = "operation", regex = "  Operation: new = (.+)") %>%
    extract(test_divisible, into = "test_divisible", regex = "  Test: divisible by ([0-9]+)", convert = TRUE) %>%
    extract(monkey_if_true, into = "monkey_if_true", regex = "  If true: throw to monkey ([0-9])") %>%
    extract(monkey_if_false, into = "monkey_if_false", regex = "  If false: throw to monkey ([0-9])") %>%
    mutate(operation = paste0("function(old)", operation))

  monkey_items <- grep(monkey_notes, pattern = "Starting items", value = TRUE) %>%
    gsub(pattern = "  Starting items: ", replacement = "") %>%
    strsplit(split = ", ") %>%
    lapply(as.integer)
  names(monkey_items) <- monkey_attributes$monkey

  return(list(monkey_attributes, monkey_items))
}

monkey_business <- function(monkey_data, n_rounds){
  monkey_attributes <- monkey_data[[1]]
  monkey_items <- monkey_data[[2]]

  round_stats <- matrix(nrow = n_rounds, ncol = nrow(monkey_attributes))

  for (round in 1L:n_rounds) {
    # print(round)
    for (inspector in monkey_attributes$monkey) {
      # print(inspector)
      starting_items <- monkey_items[[inspector]]
      monkey_function <- eval(parse(text = monkey_attributes %>% filter(monkey %in% inspector) %>% pull(operation)))

      # Store number of items inspected
      round_stats[round, as.integer(inspector) + 1] <- length(starting_items)

      # Find where items go
      if(length(starting_items) == 0){
        next
      }else{
        item_destinations <- tibble(
          monkey = inspector,
          starting_worry = starting_items) %>%
          left_join(monkey_attributes, by = "monkey") %>%
          mutate(updated_worry = monkey_function(starting_worry),
                 final_worry = floor(updated_worry/3),
                 destination_monkey = if_else((final_worry %% test_divisible) == 0,
                                              monkey_if_true, monkey_if_false))

        # Update item lists
        for (row in 1:nrow(item_destinations)) {
          monkey_items[[item_destinations$destination_monkey[row]]] <- c(monkey_items[[item_destinations$destination_monkey[row]]], item_destinations$final_worry[row])
        }
        monkey_items[[inspector]] <- c()
      }
    }
  }
  monkey_business <- round_stats %>%
    as_tibble() %>%
    pivot_longer(cols = everything(), names_to = "monkey", values_to = "inspected_items") %>%
    group_by(monkey) %>%
    summarise(inspected_items = sum(inspected_items), .groups = "drop") %>%
    arrange(desc(inspected_items)) %>%
    pull(inspected_items) %>%
    head(2) %>%
    prod()

  return(monkey_business)
}


monkey_data <- read_monkey_notes("day_11_input.txt")
monkey_business(monkey_data, 20L)
