library(dplyr)


datastream <- readLines("day_6_input.txt")

signal <- strsplit(datastream, split = "")[[1]]

for (i in 4:(length(signal))) {
  possible_marker <- signal[(i-3):i]

  n_unique_letters <- possible_marker %>% unique() %>% length()

  if(n_unique_letters %in% 4){
    print(paste0("We've got a marker in position ", i, ":"))
    print(possible_marker)
    break
  }
}
