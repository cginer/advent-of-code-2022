library(dplyr)


find_marker <- function(filename, marker_length){
  datastream <- readLines("day_6_input.txt")

  signal <- strsplit(datastream, split = "")[[1]]

  for (i in marker_length:(length(signal))) {
    possible_marker <- signal[(i-marker_length+1):i]

    n_unique_letters <- possible_marker %>% unique() %>% length()

    if(n_unique_letters %in% marker_length){
      print(paste0("We've got a marker in position ", i, ":"))
      print(possible_marker)
      break
    }
  }
}

find_marker("day_6_input", 4)
find_marker("day_6_input", 14)
