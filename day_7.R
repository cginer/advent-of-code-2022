library(dplyr)

determine_type <- function(line_vector){
  if(line_vector[1] %in% "$" & line_vector[2] %in% "ls"){
    line_type <- "list"
  }else if(line_vector[1] %in% "$" & line_vector[2] %in% "cd"){
    if(line_vector[3] %in% ".."){
      line_type <- "cd_up"
    }else{
      line_type <- "cd_down"
    }
  }else if(line_vector[1] %in% "dir"){
    line_type <- "dir"
  }else{
    line_type <- "file"
  }
  return(line_type)
}

read_structure <- function(puzzle_filename){
  file_content <- readLines(puzzle_filename)
  n_lines <- length(file_content)

  split_lines <- strsplit(file_content, split = " ")
  split_lines <- sapply(split_lines, function(x) x %>% gsub(pattern = "/", replacement = "ROOT"))
  in_list <- FALSE
  system_structure <- list()
  current_dir <- NULL

  for (i in 1:n_lines) {
    print(paste0("line ", i))
    line_type <- determine_type(split_lines[[i]])

    if(line_type %in% "cd_down"){
      in_list <- FALSE
      if(is.null(current_dir)){
        current_dir <- split_lines[[i]][3]
      }else{
        current_dir <- paste(current_dir, split_lines[[i]][3], sep = "/")
      }

      # initialise if it doesn't exist
      if(!current_dir %in% names(system_structure)){
        system_structure[[current_dir]] <- list(
          files = c(),
          sub_dirs = c(),
          size = 0L)
      }

    }else if(line_type %in% "list"){
      in_list <- TRUE

    }else if(line_type %in% "file"){
      filename <- split_lines[[i]][2]
      filesize <- split_lines[[i]][1]
      system_structure[[current_dir]][["files"]] <- c(system_structure[[current_dir]][["files"]], filename = filesize) %>% unique()

    }else if(line_type %in% "dir"){
      dirname <- paste(current_dir, split_lines[[i]][2], sep = "/")
      system_structure[[current_dir]][["sub_dirs"]] <- c(system_structure[[current_dir]][["sub_dirs"]], dirname) %>% unique()

    }else if(line_type %in% "cd_up"){
      filepath_elements <- strsplit(current_dir, split = "/")[[1]]
      current_dir <- paste(filepath_elements[1:(length(filepath_elements)-1)], collapse = "/")
      in_list <- FALSE
    }

  }
  return(system_structure)
}

get_directory_size <- function(this_dir, system_structure){

  if(is.null(system_structure[[this_dir]][["files"]])){
    direct_size <- 0L
  }else{
    direct_size <- sum(as.integer(system_structure[[this_dir]][["files"]]))
  }

 return(direct_size)
}

populate_directory_size <- function(system_structure){
  for (this_dir in names(system_structure)) {
    print(this_dir)

    direct_size <- get_directory_size(this_dir, system_structure)
    system_structure[[this_dir]][["size"]] <- system_structure[[this_dir]][["size"]] + direct_size

    current_dir <- paste(filepath_elements[1:(length(filepath_elements)-1)], collapse = "/")

    if(this_dir != "ROOT"){
      filepath_elements <- strsplit(this_dir, split = "/")[[1]]
      for (i in 1:(length(filepath_elements)-1)) {
        this_parent_dir <- paste(filepath_elements[1:i], collapse = "/")
        system_structure[[this_parent_dir]][["size"]] <- system_structure[[this_parent_dir]][["size"]] + direct_size
      }
    }

  }
  return(system_structure)
}

# example_structure <- read_structure("day7_example.txt")
# example_structure <- populate_directory_size(example_structure)
# size_vector <- names(example_structure) %>%
#   sapply(function(x){example_structure[[x]][['size']]})
#
# sum(size_vector[size_vector < 100000])


real_structure <- read_structure("day_7_input.txt")
real_structure <- populate_directory_size(real_structure)

size_vector <- names(real_structure) %>%
  sapply(function(x){real_structure[[x]][['size']]})

sum(size_vector[size_vector < 100000])
