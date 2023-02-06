library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(magrittr)

set_work_dir <- function() {
  main_dir <- "~/Documents/R_files"
  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(main_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(main_dir, out_dir)
  # dir.create(full_path_to_out_dir)
  setwd(full_path_to_in_dir)

  my_paths <- list("inputs" = full_path_to_in_dir,
    "outputs" = full_path_to_out_dir)
  return(my_paths)
}

# csv_names_list = list("report1".csv", "report2.csv")

load_csv_names <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, x))

  contents <- sapply(myfiles, read.csv, header = TRUE, simplify = FALSE)

  return(contents)
}

clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    str_replace_all("\\s", "") %<>%
    str_replace_all("\\.", "") %<>% tolower()
  return(my_df)
}