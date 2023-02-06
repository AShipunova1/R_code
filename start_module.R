#---
# How to use:
# source("~/GitHub/R_code/start_module.R")
# my_paths <- set_work_dir()
# csv_names_list = list("report1".csv", "report2.csv")
# csv_content_1 <- load_csv_names(my_paths, csv_names_list)[[1]]
#---

library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(magrittr)

set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"
  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)
  # dir.create(full_path_to_out_dir)
  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
    "outputs" = full_path_to_out_dir)
  return(my_paths)
}

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