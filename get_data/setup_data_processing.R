# files to have:
# from "Logbook Processing (Do this before all Logbook Analyses)" google drive
# https://drive.google.com/drive/folders/18ociLUchXpLxrhb3-gJRuIV_0PaQGUFy?usp=sharing
# 1) "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv"
# 2) "2023SRHSvessels.csv"

# The tidyverse is a collection of R packages that work together seamlessly for data manipulation, visualization, and analysis. It includes popular packages like dplyr, ggplot2, tidyr, and more, all designed to follow a consistent and "tidy" data processing philosophy.
library(tidyverse)

library(readxl)
# help functions for get data ----
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories ----

get_current_file_directory <-
  function() {
    dirname(rstudioapi::getSourceEditorContext()$path)
  }

# change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()

  # for others
  add_dir <- ""
  # for Anna's computer
  if (get_username() == "anna.shipunova") {
    add_dir <- "R_files_local/test_dir"
  }

  # add an empty or Anna's folder in front
  main_r_dir <- file.path(add_dir, "SEFHIER/R code")

  in_dir <- "Inputs"
  # file.path instead of paste, because it provides correct concatenation, "\" or "/" etc.
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "Outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # git_r_dir <- "R_code_github"
  # full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir) #,
                   #"git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

set_work_dir_local <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"

  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  git_r_dir <- "R_code_github"
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

if (get_username() == "anna.shipunova") {
  set_work_dir <- set_work_dir_local
}

# Use my function in case we want to change the case in all functions
my_paths <- set_work_dir()

# ===
# The fix_names function is used to clean and standardize column names to make them suitable for use in data analysis or further processing.
# to use in a function,
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  # Use the pipe operator %>%
  x %>%

    # Remove dots from column names
    str_replace_all("\\.", "") %>%

    # Replace all characters that are not letters or numbers with underscores
    str_replace_all("[^A-z0-9]", "_") %>%

    # Ensure that letters are only in the beginning of the column name
    str_replace_all("^(_*)(.+)", "\\2\\1") %>%

    # Convert column names to lowercase using 'my_headers_case_function'
    my_headers_case_function()
}
