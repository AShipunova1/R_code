# nolint: commented_code_linter.
#---
# How to use:
# source("~/GitHub/R_code/start_module.R")
# my_paths <- set_work_dir()
# csv_names_list = list("report1.csv", "report2.csv")
# xls_names_list = list("report1a.xls", "report2a.xls")
# csv_content_1 <- load_csv_names(my_paths, csv_names_list)[[1]]
# xls_content_1 <- load_xls_names(my_paths, xls_names_list)[[1]]

#---

library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(magrittr)
library(readxl)  # reading in .xlsx

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

load_xls_names <- function(my_paths, xls_names_list, sheet_num = 1) {
  my_inputs <- my_paths$inputs

  myfiles <- sapply(xls_names_list, function(x) file.path(my_inputs, x))

  # xls_content_1 <- read_excel(paste(my_paths$inputs,
  # xsl_names_list[[1]],
  # sep = "/"), 1)  # nolint: commented_code_linter.

  contents <- sapply(myfiles, read_excel, sheet_num)

  return(contents)
}

clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    str_replace_all("\\s", "_") %<>%
    str_replace_all("\\.", "") %<>%
    tolower()
  return(my_df)
}