# dual_landing_2023.R

# setup ----
# 2022, 2023
# dual + SA
# library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
# library(zoo)   # Load the 'zoo' library, which deals with time series data.
# library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
# library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

# Read R Code from a File
source("~/R_code_github/useful_functions_module.r")

my_year1 <- "2022"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2023"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions' definition.
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

dir.create(curr_proj_output_path)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

dir.create(curr_proj_input_path)

project_name <- current_project_basename
# "dual_landing_2023"

# get data ----
dual_landing_2023_get_data_path <- 
  file.path(current_project_dir_name,
            "dual_landing_2023_get_data.R")

source(dual_landing_2023_get_data_path)
