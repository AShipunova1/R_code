# Set up ----

install.packages("devtools")
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions@development")
library(auxfunctions)

my_paths <- set_work_dir()

# get this project name
current_project_dir_name <- this.path::this.dir()

# find its base name
current_project_name <-
  basename(current_project_dir_name)

# use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
get_data_file_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_file_path)

source(get_data_file_path)

