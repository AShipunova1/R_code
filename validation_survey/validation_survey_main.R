# Set up ----

# install.packages("devtools")
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions@development")
                         # , 
                         # force = T)
library(auxfunctions)
library(lubridate)

Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

my_paths <- auxfunctions::set_work_dir()

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

# Data are in:
# survey_data_l_2022
# processed_logbooks_2022
# db_logbooks_2022

# prepare data ----

source(file.path(current_project_dir_name, "validation_survey_prepare_data.R"))

glimpse(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short)

# plots ----
# source(file.path(current_project_dir_name, "validation_survey_plots.R"))

# compare field names ----
# source(file.path(current_project_dir_name, "validation_survey_fields.R"))

# Catch ----

## add catch_info to compare
if (!require('taxize')) {
  install.packages('taxize')
  library('taxize')
}
