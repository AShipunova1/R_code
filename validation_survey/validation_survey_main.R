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

# compare vessel names ----
unify_names <- function(column_name) {
  tolower(column_name) |> 
    stringr::str_replace("\\s", "")
}

catch_info_i3 |>
  filter(!unify_names(vessel_name) == unify_names(VESSEL_NAME)) |>
  select(VESSEL_OFFICIAL_NBR, vsl_num, VESSEL_NAME, vessel_name) |>
  distinct() |>
  glimpse()
# Rows: 101 w/o spaces
# Rows: 112
# select(VESSEL_OFFICIAL_NBR) |> 
# 79 unique vessels

# filter(!VESSEL_OFFICIAL_NBR == vsl_num)
# 0

# compare TRIP_TYPE_NAME, operating_type ----

# 6=’HB’, 7=’CB’, 0=’Neither’
# catch_info_i3$TRIP_TYPE_NAME |> unique()
# [1] "CHARTER" "UNKNOWN"

catch_info_i3 |> 
  select(VESSEL_OFFICIAL_NBR, TRIP_TYPE_NAME, operating_type) |>
  distinct() |>
  mutate(surv_trip_type = case_when(operating_type == 6 ~ "headboat",
                                    operating_type == 7 ~ "CHARTER",
                                    operating_type == 0 ~ "Neither")) |> 
  filter(!unify_names(TRIP_TYPE_NAME) == unify_names(surv_trip_type)) |>
  glimpse()
# 16
# same type "CHARTER" 225

# compare NUM_ANGLERS, people_fishing ----
# catch_info_i3$people_fishing |> unique()

compare_fields <- 
  c("NUM_ANGLERS", "people_fishing")

catch_info_i3 |> 
  select(VESSEL_OFFICIAL_NBR, all_of(compare_fields)) |>
  distinct() |>
  rowwise() |> 
  # filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  # 135
  # filter(as.integer(!!sym(compare_fields[[1]])) > as.integer(!!sym(compare_fields[[2]]))) |>
  # 90
  # filter(as.integer(!!sym(compare_fields[[1]])) < as.integer(!!sym(compare_fields[[2]]))) |>
  # 45
  filter(abs(as.integer(!!sym(compare_fields[[1]])) - as.integer(!!sym(compare_fields[[2]]))) > 1) |>
# 68
  dim()

# compare ACTIVITY_TYPE_NAME	no_harvested_selected

# ACTIVITY_TYPE_NAME
# [1] "TRIP WITH EFFORT"
# catch_info_i3$no_harvested_selected |> unique()
# 1, 2 (1=YES, 2=NO)

compare_fields <- 
  c("ACTIVITY_TYPE_NAME", "no_harvested_selected")

catch_info_i3 |>
  select(all_of(compare_fields)) |>
  distinct() |>
  glimpse()
# $ ACTIVITY_TYPE_NAME    <chr> "TRIP WITH EFFORT", "TRIP WITH EFFORT"
# $ no_harvested_selected <int> 2, 1

# compare DISTANCE_CODE_NAME	fishing_distance ----

compare_fields <-
  c("DISTANCE_CODE_NAME", "fishing_distance")

catch_info_i3 |>
  select(all_of(compare_fields)) |>
  distinct() |>
  glimpse()
