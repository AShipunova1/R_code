# Set up ----

library('devtools')

# Explanations:
# - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
# - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
# 
# This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
if (!require('auxfunctions')) {
  devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = TRUE)
  
  library('auxfunctions')
}

library(lubridate)
library(ROracle)
library(tidycensus)
library(usmap)
library(stringdist)

## Install and attach R packages for googlesheets ----
# https://felixanalytix.medium.com/how-to-read-write-append-google-sheet-data-using-r-programming-ecf278108691#:~:text=There%20are%203%20ways%20to%20read%20this%20Google%20sheet%20into%20R.&text=Just%20to%20take%20the%20URL,URL%20but%20just%20the%20ID).
if (!require(googlesheets4)) install.packages("googlesheetsa") 
if (!require(googledrive)) install.packages("googledrive") 
library(googlesheets4) # Google Sheets via the Sheets API v4 
library(googledrive) # interact with Google Drive 

# ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggplot2)

#' don't convert long numbers to scientific notation
options(scipen = 999)

#' Keep the same timezone across subsystems
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

#' Change the following 2 lists to your environment if needed. The variable names are used throughout the code, so please change only the values inside the lists.

if (auxfunctions::get_username() == "anna.shipunova") {
  # If the condition is true, use Anna's directory structure
  my_paths <- auxfunctions::set_work_dir()
  current_in_out_paths <- auxfunctions::current_project_paths()
  
} else {
  
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  #' 1) General directories (to look up additional files, e.g. processed data)
  my_paths <- list(inputs  = "~/my_inputs",
                   outputs = "~/my_outputs",
                   git_r   = "~/R_code")
  
  #' 2) Current project code, input and output directories
  current_in_out_paths <-
    list(
      project_name = "validation_survey",
      code = "~/validation_survey/code",
      input = "~/validation_survey/input",
      output = "~/validation_survey/output"
    )
}

#' The following section uses provided directory names lists to create separate variables for future use and create current input/output directories if they do not exists.
#' 

current_project_name <- current_in_out_paths$project_name

current_project_dir_name <- current_in_out_paths$code
            
curr_proj_input_path <- current_in_out_paths$input

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- current_in_out_paths$output

auxfunctions::create_dir_if_not(curr_proj_output_path)

# Get data ----
#' %%%%% Get data

get_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_path)

source(get_data_path)

#' Data are in:
#' 
#' survey_data_l_2022
#'
#' processed_logbooks_2022
#'
#' processed_logbooks_2022_calendar
#'
#' db_logbooks_2022
#'
#' db_dnfs_2022
#'
#' permit_info_from_db
#'
#' permits_from_pims__split1_short__split2
#'
#' vessels_from_pims_double_bind
#'
#' vessel_permit_owner_from_db
#'
#' vesl_suppressed_logbooks_clean_2022
#' 
# ---

# Prepare data ----
prepare_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "prepare_date.R"))

file.exists(prepare_data_path)

source(prepare_data_path)

# Prepared data are in: 
# lgb_join_i1__int_lgb
# vms_raw_data_all_clean

print_df_names(lgb_join_i1__int_lgb)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, TRIP_START_DATE, TRIP_START_TIME, TRIP_END_DATE, TRIP_END_TIME, trip_end_date_only, trip_start_hour, trip_start_sec, trip_end_hour, trip_end_sec, trip_start_date_time, trip_end_date_time, id_code, vessel_name, interviewee_f_name, interviewee_l_name, survey_vessel_id, st_2, cnty_3, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, VESSEL_NAME, FIRST_NAME, LAST_NAME, county_short, state_code, state_name, county_code, vessel_id_dist, vsl_names_dissim, st_pass, cnty_pass, name_pass, vsl_name_pass, int_lgb"

# Answer questions ----

## How many interview have logbooks ----
# Note. trip_end_date_only == interview_date

lgb_join_i1__int_lgb__short <-
  lgb_join_i1__int_lgb |>
  select(TRIP_ID,
         VESSEL_OFFICIAL_NBR,
         id_code,
         survey_vessel_id,
         trip_end_date_only,
         int_lgb) |>
  distinct()

lgb_join_i1__int_lgb__short |> 
  count(int_lgb)
# 1 has_lgb  1353
# 2 no_lgb   1096

1353 + 1096 == 2449

# 1353*100/(1353 + 1096)
# 55.2% has_lgb

# 1096*100/(1353 + 1096)
# 44.7 no lgb

lgb_join_i1__int_lgb__short |>
  data_overview()

n_distinct(lgb_join_i1__int_lgb__short$TRIP_ID)
# 1161 logbooks

n_distinct(lgb_join_i1__int_lgb__short$id_code)
# 1835 interviews

## Time difference between interview and logbook submission ----
int_before_lgb_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "int_before_lgb.R"))

file.exists(int_before_lgb_path)

# uncomment to run
# source(int_before_lgb_path)

## Maps ---- 
# - overlay map of where surveys occurred without logbooks (in Gulf) with where non-compliant Gulf + dual Gulf/S. Atl. vessels (for all 2022) are located

## make a df with needed fields
lgb_join_i1__int_lgb__short_for_map <-
  lgb_join_i1__int_lgb |>
  select(
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    id_code,
    survey_vessel_id,
    trip_end_date_only,
    int_lgb,
    st_2,
    cnty_3,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    county_short,
    state_code,
    state_name,
    county_code,
    int_lgb
  ) |>
  distinct()

lgb_join_i1__int_lgb__short_for_map |> dim()
# 2455

lgb_join_i1__int_lgb__short_for_map_no_lgb <-
  lgb_join_i1__int_lgb__short_for_map |>
  filter(int_lgb == "no_lgb" &
           !is.na(VESSEL_OFFICIAL_NBR)) |>
  select(-int_lgb, -TRIP_ID,
         -starts_with("SERO_HOME_PORT")) |>
  distinct()

dim(lgb_join_i1__int_lgb__short_for_map_no_lgb)
# 966

glimpse(lgb_join_i1__int_lgb__short_for_map_no_lgb)

maps_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "maps.R"))

file.exists(maps_path)

# source(maps_path)
