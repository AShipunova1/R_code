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

# get data ----
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
            paste0(current_project_name, "_", "maximize_join.R"))

file.exists(prepare_data_path)

source(prepare_data_path)

# result is in lgb_join_i1__int_lgb

# print_df_names(lgb_join_i1__int_lgb)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, TRIP_START_DATE, TRIP_START_TIME, TRIP_END_DATE, TRIP_END_TIME, trip_end_date_only, trip_start_hour, trip_start_sec, trip_end_hour, trip_end_sec, trip_start_date_time, trip_end_date_time, id_code, vessel_name, interviewee_f_name, interviewee_l_name, survey_vessel_id, st_2, cnty_3, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, VESSEL_NAME, FIRST_NAME, LAST_NAME, county_short, state_code, state_name, county_code, vessel_id_dist, vsl_names_dissim, st_pass, cnty_pass, name_pass, vsl_name_pass, int_lgb"

# Answer questions ----
## did they interview before or after logbook submission ----

# Quantify when surveyors are intercepting vessels at the dock, in relation to when the trip is transmitted. The specific question is, did they interview before or after logbook submission?  Using the matched surveys to logbooks and transmission date/time vs survey data/time fields, could you please quantify how many surveys occur before the logbook is transmitted vs how many surveys occur after the logbook is transmitted? You can do this as a % of the total (matched) surveys (e.g. 40% of surveys occurred before logbook was submitted; 60% after)

# Note. trip_end_date_only == interview_date

lgb_join_i1__int_lgb__has_lgb_short <-
  lgb_join_i1__int_lgb |>
  filter(int_lgb == "has_lgb") |>
  select(TRIP_ID,
         VESSEL_OFFICIAL_NBR,
         id_code,
         survey_vessel_id,
         trip_end_date_only) |>
  distinct()

survey_data_time <-
  survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num |>
  select(id_code,
         interview_date,
         interview_date_time,
         survey_vessel_id) |> 
  mutate(id_code = as.character(id_code))

# Note. TRANSMISSION_DATE is missing in most entries and often is before the trip date, and TRIP_DE seems more accurate, will use it instead.

logbooks_transmission_time_short <-
  db_logbooks_2022_clean_vesl_clean |>
  select(TRIP_ID,
         VESSEL_OFFICIAL_NBR,
         TRIP_START_DATE,
         TRIP_END_DATE,
         TRIP_DE
         ) |>
  distinct() |> 
  remove_empty_cols()

dim(logbooks_transmission_time_short)
# [1] 94870     5

# View(logbooks_transmission_time_short)

# check the trip year
logbooks_transmission_time_short |> 
filter(!lubridate::year(TRIP_START_DATE) == "2022") |> 
  filter(!lubridate::year(TRIP_END_DATE) == "2022")
# 0
# OK, at least one date in 2022

# check dates
logbooks_transmission_time_short |> 
    filter(lubridate::date(TRIP_DE) < lubridate::date(TRIP_START_DATE)) |> 
  nrow()
# 7

logbooks_transmission_time_short |> 
    filter(is.na(TRIP_DE)) |> 
  nrow()
# 0

### combine data for transmission date ----
lgb_int__join_lgb_de <- 
  lgb_join_i1__int_lgb__has_lgb_short |> 
  left_join(logbooks_transmission_time_short,
            join_by(TRIP_ID, VESSEL_OFFICIAL_NBR))

dim(lgb_join_i1__int_lgb__has_lgb_short)
# [1] 1353    5

dim(lgb_int__join_lgb_de)
# [1] 1353    8

# Add survey time
lgb_int__join_lgb_de__surv_time <- 
  lgb_int__join_lgb_de |> 
  left_join(survey_data_time,
            join_by(id_code, survey_vessel_id))

dim(lgb_int__join_lgb_de__surv_time)
# [1] 1353   10

# Michelle:
# Yes, I’d like to see the difference in mins in both directions. If the logbook was submitted before the survey you could list those in negative mins, and if it was after then in positive mins. If it was seconds after then just round even 1 sec up to the min. 
# 
# Then we can do some summary stats on that: min, max, mean, median

# Find the time difference
lgb_int__join_lgb_de__surv_time__diff <-
  lgb_int__join_lgb_de__surv_time |>
  mutate(trip_de__interview_diff =
           difftime(TRIP_DE, interview_date_time, units = "mins")) |>
  mutate(trip_de__interview_diff_num =
           as.numeric(trip_de__interview_diff, units = "mins")) |>
  mutate(trip_de__interview_diff_dur =
           duration(trip_de__interview_diff_num, "minutes"))

View(lgb_int__join_lgb_de__surv_time__diff)

# stats
summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num)
# Min.:-447.5
# 1st Qu.:49.5
# Median:167.4
# Mean:9156.8
# 3rd Qu.:446.3
# Max.:396382.8
# NA's   :159

lgb_int__join_lgb_de__surv_time__diff |> 
  filter(trip_de__interview_diff_num > 9156) |> glimpse()

summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_dur)
# Min.                          1st Qu. 
# "-26852s (~-7.46 hours)"      "2972.25s (~49.54 minutes)" 
# Median                             Mean 
# "10043.5s (~2.79 hours)" "549406.476549414s (~6.36 days)" 
# 3rd Qu.                             Max. 
# "26780.5s (~7.44 hours)"       "23782965s (~39.32 weeks)" 
# NA's 
# "159" 

summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num)

# cut(ages, breaks = c(20, 30, 40, 50), right = FALSE)

q_limits <-
  quantile(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num,
           na.rm = TRUE)


rr <-
  lgb_int__join_lgb_de__surv_time__diff |>
  select(VESSEL_OFFICIAL_NBR, trip_de__interview_diff_num) |>
  mutate(diff_groups =
           cut(trip_de__interview_diff_num, q_limits))
  
# , include.lowest = TRUE
# View(rr)    

rr |> 
    group_by(diff_groups) |> 
    count(VESSEL_OFFICIAL_NBR) |> glimpse()

lgb_int__join_lgb_de__surv_time__diff |> 
  mutate(diff_groups = 
           cut(trip_de__interview_diff_num,
                           ))
  select(VESSEL_OFFICIAL_NBR, trip_de__interview_diff_num) |>
  count(trip_de__interview_diff_num)
