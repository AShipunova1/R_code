# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired and were active for the same period as (1);
# 3) the grace period is 7 days back from today.
# 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# ----set up----

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
library(diffdf)
# library(RColorBrewer)

my_paths <- set_work_dir()

current_project_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

current_project_name <- current_project_basename

all_inputs <- my_paths$inputs

my_year1 <- "2023"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2024"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

data_file_date <- today()
  lubridate::ymd("2024-02-21")
  
number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7
# 182

grace_period = 7 #days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period
# [1] "2023-08-11"

# 30 days from today
permit_expired_check_date <- data_file_date + 30

last_week_start <- data_file_date - grace_period
# [1] "2024-02-10"
  
# get_data ----

get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)

