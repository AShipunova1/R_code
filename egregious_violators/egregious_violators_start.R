#' title: Egregious Violators

#' %%%%% Set Up
#' 

# The "egregious violator" definition ----
#' 
#' 1) NO reports for all 26 weeks back from week ago today;
#' 
#' 2) permits have not expired and were active for the same period as (1);
#' 
#' 3) the grace period is 7 days back from today.
#' 
#' 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.
#' 
#' 5) not counting any correspondence (regardless of the type - email/call, voicemail or not) that includes "No contact made" in the text of the entry as a actual "direct" contact for any egregious vessel (May 6 2024)

#' NB. Update (download) all input files every time before run.
#' 

## new requirement 2023-08-09 ----
#' It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.
#' at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough
#' 

## new requirement 2024-02-26 ----
#' It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.
#' 

## new requirement 2024-05-06 ----
#' Exclude any correspondence (regardless of the type - email/call, voicemail or not) that includes "No contact made" in the text of the entry as a actual "direct" contact for any egregious vessel.
#' 

# set up ----

#' Install if needed and load all the packages
#' NB. It is better to install/load each package separately, if any one suggest updates it is safe to choose option 1 (update all)

#' Instantiate an Oracle client from the current R session
library(ROracle)
#' Collection of package development tools.
library(devtools)

#' Explanations:
#' - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#'   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#'   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
#' - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#'   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#'   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
#' 
#' This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
#' One doesn't have to have a github account to use it.
#' Check the username
#' 
if (!auxfunctions::get_username() == "anna.shipunova") {
  if (!require('auxfunctions')) {
    devtools::install_github("AShipunova1/R_code/auxfunctions")
  }
} else {
  # For Anna Shipunova
  # rebuild the package
  devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = TRUE)
  # restart R session to pick up changes
  # .rs.restartR()
}

#' Auxiliary functions for SEFHIER data analysis.
library(auxfunctions)
#' Helps with time series.
library(zoo)
#' Compares 2 dataframes and outputs any differences.
library(diffdf)

## Set up paths ----

#' Change the following 2 lists to your environment if needed. The variable _names_ are used throughout the code, so please change only the quoted _values_ inside the lists.

if (!auxfunctions::get_username() == "anna.shipunova") {
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  #' 1) General directories (to look up additional files, e.g. processed data). It can be left as is if you don't have it.
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
  
} else {
  # If the username is "anna.shipunova", use Anna's directory structure.
  my_paths <- auxfunctions::set_work_dir()
  current_in_out_paths <- auxfunctions::current_project_paths()
}

#' The following section uses provided directory names lists to automatically create separate variables for future use and create current input/output directories if they do not exists.

#' This is usually the current directory name
current_project_name <- current_in_out_paths$project_name

current_project_path <- current_in_out_paths$code
            
current_project_input_path <- current_in_out_paths$input

auxfunctions::create_dir_if_not(current_project_input_path)

current_project_output_path <- current_in_out_paths$output

auxfunctions::create_dir_if_not(current_project_output_path)

### Additional individual paths to data files ----
#### Compliance and Correspondence ----
#' 
#' Download from FHIER first.
#' 
#' Provide full paths here, changing _values_ inside the quotes:
#' 
correspondence_csv_path <- "Your full path to correspondence.csv"
fhier_compliance_csv_path_list <- 
  list("Your full path to fhier_compliance.csv year 1",
       "Your full path to fhier_compliance.csv year 2")

#' Depending on a user name who runs the code, the file paths are constructed here.
if (!auxfunctions::get_username() == "anna.shipunova") {
  all_csv_full_paths_list <- c(correspondence_csv_path, fhier_compliance_csv_path_list)
} else {
  #' For Anna Shipunova
  #' Change file names to the last download
  all_csv_names_list = c(
    "Correspondence_2024_06_17.csv",
    r"(2024_06_17\FHIER_Compliance_2023__06_17_2024.csv)",
    r"(2024_06_17\FHIER_Compliance_2024__06_17_2024.csv)"
  )
  
  #' add a full path in front of each file name
  corresp_full_path <-
    prepare_csv_full_path(all_csv_names_list[[1]],
                          add_path = "from_Fhier/Correspondence",
                          input_dir_part = my_paths$inputs)
  
  compliance_full_paths <-
    auxfunctions::prepare_csv_full_path(all_csv_names_list[2:3],
                                        add_path = "from_Fhier/FHIER Compliance",
                                        input_dir_part = my_paths$inputs)
  
  all_csv_full_paths_list <-
    c(corresp_full_path,
      compliance_full_paths)

  # check if files exist
  purrr::map(all_csv_full_paths_list, file.exists)
}

#### Processed Metric Tracking (permits from FHIER) ----
#' 
#' Add your full path to processed Metrics tracking for each year instead of "Your full path here"
#' 
if (!auxfunctions::get_username() == "anna.shipunova") {
  processed_metrics_tracking_file_names <- 
    c("Your full path here/SEFHIER_permitted_vessels_nonSRHS_2022.rds",
      "Your full path here/SEFHIER_permitted_vessels_nonSRHS_2023.rds")
} else {
  # for Anna Shipunova
  processed_input_data_path <-
    file.path(my_paths$inputs, "processing_logbook_data", "Outputs")

  #' check
  dir.exists(processed_input_data_path)
  #' if not TRUE: Check your provided path and/or create manually.
  
  #' Get file names for all years
  processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

  #' Exclude links
  processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

}

#' Check if provided paths are correct
purrr::map(processed_metrics_tracking_file_names, file.exists)
#' if not TRUE: Check your provided path and/or create manually.

#### Physical Address List from FHIER ----
#' Download first from REPORTS / For-hire Primary Physical Address List
#' 
#' Add your full path instead of "Your full path here"
#' 
if (!auxfunctions::get_username() == "anna.shipunova") {
  fhier_addresses_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  fhier_addresses_path <-
    file.path(
      my_paths$inputs,
      r"(from_Fhier\address\For-hire Primary Physical Address List_06_17_2024.csv)"
    )
}

#' Check, correct the path if it is doesn't exist
file.exists(fhier_addresses_path)

#### home port processed city and state ----
#' Download first from Google drive
#' 
#' Add your full path instead of "Your full path here"
#' 
if (!auxfunctions::get_username() == "anna.shipunova") {
  processed_pims_home_ports_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  processed_pims_home_ports_path <-
    file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports_2024-06-18.csv")
}

# Check, correct the path if it is doesn't exist
file.exists(processed_pims_home_ports_path)

#### Data from the previous results of "egregious violators for investigation" ----
# Download first as .xlsx from Google drive

#' Add your full path instead of "Your full path here"
#' 
if (!auxfunctions::get_username() == "anna.shipunova") {
  prev_result_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  prev_result_path <-
    file.path(current_project_input_path,
              "egregious_violators_to_investigate_2024-05-17.xlsx")
}

# Check, correct the path if it is doesn't exist
file.exists(prev_result_path)

## Define dates ----

#' my_year1 and my_year2 values might be changed
#' start year for the analysis
my_year1 <- "2023"
my_beginning1 <- stringr::str_glue("{my_year1}-01-01")
my_end1 <- stringr::str_glue("{my_year1}-12-31")

#' last year for the analysis
my_year2 <- "2024"
my_beginning2 <- stringr::str_glue("{my_year2}-01-01")
my_end2 <- stringr::str_glue("{my_year2}-12-31")

#' Following are the definitions of dates used throughout the code.
data_file_date <- 
  lubridate::today()
  
#' How many weeks and days to take in to the account?
number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7

#' test
days_in_non_compl_weeks == 182

grace_period = 7 # days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period

#' check week and day of the period's start, can compare with a calendar
lubridate::week(half_year_ago)

lubridate::wday(half_year_ago, label = T)

#' Permit expiration minimum is 30 days from today
permit_expired_check_date <- data_file_date + 30

#' We will not use the last week data
last_week_start <- data_file_date - grace_period

# get_data ----
#' %%%%% Prepare data
#' 

get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)

#' Data are in:
#' compl_clean
#' corresp_contact_cnts_clean0
#' processed_metrics_tracking_permits
#' fhier_addresses
#' processed_pims_home_ports
#' db_participants_address
#' permit_vessel_w_changed_owner
#' prev_result
# 

# Preparing compliance info ----

## Permit Expiration ----
### add permit_expired column ----
#' Explanations:
#' 
#' 1. Add a new column 'permit_expired' using 'mutate'.
#' 
#' 2. Use 'case_when' to determine if 'permit_groupexpiration' is greater than permit_expired_check_date.
#' 
#' 3. If true, set 'permit_expired' to "no", otherwise set it to "yes".
#' 

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is after permit_expired_check_date than "not expired"
  dplyr::mutate(permit_expired =
           dplyr::case_when(
             permit_groupexpiration > permit_expired_check_date ~ "no",
             .default = "yes"
           ))

# glimpse(compl_clean_w_permit_exp)

### get only not expired last 27 weeks of data minus grace period (total 26 weeks) ----
compl_clean_w_permit_exp__not_exp <-
  compl_clean_w_permit_exp |>
  # the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_end < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

#' Check if the dates make sense
min(compl_clean_w_permit_exp__not_exp$permit_groupexpiration)
# E.g.
# [1] "2024-08-31 EDT"

min(compl_clean_w_permit_exp__not_exp$week_start)
# E.g.
# [1] "2024-01-08"

max(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2024-06-17"

max(compl_clean_w_permit_exp__not_exp$week_end)
# [1] "2024-06-23"

### add year_month column from week_start ----

compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp__not_exp |>
  dplyr::mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the current check period
  dplyr::filter(year_month >= as.yearmon(half_year_ago))

#' Check
dim(compl_clean_w_permit_exp)
# [1] 221081     21

dim(compl_clean_w_permit_exp_last_half_year)
# [1] 57296    22

### Have only SA and dual permits ----
#' Use 'filter' to select rows where 'permitgroup' contains "CDW", "CHS", or "SC".
compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  dplyr::filter(grepl("CDW|CHS|SC", permitgroup))

# check 
dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 38761    22

### Keep fewer columns ----
remove_columns_from_compliance <- c(
  "name",
  "gom_permitteddeclarations__",
  "captainreports__",
  "negativereports__",
  "complianceerrors__",
  "set_permits_on_hold_",
  "override_date",
  "override_by",
  "contactedwithin_48_hours_",
  "submittedpower_down_",
  "permit_expired"
)

#' Explanations:
#' 
#' 1. Use 'select' to remove columns specified in 'remove_columns'.
#' 
#' 2. Use 'distinct' to keep only unique rows in the resulting data frame.
#' 
compl_clean_w_permit_exp_last_half_year__sa__short <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::select(-tidyselect::any_of(remove_columns_from_compliance)) |> 
  dplyr::distinct()

# Check
dim(compl_clean_w_permit_exp_last_half_year__sa__short)
# [1] 38761    11

#' Work with the whole period
#' 

### add compliant_after_overr ----
#' use tictoc package for benchmarking 

tictoc::tic("compl_overr")
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr <-
  compl_clean_w_permit_exp_last_half_year__sa__short |>
  auxfunctions::add_compliant_after_override(overridden_col_name = "overridden_",
                                             compliance_col_name = "compliant_")
tictoc::toc()
# compl_overr: 8.76 sec elapsed

#' check compliant/overridden combinations' counts
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |> 
  dplyr::select(compliant_, overridden_, compliant_after_override) |>
  dplyr::count(compliant_, overridden_, compliant_after_override)
# E.g.
#   compliant_ overridden_ compliant_after_override     n
#   <chr>      <chr>       <chr>                    <int>
# 1 NO         NO          no                       10768
# 2 NO         YES         yes                        199
# 3 YES        NO          yes                      27794

#' check the results
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$compliant_after_override |>
  unique() == c("yes", "no")

dim(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr)
# E.g.
# [1] 38761    12

#' check if the amount of vessels didn't change
dplyr::n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number) ==
  dplyr::n_distinct(
    compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$vessel_official_number
  )
# T

### get only non-compliant entries for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |>
  # not compliant
  dplyr::filter(tolower(compliant_after_override) == "no")

# check
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)
# E.g.
# [1] 10768    12

### keep only vessels with info for all weeks in the period ----

#' That should eliminate entries for vessels having permits only a part of the period
 
#' get the current number of weeks 
all_weeks_num <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  dplyr::select(week) |>
  dplyr::distinct() |>
  nrow()

#' Explanations:
#'
#' 1. Group the data frame by 'vessel_official_number'.
#'
#' 2. Filter the groups based on the condition that the number of distinct weeks is greater than or equal to 'all_weeks_num'.
#'
#' 3. Remove the grouping from the data frame.
#'
#' 4. Exclude the 'week' column from the resulting data frame, we don't need it anymore.
#' 

compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(dplyr::n_distinct(week) >= all_weeks_num) |> 
  dplyr::ungroup() |> 
  dplyr::select(-week)

#' Check how many entries were removed
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)
# E.g.
# [1] 10768    12

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present)
# E.g.
# [1] 3278   11

### check the last report date ----
#### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

#' check vessel's number
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids)
#' E.g.
#' 149 

#### check these ids in the full compliance information ----
#' Is there a new submitted report?
compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids$vessel_official_number
  ) |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(
    tolower(compliant_) == "yes" &
      tolower(overridden_) == "yes" &
      # not the current month
      year_month < as.yearmon(data_file_date)
  ) |>
  nrow()
#' 0 OK!
#' 

#' End of Compliance preparations 
#' Results: processed Compliance is in compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present
#' 

# Preparing Correspondence ----

## remove 999999 ----
#' Use 'filter' to select rows where 'vessel_official_number' does not start with "99999".
#' 
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  dplyr::filter(!grepl("^99999", vessel_official_number))

#' check number of vessels in correspondence 
dplyr::n_distinct(corresp_contact_cnts_clean$vesselofficial_number)
#' E.g.
#' 4281

#' check
corresp_contact_cnts_clean |>
  dplyr::select(calltype, voicemail, contacttype) |>
  dplyr::distinct() |> head(10)

## Correspondence Filters ----
#' The functions below are creating filter conditions using quosures. Quosures are a part of tidy evaluation in R, allowing expressions to be captured without evaluation, which is useful for creating functions with flexible inputs.

#' Explanations:
#' 
#' **Expression inside quo()**:
#'
#'Check if any entry was an "outgoing call"
#'
we_called_filter <-
  dplyr::quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))

#' Explanations:
#' 
#' **Expression inside quo()**:
#'
#'Check if any entry was an "outgoing email or other"
#'
we_emailed_once_filter <-
  dplyr::quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outgoing"
  ))

#' Explanations:
#' 
#' **Expression inside quo()**:
#'
#'    - `!grepl("No contact made", contactcomments, ignore.case = TRUE)`: This expression is a negation of the `grepl` function, which is used to search for a pattern ("No contact made") in the `contactcomments` column.
#'
#'    - `grepl()` returns `TRUE` for each element of `contactcomments` that contains the pattern, and `FALSE` otherwise.
#'
#'    - The `!` operator negates the result, so the filter condition will be `TRUE` for rows where "No contact made" is not found in the `contactcomments` column.
#' 
#' The `exclude_no_contact_made_filter` function effectively creates a filter condition that can be used to exclude rows where "No contact made" is found in the `contactcomments` column when applied to a dataset.
#' 
exclude_no_contact_made_filter <-
  dplyr::quo(!grepl("No contact made", 
            contactcomments, 
            ignore.case = TRUE))

#' don't need a second contact
#' Explanations:
#' 
#' **Expression inside quo()**:
#'
#' Check if any contact was "incoming"
#'
they_contacted_direct_filter <-
  dplyr::quo(
    any(
      tolower(calltype) == "incoming"
      )
  )

### use the filters ----

#' Explanations:

#' - `corresp_contact_cnts_clean |> ...` starts the pipeline with the data frame `corresp_contact_cnts_clean`.

#' - `dplyr::filter(tolower(calltype) == "incoming" | ...)` filters rows based on the conditions specified:

#'   - `dplyr::filter()` is used to subset rows in the data frame.

#'   - `tolower(calltype) == "incoming"` converts the `calltype` column to lowercase and checks if it equals "incoming".

#'   - The `|` operator means logical OR, so it includes rows where the condition on either side is true.

#'   - `(contact_freq > 1 & (!!we_called_filter & !!we_emailed_once_filter))`:

#'     - `contact_freq > 1` checks if the `contact_freq` column is greater than 1.

#'     - `&` is the logical AND operator, so it requires both conditions to be true.

#'     - `!!we_called_filter` evaluates the `we_called_filter` variable as a logical expression.

#'     - `!!we_emailed_once_filter` evaluates the `we_emailed_once_filter` variable as a logical expression.

#'     - The parentheses around this subexpression ensure it is evaluated as a single logical unit.

#' - `dplyr::filter(!!exclude_no_contact_made_filter)` applies another filter condition:

#'   - `!!exclude_no_contact_made_filter` evaluates the `exclude_no_contact_made_filter` variable as a logical expression.

#' 

#' This code filters the `corresp_contact_cnts_clean` data frame to include rows where either the `calltype` is "incoming" or the `contact_freq` is greater than 1 and both `we_called_filter` and `we_emailed_once_filter` are true. Additionally, it filters rows based on the `exclude_no_contact_made_filter` condition.

corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  dplyr::filter(tolower(calltype) == "incoming" |
           (
             contact_freq > 1 &
               (!!we_called_filter &
                  !!we_emailed_once_filter)
           )) |> 
  dplyr::filter(!!exclude_no_contact_made_filter)

#' check amount of rows before and after filtering
dim(corresp_contact_cnts_clean)
# E.g. [1] 33001    22
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# E.g. [1] 31061    22

#' Check how many vessels left after filtering 
dplyr::n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)
# E.g.
# [1] 3865

## fix dates ----
#' check how the dates look
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1) |> str()
 # chr "02/15/2024 03:15PM"

#'
#' Explanations:
#'
#' Mutate new columns 'created_on_dttm' and 'contact_date_dttm' by parsing 'created_on' and 'contact_date' using lubridate package.
#'
#' The date-time formats considered are "mdY R".
#'
#' 1. Use the pipe operator to pass 'corresp_contact_cnts_clean_direct_cnt_2atmps' as the left-hand side of the next expression.
#'
#' 2. Use 'mutate' to create new columns with parsed date-time values.
#'
#' 3. Use 'lubridate::parse_date_time' to parse the date-time values using the specified formats.
#' 

corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  dplyr::mutate(
    created_on_dttm =
      lubridate::parse_date_time(created_on,
                                 c("mdY R")),
    contact_date_dttm =
      lubridate::parse_date_time(contact_date,
                                 c("mdY R"))
  )

#' check how the dates look now
head(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm, 1) |> 
  str()
# E.g.
# POSIXct[1:1], format: "2024-06-17 15:24:00"

#' End of Correspondence preparations 
#' Processed Correspondence is in 
#' corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates
#' 

# Join correspondence with compliance ----
#'
#' Explanations:
#'
#' Create a new dataframe 'compl_corr_to_investigation' by performing an inner join between
#'
#' 'correspondence' and 'compliance'.
#'
#' The join is performed on the column 'vessel_official_number'.
#'
#' Use 'multiple = "all"' and 'relationship = "many-to-many"' to handle multiple matches during the join.
#'
#' 1. Use the 'inner_join' function from the dplyr package to combine the two dataframes based on the specified columns.
#'
#' 2. Pass the column names and other parameters to the 'by', 'multiple', and 'relationship' arguments.

compl_corr_to_investigation <-
  dplyr::inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

#' check
dim(compl_corr_to_investigation)
# E.g.
# [1] 30844    32
dplyr::n_distinct(compl_corr_to_investigation$vesselofficial_number)
# E.g.
# 141

head(compl_corr_to_investigation) |> 
  glimpse()

## save number of vessels to investigate for further checks ----
num_of_vsl_to_investigate <- 
  dplyr::n_distinct(compl_corr_to_investigation$vesselofficial_number)

#' Results: Compliance & Correspondence joined together are in
#' compl_corr_to_investigation
#' 

# Output needed investigation ----
#' %%%%% Prepare output
#' 
#' 1. Remove unused columns.
#' 2. Create additional columns.
#' 3. Mark vessels already in the know list (prev_result).
#' 4. duals vs. sa_only
#' 

## 1. remove extra columns ----

unused_fields <- c(
  "vesselofficial_number",
  "primary",
  # "contact_date",
  "follow_up",
  "log_group",
  "calltype",
  "voicemail",
  # "contacttype",
  "contact_reason",
  # "contactrecipientname",
  # "contactphone_number",
  # "contactemailaddress",
  "contactcomments",
  "srfhuser",
  "created_on",
  "follow_up_nbr",
  "srhs_vessel",
  # "vessel_official_number",
  "was_contacted",
  "contact_freq",
  "created_on_dttm",
  # "contact_date_dttm",
  # "name",
  # "permit_expired",
  # "permitgroup",
  # "permit_groupexpiration",
  "compliant_after_override")

#'
#' Explanations:
#'
#' 1. Exclude columns specified in 'unused_fields' from the data frame.
#'
#' 2. Group the data frame by 'vessel_official_number'.
#'
#' 3. `summarise_all` applies the function 'concat_unique' to all columns to concatenate unique non-missing values into a single string.
#' 
#' 4. Remove the grouping from the data frame.
#' 

compl_corr_to_investigation_short <-
  compl_corr_to_investigation |>
  dplyr::select(-any_of(unused_fields)) |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::summarise_all(auxfunctions::concat_unique) |>
  dplyr::ungroup()

#' Visual check if data make sense
compl_corr_to_investigation_short |> 
  head() |> 
  dplyr::glimpse()

#' Check if number of vessels didn't change
nrow(compl_corr_to_investigation_short) == num_of_vsl_to_investigate

## 2. create additional columns ----
### add list of contact dates and contact type in parentheses  ----

#' put coumn names into variables (needed, bc spaces and underscores placements vary from source to source)
contactdate_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, "contact", "date")[1]

contacttype_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, "contact", "type")[1]

contactphonenumber_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, ".*contact", "number.*")[1]

#' Explanations:
#' 
#' Define a function 'get_date_contacttype' that takes a dataframe 'compl_corr_to_investigation' as input.
#' 
#' 1. Add a new column 'date__contacttype' by concatenating the values from 'contactdate_field_name' and 'contacttype'.
#' 
#' 2. Select only the 'vessel_official_number' and 'date__contacttype' columns.
#' 
#' 3. Arrange the dataframe by 'vessel_official_number' and 'date__contacttype'.
#' 
#' 4. Keep distinct rows based on 'vessel_official_number' and 'date__contacttype'.
#' 
#' 5. Group the dataframe by 'vessel_official_number'.
#' 
#' 6. Summarize the data by creating a new column 'date__contacttypes' that concatenates all 'date__contacttype' values for each vessel separated by a comma.
#' 
#' 7. Return the resulting dataframe.
#' 
get_date_contacttype <-
  function(my_df) {
  
    res <-
      my_df |>
      # add a new column date__contacttype with contactdate and contacttype
      dplyr::mutate(date__contacttype =
                      paste(
                        !!rlang::sym(contactdate_field_name),
                        !!rlang::sym(contacttype_field_name)
                      )) |>
      # use 2 columns only
      dplyr::select(vessel_official_number, date__contacttype) |>
      dplyr::distinct() |>
      # sort
      dplyr::arrange(vessel_official_number, date__contacttype) |>
      # for each vessel id...
      dplyr::group_by(vessel_official_number) |>
      # ...combine all date__contacttypes separated by comma in one cell
      dplyr::summarise(date__contacttypes =
                         paste(date__contacttype, collapse = ", ")) |> 
      dplyr::ungroup()
    
    return(res)
  }

#' use the function
date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation_short)

#' Check if number of vessels didn't change
nrow(date__contacttype_per_id) == num_of_vsl_to_investigate

#' Check how the result looks like 
date__contacttype_per_id |>
  head() |>
  dplyr::glimpse()

#### add the new column back ----
compl_corr_to_investigation__corr_date <-
  dplyr::left_join(compl_corr_to_investigation_short,
            date__contacttype_per_id) |>
  # Joining with `by = join_by(vessel_official_number)`
  # these columns are not longer needed
  dplyr::select(-dplyr::all_of(c(
    contactdate_field_name,
    contacttype_field_name
  )))
  
#' check, the last column should be like 
#' $ date__contacttypes     <chr> "03/13/2024 11:59AM, 09/21/2023 03:41PM, 08/18/2023 10:52AM,…
compl_corr_to_investigation__corr_date |> 
  head() |> 
  dplyr::glimpse()

### add pims home port info ----

#' change pims home port info column names 
processed_pims_home_ports_renamed <- 
  processed_pims_home_ports |> 
  dplyr::rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)

#' join home ports and compliance/correspondence by vessel id
compl_corr_to_investigation__corr_date__hailing_port <- 
  dplyr::left_join(
    compl_corr_to_investigation__corr_date,
    processed_pims_home_ports_renamed,
    dplyr::join_by(vessel_official_number)
  )

### add prepared addresses ----

prep_addresses_path <-
  file.path(current_project_path,
            stringr::str_glue("{current_project_name}_prep_addresses.R"))

file.exists(prep_addresses_path)

source(prep_addresses_path)

#' Results are in compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr
#' 

## 3. mark vessels already in the know list ----
#' From the email:
#' The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

#' get vessel ids from the previous result 
vessels_to_mark_ids <-
  prev_result |>
  dplyr::select(vessel_official_number)

#' Check the amount 
dim(vessels_to_mark_ids)

#### Mark these vessels ----

#' Explanations:
#'
#' Create a new column 'duplicate_w_last_time' in the dataframe 'compl_corr_to_investigation_short'.
#'
#' This column is marked with "duplicate" for rows where 'vessel_official_number' is present in the list of vessel IDs to mark as duplicates ('vessels_to_mark_ids').
#'
#' For all other rows, it is marked as "new".
#' 

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
  dplyr::mutate(
    duplicate_w_last_time =
      dplyr::case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )


### check ----
#' Check that number of vessels didn't change.
dplyr::n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked$vessel_official_number) ==
  num_of_vsl_to_investigate

#' Check that there is one row per vessel.
nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked) == 
  num_of_vsl_to_investigate 

#' Count how many duplicates 
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |>
  dplyr::count(duplicate_w_last_time)
# 1 duplicate               108
# 2 new                      48

## 4. how many are duals? ----
#' Explanations:
#' 
#' Create a new dataframe 
#' 
#' Use the 'mutate' function to add a new column 'permit_region' based on conditions.
#' 
#' If 'permitgroup' contains any of the specified patterns ("RCG", "HRCG", "CHG", "HCHG"),
#' 
#' set 'permit_region' to "dual". Otherwise, set 'permit_region' to "sa_only".
#' 
#' If none of the conditions are met, set 'permit_region' to "other".
#' 
#' The resulting dataframe includes the original columns from 'compl_corr_to_investigation_short_dup_marked'
#' along with the newly added 'permit_region' column.
#' 

compl_corr_to_investigation_short_dup_marked__permit_region <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |> 
  # compl_corr_to_investigation_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols |>
  dplyr::mutate(permit_region =
           dplyr::case_when(
             grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "dual",
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "other"
           ))

#'
#' Explanations:
#'
#' Use the 'select' function to extract the columns 'vessel_official_number' and 'permit_region'
#'
#' from the dataframe 'compl_corr_to_investigation_short_dup_marked__permit_region'.
#'
#' Use the 'distinct' function to keep only unique combinations of 'vessel_official_number' and 'permit_region'.
#'
#' Use the 'count' function to count the occurrences of each unique 'permit_region'.
#'
#' The resulting count provides the frequency of each 'permit_region'.
#' 
region_counts <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  dplyr::select(vessel_official_number, permit_region) |>
  dplyr::distinct() |>
  dplyr::count(permit_region)

dplyr::n_distinct(compl_corr_to_investigation_short_dup_marked__permit_region$vessel_official_number)

### dual permitted cnts ----

region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100

## 5. Changed owner ----
res_join_permit <-
  dplyr::left_join(
    compl_corr_to_investigation_short_dup_marked__permit_region,
    permit_vessel_w_changed_owner,
    dplyr::join_by(vessel_official_number == vessel_id)
  )

dim(permit_vessel_w_changed_owner)

dim(res_join_permit)

### keep only permit_status from permits ----
res_join_permit_short <-
  res_join_permit |>
  dplyr::select(tidyselect::all_of(
    names(
      compl_corr_to_investigation_short_dup_marked__permit_region
    )
  ), permit_status) |>
  dplyr::distinct()

dim(res_join_permit_short)
# 138

n_distinct(res_join_permit_short$vessel_official_number)
# 137

#' combine all permit_status
compl_corr_to_investigation_short_dup_marked__permit_region__status <-
  res_join_permit_short |>   
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(permit_status_all =
                     paste(permit_status, 
                           collapse = ", "),
                  .keep = c("unused")) |>
  dplyr::ungroup() |> 
  distinct()

dim(compl_corr_to_investigation_short_dup_marked__permit_region__status)
# 137 41

#' check
compl_corr_to_investigation_short_dup_marked__permit_region__status |>
  dplyr::filter(!is.na(permit_status_all) &
           !permit_status_all == "NA") |>
  dplyr::glimpse()


# Print out results ----
## add additional columns in front ----

#' Create a variable with the long column name
additional_column_name1 <-
  stringr::str_glue(
    "Confirmed Egregious? (permits must still be active till {permit_expired_check_date}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call/other (voicemail counts) and at least 1 email)"
  )

#' Explanation:
#' 
#' This code adds new columns to the dataframe `compl_corr_to_investigation_short_dup_marked__permit_region`. Here's what each part does:
#' 
#' 1. **Add Columns Function:**
#' 
#'    - `tibble::add_column()`: This function from the `tibble` package is used to add new columns to a dataframe.
#' 
#'
#' 2. **Column Specifications:**
#'
#'    - `!!(additional_column_name1) := NA`: Adds a new column with a name in `additional_column_name1` variable filled with NA values.
#'
#'      - `!!`: This is a tidy evaluation feature that allows the use of non-standard evaluation. It evaluates the expression `additional_column_name1` dynamically.
#'
#'      - `:= NA`: Assigns NA values to the new column.
#'
#'    - `Notes = NA`: Adds another new column named "Notes" filled with NA values.
#'
#'    - `.before = 2`: Specifies that the new columns should be inserted before the second column in the dataframe.
#' 
#' This code effectively adds two new columns, with names from `additional_column_name1` and "Notes", filled with NA values, to the dataframe.
#' 

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |>
  # compl_corr_to_investigation_short_dup_marked__permit_region__status |> 
  tibble::add_column(
    !!(additional_column_name1) := NA,
    Notes = NA,
    .before = 2
  )

#' Check what are the column names now
auxfunctions::print_df_names(compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

out_file_name <-
  stringr::str_glue("egregious_violators_to_investigate_{lubridate::today()}.csv")

result_path <- 
  file.path(current_project_output_path,
            out_file_name)

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns |>
  readr::write_csv(result_path)

print_df_names(compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

cat("Results:",
    "compl_corr_to_investigation_short_dup_marked__permit_region__add_columns",
    "and",
    out_file_name,
    sep = "\n")

