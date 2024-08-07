#' title: Egregious Violators

# Setup ----
#' %%%%% Setup
#' 
#' This script identifies and processes "egregious violators" in the SEFHIER program.
#' 
#' SEFHIER stands for Southeast For-Hire Integrated Electronic Reporting.
#' 
#' It combines compliance and correspondence data, applies specific filters,
#' and prepares a report of vessels requiring further investigation.
#'
#' You will need to load various files, and use various R scripts in order for this analytical file to works.
#' 
#' The Setup section includes necessary libraries, functions, and data loading.
#' 
## The "egregious violator" definition ----
#' 
#' 1. No reports for 26 weeks, starting from a week ago today, and going back 26 weeks.
#'
#' 2. Permits have not expired and were active for the same period as (1).
#'
#' 3. The grace period is the 7 days prior to today, and is not included in the 26 weeks.
#'
#' 4. We need to have called at least 1 time and emailed at least 1 time. Or they contacted us at least once.
#'
#' 5. Do not count any correspondence (regardless of the type - email/call, voicemail or not) that includes "No contact made" in the text of the entry as an actual "direct" contact for any egregious vessel (May 6 2024).
#'

## New requirement 2023-08-09 ----
#' 6. We should have made at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.
#' 
#' About comments:
#' 
#' There are several types of comments in the following code.
#' 
#' 1) If the comment starts with "Note." or "Manually:" it is essential for running the code.
#' 
#' 2) '<<<<' and '>>>>' mark the start and the end of definitions and help documents for helper functions. Another way to see an R help document is to type a question mark in the Console followed by the function name, e.g. ?mutate
#'
#' 3) All other comments explain the logic and the syntax.
#' 
#' 
#' Note. Update (download) all input files every time, the same day, before running this code.
#' 
#' This is important because we use the function “today” to establish the 26 week time period, so all files need to be generated on the same “today”. #' 
#' If there is no comment with the word "manually" before the code, it will work automatically.
#' 
## Install packages if needed ----
# 
#' If any package suggests updates it is safe to choose option 1 (update all). Or run the whole code from "Source".
#'
#' We don't load most of the packages to the current session namespace with library(), instead, functions are called from their packages with "::" notation. 
#' 
#' Install packages not yet installed
#' 

# Create list of needed packages
needed_packages <- c(
  "tidyverse",
  "ROracle",
  "DBI",
  "devtools", # Collection of package development tools.
  "zoo", # Handling time series data.
  "diffdf" # Compares dataframes and identifies differences.
)

#' Explanations for the following code:
# - `needed_packages %in% rownames(installed.packages())` checks which packages from `needed_packages` are installed:
#   - `installed.packages()` returns a matrix of information about all installed packages.
#   - `rownames(installed.packages())` extracts the names of the installed packages.
#   - `needed_packages %in% ...` checks if each package in `needed_packages` is in the list of installed packages, returning a logical vector indicating the presence of each package.
# - `if (any(installed_packages == FALSE)) { ... }` checks if any package is not installed:
#   - `any(installed_packages == FALSE)` returns `TRUE` if at least one element in `installed_packages` is `FALSE`.
#   - `install.packages(packages[!installed_packages])` installs the packages that are not installed:
#     - `needed_packages[!installed_packages]` selects the packages from `needed_packages` that are not installed.
#     - `install.packages()` installs the selected packages.  
installed_packages <-
  needed_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

#' Optional: Install helper functions from GitHub for SEFHIER data analysis.
#'
#' Explanations for the following code:
#' 
#' The installation details depend on the username.
#' 
#' For most users, install from the main branch if not already installed.
#' 
#' One doesn't have to have a GitHub account to use it.
#' 
#' For the package developer, install from the development branch.
#'
#' - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#'
#'   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#'
#'   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
#'
#' - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#'
#'   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#'
#'   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
#' 
#' This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
#' 

install_helper_functions <- function() {
  # Check if the username is not "anna.shipunova"
  if (!auxfunctions::get_username() == "anna.shipunova") {
    # If the auxfunctions package is not installed, install it from GitHub
    if (!require('auxfunctions')) {
      devtools::install_github("AShipunova1/R_code/auxfunctions")
    }
  } else {
    # For a developer, rebuild the package from the development branch. To force the installation change to 'force = TRUE'
    devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = FALSE)
    # restart R session to pick up changes
    # .rs.restartR()
    library(auxfunctions)
  }
}

install_helper_functions()

## Load packages ----

# Load packages for database interactions with Oracle databases.
library(ROracle)
library(DBI)

# Load the magrittr package for piping operation %>%. In some cases the base R "|>" works differently. See more at https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/
library(magrittr)

## Define dates ----
#'
#' Define start and end years for the analysis period
#' 
#' Manually: Values for `my_year1` and `my_year2` may be adjusted as needed
#' 
#' Ex. my_year1 is the previous calendar year, and my_year2 is the current calendar year
#' 

# start year for the analysis
my_year1 <- "2023"
my_beginning1 <- stringr::str_glue("{my_year1}-01-01")
my_end1 <- stringr::str_glue("{my_year1}-12-31")

# last year for the analysis
my_year2 <- "2024"
my_beginning2 <- stringr::str_glue("{my_year2}-01-01")
my_end2 <- stringr::str_glue("{my_year2}-12-31")
#'
#' Following are the definitions of dates used throughout the code.
#' 
#' Set the current date as the data file date
data_file_date <- 
  lubridate::today()
  
#' How many weeks and days to take into account?
#' 
# The 26-week period is used to define long-term non-compliance
number_of_weeks_for_non_compliance = 26

# Calculate number of days in non compl weeks
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliance * 7

# test, should be TRUE
days_in_non_compl_weeks == 182
#
# The 7-day grace period allows for recent reports that may not yet be processed
grace_period = 7 # days

# Calculate the date that is 26 weeks plus the grace period before the current date
half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period
#'
#' Check the week number and day of the week for the period's start
#' 
#' This can be used to verify the calculation against a calendar
#' 
lubridate::week(half_year_ago)

lubridate::wday(half_year_ago, label = T)
#'
#' Set the minimum date for permit expiration (30 days from today).
#' 
#' For now, for those permits that expire within the next 30 days, we are not evaluating them. They will be caught at permit renewal.
#' 

permit_expired_check_date <- data_file_date + 30
#'
#' Define the start of the last week, excluding it from analysis
last_week_start <- data_file_date - grace_period
#'

## Set up paths ----
#'
#' Different methods are used based on the user to accommodate different directory structure.
#' 
#' This allows the script to run correctly on multiple systems without manual path changes.
#' 
#' In the code in this section all user provided values have the word "manually" in the description. Everything else is created automatically.
#'
#' Manually: Change the following 2 lists (**my_paths** and **current_in_out_paths**) to your environment if needed. The variable _names_ are used throughout the code, so please change only the quoted _values_ inside the lists.
#' 

# Check if the current username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  # 1) General directories (to look up additional files, e.g. processed data). It can be left as is if you don't have it. You can provide path to individual files later.
  my_paths <- list(inputs  = "~/my_inputs",
                   outputs = "~/my_outputs",
                   git_r   = "~/R_code")
  
  # 2) Current project code, input and output directories
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
#'
#' The following section uses provided directory names lists to automatically create separate variables for future use and create current input/output directories if they do not exists.
#' 
#' 
#' Create variables to store shortcuts to project directories
#' 
# This is usually the current directory name.
current_project_name <- current_in_out_paths$project_name

current_project_path <- current_in_out_paths$code
            
current_project_input_path <- current_in_out_paths$input

current_project_output_path <- current_in_out_paths$output

#' Create input and output directories if they don't exist
auxfunctions::create_dir_if_not(current_project_input_path)

auxfunctions::create_dir_if_not(current_project_output_path)

### Additional individual paths to data files ----
#' This section sets up paths for specific data files used in the project
#' 

#### Compliance and Correspondence ----
#' 
#' Download files from FHIER first, see Get Data section of this code.
#' 
#' Manually: Provide full paths here, changing _values_ inside the quotes:
#' 
correspondence_csv_path <- "Your full path to correspondence.csv"
fhier_compliance_csv_path_list <- 
  list("Your full path to fhier_compliance.csv year 1",
       "Your full path to fhier_compliance.csv year 2")
#'
#' Depending on a user name who runs the code, the file paths are constructed here.
#' 
# Check if the username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
    # Combine correspondence CSV path and compliance CSV paths into one list
  all_csv_full_paths_list <-
    c(correspondence_csv_path, fhier_compliance_csv_path_list)
} else {
  # For Anna Shipunova
  
  # Manually: Change file names to the last download
  # Using raw string literals (r"()") to handle backslashes in file paths

  all_csv_names_list = c(
    "Correspondence_2024_07_24.csv",
    r"(2024_07_24\FHIER_Compliance_2024__07_24_2024.csv)"
  )
  
  # Add a full path in front of each file name for correspondence CSV
  # prepare_csv_full_path function constructs full file paths by combining base path, additional path, and file names

  corresp_full_path <-
    auxfunctions::prepare_csv_full_path(all_csv_names_list[[1]],
                          add_path = "from_Fhier/Correspondence",
                          input_dir_part = my_paths$inputs)
  
  # Add a full path in front of each file name for compliance CSVs
  compliance_full_paths <-
    auxfunctions::prepare_csv_full_path(all_csv_names_list[2:length(all_csv_names_list)],
                                        add_path = "from_Fhier/FHIER Compliance",
                                        input_dir_part = my_paths$inputs)
  
  # Combine correspondence full path and compliance full paths into one list
  all_csv_full_paths_list <-
    c(corresp_full_path,
      compliance_full_paths)

  # all_csv_full_paths_list contains full paths to all required CSV files for correspondence and compliance

  # Check if all the specified files exist
  purrr::map(all_csv_full_paths_list, file.exists)
}

#### Processed Metrics Tracking (permits from FHIER) ----
#' 
#' processed_metrics_tracking_file_names contains paths to RDS files with SEFHIER permitted vessels data for different years. Created separately, see Get data.
#' 
#' Manually: Add your full path to processed Metrics tracking for each year instead of "Your full path here".
#' 
#' Define paths for processed Metrics tracking CSVs.
#' 
#' Depending on a user name who runs the code, the file paths are constructed here.
#' 
#' 
# Check if the username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  processed_metrics_tracking_file_names <-
    c(
      stringr::str_glue(
        "Your full path here/SEFHIER_permitted_vessels_nonSRHS_{my_year1}.rds"
      ),
      stringr::str_glue(
        "Your full path here/SEFHIER_permitted_vessels_nonSRHS_{my_year2}.rds"
      )
    )
} else {
  # for Anna Shipunova
  processed_input_data_path <-
    file.path(my_paths$inputs, "processing_logbook_data", "Outputs")

  # check
  dir.exists(processed_input_data_path)
  # if not TRUE: Check your provided path and/or create manually.
  
  # Get file names for all years
  processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "Permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

  # Exclude links (shortcuts) to ensure we're only working with actual data files
  processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

}
#'
#' Check if all processed metrics tracking files exist
purrr::map(processed_metrics_tracking_file_names, file.exists)
#'
#' if not TRUE: Check your provided path and/or create manually.
#' 

#### Physical Address List from FHIER path ----
#' Download first from REPORTS / For-hire Primary Physical Address List.
#' 
#' Set the path for FHIER addresses based on the user.
#' 
#' Manually: Add your full path instead of "Your full path here".
#' 
# Check if the username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  fhier_addresses_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  fhier_addresses_path <-
    file.path(
      my_paths$inputs,
      r"(from_Fhier\address\For-hire Primary Physical Address List_07_24_2024.csv)"
    )
}
#'
#' fhier_addresses_path points to a CSV file containing the primary physical addresses of for-hire vessels
#' 
#' Verify that the FHIER addresses file exists at the specified path, correct the path if it is doesn't exist
#' 
file.exists(fhier_addresses_path)

#### Home port processed city and state path ----
#' Download first from Google Drive or recreate it if the date is more than 4 months old.
#' 
#' Set the path for processed PIMS home ports based on the user.
#' 
#' Manually: Add your full path instead of "Your full path here".
#' 
# Check if the username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  processed_pims_home_ports_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  processed_pims_home_ports_path <-
    file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports_2024-06-18.csv")
}

#' processed_pims_home_ports_path points to a CSV file containing processed data about vessel home ports, including city and state information
#' 
#' Verify that the processed PIMS home ports file exists at the specified path, correct the path if it is doesn't exist.
file.exists(processed_pims_home_ports_path)

#### Data from the previous results of "egregious violators for investigation" path ----
#'
#' Depending on a user name who runs the code, define the path to the previous results file.
#' 
#' Manually: Add your full path instead of "Your full path here".
#' 
# Check if the username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  prev_result_path <- "Your full path here"
} else {
  # for Anna Shipunova, update file name's date
  prev_result_path <-
    file.path(current_project_input_path,
              "egregious_violators_to_investigate_2024-05-17.xlsx")
}

#' Check if the previous results file exists.
#' If it is doesn't exist download it first and correct the path, or load directly from Google drive, see Get data.
file.exists(prev_result_path)

### Set up Google Drive paths ----
#' Hard coded Google drive folder names, manually change here if changing in Google drive.
egr_violators_googledrive_folder_name <- "Egregious violators"
output_egr_violators_googledrive_folder_name <- "output"
#'
#' Get the path to the main Egregious violators folder on Google Drive
#' 
#' It is used to read the previous result and for saving the new result.
#' 
#' When asked for the authentication the first time choose the appropriate option and follow the instructions. 
#' 
#' If there is an option with your google email account (like your.name@noaa.gov) you can choose that option (usually #2) and it will confirm your access automatically.
#'
#' `n_max = 1` means we will use the first result, assuming we have only one folder with such name on Google dive.
#'

egr_violators_googledrive_folder_path <-
  googledrive::drive_find(pattern =
                            egr_violators_googledrive_folder_name,
                          type = "folder",
                          n_max = 1)
#'
#' Get the path to the output folder within the Egregious violators folder on Google Drive
#'
#' Explanations for the following code:
#'
#' - `output_egr_violators_googledrive_folder_path <- ...` assigns the result of the `googledrive::drive_ls` function call to the variable `output_egr_violators_googledrive_folder_path`.
#'
#' - `googledrive::drive_ls(...)` lists the contents of a Google Drive folder based on the specified parameters:
#'
#'   - `path = googledrive::as_id(egr_violators_googledrive_folder_path)` specifies the path to the folder using its ID:
#'
#'     - `googledrive::as_id(egr_violators_googledrive_folder_path)` converts `egr_violators_googledrive_folder_path` into a format recognized as an ID by the `googledrive` package.
#'
#'   - `pattern = output_egr_violators_googledrive_folder_name` specifies a pattern to match folder names against, using `output_egr_violators_googledrive_folder_name`.
#'
#'   - `type = "folder"` specifies that only folders should be listed.
#'
#'   - `n_max = 1` specifies that only the first matching folder should be returned.
#'   

output_egr_violators_googledrive_folder_path <-
  googledrive::drive_ls(
    path = googledrive::as_id(egr_violators_googledrive_folder_path),
    pattern = output_egr_violators_googledrive_folder_name,
    type = "folder",
    n_max = 1
  )

## Other setup ----
# Set options to prevent converting long numbers to scientific notation for input/output in spreadsheets and csv files
# This ensures vessel numbers and other large integers are displayed in full

options(scipen = 999)

# Synchronize timezone settings between R and Oracle to ensure consistent date-time handling
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

#' %%%%% Prepare data
#' 

## Get data ----
#' 
# This is used only with source()
get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)
#'
#' Data are in:
#' 
#' compl_clean
#' 
#' corresp_contact_cnts_clean0
#'
#' processed_metrics_tracking_permits
#'
#' fhier_addresses
#'
#' processed_pims_home_ports
#'
#' db_participants_address
#'
#' prev_result
#'

#' %%%%% Find egregious violators
#' 

# Preparing compliance info ----

## Permit Expiration ----
### Add permit_expired column ----
#' Explanations for the following code:
#' 
#' 1. Add a new column 'permit_expired' using 'mutate'.
#' 
#' 2. Use 'case_when' to determine if 'permit_groupexpiration' is greater than permit_expired_check_date defined earlier.
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

# check
# dplyr::glimpse(compl_clean_w_permit_exp)

### Get only not expired last 27 weeks of data minus grace period (total 26 weeks) ----

#' 
#' 27 weeks are used to account for a one-week grace period, resulting in 26 weeks of usable data
#' 
compl_clean_w_permit_exp__not_exp <-
  compl_clean_w_permit_exp |>
  # the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_end < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")
#'
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

### Add year_month column from week_start ----

# as.yearmon converts dates to a year-month format (e.g., "Jan 2024")
compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp__not_exp |>
  dplyr::mutate(year_month = zoo::as.yearmon(week_start)) |>
  # keep entries for the current check period only
  dplyr::filter(year_month >= zoo::as.yearmon(half_year_ago))
#'
#' Compare dimensions to verify the filtering has reduced the dataset as expected
dim(compl_clean_w_permit_exp)
# [1] 221081     21

dim(compl_clean_w_permit_exp_last_half_year)
# [1] 57296    22

### Have only SA and dual permits ----
#' This section filters the data to include only SA and dual permits.
#'
#' Filter rows where 'permitgroup' contains "CDW", "CHS", or "SC"
#' 
compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  dplyr::filter(grepl("CDW|CHS|SC", permitgroup))

# Check the dimensions of the resulting dataframe
dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 38761    22

### Keep fewer columns in the compliance df ----
#' Define a vector of column names to be removed from the compliance dataframe
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
#'
#' Remove specified columns and keep only unique rows
#' 
compl_clean_w_permit_exp_last_half_year__sa__short <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::select(-tidyselect::any_of(remove_columns_from_compliance)) |> 
  dplyr::distinct()

#' Check the dimensions of the resulting dataframe
dim(compl_clean_w_permit_exp_last_half_year__sa__short)
# [1] 38761    11
#'
#' Work with the whole period now
#' 

### Add compliant_after_overr ----
#' use tictoc package for benchmarking 
#' 
#' Apply the add_compliant_after_override function to add a new column indicating compliance status after overrides
#' 
tictoc::tic("compl_overr")
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr <-
  compl_clean_w_permit_exp_last_half_year__sa__short |>
  auxfunctions::add_compliant_after_override(overridden_col_name = "overridden_",
                                             compliance_col_name = "compliant_")
tictoc::toc()
# compl_overr: 8.76 sec elapsed
#'
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
#'
#' Verify that the compliant_after_override column contains only "yes" and "no" values
setdiff(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$compliant_after_override |>
  unique(), c("yes", "no")) == 0

dim(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr)
# E.g.
# [1] 38761    12
#'
#' Ensure that the number of unique vessels remains the same after data transformations
#' 
dplyr::n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number) ==
  dplyr::n_distinct(
    compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$vessel_official_number
  )
# T

### Get only non-compliant entries for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |>
  # not compliant
  dplyr::filter(tolower(compliant_after_override) == "no")

# check
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)
# E.g.
# [1] 10768    12

### Keep only vessels with info for all weeks in the period ----
#'
#' That should eliminate entries for vessels having permits only a part of the period
#' 
#' Calculate the total number of distinct weeks in the dataset
#' 
all_weeks_num <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  dplyr::select(week) |>
  dplyr::distinct() |>
  nrow()
#'
#' Explanations for the following code:
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
#'
#' Check how many entries were removed
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)
# E.g.
# [1] 10768    12

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present)
# E.g.
# [1] 3278   11

### Check the last report date ----
#### Get ids only ----

#' Create a dataframe with unique vessel official numbers
compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()
#'
#' check vessel's number
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids)
# E.g.
# 149 

#### Check these ids in the full compliance information ----

#' Check if there are any new submitted reports for previously non-compliant vessels
#' 
compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids$vessel_official_number
  ) |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(
    tolower(compliant_) == "yes" &
      tolower(overridden_) == "yes" &
      # not the current month
      year_month < zoo::as.yearmon(data_file_date)
  ) |>
  nrow()
#' A result of 0 indicates no new compliant reports have been submitted for selected vessels. If it is not a zero, comment "nrow()" at the last line and the pipe sign before and investigate.
#'
#' End of Compliance preparations 
#' 
#' Results: processed Compliance is in `compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present`
#' 

# Preparing Correspondence ----

## Remove 999999 ----
#' 
#' Remove vessels with official numbers starting with "99999" as they are placeholder or test entries
#' 
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  dplyr::filter(!grepl("^99999", vessel_official_number))
#'
#' check number of vessels in correspondence 
dplyr::n_distinct(corresp_contact_cnts_clean$vesselofficial_number)
#' E.g.
#' 4281
#'
#' check call type, voicemail and contact type combinations
corresp_contact_cnts_clean |>
  dplyr::select(calltype, voicemail, contacttype) |>
  dplyr::distinct() |> head(10)

## Correspondence Filters ----
#' 
#' This section defines various filters for correspondence data using quosures.
#' 
#' Quosures are a part of tidy evaluation in R, allowing expressions to be captured without evaluation, which is useful for creating functions with flexible inputs.
#'
#' Define a filter to check if any entry was an outgoing call
#'
we_called_filter <-
  dplyr::quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))
#'
#' Define a filter to check if any entry was an outgoing email or other contact type
#'
we_emailed_once_filter <-
  dplyr::quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outgoing"
  ))
#'
#' Explanations for the following code:
#' 
#' **Expression inside quo()**:
#'
#'    - `!grepl("No contact made", contactcomments, ignore.case = TRUE)`: This expression is a negation of the `grepl` function, which is used to search for a pattern ("No contact made") in the `contactcomments` column.
#'
#'    - `grepl()` returns `TRUE` for each element of `contactcomments` that contains the pattern, and `FALSE` otherwise.
#'
#'    - The `!` operator negates the result, so the filter condition will be `TRUE` for rows where "No contact made" is not found in the `contactcomments` column.
#' 
#' The `exclude_no_contact_made_filter` function creates a filter condition to exclude rows where "No contact made" is found in the `contactcomments` column.
#' 
exclude_no_contact_made_filter <-
  dplyr::quo(!grepl("No contact made", 
            contactcomments, 
            ignore.case = TRUE))
#'
#' Don't need a second contact if any contact was incoming
#' 
#' Define a filter to check if any contact was incoming
#'
they_contacted_direct_filter <-
  dplyr::quo(
    any(
      tolower(calltype) == "incoming"
      )
  )

### Use the filters ----
#' Apply filters to create a subset of correspondence data meeting specific criteria
#' 

#' Explanations for the following code:
#'
#' - `corresp_contact_cnts_clean |> ...` starts the pipeline with the data frame `corresp_contact_cnts_clean`.
#'
#' - `dplyr::filter(tolower(calltype) == "incoming" | ...)` filters rows based on the conditions specified:
#'
#'   - `dplyr::filter()` is used to subset rows in the data frame.
#'
#'   - `tolower(calltype) == "incoming"` converts the `calltype` column to lowercase and checks if it equals "incoming".
#'
#'   - The `|` operator means logical OR, so it includes rows where the condition on either side is true.
#'
#'   - `(contact_freq > 1 & (!!we_called_filter & !!we_emailed_once_filter))`:
#'
#'     - `contact_freq > 1` checks if the `contact_freq` column is greater than 1.
#'
#'     - `&` is the logical AND operator, so it requires both conditions to be true.
#'
#'     - `!!we_called_filter` evaluates the `we_called_filter` variable as a logical expression.
#'
#'     - `!!we_emailed_once_filter` evaluates the `we_emailed_once_filter` variable as a logical expression.
#'
#'     - The parentheses around this subexpression ensure it is evaluated as a single logical unit.
#'
#' - `dplyr::filter(!!exclude_no_contact_made_filter)` applies another filter condition:
#'
#'   - `!!exclude_no_contact_made_filter` evaluates the `exclude_no_contact_made_filter` variable as a logical expression.
#' 
#' This code filters the `corresp_contact_cnts_clean` data frame to include rows where either the `calltype` is "incoming" or the `contact_freq` is greater than 1 and both `we_called_filter` and `we_emailed_once_filter` are true. Additionally, it filters rows based on the `exclude_no_contact_made_filter` condition.
#'

corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  dplyr::filter(tolower(calltype) == "incoming" |
           (
             contact_freq > 1 &
               (!!we_called_filter &
                  !!we_emailed_once_filter)
           )) |> 
  dplyr::filter(!!exclude_no_contact_made_filter)
#'
#' Check the dimensions of the original and filtered datasets
#' 
dim(corresp_contact_cnts_clean)
# E.g. [1] 33001    22
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# E.g. [1] 31061    22
#'
#' Check how many vessels left after filtering 
dplyr::n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)
# E.g.
# [1] 3865

## Fix dates ----
#' Prepare to clean and standardize date formats
#' 
#' check how the dates look
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1) |> str()
# chr "02/15/2024 03:15PM"
#'
#'
#' Explanations for the following code:
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
#'
#' check how the dates look now
head(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm, 1) |> 
  str()
# E.g.
# POSIXct[1:1], format: "2024-06-17 15:24:00"
#'
#' End of Correspondence preparations 
#' 
#' Processed Correspondence is in 
#' `corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates`
#' 

# Join correspondence with compliance ----
#'
#' Combine correspondence and compliance data for further analysis.
#' 
#' An inner join is used to combine correspondence and compliance data.
#'
#' This ensures we only keep vessels that appear in both datasets, effectively filtering for vessels with both compliance and correspondence records
#' 

#' Explanations for the following code:
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
#' 

compl_corr_to_investigation <-
  dplyr::inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )
#'
#' Verify the dimensions and content of the joined dataset
#' 
dim(compl_corr_to_investigation)
# E.g.
# [1] 30844    32

dplyr::n_distinct(compl_corr_to_investigation$vesselofficial_number)
# E.g.
# 141

head(compl_corr_to_investigation) |> 
  dplyr::glimpse()

## Store the count of unique vessels ----
#' For later verification and reporting
num_of_vsl_to_investigate <- 
  dplyr::n_distinct(compl_corr_to_investigation$vesselofficial_number)
#'
#' Results: Egregious violators Compliance & Correspondence joined together are in
#' `compl_corr_to_investigation`
#' 

#' %%%%% Prepare output
#' 
# Output needed investigation ----
#' 
#' "Investigation" in this context refers to vessels that meet the criteria for egregious violations and require further action.
#' 
#' Steps:
#' 
#' 1. Remove unused columns.
#' 
#' 2. Create additional columns.
#' 
#' 3. Mark vessels already in the know list (prev_result).
#' 
#' 4. Duals vs. sa_only
#' 

## 1. Remove extra columns ----
#' List of columns to be excluded from the final output for simplification and focus on relevant data. Commented are column names to retain.
#' 
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
#'
#' Create a simplified version of the investigation data, removing unused fields and concatenating unique values for each vessel
#' 
#' Explanations for the following code:
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
#'
#' Visual check if data make sense
compl_corr_to_investigation_short |> 
  head() |> 
  dplyr::glimpse()
#'
#' Check if number of vessels didn't change
nrow(compl_corr_to_investigation_short) == num_of_vsl_to_investigate

## 2. Create additional columns ----
### Add list of contact dates and contact type in parentheses  ----
#'
#' Define column name variables for flexibility across different data sources.
#' 
#' Spaces and underscores placements vary from source to source.
#' 
contactdate_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, "contact", "date")[1]

contacttype_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, "contact", "type")[1]

contactphonenumber_field_name <-
  auxfunctions::find_col_name(compl_corr_to_investigation_short, ".*contact", "number.*")[1]
#'
#'  Function to create a summary of contact dates and types for each vessel
#'  
#' Explanations for the following code:
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
      # Combine contact date and type into a single column

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
#'
#' Apply the get_date_contacttype function
date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation_short)
#'
#' Verify that the number of vessels remains consistent
nrow(date__contacttype_per_id) == num_of_vsl_to_investigate
#'
#' Display a sample of the resulting data for verification
date__contacttype_per_id |>
  head() |>
  dplyr::glimpse()

#### Add the new column back ----
# Join the short compliance/correspondence data with date and contact type information
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
#' 
#' $ date__contacttypes     <chr> "03/13/2024 11:59AM, 09/21/2023 03:41PM, 08/18/2023 10:52AM,…
compl_corr_to_investigation__corr_date |> 
  head() |> 
  dplyr::glimpse()

### Add pims home port info ----
#'
#' Rename columns in the processed PIMS home ports data for consistency
#' 
processed_pims_home_ports_renamed <- 
  processed_pims_home_ports |> 
  dplyr::rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)
#'
#' Combine compliance/correspondence data with hailing port information
#' 
compl_corr_to_investigation__corr_date__hailing_port <- 
  dplyr::left_join(
    compl_corr_to_investigation__corr_date,
    processed_pims_home_ports_renamed,
    dplyr::join_by(vessel_official_number)
  )

### Add prepared addresses ----

# Define the path to the address preparation script
# This is used only with source()
prep_addresses_path <-
  file.path(current_project_path,
            stringr::str_glue("{current_project_name}_prep_addresses.R"))
#'
#' Check if the file exists.
file.exists(prep_addresses_path)

source(prep_addresses_path)
#'
#' Results are in compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr
#' 
## 3. Mark vessels already in the know list ----
#' Identify and mark vessels that have been previously marked as egregious, to track repeat offenders
#' 
#' From the email:
#' 
#' The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.
#' 
#'
#' get vessel ids from the previous result 
vessels_to_mark_ids <-
  prev_result |>
  dplyr::select(vessel_official_number)
#'
#' Check the amount 
dim(vessels_to_mark_ids)

#### Mark these vessels ----
#'
#' Create a new column to distinguish between previously processed vessels and new entries
#' 
#' Explanations for the following code:
#'
#' Create a new column 'duplicate_w_last_time' in the dataframe 'compl_corr_to_investigation_short'.
#'
#' This column is marked with "duplicate" for rows where 'vessel_official_number' is present in the list of vessel IDs ('vessels_to_mark_ids').
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

### Check ----
#' Perform validation checks on the processed data

#' Check that number of vessels didn't change.
dplyr::n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked$vessel_official_number) ==
  num_of_vsl_to_investigate
#'
#' Check that there is one row per vessel.
nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked) == 
  num_of_vsl_to_investigate 
#'
#' Count how many duplicates 
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |>
  dplyr::count(duplicate_w_last_time)
# 1 duplicate               108
# 2 new                      48

## 4. How many are duals? ----
#' Identify and count vessels with dual permits (both SA and GOM)
#' 
#' 
#' Explanations for the following code:
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
  dplyr::mutate(permit_region =
           dplyr::case_when(
             grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "dual",
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "other"
           ))
#'
#'
#' Explanations for the following code:
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

### Dual permitted cnts ----
#' Calculate the percentage of dual-permitted vessels

region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100

# Print out results ----
## Add additional columns in front ----
#'
#' Create a variable with a long column name for confirmation status
#' 

additional_column_name1 <-
  stringr::str_glue(
    "Confirmed Egregious? (permits must still be active till {permit_expired_check_date}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call/other (voicemail counts) and at least 1 email)"
  )
#'
#' Explanations for the following code:
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
  compl_corr_to_investigation_short_dup_marked__permit_region |>

  tibble::add_column(
    !!(additional_column_name1) := NA,
    Notes = NA,
    .before = 2
  )
#'
#' Don't remove the "year" column, in case there are 2 years in the current period.
#' 
#'
#' Check and display the updated column names
#' 
auxfunctions::print_df_names(compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

#' Generate output file name with current date
#' 
#' The file name is split into a basename and a full name with extension.
#' 
#' The basename will be used for the spreadsheet saved on Google Drive.
#'  
out_file_basename <- 
  stringr::str_glue("egregious_violators_to_investigate_{lubridate::today()}")

out_file_name <-
  stringr::str_glue("{out_file_basename}.csv")

result_path <- 
  file.path(current_project_output_path,
            out_file_name)

#' Write the results to a CSV file
compl_corr_to_investigation_short_dup_marked__permit_region__add_columns |>
  readr::write_csv(result_path)

## Write to google sheets ----

#' 
#' Explanations for the following code:
#' 
#' Define a function to write results to Google Sheets
#' 
#' This function performs the following steps:
#'
#' 1. Renames the existing 'Egregious Violators Current' spreadsheet to a spreadsheet with the name with a date from its tab (e.g. "egregious_violators_to_investigate_2024-06-18"
#'
#' 2. Creates a new 'current' spreadsheet
#'
#' 3. Writes the new results to the new 'Egregious Violators Current' spreadsheet in the same Google drive directory ("Egregious violators/output")
#'
#' 4. Removes the default empty sheet
#'
#' 5. Opens the new spreadsheet in the browser for verification
#'
#' 6. Returns a shareable link to the new spreadsheet
#' 
#' It has to be a function, this way we can call it if needed, not every time we run the code.
#'

write_res_to_google_sheets <- 
  function() {
    # Define the current result Google Sheets name
    current_result_google_ss_name <- "Egregious Violators Current"
    
    # my_current_ss contains information about the existing 'Egregious Violators Current' file
    my_current_ss <-
      googledrive::drive_ls(
        path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
        pattern = current_result_google_ss_name,
        type = "spreadsheet",
        n_max = 1
      )
    
    # An example of my_current_ss:
    #   name                        id                                           drive_resource
    # 1 Egregious Violators Current ...--o6BpLWpb4-... <named list [36]>
    
    # Next:
    # 1) load it to R;
    # 2) create a new spreadsheet with the date of the loaded worksheet and dump the content into it;
    # 3) create a new worksheet in the current spreadsheet with today's date;
    # 4) write the code output into it;
    # 5) check in browser.
    
    # 1) load it to R
    previous_current_content <- googlesheets4::read_sheet(my_current_ss)
    
    # 2) create a new spread sheet with the date of loaded worksheet and dump the content into it
    # a) get the previous spreadsheet name
    
    # ss_info contains detailed information about the current spreadsheet, including sheet names
    ss_info <- googlesheets4::gs4_get(my_current_ss)
    
    # grep for the pattern in case there are additional tabs
    previous_current_spread_sheet_name <-
      grep(
        "egregious_violators_to_investigate_20\\d\\d-\\d\\d-\\d\\d",
        ss_info$sheets$name,
        value = T
      )
    # E.g. "egregious_violators_to_investigate_2024-06-18"
    
    # Rename the file from "current" to the previous_current_spread_sheet_name with the previous date.
    # In case of an error print the message and keep going.
    # If there is a file with this name this code will create another one with the same name.

    tryCatch({
      message("Try to rename the file")
      
      googledrive::drive_mv(
        my_current_ss,
        path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
        name = previous_current_spread_sheet_name
      )
      # E.g.
      # Original file:
      # • Egregious Violators Current
      # Has been renamed:
      # • output/egregious_violators_to_investigate_2024-06-18
      
    }, error = function(cond) {
      message(
        paste(
          "Failed to rename this file: ",
          previous_current_spread_sheet_name
        )
      )
      
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
    }, warning = function(cond) {
      
    }, finally = {
      # message("Some other message at the end")
    })
    
    # Create a new empty spreadsheet in the Google Drive output folder to replace the renamed one
    # And save its properties into current_result_google_ss_name_info
    # In case of an error print the message and keep going.
    # If there is a file with this name this code will create another one with the same name.
    
    tryCatch({
      message("Try to create a new file")
      
      current_result_google_ss_name_info <-
        googledrive::drive_create(
          name = current_result_google_ss_name,
          path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
          type = "spreadsheet"
        )
      
    }, error = function(cond) {
      message(
        paste(
          "Failed to create a new file: ",
          current_result_google_ss_name
        )
      )
      
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
    }, warning = function(cond) {
      
    }, finally = {
      # message("Some other message at the end")
    })
    
    # Write our results into the newly created spreadsheet "Egregious Violators Current"
    # into a sheet/tab with a name defined in out_file_basename
    googlesheets4::write_sheet(
      compl_corr_to_investigation_short_dup_marked__permit_region__add_columns,
      ss = current_result_google_ss_name_info,
      sheet = out_file_basename
    )
    
    # See sheets/tabs to check
    googlesheets4::sheet_properties(ss = current_result_google_ss_name_info)
    
    # Remove the empty Sheet1 created automatically by googledrive::drive_create()
    googlesheets4::sheet_delete(ss = current_result_google_ss_name_info, "Sheet1")
    
    # Check the existing tabs again
    googlesheets4::sheet_properties(ss = current_result_google_ss_name_info)$name
    # Should be only one name now, like
    # [1] "egregious_violators_to_investigate_2024-07-15"
    
    # See in browser to check
    googledrive::drive_browse(current_result_google_ss_name_info)
    
    # Generate a shareable link for the new spreadsheet
    current_output_file_link <- googledrive::drive_link(current_result_google_ss_name_info)
    
    auxfunctions::pretty_print(current_output_file_link, "Link to the new spreadsheet:")
    
    # The function returns the current output file link
    return(current_output_file_link)
    
  }
#'
#' Manually: Un-comment to write results directly to Google drive
# current_output_file_link <- write_res_to_google_sheets()
#'
#' Print result names to console 
cat("Results:",
    "compl_corr_to_investigation_short_dup_marked__permit_region__add_columns",
    out_file_name,
    sep = "\n")
