# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired and were active for the same period as (1);
# 3) the grace period is 7 days back from today.
# 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# NB. Update (download) all input files every time before run.

# set up ----

# Load the 'ROracle' library, which provides an interface for working with Oracle databases in R.
# library(ROracle)

# Load the 'tidyverse' library, which is a collection of R packages for data manipulation and visualization.
library(tidyverse)

# Load the 'magrittr' library, which provides piping data and functions.
library(magrittr)

# Load the 'tictoc' library, which allows measuring code execution time.
# library(tictoc)

# Load the 'openxlsx' library, which allows 
# library(openxlsx)

# library(zoo)
# library(diffdf)
 
# Set up paths ----

# function that will return current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  # Set the working directory to the user's home directory (~)
  setwd("~/")
  base_dir <- getwd()

  # Initialize 'add_dir' as an empty string (for others)
  add_dir <- ""

  # Check if the username is "anna.shipunova" (Anna's computer)
  if (get_username() == "anna.shipunova") {
    # Set 'add_dir' to a specific directory path for Anna
    add_dir <- "R_files_local/test_dir"
  }

  # Construct the path to the main R directory
  main_r_dir <- file.path(add_dir, "SEFHIER/R code")

  # Define directory names for 'Inputs' and 'Outputs'
  in_dir <- "Inputs"
  out_dir <- "Outputs"

  # Construct full paths to 'Inputs' and 'Outputs' directories using 'file.path'
  # file.path is a function used to create platform-independent file paths by joining its arguments using the appropriate path separator (e.g., "\" on Windows, "/" on Unix-like systems).
  #
  # base_dir is the base directory obtained from the user's home directory.
  #
  # main_r_dir is the path to the main R directory, which may vary depending on whether the user is Anna or not.
  #
  # in_dir is the name of the 'Inputs' directory.
  #
  # So, this line effectively combines these components to create the full path to the 'Inputs' directory, ensuring that the path is correctly formatted for the user's operating system.

  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # Change the working directory to the main R directory
  setwd(file.path(base_dir, main_r_dir))

  # Create a list of directory paths for 'inputs' and 'outputs'
  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir)
  return(my_paths)
}

# Define a function named 'set_work_dir_local'
# This function sets the working directory to the user's home directory, defines paths to 'my_inputs,' 'my_outputs,' and 'R_code_github' directories, and returns these directory paths as a list. The use of file.path ensures that the path construction is platform-independent.

set_work_dir_local <- function() {

  # Set the working directory to the user's home directory (~)
  setwd("~/")
  base_dir <- getwd()

  # Define 'main_r_dir' as "R_files_local"
  main_r_dir <- "R_files_local"

  # Define 'in_dir' as "my_inputs"
  in_dir <- "my_inputs"

  # Construct the full path to 'my_inputs' directory
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)

  # Define 'out_dir' as "my_outputs"
  out_dir <- "my_outputs"

  # Construct the full path to 'my_outputs' directory
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # Define 'git_r_dir' as "R_code_github"
  git_r_dir <- "R_code_github"

  # Construct the full path to 'R_code_github' directory
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  # Change the working directory to 'R_files_local'
  setwd(file.path(base_dir, main_r_dir))

  # Create a list of directory paths for 'inputs,' 'outputs,' and 'git_r'
  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)

  # Return the list of directory paths
  return(my_paths)
}

# ===
# Change the behavior of the set_work_dir function based on the username. If the username matches "anna.shipunova," it reassigns set_work_dir to the set_work_dir_local function, effectively using a different directory structure for Anna compared to other users.

# Check if the current username is "anna.shipunova"
if (get_username() == "anna.shipunova") {
  # If the condition is true, assign the 'set_work_dir_local' function to 'set_work_dir'
  set_work_dir <- set_work_dir_local
}

annas_path <- set_work_dir()
my_paths <- annas_path

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

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

data_file_date <- 
  today()

number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7

grace_period = 7 #days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period

# 30 days from today
permit_expired_check_date <- data_file_date + 30

last_week_start <- data_file_date - grace_period

# get_data ----
# get_data_path <- 
#   file.path(current_project_path, "egregious_violators_get_data.R")
# source(get_data_path)

# get data for egregious violators
# use from egregious_violators_start.R

# 1) compliance data
# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT 
# For the last 6 month
# FHIER_Compliance_...csv

# 2) correspondence data
# Download files from FHIER / Home / Correspondence
# Actions / Download 
# For the whole period, starting 01/01/2021
# "~\my_inputs\from_Fhier\Correspondence\Correspondence_2024_02_15.csv"

# 3) processed Metrics tracking
# For the last 6 month
# SEFHIER_permitted_vessels_nonSRHS_YEAR.csv

# 4) Physical Address List from FHIER
# Downloaded from REPORTS / For-hire Primary Physical Address List
# For the whole period, starting 01/01/2021
# "For-hire Primary Physical Address List.csv"

# 5) home port processed city and state from PIMS
# "~\R_files_local\my_outputs\home_ports\vessels_from_pims_ports.csv"

# 6) address information from Oracle db
# "db_participants_address.rds"

# 7) Previous results (from google drive)
# ~\R_files_local\my_inputs\egregious_violators\egregious violators for investigation_DATES...xlsx"

# FHIER ----

# Compliance
# Correspondence
# permit info from processed metrics tracking

# Download from FHIER first
all_csv_names_list = c("Correspondence_2024_04_09.csv",
                         r"(2024_04_09\FHIER_Compliance_2023__04_09_2024.csv)",
                         r"(2024_04_09\FHIER_Compliance_2024__04_09_2024.csv)")

## ---- get compliance and correspondence csv data into variables ----
from_fhier_data_path <-
  file.path(my_paths$inputs)

# Define a function to retrieve compliance and correspondence data from CSV files.
# This function takes 'my_paths' (file paths configuration), 'filenames'
# (list of CSV file names), and 'vessel_id_field_name' (optional field name)
# as input.
get_compl_and_corresp_data <-
  function(my_paths,
           filenames = csv_names_list_22_23,
           vessel_id_field_name = NA) {
    # Add folder names and categorize CSV files into correspondence and compliance.
    csv_names_list <- prepare_csv_names(filenames)

    # Read the contents of all CSV files.
    csv_contents <- load_csv_names(my_paths, csv_names_list)

    # Clean and standardize headers, and trim 'vesselofficialnumber' field if needed.
    csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name)

    # Specific correspondence manipulations ----
    # Perform cleaning and processing specific to correspondence data.
    corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

    ## Specific compliance manipulations ----
    # Extract compliance data from the cleaned CSVs.
    compl_arr <- csvs_clean1[2:length(csvs_clean1)]

    # Clean and process compliance data.
    compl_clean <- compliance_cleaning(compl_arr)

    # Return a list containing cleaned compliance and correspondence data.
    return(list(compl_clean, corresp_arr_contact_cnts_clean))
  }

# specific correspondence manipulations ----
# Define a function to clean and process correspondence data.
# This function takes 'csvs_clean1' as input, which is a list containing cleaned CSV data.
corresp_cleaning <- function(csvs_clean1) {

  # Extract the first element (correspondence data) from the cleaned CSV list.
  corresp_arr <- csvs_clean1[[1]]

  # Add a new column 'contact_freq' with "yes" if there is a 'contactdate' (and "no" if not).
  # Group the data by 'vesselofficialnumber' and count how many "contacts" there are for each. Save in the "contact_freq" column.
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)

  # Find the column names for 'createdon' and 'contactdate'.
  createdon_field_name <-
    find_col_name(corresp_arr, "created", "on")[1]
  contactdate_field_name <-
    find_col_name(corresp_arr, "contact", "date")[1]

  # Change the data types of 'createdon' and 'contactdate' columns to POSIXct.
  corresp_arr_contact_cnts <-
    change_to_dates(corresp_arr_contact_cnts,
                    createdon_field_name)
  corresp_arr_contact_cnts <-
    change_to_dates(corresp_arr_contact_cnts,
                    contactdate_field_name)

  # Return the cleaned and processed correspondence data.
  return(corresp_arr_contact_cnts)
}

## specific compliance manipulations ----
# Define a function to clean and process compliance data.
# This function takes 'compl_arr' as input, which is a list of compliance dataframes.
compliance_cleaning <- function(compl_arr) {
  # Initialize 'compl' as the input 'compl_arr'.
  # if it is just one df already, do nothing
  compl <- compl_arr

  # Clean the 'week' column by splitting it into three columns with proper classes: 'week_num' (week order number), 'week_start', and 'week_end'.
  compl_clean <-
    map(compl, clean_weeks)

  # Find a column name containing 'permit', 'group', and 'expiration' (permitgroupexpiration).
  permitgroupexpirations <-
    map(compl,
        \(x) {
          grep("permit.*group.*expiration",
               tolower(names(x)),
               value = TRUE)
        })

  # Change the classes of dates in the 'permitgroupexpiration' columns from character to POSIXct.
  compl_dates <-
    compl_clean |>
    imap(\(x, idx) {
      field_name <- permitgroupexpirations[[idx]]
      x |>
        mutate({{field_name}} := as.POSIXct(pull(x[field_name]),
                                            format = "%m/%d/%Y"))
      # change_to_dates(x, permitgroupexpirations[[idx]], "%m/%d/%Y")
    })

  # Return the cleaned and processed compliance data.
  return(compl_dates)
}

# Define a function to prepare file names by categorizing them into two
# subdirectories based on their prefixes.
# This function takes a vector of 'filenames' as input.
prepare_csv_names <- function(filenames) {
  # Define subdirectory names for correspondence and compliance files.
  add_path_corresp <- "from_Fhier/Correspondence"
  add_path_compl <- "from_Fhier/FHIER Compliance"

  # Use 'sapply' to process each filename in the 'filenames' vector.
  my_list <- sapply(filenames, function(x) {
    # Use 'case_when' to categorize filenames based on their prefixes.
    # If a filename starts with "correspond," it is placed in the
    # 'Correspondence' subdirectory. If it starts with "fhier_compliance,"
    # it is placed in the 'FHIER Compliance' subdirectory. Otherwise, it is
    # placed in the 'FHIER Compliance' subdirectory as a default.
    dplyr::case_when(
      startsWith(my_headers_case_function(x), "correspond") ~
        file.path(add_path_corresp,  x),
      startsWith(my_headers_case_function(x), "fhier_compliance") ~
        file.path(add_path_compl,  x),
      .default = file.path(add_path_compl,  x)
    )
  })

  # Convert the resulting list into a character vector and return it.
  return(paste(my_list) %>% as.list())
}

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# Define a function named 'load_csv_names' that takes two parameters: 'my_paths' and 'csv_names_list'
load_csv_names <- function(csv_files_paths, csv_names_list) {
 # Use 'lapply' to add the 'my_inputs' directory path in front of each file name in 'csv_names_list'
  # This creates a list of full file paths for the CSV files
  myfiles <- lapply(csv_names_list, function(x) file.path(csv_files_paths, x))

  # browser()
  print(myfiles)

  # Use 'lapply' again to read all CSV files listed in 'myfiles'
  # The 'read_csv' function from the 'readr' package is used, specifying default column types as 'c' ('character')
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  # Return the contents of the CSV files as a list
  return(contents)
}

# cleaning, regularly done for csvs downloaded from PHIER
# The clean_all_csvs function is defined to clean a list of CSVs (csvs) and has an optional parameter vessel_id_field_name, which specifies the column to trim.
# It returns the list of cleaned CSVs, where each CSV has had its headers unified and the vessel ID column (if specified) trimmed for consistency.

clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
  # Clean headers of all CSVs using the 'clean_headers' function
  csvs_clean0 <- lapply(csvs, clean_headers)

  # Trim 'vesselofficialnumber' column (if specified) in all cleaned CSVs
  csvs_clean1 <-
    trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)

  # Return the list of cleaned CSVs
  return(csvs_clean1)
}

# The clean_headers function is designed to clean and fix the column names of a given dataframe (my_df).
clean_headers <- function(my_df) {
    # Use the 'fix_names' function to clean and fix the column names of the dataframe.
    colnames(my_df) %<>%
        fix_names()

    # Return the dataframe with cleaned and fixed column names.
    return(my_df)
}

# The fix_names function is used to clean and standardize column names to make them suitable for use in data analysis or further processing.
# to use in a function,
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  # Use the pipe operator %>%
  x %>%

    # Remove dots from column names
    str_replace_all("\\.", "") %>%

    # Replace all characters that are not letters or numbers with underscores
    str_replace_all("[^A-z0-9]", "_") %>%

    # Ensure that letters are only in the beginning of the column name
    str_replace_all("^(_*)(.+)", "\\2\\1") %>%

    # Convert column names to lowercase using 'my_headers_case_function'
    my_headers_case_function()
}

# trim vesselofficialnumber, there are 273 white spaces in Feb 2023
# Define a function named 'trim_all_vessel_ids_simple' with two parameters:
# 'csvs_clean_ws' is a list of data frames to be processed,
# 'col_name_to_trim' is an optional column name to trim, default is NA.
# It returns the list of data frames (csvs_clean) where each data frame has a trimmed 'vessel_official_number' column.
trim_all_vessel_ids_simple <- function(csvs_clean_ws, col_name_to_trim = NA) {

  # Use lapply to iterate through each data frame in 'csvs_clean_ws'
  csvs_clean <- lapply(csvs_clean_ws, function(x) {

    # Check if 'col_name_to_trim' is NA
    if (is.na(col_name_to_trim)) {

      # If it's NA, find the column name matching a pattern and store it in 'col_name_to_trim'
      col_name_to_trim <- grep("vessel.*official.*number",
                               tolower(names(x)),
                               value = TRUE)
    }

    # Convert 'col_name_to_trim' to a symbol using 'sym' from tidyverse
    col_name_to_trim_s <- rlang::sym(col_name_to_trim)

    # Trim leading and trailing white spaces in the selected column
    # Hard code vessel_official_number as vessel id
    x %>%
      dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
      # Alternative way of doing the same, not tested
      # dplyr::mutate({{col_name_to_trim_s}} := trimws(!!col_name_to_trim_s)) %>%
      return()
  })

  # Return the list of data frames with trimmed vessel IDs
  return(csvs_clean)
}

# The add_count_contacts function is defined to add two new columns ('was_contacted' and 'contact_freq') to the input data frame ('all_data_df_clean') based on the presence of contact dates.

# It returns the 'result_df', which is the input data frame with the added columns indicating whether a vessel was contacted ('was_contacted') and the frequency of contacts ('contact_freq').

# Use for contacts in the setup function before combining with compliant dataframes
add_count_contacts <- function(all_data_df_clean) {
  # Find the column name for 'contactdate' and 'vesselnumber' in 'all_data_df_clean'
  contactdate_field_name <-
    find_col_name(all_data_df_clean, "contact", "date")[1]
  vessel_id_field_name <-
    find_col_name(all_data_df_clean, "vessel", "number")[1]

  # Apply a series of transformations to 'all_data_df_clean'
  result_df <- all_data_df_clean %>%

    # Add a new column 'was_contacted' with "yes" if 'contactdate' is not NA, or "no" if it is NA
    # TODO: as.factor
    dplyr::mutate(was_contacted =
                    dplyr::if_else(is.na(contactdate_field_name), "no", "yes")) %>%

    # Group the data by 'vesselofficialnumber' and 'was_contacted', and count the occurrences, saving it in 'contact_freq' column
    dplyr::add_count(!!dplyr::sym(vessel_id_field_name), was_contacted, name = "contact_freq")

  # Return the modified data frame with the added 'was_contacted' and 'contact_freq' columns
  return(result_df)
}

# usage: complianceerrors_field_name <- find_col_name(compl_clean_sa, ".*xcompliance", "errors.*")[1]
# TODO what if two names?
# Define a function to find column names in a dataframe based on partial matches.
# This function takes 'mydf' (a dataframe), 'start_part' (the start of the column name),
# and 'end_part' (the end of the column name) as inputs.
find_col_name <- function(mydf, start_part, end_part) {
  # Create a regular expression pattern to search for column names that start with 'start_part'
  # and end with 'end_part'.
  to_search <- paste0(start_part, ".*", end_part)

  # Use 'grep' to search for column names in lowercase that match the pattern.
  # 'value = TRUE' returns the matching column names as a character vector.
  matching_names <- grep(to_search, tolower(names(mydf)), value = TRUE)

  # Return the matching column name(s) as a character vector.
  return(matching_names)
}

# Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"
# The change_to_dates function is defined to convert a specific column ('field_name') in the input data frame ('my_df') to POSIXct date format using the specified 'date_format'.
#
# Inside the function, it uses the mutate function from the dplyr package to modify 'my_df'. The {{field_name}} syntax is used to refer to the column specified by 'field_name'.
# It returns the 'result_df', which is the input data frame with the specified column converted to dates according to the specified 'date_format'.

# ===
change_to_dates <- function(my_df, field_name, date_format = "") {
  # Convert the specified column ('field_name') in 'my_df' to POSIXct date format using 'as.POSIXct'
  # Within the mutate function, it uses pull to extract the column specified by 'field_name' and then applies as.POSIXct to convert the values in that column to POSIXct date format using the provided 'date_format'.

  # browser()
  if (date_format == "") {
    my_tryFormats = c(
      "%m/%d/%Y %I:%M%p",
      "%m/%d/%Y %I:%M %p",
      "%m/%d/%Y %R%OS",
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%d",
      "%Y/%m/%d"
    )
  }

  new_field_name <- str_glue("{field_name}_dttm")

  result_df <-
    my_df |>
    mutate(!!new_field_name := as.POSIXct(!!field_name,
                                      tryFormats = my_tryFormats,
                                      format = date_format))

  # Return the data frame with the specified column converted to dates
  return(result_df)
}

temp_var <-
  get_compl_and_corresp_data(from_fhier_data_path, all_csv_names_list)


compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

names(compl_clean_list) <- c(my_year1, my_year2)

# check
map(compl_clean_list, dim)

# combine years in one df
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

dim(compl_clean)

dim(corresp_contact_cnts_clean0)

## get Metric Tracking (permits from FHIER) ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# file names for all years
processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

# exclude links
processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

# read the rest
processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

# lower names case
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

dim(processed_metrics_tracking_permits)

## get Physical Address List from FHIER ----
# REPORTS / For-hire Primary Physical Address List

fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from_Fhier\address\For-hire Primary Physical Address List_04_09_2024.csv)"
  )

file.exists(fhier_addresses_path)

fhier_addresses <-
  read_csv(fhier_addresses_path,
           # read all as characters
           col_types = cols(.default = 'c'),
           name_repair = fix_names)

# View(fhier_addresses)

# PIMS ----
## get home port processed city and state ----

processed_pims_home_ports_path <-
  file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports.csv")

processed_pims_home_ports <- 
  read_csv(processed_pims_home_ports_path)

# View(processed_pims_home_ports)
# View(vessels_from_pims) - more fields

# Oracle db ----
## get owners addresses ----
db_participants_address_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@secapxdv_dblk
"

db_participants_address_file_path <-
  file.path(my_paths$inputs,
            current_project_name,
            "db_participants_address.rds")
 
# dir.exists(file.path(my_paths$inputs,
#             current_project_name))

# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- connect_to_secpr())
}

db_participants_address_fun <-
  function(db_participants_address) {
    # browser()
    return(dbGetQuery(con,
                      db_participants_address))
  }

db_participants_address <-
  read_rds_or_run(
    db_participants_address_file_path,
    db_participants_address_query,
    db_participants_address_fun
    # force_from_db = "yes"
  ) |>
  remove_empty_cols() |>
  clean_headers()
# 2024-04-09 run for db_participants_address.rds: 52.22 sec elapsed

dim(db_participants_address)

# Data from the previous results of "egregious violators for investigation" ----
# Download first as .xlsx

# get previous results ---
prev_result_path <- 
  file.path(my_paths$inputs,
            current_project_basename,
            "egregious violators for investigation_2023-08-15_to_2024-02-13_OLE.xlsx")

# file.exists(prev_result_path)

prev_result <-
  read_xlsx(prev_result_path) |> 
  remove_empty_cols() |> 
  clean_headers()

dim(prev_result)

# Results ----
results <-
  c(
    "compl_clean",
    "corresp_contact_cnts_clean0",
    "prev_result",
    "processed_metrics_tracking_permits",
    "fhier_addresses",
    "processed_pims_home_ports",
    "db_participants_address"
  )

cat(c("Data are in:",
      results),
    sep = "\n")

# end of get data

# compl_clean
# corresp_contact_cnts_clean0
# prev_result
# processed_metrics_tracking_permits
# fhier_addresses
# processed_pims_home_ports
# db_participants_address
# vessels_permits_participants

# ---- Preparing compliance info ----

## Permit Expiration ----
### ---- add permit_expired column ----
# Explanations:
# 1. Add a new column 'permit_expired' using 'mutate'.
# 2. Use 'case_when' to determine if 'permit_groupexpiration' is greater than permit_expired_check_date.
# 3. If true, set 'permit_expired' to "no", otherwise set it to "yes".

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is after permit_expired_check_date than "not expired"
  mutate(permit_expired =
           case_when(
             permit_groupexpiration > permit_expired_check_date ~ "no",
             .default = "yes"
           ))

# glimpse(compl_clean_w_permit_exp)

### get only not expired last 27 weeks of data minus grace period ----
compl_clean_w_permit_exp__not_exp <-
  compl_clean_w_permit_exp |>
  # the last 27 week
  filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  filter(week_end < last_week_start) |>
  # not expired
  filter(tolower(permit_expired) == "no")

min(compl_clean_w_permit_exp__not_exp$permit_groupexpiration)
# [1] "2024-02-29 EST"

min(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2023-08-14"

max(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2024-01-29"

max(compl_clean_w_permit_exp__not_exp$week_end)
# [1] "2024-02-04"

## ---- add year_month column ----

compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp__not_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last check period
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)

dim(compl_clean_w_permit_exp_last_half_year)

## ---- Have only SA and dual permits ----
# Use 'filter' to select rows where 'permitgroup' contains "CDW", "CHS", or "SC".
compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  filter(grepl("CDW|CHS|SC", permitgroup))

# today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"
# [1] "2024-02-16"
# [1] "2024-04-09"

dim(compl_clean_w_permit_exp_last_half_year__sa)

## fewer columns ----
remove_columns <- c(
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

# Explanations:
# 1. Use 'select' to remove columns specified in 'remove_columns'.
# 2. Use 'distinct' to keep only unique rows in the resulting data frame.
compl_clean_w_permit_exp_last_half_year__sa__short <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  select(-any_of(remove_columns)) |> 
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa__short)

## work with the whole period ----
## add compliant_after_overr ----

tic("compl_overr")
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr <-
  compl_clean_w_permit_exp_last_half_year__sa__short |>
  add_compliant_after_override(overridden_col_name = "overridden_",
                               compliance_col_name = "compliant_")
toc()
# compl_overr: 8.76 sec elapsed

# check
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |> 
  select(compliant_, overridden_, compliant_after_override) |>
  count(compliant_, overridden_, compliant_after_override)
#   compliant_ overridden_ compliant_after_override     n
#   <chr>      <chr>       <chr>                    <int>
# 1 NO         NO          no                       11258
# 2 NO         YES         yes                         70
# 3 YES        NO          yes                      29628

# check
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$compliant_after_override |> 
  unique()
# [1] "yes" "no" 

dim(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr)

# check
n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number) ==
  n_distinct(
    compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$vessel_official_number
  )
# T

## get only non-compliant for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |>
  # not compliant
  filter(tolower(compliant_after_override) == "no")

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)

## keep only vessels with info for all weeks in the period ----
all_weeks_num <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  select(week) |>
  distinct() |>
  nrow()

# Explanations:
# 1. Group the data frame by 'vessel_official_number'.
# 2. Filter the groups based on the condition that the number of distinct weeks is greater than or equal to 'all_weeks_num'.
# 3. Remove the grouping from the data frame.
# 4. Exclude the 'week' column from the resulting data frame, we don't need it anymore.

compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  group_by(vessel_official_number) |>
  filter(n_distinct(week) >= all_weeks_num) |> 
  ungroup() |> 
  select(-week)

compl_clean_w_permit_exp_last_half_year__sa_non_c |> dim()

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present)

## check the last report date ----
### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present |>
  select(vessel_official_number) |>
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids)

### check these ids in the full compliance information ----
compl_clean_w_permit_exp_last_half_year__sa |>
  filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # [1] 1938   22
  group_by(vessel_official_number) |>
  filter(
    tolower(compliant_) == "yes" &
      tolower(overridden_) == "yes" &
      # not the current month
      year_month < as.yearmon(data_file_date)
  ) |>
  nrow()
# 0 OK!

# Results: prepared Compliance is in compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present

# ---- Preparing Correspondence ----

## ---- remove 999999 ----
# Explanations:
# Create a new data frame 'corresp_contact_cnts_clean' by filtering 'corresp_contact_cnts_clean0' based on the condition.
# 1. Use 'filter' to select rows where 'vessel_official_number' does not start with "99999".
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

n_distinct(corresp_contact_cnts_clean$vesselofficial_number)

# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

## new requirement 2024-02-26 ----
# Michelle
# It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# check
corresp_contact_cnts_clean |>
  select(calltype, voicemail, contacttype) |>
  distinct() |> head(10)

we_called_filter <-
  quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))

we_emailed_once_filter <-
  quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outgoing"
  ))

# don't need a second contact
they_contacted_direct_filter <-
  quo(
    any(
      tolower(calltype) == "incoming"
      )
  )

# corresp_filter <-
#   quo(!!they_contacted_direct_filter |
#         (
#           contact_freq > 1 &
#             (!!we_called_filter &
#                !!we_emailed_once_filter)
#         ))

# calltype voicemail contacttype

# two_attempts_filter <-
#   quo(contact_freq > 1 &
#         any(tolower(contacttype) == "call"))

# use the filter
corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  # select(calltype) |> distinct()
  filter(tolower(calltype) == "incoming" |
           (
             contact_freq > 1 &
               (!!we_called_filter &
                  !!we_emailed_once_filter)
           ))
  # filter(tolower(calltype) == "incoming" |
  #          (contact_freq > 1 &
  #             (
  #               any(
  #                 tolower(contacttype) == "call" &
  #                   tolower(calltype) == "outgoing" &
  #                   tolower(contacttype) %in% c("email", "other") &
  #                   tolower(calltype) == "outcoming"
  #               )
  #             ))) |>
  # filter(vesselofficial_number == '1149600') |>
  # glimpse()
# filter(!!corresp_filter)

corresp_contact_cnts_clean_direct_cnt_2atmps |> 
  filter(vesselofficial_number == '1168661') |> 
  glimpse()

# corresp_contact_cnts_clean |>
#   filter(!!they_contacted_direct_filter |
#   (contact_freq > 1 &
#             (!!we_called_filter &
#                !!we_emailed_once_filter))) |> 
#   glimpse()

dim(corresp_contact_cnts_clean)
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)

n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)

## fix dates ----
# check
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1) |> str()
 # chr "02/15/2024 03:15PM"

# Explanations:
# Mutate new columns 'created_on_dttm' and 'contact_date_dttm' by parsing 'created_on' and 'contact_date' using lubridate package.
# The date-time formats considered are "mdY R".
# 1. Use the pipe operator to pass 'corresp_contact_cnts_clean_direct_cnt_2atmps' as the left-hand side of the next expression.
# 2. Use 'mutate' to create new columns with parsed date-time values.
# 3. Use 'lubridate::parse_date_time' to parse the date-time values using the specified formats.

corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  mutate(
    created_on_dttm =
      lubridate::parse_date_time(created_on,
                                 c("mdY R")),
    contact_date_dttm =
      lubridate::parse_date_time(contact_date,
                                 c("mdY R"))
  )

# check
str(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm)
# POSIXct[1:29089], format: "2024-02-15 15:15:00" 

# preprared Correspondence is in 
# corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates

# Join correspondence with compliance ----
# Explanations:
# Create a new dataframe 'compl_corr_to_investigation' by performing an inner join between
# 'correspondence' and 'compliance'.
# The join is performed on the column 'vessel_official_number'.
# Use 'multiple = "all"' and 'relationship = "many-to-many"' to handle multiple matches during the join.
# 1. Use the 'inner_join' function from the dplyr package to combine the two dataframes based on the specified columns.
# 2. Pass the column names and other parameters to the 'by', 'multiple', and 'relationship' arguments.

compl_corr_to_investigation <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation)

# check
n_distinct(compl_corr_to_investigation$vesselofficial_number)

# View(compl_corr_to_investigation)

## save number of vessels to investigate for checks ----
num_of_vsl_to_investigate <- 
  n_distinct(compl_corr_to_investigation$vesselofficial_number)

# Results: Compl & corresondence together are in
# compl_corr_to_investigation

# ---- output needed investigation ----
# 1. remove unused columns
# 2. create additional columns
# 3. mark vessels already in the know list (prev_result)
# 4. duals vs. sa_only

## 1. remove extra columns ----

# Explanations:
# Group the dataframe by the 'vessel_official_number' column and then apply the 'summarise_all' function.
# The 'summarise_all' function applies the specified function (in this case, 'concat_unique') to each column.

# Note: 'concat_unique' is not a standard R function, it is a custom function defined previously.

colnames(compl_corr_to_investigation) |> 
  cat(sep = '",\n"')

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

# Explanations:
# 1. Exclude columns specified in 'unused_fields' from the data frame.
# 2. Group the data frame by 'vessel_official_number'.
# 3. Apply the custom function 'concat_unique' to all columns to concatenate unique non-missing values into a single string.
# 4. Remove the grouping from the data frame.

compl_corr_to_investigation_short <-
  compl_corr_to_investigation |>
  # compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id |>
  select(-any_of(unused_fields)) |>
  group_by(vessel_official_number) |>
  summarise_all(concat_unique) |>
  ungroup()

# print_df_names(compl_corr_to_investigation_short)

compl_corr_to_investigation_short |> glimpse()

dim(compl_corr_to_investigation_short)

## 2. create additional columns ----
### add list of contact dates and contact type in parentheses  -----

# put names into vars (needed, bc spaces and underscores placements vary from source to source)
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation_short, "contact", "date")[1]

contacttype_field_name <-
  find_col_name(compl_corr_to_investigation_short, "contact", "type")[1]

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation_short, ".*contact", "number.*")[1]

# Explanations:
# Define a function 'get_date_contacttype' that takes a dataframe 'compl_corr_to_investigation' as input.
# Perform several data manipulation steps to extract and organize relevant information.
# 1. Add a new column 'date__contacttype' by concatenating the values from 'contactdate_field_name' and 'contacttype'.
# 2. Select only the 'vessel_official_number' and 'date__contacttype' columns.
# 3. Arrange the dataframe by 'vessel_official_number' and 'date__contacttype'.
# 4. Keep distinct rows based on 'vessel_official_number' and 'date__contacttype'.
# 5. Group the dataframe by 'vessel_official_number'.
# 6. Summarize the data by creating a new column 'date__contacttypes' that concatenates all 'date__contacttype' values for each vessel separated by a comma.
# 7. Return the resulting dataframe.
get_date_contacttype <-
  function(my_df) {
    my_df |>
      # add a new column date__contacttype with contactdate and contacttype
      mutate(date__contacttype =
                      paste(!!sym(contactdate_field_name),
                            !!sym(contacttype_field_name))) |>
      # use 2 columns only
      select(vessel_official_number, date__contacttype) |>
      # sort
      arrange(vessel_official_number, date__contacttype) |>
      distinct() |>
      group_by(vessel_official_number) |>
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = 
                  paste(date__contacttype, collapse = ", ")) %>%
      return()
  }

# use the function
date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation_short)

dim(date__contacttype_per_id)

# glimpse(date__contacttype_per_id)

#### add the new column back ----
compl_corr_to_investigation__corr_date <-
  left_join(compl_corr_to_investigation_short,
            date__contacttype_per_id) |>
  # Joining with `by = join_by(vessel_official_number)`
  # this columns are not longer needed
  select(-all_of(c(
    contactdate_field_name,
    contacttype_field_name
  )))
  
# check
compl_corr_to_investigation__corr_date |> 
  glimpse()

### add pims home port info ----
# compl_corr_to_investigation_short_dup_marked__hailing_port <-
compl_corr_to_investigation__corr_date__hailing_port <- 
  left_join(
    compl_corr_to_investigation__corr_date,
    processed_pims_home_ports,
    join_by(vessel_official_number)
  ) |> 
  rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)

# stopped here with compl_corr_to_investigation__corr_date__hailing_port

### add prepared addresses ----

prep_addresses_path <-
  file.path(current_project_path,
            str_glue("{current_project_basename}_prep_addresses.R"))

file.exists(prep_addresses_path)

source(prep_addresses_path)

# result: compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr

## 3. mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

vessels_to_mark_ids <-
  prev_result |>
  select(vessel_official_number)

dim(vessels_to_mark_ids)

#### mark these vessels ----
# Explanations:
# Create a new column 'duplicate_w_last_time' in the dataframe 'compl_corr_to_investigation_short'.
# This column is marked with "duplicate" for rows where 'vessel_official_number' is present in the list of vessel IDs to mark as duplicates ('vessels_to_mark_ids').
# For all other rows, it is marked as "new".
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked)

### check ----
n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked$vessel_official_number)

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |>
  count(duplicate_w_last_time)
# 1 duplicate               108
# 2 new                      48

## 4. how many are duals? ----
# Explanations:
# Create a new dataframe 
# Use the 'mutate' function to add a new column 'permit_region' based on conditions.
# If 'permitgroup' contains any of the specified patterns ("RCG", "HRCG", "CHG", "HCHG"),
# set 'permit_region' to "dual". Otherwise, set 'permit_region' to "sa_only".
# If none of the conditions are met, set 'permit_region' to "other".
# The resulting dataframe includes the original columns from 'compl_corr_to_investigation_short_dup_marked'
# along with the newly added 'permit_region' column.

compl_corr_to_investigation_short_dup_marked__permit_region <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |> 
  # compl_corr_to_investigation_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols |>
  mutate(permit_region =
           case_when(
             grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "dual",
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "other"
           ))

# Explanations:
# Use the 'select' function to extract the columns 'vessel_official_number' and 'permit_region'
# from the dataframe 'compl_corr_to_investigation_short_dup_marked__permit_region'.
# Use the 'distinct' function to keep only unique combinations of 'vessel_official_number' and 'permit_region'.
# Use the 'count' function to count the occurrences of each unique 'permit_region'.
# The resulting count provides the frequency of each 'permit_region'.
region_counts <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  select(vessel_official_number, permit_region) |>
  distinct() |>
  count(permit_region)

n_distinct(compl_corr_to_investigation_short_dup_marked__permit_region$vessel_official_number)

### dual permitted cnts ----

region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100

# Print out results ----
## add additional columns in front ----

additional_column_name1 <-
  str_glue(
    "Confirmed Egregious? (permits must still be active till {permit_expired_check_date}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call/other (voicemail counts) and at least 1 email)"
  )

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  add_column(
    !!(additional_column_name1) := NA,
    Notes = NA,
    .before = 2
  )

# print_df_names(compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

result_path <- 
  file.path(my_paths$outputs,
            current_project_basename,
            str_glue("egregious_violators_to_investigate_{today()}.csv"))

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns |>
write_csv(result_path)

cat("Result:",
    "compl_corr_to_investigation_short_dup_marked__permit_region__add_columns",
    "and",
    str_glue("egregious_violators_to_investigate_{today()}.csv"),
    sep = "\n")

