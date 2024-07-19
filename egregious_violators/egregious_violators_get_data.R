# get data for egregious violators
# use from egregious_violators_start.R

#' This section loads and prepares various datasets required for the egregious violators analysis
#'
#' - Compliance records
#'
#' - Correspondence logs
#'
#' - Permit information
#'
#' - Address data
#'
#' The data is loaded from CSV files, databases, and previously processed results.
#'
#' This section outlines the different data sources and their purposes in the analysis.
#'  
#' The following data are loaded from files or from the Oracle database.
#' 
#' 1) compliance data
#' Download files from FHIER / Reports / FHIER COMPLIANCE REPORT 
#' 
#' For the last 6 month
#' 
#' FHIER_Compliance_...csv
#' 
#' 2) correspondence data
#' 
#' Download files from FHIER / Home / Correspondence
#' 
#' Actions / Download 
#' 
#' For the whole period, starting 01/01/2021
#' 
#' "~\my_inputs\from_Fhier\Correspondence\Correspondence_2024_02_15.csv"
#' 

#' 3) processed Metrics tracking
#' 
#' From a separate code
#'  
#' For the last 6 month
#' 
#' SEFHIER_permitted_vessels_nonSRHS_YEAR.csv
#' 

#' 4) Physical Address List from FHIER
#' 
#' Downloaded from REPORTS / For-hire Primary Physical Address List
#' 
#' For the whole period, starting 01/01/2021
#' 
#' "For-hire Primary Physical Address List.csv"
#' 

#' 5) home port cleaned city and state from PIMS
#' 
#' "~\R_files_local\my_outputs\home_ports\vessels_from_pims_ports.csv"
#' 

#' 6) address information from Oracle db
#' 
#' "db_participants_address.rds"
#' 

#' 7) Previous results (from google drive)
#' 
#' There are two options here, to downloaded the file as .xlsx or to get it directly from Google drive into R.
#' 
#' "~\egregious_violators\egregious_violators_to_investigate_2024-05-17.xlsx"
#' 

# FHIER ----
#' This section focuses on data from the Fisheries Information System for Harvest and Effort Reporting (FHIER)
#' 
#' Compliance
#' 
#' Correspondence
#' 
#' Permit info from processed metrics tracking
#' 

## Compliance and Correspondence data ----

#' This section handles the processing of compliance reports and correspondence data from FHIER
#' 
#' It reads Compliance and Correspondence CSV files, cleans them by trimming vessel IDs and cleaning column names, and processes correspondence data to add a column indicating if a contact was made. The file paths and the processing logic differ based on the user running the code.
#' 

#' Read correspondence and compliance csvs
#' 
#' Load CSV files into a list, treating all columns as character type
#' 
csv_contents <- 
  lapply(all_csv_full_paths_list, 
         readr::read_csv, 
         col_types = readr::cols(.default = 'c'))

#' Clean all CSVs: Trim vessel IDs and clean column names
#' 
#' Apply cleaning functions to standardize data across all CSV files
#' 
#' Replace all non-alphanumeric characters with underscores ('_'), unify the case
#' 
csvs_clean1 <- 
  auxfunctions::clean_all_csvs(csv_contents)

#' Every time processing for Compliance and Correspondence downloaded from FHIER
#' 
#' For correspondence:
#'
#' Extract the first element (correspondence data) from the cleaned CSV list.
#'
#' Add a new column named "was_contacted", which indicates whether a contact was made with each vessel based on the presence of a contact date. If the contact date is missing (`NA`), it assigns "no"; otherwise, it assigns "yes".
#'
#' - The `add_count` function is then used to count the number of contacts per vessel, distinguishing between vessels that were contacted and those that were not. The result is stored in a new column named "contact_freq".
#'
#' Change to date format `created_on` and `contact_date` fields
#'
#' Clean and process correspondence data, adding contact information and frequency
#' 

corresp_contact_cnts_clean0 <- 
  csvs_clean1[[1]] |> 
  auxfunctions::corresp_cleaning()

# Display the structure of the cleaned correspondence data
head(corresp_contact_cnts_clean0) |> 
  glimpse()

#' For compliance:
#' 
#' Clean and process compliance data for all years
#' 
#' Use all dataframes from the csvs_clean1 list except the first (correspondence)
#' 
compl_clean_list <-
  csvs_clean1[2:length(csvs_clean1)] |>
  auxfunctions::compliance_cleaning()

#' Assign analysis years as names to the compliance data frames for easy reference
names(compl_clean_list) <- c(my_year1, my_year2)

#' Check the size of each cleaned compliance data frame to ensure proper data loading and processing
purrr::map(compl_clean_list, dim)

#' Example result for dimensions check
# $`2023`
# [1] 149731     20
# 
# $`2024`
# [1] 71350    20

#' This step merges the cleaned compliance data from multiple years into a single dataset for easier analysis

compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

#' Check dimensions of the combined compliance data frame
#' This helps verify the size of the merged dataset and ensure all data was combined correctly
dim(compl_clean)
# [1] 221081     20

#' Check dimensions of the cleaned correspondence data frame
dim(corresp_contact_cnts_clean0)
# [1] 34549    22

## Get Metric Tracking (permits from FHIER) ----

#' The metrics tracking data contains permit information from FHIER (For-Hire Integrated Electronic Reporting Program)
#' 
#' It is processed using a separate script (processing_metrics_tracking.R) stored on Google Drive
#'  
#' Read the processed metrics tracking files for all years
processed_metrics_tracking_permits <-
  purrr::map_df(processed_metrics_tracking_file_names,
         readr::read_rds)

#' Convert column names to lowercase for consistency
#' 
#' This ensures uniform naming conventions across different datasets
#' 
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

#' Example of column names
# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

#' Check dimensions of the processed metrics tracking data frame
dim(processed_metrics_tracking_permits)
#' An example 
# [1] 9977    9

## Physical Address List from FHIER ----
#' 
#' Download first from REPORTS / For-hire Primary Physical Address List.
#' 
#' Load FHIER addresses from the provided path
#' 
#' This dataset contains physical address information for for-hire vessels.
#' 
#' Read all columns as characters.
#' 
#' Use the same column names convention.
#' 

fhier_addresses <-
  readr::read_csv(fhier_addresses_path,
           col_types = readr::cols(.default = 'c'),
           name_repair = auxfunctions::fix_names)

#' Check dimensions of the loaded FHIER addresses data frame
dim(fhier_addresses)
# Example result: [1] 3386    7

# PIMS ----

#' 
#' Load processed PIMS (Permit Information Management System) home port data
#' 

## Home port processed city and state ----

#' This dataset contains information about vessel home ports, including city and state
#' 

processed_pims_home_ports <- 
  readr::read_csv(processed_pims_home_ports_path)

# Example
dim(processed_pims_home_ports)
# [1] 23303     3

# Load from Oracle db ----
## Get owners addresses ----

#' Create parameters for `read_rds_or_run` function to read or download "participants address"
#' 
# Define the SQL query to fetch participant address data
db_participants_address_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@secapxdv_dblk
"

# Set the file path for storing or reading the participant address data

# It uses the predefined path to the input directory and a file name to read or write to.
db_participants_address_file_path <-
  file.path(current_project_input_path,
            "db_participants_address.rds")
 
#' 
#' Attempt to establish a connection to Oracle database
#' 
#' Print an error message if no connection, but keep running the code.
if (!exists("con")) {
  try(con <- auxfunctions::connect_to_secpr())
}

#' Define a parameter for a function to fetch participant address data from the database
db_participants_address_fun <-
  function(db_participants_address) {
    # browser() # Commented out browser function for debugging
    return(dbGetQuery(con,
                      db_participants_address))
  }

#' 
#' Fetch or load participant address data, clean it, and prepare for analysis, using the parameters.
#' 
#' Read the file with db_participants_address if exists, 
#'
#' load from the Oracle database if not,
#'
#' remove empty columns,
#'
#' change column names the same way as everything else.
#' 
db_participants_address <-
  auxfunctions::read_rds_or_run(
    db_participants_address_file_path,
    db_participants_address_query,
    db_participants_address_fun,
    #' If you want to update the existing file, change the NULL to "yes" 
    force_from_db = NULL
  ) |>
  auxfunctions::remove_empty_cols() |>
  auxfunctions::clean_headers()

# Data from the previous results of "egregious violators for investigation" ----

#' The following section deals with data from previous "egregious violators for investigation" results
#' 
#' Instructions for retrieving previous results
#' 
#' There are 2 functions,
#' 
#' get_previous_result_from_local_file() assumes you have downloaded the previous results and 
#' 
#' get_previous_result_from_google_drive() gets data directly from Google drive
#' 
#' Run only one of them and save the dataframe in `prev_result` variable. 
#' 

#' Function to retrieve and process previous results from a downloaded file
get_previous_result_from_local_file <- function() {
  
  # Download first as .xlsx from Google drive
  
  # Read,
  # remove empty columns,
  # change column names the same way as everything else.
  prev_result0 <-
    auxfunctions::my_read_xlsx(prev_result_path) |>
    auxfunctions::remove_empty_cols() |>
    auxfunctions::clean_headers()
  
  # An example
  dim(prev_result0)
  # [1] 151  42
  
  # clean excel number conversions, remove ".0" at the end
  prev_result <-
    prev_result0 |>
    dplyr::mutate(vessel_official_number =
                    stringr::str_replace(vessel_official_number, "\\.0$", ""))
  
  return(prev_result)
}

#' Function to retrieve and process previous results directly from Google Drive
get_previous_result_from_google_drive <- function() {
  
  previous_result_google_ss_name <-
    # Takes the base name of the file from `prev_result_path` (removes directory path)
    basename(prev_result_path) |>
    # Removes the file extension from the base name
    tools::file_path_sans_ext()
  
  # When asked for the authentication the first time choose the appropriate option and follow the instructions. If you are writing again in the same R session you can choose the option 2 and it will confirm your access automatically.
  # We assuming that there is only one file with that name in your Google drive.
  
  my_previous_ss <- googlesheets4::gs4_find(previous_result_google_ss_name, n_max = 1)
  
  # Load it to R.
  # And clean it as usual, changing headers to lower case with underscores and removing empty columns
  previous_result <-
    googlesheets4::read_sheet(my_previous_ss) |>
    auxfunctions::remove_empty_cols() |>
    auxfunctions::clean_headers()
  
  return(previous_result)
}

#' Un-comment and run one of the functions.
#' 
# prev_result <- get_previous_result_from_local_file()
# or
prev_result <- get_previous_result_from_google_drive()

# Results ----
#' Create a vector of data frame names containing the results
#' 
results <-
  c(
    "compl_clean",
    "corresp_contact_cnts_clean0",
    "processed_metrics_tracking_permits",
    "fhier_addresses",
    "processed_pims_home_ports",
    "db_participants_address",
    "prev_result"
  )

auxfunctions::pretty_print(results, "Data are in:")
