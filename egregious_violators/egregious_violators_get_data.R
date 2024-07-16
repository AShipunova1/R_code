# get data for egregious violators
# use from egregious_violators_start.R

#' The following data are loaded from files or from the Oracle database
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
#' "~\egregious_violators\egregious_violators_to_investigate_2024-05-17.xlsx"
#' 

# FHIER ----

#' Compliance
#' 
#' Correspondence
#' 
#' permit info from processed metrics tracking
#' 

## Compliance and Correspondence data ----
#' This part reads Compliance and Correspondence CSV files, cleans them by trimming vessel IDs and cleaning column names, and processes correspondence data to add a column indicating if a contact was made. The file paths and the processing logic differ based on the username running the code.

#' Read correspondence and compliance csvs
#' 
#' Read all columns as characters
csv_contents <- 
  lapply(all_csv_full_paths_list, 
         readr::read_csv, 
         col_types = readr::cols(.default = 'c'))

#' Clean all CSVs: Trim vessel IDs and clean column names
#' Replace all non-alphanumeric characters with underscores ('_')
csvs_clean1 <- 
  auxfunctions::clean_all_csvs(csv_contents)

#' Every time processing for Compliance and Correspondence downloaded from FHIER

#' For correspondence:
#' Extract the first element (correspondence data) from the cleaned CSV list.
#' Add a new column named "was_contacted", which indicates whether a contact was made with each vessel based on the presence of a contact date. If the contact date is missing (`NA`), it assigns "no"; otherwise, it assigns "yes".
#' - The `add_count` function is then used to count the number of contacts per vessel, distinguishing between vessels that were contacted and those that were not. The result is stored in a new column named "contact_freq".
#' Change to date format `created_on` and `contact_date` fields

#' Process and clean the correspondence data
corresp_contact_cnts_clean0 <- 
  csvs_clean1[[1]] |> 
  auxfunctions::corresp_cleaning()

# Display the structure of the cleaned correspondence data
head(corresp_contact_cnts_clean0) |> 
  glimpse()

#' For compliance:
#' Clean compliance data
compl_clean_list <-
  csvs_clean1[2:length(csvs_clean1)] |>
  auxfunctions::compliance_cleaning()

#' Use the analysis years as data frame names for compliance
names(compl_clean_list) <- c(my_year1, my_year2)

#' Check dimensions of each cleaned compliance data frame
purrr::map(compl_clean_list, dim)

#' Example result for dimensions check
# $`2023`
# [1] 149731     20
# 
# $`2024`
# [1] 71350    20

#' Combine compliance data for all years into one data frame
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

#' Check dimensions of the combined compliance data frame
dim(compl_clean)
# [1] 221081     20

#' Check dimensions of the cleaned correspondence data frame
dim(corresp_contact_cnts_clean0)
# [1] 34549    22

## get Metric Tracking (permits from FHIER) ----

#' Read the processed metrics tracking files for all years
processed_metrics_tracking_permits <-
  purrr::map_df(processed_metrics_tracking_file_names,
         readr::read_rds)

#' Convert column names to lowercase for consistency
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
#' Download first from REPORTS / For-hire Primary Physical Address List
#' 
#' Load FHIER addresses from the provided path
#' 
#' Read all columns as characters
#' 
#' Use the same column names convention

fhier_addresses <-
  readr::read_csv(fhier_addresses_path,
           col_types = readr::cols(.default = 'c'),
           name_repair = auxfunctions::fix_names)

#' Check dimensions of the loaded FHIER addresses data frame
dim(fhier_addresses)
# Example result: [1] 3386    7

# PIMS ----
## home port processed city and state ----

processed_pims_home_ports <- 
  readr::read_csv(processed_pims_home_ports_path)

# Example
dim(processed_pims_home_ports)
# [1] 23303     3

# Load from Oracle db ----
## get owners addresses ----
#' Create parameters for `read_rds_or_run` function to read or download "participants address"
db_participants_address_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@secapxdv_dblk
"

#' It uses the predefined path to the input directory and a file name to read or write to.
db_participants_address_file_path <-
  file.path(current_project_input_path,
            "db_participants_address.rds")
 
#' Try to connect to Oracle if it is not done already.
#' Print an error message if no connection, but keep running the code.
if (!exists("con")) {
  try(con <- auxfunctions::connect_to_secpr())
}

#' The function parameter for read_rds_or_run
db_participants_address_fun <-
  function(db_participants_address) {
    # browser() # Commented out browser function for debugging
    return(dbGetQuery(con,
                      db_participants_address))
  }

#' Read the file with db_participants_address if exists, 
#' load from the Oracle database if not,
#' remove empty columns,
#' change column names the same way as everything else.
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

## Get vessels with changed owner ----
#' The same as above with new parameters. 
#' Prepare the parameters.

#' The SQL query.
#' Select only vessels with SA permits,
#' not expired by today,
#' with different owners or 
#' permit status indicating something other than usual
#' 
permit_vessel_w_changed_owner_query <- 
"SELECT
  *
FROM
  srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE
  top IN ( 'CHS', 'SC', 'CDW' )
  AND ( expiration_date >= sysdate
        OR end_date >= sysdate )
  AND ( ( new_owner != prior_owner
          AND new_owner != 0
          AND prior_owner != 0 )
        OR permit_status NOT IN ( 'MAILED', 'RENEWED' ) )
ORDER BY
  vessel_id
"

#' It uses the predefined path to the input directory and a file name to read or write to.
permit_vessel_w_changed_owner_file_path <-
  file.path(current_project_input_path,
            "permit_vessel_w_changed_owner.rds")
 
# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- auxfunctions::connect_to_secpr())
}

permit_vessel_w_changed_owner_fun <-
  function(permit_vessel_w_changed_owner) {
    return(dbGetQuery(con,
                      permit_vessel_w_changed_owner))
  }

#' Read the file with db_participants_address if exists, 
#' load from the Oracle database if not,
#' remove empty columns,
#' change column names the same way as everything else.
permit_vessel_w_changed_owner <-
  auxfunctions::read_rds_or_run(
    permit_vessel_w_changed_owner_file_path,
    permit_vessel_w_changed_owner_query,
    permit_vessel_w_changed_owner_fun,
    #' If you want to update the existing file, change the NULL to "yes" 
    force_from_db = NULL
  ) |>
  auxfunctions::remove_empty_cols() |>
  auxfunctions::clean_headers()

dim(permit_vessel_w_changed_owner)
# 75
# 143 with statuses

permit_vessel_w_changed_owner |> 
  head() |> 
  dplyr::glimpse()

# Data from the previous results of "egregious violators for investigation" ----

#' There are 2 functions, 
#' 
#' get_previous_result_from_local_file() assumes you have downloaded the previous results and 
#' 
#' get_previous_result_from_google_drive() gets data directly from Google drive
#' 
#' Run only one of them and save the dataframe in prev_result variable. 

#' From a local file
get_previous_result_from_local_file <- function() {
  
  #' Download first as .xlsx from Google drive
  
  #' Read,
  #' remove empty columns,
  #' change column names the same way as everything else.
  prev_result0 <-
    auxfunctions::my_read_xlsx(prev_result_path) |>
    auxfunctions::remove_empty_cols() |>
    auxfunctions::clean_headers()
  
  # An example
  dim(prev_result0)
  # [1] 151  42
  
  #' clean excel number conversions, remove ".0" at the end
  prev_result <-
    prev_result0 |>
    dplyr::mutate(vessel_official_number =
                    stringr::str_replace(vessel_official_number, "\\.0$", ""))
  
  return(prev_result)
}

#' To get previous data directly from Google drive
get_previous_result_from_google_drive <- function() {
  
  
  previous_result_google_ss_name <-
    basename(prev_result_path) |>
    tools::file_path_sans_ext()
  
  # When asked for the authentication the first time choose the appropriate option and follow the instructions. If you writing again in the same R session you can choose the option 2 and it will confirm your access automatically.
  # Assuming that there is only one file with that name.
  
  my_previous_ss <- googlesheets4::gs4_find(previous_result_google_ss_name, n_max = 1)
  
  # load it to R
  # And clean it as usual, changing headers to lower case with underscores and removing empty columns
  previous_result <-
    googlesheets4::read_sheet(my_previous_ss) |>
    auxfunctions::remove_empty_cols() |>
    auxfunctions::clean_headers()
  
  return(previous_result)
}

#' Un-comment and run one of the functions
# prev_result <- get_previous_result_from_local_file()
# or
prev_result <- get_previous_result_from_google_drive()

# Results ----
results <-
  c(
    "compl_clean",
    "corresp_contact_cnts_clean0",
    "processed_metrics_tracking_permits",
    "fhier_addresses",
    "processed_pims_home_ports",
    "db_participants_address",
    "permit_vessel_w_changed_owner",
    "prev_result"
  )

auxfunctions::pretty_print(results, "Data are in:")
