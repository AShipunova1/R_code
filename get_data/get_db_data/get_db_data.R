# files to read or create:
# mv_safis_trip_download.rds
# mv_tms_trip_notifications.rds
# permit_info.rds
# trips.rds
# trip_coord.rds
# trip_neg.rds
# trips_notifications.rds
# vessels_permits.rds
# dates.rds
# compl_err_db_data_raw.rds

# help functions (in get_data) ----
# Load the 'tidyverse' library, which includes a collection of packages for data manipulation and visualization.
library(tidyverse)

# Load the 'magrittr' library, which provides piping data between functions.
library(magrittr)

# Load the 'readxl' library, which is used for reading Excel files with the '.xlsx' extension.
library(readxl)

# Load the 'ROracle' library, which provides Oracle database access for R.
library(ROracle)

# Load the 'tictoc' library, which is used for timing code execution.
library(tictoc)

# Set an option in the 'dplyr' package to control the display of summarization information.
# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# Define a function named 'get_username' that retrieves the username of the current user.
get_username <- function() {
    # Use 'Sys.info()' to obtain system information and then extract the 'user' field.
    # Convert the result to a character to ensure it's in a usable format.
    return(as.character(Sys.info()["user"]))
}

# set working directories for get data ----

# Define a function named 'get_current_file_directory' to obtain the directory path of the current file.
get_current_file_directory <- function() {
    # Use the 'rstudioapi::getSourceEditorContext()' function to access the editor context in RStudio,
    # and then extract the 'path' field to get the full path of the currently open file.
    # Finally, use 'dirname()' to extract the directory part of the path.
    return(dirname(rstudioapi::getSourceEditorContext()$path))
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

# ===
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

# ===
# Define a function named 'connect_to_secpr'.
# It returns the established database connection (con), which can be used to interact with the "SECPR" database in R.
# usage:
# con <- connect_to_secpr()
connect_to_secpr <- function() {
    # Retrieve the username associated with the "SECPR" database from the keyring.
    my_username <- keyring::key_list("SECPR")[1, 2]

    # Use 'dbConnect' to establish a database connection with the specified credentials.
    con <- dbConnect(
        dbDriver("Oracle"),  # Use the Oracle database driver.
        username = my_username,  # Use the retrieved username.
        password = keyring::key_get("SECPR", my_username),  # Retrieve the password from the keyring.
        dbname = "SECPR"  # Specify the name of the database as "SECPR."
    )

    # Return the established database connection.
    return(con)
}

# ===

# to use on download from db
# Define a function named 'vessels_permits_id_clean' to clean a dataframe.
vessels_permits_id_clean <- function(my_df) {
    # Create a new dataframe 'vessels_permits' by renaming two specific columns.
    vessels_permits <- my_df |>
        rename("PERMIT_VESSEL_ID" = "QCSJ_C000000000300000") |>
        rename("VESSEL_VESSEL_ID" = "QCSJ_C000000000300001")

    # Return the cleaned dataframe.
    return(vessels_permits)
}

# The clean_headers function is designed to clean and fix the column names of a given dataframe (my_df).
clean_headers <- function(my_df) {
    # Use the 'fix_names' function to clean and fix the column names of the dataframe.
    colnames(my_df) %<>%
        fix_names()

    # Return the dataframe with cleaned and fixed column names.
    return(my_df)
}

# setup (in get data) ----
get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

# The source function is a built-in R function that loads and executes R code from an external file. It does not return any value; it simply executes the code in the specified file.

source("~/R_code_github/useful_functions_module.r")

# Set the working directory to the root directory "~/" and obtain its path.
my_paths <- set_work_dir()

# Define the current project name as "get_db_data."
current_project_name <- "get_db_data"

# Construct the input path by combining the 'inputs' directory from 'my_paths' with the current project name.
# The file.path function in R is used to construct file paths in a platform-independent way. It automatically takes care of the appropriate path separator (e.g., "/" on Unix-like systems or "\" on Windows).
input_path <- file.path(my_paths$inputs, current_project_name)

# err msg if no connection, but keep running
try(con <- connect_to_secpr())

# get data from db ----
# RDS (R Data Serialization) files are a common format for saving R objects in RStudio, and they allow you to preserve the state of an object between R sessions.

## logbooks as in FHIER ----
# mv_safis_trip_download

# Create a file path by combining 'input_path' with the filename "mv_safis_trip_download.rds."
file_name_mv_safis_trip_download <-
  file.path(input_path, "mv_safis_trip_download.rds")

mv_safis_trip_download_query <-
  "select * from
srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
"

# Define a function 'mv_safis_trip_download_fun' to retrieve data from the database using a specified query.
mv_safis_trip_download_fun <-
  function(mv_safis_trip_download_query) {
  # Use 'dbGetQuery' to execute the query on the database connection 'con' and return the result.
  result <- dbGetQuery(con, mv_safis_trip_download_query)

  # Return the result of the database query.
  return(result)
}

get_mv_safis_trip_download <-
  function(force_from_db) {
    # Use 'read_rds_or_run' to either read permit information from an RDS file or execute a query to obtain it.
    read_rds_or_run(file_name_mv_safis_trip_download,
                    mv_safis_trip_download_query,
                    mv_safis_trip_download_fun,
                    force_from_db)
  }

# to use alone:
# mv_safis_trip_download_data <- get_mv_safis_trip_download()
# 2023-10-19 run for mv_safis_trip_download.rds: 746.39 sec elapsed

## SEFHIER declarations as in FHIER ----
# MV_TMS_TRIP_NOTIFICATIONS

# Create a file path by combining 'input_path' with the filename "mv_tms_trip_notifications.rds."
file_name_mv_tms_trip_notifications <- file.path(input_path, "mv_tms_trip_notifications.rds")

mv_tms_trip_notifications_query <-
  "select * from
srh.mv_tms_trip_notifications@secapxdv_dblk.sfsc.noaa.gov
"

# Define a function 'mv_tms_trip_notifications_fun' to retrieve data from the database using a specified query.
mv_tms_trip_notifications_fun <- function(mv_tms_trip_notifications_query) {
  # Use 'dbGetQuery' to execute the query on the database connection 'con' and return the result.
  result <- dbGetQuery(con, mv_tms_trip_notifications_query)

  # Return the result of the database query.
  return(result)
}

get_mv_tms_trip_notifications <-
  function(force_from_db) {
    # Use 'read_rds_or_run' to either read permit information from an RDS file or execute a query to obtain it.
    read_rds_or_run(file_name_mv_tms_trip_notifications,
                    mv_tms_trip_notifications_query,
                    mv_tms_trip_notifications_fun,
                    force_from_db)
  }

# to use alone:
# mv_tms_trip_notifications_data <-
#   get_mv_tms_trip_notifications()
# 2023-10-19 run for mv_tms_trip_notifications.rds: 11.04 sec elapsed

## permit ----
# Create a file path by combining 'input_path' with the filename "permit_info.rds."
file_name_permits <- file.path(input_path, "permit_info.rds")

mv_sero_fh_permits_his_query <-
  "select * from
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
"

# Define a function 'permit_info_fun' to retrieve data from the database using a specified query.
permit_info_fun <- function(mv_sero_fh_permits_his_query) {
  # Use 'dbGetQuery' to execute the query on the database connection 'con' and return the result.
  result <- dbGetQuery(con, mv_sero_fh_permits_his_query)

  # Return the result of the database query.
  return(result)
}

get_permit_info <-
  function(force_from_db) {
    # Use 'read_rds_or_run' to either read permit information from an RDS file or execute a query to obtain it.
    read_rds_or_run(file_name_permits,
                    mv_sero_fh_permits_his_query,
                    permit_info_fun,
                    force_from_db)
  }
# 2023-09-20 run the function: 40.74 sec elapsed

### permit + vessel from db ----
# Doesn't work
# permit_vessel_query_exp21_query <-
# "select * from srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
# join safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
# ON (v.sero_official_number = p.vessel_id)
# where expiration_date > TO_DATE('01-JAN-21')
# "
# and top in ('CDW', 'CHS', 'SC')

# permit_vessel_query_exp21 <- dbGetQuery(con,
                          # permit_vessel_query_exp21_query)

# View(permit_vessel_query_exp21)

# add sa gom field

# names(permit_vessel_query_exp21) <-
  # make.unique(names(permit_vessel_query_exp21), sep = "_")

# print_df_names(permit_vessel_query_exp21)

# permit_vessel_query_exp21 %>%
  # filter(!(VESSEL_ID == SERO_OFFICIAL_NUMBER)) %>%
  # dim()
# 0

# Logbooks
# get trips info ----
trips_file_name <-
    file.path(input_path, "trips.rds")

trips_query <-
  "SELECT
  *
FROM
  safis.trips@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-21', 'dd-mon-yy') AND CURRENT_DATE
  )
ORDER BY
  trip_end_date DESC
"

trips_fun <- function(trips_query) {
  return(dbGetQuery(con,
             trips_query))
}

get_trips_info <-
  function(force_from_db) {
      read_rds_or_run(trips_file_name,
                      trips_query,
                      trips_fun,
                      force_from_db)
  }
# 2023-09-20 run the function: 33.02 sec elapsed

# grep("long", names(trips_info), ignore.case = T, value = T)
# 0

# latitude/longitude ----
# select * from safis.EFFORTS@secapxdv_dblk.sfsc.noaa.gov;

trip_coord_query <- "
  SELECT
  trip_id,
  area_code,
  sub_area_code,
  distance_code,
  fishing_hours,
  latitude,
  longitude,
  local_area_code,
  in_state,
  avg_depth_in_fathoms,
  e.de e_de,
  e.ue e_ue,
  e.dc e_dc,
  e.uc e_uc,
  anything_caught_flag,
  depth,
  minimum_bottom_depth,
  maximum_bottom_depth,
  fishing_gear_depth,
  ten_minute_square_list,
  trip_type,
  supplier_trip_id,
  days_at_sea,
  t.de t_de,
  t.ue t_ue,
  t.dc t_dc,
  t.uc t_uc,
  vessel_id,
  cf_permit_id,
  trip_start_date,
  port,
  state,
  trip_end_date,
  trip_end_time,
  trip_start_time,
  submit_method,
  activity_type,
  end_port,
  start_port,
  sero_vessel_permit,
  sea_time
FROM
       safis.efforts@secapxdv_dblk.sfsc.noaa.gov e
  JOIN safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  USING ( trip_id )
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-21', 'dd-mon-yy') AND CURRENT_DATE
  )
"

trip_coord_file_name <-
    file.path(input_path, "trip_coord.rds")

trip_coord_fun <- function(trip_coord_query) {
  return(dbGetQuery(con,
             trip_coord_query))
}

get_trip_coord_info <-
  function(force_from_db) {
      read_rds_or_run(trip_coord_file_name,
                      trip_coord_query,
                      trip_coord_fun,
                      force_from_db)
  }

# 2023-09-20 run the function: 30.94 sec elapsed

# DNF reports
# get trip neg ----

trip_neg_file_path <-
  file.path(input_path, "trip_neg.rds")

trip_neg_query <-
  "SELECT *
  FROM
    safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_date BETWEEN TO_DATE('01-JAN-21', 'dd-mon-yy') AND CURRENT_DATE
"

# 1495929

trip_neg_fun <-
  function(trip_neg_query) {
    return(dbGetQuery(con, trip_neg_query))
  }

# trip_neg_query: 201.21 sec elapsed
# trip_neg_query: 60.06 sec elapsed
# trip_neg_query: 89.38 sec elapsed

get_trip_neg <-
  function(force_from_db) {
    read_rds_or_run(trip_neg_file_path,
                    trip_neg_query,
                    trip_neg_fun,
                    force_from_db)
  }
# run the function: 98.23 sec elapsed

# Declarations
# trips_notifications ----
trips_notifications_query <-
  "SELECT
 *
FROM
  safis.trip_notifications@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-21', 'dd-mon-yy') AND CURRENT_DATE )
  OR ( trip_end_date BETWEEN TO_DATE('01-JAN-21', 'dd-mon-yy') AND CURRENT_DATE )
"

trips_notifications_file_path <-
  file.path(input_path, "trips_notifications.rds")

trips_notifications_fun <-
  function(trips_notifications_query) {
    return(dbGetQuery(con, trips_notifications_query))
  }
# trips_notifications_query: 52.08 sec elapsed
# 97279
# trips_notifications_query: 7.65 sec elapsed

get_trips_notifications <-
  function(force_from_db) {
    read_rds_or_run(
      trips_notifications_file_path,
      trips_notifications_query,
      trips_notifications_fun,
      force_from_db
    )
  }
# 2023-07-15 run the function: 13.41 sec elapsed

# get_vessels with permits 2021+ ----

dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
  AND effective_date <= CURRENT_DATE
"
# Use that "dates_filter" in all parts of the union below.

# The 3 part union is needed because while the permit table has only one vessel id, the vessel table has 3 different columns for that (sero_official_number, coast_guard_nbr and state_reg_nbr) and we want to join tables by all 3 in turn.
# stringr::str_glue is a function that allows you to create strings with placeholders for variable values. It works by using curly braces {} to enclose variable names within a string.
vessels_permits_query <-
  stringr::str_glue("SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = sero_official_number )
WHERE {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = coast_guard_nbr )
WHERE
  {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = state_reg_nbr )
WHERE
{dates_filter}
")

vessels_permits_file_path <- file.path(input_path, "vessels_permits.rds")

vessels_permits_fun <-
  function(vessels_permits_query) {
    return(dbGetQuery(con,
                      vessels_permits_query))
  }

get_vessels_permits <-
  function(force_from_db) {
    read_rds_or_run(vessels_permits_file_path,
                    vessels_permits_query,
                    vessels_permits_fun,
                    force_from_db) |>
      vessels_permits_id_clean()
  }
# 2023-09-20 run the function: 14.08 sec elapsed

# vessels_permits1 <-
#     read_rds_or_run(vessels_permits_file_path,
#                     vessels_permits_query,
#                     vessels_permits_fun,
#                     force_from_db = TRUE) |>
#       vessels_permits_id_clean()
# 2024-01-02 run for vessels_permits.rds: 32.97 sec elapsed

# an additional procedure, usually is not needed
# find 0 column ----
# get vessels
# can't because of "\\0"
# use:
# replace(VESSEL_NAME, chr(0), '') VESSEL_NAME,

#
# field_names <-
#   c("VESSEL_ID",
#     "COUNTY_CODE",
#     "STATE_CODE",
#     "ENTRY_DATE",
#     "SUPPLIER_VESSEL_ID",
#     "PORT_CODE",
#     "HULL_ID_NBR",
#     "COAST_GUARD_NBR",
#     "STATE_REG_NBR",
#     "REGISTERING_STATE",
#     # "VESSEL_NAME",
#     "PASSENGER_CAPACITY",
#     "VESSEL_TYPE",
#     "YEAR_BUILT",
#     "UPDATE_DATE",
#     "PRIMARY_GEAR",
#     "OWNER_ID",
#     "EVENT_ID",
#     "DE",
#     "UE",
#     "DC",
#     "UC",
#     "STATUS",
#     "SER_ID",
#     "UPDATED_FLAG",
#     "SERO_HOME_PORT_CITY",
#     "SERO_HOME_PORT_COUNTY",
#     "SERO_HOME_PORT_STATE",
#     "SERO_OFFICIAL_NUMBER")
#
# vessels_zero_query <-
#   "select
#   distinct {field_name}
#   from
#   safis.vessels@secapxdv_dblk.sfsc.noaa.gov"
#
# rr <-
#   purrr::map(field_names,
#     function(field_name) {
#       print(str_glue("field_name = {field_name}"))
#       q <- str_glue(vessels_zero_query)
#       tic("vessels_all1")
#       vessels_all <- dbGetQuery(con,
#                                 q)
#       toc()
#       return(dim(vessels_all))
#     }
# )
#
# # \\0 err:
# #   field_name = VESSEL_NAME
#
#
# # distinct vessel_id ok
# tic("vessels_all1")
# vessels_all <- dbGetQuery(con,
#                           vessels_zero_query)
# toc()
#
#
#
# dim(permit_info)
# dim(trip_neg)
# dim(trips_notifications)
# dim(trips_info)
# dim(vessels_all)

# dates ----
dates_query <-
  "SELECT
  dd.year,
  dd.month_of_year,
  dd.week_of_year,
  dd.complete_date
FROM
  srh.dim_dates@secapxdv_dblk.sfsc.noaa.gov dd
WHERE
  dd.complete_date BETWEEN '01-DEC-2021' AND '31-JAN-2023'
"

dates_file_path <- file.path(input_path, "dates.rds")

dates_fun <-
  function(dates_query) {
    return(dbGetQuery(con,
                      dates_query))
  }

get_dates <- function(force_from_db) {
  read_rds_or_run(dates_file_path,
                  dates_query,
                  dates_fun,
                  force_from_db)
}

# get override data ----
compl_err_query <-
    "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year > '2020'
"
# common fields
#   SRH_VESSEL_COMP_ID
# CREATED_DT
# CREATED_USER_ID
# LU_DT
# LU_USER_ID

# Define a function 'get_compl_err_data_from_db' to retrieve compliance error data from the database.
get_compl_err_data_from_db <- function(compl_err_query) {

  # Use 'ROracle::dbGetQuery' to execute the 'compl_err_query' on the database connection 'con'
  # and store the result in 'compl_err_db_data_0'.
  compl_err_db_data_0 =
    ROracle::dbGetQuery(con, compl_err_query)

  compl_err_db_data_1 <-
    compl_err_db_data_0 %>%
    # remove duplicated columns
    dplyr::select(-c(CREATED_DT,
                     CREATED_USER_ID,
                     LU_DT,
                     LU_USER_ID))

  return(compl_err_db_data_1)
}

file_name_overr <-
  file.path(input_path, "compl_err_db_data_raw.rds")

get_compl_err_db_data <-
  function(force_from_db) {
    compl_err_db_data_raw <-
      read_rds_or_run(file_name_overr,
                      compl_err_query,
                      get_compl_err_data_from_db,
                      force_from_db)
# 2023-09-20 run the function: 14.99 sec elapsed

# Clean the column names of the 'compl_err_db_data_raw' data frame using the 'clean_headers' function defined above.
  compl_err_db_data <- clean_headers(compl_err_db_data_raw)

  return(compl_err_db_data)
}

# get metric_tracking_no_srhs ----

# Use the 'source' function to execute an R script file located at the specified path.
get_metrics_tracking_path <-
  file.path(my_paths$git_r,
                 get_data_from_fhier_dir,
                 "get_metrics_tracking.R")
source(get_metrics_tracking_path)

# or source separate files instead of the flat one:
# source(file.path(my_paths$git_r,
#                 get_data_from_fhier_dir,
#                  "metric_tracking_no_srhs.R"))

# How to create a flat file:
# flat_file_name <- "make_metric_tracking_no_srhs.R"
# files_to_combine_list <-
#   c(
#     file.path(my_paths$git_r,
#                 get_data_from_fhier_dir,
#                  "get_srhs_vessels.R"),
#     file.path(my_paths$git_r,
#                 get_data_from_fhier_dir,
#                  "get_metrics_tracking.R"),
#     file.path(my_paths$git_r,
#                 get_data_from_fhier_dir,
#                  "metric_tracking_no_srhs.R")
#   )
#
# make_a_flat_file(flat_file_name,
#            files_to_combine_list)

# dim(fhier_reports_metrics_tracking_not_srhs_ids)
# 4063

# --- main ----
# Define a function 'run_all_get_db_data' to fetch data from the database and store it in a result list.
# force_from_db = NULL: read data from files if exist, change to TRUE to download again
run_all_get_db_data <-
  function(force_from_db = NULL) {
    # Initialize an empty list to store the results.
    result_l = list()

    # 1) Call the 'get_permit_info' function to retrieve permit information from the database.
    mv_sero_fh_permits_his <- get_permit_info(force_from_db)

    # 2) Store the retrieved data in the result list under the name "mv_sero_fh_permits_his."
    result_l[["mv_sero_fh_permits_his"]] <- mv_sero_fh_permits_his
    # dim(mv_sero_fh_permits_his)
    # [1] 183204     22

    # Repeat the steps 1 and 2 for all other types of data using the predefined functions.

    mv_safis_trip_download_data <- get_mv_safis_trip_download(force_from_db)
    result_l[["mv_safis_trip_download"]] <-
      mv_safis_trip_download_data
    # dim(mv_safis_trip_download_data)
    # [1] 735666    149

    mv_tms_trip_notifications_data <-
      get_mv_tms_trip_notifications(force_from_db)
    result_l[["mv_tms_trip_notifications"]] <-
      mv_tms_trip_notifications_data
    # dim(mv_tms_trip_notifications_data)
    # [1] 118730     41

    trips_info <- get_trips_info(force_from_db)
    result_l[["trips_info"]] <- trips_info
    # dim(trips_info)
    # [1] 98528    72 2022
    # [1] 142037   72 2021+

    trip_coord_info <- get_trip_coord_info(force_from_db)
    result_l[["trip_coord_info"]] <- trip_coord_info
    # dim(trip_coord_info)
    # [1] 141350     41

    trip_neg <- get_trip_neg(force_from_db)
    result_l[["trip_neg"]] <- trip_neg
    # dim(trip_neg)
    # Rows: 1,495,929
    # [1] 746087     12
    # [1] 747173     12

    trips_notifications <- get_trips_notifications(force_from_db)
    result_l[["trips_notifications"]] <-
      trips_notifications
    # dim(trips_notifications)
    # Rows: 129,701
    # [1] 70056    33

    vessels_permits <- get_vessels_permits(force_from_db)
    result_l[["vessels_permits"]] <- vessels_permits
    # dim(vessels_permits)
    # [1] 78438    51

    dates <- get_dates(force_from_db)
    result_l[["dates"]] <- dates
    # dim(dates)
    # 427 4

    compl_err_db_data <- get_compl_err_db_data(force_from_db)
    result_l[["compl_err_db_data"]] <- compl_err_db_data
    # dim(compl_err_db_data)
    # [1] 99832    38

    return(result_l)
  }

force_from_db <- NULL # read data from files if exist
# force_from_db <- "YES"

# How to use:
# Add to your code, uncomment and run:
# tic("run_all_get_db_data()")
# all_get_db_data_result_l <- run_all_get_db_data()
# toc()

# Then use like this, for example:
# View(all_get_db_data_result_l)
# mv_safis_trip_download <- all_get_db_data_result_l$mv_safis_trip_download

# Benchmark:
# reading RDS
# run_all_get_db_data(): 1.69 sec elapsed
# reading from db
# run_all_get_db_data(): 259.81 sec elapsed ~ 4 min
# run_all_get_db_data(): 606.66 sec elapsed ~ 10 min with MVs reading

# str(all_get_db_data_result_l[["compl_err_db_data"]])
# 'data.frame':	99832 obs. of  38 variables:

### check ----
# for each df print its name and dim()
# names(all_get_db_data_result_l) |>
#   purrr::map(\(df_name) {
#     c(df_name, dim(all_get_db_data_result_l[[df_name]]))
#   })

# To test pulling from the db
# force_from_db <- "NULL"
# dates <- get_dates()


# close the db connection ----
# Don't use from here, otherwise can't run the above functions from another file
# try(ROracle::dbDisconnect(con))

