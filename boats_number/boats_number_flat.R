#### add-ons 1 ----
# The tidyverse is a collection of R packages that work together seamlessly for data manipulation, visualization, and analysis. It includes popular packages like dplyr, ggplot2, tidyr, and more, all designed to follow a consistent and "tidy" data processing philosophy.
library(tidyverse)

library(zoo)
# Determine the path of the executing script
library(this.path)

# Prints an R object in markdown, needed to print pretty table from list of dfs.
library(pander)

# maps:
library(mapview)
library(sf)
library(ggmap) ## extends 'ggplot2' for creating maps and working with spatial data.
library(viridis)
library(grid)
library(gridExtra)

# Load the 'magrittr' library, which provides piping data between functions.
library(magrittr)

# Load the 'readxl' library, which is used for reading Excel files with the '.xlsx' extension.
library(readxl)

# Load the 'ROracle' library, which provides Oracle database access for R.
library(ROracle)

# Load the 'tictoc' library, which is used for timing code execution.
library(tictoc)

## Load the 'tigris' package to access geographic data.
library(tigris)

## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# Set an option in the 'dplyr' package to control the display of summarization information.
# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# help functions 1 ----
# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories ----

# Define a function named 'get_current_file_directory',
# to obtain the directory where the script is located.
get_current_file_directory <- function() {

  # Use 'rstudioapi::getSourceEditorContext()' to access information about the currently open script
  # Extract the 'path' from the source editor context and obtain its directory using 'dirname'
  dirname(rstudioapi::getSourceEditorContext()$path)
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
# This function sets the working directory to the Anna's home directory, defines paths to 'my_inputs,' 'my_outputs,' and 'R_code_github' directories, and returns these directory paths as a list. The use of file.path ensures that the path construction is platform-independent.

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

#### add-ons 2 ----


my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

#### Current file: misc_info.R ----

# states lists ----
south_east_coast_states <- c(
  "Alabama",
  "Florida",
  "Georgia",
  "Louisiana",
  "Mississippi",
  "North Carolina",
  "South Carolina",
  "Texas"
)

south_atlantic_states <-
  c(
    "Maryland",
    "Delaware",
    "West Virginia",
    "Virginia",
    "North Carolina",
    "South Carolina",
    "Georgia",
    "Florida"
  )

east_coast_states <- list(
  gom = c("Alabama",
          "Florida",
          "Louisiana",
          "Mississippi",
          "Texas"),
  sa = c(
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Maine",
    "Maryland",
    "Massachusetts",
    "New Hampshire",
    "New Jersey",
    "New York",
    "North Carolina",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "Virginia",
    "Washington DC"
  )
)

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

sa_council_states <-
  c(
    "Florida", # should be separated by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )

# Florida counties by region (from the Internet) ----
# NB. "Monroe" is in both regions
fl_counties <- list(
  "sa" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Monroe",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "gom" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  ),
  "gom_interior" = c(
    "Alachua",
    "Clay",
    "Glades",
    "Hendry",
    "Lake",
    "Marion",
    "Orange",
    "Polk",
    "Putnam",
    "Seminole",
    "Suwannee",
    "Unknown"
  )
)

# prepare state names and abbs ----
# have to save first, to use the original once as names
my_state_abb <- state.abb
my_state_name <- state.name
names(my_state_abb) <- tolower(state.name)
names(my_state_name) <- tolower(state.abb)

# result names
result_names <- c(
  "south_east_coast_states",
  "east_coast_states",
  "sa_council_states",
  "south_atlantic_states",
  "fl_counties",
  "my_state_abb",
  "my_state_name"
)

# to print the title message in blue.
title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg), sep = "\n")
}

title_message_print(result_names)

#### Current file: get_metrics_tracking.R ----

## fhier_reports_metrics_tracking ----

# help functions (in metric tracking) ----
# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

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

# Download from FHIER / Reports / Metrics Tracking
# Put dates in, e.g. 01/01/2022 - 12/31/2022
# Click search
# Under "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)	" section below click "Actions", then "Download"

fhier_reports_metrics_tracking_file_names <-
  c("Detail_Report_12312021_12312022__08_23_2023.csv",
    "Detail_Report_12312022_12312023__08_23_2023.csv")

common_dir <-
  file.path(my_paths$inputs,
  r"(from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source))")

# save all file names to a list
# Create a vector named 'fhier_reports_metrics_tracking_file_path' using the purrr::map function.
# This vector will store file paths based on the 'fhier_reports_metrics_tracking_file_names' vector.
fhier_reports_metrics_tracking_file_path <-
  purrr::map(
    # Iterate over each element in the 'fhier_reports_metrics_tracking_file_names' vector.
    fhier_reports_metrics_tracking_file_names,
    # For each file name ('x'), create a file path by combining it with 'common_dir'.
    ~ file.path(common_dir, .x)
  )

# test
# Use the purrr::map function to check if files exist at the specified paths.
# The result will be a logical vector indicating file existence for each path.
purrr::map(fhier_reports_metrics_tracking_file_path, file.exists)
# T

# read each csv in a list of dfs
# Use the purrr::map function to read multiple CSV files into a list of data frames.
fhier_reports_metrics_tracking_list <- purrr::map(
  fhier_reports_metrics_tracking_file_path,
  # A vector of file paths to CSV files.
  ~ readr::read_csv(
    # The current file path being processed in the iteration.
    .x,
    # Specify column types; here, all columns are read as characters.
    col_types = cols(.default = 'c'),
    name_repair = fix_names  # Automatically repair column names to be syntactically valid.
  )
)

names(fhier_reports_metrics_tracking_list) <-
  c("2022", "2023")

# check how many in diff years ----
# Use the 'dplyr::setdiff' function to find the set difference between two vectors.
# (1 minus 2)
dplyr::setdiff(
  fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
  fhier_reports_metrics_tracking_list[[2]]$vessel_official_number
) |>
  length()  # Calculate the length of the resulting set difference.
# [1] 669

# (2 minus 1)
dplyr::setdiff(
  fhier_reports_metrics_tracking_list[[2]]$vessel_official_number,
  fhier_reports_metrics_tracking_list[[1]]$vessel_official_number
) |>
  length()
# [1] 493

# in both years
# Use the 'dplyr::intersect' function to find the intersection of two vectors.
# In this case, we're finding the common unique values between the two vectors.
dplyr::intersect(
  fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
  fhier_reports_metrics_tracking_list[[2]]$vessel_official_number
) |>
  length()  # Calculate the length of the resulting intersection.
# 2965

#### Current file: get_db_data.R ----

# setup (in get data) ----
get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

# Define the current project name as "get_db_data."
get_db_data_project_name <- "get_db_data"

# Construct the input path by combining the 'inputs' directory from 'my_paths' with the current project name.
# The file.path function in R is used to construct file paths in a platform-independent way. It automatically takes care of the appropriate path separator (e.g., "/" on Unix-like systems or "\" on Windows).
input_path <- file.path(my_paths$inputs, get_db_data_project_name)

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

# err msg if no connection, but keep running
try(con <- connect_to_secpr())

# Get data ----
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

# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

# The read_rds_or_run function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run <- function(my_file_path,
                            my_data = as.data.frame(""),
                            my_function,
                            force_from_db = NULL) {

  if (file.exists(my_file_path)) {
    modif_time <- file.info(my_file_path)$mtime
  }

    # Check if the file specified by 'my_file_path' exists and 'force_from_db' is not set.
    if (file.exists(my_file_path) &
        is.null(force_from_db)) {
        # If the file exists and 'force_from_db' is not set, read the data from the RDS file.

        function_message_print("File already exists, reading.")

        my_result <- readr::read_rds(my_file_path)

    } else {

      # If the file doesn't exist or 'force_from_db' is set, perform the following steps:

      # 0. Print this message.
      function_message_print(c(
        "File",
        my_file_path,
        "doesn't exists, pulling data from database.",
        "Must be on VPN."
      ))

      # 1. Generate a message indicating the date and the purpose of the run for "tic".
      msg_text <-
        paste(today(), "run for", basename(my_file_path))
      tictoc::tic(msg_text)  # Start timing the operation.

      # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result. I.e. download data from the Oracle database. Must be on VPN.

      my_result <- my_function(my_data)

      tictoc::toc()  # Stop timing the operation.

      # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
      # try is a wrapper to run an expression that might fail and allow the user's code to handle error-recovery.

      # 4. Print this message.
      function_message_print(c("Saving new data into a file here: ",
                       my_file_path))

      try(readr::write_rds(my_result,
                           my_file_path))

      modif_time <- date()
    }

  my_file_name <- basename(my_file_path)
  function_message_print(
    str_glue("File: {my_file_name} modified {modif_time}"))

    # Return the generated or read data.
    return(my_result)
}

get_vessels_permits <-
  function() {
    read_rds_or_run(vessels_permits_file_path,
                    vessels_permits_query,
                    vessels_permits_fun) |>
      vessels_permits_id_clean()
  }

#### Current file: boats_number_get_data.R ----

# Prepare all_logbooks_db_data_2022_short_p_region
# 1) download all db data
# 2) a) use processed logbooks
#    b) or "all logbooks = mv_safis_trip_download
# 3) Filter 2022 only
# 4) Remove unused columns
# 5) Don't do this until further notice: Mark sa_only vs. gom and dual for 2022 using vessel list from Jeanette’s comparison

rm_columns <- c("ACTIVITY_TYPE",
"ANYTHING_CAUGHT_FLAG",
"APP_VERSION",
"APPROVAL_DATE",
"APPROVED_BY",
"AVG_DEPTH_IN_FATHOMS",
"CAPT_NAME_FIRST",
"CAPT_NAME_LAST",
"CATCH_DC",
"CATCH_DE",
"CATCH_SEQ",
"CATCH_SOURCE_NAME",
"CATCH_SOURCE",
"CATCH_SPECIES_ITIS",
"CATCH_UE",
"COMMON_NAME",
"CONFIRMATION_SIGNATURE",
"DC",
"DE",
"DEA_PERMIT_ID",
"DEPTH",
"DISPOSITION_CODE",
"DISPOSITION_NAME",
"EFFORT_SEQ",
"EFFORT_TARGET_COMMON_NAMES",
"EFFORT_TARGET_SPECIES_LIST",
"EVENT_ID",
"FISHING_GEAR_DEPTH",
"FISHING_HOURS",
"FORM_VERSION",
"FUEL_DIESEL_GALLON_PRICE",
"FUEL_DIESEL_GALLONS",
"FUEL_GALLON_PRICE",
"FUEL_GALLONS",
"FUEL_GAS_GALLON_PRICE",
"FUEL_GAS_GALLONS",
"GEAR_CATEGORY_CODE",
"GEAR_CATEGORY_NAME",
"GEAR_CODE",
"GEAR_DESC",
"GEAR_NAME",
"GEAR_QUANTITY",
"GEAR_SIZE",
"GEAR_TYPE_CODE",
"GEAR_TYPE_NAME",
"GEARS_FISHING",
"GRADE_CODE",
"GRADE_NAME",
"HOOKS_PER_LINE",
"HOURS_DAYS_FLAG",
"IN_STATE",
"LANDING_SEQ",
"LMA_CODE",
"MARKET_CATEGORY_CODE",
"MARKET_CATEGORY_NAME",
"MAXIMUM_BOTTOM_DEPTH",
"MESH_RING_LENGTH",
"MESH_RING_WIDTH",
"MINIMUM_BOTTOM_DEPTH",
"NBR_OF_CREW",
"NBR_PAYING_PASSENGERS",
"NUM_ANGLERS",
"REPORTED_QUANTITY",
"REPORTING_SOURCE",
"RIG_CODE",
"SPECIES_ITIS",
"SPLIT_TRIP",
"STRETCH_SIZE",
"SUB_TRIP_TYPE",
"SUBMIT_METHOD",
"SUBMITTED_BY_CORPORATE_NAME",
"SUBMITTED_BY_FIRST_NAME",
"SUBMITTED_BY_LAST_NAME",
"SUBMITTED_BY_MIDDEL_NAME",
"SUBMITTED_BY_PARTICIPANT",
"SUPPLIER_EFFCAT_ID",
"SUPPLIER_TRIP_ID",
"TICKET_TYPE",
"TRANSMISSION_DATE",
"TRIP_END_TIME",
"TRIP_FEE",
"TRIP_NBR",
"TRIP_START_TIME",
"TRIP_TYPE",
"UC",
"UE",
"UNIT_MEASURE")

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

vessels_permits_from_db <- get_vessels_permits()

# get logbooks 2022 only ----
## logbooks data----

# use all logbooks from https://drive.google.com/drive/folders/1HipnxawNsDjrsMc4dXgFwdRPQ3X6-x3n
processed_logb_path <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs\SEFHIER_usable_logbooks_2022.rds)")

# file.exists(processed_logb_path)

processed_logbooks <-
  read_rds(processed_logb_path)

processed_logbooks_clean_names <-
  clean_headers(processed_logbooks)

## input data names ----
input_data_df_names <-
  c("vessels_permits_from_db",
    "processed_logbooks_clean_names")

title_message_print(input_data_df_names)

#### Current file: waters_shape_prep.R ----
# setup ----

# Get the current project directory name using the 'this.path' package.
waters_project_dir_name <- this.path::this.dir()

waters_project_basename <-
  basename(waters_project_dir_name)

waters_output_path <- file.path(my_paths$outputs,
                         waters_project_basename)

my_crs = 4326

# shape files ----

## GOM state and fed ----
GOM_400fm_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOM_400fm\GOM_400fm.shp)")

# file.exists(GOM_400fm_path)
# T

GOMsf <-
  sf::read_sf(GOM_400fm_path)
# mapview(GOMsf)

## SA federal waters ----
sa_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# file.exists(sa_path)

sa_shp <- sf::read_sf(sa_path)

# mapview(sa_shp)

## world waters ----
world_state_and_fed_waters_path <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

# file.exists(state_and_fed_waters_path)

world_state_and_fed_waters_shp <-
  sf::read_sf(world_state_and_fed_waters_path)

# mapview(state_and_fed_waters_shp)

## SA state waters ----

east_coast_sa_state_waters_shp <-
  world_state_and_fed_waters_shp |>
  filter(Jurisdicti %in% east_coast_states$sa)

# mapview(east_coast_sa_state_waters_shp)

# SA state and fed waters ---

## Florida state waters ----
# bc FL is in both regions

### fl_state_w_counties ----
fl_state_w_counties_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

# file.exists(fl_state_w_counties_path)

fl_state_w_counties_shp <-
  sf::read_sf(fl_state_w_counties_path)

# mapview(fl_state_w_counties_shp)

### fl sa ----
sa_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$sa)

# mapview(sa_fl_state_w_counties_shp)

### fl gom ----

gom_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$gom)

## GOM South Florida state_waters_only ----
GOM_s_fl_state_waters_only <-
  GOMsf |>
  filter(Jurisdict == "State" &
           StatZone %in% c(1, 2, 3)) |>
  select(-c(DepZone,
            Activity,
            Shape_Area)) |>
  distinct()

# uncomment to see
# mapview(fl_state_w_counties_shp,
#         col.regions = "green") +
#   mapview(GOM_s_fl_state_waters_only)

# When getting points in SA Monroe
# Have to do it by steps, otherwise it takes too long
# 1) get all points not in GOM_s_fl_state_waters_only

## all US states ----
## The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_state_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

sa_states_shp <-
  us_state_shp |>
  filter(NAME %in% south_atlantic_states)

gom_states_shp <-
  us_state_shp |>
  filter(NAME %in% east_coast_states$gom)

## Convert to common crs ----
st_crs(east_coast_sa_state_waters_shp)
    # ID["EPSG",3395]]

st_crs(gom_fl_state_w_counties_shp)
    # ID["EPSG",4269]]

st_crs(GOMsf)
    # ID["EPSG",4326]]

st_crs(sa_shp)
    # ID["EPSG",4269]]

st_crs(us_state_shp)
    # ID["EPSG",4269]]

my_dfs_to_transform_names <-
  c(
    "east_coast_sa_state_waters_shp",
    "gom_fl_state_w_counties_shp",
    "sa_fl_state_w_counties_shp",
    "sa_shp",
    "gom_states_shp",
    "sa_states_shp"
  )

my_dfs_to_transform <-
  list(east_coast_sa_state_waters_shp,
       gom_fl_state_w_counties_shp,
       sa_fl_state_w_counties_shp,
       sa_shp,
       gom_states_shp,
       sa_states_shp
       )

tic("shp_4326_list")
shp_4326_list <-
  lapply(my_dfs_to_transform,
         function(x) st_transform(x, my_crs))
toc()
# shp_4326_list: 14.56 sec elapsed

names(shp_4326_list) <- my_dfs_to_transform_names

# Results ----
result_names <- c("GOMsf",
             "world_state_and_fed_waters_path",
             "fl_state_w_counties_shp",
             "GOM_s_fl_state_waters_only",
             "shp_4326_list: ",
             my_dfs_to_transform_names)
title_message_print(result_names)

#### Current file: boats_number.R ----

#' %%%%% Prepare data
#'
#' Questions:
#' How many SEFHIER vessels have a different start port county than end port county?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port state than end port state?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)?
#' Numbers, by quarter (1-4):
#' How many Gulf permitted SEFHIER vessels fish in both the Gulf and South Atlantic?
#'   Numbers, by quarter (1-4):
#' For counties and states
#' GOM:
#' permit,
#' home_port,
#' end_port
#' retain Monroe
#'
#' For home port region to SA region:
#' GOM permit,
#' GOM home_port
#' exclude Monroe

#' For fishing region to region
#' GOM permit
#' retain Monroe
#' Create 2 dfs fished in GOM or in SA using lat and lon for area fished
#' grouping by vessel ID and quarter, check if unique vessel fishing in GOM and in SA

# changes
# "2024-01-04"
# new date filter, add interior FL counties

## processed_logbooks ----

# to use in lists
start_end_words <-
  c("start", "end")

port_fields_short <-
  c(
    "vessel_official_number",
    "end_port_name",
    "end_port_state",
    "end_port_county",
    "end_port",
    "permit_region",
    "start_port_name",
    "start_port_state",
    "start_port_county",
    "start_port",
    "trip_id",
    "trip_end_date",
    "trip_start_date",
    "latitude",
    "longitude"
  )

# Explanation:
# 1. The pipe operator (`|>`) is used to pass the data frame 'processed_logbooks_clean_names' to the next operation, making the code more readable.
# 2. The 'dplyr::select' function is employed to choose specific columns from the data frame. The columns to be selected are determined by the vector 'port_fields_short', which contains column names.
# 3. The 'remove_empty_cols()' function is a custom function (F2 - see the definition) that removes columns containing only missing values (NA).
# 4. The next pipe (`|>`) continues the data flow, passing the modified data frame to the 'dplyr::distinct()' function.
# 5. The 'dplyr::distinct()' function ensures that only unique rows are retained in the data frame, removing any duplicate rows based on all columns.
#
# In summary, this code snippet processes a data frame by selecting specific columns, removing empty columns, and keeping only unique rows. The result is stored in the 'processed_logbooks_short' data frame.

# Function to remove empty columns from a data frame
remove_empty_cols <- function(my_df) {
  # Define an inner function "not_all_na" that checks if any value in a vector is not NA.
  not_all_na <- function(x) any(!is.na(x))

  my_df |>
    # Select columns from "my_df" where the result of the "not_all_na" function is true,
    # i.e., select columns that have at least one non-NA value.
    select(where(not_all_na)) %>%
    # Return the modified data frame, which contains only the selected columns.
    return()
}

processed_logbooks_short <-
  processed_logbooks_clean_names |>
  dplyr::select(dplyr::all_of(port_fields_short)) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short)
# [1] 73368    15 with trip_id

n_distinct(processed_logbooks_clean_names$vessel_official_number)
# 1629

processed_logbooks_short |>
  select(vessel_official_number, permit_region) |>
  distinct() |>
  count(permit_region)
#   permit_region   n
# 1           GOM 754
# 2            SA 875

### add date related columns ----

# Explanation:
# 1. The 'processed_logbooks_short' data frame is piped into the 'dplyr::mutate' function, which is used to create new columns based on transformations.
# 2. The 'trip_start_week_num' and 'trip_end_week_num' columns are created using the 'strftime' function to extract the week number (1-7, Monday-Sunday) from the 'trip_start_date' and 'trip_end_date'.
# 3. 'trip_start_y' and 'trip_end_y' columns are created, representing the year of 'trip_start_date' and 'trip_end_date', respectively, using the 'lubridate::year' function.
# 4. 'trip_start_m' and 'trip_end_m' columns are created, representing the year and month (as a decimal) of 'trip_start_date' and 'trip_end_date' using the 'zoo::as.yearmon' function.
# 5. 'trip_start_year_quarter' and 'trip_end_year_quarter' columns are created, representing the year and quarter of 'trip_start_date' and 'trip_end_date' using the 'zoo::as.yearqtr' function.
# 6. 'trip_start_quarter_num' and 'trip_end_quarter_num' columns are created, representing the quarter number (1-4) of 'trip_start_date' and 'trip_end_date' using the 'format' function and the '%q' format specifier.

tic("processed_logbooks_short_dates")
processed_logbooks_short_dates <-
  processed_logbooks_short |>
  dplyr::mutate(
    trip_start_week_num =
      strftime(trip_start_date, format = "%u"),
    trip_end_week_num =
      strftime(trip_end_date, format = "%u"),
    trip_start_y =
      lubridate::year(trip_start_date),
    trip_end_y =
      lubridate::year(trip_end_date),
    trip_start_m =
      zoo::as.yearmon(trip_start_date),
    trip_end_m =
      zoo::as.yearmon(trip_end_date),
    trip_start_year_quarter = zoo::as.yearqtr(trip_start_date),
    trip_start_quarter_num =
      format(trip_start_year_quarter, "%q"),
    trip_end_year_quarter = zoo::as.yearqtr(trip_end_date),
    trip_end_quarter_num =
      format(trip_end_year_quarter, "%q")
  )
toc()
# processed_logbooks_short_dates: 4.28 sec elapsed

## Prepare home_port data ----

vessel_permit_port_info <-
  vessels_permits_from_db |>
  # active permits in 2022
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  )

dim(vessel_permit_port_info)
# [1] 79813    51

### remove unused columns ----
vessel_permit_port_info_short <-
  vessel_permit_port_info |>
  select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    # PORT_CODE, mostly empty
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER
  ) |>
  dplyr::ungroup() |>
  remove_empty_cols() |>
  dplyr::distinct()

n_distinct(vessel_permit_port_info_short$VESSEL_VESSEL_ID)
# 6168

vessel_permit_port_info_short_clean <-
  clean_headers(vessel_permit_port_info_short)

## add vessel_permit home port information to trip (logbook) information ----

# Explanation:
# 1. The 'dplyr::left_join' function is applied to combine two data frames, namely 'processed_logbooks_short_dates' and 'vessel_permit_port_info_short_clean', using a left join.
# 2. The third argument of the 'left_join' function, 'dplyr::join_by', specifies the columns used for the join operation. In this case, it indicates that the column 'vessel_official_number' from the left data frame should be matched with the column 'sero_official_number' from the right data frame.
# 3. The result of the join is stored in the 'join_trip_and_vessel' data frame, which includes columns from both input data frames, combining information about trips and vessel permits. The left join ensures that all rows from 'processed_logbooks_short_dates' are retained, and matching rows from 'vessel_permit_port_info_short_clean' are added based on the specified join condition.

join_trip_and_vessel <-
  dplyr::left_join(
    processed_logbooks_short_dates,
    vessel_permit_port_info_short_clean,
    dplyr::join_by(vessel_official_number ==
                     sero_official_number)
  )

dim(join_trip_and_vessel)
# [1] 73368    30 with trip_id

## remove trailing spaces ----

# Explanation:
# 1. The 'join_trip_and_vessel' data frame is piped into the 'mutate_if' function, which allows selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'str_trim' function to all character columns of the 'join_trip_and_vessel' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the trimming operation is only performed on columns containing character data.
# 4. The result is stored in the 'join_trip_and_vessel_trim' data frame, where leading and trailing whitespaces are removed from all character columns. This operation helps clean and standardize the textual data in those columns.

join_trip_and_vessel_trim <-
  join_trip_and_vessel |>
  mutate_if(is.character, str_trim)

diffdf::diffdf(join_trip_and_vessel,
               join_trip_and_vessel_trim)

## lower case of all data ----

# Explanation:
# 1. The 'join_trip_and_vessel_trim' data frame is piped into the 'mutate_if' function, which allows selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'tolower' function to all character columns of the 'join_trip_and_vessel_trim' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the conversion to lowercase is only performed on columns containing character data.
# 4. The result is stored in the 'join_trip_and_vessel_low' data frame, where all character columns are converted to lowercase. This operation helps standardize the case of text data for consistency and ease of analysis.
join_trip_and_vessel_low <-
  join_trip_and_vessel_trim |>
  mutate_if(is.character, tolower)

## remove not a-z in strings ----
# (e.g. "St. John" or "miami dade" vs. "miami-dade")

# Explanation:
# 1. The 'join_trip_and_vessel_low' data frame is piped into the 'mutate_if' function, allowing selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'str_replace_all' function to all character columns of the 'join_trip_and_vessel_low' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the replacement is only performed on columns containing character data.
# 4. The replacement pattern "[^a-z0-9]+" in 'str_replace_all' uses a regular expression to match any non-alphanumeric characters and replaces them with a single space.
# 5. The result is stored in the 'join_trip_and_vessel_clean' data frame, where non-alphanumeric characters in character columns are replaced with spaces. This operation helps clean and standardize the textual data for further analysis.
join_trip_and_vessel_clean <-
  join_trip_and_vessel_low |>
  mutate_if(is.character,
         ~str_replace_all(., "[^a-z0-9]+", " "))

# diffdf::diffdf(join_trip_and_vessel_low,
#                join_trip_and_vessel_clean)

# An aux function to add regions to states ----

# join_trip_and_vessel_clean |>
#   select(start_port_state) |>
#   distinct()

# Explanation:
# 1. The function takes a data frame (`my_df`) and a specifier (`start_or_end`) indicating whether to consider the start or end of a trip.
# 2. Dynamic column names are created using the 'str_glue' function for the result column and columns related to state and county based on the 'start_or_end' parameter.
# 3. Quosures (symbolic expressions) are created to check conditions related to Florida state, Gulf of Mexico region, and Gulf of Mexico counties in Florida.
# 4. The function then uses 'rowwise' and 'mutate' functions to add a new column to the data frame ('new_df'). The new column values are determined based on conditional logic using the 'case_when' function.
# 5. The conditions check if the state is not Florida and is in the Gulf of Mexico region, or if the state is Florida and the county is in the Gulf of Mexico region. The default case sets the result column to "sa" (South Atlantic) if the state is not Florida or the county is not in the Gulf of Mexico region.
# 6. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 7. The modified data frame is returned.
add_region_to_state <-
  function(my_df, start_or_end) {
    result_column_name <-
      str_glue("{start_or_end}_state_region")

    port_state_column <-
      sym(str_glue("{start_or_end}_port_state"))

    port_county_column <-
      sym(str_glue("{start_or_end}_port_county"))

    is_st_florida <-
      rlang::quo(!!port_state_column == "fl")

    is_gom_state <-
      rlang::quo(my_state_name[[!!port_state_column]]
                 %in% east_coast_states$gom)

    is_gom_fl_county <-
      rlang::quo(
        !!port_county_column %in% tolower(fl_counties$gom) |
          !!port_county_column %in% tolower(fl_counties$gom_interior)
      )

    new_df <-
      my_df |>
      rowwise() |>
      mutate(
        !!result_column_name :=
          case_when(
            !(!!is_st_florida) & !!is_gom_state ~ "gom",

            !!is_st_florida & !!is_gom_fl_county ~ "gom",

            is.na(!!port_state_column) ~ NA,

            .default = "sa"
          )
      ) |>
      ungroup()

    return(new_df)
  }

n_distinct(join_trip_and_vessel_clean$vessel_official_number)
# 1629

# TODO: Why there is no home port?
join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(contains("port")) |>
  distinct() |>
  glimpse()
# 1

no_home_port_vessels <-
  join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(vessel_official_number) |>
  distinct()

n_distinct(no_home_port_vessels$vessel_official_number)
# 1 after changing the date filter

vessels_permits_from_db |>
  filter(SERO_OFFICIAL_NUMBER %in% no_home_port_vessels$vessel_official_number) |>
  select(SERO_OFFICIAL_NUMBER,
         END_DATE,
         EXPIRATION_DATE,
         LAST_EXPIRATION_DATE,
         SERO_HOME_PORT_STATE) |> distinct() |>
  arrange(SERO_OFFICIAL_NUMBER,
          END_DATE,
          EXPIRATION_DATE,
          LAST_EXPIRATION_DATE) |>
  group_by(SERO_OFFICIAL_NUMBER) |>
  count(name = 'date_by_vsl') |>
  ungroup() |>
  nrow()
# 0

n_distinct(join_trip_and_vessel_clean$vessel_official_number)
# 1629

# Add port state regions ----
# Don't use a start port state instead of filter(!is.na(sero_home_port_state)) for consistency.

n_distinct(join_trip_and_vessel_clean$vessel_official_number)
# 1629

# Explanation:
# 1. The 'join_trip_and_vessel_clean' data frame is piped into the 'filter' function to exclude rows where the 'sero_home_port_state' column is NA.
# 2. The 'add_region_to_state' function is applied twice using the pipe operator ('|>') to add region information for both the start and end of the trip.
# 3. The 'add_region_to_state("sero_home")' line adds region information for the start of the trip using the 'sero_home' prefix.
# 4. The 'add_region_to_state("end")' line adds region information for the end of the trip using the 'end' prefix.
# 5. The result is stored in the 'join_trip_and_vessel_clean_state_regions' data frame, which now includes additional columns indicating the regions for both the start and end of the trip.
# 6. The 'tic' and 'toc' functions are used to measure the time taken for the entire operation between the two calls. Timing is enclosed in the 'join_trip_and_vessel_clean_state_regions' operation to measure its execution time.

tic("join_trip_and_vessel_clean_state_regions")
join_trip_and_vessel_clean_state_regions <-
  join_trip_and_vessel_clean |>
  filter(!is.na(sero_home_port_state)) |>
  add_region_to_state("sero_home") |>
  add_region_to_state("end")
toc()
# join_trip_and_vessel_clean_state_regions: 37.61 sec elapsed

# Split by home port regions ----

# Explanation:
# 1. The 'join_trip_and_vessel_clean_state_regions' data frame is piped into the 'split' function.
# 2. The 'split' function is applied to create a list of data frames. The splitting is based on the levels of the 'sero_home_state_region' column.
# 3. The 'as.factor' function is used to ensure that the column is treated as a factor, and it provides the levels for splitting.
# 4. The result is stored in the 'join_trip_and_vessel_clean_state_regions_l' variable, which is a list of data frames. Each data frame in the list corresponds to a unique level of the 'sero_home_state_region' column.

join_trip_and_vessel_clean_state_regions_l <-
  join_trip_and_vessel_clean_state_regions |>
  split(as.factor(
    join_trip_and_vessel_clean_state_regions$sero_home_state_region
  ))

# Define a function 'count_uniq_by_column' to count the number of unique values in each column of a data frame.

# Within the function, the sapply function is used to apply another function to each column of the input data frame. Specifically, it counts the number of unique values in each column using the length(unique(x)) expression, where x represents each column of the data frame.
# The result of sapply is a vector containing the counts of unique values for each column.

# It returns the resulting data frame, which provides a summary of the counts of unique values for each column in the input data frame. This information can be valuable for assessing the diversity of values within each column.

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>%  # Apply a function to each column to count unique values.
    as.data.frame()  # Convert the result to a data frame.
}

# check
map(join_trip_and_vessel_clean_state_regions_l,
    count_uniq_by_column)

# $gom
# [1] 973
#
# $sa
# [1] 655

dim(join_trip_and_vessel_clean_state_regions_l$gom)
# [1] 54257    32

## Shorten GOM df ----
columns_to_keep <- c(
  "vessel_official_number",
  "trip_id",
  "sero_home_port_county",
  "sero_home_port_state",
  "end_port_county",
  "end_port_state"
)

short_port_gom <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(all_of(columns_to_keep),
         contains("region"),
         contains("quarter")) |>
  # can use distinct, because we are not interested in the number of such trips, just the number of vessel
  distinct()

#' %%%%% Boat movement numbers for GOM
#'

# How many GOM SEFHIER vessels have a different start port county than end port county? ----

## different counties ----

# Explanation:
# 1. The 'short_port_gom' data frame is piped into the 'group_by' function to group the data based on specified columns: 'vessel_official_number', 'trip_id', 'permit_region', and 'trip_end_year_quarter'.
# 2. The 'filter' function is applied to exclude rows where 'sero_home_port_county' is equal to 'end_port_county', effectively keeping rows where the start and end port counties are different.
# 3. The 'ungroup' function is used to remove the grouping introduced by 'group_by', ensuring that subsequent operations are applied to the entire data frame.
# 4. The 'select' function is used to exclude the 'trip_id' column from the resulting data frame.
# 5. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns. The resulting data frame is stored in the 'start_end_county_diff_gom' variable.

start_end_county_diff_gom <-
  short_port_gom |>
  group_by(vessel_official_number,
           trip_id,
           permit_region,
           trip_end_year_quarter) |>
  filter(!sero_home_port_county == end_port_county) |>
  ungroup() |>
  select(-trip_id) |>
  distinct()

### check different start and end quarters ----
join_trip_and_vessel_clean |>
  select(trip_start_year_quarter, trip_end_year_quarter) |>
  distinct() |>
  filter(!trip_start_year_quarter == trip_end_year_quarter)
  # trip_start_year_quarter trip_end_year_quarter
# 1                 2022 Q1               2022 Q2
# 2                 2022 Q2               2022 Q3

start_end_county_diff_gom |>
  filter(vessel_official_number %in% c("al4295ak", "1270320")) |>
  glimpse()
# $ vessel_official_number  <chr> "1270320", "al4295ak", "al4295ak", "al4295…
# $ sero_home_port_county   <chr> "collier", "mobile", "mobile", "mobile"
# $ sero_home_port_state    <chr> "fl", "al", "al", "al"
# $ end_port_county         <chr> "st bernard", "baldwin", "sarasota", "sara…
# $ end_port_state          <chr> "la", "al", "fl", "fl"

## count different counties ----

# Explanation:
# 1. The 'start_end_county_diff_gom' data frame is piped into the 'add_count' function.
# 2. The 'add_count' function is applied to count the occurrences of each unique combination of values in the specified columns: 'permit_region', 'sero_home_port_county', 'end_port_county', and 'trip_end_year_quarter'.
# 3. The result is stored in a new column named 'cnt_diff_county', which represents the count of occurrences for each unique combination of the specified columns.
# 4. The resulting data frame is stored in the 'start_end_county_diff_gom_num' variable, which now includes the count information in the 'cnt_diff_county' column.

start_end_county_diff_gom_num <-
  start_end_county_diff_gom |>
  add_count(
    permit_region,
    sero_home_port_county,
    end_port_county,
    trip_end_year_quarter,
    name = "cnt_diff_county"
  )

dim(start_end_county_diff_gom_num)
# [1] 321  13

### spot check counts ----
join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 2

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  head()
#   vessel_official_number trip_end_year_quarter
# 1                 958876               2022 Q1
# 2                 958876               2022 Q2
# 3               fl5321kd               2022 Q2
# 4               fl5321kd               2022 Q3

# vessels by quarter, correct
start_end_county_diff_gom_num |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  arrange(trip_end_quarter_num) |>
  glimpse()
# $ trip_end_quarter_num    <chr> "1", "2", "2", "3"
# $ cnt_diff_county         <int> 1, 2, 2, 1

## Result table for GOM permit region (and the state region) ----
# GOM for Metrics tracking permit region
start_end_county_diff_gom_num_gom_permit_only <-
  start_end_county_diff_gom_num |>
  filter(permit_region == "gom")

# check
dim(start_end_county_diff_gom_num_gom_permit_only)
# [1] 286  13 2024-01-04

start_end_county_diff_gom_num |>
dim()
# [1] 321  13 2024-01-04

#### make the result table ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only' data frame is piped into the 'select' function to exclude the 'permit_region' column.
# 2. The 'rowwise' function is applied to ensure that subsequent mutations are applied row-wise.
# 3. Two 'mutate' functions are applied to create new columns 'home_port_state' and 'end_port_state' by looking up state names based on their abbreviations.
# 4. Two additional 'mutate' functions are applied to create new columns 'home_port_county' and 'end_port_county' by converting the port county names to title case.
# 5. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 6. The 'select' function is applied to exclude several columns that are no longer needed for the final result.
# 7. The 'relocate' function is used to reorder columns for better readability, placing important columns at the beginning.
# 8. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 9. The resulting data frame is stored in the 'start_end_county_diff_gom_num_gom_permit_only_res' variable.

start_end_county_diff_gom_num_gom_permit_only_res <-
  start_end_county_diff_gom_num_gom_permit_only |>
  select(-permit_region) |>
  rowwise() |>
  mutate(home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  mutate(
    home_port_county = str_to_title(sero_home_port_county),
    end_port_county = str_to_title(end_port_county)
  ) |>
  ungroup() |>
  select(
    -c(
      sero_home_port_county,
      sero_home_port_state,
      trip_start_year_quarter,
      trip_start_quarter_num,
      trip_end_quarter_num,
      vessel_official_number
    )
  ) |>
  relocate(
    trip_end_year_quarter,
    home_port_state,
    home_port_county,
    end_port_state,
    end_port_county,
    diff_county_num_of_vessels = cnt_diff_county
  ) |>
  distinct()
# Can use distinct here, because we only look at county to county vessels, not trips

# spot check
start_end_county_diff_gom_num_gom_permit_only_res |>
    filter(home_port_county == "Pasco" &
           end_port_county == "Pinellas" &
           trip_end_year_quarter == "2022 Q1" ) |>
  glimpse()

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "pasco" &
           end_port_county == "pinellas" &
           trip_end_year_quarter == "2022 Q1" &
           permit_region == "gom") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # arrange(vessel_official_number) |>
  count(vessel_official_number)
#   vessel_official_number  n
# 1                1070441 11
# 2               fl2045nb 59
# 2 vessels, ok

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    # home_port_county == "Brazoria" &
    #   end_port_county == "Galveston" &
      trip_end_year_quarter == "2022 Q4"
  ) |>
  count(wt = diff_county_num_of_vessels)
# 2 for Brazoria - Galveston
# 50 2024-01-04

join_trip_and_vessel_clean |>
  filter(
    sero_home_port_county == "brazoria" &
      end_port_county == "galveston" &
      trip_end_year_quarter == "2022 Q4" &
      permit_region == "gom"
  ) |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # distinct()
# 970060
# tx2118fj
  count(vessel_official_number) |>
  count()
# 2, correct

join_trip_and_vessel_trim |>
  filter(grepl("monroe", sero_home_port_county, ignore.case = T)) |>
  filter(grepl("baldwin", end_port_county, ignore.case = T)) |>
  select(
    vessel_official_number,
    sero_home_port_county,
    sero_home_port_state,
    end_port_county,
    end_port_state,
    # trip_end_quarter_num,
    trip_end_year_quarter
  ) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  glimpse()
# Q2 2
# Q3 1

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    home_port_county == "Monroe" &
      end_port_county == "Baldwin"
  ) |>
  count(trip_end_year_quarter,
        wt = diff_county_num_of_vessels)
# 1 2022 Q2                   2
# 2 2022 Q3                   1
# ok

##### Write out ----

write_csv(
  start_end_county_diff_gom_num_gom_permit_only_res,
  file.path(
    curr_proj_output_path,
    "start_end_county_diff_gom_num_gom_permit_only_res.csv"
  )
)

#### Diff county numbers, by quarter (1-4) ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only_res' data frame is piped into the 'group_by' function, grouping the data by 'trip_end_year_quarter'.
# 2. The 'group_by' function is applied to group the data by the 'trip_end_year_quarter' column.
# 3. The 'count' function is applied to calculate the total count of 'diff_county_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_county_num_of_vessels_tot'.
# 4. The resulting data frame is stored in the 'start_end_county_diff_gom_num_gom_permit_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_county_num_of_vessels' column.

start_end_county_diff_gom_num_gom_permit_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_county_num_of_vessels_tot")

## Result for diff counties ----
head(start_end_county_diff_gom_num_gom_permit_only_res_quarter)
#   trip_end_year_quarter diff_county_num_of_vessels_tot
# 1 2022 Q1                                           25
# 2 2022 Q2                                          109
# 3 2022 Q3                                          102
# 4 2022 Q4                                           50

# How many SEFHIER vessels have a different start port state than end port state? ----

## different start and end states in one trip ----

# Explanation:
# 1. The 'short_port_gom' data frame is piped into the 'filter' function to include only rows where 'permit_region' is "gom".
# 2. The 'filter' function is applied to include only rows where 'permit_region' is "gom".
# 3. The 'select' function is applied to keep only selected columns: 'vessel_official_number', 'sero_home_port_state', 'end_port_state', and 'trip_end_year_quarter'.
# 4. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 5. Another 'filter' function is applied to exclude rows where 'sero_home_port_state' is equal to 'end_port_state', effectively keeping rows where the start and end states are different.
# 6. The 'count' function is applied to calculate the total count of vessels for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'. The result is stored in a new column named 'diff_states_num_of_vessels'.
# 7. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res' variable, which now includes the total count of vessels for each quarter based on the 'diff_states_num_of_vessels' column.

# All functions are from dplyr

start_end_state_diff_num_gom_only_res <-
  short_port_gom |>
  filter(permit_region == "gom") |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state,
    trip_end_year_quarter
  ) |>
  distinct() |>
  filter(!sero_home_port_state == end_port_state) |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        end_port_state,
        name = "diff_states_num_of_vessels")

### spot check ----

start_end_state_diff_num_gom_only_res |>
  filter(trip_end_year_quarter == "2022 Q1") |>
  glimpse()

start_end_state_diff_num_gom_only_res |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_year_quarter == "2022 Q2") |>
  glimpse()
# 2022 Q2
# Florida
# Alabama
# 5

join_trip_and_vessel_clean |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_quarter_num == 2) |>
  select(vessel_official_number,
         trip_end_year_quarter,
         sero_home_port_state,
         end_port_state) |>
  distinct() |>
  nrow()
# 5
# OK

## Diff states numbers, by quarter (1-4) ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'group_by' function, grouping the data by 'trip_end_year_quarter'.
# 2. The 'group_by' function is applied to group the data by the 'trip_end_year_quarter' column.
# 3. The 'count' function is applied to calculate the total count of 'diff_states_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_states_num_of_vessels_tot'.
# 4. The 'ungroup' function is used to remove the grouping introduced by 'group_by', ensuring that subsequent operations are applied to the entire data frame.
# 5. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_states_num_of_vessels_tot' column.

# All functions are from dplyr

start_end_state_diff_num_gom_only_res_quarter <-
  start_end_state_diff_num_gom_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_tot") |>
  ungroup()

head(start_end_state_diff_num_gom_only_res_quarter)
#   trip_end_year_quarter diff_states_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                            3
# 2 2022 Q2                                           31
# 3 2022 Q3                                           27
# 4 2022 Q4                                           10

### spot check ----
# 2022 Q4
join_trip_and_vessel_clean_state_regions |>
  filter(sero_home_state_region == "gom") |>
  filter(permit_region == "gom") |>
  filter(trip_end_quarter_num == 4) |>
  filter(!sero_home_port_state == end_port_state) |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state
  ) |>
  distinct() |>
  arrange(sero_home_port_state,
          end_port_state) |>
  # View()
  count(sero_home_port_state,
        end_port_state) |>
  glimpse()
# ok
# $ sero_home_port_state <chr> "al", "fl", "fl", "fl", "fl", "ms", "tx"
# $ end_port_state       <chr> "fl", "al", "la", "nc", "tx", "la", "la"
# $ n                    <int> 1, 1, 3, 1, 2, 1, 1

## save results to csv ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'rowwise' function to ensure that subsequent mutations are applied row-wise.
# 2. Two 'mutate' functions are applied to create new columns 'sero_home_port_state' and 'end_port_state' by looking up state names based on their abbreviations using the 'my_state_name' list.
# 3. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 4. The 'write_csv' function is applied to write the data frame to a CSV file. The 'file.path' function is used to create the file path by combining the 'curr_proj_output_path' (current project output path) and the desired filename "start_end_state_diff_num_gom_only_res.csv".
# 5. The resulting CSV file is saved in the specified output path for further use or analysis.

start_end_state_diff_num_gom_only_res |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  ungroup() |>
  write_csv(file.path(curr_proj_output_path,
                      "start_end_state_diff_num_gom_only_res.csv"))


# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'count' function to calculate counts for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'.
# 2. The 'count' function is applied with the 'wt' parameter set to 'diff_states_num_of_vessels' for weighted counting. The resulting counts are stored in a new column named 'states_cnt'.
# 3. Two 'mutate' functions are applied to convert the state names in the 'sero_home_port_state' and 'end_port_state' columns to uppercase. This ensures consistency in the representation of state names.
# 4. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_cnts_by_home' variable, which includes counts for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'.
start_end_state_diff_num_gom_only_res_cnts_by_home <-
  start_end_state_diff_num_gom_only_res |>
  count(
    trip_end_year_quarter,
    sero_home_port_state,
    end_port_state,
    wt = diff_states_num_of_vessels,
    name = "states_cnt"
  ) |>
  mutate(
    sero_home_port_state = toupper(sero_home_port_state),
    end_port_state = toupper(end_port_state)
  )

# ---
# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res_cnts_by_home' data frame is piped into the 'add_count' function to calculate the total count for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.
# 2. The 'add_count' function is applied with the 'wt' parameter set to 'states_cnt' for weighted counting. The resulting total counts are stored in a new column named 'sum_by_q_and_home'.
# 3. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_cnts_by_home_sum' variable, which includes the total counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.

start_end_state_diff_num_gom_only_res_cnts_by_home_sum <-
  start_end_state_diff_num_gom_only_res_cnts_by_home |>
  add_count(trip_end_year_quarter,
            sero_home_port_state,
            wt = states_cnt,
            name = "sum_by_q_and_home")

# Write to csv
start_end_state_diff_num_gom_only_res_cnts_by_home_sum |>
  write_csv(
    file.path(
      curr_proj_output_path,
      "start_end_state_diff_num_gom_only_res_cnts_by_home_sum.csv"
    )
  )

## State to state by state and quarter res table ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'count' function to calculate counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.
# 2. The 'count' function is applied with the 'wt' parameter set to 'diff_states_num_of_vessels' for weighted counting. The resulting counts are stored in a new column named 'diff_states_num_of_vessels_home'.
# 3. The 'rowwise' function is applied to ensure that subsequent mutations are applied row-wise.
# 4. The 'mutate' function is applied to create a new column 'sero_home_port_state' by looking up state names based on their abbreviations using the 'my_state_name' list.
# 5. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 6. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_home' variable, which now includes counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state', with the state names updated for better readability.

start_end_state_diff_num_gom_only_res_home <-
  start_end_state_diff_num_gom_only_res |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_home") |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  ungroup()

# Write to a file
write_csv(
  start_end_state_diff_num_gom_only_res_home,
  file.path(
    curr_proj_output_path,
    "start_end_state_diff_num_gom_only_res_home.csv"
  )
)

# How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)? ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only_res' data frame is piped into the 'select' function to keep only selected columns.
# 2. The 'select' function is applied to keep only selected columns: 'trip_end_year_quarter', 'sero_home_state_region', 'end_state_region', and 'diff_county_num_of_vessels'.
# 3. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 4. Another 'filter' function is applied to exclude rows where 'sero_home_state_region' is equal to 'end_state_region', effectively keeping rows where the start and end state regions are different.
# 5. The 'group_by' function is applied to group the data by 'trip_end_year_quarter'.
# 6. The 'count' function is applied to calculate the total count of 'diff_county_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_port_regions_num_of_vessels_tot'.
# 7. The resulting data frame is stored in the 'start_end_state_region_diff_num_gom_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_port_regions_num_of_vessels_tot' column.

# All functions are from dplyr

start_end_state_region_diff_num_gom_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  select(
    trip_end_year_quarter,
    sero_home_state_region,
    end_state_region,
    diff_county_num_of_vessels
  ) |>
  distinct() |>
  filter(!sero_home_state_region == end_state_region) |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_port_regions_num_of_vessels_tot")

## result for state region to region ----
# 3 in the whole year (for GOM home port, and GOM permit)
#   trip_end_year_quarter diff_port_regions_num_of_vessels_tot
#   <yearqtr> <int>
# 1 2022 Q1   1
# 2 2022 Q2   1
# 3 2022 Q4   1

#' Nothing to show, only 1 vessel has trips starting in Fl and ending in North Carolina in Q2 and 1 vessel in Q4.
#' Plus 1 vessel in Q1 from Sarasota, Florida to Duval, Florida

# Quantify the # of vessels who fish in both the gulf and S Atl ----
# Notes:
# GOM permit
# retain Monroe
# Create 2 dfs fished in GOM or in SA using lat and lon for area fished
# grouping by vessel ID and quarter, check if unique vessel fishing in GOM and in SA

## prep fishing locations ----

### Get GOM permitted vessels with Lat and Long ----
# Explanation:
# 1. The 'join_trip_and_vessel_clean_state_regions_l$gom' data frame, representing the GOM (Gulf of Mexico) region from the joined data, is piped into the 'filter' function to include only rows where 'permit_region' is "gom".
# 2. The 'filter' function is applied to include only rows where 'permit_region' is "gom".
# 3. The 'select' function is applied to keep only selected columns: 'vessel_official_number', 'latitude', 'longitude', and 'trip_end_year_quarter'.
# 4. Another 'filter' function is applied to exclude rows where 'latitude' or 'longitude' is NA.
# 5. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 6. The resulting data frame is stored in the 'lat_lon_gom_state' variable, which includes information about the latitude, longitude, and vessel details for the GOM region.
lat_lon_gom_state <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  filter(permit_region == "gom") |>
  select(vessel_official_number,
         latitude,
         longitude,
         trip_end_year_quarter) |>
  filter(!is.na(latitude) &
           !is.na(longitude)) |>
  distinct()

### Count points ----

# Explanation:
# 1. The 'lat_lon_gom_state' data frame is piped into the 'mutate' function to modify the 'latitude' and 'longitude' columns by taking their absolute values. This is done to handle potential discrepancies in latitude and longitude data.
# 2. The 'mutate' function is applied to modify the 'latitude' and 'longitude' columns by taking their absolute values. And then converting all longitude to negative values.
# 3. The 'add_count' function is applied to calculate the total count of vessels for each unique combination of 'latitude' and 'longitude'. The result is stored in a new column named 'cnt_v_coords_by_y'.
# 4. Another 'add_count' function is applied to calculate the total count of vessels for each unique combination of 'latitude', 'longitude', and 'trip_end_year_quarter'. The result is stored in a new column named 'cnt_v_coords_by_q'.
# 5. The resulting data frame is stored in the 'lat_lon_gom_state_cnt' variable, which includes counts for each unique combination of coordinates and quarter based on the 'cnt_v_coords_by_y' and 'cnt_v_coords_by_q' columns.

lat_lon_gom_state_cnt <-
  lat_lon_gom_state |>
  mutate(latitude = abs(latitude),
         longitude = -abs(longitude)) |>
  add_count(latitude, longitude,
            name = "cnt_v_coords_by_y") |>
  add_count(latitude,
            longitude,
            trip_end_year_quarter,
            name = "cnt_v_coords_by_q")

## Define a common crs ----
my_crs <- 4326

# Create a new object 'lat_lon_gom_state_cnt_sf' by piping the data frame
# 'lat_lon_gom_state_cnt' into the st_as_sf function from the sf package

# The st_as_sf function is used to convert a data frame with latitude and
# longitude columns into an sf (simple feature) object
  # Specify the latitude and longitude columns for the sf object
    # Set the coordinate reference system (CRS) for the sf object
    # Keep the original columns in the resulting sf object

lat_lon_gom_state_cnt_sf <-
  lat_lon_gom_state_cnt |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = my_crs,
    remove = FALSE
  )

dim(lat_lon_gom_state_cnt_sf)
# [1] 36545     7

## List of loaded shapefiles ----
# GOMsf
# world_state_and_fed_waters_path
# fl_state_w_counties_shp
# GOM_s_fl_state_waters_only
# shp_4326_list:
# east_coast_sa_state_waters_shp
# gom_fl_state_w_counties_shp
# sa_fl_state_w_counties_shp
# sa_shp
# gom_states_shp
# sa_states_shp

## Split by region using shape files ----

### Aux function ----
# Explanation:
# 1. The 'intersect_waters_and_points' function is defined to take two spatial data frames, 'my_shp' (shapefile) and 'my_points'.
# 2. Inside the function, the 'st_intersection' function from the 'sf' package is applied to calculate the intersection of the two spatial data frames. This function identifies the common geometries between the shapefile and the points.
# 3. The resulting spatial data frame representing the intersection is stored in the 'intersect_result' variable.
# 4. The function returns the 'intersect_result', which contains the geometries that are common to both 'my_shp' and 'my_points'.
intersect_waters_and_points <-
  function(my_shp,
           my_points) {
    intersect_result <-
      st_intersection(my_shp,
                      my_points)
    return(intersect_result)
  }

### fishing in GOM  ----
# Read a file or run the function
gom_lat_lon_gom_state_cnt_sf_fed_w_file_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "gom_lat_lon_gom_state_cnt_sf_fed_w.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

read_rds_or_run_no_db <-
  function(my_file_path,
           my_data_list_of_dfs,
           my_function) {
    # browser()

    if (file.exists(my_file_path)) {
      # read a binary file saved previously
      my_df <-
        readr::read_rds(my_file_path)
    } else {
      tic("run the function")
      my_df <-
        my_function(my_data_list_of_dfs[[1]],
                    my_data_list_of_dfs[[2]])
      toc()

      # write all as binary
      readr::write_rds(my_df,
                       my_file_path)
    }

    return(my_df)
  }

gom_lat_lon_gom_state_cnt_sf_fed_w <-
  read_rds_or_run_no_db(
    gom_lat_lon_gom_state_cnt_sf_fed_w_file_path,
    list(GOMsf,
         lat_lon_gom_state_cnt_sf),
    intersect_waters_and_points
  )
# run the function: 56.91 sec elapsed

# mapview(gom_lat_lon_gom_state_cnt_sf_fed_w)

### Fishing in SA ----
#### Federal waters ----

# Read a file or run the function for SA fed

sa_fed_waters_points_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "sa_fed_waters_points.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

sa_fed_waters_points <-
  read_rds_or_run_no_db(
    sa_fed_waters_points_path,
    list(shp_4326_list$sa_shp,
         lat_lon_gom_state_cnt_sf),
    intersect_waters_and_points
  )
# run the function: 43.64 sec elapsed

# mapview(sa_fed_waters_points)

### fewer points by SA fed waters, to speed up ----
sa_bb <- st_bbox(shp_4326_list$sa_shp)
sa_bb_points <- st_crop(lat_lon_gom_state_cnt_sf, sa_bb)

#### state waters, Monroe in both regions ----
# mapview(east_coast_sa_state_waters_shp)

# Read a file or run the function to subset points by SA state waters
sa_state_waters_points_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "sa_all_state_waters_points.rds")

# file.exists(sa_state_waters_points_path)

sa_state_waters_points <-
  read_rds_or_run_no_db(
    sa_state_waters_points_path,
    list(shp_4326_list$east_coast_sa_state_waters_shp,
         sa_bb_points),
    intersect_waters_and_points
  )
# run the function: 701.09 sec elapsed

# mapview(sa_state_waters_points)

#### Remove GOM Monroe points from all state waters ----
# Fewer fields
gom_lat_lon_gom_state_cnt_sf_fed_w_short <-
  gom_lat_lon_gom_state_cnt_sf_fed_w |>
  select(-c(StatZone,
            DepZone,
            Jurisdict,
            Activity,
            Shape_Area))

dim(gom_lat_lon_gom_state_cnt_sf_fed_w_short)
# [1] 31081     7

sa_state_waters_points_short <-
  sa_state_waters_points |>
  select(
    vessel_official_number,
    latitude,
    longitude,
    trip_end_year_quarter,
    cnt_v_coords_by_y,
    cnt_v_coords_by_q,
    geometry
  )

dim(sa_state_waters_points_short)
# [1] 6144    7

# Convert back to dfs
sa_state_waters_points_short_df <-
  st_drop_geometry(sa_state_waters_points_short)

gom_lat_lon_gom_state_cnt_sf_fed_w_short_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w_short)

# Keep only SA state water points which are not in GOM
sa_state_waters_points_short_df_no_gom <-
  anti_join(sa_state_waters_points_short_df,
            gom_lat_lon_gom_state_cnt_sf_fed_w_short_df)

dim(sa_state_waters_points_short_df_no_gom)
# 560 6

# Remove not sa counties ---

# Get the county column
logbooks_w_county <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(sero_home_port_county,
    any_of(names(sa_state_waters_points_short_df_no_gom))) |>
  distinct()

dim(logbooks_w_county)
# [1] 46966     5 2024-01-04

# add the county column to the cropped df
sa_state_waters_points_short_df_no_gom_counties <-
  sa_state_waters_points_short_df_no_gom |>
  left_join(logbooks_w_county)
# Joining with `by = join_by(vessel_official_number, latitude, longitude,
# trip_end_year_quarter)`

# The 'filter' function is applied to include only rows where 'sero_home_port_county' is in the list of lowercased counties corresponding to the South Atlantic (SA) region in Florida.
sa_state_waters_points_short_df_no_gom_counties_sa <-
  sa_state_waters_points_short_df_no_gom_counties |>
  filter(sero_home_port_county %in% tolower(fl_counties$sa))

dim(sa_state_waters_points_short_df_no_gom_counties_sa)
# [1] 544   7
# 560 - 544 = 16 points removed

# Check counties
# sa_state_waters_points_short_df_no_gom_counties_sa |>
#   select(sero_home_port_county) |>
#   distinct()
# monroe

# mapview(sa_state_waters_points_short_df_no_gom_counties_sa,
#         xcol = "longitude",
#         ycol = "latitude",
#         crs = my_crs)

## Join all points with regions by vessel ----
### back to dfs for join ----
gom_lat_lon_gom_state_cnt_fed_w_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w)

sa_lat_lon_gom_state_cnt_sf_fed_w_df <-
  st_drop_geometry(sa_fed_waters_points)

### join point data frames ----
# get common names
keep_sa_fields <-
  intersect(
    names(sa_lat_lon_gom_state_cnt_sf_fed_w_df),
    names(sa_state_waters_points_short_df_no_gom_counties_sa)
  )

# Explanations:
#
# 1. **List Creation:**
#    - The `list` function creates a list containing two data frames: `sa_lat_lon_gom_state_cnt_sf_fed_w_df` and `sa_state_waters_points_short_df_no_gom_counties`.
#
# 2. **Mapping and Combining Data Frames:**
#    - The `map_df` function is applied to the list of data frames.
#    - The lambda function with a `curr_df` parameter selects specific columns from each data frame.
#    - The results are combined into a single data frame.
#
# 3. **Column Selection:**
#    - The pipe operator (`|>`) passes each data frame to the next operation.
#    - The `select` function is used to subset the columns of each data frame based on the column names specified in `keep_sa_fields`.
#
# 4. **Final Result:**
#    - The variable `all_points_sa` holds the resulting data frame with columns selected from the original data frames.

all_points_sa <-
  list(sa_lat_lon_gom_state_cnt_sf_fed_w_df,
       sa_state_waters_points_short_df_no_gom_counties) |>
  map_df(\(curr_df) {
    curr_df |>
      select(all_of(keep_sa_fields))
  })

dim(all_points_sa)
# [1] 1013    6

# Keep the same columns for gom
gom_lat_lon_gom_state_cnt_fed_w_df_short <-
  gom_lat_lon_gom_state_cnt_fed_w_df |>
  select(all_of(keep_sa_fields))

# join gom and sa points
all_fish_points <-
  full_join(
    gom_lat_lon_gom_state_cnt_fed_w_df_short,
    all_points_sa,
    join_by(vessel_official_number,
            trip_end_year_quarter),
    relationship = "many-to-many",
    suffix = c(".gom", ".sa")
  )

## add markers for having gom or sa fishing locations ----

# 1. **Grouping by Vessel Official Number:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points' to the next operation.
#    - The `group_by` function is used to group the data frame by the 'vessel_official_number' column.
#
# 2. **Adding New Columns with Mutate:**
#    - The `mutate` function adds two new columns, 'has_gom_point_y' and 'has_sa_point_y', to the data frame.
#    - The 'has_gom_point_y' column is determined using the `any` function to check if any values in the 'latitude.gom' column are not NA.
#    - The 'has_sa_point_y' column is determined using the `any` function to check if any values in the 'latitude.sa' column are not NA.
#
# 3. **Ungrouping:**
#    - The `ungroup` function removes the grouping structure from the data frame, returning it to an ungrouped state.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_y' holds the modified data frame with added columns indicating the presence of non-NA values in specific latitude columns for each vessel.

all_fish_points_reg_y <-
  all_fish_points |>
  group_by(vessel_official_number) |>
  mutate(has_gom_point_y =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_y =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

# Keep vessels, having both SA and GOM points
all_fish_points_reg_both_y <-
  all_fish_points_reg_y |>
  filter(has_gom_point_y & has_sa_point_y)

dim(all_fish_points_reg_both_y)
# [1] 4479   12 gom permit only

# vessel_official_number   76 all permits
n_distinct(all_fish_points_reg_both_y$vessel_official_number)
# 30

### same by quarter ----
all_fish_points_reg_q <-
  all_fish_points |>
  group_by(vessel_official_number,
           trip_end_year_quarter) |>
  mutate(has_gom_point_q =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_q =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

all_fish_points_reg_both_q <-
  all_fish_points_reg_q |>
  filter(has_gom_point_q & has_sa_point_q)

dim(all_fish_points_reg_both_q)
# [1] 3940   12 gom permit only

n_distinct(all_fish_points_reg_both_q$vessel_official_number)
# [1] 30

# Count vessels fishing in both regions by quarter ---
# Explanations
# 1. **Column Selection:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points_reg_both_q' to the next operation.
#    - The `select` function is used to subset the data frame to include only the columns 'trip_end_year_quarter' and 'vessel_official_number'.
#
# 2. **Distinct Rows:**
#    - The `distinct` function returns unique combinations of the specified columns, effectively removing duplicate rows.
#
# 3. **Counting Rows:**
#    - The `count` function is used to count the number of rows for each unique combination of 'trip_end_year_quarter'.
#    - The result is the count of unique combinations of 'trip_end_year_quarter', providing information about the number of vessels in each quarter.
#
# 4. **Final Result:**
#    - The output of the entire sequence is not assigned to a variable, but it represents the count of unique combinations of 'trip_end_year_quarter' in the original data frame 'all_fish_points_reg_both_q'.

all_fish_points_reg_both_q |>
  select(trip_end_year_quarter, vessel_official_number) |>
  distinct() |>
  count(trip_end_year_quarter)
# no SA permit
#   trip_end_year_quarter     n
# 1 2022 Q1                  13
# 2 2022 Q2                  14
# 3 2022 Q3                  15
# 4 2022 Q4                  13

## map all_fish_points_reg_both_q ----

# Explanations
#
# 1. **Conversion to Simple Feature (sf) Data Frame:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points_reg_both_q' to the next operation.
#    - The `st_as_sf` function converts the data frame to a simple feature (sf) data frame, using the columns 'longitude.gom' and 'latitude.gom' as coordinates and setting the coordinate reference system (crs) to 'my_crs'. The 'remove' parameter is set to FALSE, preserving the original data frame.
#
# 2. **Column Subsetting:**
#    - The `select` function is used to subset the columns of the resulting sf data frame, excluding those ending with ".sa".
#
# 3. **Column Renaming:**
#    - The `rename` function is used to rename the 'geometry' column to 'geometry_gom'.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_both_q_gom_sf' holds the modified sf data frame with coordinates from the Gulf of Mexico region, excluding columns related to South Atlantic coordinates and with the 'geometry' column renamed to 'geometry_gom'.

all_fish_points_reg_both_q_gom_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.gom", "latitude.gom"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".sa")) |>
  rename(geometry_gom = geometry)

# The same for SA
all_fish_points_reg_both_q_sa_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.sa", "latitude.sa"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".gom")) |>
  rename(geometry_sa = geometry)

# Create the map
all_sa_gom_map <-
  mapview(all_fish_points_reg_both_q_sa_sf,
          col.regions = "blue") +
  mapview(all_fish_points_reg_both_q_gom_sf,
          col.regions = "green")

# Uncomment to see the map
# all_sa_gom_map

### Fishing in SA and GOM map by quarter ----

# Explanations:
#
# 1. **List Creation:**
#    - The `list` function is used to create a list containing two sf data frames: `all_fish_points_reg_both_q_sa_sf` and `all_fish_points_reg_both_q_gom_sf`.
#
# 2. **Mapping and Splitting Data Frames:**
#    - The `map` function applies the following lambda function to each element of the list.
#    - For each sf data frame (`curr_df`), the data is split into a list of data frames (`list_by_reg_q`) based on the 'trip_end_year_quarter' column.
#
# 3. **Return of List of Data Frames:**
#    - The lambda function returns the list of data frames (`list_by_reg_q`) created by the `split` operation.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_both_q_sf_quarters' holds a list of data frames, each representing a subset of the original sf data frames based on the 'trip_end_year_quarter' column. The list is created by splitting the data frames for both South Atlantic and Gulf of Mexico regions.

all_fish_points_reg_both_q_sf_quaters <-
  list(all_fish_points_reg_both_q_sa_sf,
       all_fish_points_reg_both_q_gom_sf) |>
  map(\(curr_df) {
    list_by_reg_q <-
      curr_df |>
      split(as.factor(curr_df$trip_end_year_quarter))

    return(list_by_reg_q)
  })

# Add names
names(all_fish_points_reg_both_q_sf_quaters) <-
  c("sa",
    "gom")

# List of all quarters
all_quarters_list <-
  names(all_fish_points_reg_both_q_sf_quaters$sa)

# uncomment to see an example for the 1 quarter
# mapview(all_fish_points_reg_both_q_sf_quaters$sa$`2022 Q1`,
        # col.regions = "green") +
# mapview(all_fish_points_reg_both_q_sf_quaters$gom$`2022 Q1`,
        # col.regions = "blue")

# Explanations
#
# 1. **Mapping and Creating Views:**
#    - The pipe operator (`|>`) passes the 'all_quarters_list' to the next operation.
#    - The `map` function applies the following lambda function to each element of the list (`curr_quarter`).
#
# 2. **MapView for South Atlantic and Gulf of Mexico:**
#    - Inside the lambda function:
#      - The `mapview` function is used to create map views for both the South Atlantic and Gulf of Mexico regions.
#      - Data frames for each region and quarter are accessed from the list 'all_fish_points_reg_both_q_sf_quarters'.
#      - For South Atlantic, 'col.regions' is set to "green", and for Gulf of Mexico, it's set to "blue".
#
# 3. **Final Result:**
#    - The variable 'all_maps_by_q' holds a list of map views, each representing data for a specific quarter. The maps include points from both the South Atlantic and Gulf of Mexico regions, with distinct colors for each region.
all_maps_by_q <-
  all_quarters_list |>
  map(\(curr_quarter) {
    mapview(all_fish_points_reg_both_q_sf_quaters$sa[[curr_quarter]],
            col.regions = "green") +
      mapview(all_fish_points_reg_both_q_sf_quaters$gom[[curr_quarter]],
              col.regions = "blue")
  })

names(all_maps_by_q) <- all_quarters_list

# same in plots ----
all_plots_by_q <-
  all_quarters_list |>
  map(\(curr_quarter) {
    my_title = curr_quarter
  ggplot() +
  geom_sf(data =
            all_fish_points_reg_both_q_sf_quaters$sa[[curr_quarter]],
          aes(
            geometry = geometry_sa,
            # fill = q_factors,
            colour = "sa"
          )) +
  geom_sf(data =
            all_fish_points_reg_both_q_sf_quaters$gom[[curr_quarter]],
          aes(
            geometry = geometry_gom,
            # fill = q_factors,
            colour = "gom"
          )) +
  geom_sf(data = sa_states_shp, fill = NA) +
  geom_sf(data = gom_states_shp, fill = NA) +
        labs(title = my_title) +
    theme_bw()
  })

# all_plots_by_q[[1]]

# combine plots
super_title <-
  "Vessels with GOM or dual permits fishing in both regions in 2022"

plots_by_q_arranged <-
  grid.arrange(grobs = all_plots_by_q,
             top = super_title,
             # left = my_legend,
             ncol = 2)

plots_by_q_arranged_dir <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits")

ggsave(
  file = "plots_by_q_arranged.png",
  plot = plots_by_q_arranged,
  device = "png",
  path = plots_by_q_arranged_dir,
  width = 30,
  height = 20,
  units = "cm"
)
