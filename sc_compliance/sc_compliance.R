# This script fulfills a monthly request from SCDNR for compliance data from FHIER.

# Essentially SC (Eric Hiltz) sends us a list of active SC/SEFHIER permitted vessels each month, which Anna has been using to update the FHIER flag ("SC permitted") in the FHIER website. Eric will still be sending us that same list of vessels each month but with added columns that now reflect whether the vessel is compliant or not.
# In addition to using the list to update FHIER, we will use this code to do two things:

# 1) Vessels that are non-compliant in SC and compliant in FHIER.
# Read in this file of SC/SEFHIER vessel IDs for the given month (e.g. March 2024) and then pull out all vessels marked as "1" (NON-COMPLIANT) - herein, "SC non-compliant vessels list". Then with that SC non-compliant vessel list, this code pulls all those vessels that are COMPLIANT in the FHIER compliance table and creates an output file that consists of a check that we will send back to Eric.
# Add logbooks and dnfs for FHIER compliant weeks for the SC non-compliant month.

# 2) Vessels that are compliant in SC and non-compliant in FHIER.
# Grab the compliant vessels (herein "SC compliant vessels list"), and then check FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

# So, in the output file we have: (sheet 1) the list of those SC NON-COMPLIANT vessels that are COMPLIANT in FHIER,
# with (on sheets 2-3) the list of all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month).
# (sheet 4) is the list of those SC COMPLIANT vessels that are NON-COMPLIANT in FHIER.


# Needed files (6):
#   1) "scdnrFedVessels_04012024.xlsx" (South Carolina compliance, instead of "04012024" there will be the date of the latest file)
          # this file comes from Eric Hilts at SCDNR
#   2) "Raw_Oracle_Downloaded_compliance_2021_plus.rds"
          # this file is downloaded from the Oracle db or read in below
#   3) "SEFHIER_processed_dnfs_{my_year}.rds"
          # this file comes from running the DNF processing code for a given year, it should be available to download on Google Drive
#   4) "SEFHIER_processed_Logbooks_{my_year}.rds"
          # this file comes from running the logbook processing code for a given year, it should be available to download on Google Drive
#   5) "Vessel_List_{my_year}.csv" (Southeast Region Headboat Survey, SRHS)
          # this file comes from Ken Brennan at the SRHS program, it should be available to download on Google Drive
#   6) “column_definitions.csv” (csv file of column definitions written by Michelle, used for the ReadMe tab of the output Excel spreadsheet, it should be available to download on Google Drive)

# set up ----
# Load the 'ROracle' library, which provides an interface for working with Oracle databases in R.
library(ROracle)

# Load the 'tidyverse' library, which is a collection of R packages for data manipulation and visualization.
library(tidyverse)

# Load the 'magrittr' library, which provides piping data and functions.
library(magrittr)

# Load the 'tictoc' library, which allows for measuring code execution time.
library(tictoc)

# Load the 'openxlsx' library, used for reading and writing Excel (.xlsx) files.
library(openxlsx)

# assign dates to variables,
my_year <- "2024" # the year of the analysis
db_year_1 <- "2023" # set the range based on how many years of data you want to pull, ex. the year before the year of the analysis
db_year_2 <- "2024" # set the range based on how many years of data you want to pull, ex. the year after the year of the analysis

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

# set the path to processed logbook data on Anna’s computer
annas_processed_data_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)"

# set the current project directory name to the directory that has this R script in it
current_project_dir_name <- this.path::this.dir()

# set the current project base name to the name of the directory that has this R script in it
current_project_basename <-
  basename(current_project_dir_name)

# set the path to SC vessels data on Anna’s computer
annas_sc_mismatch_file_path <-
  file.path(annas_path$inputs,
            r"(sc_mismatches\2024_04)",
            "scdnrFedVessels_04012024.xlsx")

# check that the file exists
file.exists(annas_sc_mismatch_file_path)

# set the path to SRHS data on Anna’s computer
annas_srhs_2024_file_path <-
  file.path(annas_path$inputs,
            "SRHS_headboat_survey",
            str_glue("Vessel_List_{my_year}.csv"))

# check that the file exists
file.exists(annas_srhs_2024_file_path)

## Add correct paths for your environment in the next 5 lines ----

input_path <- file.path(annas_path$inputs, current_project_basename)
output_path <- file.path(annas_path$outputs, current_project_basename)
processed_data_path <- annas_processed_data_path
sc_file_path <- annas_sc_mismatch_file_path
srhs_2024_file_path <- annas_srhs_2024_file_path

# get data ----
## Import and prep compliance/override data ----

### Import compliance/override data ----
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(processed_data_path,
            "Raw_Oracle_Downloaded_compliance_2021_plus.rds")

# Check if the file path is correct, optional
file.exists(compl_override_data_file_path)

# 2) Create a variable with a table name to call data from, define year.
# >= 2021 because of when the program started or between 2 years defined above
compl_err_query <-
  str_glue(
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '{db_year_1}' AND comp_year <= '{db_year_2}'")

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

# The read_rds_or_run function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
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
        "doesn't exist, pulling data from database.",
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
      # try is a wrapper to run an expression that might fail and allow the user's code to handle error recovery.

      # 4. Print this message.
      function_message_print(c("Saving new data into a file here: ",
                       my_file_path))

      try(readr::write_rds(my_result,
                           my_file_path))

      modif_time <- date()
    }

  # Print out the formatted string with the file name ('my_file_name') and the modification time ('modif_time') to keep track of when the data were downloaded.
  my_file_name <- basename(my_file_path)
  function_message_print(
    str_glue("File: {my_file_name} modified {modif_time}"))

    # Return the generated or read data.
    return(my_result)
}

# Run the function
compl_override_data <-
  read_rds_or_run(compl_override_data_file_path,
                  my_function = compl_err_query)
# File: Raw_Oracle_Downloaded_compliance_2021_plus.rds modified 2024-04-02 12:18:54.299425

### prep the compliance/override data ----

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

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

# The clean_headers function is designed to clean and fix the column names of a given dataframe (my_df).
clean_headers <- function(my_df) {
    # Use the 'fix_names' function to clean and fix the column names of the dataframe.
    colnames(my_df) %<>%
        fix_names()

    # Return the dataframe with cleaned and fixed column names.
    return(my_df)
}

# Change column names for consistency with other datasets
compl_override_data__renamed <-
  compl_override_data |>
  dplyr::rename(vessel_official_number =
                  "VESSEL_OFFICIAL_NBR",
                overridden = "IS_COMP_OVERRIDE") |>
  clean_headers()

# change data type of this column if needed
if (!class(compl_override_data__renamed$vessel_official_number) == "character") {
  compl_override_data__renamed$vessel_official_number <-
    as.character(compl_override_data__renamed$vessel_official_number)
}

# Add both months for straddle weeks

# Explanations:
# 1. 'compl_override_data__renamed_interv' is created from 'compl_override_data__renamed'.
# 2. It groups the data by the start and end dates of the compliance weeks.
# 3. It calculates the minimum month value between the 'comp_week_start_dt' and 'comp_week_end_dt' dates, storing it in a new column called 'comp_month_min'.
# 4. It calculates the maximum month value between the 'comp_week_start_dt' and 'comp_week_end_dt' dates, storing it in a new column called 'comp_month_max'.
# 5. The data is ungrouped after the calculations are done.

compl_override_data__renamed_interv <-
  compl_override_data__renamed |>
  group_by(comp_week_start_dt, comp_week_end_dt) |>
  mutate(comp_month_min =
           min(month(comp_week_start_dt),
               month(comp_week_end_dt))) |>
  mutate(comp_month_max =
           max(month(comp_week_start_dt),
               month(comp_week_end_dt))) |>
  ungroup()

# check
compl_override_data__renamed_interv |>
  filter(!comp_month_min == comp_month_max) |>
  select(
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    comp_month_min,
    comp_month_max
  ) |>
  distinct() |>
  glimpse()

### keep fewer fields from compliance info ----
# Keep only entries for "my_year" defined earlier and the previous year. Could be changed depending on the data provided by SC.

compl_override_data__renamed_m_short <-
  compl_override_data__renamed_interv |>
  select(
    vessel_official_number,
    permit_group,
    prm_grp_exp_date,
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    comp_month_min,
    comp_month_max
  ) |>
  distinct() |>
  filter(comp_year %in% c(my_year, as.integer(my_year) - 1))

# dim(compl_override_data__renamed_m)
# dim(compl_override_data__renamed_m_short)

### add compliance/overridden combinations by week ----
# This is needed so that we can easily filter out compliant or non-compliant vessels in the dataset, by adding an extra column that states yes or no regarding compliance. The NA represents one of two possible scenarios:
# 1) a DNF was submitted for a vessel that is missing from the compliance module but is in metrics tracking, or
# 2) a DNF was submitted for a week when the vessel was not permitted.
# It is not simple to determine which. Deciding what to do with these DNFs will depend on the individual analysis question, and so is not addressed here, but simply left as NA.


# Explanations:
# 1. Create a new variable 'res' to store the result.
# 2. Use 'rowwise' to perform operations row by row.
# 3. Use 'mutate' to create a new column 'compliant_after_override' based on conditions specified in 'case_when'.
#    - If 'is_comp' is 0 and 'overridden' is 0, set 'compliant_after_override' to "no".
#    - If 'is_comp' is 1 or 'overridden' is 1, set 'compliant_after_override' to "yes".
#    - If 'is_comp' is NA, set 'compliant_after_override' to NA.
#    - For all other cases, set 'compliant_after_override' to the string representation of 'is_comp'.
# 4. Use 'ungroup' to remove grouping from the data frame.
## NOTE: IF “Is_Overriden == 1 & is_Comp == 0, then the vessel should be considered compliant in any compliance analyses

add_compliant_after_override <- function(my_compl_df) {
  # browser()
  res <-
    my_compl_df |>
    rowwise() |>
    mutate(
      compliant_after_override =
        case_when(
          is_comp == 0 & overridden == 0  ~ "no",
          is_comp == 1 ~ "yes",
          overridden == 1 ~ "yes",
          is.na(is_comp) ~ NA,
          .default = toString(is_comp)
        )
    ) |>
    ungroup()

  return(res)
}

# Run the function
tic("get comp/overridden")
compl_override_data__renamed_m_short__compl_overr_by_week <-
  add_compliant_after_override(compl_override_data__renamed_m_short)
toc()
# get comp/overridden: 72.5 sec elapsed

# check all is_comp and overridden combinations
compl_override_data__renamed_m_short__compl_overr_by_week |>
    select(is_comp, overridden, compliant_after_override) |>
    distinct()
#   is_comp overridden compliant_after_override
# 1       1          0 yes
# 2       0          0 no
# 3       0          1 yes
# 4       1          1 yes

### Combine weekly compliance to create monthly compliance ----

# Explanations:
# - This code snippet combines weekly compliance data to create monthly compliance data for each vessel.
# - The process is timed using `tic()` and `toc()` to measure its execution time.
# - The `compl_override_data__renamed_m_short__compl_overr_by_week` dataframe is used as input.
# - The data is first grouped by `vessel_official_number`, `comp_year`, and `comp_month_min` (the month of the compliance week start).
# - For each group, the unique compliant statuses after override are calculated and concatenated into a string (`all_m_comp_min`).
# - Monthly compliance (`month_comp_min`) is determined based on the concatenated compliant statuses.
# - The process is repeated for `comp_month_max` to get the monthly compliance for the month of the end of the compliance week.
# - Finally, the `compl_override_data__renamed_m_short__m_compl__both_months` dataframe is created with both `comp_month_min` and `comp_month_max` data.
tic("get month_comp")
compl_override_data__renamed_m_short__m_compl__both_months <-
  compl_override_data__renamed_m_short__compl_overr_by_week |>
  group_by(vessel_official_number, comp_year, comp_month_min) |>
  mutate(all_m_comp_min =
           toString(unique(sort(
             compliant_after_override
           )))) |>
  mutate(month_comp_min =
           case_when(all_m_comp_min %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup() |>
    group_by(vessel_official_number, comp_year, comp_month_max) |>
  mutate(all_m_comp_max =
           toString(unique(sort(
             compliant_after_override
           )))) |>
  mutate(month_comp_max =
           case_when(all_m_comp_max %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup()
toc()
# get month_comp: 31.7 sec elapsed

# combine month compliance for each week

# Explanations:
# 1. 'compl_override_data__renamed_m_short__m_compl' is created from 'compl_override_data__renamed_m_short__m_compl__both_months'.
# 2. It groups the data by the vessel official number, compliance year, compliance week, start date of the compliance week, and end date of the compliance week.
# 3. It calculates a new column 'common_month_compliance' based on conditions:
#    a. If the minimum and maximum month values of compliance are both 'compl', then the 'common_month_compliance' is set to 'compl'.
#    b. For all other cases, it is set to 'non_compl'. Meaning if a week that overlaps these 2 months is non_compl, both months are non-compliant.
# 4. The data is ungrouped after the calculations are done.

tic("min_max_compl")
compl_override_data__renamed_m_short__m_compl <-
  compl_override_data__renamed_m_short__m_compl__both_months |>
  group_by(vessel_official_number,
           comp_year,
           comp_week,
           comp_week_start_dt,
           comp_week_end_dt) |>
  mutate(
    common_month_compliance =
      case_when(
        month_comp_min == month_comp_max &
          month_comp_min == "compl" ~
          month_comp_min,
        .default = "non_compl"
      )
  ) |>
  ungroup()
toc()
# min_max_compl: 35.58 sec elapsed

# check
compl_override_data__renamed_m_short__m_compl |>
  select(contains("month")) |>
  # filter(!month_comp_min == month_comp_max) |>
  distinct() |>
  glimpse()

## get processed logbooks ----

# set the path to processed logbook data
logbook_file_path <-
  file.path(processed_data_path,
            str_glue("SEFHIER_processed_Logbooks_{my_year}.rds"))

# read in logbook data, clean up headers
logbooks <-
  read_rds(logbook_file_path) |>
  clean_headers()

# checks dimensions of the dataframe
dim(logbooks)

## get dnfs ----

# set the path to processed DNF data
dnf_file_path <-
  file.path(processed_data_path,
            str_glue("SEFHIER_processed_dnfs_{my_year}.rds"))

# read in DNF data, clean up headers
dnfs <-
  read_rds(dnf_file_path) |>
  clean_headers()

# checks dimensions of the dataframe
dim(dnfs)

## get srhs vessels ----

# read csv and clean headers
srhs_2024 <-
  read_csv(srhs_2024_file_path,
           col_types = cols(.default = 'c')) |>
  clean_headers()

# glimpse(srhs_2024)

## read sc permitted data ----

# read xlsx and clean headers
SC_permittedVessels <- read.xlsx(
  sc_file_path
) |> clean_headers()

# Define a function 'print_df_names' to print the names of columns in a data frame.
# This function retrieves column names, limits the number to 'names_num' (default = 100),
# and returns them as a comma-separated string.
print_df_names <- function(my_df, names_num = 100) {
  # Use 'names' to get column names,
  # 'head' to limit the number of names to 'names_num',
  # 'paste0' to concatenate them with a comma separator, and return the result.
  names(my_df) %>%
    head(names_num) %>%
    paste0(collapse = ", ") %>%
    return()
}

print_df_names(SC_permittedVessels)

### fix dates in headers ----
# TODO: grab just the month of interest (depends on the SC file)

# grep for numbers only in the names and convert them

# save the not digit headers
not_date_names <-
  grep("^\\D+$", names(SC_permittedVessels), value = T)

# find all digit headers
date_names_raw <-
  grep("^\\d+$", names(SC_permittedVessels), value = T)

# Explanations:
# 1. Take the data frame 'date_names_raw'.
# 2. Use the function 'convertToDate()' to convert the Excel integer values to Date objects.
# 3. Use the 'format()' function to format the dates as "%m-%y", which represents month and year.
# 4. Assign the result to 'date_names_ok'.
date_names_ok <-
  date_names_raw |>
  convertToDate() |>
  format("%m-%y")

# combine the saved non-digit headers and the newly converted ones
all_names <- c(not_date_names, date_names_ok)

# check
all_names

# apply the names to the DF
names(SC_permittedVessels) <-
  all_names

# check
names(SC_permittedVessels)

# dim(SC_permittedVessels)
# [1] 215  18

# Convert federalfor_hirepermitexpiration column to a date format.
# convertToDate() converts from excel date number to R Date type
SC_permittedVessels_correct_dates <-
  SC_permittedVessels |>
  mutate(federalfor_hirepermitexpiration =
           convertToDate(federalfor_hirepermitexpiration))

### change sc data format ----

# glimpse(SC_permittedVessels)

# Explanations:
# 1. 'SC_permittedVessels_longer' is created by reshaping the data frame 'SC_permittedVessels_correct_dates'.
# 2. The 'pivot_longer()' function from the 'tidyr' package is used to reshape the data from wide to long format.
# 3. All columns except those specified in "!c()" are pivoted.
# 4. The 'names_to' parameter specifies the name of the new column that will store the names of the original columns. Here, it's set to "month_year".
# 5. The 'values_to' parameter specifies the name of the new column that will store the values from the pivoted columns. Here, it's set to "delinquent_month".

SC_permittedVessels_longer <-
  SC_permittedVessels_correct_dates |>
  pivot_longer(
    !c(
      "vesselreg_uscg_",
      "vesselname",
      "reportstosrhs",
      "federalfor_hirepermitexpiration",
      "markedasfederallypermittedinvesl",
      "delinquent"
    ),
    names_to = "month_year",
    values_to = "delinquent_month"
  )

glimpse(SC_permittedVessels_longer)

# Explanations:
# 1. 'SC_permittedVessels_longer_m_y' is created by processing the 'SC_permittedVessels_longer' data frame.
# 2. The 'separate_wider_delim()' function from the 'tidyr' package is used to separate the 'month_year' column into two separate columns ('month_sc' and 'year_sc') based on the specified delimiter '-'.
# 3. The 'names' parameter specifies the names for the new columns created by separation.
# 4. The 'delim' parameter specifies the delimiter used to separate the 'month_year' column.
# 5. The 'mutate()' function is used to modify the 'year_sc' column by pasting "20" in front of each entry to ensure it's in a four-digit format. This is done, because we have the four-digit ("2024") format for a year in the compliance report.
# 6. The 'mutate()' function is applied across all columns containing month and year information ('month_sc' and 'year_sc') to convert them to numeric format. Again, for compatibility with the other data sets.
# 7. The 'distinct()' function is used to remove duplicate rows from the data frame.

SC_permittedVessels_longer_m_y <-
  SC_permittedVessels_longer |>
  separate_wider_delim(cols = month_year,
                       delim = "-",
                       names = c("month_sc", "year_sc")) |>
  mutate(year_sc = paste0("20", year_sc)) |>
  mutate(across(all_of(c("month_sc", "year_sc")), as.numeric)) |>
  distinct()

# glimpse(SC_permittedVessels_longer_m_y)

# SRHS: check and remove reports_to_srhs ----

# Join the SC data with the SRHS list by vessel
sc__srhs_join <-
  full_join(SC_permittedVessels_longer_m_y,
            srhs_2024,
            join_by(vesselreg_uscg_ == uscg__))

# glimpse(sc__srhs_join)

# Get all the combinations of SC and SRHS lists.
# In this results we have:
# 1               0 NA
# Both are not SRHS
# 2               1 Y
# Both are SRHS
# 3              NA Y
# The vessel is not in the SC list, which is expected.

# For this SC entry file there are no discrepancies, so we can simply remove all the vessels marked as reports_to_srhs from the future analysis. We don't have compliance information for them.

sc__srhs_join |>
  select(reportstosrhs, is_insurvey) |>
  distinct()
#   reports_to_srhs is_insurvey
# 1               0 NA
# 2               1 Y
# 3              NA Y

# Keep only non-SRHS vessels
SC_permittedVessels_longer_m_y_no_srhs <-
  SC_permittedVessels_longer_m_y |>
  filter(reportstosrhs == 0)

# combine data ----

# Join the SC data with the compliance data we prepared, by vessel, month and year.

# Explanations:
# 1. 'sc__fhier_compl__join_w_month' is created by left joining two data frames: 'SC_permittedVessels_longer_m_y_no_srhs' and 'compl_override_data__renamed_m_short__m_compl'.
# 2. The join is performed based on the following conditions:
#    a. The vessel registration number from 'SC_permittedVessels_longer_m_y_no_srhs' matches the vessel official number from 'compl_override_data__renamed_m_short__m_compl'.
#    b. The month_sc column (representing the month in 'SC_permittedVessels_longer_m_y_no_srhs') falls within the range of months (comp_month_min to comp_month_max) in 'compl_override_data__renamed_m_short__m_compl'.
#    c. The year_sc column (representing the year in 'SC_permittedVessels_longer_m_y_no_srhs') matches the comp_year column in 'compl_override_data__renamed_m_short__m_compl'.

sc__fhier_compl__join_w_month <-
  left_join(
    SC_permittedVessels_longer_m_y_no_srhs,
    compl_override_data__renamed_m_short__m_compl,
    join_by(
      vesselreg_uscg_ == vessel_official_number,
      between(month_sc, comp_month_min, comp_month_max),
      year_sc == comp_year,
    )
  )

# check
dim(SC_permittedVessels_correct_dates)
# 215
dim(SC_permittedVessels_longer_m_y_no_srhs)
# SC_permittedVessels_longer_m_y_no_srhs
dim(sc__fhier_compl__join_w_month)
n_distinct(SC_permittedVessels_correct_dates)
# 215
n_distinct(sc__fhier_compl__join_w_month$vesselreg_uscg_)
# 207 (rm SRHS)
# glimpse(sc__fhier_compl__join_w_month)


sc__fhier_compl__join_w_month |>
  select(contains("month")) |>
  distinct() |>
  filter(!month_comp_min == month_comp_max) |>
  glimpse()
# 44

# Answering the questions

# save common column names
common_outpt_fields <-
  c("delinquent",
    "month_sc",
    "year_sc",
    "comp_week_start_dt",
    "comp_week_end_dt")

# 1. Non-compliant in SC and compliant in FHIER ----

# a)
non_compliant_vessels_in_sc_and_compl_in_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 1 &
           common_month_compliance == "compl")

dim(non_compliant_vessels_in_sc_and_compl_in_fhier)
# 8 24

# Get month and weeks when the vessels are marked as non-compliant in SC, but are compliant in FHIER
non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output <-
  non_compliant_vessels_in_sc_and_compl_in_fhier |>
  select(
    vesselreg_uscg_,
    all_of(common_outpt_fields),
    compliant_after_override
  ) |>
  distinct() |>
  arrange(vesselreg_uscg_, comp_week_start_dt)

# b) list all the dates of DNFs and/or logbooks we have in FHIER by vessel.

## add logbooks info ----
# Logbook (list any dates for that month)
# Get all logbooks info for this vessels filtered by month

logbooks__sc_fhier <-
  logbooks |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    join_by(
      vessel_official_number == vesselreg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    )
  )

# This is a good example of a trip that happens in Jan (1/30), in the week started in Jan and ended in Feb, hence it is marked as compliant in FHIER for February.
# glimpse(logbooks__sc_fhier)

# subset columns of data to output
logbooks__sc_fhier_for_output <-
  logbooks__sc_fhier |>
  select(
    vessel_official_number,
    all_of(common_outpt_fields),
    trip_start_date,
    trip_end_date,
    # vendor_app_name,
    trip_de,
    trip_ue
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_start_date)

# glimpse(logbooks__sc_fhier_for_output)

## add DNF info ----
# DNF (list week date range for any for that month)
dnfs__sc_fhier <-
  dnfs |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    join_by(
      vessel_official_number == vesselreg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    ),
    suffix = c("__dnf", "__fhier")
  )

# check
# dim(dnfs__sc_fhier)

# subset columns of data to output
dnfs__sc_fhier_for_output <-
  dnfs__sc_fhier |>
  select(
    vessel_official_number,
    all_of(common_outpt_fields),
    trip_date,
    compliant_after_override__fhier
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_date)

# dim(dnfs__sc_fhier_for_output)
# 0

# 3. SC compliant and not compliant in FHIER ----

# 3) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 0 &
           common_month_compliance == "non_compl")

dim(compliant_vessels_in_sc_and_non_compl_fhier)

# "all_m_comp" field shows if any weeks of that month were compliant. We consider the whole month non-compliant if even one week was non-compliant. If SC considers the month compliant if at least one week was compliant that makes a big difference in the monthly compliance counts between SC and FHIER.

# subset columns of data to output
compliant_vessels_in_sc_and_non_compl_fhier__for_output <-
  compliant_vessels_in_sc_and_non_compl_fhier |>
  select(
    vesselreg_uscg_,
    all_of(common_outpt_fields),
    compliant_after_override
  ) |>
  filter(compliant_after_override == "no") |>
  distinct() |>
  arrange(vesselreg_uscg_, comp_week_start_dt)

dim(compliant_vessels_in_sc_and_non_compl_fhier__for_output)

# Write results to xlsx ----
# (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or
# (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# 3. we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER

# 1. Create a wb with all output dfs ----

# Explanations:
# Create a list 'output_df_list' containing multiple data frames.
# This list is constructed using the 'lst' function from the tibble package,
# which combines several objects into a list. lst() also generates missing names automatically.

# 1. 'non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output': A data frame of non-compliant vessels in SC but compliant in FHIER.
# 2. 'logbooks__sc_fhier_for_output': A data frame of logbooks data from FHIER.
# 3. 'dnfs__sc_fhier_for_output': A data frame of DNFs data from FHIER.
# 4. 'compliant_vessels_in_sc_and_non_compl_fhier__for_output': A data frame of compliant vessels in SC and non-compliant FHIER.

output_df_list <-
  lst(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    logbooks__sc_fhier_for_output,
    dnfs__sc_fhier_for_output,
    compliant_vessels_in_sc_and_non_compl_fhier__for_output
  )

# a simple list of sheet names
sheet_names <-
  list(
    "non_compl_sc__compl_fhier",
    "non_compl_sc__compl_fhier_lgb",
    "non_compl_sc__compl_fhier_dnf",
    "compl_sc__non_compl_fhier"
  )

# Make a copy with different names. output_df_list has dataframe names for column names and print_result_list has sheet names for column names.
print_result_list <-
  output_df_list

names(print_result_list) <- sheet_names

# Explanations:
# Build a workbook using a list of data frames ('print_result_list') and create tables in the workbook.

# 1. 'wb': An object representing the workbook to be created.
# 2. The function 'buildWorkbook()' is used to create a workbook from a list of data frames.
#    It takes the list of data frames ('print_result_list') as input and an option to convert them to tables ('asTable = TRUE').
#    The 'asTable = TRUE' option ensures that each data frame is added to the workbook as a formatted table.

wb <- buildWorkbook(print_result_list, asTable = TRUE)

# 2. create a readme sheet ----
## today ----

# Explanations:
# Create a tibble with the current date in a specific format and set it as the "Read me" column.

# 1. 'top_of_read_me_text': A variable that stores a tibble containing the current date as its content.

# 2. The 'today()' function from the 'lubridate' package is used to get the current date.

# 3. The 'as_tibble_col()' function is used to create a tibble with a single column.
#    - The first argument is the value to be used for the tibble (in this case, the current date).
#    - The 'column_name' argument specifies the name of the single column ("Read me").
#    This results in a tibble with the current date stored under the "Read me" column.

top_of_read_me_text <-
  today() |>
  as_tibble_col(column_name =  "Read me")

## sheet names with sheet descriptions ----
# save sheet_descriptions
sheet_descriptions <-
  c(
    "vessels that are non-compliant with SC but compliant with SEFHIER",
    "logbooks in FHIER from non-compliant SC vessels",
    "DNFs in FHIER from non-compliant SC vessels",
    "vessels that are compliant with SC but not with SEFHIER"
  )

# Explanations:
# Combine 'sheet_names' and 'sheet_descriptions' into a data frame.

# 1. 'sheet_names_with_df_names': A variable that stores a data frame containing sheet names and descriptions.

# 2. The 'cbind()' function combines the two vectors 'sheet_names' and 'sheet_descriptions' into a single data frame.
#    - This function combines the columns of the input vectors.
#    - Since 'sheet_names' and 'sheet_descriptions' are vectors, the resulting output is a two-column matrix.

# 3. The matrix is then converted to a data frame using the 'as.data.frame()' function.
#    - This conversion is necessary to work with the data structure more efficiently.
#    - The resulting data frame contains one column for sheet names and one column for sheet descriptions.

sheet_names_with_df_names <-
  cbind(sheet_names, sheet_descriptions) |>
  as.data.frame()

# rename columns in the new df
names(sheet_names_with_df_names) <-
  c("Sheet name", "Sheet Details")

# glimpse(sheet_names_with_df_names)

## column explanations for each tab ----
### colnames_for_each_df ----

# Use to create the list of colnames for each DF to be corrected in Excel
# Explanations:
# 1. 'colnames_for_each_df' is created by mapping over 'output_df_list' and 'sheet_names' simultaneously.
# 2. For each dataframe 'my_df' in 'output_df_list' and corresponding 'sheet_name':
#    a. Retrieve the column names of 'my_df' using the 'names' function.
#    b. Convert the column names into a tibble format with a single column named 'column_name' and assign the value of 'sheet_name' to each row.
#    c. Return the tibble containing the column names with associated sheet names.

get_each_df_colnames <-
  function(output_df_list, sheet_names) {

  colnames_for_each_df <-
    map2(output_df_list, sheet_names,
         \(my_df, sheet_name) {
           names(my_df) |>
             as_tibble_col(column_name = sheet_name) %>%
             return()
         })

  return(colnames_for_each_df)
}

# Uncomment and run if needed
# colnames_for_each_df <- get_each_df_colnames(output_df_list, sheet_names)

# Use column definitions saved in a csv file

# define a path
column_definitions_file_path <-
    file.path(input_path,
            "column_definitions.csv")

# optional check
# file.exists(column_definitions_path)

# read csv
column_definitions <-
  read_csv(column_definitions_file_path)

# combine 3 dfs and convert to a type needed for output.
readme_text <-
  list(
    top_of_read_me_text,
    sheet_names_with_df_names,
    column_definitions
  )

# check class for each df, optional
map(readme_text, class)

## Add a new sheet ----
addWorksheet(wb, "Readme")

## Add a new style ----
bold.style <- createStyle(textDecoration = "Bold")

## Write each df from readme_text on to the same sheet ----

# Explanations:
# - This function, `write_readme_sheet`, writes the given `readme_text` data into a specified sheet of an Excel workbook.
# - `readme_text`: A list of data frames to be written into the workbook's specified sheet.
# - `workbook`: The Excel workbook object where data will be written. Defaults to the `wb` variable.
# - `sheet_name`: The name of the sheet where the data will be written. Defaults to "Readme".

  # 1. Initialize the `curr_row` variable to 1.
  #    This variable keeps track of the current row position in the sheet where data will be written.

  # 2. Loop through each element in `readme_text`:
  #    - `i` is the index of the current element.
  #    - `one_df` is the current dataframe.

  # 3. Check if `one_df` is indeed a data frame:
  #    - If not, convert it to a data frame using `as.data.frame()`.
  #    - This is done to ensure compatibility with the `writeData` function.

  # 4. Calculate the number of rows in `one_df` using `nrow()`.
  #    - This is stored in `one_df_size`.

  # 5. Use the `writeData` function to write `one_df` to the workbook:
  #    - `workbook` is the Excel workbook object where data will be written.
  #    - `sheet_name` is the name of the sheet where data will be written.
  #    - `one_df` is the data to be written.
  #    - `startRow` is set to `curr_row`, specifying the starting row in the sheet.
  #    - `headerStyle` is set to `bold.style` to apply bold styling to headers.

  # 6. Update `curr_row` to point to the next available row in the sheet:
  #    - Add the number of rows in `one_df` to `curr_row`.
  #    - Add 2 to account for spacing between each data frame.

write_readme_sheet <-
  function(readme_text, workbook = wb, sheet_name = "Readme") {
    curr_row <- 1
    for (i in seq_along(readme_text)) {
      # browser()
      one_df <- readme_text[[i]]

      if (!any(grepl("data.frame", class(one_df))))
      {
        one_df <- as.data.frame(one_df)
      }
      one_df_size <- nrow(one_df)

      writeData(workbook,
                sheet_name,
                one_df,
                startRow = curr_row,
                headerStyle = bold.style)
      curr_row <- curr_row + one_df_size + 2
    }
  }

# run the function
write_readme_sheet(readme_text,
                   workbook = wb,
                   sheet_name = "Readme")

# Optional. Not needed for processing.

# The readme sheet will be in the last position.
# worksheetOrder(wb)
# [1] 1 2 3 4 5

# To see the wb.
# openXL(wb)

## Move readme to the first position ----

# Explanations:
# 1. Get the current order of worksheets in the Excel workbook 'wb' using 'worksheetOrder' function and store it in 'old_order'.
# 2. Calculate the length of 'old_order' and store it in 'length_of_wb'.
# 3. Rearrange the order of worksheets in 'wb' by assigning a new order:
#    a. Start with the last worksheet by placing it at the first position using 'length_of_wb'.
#    b. Followed by the rest of the worksheets from the first to the second-to-last position using '1:(length_of_wb - 1)'.

old_order <- worksheetOrder(wb)
length_of_wb <- old_order |> length()
worksheetOrder(wb) <- c(length_of_wb, 1:(length_of_wb - 1))

# Optional. Not needed for processing.

# To see the new sheet order.
# worksheetOrder(wb)
# [1] 5 1 2 3 4

# To see the wb
# openXL(wb)

# Write the Excel file ----

# define the path
output_file_name <-
  file.path(output_path,
            "sc_compliance.xlsx")

# write the workbook unto file
saveWorkbook(wb, output_file_name, overwrite = T)

