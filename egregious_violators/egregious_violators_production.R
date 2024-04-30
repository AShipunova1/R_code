# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired and were active for the same period as (1);
# 3) the grace period is 7 days back from today.
# 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# NB. Update (download) all input files every time before run.

# set up ----

# Load the 'ROracle' library, which provides an interface for working with Oracle databases in R.
library(ROracle)

# Load the 'tidyverse' library, which is a collection of R packages for data manipulation and visualization.
library(tidyverse)

# Load the 'magrittr' library, which provides piping data and functions.
library(magrittr)

# Load the 'tictoc' library, which allows measuring code execution time.
library(tictoc)

# Load the 'openxlsx' library, which simplifies the creation of Excel .xlsx files by providing a high level interface to writing, styling and editing worksheets. 
library(openxlsx)

# S3 Infrastructure for Regular and Irregular Time Series (Z's Ordered Observations)
library(zoo)

# Auxiliary
# Set up paths ----

# function that will return current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
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

# Add your paths here
annas_path <- set_work_dir()

# Change to your path
my_paths <- annas_path

# get this project name
current_project_dir_name <- this.path::this.dir()

# find its base name
current_project_name <-
  basename(current_project_dir_name)

# use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

# set years and dates
my_year1 <- "2023"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2024"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

# save today's date
data_file_date <- 
  today()

# define constants and variables to calculate egregious violation period
# how many weeks
number_of_weeks_for_non_compliancy = 26

# how many days
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

# Auxiliary
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

# Functions always used for all compliance/correspondence preparation. Start ----

# split week column ("52: 12/26/2022 - 01/01/2023") into 3 columns with proper classes, week_num (week order number), week_start and week_end
# Define a function named 'clean_weeks' that takes a data frame 'my_df' as input.
# It returns the modified 'my_df' with the cleaned and transformed 'week' columns, including 'week_num', 'week_start', and 'week_end'.

clean_weeks <- function(my_df) {
  my_df %>%

    # Separate the 'week' column using ":" as the delimiter and create new columns 'week_num' and 'week_rest'
    separate_wider_delim(week, ":", names = c("week_num", "week_rest")) %>%

    # Further separate 'week_rest' using " - " as the delimiter and create 'week_start' and 'week_end' columns
    separate_wider_delim(week_rest, " - ", names = c("week_start", "week_end")) ->
    temp_df

  # Convert 'week_num' to integer and update 'my_df' with the result
  my_df$week_num <- as.integer(trimws(temp_df$week_num))

  # Convert 'week_start' to Date format using the specified date format "%m/%d/%Y"
  my_df$week_start <- as.Date(trimws(temp_df$week_start), "%m/%d/%Y")

  # Convert 'week_end' to Date format using the specified date format "%m/%d/%Y"
  my_df$week_end <- as.Date(trimws(temp_df$week_end), "%m/%d/%Y")

  # Return the modified 'my_df' with cleaned and transformed 'week' columns
  return(my_df)
}

# ===
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

# ===
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

# ===
# The join_same_kind_csvs function is defined to concatenate multiple data frames in the 'csvs_list_2_plus' parameter vertically.
join_same_kind_csvs <- function(csvs_list_2_plus) {

  # Concatenate the data frames in 'csvs_list_2_plus' vertically using 'bind_rows'. This function binds the rows of the data frames together, assuming that they have the same column structure.
  result_df <- dplyr::bind_rows(csvs_list_2_plus)

  # Return the combined data frame
  return(result_df)
}

# ===
# Combine correspondence and compliance information into one dataframe by "vesselofficialnumber" only. Not by time!
# The join_all_csvs function is defined to perform a full join operation on two data frames: 'corresp_arr' and 'compl_arr'. It handles cases where these parameters might be lists of data frames or individual data frames.
# It returns the resulting data frame ('result_df') containing the merged data from 'compl' and 'corresp' data frames.

join_all_csvs <- function(corresp_arr, compl_arr) {

  # Initialize 'corresp' with 'corresp_arr' or join data frames in 'corresp_arr' if it's not already a data frame
  corresp <- corresp_arr
  if (!is.data.frame(corresp_arr)) {
    corresp <- join_same_kind_csvs(corresp_arr)
  }

  # Initialize 'compl' with 'compl_arr' or join data frames in 'compl_arr' if it's not already a data frame
  compl <- compl_arr
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  # Perform a full join of 'compl' and 'corresp' data frames on the 'vesselofficialnumber' column, retaining all rows
  result_df <- compl %>%
    full_join(corresp,
              by = c("vesselofficialnumber"),
              multiple = "all")

  # Return the resulting data frame
  return(result_df)
}

# ===
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

# ===
# The aux_fun_for_dates function is defined as a utility function to convert a given vector 'x' to POSIXct date format using the specified 'date_format'.
aux_fun_for_dates <- function(x, date_format) {

  # Convert 'x' to POSIXct date format using 'as.POSIXct'
  out <- as.POSIXct(x, format = date_format)

  # Return the result as a POSIXct date object
  return(out)
}

# The change_fields_arr_to_dates function is defined to convert multiple columns specified in 'field_names_arr' in the input data frame ('my_df') to POSIXct date format using the provided 'date_format'.
# Inside the function, it uses the mutate function along with across from the dplyr package to target and modify the specified columns in 'field_names_arr'. The all_of(field_names_arr) ensures that all the columns listed in 'field_names_arr' are selected.

# Within the across function, it applies the as.POSIXct function to each column ('x') in 'field_names_arr' using the provided 'date_format'. This step converts the values in these columns to POSIXct date format.
# It returns the 'result_df', which is the input data frame with the specified columns converted to dates according to the specified 'date_format'.

change_fields_arr_to_dates <-
  function(my_df, field_names_arr, date_format) {
    # Use 'mutate' and 'across' to convert all specified columns in 'field_names_arr' to POSIXct date format
    result_df <- my_df %>%
      dplyr::mutate(dplyr::across(
        all_of(field_names_arr),
        ~ as.POSIXct(.x, format = date_format)  # Apply 'as.POSIXct' to each column with the provided 'date_format'
      ))

    # Return the data frame with the specified columns converted to dates
    return(result_df)
  }

# ===
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

# ===
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

# ===
# Get frequencies for each column in the list
# usage:
# group_by_arr <- c("vesselofficialnumber", "contacttype")
# count_by_column_arr(my_df, group_by_arr)
# Define a function 'count_by_column_arr' to count the frequency of combinations of columns.
# This function takes two arguments: my_df, which is the input data frame, and group_by_arr, which is a character vector containing the names of columns to group by.

count_by_column_arr <- function(my_df, group_by_arr) {
  my_df %>%
    dplyr::arrange(group_by_arr[1]) %>%          # Arrange the data by the first column in 'group_by_arr'.
    group_by_at(group_by_arr) %>%         # Group the data by the columns specified in 'group_by_arr'.
    summarise(my_freq = n()) %>%           # Calculate the frequency of each combination.
    return()                              # It returns the summary result.
}

# ===
# Define a function 'count_uniq_by_column' to count the number of unique values in each column of a data frame.

# Within the function, the sapply function is used to apply another function to each column of the input data frame. Specifically, it counts the number of unique values in each column using the length(unique(x)) expression, where x represents each column of the data frame.
# The result of sapply is a vector containing the counts of unique values for each column.

# It returns the resulting data frame, which provides a summary of the counts of unique values for each column in the input data frame. This information can be valuable for assessing the diversity of values within each column.

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>%  # Apply a function to each column to count unique values.
    as.data.frame()  # Convert the result to a data frame.
}

# ===
# The data_overview function is designed to provide an overview of a given data frame, including summary statistics and counts of unique values in each column.

data_overview <- function(my_df) {
  # Use 'summary' function to generate summary statistics and print the results.
  summary(my_df) %>% print()

  # Print a header indicating the next section of the output.
  cat("\nCount unique values in each column:\n")

  # Call the 'count_uniq_by_column' function to count unique values in each column of the data frame.
  count_uniq_by_column(my_df)
}

# ===

# from https://stackoverflow.com/questions/53781563/combine-rows-based-on-multiple-columns-and-keep-all-unique-values
# concat_unique <- function(x){paste(unique(x),  collapse=', ')}

# Explanations:
# 1. Extract unique non-NA elements from the input vector 'x' using 'unique'.
# 2. Concatenate these unique elements into a single string with ", " as the separator using 'paste0' and 'collapse'.

concat_unique <- function(x) {
  paste0(unique(x[!is.na(x)]), collapse = ", ")
}

# ===
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

# ===
# Define a function to combine rows based on multiple columns while keeping all unique values.
# This function groups the data frame by specified columns,
# applies 'concat_unique' to combine values in each column,
# and returns the result.
combine_rows_based_on_multiple_columns_and_keep_all_unique_values <-
  function(my_df, group_by_arr) {
    # Group the data frame by specified columns.
    my_df %>%
      dplyr::group_by_at(group_by_arr) %>%
      # Summarize all columns by applying 'concat_unique' to combine unique values.
      dplyr::summarise_all(concat_unique) %>%
      return()
  }

# ===
# Define a function to concatenate unique values in a sorted manner.
# This function takes a vector 'x', removes NA values, sorts the unique values,
# and then concatenates them with a comma separator.
concat_unique_sorted <- function(x) {
  # Remove NA values from the input vector 'x' and store the result.
  non_na_values <- x[!is.na(x)]

  # Sort the unique values obtained from the previous step.
  sorted_unique <- unique(sort(non_na_values))

  # Concatenate the sorted unique values with a comma separator.
  result <- paste0(sorted_unique, collapse = ", ")

  # Return the concatenated result.
  return(result)
}

# ===
# Define a function to combine rows based on multiple columns and keep all
# unique values sorted within each group.
# This function takes a data frame 'my_df' and a vector of column names
# 'group_by_arr' as input.
combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values <-
  function(my_df, group_by_arr) {
  # Group the data frame 'my_df' by the columns specified in 'group_by_arr'.
  # This step ensures that we create groups based on unique combinations of
  # values in the specified columns.
  grouped_df <- my_df %>%
    dplyr::group_by_at(group_by_arr)

  # Apply the 'concat_unique_sorted' function to all columns in each group.
  # This function concatenates all unique values within each group and sorts
  # them in ascending order.
  summarized_df <- grouped_df %>%
    dplyr::summarise_all(concat_unique_sorted)

  # Return the resulting data frame 'summarized_df'.
  return(summarized_df)
}

## usage:
# my_paths <- set_work_dir()
#
## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths)
# compl_clean <- temp_var[[1]]
# corresp_clean <- temp_var[[2]]

csv_names_list_22_23 = c("Correspondence.csv",
                         "FHIER_Compliance_22.csv",
                         "FHIER_Compliance_23.csv")

# To add my additional folder names to each filename.

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

# ===

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

# ===
# Define a function to read CSV files with EOFs (End of File) from a specified directory.
# This function takes 'my_paths' (directory paths) and 'csv_names_list' (list of CSV file names) as input.
read_csv_w_eofs <- function(my_paths, csv_names_list) {
  # Get the input directory path from 'my_paths'.
  my_inputs <- my_paths$inputs

  # Create a vector 'myfiles' that contains the full paths to each CSV file.
  # Add the input directory path in front of each file name.
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, add_csv_path, x))

  # Read CSV files using 'fread' from the 'data.table' package with 'header = TRUE' (considering the first row as column names).
  contents <- sapply(myfiles, fread, header = TRUE)

  # Convert the first CSV file into a data frame.
  # TODO: Consider changing this function to handle multiple files.
  # For now, it returns the first CSV file as a data frame.
  contents[, 1] %>%
    as.data.frame() %>%
    return()
}

# ===
# To use as a filter in FHIER
# Define a function to concatenate and output character data to a text file.
# This function takes 'my_characters' (a character vector) as input.
cat_filter_for_fhier <- function(my_characters) {
  # Concatenate the elements of 'my_characters' using a comma and space as the separator.
  # Output the concatenated string to a text file named "cat_out.txt" in the outputs directory.
  cat(my_characters,
      sep = ', ',
      file = file.path(my_paths$outputs, "cat_out.txt"))
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
# Explanations:
# 
# 1. The function `function_message_print` is defined, which takes one argument `text_msg` (the text message to be printed).
# 2. Inside the function, the `cat` function is used to print `text_msg` to the console.
# 3. The function applies custom styling to `text_msg` using the `crayon` package:
#    - `crayon::bgCyan$bold(text_msg)` applies a cyan background color and bold styling to `text_msg`.
# 4. The `sep = "\n"` argument to `cat` ensures that the output is followed by a new line, so each message is printed on a separate line.
# 5. The function prints the styled `text_msg` to the console and does not return any value (`NULL` by default).
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

# Explanations:
# 1. The function `get_df_name_as_text` is defined, which takes one argument `my_df` (the data frame for which the name should be retrieved as text).
# 2. The function uses the `substitute` function to capture the expression used to pass the data frame as `my_df`.
# 3. The `deparse` function is then used to convert the expression from `substitute` into a character string, which represents the name of the data frame.
# 4. The resulting character string (`df_name`) is stored in a variable of the same name.
# 5. The function returns the name of `my_df` as a string (`df_name`).
get_df_name_as_text <-
  function(my_df) {
    df_name = deparse(substitute(my_df))
    return(df_name)
  }

# to print the title message in blue.
title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg), sep = "\n")
}

# ===
# Explanations:
# - This function, `my_tee`, is designed to print messages to both the console and a log file.
# - The function takes four parameters: 
#   - `my_text`: The text message to print and log.
#   - `my_title`: An optional title for the message. If not provided, the function will default to `NA`.
#   - `stat_log_file_path`: The file path where the message should be logged. If not provided, a default path is created.
#   - `date_range`: An optional date range used to customize the log file's name. The default value is `2022`.
# 
# - The function includes three main operations:
#   - Print the message to the console with a title.
#   - Create a log file path and write the message to the log file.
#   
# Here's what's happening in detail:
# 
# 1. The function `my_tee` is defined with four parameters: `my_text`, `my_title`, `stat_log_file_path`, and `date_range`.
# 
# 2. The function initializes a constant `the_end` with the value `"---"`, which is used to mark the end of each message.
# 
# 3. If the `date_range` parameter is not provided, it defaults to `2022`.
# 
# 4. The function prints the title and message to the console. It uses the function `title_message_print` (not defined in the provided code) to print the title. Then, it prints the text of the message followed by the end mark (`the_end`).
# 
# 5. If the `stat_log_file_path` parameter is not provided, the function constructs a default file path using the `Path` and `Outputs` variables (not defined in the provided code) and the current date (`today()`). The file path is constructed with the title, date range, and date as part of the file name.
# 
# 6. Finally, the function writes the title, message, and end mark to the log file at the specified path. The function appends the new message to the file if it already exists.
my_tee <- function(my_text,
                   my_title = NA,
                   stat_log_file_path = NA,
                   date_range = NA) {

  the_end = "---"

  if (is.na(date_range)) date_range = 2022

  # Print out to console
  title_message_print(my_title)
  cat(c(my_text, the_end),
      sep = "\n")

  # Create a new file every day
  if (is.na(stat_log_file_path)) {
    stat_log_file_path <-
      file.path(Path,
                Outputs,
                str_glue("{my_title}_{date_range}_run_{today()}.log"))
  }

  # Write to a log file
  cat(c(my_title, my_text, the_end),
      file = stat_log_file_path,
      sep = "\n",
      append = TRUE)
}


# ===
# A function to use every time we want to read a ready file or query the database if no files exist. Pressing F2 when the function name is under the cursor will show the function definition.

# Explanations:
# - This function, `read_rds_or_run`, is designed to read data from an RDS file if it exists or run a specified function to obtain the data and save it as an RDS file if the file does not exist or if the `force_from_db` parameter is set.
# 
# Here's what's happening in detail:
# 
# 1. **Function Definition**: The function takes four parameters: 
#    - `my_file_path`: The path to the RDS file to be read or saved.
#    - `my_data`: The data to be used with the function. Default is an empty data frame.
#    - `my_function`: The function to be run to obtain the data if necessary.
#    - `force_from_db`: A flag that, when set, will force the function to run the specified function instead of reading from the file, even if the file exists.
# 
# 2. **Check File Existence**: The function first checks if the file specified by `my_file_path` exists and, if so, retrieves its modification time. 
# 
# 3. **Read or Run**: Depending on the existence of the file and the `force_from_db` flag:
#     - **File Exists and `force_from_db` is not set**: If the file exists and `force_from_db` is not set, the function reads the data from the RDS file using `readr::read_rds(my_file_path)` and assigns it to `my_result`.
#     - **File Does Not Exist or `force_from_db` is set**: If the file does not exist or `force_from_db` is set, the function follows these steps:
#         - Prints a message indicating the file doesn't exist and data will be pulled from the database.
#         - Times the function execution using `tictoc::tic()` and starts with a message indicating the date and purpose of the run.
#         - Runs the specified function (`my_function`) on the provided `my_data` to generate the result (`my_result`), e.g., downloading data from the Oracle database.
#         - Stops timing the function execution using `tictoc::toc()`.
#         - Saves the result as an RDS file to the specified `my_file_path` for future use using `readr::write_rds(my_result, my_file_path)`. A `try` block is used to handle potential errors in writing the file.
#         - Prints a message indicating that the new data is being saved into a file.
# 
# 4. **Print File Information**: After obtaining the data, the function prints the file name and modification time to provide information on when the data was last downloaded or modified.
# 
# 5. **Return**: The function returns the generated or read data (`my_result`).

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

  # Print out the formatted string with the file name ('my_file_name') and the modification time ('modif_time') to keep track of when the data were downloaded.
  my_file_name <- basename(my_file_path)
  function_message_print(
    str_glue("File: {my_file_name} modified {modif_time}"))

    # Return the generated or read data.
    return(my_result)
}

# ===
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

# Functions always used for all compliance/correspondence preparation. End

# get cleaned compliance and correspondence dfs
temp_var <-
  get_compl_and_corresp_data(my_paths$inputs, all_csv_names_list)

# rename variables
compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

# add names
names(compl_clean_list) <- c(my_year1, my_year2)

# check
map(compl_clean_list, dim)

# combine years in one df
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

# check
dim(compl_clean)
dim(corresp_contact_cnts_clean0)

## get Metric Tracking (permits from FHIER) ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")

# optional
# dir.exists(processed_input_data_path)
# T  

# file names for all years
processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_.*",
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

# read the rest in a loop
processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

# lower names case
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# check
dim(processed_metrics_tracking_permits)

## get Physical Address List from FHIER ----
# download first
# REPORTS / For-hire Primary Physical Address List
fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from_Fhier\address\For-hire Primary Physical Address List_04_09_2024.csv)"
  )

# optional
file.exists(fhier_addresses_path)

# read file
# Explanations:
# - The code snippet reads a CSV file and stores its data in the `fhier_addresses` variable. The `read_csv` function from the `readr` package is used for this purpose.
# 
# Here's what's happening in detail:
#   
#   1. **Function Call**: The `read_csv` function is called with specific parameters:
#   - `fhier_addresses_path`: The path to the CSV file to be read. This is the source file that contains the data you want to load into the `fhier_addresses` variable.
# - `col_types = cols(.default = 'c')`: This argument specifies the types of columns in the CSV file. The `.default = 'c'` part indicates that all columns should be treated as character data (text data).
# - `name_repair = fix_names`: This parameter specifies a function (in this case, `fix_names`) that will be used to clean and repair the column names of the data frame after reading the CSV file.
# 
# 2. **Loading Data**: The function reads the CSV file located at `fhier_addresses_path` according to the specified column types (all character) and repairs the column names using `fix_names`.
# 
# 3. **Storing Data**: The data read from the CSV file is stored in the `fhier_addresses` variable, which is now a data frame with the data from the CSV file.
# 
# Overall, the function reads a CSV file, treats all columns as character data, repairs column names, and stores the resulting data frame in the `fhier_addresses` variable.
fhier_addresses <-
  read_csv(fhier_addresses_path,
           col_types = cols(.default = 'c'),
           name_repair = fix_names)

# PIMS ----
## get home port processed city and state ----
# download first

# set a path
processed_pims_home_ports_path <-
  file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports.csv")

# read
processed_pims_home_ports <- 
  read_csv(processed_pims_home_ports_path)

# Oracle db ----
## get owners addresses ----
# set variables to use as read_rds_or_run parameters
# the SQL query
db_participants_address_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@secapxdv_dblk
"

# the existing file path
db_participants_address_file_path <-
  file.path(my_paths$inputs,
            current_project_name,
            "db_participants_address.rds")
 
# optional
# dir.exists(file.path(my_paths$inputs,
#             current_project_name))

# Explanations:
# Establish a connection to a database, only if one does not already exist.
# 
# 1. **Condition Check**: The code begins with a conditional statement that checks if the object `con` exists in the current environment using the `exists` function. 
#     - `exists("con")`: This function checks whether a variable or object named `con` exists in the current environment.
#     - The function returns `TRUE` if `con` exists, and `FALSE` otherwise.
# 
# 2. **Establishing Connection**: If the object `con` does not exist (`!exists("con")` evaluates to `TRUE`), the code inside the block executes. This means a connection needs to be established.
# 
# 3. **Using `try`**: The code inside the block uses the `try` function:
#     - `try(con <- connect_to_secpr())`: This expression attempts to establish a connection using the `connect_to_secpr()` function and assigns the result to the `con` variable.
#     - `try` is used here to handle potential errors that might occur during the connection process, such as connection timeouts or incorrect credentials. If an error occurs, the `try` function will catch it and prevent the script from terminating.
# 
# 4. **End Result**: If the `connect_to_secpr()` function executes successfully, the connection object (`con`) is created and available for further operations. If an error occurs, the `try` function will handle it gracefully.
# 
# In summary, the code checks if a database connection (`con`) already exists. If not, it attempts to establish a connection using the `connect_to_secpr()` function, and any potential errors during the connection process are handled using the `try` function.
if (!exists("con")) {
  try(con <- connect_to_secpr())
}

# crate an aux function to use in read_rds_or_run()
# Explanations:
# This function executes a given SQL query on a database using an established connection (`con`) and returns the results.
# 
# 1. **Function Definition**: 
#     - `function(db_participants_address)`: This line defines the function named `db_participants_address_fun` and specifies that it accepts one argument (`db_participants_address`). This argument is expected to be a string containing an SQL query.
# 
# 2. **Query Execution and Return**:
#     - `return(dbGetQuery(con, db_participants_address))`: 
#         - The function executes the SQL query provided in the `db_participants_address` argument using the established database connection `con`.
#         - The function `dbGetQuery` is part of the `DBI` package, which is commonly used for interacting with databases in R.
#         - The function takes two arguments: the connection object (`con`) and the SQL query (`db_participants_address`).
#         - It executes the SQL query on the database and returns the result as a data frame.
#     - The result of the `dbGetQuery` function is returned as the output of the function `db_participants_address_fun`.
# 
# In summary, `db_participants_address_fun` takes an SQL query string as input, executes the query on the database using an established connection (`con`), and returns the result as a data frame.
db_participants_address_fun <-
  function(db_participants_address) {
    return(dbGetQuery(con,
                      db_participants_address))
  }

# This code reads data from a file (or a database query if the file doesn't exist) using read_rds_or_run, cleans up the data by removing empty columns with remove_empty_cols, and standardizes column names with clean_headers.
# See the full functions definition above (F2).
# Use force_from_db = "yes" parameter to force a new pull for the DB

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

# check
dim(db_participants_address)

# Data from the previous results of "egregious violators for investigation" ----
# Download first as .xlsx

# get previous results ---
# download from Google drive first
# set the filepath
prev_result_path <- 
  file.path(my_paths$inputs,
            current_project_name,
            "egregious violators for investigation_2023-08-15_to_2024-02-13_OLE.xlsx")

# optional
# file.exists(prev_result_path)
# This code reads data from an Excel file using read.xlsx, cleans up the data by removing empty columns with remove_empty_cols, and standardizes column names with clean_headers. The processed data is stored in the variable prev_result.

prev_result <-
  read.xlsx(prev_result_path) |> 
  remove_empty_cols() |> 
  clean_headers()

# check
dim(prev_result)

# Results ----
# Combine result names from "Get data"
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

# Print them out onto the console
cat(c("Data are in:",
      results),
    sep = "\n")

# end of get data

# Data preparation specific for Egregious violators project ----
# Preparing compliance info ----

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

### get only not expired for the predefined period of data (half_year_ago) minus the grace period ----
compl_clean_w_permit_exp__not_exp <-
  compl_clean_w_permit_exp |>
  filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  filter(week_end < last_week_start) |>
  # not expired
  filter(tolower(permit_expired) == "no")

# check, optional
min(compl_clean_w_permit_exp__not_exp$permit_groupexpiration)
# [1] "2024-02-29 EST"

min(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2023-08-14"

max(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2024-01-29"

max(compl_clean_w_permit_exp__not_exp$week_end)
# [1] "2024-02-04"

## ---- add year_month column ----
# Explanations:
# - This code snippet performs filtering and transformation on a data frame (`compl_clean_w_permit_exp__not_exp`) to keep only entries for the last six months (half a year) from a given check period.
# 
# 1. **Data Frame Transformation**:
#     - `compl_clean_w_permit_exp__not_exp`:
#         - This is the data frame that will be processed.
#         
# 2. **Adding a New Column**:
#     - `|> mutate(year_month = as.yearmon(week_start))`:
#         - The pipe operator (`|>`) is used to pass the data frame to the `mutate` function.
#         - The function adds a new column `year_month` to the data frame by converting the `week_start` column to year-month format using the `as.yearmon` function.
#         - The `as.yearmon` function converts the given date to a year-month format for easier filtering.
# 
# 3. **Filtering Data**:
#     - `|> filter(year_month >= as.yearmon(half_year_ago))`:
#         - The pipe operator (`|>`) passes the data frame from the previous step to the `filter` function.
#         - The function filters the data frame to keep only entries where the `year_month` value is greater than or equal to the value of `as.yearmon(half_year_ago)`.
#         - This means that only entries from the last six months (or half a year) from the current date will be kept.
# 
# In summary, this code transforms the data frame by adding a `year_month` column using the `week_start` column, then filters the data to keep only entries from the last six months from a given check period. The processed data is stored in the variable `compl_clean_w_permit_exp_last_half_year`.
compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp__not_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  filter(year_month >= as.yearmon(half_year_ago))

# check, optional
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

# check, optional
dim(compl_clean_w_permit_exp_last_half_year__sa)

## fewer columns ----
# create a variable with column names to remove
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

# check, optional
dim(compl_clean_w_permit_exp_last_half_year__sa__short)

# Work with the whole period
## add compliant_after_overr ----
# ===
# Combine compliant and override values
# Explanations:
# 1. Create a new variable 'res' to store the result.
# 2. Use 'rowwise' to perform operations row by row.
# 3. Use 'mutate' to create a new column 'compliant_after_override' based on conditions specified in 'case_when'.
#    - If 'is_comp' is 0 and 'overridden' is 0, set 'compliant_after_override' to "no".
#    - If 'is_comp' is 1 or 'overridden' is 1, set 'compliant_after_override' to "yes".
#    - If 'is_comp' is NA, set 'compliant_after_override' to NA.
#    - For all other cases, set 'compliant_after_override' to the string representation of 'is_comp'.
# 4. Use 'ungroup' to remove grouping from the data frame.

add_compliant_after_override <-
  function(my_compl_df,
           overridden_col_name = "overridden",
           compliance_col_name = "is_comp") {
  # browser()
  res <-
    my_compl_df |>
    rowwise() |>
    mutate(
      compliant_after_override =
        case_when(
          !!sym(compliance_col_name) %in% c(0, "NO") &
            !!sym(overridden_col_name) %in% c(0, "NO")  ~ "no",
          !!sym(compliance_col_name) %in% c(1, "YES") ~ "yes",
          !!sym(overridden_col_name) %in% c(1, "YES") ~ "yes",
          is.na(!!sym(compliance_col_name)) ~ NA,
          .default = toString(!!sym(compliance_col_name))
        )
    ) |>
    ungroup()

  return(res)
}

# See the explanations for add_compliant_after_override() above (F2)
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr <-
  compl_clean_w_permit_exp_last_half_year__sa__short |>
  add_compliant_after_override(overridden_col_name = "overridden_",
                               compliance_col_name = "compliant_")

# check, optional
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |> 
  select(compliant_, overridden_, compliant_after_override) |>
  count(compliant_, overridden_, compliant_after_override)
#   compliant_ overridden_ compliant_after_override     n
#   <chr>      <chr>       <chr>                    <int>
# 1 NO         NO          no                       11258
# 2 NO         YES         yes                         70
# 3 YES        NO          yes                      29628

# check, optional
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$compliant_after_override |> 
  unique()
# [1] "yes" "no" 

# check, optional
dim(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr)

# check, optional
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

# check, optional
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)

## keep only vessels with info for all weeks in the period ----
# get the total number weeks in the period
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

# check, optional
compl_clean_w_permit_exp_last_half_year__sa_non_c |> dim()
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present)

## check the last report date ----
### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present |>
  select(vessel_official_number) |>
  distinct()

# check, optional
dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids)

### check these ids in the full compliance information ----
# optional
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

# check, optional
n_distinct(corresp_contact_cnts_clean$vesselofficial_number)

# New requirements
# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

## new requirement 2024-02-26 ----
# Michelle
# It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# check, optional
corresp_contact_cnts_clean |>
  select(calltype, voicemail, contacttype) |>
  distinct() |> head(10)

# Create filters to be used later on for filtering data frames based on the specified criteria.

# Explanations:
#   Quotation:
# 
# we_called_filter <- quo(any(...)):
# The quo function creates a quoted expression (a lazy evaluation) that captures the provided code.
# we_called_filter is the variable where this quoted expression is stored.
# any() function returns TRUE if any element in the provided condition is true; otherwise, it returns FALSE.


# 1)
# This code defines a filtering condition (we_called_filter) using the quo function. The condition checks whether any row in a data frame has contacttype equal to "call" (case-insensitive) and calltype equal to "outgoing" (also case-insensitive).
we_called_filter <-
  quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))

# 2)
# This code defines a filtering condition (we_emailed_once_filter) using the quo function. The condition checks whether any row in a data frame has a contacttype equal to "email" or "other" (both case-insensitive) and a calltype equal to "outgoing" (case-insensitive). 
we_emailed_once_filter <-
  quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outgoing"
  ))

# 3)
# don't need a second contact
# This code defines a filtering condition (they_contacted_direct_filter) using the quo function. The condition checks whether any row in a data frame has a calltype equal to "incoming" (case-insensitive). 
they_contacted_direct_filter <-
  quo(any(tolower(calltype) == "incoming"))

# use the filters
# Explanations:
#     - The filtering operation contains multiple conditions combined with the logical OR (`|`) operator:
#         - `tolower(calltype) == "incoming"`:
#             - This condition filters rows where the `calltype` column (converted to lowercase using `tolower()`) is equal to `"incoming"`.
#         - `(contact_freq > 1 & (!!we_called_filter & !!we_emailed_once_filter))`:
#             - This condition is evaluated if `contact_freq` is greater than 1, and it combines two quoted expressions (`we_called_filter` and `we_emailed_once_filter`).
#             - `we_called_filter` and `we_emailed_once_filter` are captured expressions (using `quo`), which are then unquoted (`!!`) and evaluated in the context of the data frame.
#             - If both `we_called_filter` and `we_emailed_once_filter` conditions are true, the filter will include rows where `contact_freq` is greater than 1.
# 
# In summary, the code snippet filters the data frame `corresp_contact_cnts_clean` based on the specified conditions. It retains rows where the `calltype` is "incoming" or rows that satisfy the combined conditions of `contact_freq > 1`, `we_called_filter`, and `we_emailed_once_filter`.
corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  filter(tolower(calltype) == "incoming" |
           (
             contact_freq > 1 &
               (!!we_called_filter &
                  !!we_emailed_once_filter)
           ))

# check, optional
dim(corresp_contact_cnts_clean)
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)

## fix dates ----
# check, optional
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
      parse_date_time(created_on,
                      c("mdY R")),
    contact_date_dttm =
      parse_date_time(contact_date,
                      c("mdY R"))
  )

# check, optional
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

# check, optional
dim(compl_corr_to_investigation)
n_distinct(compl_corr_to_investigation$vesselofficial_number)

# Results: Compl & corresondence together are in
# compl_corr_to_investigation

# ---- output needed investigation ----
# 1. remove unused columns
# 2. create additional columns
# 3. mark vessels already in the know list (prev_result)
# 4. duals vs. sa_only

## 1. remove extra columns ----
# create a variable with column names to remove
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
# 3. Apply the custom function 'concat_unique' to all columns to concatenate unique non-missing values into a single string. (See the function definition by clicking F2 on its name.)
# 4. Remove the grouping from the data frame.

compl_corr_to_investigation_short <-
  compl_corr_to_investigation |>
  select(-any_of(unused_fields)) |>
  group_by(vessel_official_number) |>
  summarise_all(concat_unique) |>
  ungroup()

## 2. create additional columns ----
### add list of contact dates and contact type in parentheses  -----

# put 3 names into vars (needed, because spaces and underscores placements vary from source to source)
# (See the function definition by clicking F2 on its name.)
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

# check, optional
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
  
# check, optional
compl_corr_to_investigation__corr_date |> 
  glimpse()

### add pims home port info ----
# Explanations:
# - This code snippet joins two data frames (`compl_corr_to_investigation__corr_date` and `processed_pims_home_ports`) based on a common column (`vessel_official_number`) and renames the joined columns.
# 
# 1. **Joining Data Frames**:
#     - `compl_corr_to_investigation__corr_date__hailing_port <- left_join(`:
#         - This line begins the assignment of the resulting data frame to `compl_corr_to_investigation__corr_date__hailing_port`.
#         - The function `left_join()` is used to join the two data frames.
#     - `compl_corr_to_investigation__corr_date, processed_pims_home_ports, join_by(vessel_official_number))`:
#         - The first argument is `compl_corr_to_investigation__corr_date`, the data frame on the left side of the join.
#         - The second argument is `processed_pims_home_ports`, the data frame on the right side of the join.
#         - The join condition is specified using `join_by(vessel_official_number)`, indicating the common column (`vessel_official_number`) on which the join will be performed.
# 
# 2. **Renaming Columns**:
#     - `|> rename("hailing_port_city" = city_fixed, "hailing_port_state" = state_fixed)`:
#         - This line uses the pipe operator (`|>`) to pass the joined data frame to the `rename()` function.
#         - The `rename()` function is used to change the column names in the resulting data frame.
#         - `"hailing_port_city"` is renamed from `city_fixed`.
#         - `"hailing_port_state"` is renamed from `state_fixed`.
# 
# In summary, this code snippet performs a left join between two data frames (`compl_corr_to_investigation__corr_date` and `processed_pims_home_ports`) based on a common column (`vessel_official_number`) and then renames the resulting columns to `"hailing_port_city"` and `"hailing_port_state"`.
compl_corr_to_investigation__corr_date__hailing_port <- 
  left_join(
    compl_corr_to_investigation__corr_date,
    processed_pims_home_ports,
    join_by(vessel_official_number)
  ) |> 
  rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)

### add prepared addresses ----

# Manually check missing addresses

#### From FHIER ----

##### fewer fields ----
# fhier_addresses are from get_data (For-hire Primary Physical Address List)

# fewer columns
fhier_addr_short <-
  fhier_addresses |>
  select(
    vessel_official_number,
    permit_holder_names,
    physical_address_1,
    physical_address_2,
    physical_city,
    physical_county,
    physical_state,
    physical_zip_code,
    phone_number,
    primary_email
  )

# Explanations:
# The purpose of the function is to clean character-type columns.
# 
# 1. **Input**:
#     - The function takes a dataframe called `my_df` as input.
# 
# 2. **Data Cleaning Operations**:
#     - The function uses the `mutate` function from the `dplyr` package to modify columns in `my_df`.
#     - `across(where(is.character), ...)`: This clause specifies that the following operations should be applied across all character-type columns in `my_df`.
#     - Each operation is explained below:
# 
#         - `str_squish(.x)`: This function trims leading and trailing whitespace and reduces multiple spaces between words to a single space.
#         - `replace_na(.x, "")`: This function replaces `NA` values with an empty string `""`.
#         - `str_replace_all(.x, ", ;", ";")`: This function replaces occurrences of ", ;" with ";".
#         - `str_replace_all(.x, "\\s+[,;]", ",")`: This function replaces any spaces followed by "," or ";" with a single comma.
#         - `str_replace_all(.x, ";,+", ";")`: This function replaces sequences of ";," with a single ";".
#         - `str_replace_all(.x, ";;+", ";")`: This function replaces consecutive ";" characters with a single ";".
#         - `str_replace_all(.x, ",,+", ",")`: This function replaces consecutive "," characters with a single ",".
#         - `str_replace_all(.x, "[,;] *\\bUN\\b *", "")`: This function removes occurrences of ", UN", "; UN", or "UN ;" from the strings.
#         - `str_replace_all(.x, "\\bUN\\b", "")`: This function removes standalone occurrences of "UN" from the strings.
#         - `str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")`: This function removes occurrences of "UN" surrounded by whitespace from the strings.
#         - `str_replace_all(.x, "^[,;] ", "")`: This function removes leading ", " or "; " from the strings.
#         - `str_replace_all(.x, "^[,;]$", "")`: This function removes trailing "," or ";" from the strings.
#         - `str_replace_all(.x, "[,;]$", "")`: This function removes trailing "," or ";" from the strings.
#         - The final operation is another `str_squish(.x)`, which trims any remaining whitespace and reduces multiple spaces between words to a single space.
# 
# 3. **Output**:
#     - The function returns a cleaned dataframe named `my_df_cleaned`.
#     - This dataframe contains the original data with character-type columns cleaned according to the specified operations.
# 
clean_names_and_addresses <- function(my_df) {

  my_df_cleaned <-
    my_df |>
    mutate(
      across(where(is.character),
             ~ str_squish(.x)),
      across(where(is.character),
             ~ replace_na(.x, "")),
      across(where(is.character),
             ~ str_replace_all(.x, ", ;", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s+[,;]", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, ";,+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ";;+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ",,+", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;] *\\bUN\\b *", "")),
      across(where(is.character),
                          ~ str_replace_all(.x, "\\bUN\\b", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;] ", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;]$", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;]$", "")),
      across(where(is.character),
             ~ str_squish(.x))
    )

  return(my_df_cleaned)
}

fhier_addr_short_clean <-
  fhier_addr_short |>
  clean_names_and_addresses() |> 
  distinct()

# nrow(fhier_addr_short_clean)

# don't combine address
# fhier_addr_short__comb_addr <- 
#   fhier_addr_short |> 
#   clean_names_and_addresses() |> 
#   mutate(
#     fhier_address =
#       str_glue(
#         "
#         {physical_address_1}, {physical_address_2}, {physical_city}, {physical_county}, {physical_state}, {physical_zip_code}
#       "
#       )
#   ) |>
#   select(
#     -c(
#       physical_address_1,
#       physical_address_2,
#       physical_city,
#       physical_county,
#       physical_state,
#       physical_zip_code
#     )
#   ) |>
#   clean_names_and_addresses() |> 
#   distinct()

# dim(fhier_addr_short__comb_addr)
# [1] 2390    5

# dim(fhier_addr_short_clean)

##### Add addresses from FHIER ----
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr <-
  left_join(compl_corr_to_investigation__corr_date__hailing_port,
            fhier_addr_short_clean)
# Joining with `by = join_by(vessel_official_number)`

# View(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)

# check if no name, phone or email
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  filter(
    is.na(contactrecipientname) |
      is.na(contactphone_number) |
      is.na(contactemailaddress)
  ) |> nrow()
# 0

##### vessels with no addresses ----
# This defines a vector called is_empty that contains various values that can represent an empty state or missing value.
# The vector contains different representations of empty or missing values:
# NA: This is a special value in R representing a missing or undefined value.
# "NA": This is the string representation of NA, which might appear in data.
# "": An empty string, often used to represent a missing value in character data.
# "UN": A string that might be used to represent an unknown value in data.
# "N/A": A string abbreviation for "Not Applicable," commonly used to represent a value that is not available or relevant.

is_empty <- c(NA, "NA", "", "UN", "N/A")

# Explanations:
# 1. **Filtering Dataset**:
#     - `compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |> filter(physical_address_1 %in% is_empty) |> select(vessel_official_number) |> distinct()`
#     - The pipe operator (`|>`) chains multiple data manipulation steps together.
#     - The first step applies the `filter()` function to `compl_corr_to_investigation__corr_date__hailing_port__fhier_addr`:
#         - This function filters rows based on a condition involving the `physical_address_1` column.
#         - The condition checks if `physical_address_1` is in the `is_empty` vector. This vector contains different representations of empty or missing values (`NA`, `"NA"`, `""`, `"UN"`, `"N/A"`).
#     - The next step uses `select()` to choose only the `vessel_official_number` column from the filtered data.
#     - The final step applies `distinct()` to remove duplicate values from the `vessel_official_number` column.
# 
# 2. **Result**:
#     - The filtered dataset `no_addr_vsl_ids` contains only unique vessel official numbers (`vessel_official_number`) from the input dataset where the `physical_address_1` field is considered empty or missing based on the `is_empty` vector.
no_addr_vsl_ids <- 
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |> 
  filter(physical_address_1 %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

# check, optional
n_distinct(no_addr_vsl_ids$vessel_official_number)
# 109
# 71

#### From Oracle db ----
# for vessels still without addresses get data from DB
db_participants_address__needed <-
  db_participants_address |>
  filter(official_number %in% no_addr_vsl_ids$vessel_official_number) |>
  distinct()

# check, optional
dim(db_participants_address__needed)
# [1] 139  37
n_distinct(db_participants_address__needed$official_number)
# 71

##### keep fewer columns ----
db_participants_address__needed_short <-
  db_participants_address__needed |>
  select(
    official_number,
    all_of(ends_with("entity_name")),
    all_of(ends_with("primary_email")),
    # all_of(ends_with("is_primary")),
    all_of(ends_with("ph_area")),
    all_of(ends_with("ph_number")),
    all_of(ends_with("entity_name")),
    all_of(ends_with("physical_city")),
    all_of(ends_with("physical_county")),
    all_of(ends_with("physical_state")),
    all_of(ends_with("physical_zip_code")),
    all_of(ends_with("mailing_address1")),
    all_of(ends_with("mailing_address2")),
    all_of(ends_with("mailing_city")),
    all_of(ends_with("mailing_county")),
    all_of(ends_with("mailing_country")),
    all_of(ends_with("mailing_state")),
    all_of(ends_with("mailing_zip_code"))
  ) |>
  distinct()

# check, optional
nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
# 199
n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr$vessel_official_number)
# 199
# one vessel per row

# check, optional
# if combine rows
dim(db_participants_address__needed_short)
# 106
n_distinct(db_participants_address__needed_short$official_number)
# 71

##### combine area and phone numbers ----
# Explanations:
# 1. **Adding New Columns**:
#     - `db_participants_address__needed_short |> mutate(erv_phone = paste0(erv_ph_area, erv_ph_number), erb_phone = paste0(erb_ph_area, erb_ph_number))`
#     - The pipe operator (`|>`) is used to chain the operation.
#     - The `mutate()` function creates two new columns in the dataset:
#         - `erv_phone`: This column is created by concatenating the `erv_ph_area` and `erv_ph_number` columns using the `paste0()` function. This function concatenates the two inputs without any separator, creating a phone number string.
#         - `erb_phone`: Similarly, this column is created by concatenating the `erb_ph_area` and `erb_ph_number` columns using `paste0()`.
#     - The newly created columns (`erv_phone` and `erb_phone`) are added to the `db_participants_address__needed_short` dataset.
# 
# 2. **Result**:
#     - The `db_participants_address__needed_short` dataset now includes the two additional columns, `erv_phone` and `erb_phone`, which contain concatenated phone number strings based on their respective area codes and phone numbers.
db_participants_address__needed_short__phone0 <- 
  db_participants_address__needed_short |> 
  mutate(erv_phone = paste0(erv_ph_area, erv_ph_number),
         erb_phone = paste0(erb_ph_area, erb_ph_number))

##### make erv and erb combinations ----
# list of column parts
col_part_names <-
  c(
    "entity_name",
    "primary_email",
    # "ph_is_primary",
    # "ph_area",
    # "ph_number",
    "physical_city",
    "physical_county",
    "physical_state",
    "physical_zip_code",
    "mailing_address1",
    "mailing_address2",
    "mailing_city",
    "mailing_county",
    # "mailing_country",
    "mailing_state",
    "mailing_zip_code"
  )

# combine columns into one

# Explanations:
# - The function `list_sort_uniq` is designed to take a list of strings as input, perform various transformations on the list, and return a sorted list with unique elements. 
# 
# 1. **Input**:
#     - The function accepts a single argument `my_lists`, which is expected to be a list of strings.
# 
# 2. **Data Processing**:
#     - The function performs the following operations on `my_lists`:
#         - `str_trim()`: This function trims leading and trailing whitespace from each string in the list.
#         - `unique()`: This function removes duplicate elements from the list, keeping only unique strings.
#         - `sort()`: This function sorts the list of unique strings in alphabetical order.
#         - `list()`: This function converts the sorted unique list back to a list data type.
#         - `flatten()`: This function flattens the list, converting it to a simple list of unique, sorted strings.
# 
# 3. **Output**:
#     - The function returns a list that contains unique and sorted strings from the input list `my_lists`.
# 
list_sort_uniq <- function(my_lists) {
  res <-
    my_lists |>
    str_trim() |>
    unique() |>
    sort() |>
    list() |>
    flatten()
  return(res)
}


# Explanations:
# 1. **Time Measurement**:
#     - `tic("map all pairs")`: This line starts a timer with the label "map all pairs." The timer will measure the execution time for the subsequent code block.
#     - `toc()`: This function ends the timer started by `tic()` and prints the elapsed time with the label "map all pairs."
# 
# 2. **Data Manipulation**:
#     - The code uses `map()` to iterate over the list of column parts (`col_part_names`) and apply a transformation function to each part.
#     - `col_part_names |>`:
#         - The pipe operator (`|>`) is used to pass the list of column parts (`col_part_names`) as input to the next function.
#     - `map(\(curr_col_part) { ... })`:
#         - The `map()` function applies the provided function to each element of `col_part_names`. The function is defined using a lambda function (`\(curr_col_part)`), where `curr_col_part` is the current element being processed.
#         - `new_col_name <- str_glue("db_{curr_col_part}")`: This line creates a new column name using `str_glue()` by prefixing "db_" to the current column part (`curr_col_part`).
#         - The subsequent code block performs data manipulation on `db_participants_address__needed_short__phone0`:
#             - `group_by(official_number)`: This function groups the data by the `official_number` column.
#             - `mutate(!!new_col_name := pmap(across(ends_with(curr_col_part)), ~ list_sort_uniq(.)), .keep = "none")`:
#                 - This line applies `mutate()` to add a new column (`new_col_name`) to the grouped data.
#                 - The column is created by applying `pmap()` across columns that end with the current column part (`curr_col_part`).
#                 - `pmap()` applies the function `list_sort_uniq(.)` to the selected columns and assigns the result to the new column (`new_col_name`).
#                 - The `.keep = "none"` argument specifies that only the new column (`new_col_name`) should be kept.
#             - `ungroup()`: This function removes grouping from the data.
#             - `select(-official_number)`: This function removes the `official_number` column from the dataset.
#         - The resulting dataset for each column part is collected in a list.
#     - `bind_cols(db_participants_address__needed_short__phone0, .)`:
#         - The `%>%` pipe operator is used to bind the original dataset (`db_participants_address__needed_short__phone0`) with the list of transformed datasets from `map()`.
#         - The result is a new dataset that combines the original data with the transformed data.
# 
# 3. **Result**:
#     - The final dataset, `db_participants_address__needed_short__erv_erb_combined3`, is created by combining the original data with the transformed data from the list of column parts.
# - The time taken to execute the code block is printed by `toc()`.
tic("map all pairs")
db_participants_address__needed_short__erv_erb_combined3 <-
  col_part_names |>
  map(\(curr_col_part)  {
    new_col_name <- str_glue("db_{curr_col_part}")
    # cat(new_col_name, sep = "\n")
    
    db_participants_address__needed_short__phone0 |>
      group_by(official_number) |>
      mutate(!!new_col_name := pmap(across(ends_with(curr_col_part)),
                                    ~ list_sort_uniq(.)),
             .keep = "none" ) |>
      ungroup() |>
      select(-official_number)
    
  }) %>%
  bind_cols(db_participants_address__needed_short__phone0, .)
toc()
# map all pairs: 14.31 sec elapsed

###### shorten ----
# keep only official_number and columns starting with "db_"
db_participants_address__needed_short__erv_erb_combined_short <-
  db_participants_address__needed_short__erv_erb_combined3 |>
  select(official_number,
         all_of(starts_with("db_"))) |> 
  distinct()

# check, optional
dim(db_participants_address__needed_short__erv_erb_combined_short)
# 94 17
n_distinct(db_participants_address__needed_short__erv_erb_combined_short$official_number)
# 71
db_participants_address__needed_short__erv_erb_combined_short |> 
  filter(official_number == "1235397") |>
  glimpse()
# $ official_number      <chr> "1235397", "1235397"
# $ db_entity_name       <list> ["DC SERVICE AND MAINTENANCE"], ["DAVID A RUBINO"]
# $ db_primary_email     <list> ["Acemechanicalcd@aol.com"], ["Acemechanicalcd@aol.…
# $ db_ph_is_primary     <list> ["1"], ["1"]
# $ db_physical_city     <list> ["SOUTH ISLANDIA"], ["ISLANDIA"]

##### combine similar fields ----

# Explanations:
# 1. Iterate over each participant column using 'col_part_names'.
#    - 'map' applies the provided function to each element of the list.
# 2. Define the old and new column names based on the current participant column.
#    - 'str_glue' is used for string interpolation to create column names.
# 3. Group the DataFrame by 'official_number' using 'group_by'.
# 4. For each group, create a new column with unique sorted values for the current participant.
#    - 'list_sort_uniq' ensures unique values and sorts them.
# 5. Ungroup the DataFrame and remove the 'official_number' column.
#    - 'ungroup' removes grouping structure.
#    - 'select' is used to exclude the 'official_number' column and keep only the new column.
# 6. Bind the resulting columns to 'db_participants_address__needed_short__erv_erb_combined_short'.
#    - 'bind_cols' combines columns horizontally.
# 7. Select only the 'official_number' and columns ending with '_u'.
# 8. Keep only distinct rows in the final DataFrame using 'distinct'.
# 9. The resulting DataFrame is stored in 'db_participants_address__needed_short__erv_erb_combined_short__u'.

db_participants_address__needed_short__erv_erb_combined_short__u <-
  col_part_names |>
  map(\(curr_col_part)  {
    # browser()
    old_col_name <- str_glue("db_{curr_col_part}")
    new_col_name <- str_glue("db_{curr_col_part}_u")
    cat(new_col_name, sep = "\n")
    
    db_participants_address__needed_short__erv_erb_combined_short |>
      group_by(official_number) |>
      mutate(!!new_col_name := list(paste(sort(unique(str_trim(flatten(!!sym(old_col_name))))))),
             .keep = "none" ) |>
      ungroup() |>
      select(-official_number)
  }) %>%
  bind_cols(db_participants_address__needed_short__erv_erb_combined_short, .) |> 
  select(official_number, all_of(ends_with("_u"))) |> 
  distinct()

# check, optional
# db_participants_address__needed_short__erv_erb_combined_short__u |>
  # filter(official_number == "1235397") |>
  # glimpse()

###### convert to characters ----
# Explanations:
# In the result dataset, list columns are transformed into single strings with elements separated by semicolons (`; `).

# **Transforming the Dataset**:
#     - `db_participants_address__needed_short__erv_erb_combined_short__u` is the input dataset.
#     - `rowwise()`:
#         - This function is used to apply operations to each row of the dataset independently.
#     - `mutate_if(is.list, ~ paste(unlist(.), collapse = '; '))`:
#         - `mutate_if()` applies the function to columns based on the provided predicate.
#         - The predicate `is.list` is used to select columns that contain list data.
#         - For each selected column, the lambda function (`~ paste(unlist(.), collapse = '; ')`) is applied:
#             - `unlist(.)` converts the list into a vector.
#             - `paste(..., collapse = '; ')` joins the elements of the vector into a single string, separating them with a semicolon (`; `).
#     - `ungroup()`:
#         - This function removes any grouping from the data.
db_participants_address__needed_short__erv_erb_combined_short__u_no_c <-
  db_participants_address__needed_short__erv_erb_combined_short__u |>
  rowwise() |>
  mutate_if(is.list, ~ paste(unlist(.), collapse = '; ')) |>
  ungroup()

# check, optional
# db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
#   filter(official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state_u     <chr> "NY"
# $ db_mailing_zip_code_u  <chr> "11749; 11749-5010"

##### rename fields ----
# Explanations:
# - This code snippet modifies column names, renaming columns by removing the suffix `_u` from each column name.
# 
# 1. **Transforming Column Names**:
#     - `db_participants_address__needed_short__erv_erb_combined_short__u_no_c` is the input dataset.
#     - `rename_with(~ stringr::str_replace(.x, pattern = "_u$", replacement = ""))`:
#         - `rename_with()` is a function that renames columns according to a given lambda function.
#         - The lambda function (`~ stringr::str_replace(.x, pattern = "_u$", replacement = "")`):
#             - Takes a column name (`.x`) as input.
#             - Uses `stringr::str_replace()` to replace occurrences of the pattern `_u$` in the column name with an empty string (`""`).
#             - The pattern `_u$` specifies that the function should look for a suffix `_u` at the end of each column name.
#     - As a result, the lambda function removes the `_u` suffix from each column name in the dataset.
# 
# 2. **Result**:
#     - The final dataset, `db_participants_address__needed_short__erv_erb_combined_short__u_ok`, is created with the modified column names.
#     - All column names in the dataset that ended with `_u` now have that suffix removed.
db_participants_address__needed_short__erv_erb_combined_short__u_ok <-
  db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
  rename_with( ~ str_replace(.x,
                             pattern = "_u$",
                             replacement = ""))

#### Join fhier and Oracle db addresses ----
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  left_join(
    db_participants_address__needed_short__erv_erb_combined_short__u_ok,
    join_by(vessel_official_number == official_number)
  )

# check, optional
# compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
#   filter(vessel_official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state       <chr> "NY"
# $ db_mailing_zip_code    <chr> "11749; 11749-5010"

# Print the result name to the console
cat("Result: ",
    "compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr",
    sep = "\n")

# result: compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr

## 3. mark vessels already in the know list ----
# Get vessel ids only from the previous results
vessels_to_mark_ids <-
  prev_result |>
  select(vessel_official_number)

# check, optional
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

### check, optional ----
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

# check, optional
n_distinct(compl_corr_to_investigation_short_dup_marked__permit_region$vessel_official_number)

### dual permitted cnts ----
# get percentage
region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100

# Print out results ----
## add additional columns in front ----

# create its name
additional_column_name1 <-
  str_glue(
    "Confirmed Egregious? (permits must still be active till {permit_expired_check_date}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call/other (voicemail counts) and at least 1 email)"
  )

# Explanations:
# - This code snippet modifies a dataset called `compl_corr_to_investigation_short_dup_marked__permit_region` by adding two new columns and creating a new dataset named `compl_corr_to_investigation_short_dup_marked__permit_region__add_columns`.
# 
# 1. **Adding Columns**:
#     - The input dataset is `compl_corr_to_investigation_short_dup_marked__permit_region`.
#     - The `add_column()` function is used to add new columns to the dataset.
#     - It takes three arguments:
#         - `!!(additional_column_name1) := NA`: This argument specifies a new column to add. The name of the column is provided by `additional_column_name1`, and its initial values are set to `NA`.
#         - `Notes = NA`: This argument specifies another new column named `Notes` with its initial values set to `NA`.
#         - `.before = 2`: This argument specifies the position at which the new columns should be added. The `2` indicates that the new columns should be added before the second column in the dataset.
# 
# 2. **Result**:
#     - The function adds two new columns to the dataset: one with the name provided by `additional_column_name1` and the other named `Notes`.
#     - Both columns are initialized with `NA` values.
#     - The new columns are added before the second column in the dataset, as specified by the `.before = 2` argument.

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  add_column(
    !!(additional_column_name1) := NA,
    Notes = NA,
    .before = 2
  )

# create the output file path
result_path <- 
  file.path(my_paths$outputs,
            current_project_name,
            str_glue("egregious_violators_to_investigate_{today()}.csv"))

# write a csv
compl_corr_to_investigation_short_dup_marked__permit_region__add_columns |>
write_csv(result_path)

# Print out the text to the console
cat("Result:",
    "compl_corr_to_investigation_short_dup_marked__permit_region__add_columns",
    "and",
    str_glue("egregious_violators_to_investigate_{today()}.csv"),
    sep = "\n")

