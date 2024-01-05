
#### add-ons 1 ---- 


# Non compliant vessels (2022) by home port
# identifying any particular areas of high non-compliance to help focus future outreach efforts.
# do this as a map
# Included overriden compliance in the total counts

# home port from the permit as an area

# source the usual setup
# get data
# remove not in metrics
# separate by permit region
# add home port

# Load the 'maps' and 'mapdata' libraries, which provide functionality for working with maps in R.
library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
# Load the 'leafpop' and 'leaflet' libraries, which are used for creating interactive maps in R.
library(leafpop)
library(leaflet)

library(viridis)

# The tidygeocoder package makes getting data from geocoder services easy.
# Check if the 'tidygeocoder' package is already installed; if not, install it and load the library.
if (!require(tidygeocoder)) {
  install.packages('tidygeocoder')  # Install 'tidygeocoder' package if not already installed.
  library(tidygeocoder)  # Load the 'tidygeocoder' library after installation.
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE



#### Current file: useful_functions_module.r ----

# nolint: commented_code_linter
# useful functions

##--- start functions ---
# How to use:
# my_paths <- set_work_dir()
# csv_names_list = list("report1.csv", "report2.csv")
# xls_names_list = list("report1a.xls", "report2a.xls")
# csv_content_1 <- load_csv_names(my_paths, csv_names_list)[[1]]
# xls_content_1 <- load_xls_names(my_paths, xls_names_list, sheet_num = 2)[[1]]

## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths, filenames = csv_names_list_22_23)
# compl_clean <- temp_var[[1]]
# corresp_contact_cnts_clean <- temp_var[[2]]

#---
# curr_wd <- getwd()
# roracle_path <- r"(C:\Users\anna.shipunova\Software\ROracle_1.3-2\ROracle)"
# setwd(roracle_path)
# install.packages('ROracle')

# library('ROracle')
# drv <- dbDriver("Oracle")
# con <- dbConnect(drv, "USER GOES HERE", "PASSWORD GOES HERE", dbname='XXX')

# library('ROracle')
# drv <- dbDriver("Oracle")
# con <-
#   dbConnect(drv, "USER GOES HERE", "PASSWORD GOES HERE", dbname = 'XXX')
#
# dbReadTable(con, 'DUAL')


#install.packages("tidyverse")
# Load the 'tidyverse' library, which is a collection of R packages for data manipulation and visualization.
library(tidyverse)

# Load the 'magrittr' library, which provides piping data and functions.
library(magrittr)

# Load the 'readxl' library, used for reading Excel (.xlsx) files.
library(readxl)

# Load the 'rbenchmark' library, which is used for benchmarking code performance.
library(rbenchmark)

# Load the 'ROracle' library, which provides an interface for working with Oracle databases in R.
library(ROracle)

# Load the 'tictoc' library, which allows measuring code execution time.
library(tictoc)

# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories in useful functions ----

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

# Define a function named 'load_csv_names' that takes two parameters: 'my_paths' and 'csv_names_list'
load_csv_names <- function(my_paths, csv_names_list) {

  # Extract the 'inputs' directory path from 'my_paths' and store it in 'my_inputs'
  my_inputs <- my_paths$inputs

  # Use 'lapply' to prepend the 'my_inputs' directory path to each file name in 'csv_names_list'
  # This creates a list of full file paths for the CSV files
  myfiles <- lapply(csv_names_list, function(x) file.path(my_inputs, x))

  # Use 'lapply' again to read all CSV files listed in 'myfiles'
  # The 'read_csv' function from the 'readr' package is used, specifying default column types as 'c' ('character')
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  # Return the contents of the CSV files as a list
  return(contents)
}

# Define a function named 'load_csv_names_in_one_df' that takes two parameters: 'path_to_files' and 'csv_names_list'
load_csv_names_in_one_df <- function(path_to_files, csv_names_list) {

    # Initialize 'myfiles' with 'csv_names_list'
    myfiles <- csv_names_list

    # Check if 'path_to_files' (input directory path) is provided
    if (length(path_to_files) > 0) {

        # If provided, use 'lapply' to prepend 'path_to_files' to each file name in 'csv_names_list'
        myfiles <- lapply(csv_names_list, function(x) file.path(path_to_files, x))
    }

    # Read all CSV files listed in 'myfiles' into a single data frame using 'map_df'
    csv_content <- purrr::map_df(myfiles, function(file_name) {

        # Use 'read_csv' from the 'readr' package to read each CSV file
        readr::read_csv(
            file_name,
            col_types = cols(.default = 'c'),  # Set default column type to 'character'
            trim_ws = TRUE,  # Trim whitespace from values
            na = c("", "NA", "NaN"),  # Treat empty strings, "NA," and "NaN" as NA values
            name_repair = "universal"  # Repair column names
        )
    })

    # Return the concatenated data frame containing all CSV file contents
    return(csv_content)
}

#
# Explanation:
#
# 1. The function `load_csv_names_in_one_df` takes two parameters: `path_to_files`, which is an optional input directory path, and `csv_names_list`, a list of CSV file names.
#
# 2. Initially, the `myfiles` variable is assigned the `csv_names_list`.
#
# 3. If `path_to_files` is provided (its length is greater than 0), the function uses `lapply` to prepend `path_to_files` to each file name in `csv_names_list`. This ensures that the full file paths are correctly constructed.
#
# 4. The `map_df` function is used to read all CSV files listed in `myfiles` and concatenate them into a single data frame (`csv_content`).
#
# 5. Within the `map_df` function, each CSV file is read using `read_csv` from the `readr` package. Various options are set, including the default column type as 'character', trimming whitespace, specifying NA values, and repairing column names.
#
# 6. Finally, the function returns the concatenated data frame containing the contents of all CSV files, making it easier to work with them as a single data structure.

# ===
# The function load_xls_names returns the concatenated data frame containing data from all Excel files. This allows you to work with the combined data more easily.
load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {

  # Extract the 'inputs' directory path from 'my_paths' and store it in 'my_inputs'
  my_inputs <- my_paths$inputs

  # Use 'lapply' to prepend 'my_inputs' directory path to each Excel file name in 'xls_names_list'
  myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))

  # Read Excel files listed in 'myfiles' into one data frame using 'map_df'
  contents <- purrr::map_df(myfiles, ~read_excel(
    .x,                           # File path
    sheet = sheet_n,              # Sheet number to read (default is 1)
    .name_repair = fix_names,     # Repair column names
    guess_max = 21474836,         # Maximum number of rows to guess data types
    col_types = "text"           # Specify all columns as 'text' type
  ))

  # Return the concatenated data frame containing data from all Excel files
  return(contents)
}

# The clean_headers function is designed to clean and fix the column names of a given dataframe (my_df).
clean_headers <- function(my_df) {
    # Use the 'fix_names' function to clean and fix the column names of the dataframe.
    colnames(my_df) %<>%
        fix_names()

    # Return the dataframe with cleaned and fixed column names.
    return(my_df)
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

## functions to clean FHIER compliance and correspondense reports ----

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
change_to_dates <- function(my_df, field_name, date_format) {
  # Convert the specified column ('field_name') in 'my_df' to POSIXct date format using 'as.POSIXct'
  # Within the mutate function, it uses pull to extract the column specified by 'field_name' and then applies as.POSIXct to convert the values in that column to POSIXct date format using the provided 'date_format'.
  result_df <- my_df %>%
    dplyr::mutate({
      {
        field_name
      }
    } := as.POSIXct(dplyr::pull(my_df[field_name]), format = date_format))

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

# ===
  # # Previously
  # across(a:b, mean, na.rm = TRUE)
  #
  # # Now
  # across(a:b, \(x) mean(x, na.rm = TRUE))
# change_fields_arr_to_dates <- function(my_df, field_names_arr, date_format) {
#   my_df %>%
#     mutate(across(all_of(field_names_arr), aux_fun_for_dates, date_format)) %>%
#
#     # mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
#                                         # format = date_format)) %>%
#     return()
# }

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

# Define a function 'concat_unique' to concatenate unique non-NA values from a vector x into a single character string.
concat_unique <- function(x) {
  # Use 'unique' to extract unique values, '!is.na(x)' to remove NA values, and 'collapse = ", "' to concatenate with a comma and space.
  # Finally, paste0 is used to concatenate the unique non-NA values with a comma and space separator (", ").
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
combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values <- function(my_df, group_by_arr) {
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
  add_path_corresp <- "Correspondence"
  add_path_compl <- "FHIER Compliance"

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
                    createdon_field_name,
                    "%m/%d/%Y %H:%M")
  corresp_arr_contact_cnts <-
    change_to_dates(corresp_arr_contact_cnts,
                    contactdate_field_name,
                    "%m/%d/%Y %I:%M %p")

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

  # Check if it is a single dataframe, and if not, combine separate dataframes for all years into one.
  if (length(compl_arr) > 1) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  # Find a column name containing 'permit', 'group', and 'expiration' (permitgroupexpiration).
  permitgroupexpiration <- grep("permit.*group.*expiration",
                                tolower(names(compl)),
                                value = TRUE)

  # Clean the 'week' column by splitting it into three columns with proper classes: 'week_num' (week order number), 'week_start', and 'week_end'.
  compl <- clean_weeks(compl)

  # Change the classes of dates in the 'permitgroupexpiration' columns from character to POSIXct.
  compl <- change_to_dates(compl, permitgroupexpiration, "%m/%d/%Y")

  # Return the cleaned and processed compliance data.
  return(compl)
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
#
# benchmarking to insert inside a function
# browser()
# time_for_appl <<- benchmark(replications=rep(10, 3),
                            # lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE),
                            # sapply(myfiles, read.csv, skipNul = TRUE, header = TRUE, simplify = TRUE)
                            # ,
                            # columns = c('test', 'elapsed', 'relative')
# )

# write.csv(time_for_appl, "time_for_appl.csv")

# or
# browser()
# sappl_exp <- function(){
#   sapply(my_df, function(x) length(unique(x))) %>% as.data.frame()
# }
#
# map_exp <- function(){
#   my_fun <- function(x) length(unique(x))
#   purrr::map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# purrr::map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% dplyr::group_by(test) %>% summarise(sum(elapsed))

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

# https://stackoverflow.com/questions/23986140/how-to-call-exists-without-quotation-marks
# usage: vexists(con_psql, bogus_variable_name)
# Define a function to check the existence of one or more variables in the current environment.
# This function takes a variable number of arguments using '...' notation.
vexists <- function(...) {
  # Use 'substitute' to capture the variable names from the arguments and convert them to character vectors.
  vars <- as.character(substitute(...()))

  # Use 'sapply' to iterate over the variable names and check if each variable exists in the current environment.
  exists_check <- sapply(vars, exists)

  # Return a logical vector indicating the existence of each variable.
  return(exists_check)
}

# ===
# make a separate legend for grid.arrange
legend_for_grid_arrange <- function(legend_plot) {
  # legend_plot <-
  #   ggplot(data = legend_data, aes(x1, y1, colour = ll)) +
  #   geom_text(dat = legend_data,
  #             aes(label = ll),
  #             hjust = 0) +
  #   scale_color_manual(
  #     name = 'Lines',
  #     breaks = c('Mean', 'Num of weeks'),
  #     values = my_colors
  #   )
  #
  # legend_plot

  # Obtain the legend from a 'legend_plot' using the 'get_legend' function from the 'cowplot' package.
  my_legend <-
    cowplot::get_legend(legend_plot)

  return(my_legend)
}

# ===
# Define a function to append the contents of a single file to an existing flat file.
write_to_1_flat_file <- function(flat_file_name, file_name_to_write) {
  # Redirect the output to the specified 'flat_file_name' and append content.
  sink(flat_file_name, append = TRUE)

  # Read the contents of the current file.
  current_file_text <- readr::read_lines(file_name_to_write)

  # Print a header indicating the current file being processed.
  cat("\n\n#### Current file:", basename(file_name_to_write), "----\n\n")

  # Print the contents of the current file, separating lines with newline characters.
  cat(current_file_text, sep = "\n")

  # # Restore the default output behavior.
  sink()
}

# Function to separate permit groups into three categories based on a specified field
separate_permits_into_3_groups <-
  function(my_df, permit_group_field_name = "permitgroup") {
    my_df %>%
      # Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group
      mutate(permit_sa_gom =
               dplyr::case_when(
                 # Check if 'permit_group_field_name' doesn't contain 'RCG', 'HRCG', 'CHG', or 'HCHG'; assign "sa_only" if true
                 !grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only",
                 # Check if 'permit_group_field_name' doesn't contain 'CDW', 'CHS', or 'SC'; assign "gom_only" if true
                 !grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only",
                 # For all other cases, assign "dual"
                 .default = "dual"
               )) %>%
      # Return the modified data frame
      return()
  }


# ===

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

# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

get_df_name_as_text <-
  function(my_df) {
    df_name = deparse(substitute(my_df))
    return(df_name)
  }

# # A title
# if (is.na(title_msg))  {
#   df_name = deparse(substitute(my_df))
#   title_msg <- df_name
# }

# to print the title message in blue.
title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg), sep = "\n")
}


# Define a helper function 'my_tee' to print the message to the console and a file.
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

# The read_rds_or_run function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run <- function(my_file_path,
                            my_data = as.data.frame(""),
                            my_function,
                            force_from_db = NULL) {

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
    }

    # Return the generated or read data.
    return(my_result)
}


# Usage:
# select(-all_of(names(empty_cols)))
# empty_cols <-
#   function(my_df) {
#     my_df |>
#       purrr::map_df(function(x) {
#         browser()
#         if (length(unique(x)) == 1) {
#           return(unique(x))
#         }
#       }) %>%
#     return()
#   }

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

remove_0_cols <- function(my_df) {
  browser()
  not_all_0 <- function(x)
  {
    any(!x == 0)
  }

  my_df |>
    select(where(not_all_0)) %>%
    return()
}


# ===
# Function to create a directory if it doesn't exist
create_dir_if_not <- function(curr_dir_name) {
  # Check if the directory does not exist
  if (!dir.exists(curr_dir_name)) {
    dir.create(curr_dir_name)  # Create the directory if it doesn't exist
  }
}

# ===
# sf functions ----
# ===
# convert to sf shortcut
# Function to convert a data frame to an sf object with specified coordinates and CRS
my_to_sf <- function(my_df, my_crs = sf::st_crs(sa_shp)) {
  my_df %>%
    sf::st_as_sf(
      # Specify the field names to use as coordinates
      coords = c("longitude", "latitude"),
      # Use the provided CRS (Coordinate Reference System), default to sa_shp's CRS
      crs = my_crs,
      # Keep the LATITUDE and LONGITUDE columns in the resulting sf object
      remove = FALSE
    ) %>%
    return()
}

# ===

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
# Disable the use of the S2 library for spherical geometry operations in the sf package
sf::sf_use_s2(FALSE)

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

# ===
# to use with toc(log = TRUE, quiet = TRUE)
# Define a function named print_toc_log that takes a 'variables' parameter
 # It's useful for monitoring and debugging code execution time when using the tic and toc functions to measure time intervals.
print_toc_log <- function(variables) {
  # Retrieve the log from the tic package with formatting enabled and store it in 'log.txt'
  log.txt <- tic.log(format = TRUE)

  # Write the lines of 'log.txt' to the console
  writeLines(unlist(log.txt))

  # Clear the log, removing its contents
  tic.clearlog()
}

# ===
save_plot_to_file <-
  function(file_full_name,
           plot_name) {
    ggplot2::ggsave(
      file_full_name,
      plot_name,
      width = 30,
      height = 20,
      units = "cm"
    )
  }

#### add-ons 2 ---- 


my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)



#### Current file: get_srhs_vessels.R ----

# get SRHS vessels to exclude ----
# The file is provided by Kenneth Brennan

# "C:\Users\anna.shipunova\Documents\Official documents\srhs_boats\2022_SRHS_Vessels_08_18_2023.xlsx"
file_name <- "2022_SRHS_Vessels_08_18_2023.xlsx"
srhs_vessels_2022 <-
  file.path(r"(~\Official documents\srhs_boats)",
            file_name)

if (!file.exists(srhs_vessels_2022)) {
  srhs_vessels_2022 <-
    file.path(my_paths$inputs,
              file_name)
}

srhs_vessels_2022_info <-
  read_excel(
  srhs_vessels_2022,
  # add the sheet name if needed and uncomment the next line
  # sheet = sheet_n,
  # use my fix_names function for col names
  .name_repair = fix_names,
  # if omitted, the algorithm uses only the few first lines and sometimes guesses it wrong
  guess_max = 21474836,
  # read all columns as text
  col_types = "text"
)


#### Current file: get_metrics_tracking.R ----

## fhier_reports_metrics_tracking ----

# The tidyverse is a collection of R packages that work together seamlessly for data manipulation, visualization, and analysis. It includes popular packages like dplyr, ggplot2, tidyr, and more, all designed to follow a consistent and "tidy" data processing philosophy.
library(tidyverse)

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


#### Current file: metric_tracking_no_srhs.R ----

get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

get_metrics_tracking_path <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_metrics_tracking.R")
# source(get_metrics_tracking_path)

get_srhs_vessels_path <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_srhs_vessels.R")
# source(get_srhs_vessels_path)

## exclude srhs vessels from metric traking ----
fhier_reports_metrics_tracking_not_srhs_ids <-
  # create a data frame
  purrr::map_df(
    fhier_reports_metrics_tracking_list,
    # for each df from the list
    ~ .x |>
      # exclude SRHS vessels
      dplyr::filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__)
  ) |>
  # keep only the vessel_official_numbers, remove all other columns
  dplyr::select(vessel_official_number) |>
  # remove duplicates
  dplyr::distinct()

dim(fhier_reports_metrics_tracking_not_srhs_ids)
# [1] 2981    1

# the same, but result kept in a list
# Create a list named 'fhier_reports_metrics_tracking_not_srhs_ids_list'
fhier_reports_metrics_tracking_not_srhs_ids_list <-
  purrr::map(
    fhier_reports_metrics_tracking_list,
    # Iterate over each data frame in this list
    ~ .x |>
      # Exclude SRHS vessels:
      # Filter rows where 'vessel_official_number' is not in 'uscg__' column of 'srhs_vessels_2022_info'
      filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__) |>
      # Select only the 'vessel_official_number' column
      select(vessel_official_number) |>
      # Remove duplicate values from the selected column
      dplyr::distinct()
  )


# check
# Use 'map' to apply the 'dim' function to each data frame in 'fhier_reports_metrics_tracking_list'
purrr::map(fhier_reports_metrics_tracking_list, dim)
# [[1]]
# [1] 3634   13
#
# [[2]]
# [1] 3460   13

purrr::map(fhier_reports_metrics_tracking_not_srhs_ids_list, dim)
# [[1]]
# [1] 3571    1
#
# [[2]]
# [1] 3399    1

# Keep all metrics tracking columns one df ----
fhier_reports_metrics_tracking_not_srhs_all_cols <-
  # create a data frame
  purrr::map_df(
    fhier_reports_metrics_tracking_list,
    # for each df from the list
    ~ .x |>
      # exclude SRHS vessels
      dplyr::filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__)
  ) |>
  # remove duplicates
  dplyr::distinct()

# Keep all metrics tracking columns lists by year ----
fhier_reports_metrics_tracking_not_srhs_all_cols_list <-
  # create a data frame
  purrr::map(
    fhier_reports_metrics_tracking_list,
    # for each df from the list
    ~ .x |>
      # exclude SRHS vessels
      dplyr::filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__)
  )
  
# View(fhier_reports_metrics_tracking_not_srhs_all_cols_list)


cat("Results:",
    "fhier_reports_metrics_tracking_not_srhs_ids_list",
    "fhier_reports_metrics_tracking_not_srhs_ids",
    "fhier_reports_metrics_tracking_not_srhs_all_cols",
    "fhier_reports_metrics_tracking_not_srhs_all_cols_list",
    sep = "\n")


#### Current file: get_db_data.R ----

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
# The read_rds_or_run function is designed to read data from an RDS file if it exists or run a specified function to generate the data if the file doesn't exist.
      # read a binary file saved previously
      # write all as binary
read_rds_or_run <- function(my_file_path,
                            my_data = as.data.frame(""),
                            my_function,
                            force_from_db = NULL) {

    # Check if the file specified by 'my_file_path' exists and 'force_from_db' is not set.
    if (file.exists(my_file_path) &
        is.null(force_from_db)) {
        # If the file exists and 'force_from_db' is not set, read the data from the RDS file.
        my_result <- readr::read_rds(my_file_path)
    } else {
        # If the file doesn't exist or 'force_from_db' is set, perform the following steps:
        # 1. Generate a message indicating the date and the purpose of the run.
        msg_text <- paste(today(), "run for", basename(my_file_path))
        tic(msg_text)  # Start timing the operation.

        # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result.
        my_result <- my_function(my_data)

        toc()  # Stop timing the operation.

        # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
        readr::write_rds(my_result,
                         my_file_path)
    }

    # Return the generated or read data.
    return(my_result)
}

# to use on download from db
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

# source("~/R_code_github/useful_functions_module.r")

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
  function() {
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
  function() {
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
  function() {
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
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND CURRENT_DATE
  )
ORDER BY
  trip_end_date DESC
"

trips_fun <- function(trips_query) {
  return(dbGetQuery(con,
             trips_query))
}

get_trips_info <-
  function() {
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
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND CURRENT_DATE
  )
"

trip_coord_file_name <-
    file.path(input_path, "trip_coord.rds")

trip_coord_fun <- function(trip_coord_query) {
  return(dbGetQuery(con,
             trip_coord_query))
}

get_trip_coord_info <-
  function() {
      read_rds_or_run(trip_coord_file_name,
                      trip_coord_query,
                      trip_coord_fun,
                      force_from_db)
  }

# 2023-09-20 run the function: 30.94 sec elapsed

# DNF reports
# get trip neg ----

trip_neg_2022_file_path <-
  file.path(input_path, "trip_neg_2022.rds")

trip_neg_2022_query <-
  "SELECT *
  FROM
    safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('01-JAN-23'
  , 'dd-mon-yy') )"

# 1495929

trip_neg_2022_fun <-
  function(trip_neg_2022_query) {
    return(dbGetQuery(con, trip_neg_2022_query))
  }

# trip_neg_query_2022: 201.21 sec elapsed
# trip_neg_query_2022: 60.06 sec elapsed
# trip_neg_query_2022: 89.38 sec elapsed

get_trip_neg_2022 <-
  function() {
    read_rds_or_run(trip_neg_2022_file_path,
                    trip_neg_2022_query,
                    trip_neg_2022_fun,
                    force_from_db)
  }
# run the function: 98.23 sec elapsed

# Declarations
# trips_notifications ----
trips_notifications_2022_query <-
  "SELECT
 *
FROM
  safis.trip_notifications@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('01-JAN-23'
  , 'dd-mon-yy') )
  OR ( trip_end_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('01-JAN-23'
  , 'dd-mon-yy') )
"

trips_notifications_2022_file_path <-
  file.path(input_path, "trips_notifications_2022.rds")

trips_notifications_2022_fun <-
  function(trips_notifications_2022_query) {
    return(dbGetQuery(con, trips_notifications_2022_query))
  }
# trips_notifications_query: 52.08 sec elapsed
# 97279
# trips_notifications_query: 7.65 sec elapsed

get_trips_notifications_2022 <-
  function() {
    read_rds_or_run(
      trips_notifications_2022_file_path,
      trips_notifications_2022_query,
      trips_notifications_2022_fun,
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
  function() {
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
# dim(trip_neg_2022)
# dim(trips_notifications_2022)
# dim(trips_info_2022)
# dim(vessels_all)

# dates_2022 ----
dates_2022_query <-
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

dates_2022_file_path <- file.path(input_path, "dates_2022.rds")

dates_2022_fun <-
  function(dates_2022_query) {
    return(dbGetQuery(con,
                      dates_2022_query))
  }

get_dates_2022 <- function() {
  read_rds_or_run(dates_2022_file_path,
                  dates_2022_query,
                  dates_2022_fun,
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

get_compl_err_db_data <- function() {
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
# source(get_metrics_tracking_path)

# or source separate files instead of the flat one:
# # source(file.path(my_paths$git_r,
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

run_all_get_db_data <-
  function() {
    # Initialize an empty list to store the results.
    result_l = list()

    # 1) Call the 'get_permit_info' function to retrieve permit information from the database.
    mv_sero_fh_permits_his <- get_permit_info()

    # 2) Store the retrieved data in the result list under the name "mv_sero_fh_permits_his."
    result_l[["mv_sero_fh_permits_his"]] <- mv_sero_fh_permits_his
    # dim(mv_sero_fh_permits_his)
    # [1] 183204     22

    # Repeat the steps 1 and 2 for all other types of data using the predefined functions.

    mv_safis_trip_download_data <- get_mv_safis_trip_download()
    result_l[["mv_safis_trip_download"]] <-
      mv_safis_trip_download_data
    # dim(mv_safis_trip_download_data)
    # [1] 735666    149

    mv_tms_trip_notifications_data <-
      get_mv_tms_trip_notifications()
    result_l[["mv_tms_trip_notifications"]] <-
      mv_tms_trip_notifications_data
    # dim(mv_tms_trip_notifications_data)
    # [1] 118730     41

    trips_info <- get_trips_info()
    result_l[["trips_info"]] <- trips_info
    # dim(trips_info)
    # [1] 98528    72 2022
    # [1] 142037   72 2021+

    trip_coord_info <- get_trip_coord_info()
    result_l[["trip_coord_info"]] <- trip_coord_info
    # dim(trip_coord_info)
    # [1] 141350     41

    trip_neg_2022 <- get_trip_neg_2022()
    result_l[["trip_neg_2022"]] <- trip_neg_2022
    # dim(trip_neg_2022)
    # Rows: 1,495,929
    # [1] 746087     12
    # [1] 747173     12

    trips_notifications_2022 <- get_trips_notifications_2022()
    result_l[["trips_notifications_2022"]] <-
      trips_notifications_2022
    # dim(trips_notifications_2022)
    # Rows: 129,701
    # [1] 70056    33

    vessels_permits <- get_vessels_permits()
    result_l[["vessels_permits"]] <- vessels_permits
    # dim(vessels_permits)
    # [1] 78438    51

    dates_2022 <- get_dates_2022()
    result_l[["dates_2022"]] <- dates_2022
    # dim(dates_2022)
    # 427 4

    compl_err_db_data <- get_compl_err_db_data()
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

# force_from_db <- "NULL"
# dates_2022 <- get_dates_2022()


# close the db connection ----
# Don't use from here, otherwise can't run the above functions from another file
# try(ROracle::dbDisconnect(con))



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

# don't need, see above
gom_council_states <-
  c("Florida",
    "Alabama",
    "Mississippi",
    "Louisiana",
    "Texas")

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

title_message_print(result_names)


#### Current file: non_compliant_areas_get_data.R ----

# today()
# [1] "2024-01-03"
# Dates for non compliant area map:
# vessel home port information: Jan 2, 2024 (vessels_permits.rds);
# compliance information: Dec 4, 2024 (compl_err_db_data_raw.rds);
# metrics tracking: Aug 23, 2023 (Detail_Report_12312021_12312022__08_23_2023.csv);
# SRHS vessels: Aug 18, 2023 (2022_SRHS_Vessels_08_18_2023.xlsx)

# Colored terminal output
library(crayon)

# get db data ----
db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")

# source(db_data_path)

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 8.86 sec elapsed

# prepare (non) compliant vessels 2022 info ----
compl_err_db_data <- 
  all_get_db_data_result_l$compl_err_db_data

## use metricks only vessels ----
metric_tracking_no_srhs_path <- 
  r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)"
# source(metric_tracking_no_srhs_path)

# fhier_reports_metrics_tracking_not_srhs_ids

# Keep only ids in fhier_reports_metrics_tracking_not_srhs_ids

fhier_reports_metrics_tracking_not_srhs_all_cols_2022 <-
  fhier_reports_metrics_tracking_not_srhs_all_cols_list$`2022`

# ---
# Explanations:
# The code uses the left_join() function to merge two data frames,
# 'fhier_reports_metrics_tracking_not_srhs_all_cols_2022' and 'compl_err_db_data'.
# The join is performed based on the equality of 'vessel_official_number' and
# 'vessel_official_nbr'. The result is stored in 'compl_err_db_data_metrics'.
# A left join retains all rows from the left data frame and adds matching rows
# from the right data frame. Unmatched rows in the right frame will have NAs
# in the corresponding columns in the result.

compl_err_db_data_metrics <-
  left_join(
    fhier_reports_metrics_tracking_not_srhs_all_cols_2022,
    compl_err_db_data,
    join_by(vessel_official_number == vessel_official_nbr)
  )

# fhier_reports_metrics_tracking_not_srhs_all_cols_2022 |> 
#   filter(permit_grouping_region == "GOM") |> 
#   select(vessel_official_number) |> 
#   distinct() |> 
#   dim()
# 1325

# compl_err_db_data_metrics |> 
#   filter(permit_grouping_region == "GOM") |> 
#   select(vessel_official_number) |> 
#   distinct() |> 
#   nrow()
# [1] 1325

dim(compl_err_db_data_metrics)
# [1] 408454     31

## 2022 only ---

# Explanations:
# The code uses the filter() function from the dplyr package to subset the
# 'compl_err_db_data_metrics' data frame based on date conditions:
# - Rows where 'comp_week_start_dt' is before '2023-01-01'.
# - Rows where 'comp_week_end_dt' is on or after '2022-01-01'.
# The result is stored in 'compl_err_db_data_metrics_2022'.
compl_err_db_data_metrics_2022 <-
  compl_err_db_data_metrics |>
  filter(comp_week_start_dt < '2023-01-01' &
           comp_week_end_dt >= '2022-01-01')

dim(compl_err_db_data_metrics_2022)
# [1] 145261     31

compl_err_db_data_metrics_2022 |>
  filter(permit_grouping_region == "GOM") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 1232

# 135+75+14+121+644
# 989

## Remove empty columns ---
compl_err_db_data_metrics_2022_clean <-
  compl_err_db_data_metrics_2022 |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(compl_err_db_data_metrics_2022_clean)
# [1] 145261     29

# compl_err_db_data_metrics_2022_clean |> View()

n_distinct(compl_err_db_data_metrics_2022_clean$vessel_official_number)
# 3473

## split into separate dfs by permit region in metrics tracking ----
# check
compl_err_db_data_metrics_2022_clean |>
  select(permit_grouping_region, sa_permits_, gom_permits_) |>
  distinct()
# A tibble: 3 × 3
#   permit_grouping_region sa_permits_ gom_permits_
#   <chr>                  <chr>       <chr>       
# 1 SA                     Y           N           
# 2 GOM                    N           Y           
# 3 GOM                    Y           Y           
# I.e. GOM == "gom and dual"

## split into separate dfs by permit region ----
# Explanations:
# The code uses the split() function to divide the data frame
# 'compl_err_db_data_metrics_2022_clean' into a list of data frames.
# The splitting criterion is the values in the 'permit_grouping_region' column.
# Each unique value in this column will correspond to a separate data frame
# in the resulting list, stored in 'compl_err_db_data_metrics_2022_clean_list'.
# This is useful for further analysis or processing on subsets of the data.

compl_err_db_data_metrics_2022_clean_list <- 
  compl_err_db_data_metrics_2022_clean |> 
  split(as.factor(compl_err_db_data_metrics_2022_clean$permit_grouping_region))

map(compl_err_db_data_metrics_2022_clean_list, dim)
# $GOM
# [1] 54765    29
# 
# $SA
# [1] 90496    29

## check vessel/compl counts ----
compl_err_db_data_metrics_2022_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      dplyr::distinct() |>
      dplyr::count(is_comp)
  })
# $GOM
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0   260
# 2       1  1232
# 
# $SA
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0  1236
# 2       1  1740

map(compl_err_db_data_metrics_2022_clean_list,
    \(reg_df) {
      n_distinct(reg_df$vessel_official_number)
    })
# $GOM
# [1] 1232
# 
# $SA
# [1] 2241

# Metrics:
# Total Vessels  3,539
# Total Vessels With SA Only  2,211
# Total Vessels With GOM Permit Only  1,028
# Total Dual (SA & GOM) Permitted Vessels  300

# if compliance is checked for only when permit is active add:
# comp_week_start_dt and comp_week_end_dt to select()

# if override is taken in the account, add it

## Remove columns not use in this analysis ----
compl_err_db_data_metrics_2022_clean_list_short <- 
  compl_err_db_data_metrics_2022_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      distinct()
  })

# prepare vessel_permit_data ----
## 2022 permits ----

# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_22' using the pipe
# operator and a series of dplyr functions:
# - Filtering: Rows are filtered based on conditions involving date columns
#   ('LAST_EXPIRATION_DATE', 'END_DATE', 'EXPIRATION_DATE', 'EFFECTIVE_DATE').
#   The filter conditions involve checking whether the dates are greater than
#   or less than "2021-12-31".
# - remove_empty_cols(): This custom function removes any empty columns from
#   the resulting data frame.
# The final result is a filtered and cleaned data frame containing relevant permit data.

vessels_permits_home_port_22 <-
  all_get_db_data_result_l$vessels_permits |>
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |> 
  dplyr::filter(EFFECTIVE_DATE < "2021-12-31") |> 
  remove_empty_cols()
  
## add permit region ----
# 
# This code creates a summarized data frame for vessels with permits in 2022 by grouping, summarizing, and separating permit types into three groups. Here's the breakdown of the comments:
# 
# 1. Creating a summarized data frame for vessels with permits in 2022.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_22` to the subsequent functions.
# 
# 3. Grouping the data by vessel official number using the `dplyr::group_by` function.
# 
# 4. Adding a new column named 'all_permits' with all unique permit types as a comma-separated string using the `dplyr::mutate` function.
# 
# 5. Separating permits into three groups using a custom function named `separate_permits_into_3_groups`, with the new permit group field specified as "all_permits".
# 
# 6. Ungrouping the data to revert to the original data frame structure using the `ungroup` function.

vessels_permits_home_port_22_reg <-
  vessels_permits_home_port_22 |>
  dplyr::group_by(SERO_OFFICIAL_NUMBER) |> 
  dplyr::mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") |>
  ungroup()

vessels_permits_home_port_22_reg |>
  select(permit_sa_gom) |> 
  distinct()
# 1 sa_only      
# 2 gom_only     
# 3 dual         

# vessels_permits_home_port_22_reg |> dim()
# [1] 36784    52

## shorten permit_vessel ----
vessels_permits_home_port_22_reg_short <-
  vessels_permits_home_port_22_reg |>
  dplyr::select(SERO_OFFICIAL_NUMBER,
                permit_sa_gom,
                dplyr::starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  dplyr::distinct()

# glimpse(vessels_permits_home_port_22_reg_short)
# [1] 4729    5

vessels_permits_home_port_short <-
  all_get_db_data_result_l$vessels_permits |>
  dplyr::select(SERO_OFFICIAL_NUMBER,
                dplyr::starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  dplyr::distinct()

# View(vessels_permits_home_port_short)

cat("Result to use for vessels home port and its permit region:",
"vessels_permits_home_port_22_reg_short",
"To use all data from the db:",
"vessels_permits_home_port_short",
sep = "\n")

# Map 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes. ----
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

### Load additional dictionaries ----
misc_info_path <- file.path(my_paths$git_r,
               r"(get_data\misc_info.R)")

# source(misc_info_path)

## Rows are retained if the 'NAME' column (state name) matches any of the values in 'south_east_coast_states'. ----
south_east_coast_states_shp <-
  us_s_shp |>
  filter(NAME %in% south_east_coast_states)

## get the bounding box ----
# Explanations:
# The code uses the st_bbox() function from the sf package to create a bounding box
# for the spatial object 'south_east_coast_states_shp'. A bounding box is a rectangular
# region defined by minimum and maximum coordinates along each axis (x and y).
# The resulting bounding box is stored in the variable 'south_east_coast_states_shp_bb'.
# This bounding box can be used for various spatial operations and analyses.

south_east_coast_states_shp_bb <- 
  sf::st_bbox(south_east_coast_states_shp)

# south_east_coast_states_shp_bb
#       xmin       ymin       xmax       ymax 
# -106.64565   24.52310  -75.46062   36.58812 

## save crs ----
tigris_crs <- sf::st_crs(south_east_coast_states_shp)
# User input: NAD83 
# ID["EPSG",4269]]

# Prepare home port coordinates ----
## Fix port addresses ----
# run once, gives vessels_permits_home_port_c_st_fixed

# fix_ports_file_path <-
#   file.path(my_paths$git_r,
#             current_project_basename,
#             "non_compliant_areas_fix_lat_lon.R")
# 
# # source(fix_ports_file_path)

# --- remove from here
# run once to get lat lon and check names with no coords

# add lat/lon ----

my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_basename,
            paste0(current_project_basename, "_no_county_all.rds"))

file.exists(my_file_path_lat_lon)

# ---
# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_short_trim_no_county'
# using the pipe operator and dplyr functions:
# - mutate(): Trims leading and trailing whitespaces from 'SERO_HOME_PORT_CITY'
#   and 'SERO_HOME_PORT_STATE' columns.
# - select(): Removes the 'SERO_HOME_PORT_COUNTY' column from the resulting data frame.
# The final result is a modified data frame with trimmed city and state columns
# and without the 'SERO_HOME_PORT_COUNTY' column.
vessels_permits_home_port_short_trim_no_county <-
  vessels_permits_home_port_short |>
  mutate(
    SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
    SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
  ) |>
  select(-SERO_HOME_PORT_COUNTY)

# ---
# Explanations:
# The code defines a custom R function 'get_lat_lon_no_county':
# - Takes a data frame 'my_df' as input.
# - Utilizes the tidygeocoder::geocode() function to geocode the city and state
#   information, obtaining latitude and longitude coordinates.
# - Includes 'return_input = TRUE' to return the input data frame along with geocoded results.
# - Returns the resulting data frame 'vessels_permits_home_port_lat_longs'.
# This function can be used to add geolocation information to the original data frame.

get_lat_lon_no_county <-
  function(my_df) {
    vessels_permits_home_port_lat_longs <-
      my_df |>
      tidygeocoder::geocode(city = "SERO_HOME_PORT_CITY",
                            state = "SERO_HOME_PORT_STATE",
                            return_input = TRUE)
    return(vessels_permits_home_port_lat_longs)
  }

vessels_permits_home_port_lat_longs_city_state <-
  read_rds_or_run(
    my_file_path_lat_lon,
    my_data =
      as.data.frame(vessels_permits_home_port_short_trim_no_county),
    get_lat_lon_no_county
  )

# vessels_permits_home_port_lat_longs_city_state |> 
#   filter(SERO_OFFICIAL_NUMBER %in% compl_vessl_not_in_ves_perm$vessel_official_number) |>
#   glimpse()

# View(vessels_permits_home_port_lat_longs_city_state)
# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_short$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 1

# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 1

# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_lat_longs_city_state$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 228

# intersect(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_lat_longs_city_state$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 0?
#   
# Passing 850 addresses to the Nominatim single address geocoder
# [==================================] 850/850 (100%) Elapsed: 15m Remaining:  0s
# 2024-01-02 run for non_compliant_areas_no_county_all.rds: 881.34 sec elapsed
# Saving new data into a file here: 
# C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/non_compliant_areas/non_compliant_areas_no_county_all.rds
# Warning message:
# In query_api(api_url, api_query_parameters, method = method) :
#   Internal Server Error (HTTP 500).

# Add back lost vessels ----
# check
vessels_permits_home_port_lat_longs_city_state_df <- 
  as.data.frame(vessels_permits_home_port_lat_longs_city_state)

setdiff(tolower(unique(
  vessels_permits_home_port_lat_longs_city_state_df$SERO_OFFICIAL_NUMBER)),
  tolower(unique(
vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER))) |> 
  length()
# 9

# View(vessels_permits_home_port_lat_longs_city_state_df)
setdiff(tolower(unique(vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER)),
        tolower(unique(vessels_permits_home_port_lat_longs_city_state_df$SERO_OFFICIAL_NUMBER))) |> 
  length()
# 92

str(vessels_permits_home_port_short_trim_no_county)
str(vessels_permits_home_port_lat_longs_city_state_df)

all_vessels_permits_home_port <-
  full_join(
    vessels_permits_home_port_lat_longs_city_state_df,
    vessels_permits_home_port_short_trim_no_county
  )
# Joining with `by = join_by(SERO_OFFICIAL_NUMBER, SERO_HOME_PORT_CITY,
# SERO_HOME_PORT_STATE)`

# all_vessels_permits_home_port |>
#   filter(SERO_OFFICIAL_NUMBER %in% no_state_vessels$SERO_OFFICIAL_NUMBER) |>
#   View()

dim(all_vessels_permits_home_port)
# [1] 6894    5
# data_overview(all_vessels_permits_home_port)
# SERO_OFFICIAL_NUMBER 6854
# SERO_HOME_PORT_CITY   840
# SERO_HOME_PORT_STATE   32
# lat                   695
# long                  695

# TODO: Use all_vessels_permits_home_port later to add states back if missing

# check home port typos by lat/lon ----

# Explanations:
# The code defines a custom R function 'check_home_port_typos_by_lat_lon':
# - Takes a list of data frames 'df_list_by_reg' as input.
# - Iterates over the names of the list using map() and a lambda function.
# - For each region's data frame:
#   - Filters rows with missing latitude or longitude.
#   - Selects relevant columns for further analysis.
#   - Removes duplicate rows based on selected columns.
#   - Trims leading and trailing whitespaces from city and state columns using mutate().
# - Names the resulting list with region names.
# - Returns the list 'compl_err_db_data_metrics_permit_reg_list_home_port_err'.
# This function is designed to check and clean data related to home ports with missing
# latitude or longitude coordinates within each region's data frame.

check_home_port_typos_by_lat_lon <-
  function(df_list_by_reg) {
    
    compl_err_db_data_metrics_permit_reg_list_home_port_err <-
      names(df_list_by_reg) |>
      map(\(curr_permit_reg_name) {
        browser()
        df_list_by_reg[[curr_permit_reg_name]] |>
          filter(is.na(long) |
                   is.na(lat)) |>
          select(vessel_official_nbr,
                 SERO_HOME_PORT_CITY,
                 # SERO_HOME_PORT_COUNTY,
                 SERO_HOME_PORT_STATE) |>
          distinct() |>
          mutate(
            SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
            # SERO_HOME_PORT_COUNTY = trimws(SERO_HOME_PORT_COUNTY),
            SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
          )
      })
    
    names(compl_err_db_data_metrics_permit_reg_list_home_port_err) <-
      names(df_list_by_reg)
    
    return(compl_err_db_data_metrics_permit_reg_list_home_port_err)
  }

# vessels_permits_home_port_lat_longs_city_state |>
#   filter(is.na(long) |
#            is.na(lat)) |>
#   select(SERO_OFFICIAL_NUMBER,
#          SERO_HOME_PORT_CITY,
#          SERO_HOME_PORT_STATE) |>
#   distinct() |>
#   mutate(
#     SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
#     SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
#   )
# Rows: 80

# View(compl_err_db_data_metrics_permit_reg_list_home_port_err)

# compl_err_db_data_metrics_permit_reg_list_home_port_err_county <- 
    # check_home_port_typos_by_lat_lon(compl_err_db_data_metrics_permit_reg_list)

  # check_home_port_typos_by_lat_lon(compl_err_db_data_metrics_permit_reg_list_home_port)

# Work with compl_err_db_data_metrics_permit_reg_list_home_port_err_county in excel ----

# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_lat_longs_city_state_err'
# using the pipe operator and dplyr functions:
# - filter(): Selects rows with missing latitude or longitude coordinates.
# - select(): Chooses relevant columns for further analysis.
# - distinct(): Removes duplicate rows based on the selected columns.
# The final result is a data frame containing rows with home ports that have
# missing latitude or longitude coordinates.

vessels_permits_home_port_lat_longs_city_state_err <-
  vessels_permits_home_port_lat_longs_city_state |>
  filter(is.na(long) |
           is.na(lat)) |>
  select(SERO_OFFICIAL_NUMBER,
         SERO_HOME_PORT_CITY,
         SERO_HOME_PORT_STATE) |>
  distinct()

dim(vessels_permits_home_port_lat_longs_city_state_err)
# [1] 80  3
# 126 3
# 128 3

vessels_permits_home_port_lat_longs_city_state_err_all <-
  vessels_permits_home_port_lat_longs_city_state |>
  select(SERO_HOME_PORT_CITY,
         SERO_HOME_PORT_STATE,
         lat,
         long) |>
  distinct()

vessels_permits_home_port_lat_longs_city_state_err_all |> 
  dim()
# [1] 648   4
# [1] 851   4

csv_file_path <-
  file.path(
    my_paths$outputs,
    current_project_basename,
    stringr::str_glue(
      "{current_project_basename}_vessels_permits_home_port_lat_longs_city_state_err_all1.csv"
    )
  )

file.exists(csv_file_path)   

# uncomment and run once
# vessels_permits_home_port_lat_longs_city_state_err_all |>
  # write_csv(file = csv_file_path)

# fix home port typos ----
# the list is created manuall from the csv
to_fix_list <- 
  list(
    c(
      "BAYOU LABATRE#AL",
      "BAYOU LA BATRE#AL"),
    c("CAROLINA BEACH#UN",
      "CAROLINA BEACH#NC"),
    c("CHALESTON#SC",
      "CHARLESTON#SC"),
    c("CHAUVIN, LA#LA",
      "CHAUVIN#LA"),
    c("FERNADINA BCH#FL",
      "FERNANDINA BEACH#FL"),
    c("FORT MORGAN MARINA#AL",
      "FORT MORGAN#AL"),
    c("GALLINANO#LA",
      "GALLIANO#LA"),
    c("GEORGRTOWN#SC",
      "GEORGETOWN#SC"),
    c("GULFSHORES#AL",
      "GULF SHORES#AL"),
    c("HILISBORO INLET#FL",
      "HILLSBORO INLET#FL"),
    c("HOMOASSA#FL",
      "HOMOSASSA#FL"),
    c("HOUMA LA#LA",
      "HOUMA#LA"),
    c("INTERCOASTAL CITY#LA",
      "INTRACOASTAL CITY#LA"),
    c("ISLAMORADA#UN",
      "ISLAMORADA#FL"),
    c("KEYWEST#FL",
      "KEY WEST#FL"),
    c("LITTLE RIVERNHV1N4WH#SC",
      "LITTLE RIVER#SC"),
    c("LOXLEY AL#AL",
      "LOXLEY#AL"),
    c("MADIERA BEACH#FL",
      "MADEIRA BEACH#FL"),
    c("MAYPPORT#FL",
      "MAYPORT#FL"),
    c("MCLELLANVILLE#SC",
      "MCCLELLANVILLE#SC"),
    c("MURELLS INLET#SC",
      "MURRELLS INLET#SC"),
    c("MURRELS INLET#SC",
      "MURRELLS INLET#SC"),
    c("NEW SMYMA BEACH#FL",
      "NEW SMYRNA BEACH#FL"),
    c("NEW SYMRNA BEACH#FL",
      "NEW SMYRNA BEACH#FL"),
    c("OCEEAN CITY#MD",
      "OCEAN CITY#MD"),
    c("POINT PLEASANT NJ#NJ",
      "POINT PLEASANT#NJ"),
    c("PORT CANVERAL#FL",
      "PORT CANAVERAL#FL"),
    c("PORT O CANNOR#TX",
      "PORT O CONNOR#TX"),
    c("PORT OCONNOR#TX",
      "PORT O'CONNOR#TX"),
    c("PORT ST.LUICE#FL",
      "PORT ST LUCIE#FL"),
    c("PUNTA GORGA#FL",
      "PUNTA GORDA#FL"),
    c("RIVERIA BEACH#FL",
      "RIVIERA BEACH#FL"),
    c("S PADRE ISLE#TX",
      "S. PADRE ISLAND#TX"),
    c("SEBASTAIN#FL",
      "SEBASTIAN#FL"),
    c("ST AUGUSTIN#FL",
      "ST AUGUSTINE#FL"),
    c("ST PETERSBURG BEACH#FL",
      "ST PETERSBURG#FL"),
    c("STEINAHTCHEE#FL",
      "STEINHATCHEE#FL"),
    c("SUMMRLND KEY#FL",
      "SUMMERLAND KEY#FL"),
    c("SWANQUARTER#FL",
      "SWAN QUARTER#NC"),
    c("TAVENIER#FL",
      "TAVERNIER#FL"),
    c("WANCHEESE#NC",
      "WANCHESE#NC"),
    c("ALEXANDER CITY, AL#AL",
      "ALEXANDER CITY#AL")
  )

# ---
# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_c_st' by using the pipe
# operator and the dplyr mutate() function:
# - mutate(): Adds a new column 'city_state' to the data frame.
# - paste(): Concatenates the trimmed values of 'SERO_HOME_PORT_CITY' and
#   'SERO_HOME_PORT_STATE' columns with '#' as a separator.
# The result is a modified data frame with an additional 'city_state' column
# containing concatenated city and state information.

vessels_permits_home_port_c_st <-
  vessels_permits_home_port_short |>
  mutate(city_state =
           paste(
             trimws(SERO_HOME_PORT_CITY),
             trimws(SERO_HOME_PORT_STATE),
             sep = "#"
           ))

all_vessels_permits_home_port_clean0 <- 
  all_vessels_permits_home_port |> 
    mutate(city_state = 
           paste(trimws(SERO_HOME_PORT_CITY), 
                 trimws(SERO_HOME_PORT_STATE), 
                 sep = "#")) 

# ---

# 1. **Column Extraction Using sapply:**
#    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
# 
# 2. **Column Extraction Using '[':**
#    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#    - This operation is used to extract a specific column or element from each list or data frame within 'to_fix_list'.
# 
# 3. **Final Result:**
#    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

# ---
# Explanations:
# The code defines a custom R function 'get_correct_addr_by_wrong':
# - Takes a 'wrong_addr' as input.
# - Uses grep() to find the index in 'to_fix_list' that contains the wrong address.
# - Uses tryCatch() to handle potential errors and print informative messages.
# - Extracts the corresponding pair of names from 'to_fix_list'.
# - Returns the correct address from the pair.
# This function is designed to find the correct address given a wrong address
# by searching for it in 'to_fix_list'.

get_correct_addr_by_wrong <-
  function(wrong_addr) {
    idx <- grep(wrong_addr, to_fix_list)
    
    names_pair <-
      tryCatch(
        to_fix_list[[idx]],
        error = function(e) {
          print(e)
          print(str_glue("Index: {idx}"))
        }
      )
    good_addr <- names_pair[[2]]
    
    return(good_addr)
  }

# ---
# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_c_st_fixed' using
# the pipe operator and dplyr functions:
# - rowwise(): Specifies that operations should be applied row by row.
# - mutate(): Adds a new column 'city_state_fixed':
#   - If 'city_state' is in 'wrong_port_addr', it is replaced with the correct
#     address using get_correct_addr_by_wrong(); otherwise, the original value is kept.
# - ungroup(): Removes the rowwise grouping.
# - tidyr::separate_wider_delim(): Splits 'city_state_fixed' into 'city_fixed' and
#   'state_fixed' columns using '#' as a separator.
# The resulting data frame has fixed and separated city and state columns.

# Have to use "if", because case_when will execute all the LHS and RHS, then keep based on conditions. So get_correct_addr_by_wrong is executed, one time by each row during the RHS evaluation and produces idx = 0 error. 

vessels_permits_home_port_c_st_fixed <-
  vessels_permits_home_port_c_st |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |>
  tidyr::separate_wider_delim(city_state_fixed,
                              delim = "#",
                              names = c("city_fixed",
                                        "state_fixed"))

dim(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8
# [1] 5029    8 with permit region
# [1] 6762    7 not processed db vessel_permits

## The same for the second df ----
all_vessels_permits_home_port_clean0_fixed <-
  all_vessels_permits_home_port_clean0 |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |> 
  tidyr::separate_wider_delim(city_state_fixed, 
                              delim = "#", 
                              names = c("city_fixed", "state_fixed"))

vessels_permits_home_port_c_st_fixed |> 
  filter(!SERO_HOME_PORT_CITY == city_fixed) |> 
  dim()
# [1] 49  7
# [1] 109   8 trimmed
# [1] 115   8 with permit region
# [1] 281   7

# Manually add Bokeelia is located in western Lee County at 26°41′17″N 82°8′43″W (26.687960, -82.145249).[5] It sits at the northern end of Pine Island and is bordered by water on three sides

# View(all_vessels_permits_home_port_clean0_fixed)
all_vessels_permits_home_port_clean0_fixed |> 
  filter(!SERO_HOME_PORT_CITY == city_fixed) |> 
  dim()
# [1] 60  8

dim(all_vessels_permits_home_port_clean0_fixed)
# [1] 6894    8

cat("Result in vessels_permits_home_port_c_st_fixed",
    "And in all_vessels_permits_home_port_clean0_fixed",
    sep = "\n")
dim(vessels_permits_home_port_c_st_fixed)
# [1] 6845    7

# --- remove to here
dim(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8
# [1] 6762    7

dim(all_vessels_permits_home_port_clean0_fixed)
# [1] 6894    8

### shorten fixed ----
vessels_permits_home_port_c_st_fixed_short <-
  all_vessels_permits_home_port_clean0_fixed |> 
  # vessels_permits_home_port_c_st_fixed |>
  select(SERO_OFFICIAL_NUMBER,
         # permit_sa_gom,
         city_fixed,
         state_fixed) |>
  distinct() |>
  remove_empty_cols()
  
# glimpse(vessels_permits_home_port_c_st_fixed)
# dim(vessels_permits_home_port_c_st_fixed_short)
# [1] 4729    4
# [1] 6762    3

## Add lat long to fixed ports ----

my_file_path_lat_lon <-
  file.path(
    my_paths$outputs,
    current_project_basename,
    paste0(
      current_project_basename,
      "_no_county_fixed_all_vessels.rds"
    )
  )

file.exists(my_file_path_lat_lon)

# Add lat and lon geo coordinates by city and state

# Explanations:
# The code defines a custom R function 'get_lat_lon_no_county':
# - It takes a data frame 'vessels_permits_home_port_c_st_fixed_short' as input.
# - Utilizes the tidygeocoder::geocode() function to geocode the city and state
#   information, obtaining latitude and longitude coordinates.
# - Returns the resulting data frame 'vessels_permits_home_port_c_st_fixed_lat_longs'.
# This function is used to add geolocation information to the original data frame.

get_lat_lon_no_county <-
  function(vessels_permits_home_port_c_st_fixed_short) {
    vessels_permits_home_port_c_st_fixed_lat_longs <-
      vessels_permits_home_port_c_st_fixed_short |>
      tidygeocoder::geocode(city = "city_fixed",
                            state = "state_fixed")
    return(vessels_permits_home_port_c_st_fixed_lat_longs)
  }

vessels_permits_home_port_lat_longs_city_state <-
  read_rds_or_run(
    my_file_path_lat_lon,
    my_data =
      as.data.frame(vessels_permits_home_port_c_st_fixed_short),
    get_lat_lon_no_county
  )
# 2023-11-27 run for non_compliant_areas_no_county_fixed.rds: 632.64 sec elapsed
# Passing 825 addresses to the Nominatim single address geocoder
# [==================================] 825/825 (100%) Elapsed: 14m Remaining:  0s
# 2024-01-02 run for non_compliant_areas_no_county_fixed_all_vessels.rds: 853.8 sec elapsed

dim(vessels_permits_home_port_lat_longs_city_state)
# [1] 5029    5
# [1] 4729    6
# [1] 6892    5

# data_overview(vessels_permits_home_port_lat_longs_city_state)
# SERO_OFFICIAL_NUMBER 4729
# permit_sa_gom           3
# city_fixed            592
# lat                   547

# today()
# [1] "2024-01-02"
# SERO_OFFICIAL_NUMBER 6854
# city_fixed            802
# state_fixed            32
# lat                   704
# long                  704

# print out get_data results ----

cat(
  blue("All DB data:"),
  "all_get_db_data_result_l",
  blue("compl 2022:"),
  "compl_err_db_data_metrics_2022_clean_list_short",
  blue("vessel_permit 2022 with lat/long:"),
  "vessels_permits_home_port_lat_longs_city_state",
  blue("Maps:"),
  "us_s_shp",
  sep = "\n"
)


#### Current file: non_compliant_areas.R ----

# Non compliant vessels (2022) by home port
# identifying any particular areas of high non-compliance to help focus future outreach efforts.
# do this as a map
# Included overriden compliance in the total counts

# home port from the permit as an area

# source the usual setup
# get data
# remove not in metrics
# separate by permit region
# add home port

# Load the 'maps' and 'mapdata' libraries, which provide functionality for working with maps in R.
library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
# Load the 'leafpop' and 'leaflet' libraries, which are used for creating interactive maps in R.
library(leafpop)
library(leaflet)

library(viridis)

# The tidygeocoder package makes getting data from geocoder services easy.
# Check if the 'tidygeocoder' package is already installed; if not, install it and load the library.
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")  # Install 'tidygeocoder' package if not already installed.
  library(tidygeocoder)  # Load the 'tidygeocoder' library after installation.
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# set up common functions and get data ----
# source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_path <- this.path::this.dir()

current_project_basename <- basename(current_project_dir_path)

# prepare data ----
get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_basename,
            "non_compliant_areas_get_data.R")

# source(get_data_file_path)

## prepare permit data ----
### Check how many vessels don't have home port info ----
# vessels_permits_home_port_lat_longs_city_state |> dim()
# [1] 4729    6

# ---
# Explanation:
# The code utilizes the pipe operator |> to perform a series of operations on
# the 'vessels_permits_home_port_lat_longs_city_state' data frame.
# - Filtering: Rows are filtered based on conditions related to the 'state_fixed' column.
# - Selection: Only the 'SERO_OFFICIAL_NUMBER' column is selected for further processing.
# - Deduplication: Duplicate rows are removed based on the selected column.
# - Sorting: The resulting data frame is arranged in ascending order by
#   'SERO_OFFICIAL_NUMBER'.
vessels_permits_home_port_lat_longs_city_state |>
  filter(state_fixed %in% c("NA", "UN") | is.na(state_fixed)) |>
  select(SERO_OFFICIAL_NUMBER) |>
  distinct() |>
  arrange(SERO_OFFICIAL_NUMBER)

all_vessels_permits_home_port_na_state <-
  all_vessels_permits_home_port |>
  filter(SERO_HOME_PORT_STATE %in% c("NA", "UN") |
           is.na(SERO_HOME_PORT_STATE)) |>
  select(SERO_OFFICIAL_NUMBER) |>
  distinct() |>
  arrange(SERO_OFFICIAL_NUMBER)
# 13

vessels_permits_home_port_short_trim_no_county |>
  filter(SERO_OFFICIAL_NUMBER %in% all_vessels_permits_home_port_na_state$SERO_OFFICIAL_NUMBER) |>
  arrange(SERO_HOME_PORT_CITY)
# |>
#   select(SERO_OFFICIAL_NUMBER) ->
#   no_state_vessels
# 2              FL9026LT            BOKEELIA                   FL
# 7                615565         SWANQUARTER                   NC

### check for duplicate vessels ----
vessels_permits_home_port_lat_longs_city_state |>
  dplyr::distinct() |>
  dplyr::group_by(SERO_OFFICIAL_NUMBER) %>%
  dplyr::filter(dplyr::n() > 1) |>
  dim()
# 76 5?
# TODO

### check how many coords have more than one vessel ----
vessels_permits_home_port_lat_longs_city_state |>
  dplyr::distinct() |>
#   group_by(permit_sa_gom, lat, long) %>%
# [1] 4393    6
  # Group the data by latitude and longitude, then filter for rows with more than one occurrence.
  dplyr::group_by(lat, long) %>%
  dplyr::filter(dplyr::n() > 1) |>
  # Return the dimensions (number of rows and columns) of the resulting data frame.
  dim()
# [1] 4505    6
  # count_uniq_by_column()
# SERO_OFFICIAL_NUMBER 4505
# city_fixed            376
# state_fixed            17
# lat                   323

# [1] 6578    5

## Compliance info combine dual and GOM ----
# Not needed if use Metrics tracking permits
# compl_err_db_data_metrics_2022_clean_list_short is sourced from non_compliant_areas_get_data.R

# Use the 'map' function from the 'purrr' package to apply the 'dim' function to each element
# of the list 'compl_err_db_data_metrics_2022_clean_list_short'.
purrr::map(compl_err_db_data_metrics_2022_clean_list_short, dim)
# $dual
# [1] 474   2
#
# $gom_only
# [1] 1114    2
#
# $sa_only
# [1] 2855    2
#
# $gom_dual
# [1] 1588    2

# $GOM
# [1] 1492    2
#
# $SA
# [1] 2976    2

# apply count_uniq_by_column() function to each df in the list
purrr::map(compl_err_db_data_metrics_2022_clean_list_short,
           count_uniq_by_column)
# $dual
# vessel_official_nbr 374
# is_comp               2
#
# $gom_only
# vessel_official_nbr 939
# is_comp               2
#
# $sa_only
# vessel_official_nbr 2135
# is_comp                2
#
# $gom_dual
# vessel_official_nbr 1313
# is_comp                2

# today()
# [1] "2023-12-29"
# $GOM
#                           .
# vessel_official_number 1232
# is_comp                   2
#
# $SA
#                           .
# vessel_official_number 2241
# is_comp                   2

## Compliance info, if a vessel is non compliant even once - it is non compliant the whole year, keep only unique vessel ids ----

# Explanations:
# The code creates a new list of data frames 'compl_err_db_data_metrics_2022_clean_list_short_year_nc'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in 'compl_err_db_data_metrics_2022_clean_list_short'.
# - For each data frame in the list:
#   - Group by 'vessel_official_number'.
#   - Add a new column 'non_compl_year' that checks if 0 is present in the 'is_comp' column.
#   - Ungroup the data frame.
# The resulting list contains modified data frames with added 'non_compl_year' column
# indicating whether the year is non-compliant for each 'vessel_official_number'.

compl_err_db_data_metrics_2022_clean_list_short_year_nc <-
  compl_err_db_data_metrics_2022_clean_list_short |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(vessel_official_number) |>
      dplyr::mutate(non_compl_year = 0 %in% is_comp) |>
      dplyr::ungroup()
  })

## check compl_year ----
compl_err_db_data_metrics_2022_clean_list_short_year_nc$SA |>
  # filter(vessel_official_number == 1020822) |>
  dplyr::arrange(vessel_official_number) |>
  head(5)
#   vessel_official_number is_comp non_compl_year
#   <chr>                 <int> <lgl>
# 1000164                   0 TRUE
# 1020057                   1 FALSE
# ...
# 1020822                   1 TRUE
# 1020822                   0 TRUE

# 1020822 is non compliant for the whole year

## keep unique vessel ids only ----

compl_err_db_data_metrics_2022_clean_list_short_uniq <-
  compl_err_db_data_metrics_2022_clean_list_short_year_nc |>
  purrr::map(\(curr_df){
    # Select all columns except 'is_comp' and retain only distinct rows.
    curr_df |>
      dplyr::select(-is_comp) |>
      dplyr::distinct()
  })

compl_err_db_data_metrics_2022_clean_list_short$GOM |>
  filter(!trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_permits_home_port_22$SERO_OFFICIAL_NUMBER
  ))) |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# 228
# !!! No vessel information for 228 vessels !!!

compl_vessl_not_in_ves_perm <-
  compl_err_db_data_metrics_2022_clean_list_short$GOM |>
  filter(!trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_permits_home_port_lat_longs_city_state$SERO_OFFICIAL_NUMBER
  ))) |>
  select(vessel_official_number) |>
  distinct()

nrow(compl_vessl_not_in_ves_perm)
# 228
# !!! No vessel information for 228 vessels !!!

# "1201160", "TX5996JU", "1305731", "FL8041ME"
# compl_err_db_data_metrics_2022_clean_list_short$GOM |>
#   filter(!trimws(tolower(vessel_official_number)) %in% trimws(tolower(
#     vessels_permits1$SERO_OFFICIAL_NUMBER
#   ))) |>
#   select(vessel_official_number) |>
#   distinct() |>
#   nrow()
# 1

compl_err_db_data_metrics_2022_clean_list_short$GOM |>
  filter(!trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    all_get_db_data_result_l$vessels_permits$SERO_OFFICIAL_NUMBER
  ))) |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# 1

# check
compl_err_db_data_metrics_2022_clean_list_short_uniq$SA |>
  dplyr::filter(vessel_official_number == 1020822)
#   vessel_official_number non_compl_year
# 1 1020822             TRUE

# Join home port and compliance info by vessel ----

# In summary, this code applies a left join operation to each data frame in the 'compl_err_db_data_metrics_2022_clean_list_short' list with another data frame, and the result is stored in a new list named 'vessels_permits_home_port_22_compliance_list'. The join is based on the equality of the columns 'vessel_official_number' and 'SERO_OFFICIAL_NUMBER'. The map function is used to apply this left join operation to each element of the list.

# Use only permit information from Metrics tracking

# Explanations:
# The code creates a new list of data frames 'vessels_permits_home_port_22_compliance_list'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in 'compl_err_db_data_metrics_2022_clean_list_short_uniq'.
# - For each data frame in the list:
#   - Perform a left join with 'vessels_permits_home_port_lat_longs_city_state'.
#   - Join based on the equality of 'vessel_official_number' and 'SERO_OFFICIAL_NUMBER'.
#   - Select all columns except 'permit_sa_gom'.
# The resulting list contains modified data frames with additional geolocation information,
# excluding the 'permit_sa_gom' column if it exists.

vessels_permits_home_port_22_compliance_list <-
  compl_err_db_data_metrics_2022_clean_list_short_uniq |>
  purrr::map(\(curr_df) {
    dplyr::left_join(
      curr_df,
      vessels_permits_home_port_lat_longs_city_state,
      dplyr::join_by(vessel_official_number == SERO_OFFICIAL_NUMBER)
    ) |>
      select(-any_of("permit_sa_gom"))
  })

purrr::map(vessels_permits_home_port_22_compliance_list,
           count_uniq_by_column)
# $dual
# vessel_official_number            374
# non_compl_year                   2

# $gom_only
# vessel_official_number            939
# non_compl_year                   2

# $sa_only
# vessel_official_number            2135
# non_compl_year                    2

# $gom_dual
# vessel_official_number            1313
# non_compl_year                    2

# today()
# [1] "2023-12-29"
# $GOM
#                           .
# vessel_official_number 1232
# non_compl_year            2
# city_fixed              188
# state_fixed              16
# lat                     165
# long                    165
#
# $SA
#                           .
# vessel_official_number 2241
# non_compl_year            2
# permit_sa_gom             3
# city_fixed              308
# state_fixed              22
# lat                     288
# long                    288

# Add missing home port states ----
# TODO:
# add the same for SA

## Check missing home port states ----

# Explanations:
# The code creates a new data frame 'gom_no_home_port_state_vessel_ids' using
# the pipe operator and dplyr functions:
# - filter(): Selects rows from the 'GOM' column where 'state_fixed' is missing or
#   is "NA" or "UN".
# - select(): Retains only the 'vessel_official_number' column.
# The resulting data frame contains vessel IDs with GOM permits and no valid home port state.

gom_no_home_port_state_vessel_ids <-
  vessels_permits_home_port_22_compliance_list$GOM |>
  filter(is.na(state_fixed) |
           state_fixed %in% c("NA", "UN")) |>
  select(vessel_official_number)

nrow(gom_no_home_port_state_vessel_ids)
# 230

# sa_no_home_port_state_vessel_ids <-
#   vessels_permits_home_port_22_compliance_list$SA |>
#   filter(is.na(state_fixed) |
#            state_fixed %in% c("NA", "UN")) |>
#   select(vessel_official_number)
# nrow(sa_no_home_port_state_vessel_ids)
# 464

# print_df_names(all_get_db_data_result_l$vessels_permits)

# Explanations:
# The code creates a new data frame 'missing_states_gom' using the pipe operator
# and dplyr functions:
# - select(): Extracts relevant columns related to permits from 'all_get_db_data_result_l$vessels_permits'.
#   - Starts with "SERO_", contains "DATE", and excludes specific columns.
# - distinct(): Removes duplicate rows based on all selected columns.
# - filter(): Retains rows where 'SERO_OFFICIAL_NUMBER' is in
#   'gom_no_home_port_state_vessel_ids$vessel_official_number'.
# The resulting data frame contains permit information for vessels with GOM permits
# and no valid home port state.

missing_states_gom <-
  all_get_db_data_result_l$vessels_permits |>
  select(starts_with("SERO_"),
         # REGISTERING_STATE, # sometimes diff from home port
         # STATE_CODE, # mostly empty
         contains("DATE")) |>
  distinct() |>
  filter(SERO_OFFICIAL_NUMBER %in%
           gom_no_home_port_state_vessel_ids$vessel_official_number)

dim(missing_states_gom)
# 470 13

# View(missing_states)
missing_states_gom_uniq <-
  missing_states_gom |>
  select(-contains("DATE")) |>
  distinct()

missing_states_gom_uniq |>
  group_by(SERO_OFFICIAL_NUMBER) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  ungroup() |>
  dim()
# 0 (One home port per vessel)

# dim(missing_states_gom_uniq)
# [1] 229   4

## join missing port states to the current df list ----

# Explanations:
# The code creates a new list of data frames 'vessels_permits_home_port_22_compliance_list_add_ports'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in 'vessels_permits_home_port_22_compliance_list'.
# - For each data frame in the list:
#   - Perform a left join with 'missing_states_gom_uniq'.
#   - Join based on the equality of 'vessel_official_number' and 'SERO_OFFICIAL_NUMBER'.
# The resulting list contains modified data frames with additional permit information
# from 'missing_states_gom_uniq'.

vessels_permits_home_port_22_compliance_list_add_ports <-
  vessels_permits_home_port_22_compliance_list |>
  purrr::map(\(curr_df) {
    curr_df |>
      left_join(
        missing_states_gom_uniq,
        join_by(vessel_official_number == SERO_OFFICIAL_NUMBER)
      )
  })

# vessels_permits_home_port_22_compliance_gom_all_ports <-
#   # vessels_permits_home_port_22_compliance_list$GOM |>
#   left_join(missing_states_gom_uniq,
#             join_by(vessel_official_number == SERO_OFFICIAL_NUMBER))

### combining ports to one column ----
# For now SA home_state is the same as state_fixed

# Explanations:
# The code creates a new list of data frames 'vessels_permits_home_port_22_compliance_list_add_ports_clean'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in 'vessels_permits_home_port_22_compliance_list_add_ports'.
# - For each data frame in the list:
#   - Use mutate() to add a new column 'home_state':
#     - If 'state_fixed' is missing or is "UN" or "NA", use 'SERO_HOME_PORT_STATE';
#       otherwise, use 'state_fixed'.
#   - Use select() to remove columns starting with "SERO_".
# The resulting list contains modified data frames with additional 'home_state' column,
# and columns starting with "SERO_" are removed.

vessels_permits_home_port_22_compliance_list_add_ports_clean <-
  vessels_permits_home_port_22_compliance_list_add_ports |>
  purrr::map(\(curr_df) {
    curr_df |>
      mutate(
        home_state = case_when(
          is.na(state_fixed) |
            state_fixed %in% c("UN", "NA") ~ SERO_HOME_PORT_STATE,
          .default = state_fixed
        )
      ) |>
      select(-starts_with("SERO_"))
  })

# check
vessels_permits_home_port_22_compliance_list_add_ports_clean$GOM |>
  filter(vessel_official_number %in% c("AL0264VE",
                                       "AL4295AK")) |>
  glimpse()
# $ vessel_official_number <chr> "AL0264VE", "AL4295AK"
# $ non_compl_year         <lgl> FALSE, FALSE
# $ city_fixed             <chr> "ORANGE BEACH", NA
# $ state_fixed            <chr> "AL", NA
# $ lat                    <dbl> 30.28284, NA
# $ long                   <dbl> -87.62461, NA
# $ home_state             <chr> "AL", "AL"

# Count vessels by state name ----
## total vsls per state ----

# Explanations:
# The code creates a new list of data frames 'vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in 'vessels_permits_home_port_22_compliance_list_add_ports_clean'.
# - For each data frame in the list:
#   - Group by 'home_state'.
#   - Add a new count column 'total_vsl_by_state_cnt' representing the total count per state.
#   - Ungroup the data frame.
# The resulting list contains modified data frames with additional count information
# indicating the total vessel count per home state.

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list_add_ports_clean |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(home_state) |>
      dplyr::add_count(home_state,
                name = "total_vsl_by_state_cnt") |>
      dplyr::ungroup()
  })

# View(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt)

### cnt by state proof of concept ----

#### select first, then count ----
# Create a new list 'vessel_by_state_cnt' by applying a series of operations to each element
# of the 'vessels_permits_home_port_22_compliance_list' by using map().
# Select only 'vessel_official_number' and 'home_state' columns, retain distinct rows,
# and add a count column for each state.

vessel_by_state_cnt <-
  vessels_permits_home_port_22_compliance_list_add_ports_clean |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, home_state) |>
      dplyr::distinct() |>
      dplyr::add_count(home_state)
  })

# View(vessel_by_state_cnt)

#### group_by, then count ----
# check if the result is the same with group_by()
# Group the data by 'home_state', add a count column, select specific columns,
# retain distinct rows based on those columns.

vessel_by_state_cnt1 <-
  vessels_permits_home_port_22_compliance_list_add_ports_clean |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(home_state) |>
      dplyr::add_count(home_state) |>
      dplyr::select(vessel_official_number, home_state, n) |>
      dplyr::distinct()
  })

diffdf::diffdf(vessel_by_state_cnt$SA,
               vessel_by_state_cnt1$SA)
# T
# no difference!

head(vessel_by_state_cnt$SA)
head(vessel_by_state_cnt1$SA)

# cnt vessel by state and compliance ----

# Explanations:
# The code creates a new list of data frames
# 'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in
#   'vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt'.
# - For each data frame in the list:
#   - Group by 'home_state' and 'non_compl_year'.
#   - Add a new count column 'compliance_by_state_cnt' representing the compliance
#     count per state and year.
#   - Ungroup the data frame.
# The resulting list contains modified data frames with additional count information
# indicating the compliance count per home state and year.

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(home_state, non_compl_year) |>
      dplyr::add_count(home_state, non_compl_year,
                       name = "compliance_by_state_cnt") |>
      dplyr::ungroup()
  })

## spot check if compl and non compl vessel number is equal total counts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$SA |>
  dplyr::select(vessel_official_number, home_state, non_compl_year, total_vsl_by_state_cnt, compliance_by_state_cnt) |>
  dplyr::distinct() |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  dplyr::filter(home_state %in% c("FL", "GA")) |>
  dplyr::glimpse()
# $ home_state             <chr> "FL", "FL", "GA", "GA"
# $ non_compl_year          <lgl> TRUE, FALSE, TRUE, FALSE
# $ total_vsl_by_state_cnt  <int> 896, 896, 38, 38
# $ compliance_by_state_cnt <int> 500, 396, 16, 22

# with all states
# $ home_state              <chr> "FL", "FL", "GA", "GA"
# $ non_compl_year          <lgl> TRUE, FALSE, FALSE, TRUE
# $ total_vsl_by_state_cnt  <int> 977, 977, 40, 40
# $ compliance_by_state_cnt <int> 541, 436, 22, 18

## test counts on one input df ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$SA |>
  dplyr::filter(home_state %in% c("FL", "GA")) |>
  dplyr::glimpse()

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$SA |>
  dplyr::select(vessel_official_number,
                home_state,
                non_compl_year,
                total_vsl_by_state_cnt) |>
  dplyr::distinct() |>
  dplyr::add_count(home_state, non_compl_year) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  dplyr::filter(home_state %in% c("FL", "GA")) |>
  dplyr::glimpse()
# the result is the same as above

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$GOM |>
  dplyr::select(vessel_official_number,
                state_fixed,
                # non_compl_year,
                total_vsl_by_state_cnt) |>
  dplyr::distinct() |>
  # filter(non_compl_year == FALSE) |>
  select(-vessel_official_number) |>
  dplyr::distinct() |>
  mutate(s = sum(total_vsl_by_state_cnt)) |>
  glimpse()
# 1232

# percent of non compliant by state ----

# Explanations:
# The code creates a new list of data frames
# 'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in
#   'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt'.
# - For each data frame in the list:
#   - Group by 'home_state'.
#   - Add new columns calculating non-compliance count, proportion, and percentage per state.
#   - Ungroup the data frame.
# The resulting list contains modified data frames with additional columns indicating
# non-compliance count, proportion, and percentage per home state.

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(home_state) |>
      dplyr::mutate(
        non_compliance_by_state_cnt =
          case_when(
            non_compl_year == TRUE ~
              compliance_by_state_cnt,
            non_compl_year == FALSE ~
              total_vsl_by_state_cnt -
              compliance_by_state_cnt
          ),
        non_compl_proportion_per_st =
          non_compliance_by_state_cnt /
          total_vsl_by_state_cnt,
        non_compl_percent_per_st =
          non_compl_proportion_per_st * 100
      ) |>
      dplyr::ungroup()
  })

## check perc cnts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc$GOM |>
  dplyr::select(
    home_state,
    non_compl_year,
    total_vsl_by_state_cnt,
    compliance_by_state_cnt,
    non_compliance_by_state_cnt,
    non_compl_percent_per_st,
    non_compl_proportion_per_st
  ) |>
  dplyr::distinct() |>
  dplyr::filter(home_state %in% c("FL", "GA")) |>
  dplyr::glimpse()
# $ home_state                 <chr> "FL", "FL", "GA"
# $ non_compl_year              <lgl> FALSE, TRUE, FALSE
# $ total_vsl_by_state_cnt      <int> 644, 644, 2
# $ compliance_by_state_cnt     <int> 500, 144, 2
# $ non_compliance_by_state_cnt <int> 144, 144, 0
# $ non_compl_percent_per_st    <dbl> 22.36025, 22.36025, 0.00000
# $ non_compl_proportion_per_st <dbl> 0.2236025, 0.2236025, 0.0000000

# with all states
# $ home_state                  <chr> "FL", "FL", "GA"
# $ non_compl_year              <lgl> FALSE, TRUE, FALSE
# $ total_vsl_by_state_cnt      <int> 782, 782, 3
# $ compliance_by_state_cnt     <int> 612, 170, 3
# $ non_compliance_by_state_cnt <int> 170, 170, 0
# $ non_compl_percent_per_st    <dbl> 21.73913, 21.73913, 0.00000
# $ non_compl_proportion_per_st <dbl> 0.2173913, 0.2173913, 0.0000000

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$GOM |>
  select(state_fixed, total_vsl_by_state_cnt) |>
  distinct() |>
  count(wt = total_vsl_by_state_cnt)
# 1232

# vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc$GOM |>
#     select(non_compl_year, state_fixed, compliance_by_state_cnt) |>
#     distinct() |>
#     count(wt = compliance_by_state_cnt)
# map percentage ----
# 1232

## shorten and add labels ----
# Explanations:
# The code creates a new list of data frames
# 'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in
#   'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc'.
# - For each data frame in the list:
#   - Select relevant columns including 'state_fixed', counts, and percentages.
#   - Remove duplicate rows.
#   - Add new columns with rounded percentages and proportions, and create labeled strings.
# The resulting list contains modified data frames with concise information
# about non-compliance percentages and proportions per home state.
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::select(
        state_fixed,
        home_state,
        total_vsl_by_state_cnt,
        non_compliance_by_state_cnt,
        non_compl_percent_per_st,
        non_compl_proportion_per_st
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        nc_round_perc = round(non_compl_percent_per_st),
        nc_round_proportion = round(non_compl_proportion_per_st, 2),
        my_label_perc =
          stringr::str_glue(
            "{home_state}:
                             {nc_round_perc}% of {total_vsl_by_state_cnt}"
          ),
        my_label_cnt =
          stringr::str_glue(
            "{home_state}:
                             {non_compliance_by_state_cnt} of {total_vsl_by_state_cnt}"
          ),
        my_label_long =
          stringr::str_glue(
            "{home_state}:
                             {non_compliance_by_state_cnt}/{total_vsl_by_state_cnt} = {nc_round_proportion}"
          )
      )
  })

### Check the counts ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels$GOM |>
  select(state_fixed, total_vsl_by_state_cnt) |>
    distinct() |>
    count(wt = total_vsl_by_state_cnt)
# 1226
# 1232 (incl compl)

### check the labels ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels$GOM |>
  glimpse()

# Keep only GOM states for GOM only plots ----

# Explanations:
# The code defines a function 'get_state_fixed_full_name' that takes a parameter
# 'state_fixed' and returns the corresponding full state name from 'my_state_name'
# using double brackets. 'tolower()' is used to ensure case-insensitive matching.

get_state_fixed_full_name <- function(state_fixed){
  my_state_name[[tolower(state_fixed)]]
}

# ---
# Explanations:
# The code modifies the 'gom_states' column in the data frame
# 'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels':
# - Create a new column 'state_fixed_full' using 'possibly()' to safely apply
#   'get_state_fixed_full_name' to 'state_fixed'.
# - Filter out rows where 'state_fixed_full' is not NA.
# - Filter out rows where the lowercase 'state_fixed_full' is in the lowercase 'gom' states.
# - Ungroup the data frame.

vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels$gom_states <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels$GOM |>
  rowwise() %>%
  mutate(state_fixed_full =
           possibly(get_state_fixed_full_name,
                    otherwise = NA)(state_fixed)) |>
  filter(!is.na(state_fixed_full)) |>
  filter(tolower(state_fixed_full) %in% tolower(east_coast_states$gom)) |>
  ungroup()

## add to the shape file by state name ----

# Explanations:
# The code creates a new list of spatial data frames 'shp_file_with_cnts_list'
# using the pipe operator and purrr::map() function:
# - purrr::map(): Iterates over each data frame in
#   'vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels'.
# - For each data frame in the list:
#   - Perform a left join with 'south_east_coast_states_shp'.
#   - Join based on the equality of 'STUSPS' and 'home_state'.
# The resulting list contains spatial data frames with additional compliance count information.

shp_file_with_cnts_list <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels |>
  purrr::map(\(curr_df) {
    # browser()
    south_east_coast_states_shp |>
      left_join(curr_df,
                join_by(STUSPS ==
                          home_state))
  })

# shp_file_with_cnts_list$gom_states |> View()
# if join to a df:
# tibble [8 × 15] (S3: tbl_df/tbl/data.frame)
# if join to an sf:
# Classes ‘sf’ and 'data.frame':	8 obs. of  15 variables

# view(shp_file_with_cnts_list)

### check on one region ----
shp_file_with_cnts_sa <-
  south_east_coast_states_shp |>
  dplyr::left_join(
    vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt_perc_short_labels$SA,
    dplyr::join_by(STUSPS ==
                     home_state)
  )

# View(shp_file_with_cnts_sa)

# print_df_names(shp_file_with_cnts_sa)
# [1] "STATEFP, STATENS, AFFGEOID, GEOID, STUSPS, NAME, LSAD, ALAND, AWATER, total_vsl_by_state_cnt, compliance_by_state_cnt, non_compl_percent_per_st, nc_round_perc, my_label, geometry"

# shp_file_with_cnts_sa |>
#   mapview(zcol = "nc_round_perc")
# View(shp_file_with_cnts)

# shp_file_with_cnts_list$SA |>
#   mapview(zcol = "nc_round_perc")

# get south states map ----
# to add empty states to the map

# Explanations:
# The code creates an sf object 'states_sf' representing U.S. states:
# - Use maps::map() to generate a map of U.S. states without plotting it.
# - Convert the resulting map to an sf object using sf::st_as_sf().

states_sf <-
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# add X and Y
# Explanations:
# The code adds coordinates of centroids to the 'states_sf' sf object:
# - Use sf::st_centroid() to compute the centroid of each state in 'states_sf'.
# - Use sf::st_coordinates() to extract the coordinates of centroids.
# - Use cbind() to add the coordinates to 'states_sf'.

states_sf <-
  cbind(states_sf,
        sf::st_coordinates(sf::st_centroid(states_sf)))

# mapview(states_sf)

# combine static map ----
# get boundaries from south_east_coast_states_shp_bb

base_size = 20
label_text_size <- 5
# axis_text_size <- 4
# title_text_size <- 4

# Explanations:
# The code creates a new data frame 'gom_state_proportion_indexes':
# - Start with 'shp_file_with_cnts_list$gom_states'.
# - Drop geometry information using sf::st_drop_geometry().
# - Select the column 'nc_round_proportion'.
# - Remove duplicate rows.
# - Drop rows with missing values.
# - Arrange the data frame based on 'nc_round_proportion'.

gom_state_proportion_indexes <-
  shp_file_with_cnts_list$gom_states |>
  sf::st_drop_geometry() |>
  select(nc_round_proportion) |>
  distinct() |>
  drop_na() |>
  arrange(nc_round_proportion)

len_colors_gom_states = nrow(gom_state_proportion_indexes)

# ---
# Explanations:
# The code defines a color palette 'mypalette' using the viridis package:
# - Use the viridis::viridis() function to generate a color palette.
# - Set the number of colors with 'len_colors_gom_states'.
# - Choose the "D" option for the color map.

mypalette = viridis(len_colors_gom_states, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette) <- gom_state_proportion_indexes$nc_round_proportion

# mypalette
#        0.14        0.16        0.21        0.22        0.29
# "#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"

# The code creates a plot using the ggplot2 library to visualize spatial data.

# Explanations:
# The code creates a new list of ggplot2 maps 'shp_file_with_cnts_list_maps':
# - Start with 'shp_file_with_cnts_list'.
# - Use purrr::map() to iterate over each sf object in the list.
# - For each sf object:
#   - Filter out rows with missing 'total_vsl_by_state_cnt'.
#   - Modify the sf object for mapping by adding a 'my_nudge_y' column.
#   - Create a ggplot2 map with specified aesthetics, labels, and themes.
#   - Set coordinate limits based on the bounding box of southeast coast states.
#   - Customize axis labels, legend, and color scale.
#   - Add the resulting ggplot2 map to the list 'shp_file_with_cnts_list_maps'.

shp_file_with_cnts_list_maps <-
  shp_file_with_cnts_list |>
  purrr::map(\(curr_sf) {
    curr_sf_for_map <-
      curr_sf |>
      filter(!is.na(total_vsl_by_state_cnt)) |>
      mutate(# my_nudge_x =
        #        ifelse(grepl("MS:", my_label_long), 1, 0) ,
        my_nudge_y =
          ifelse(grepl("MS:", my_label_long), 2, 0))
    
    curr_map <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = states_sf, fill = NA) +
      ggplot2::geom_sf(data = curr_sf_for_map,
                       aes(fill = factor(nc_round_proportion))) +
      ggplot2::geom_sf_label(
        data = curr_sf_for_map,
        aes(label = my_label_long),
        size = label_text_size,
        fill = "lightgrey",
        nudge_x = curr_sf_for_map$my_nudge_x,
        nudge_y = curr_sf_for_map$my_nudge_y
      ) +
      
      # Set the coordinate limits for the plot, based on the bounding box of southeast coast states,
      ggplot2::coord_sf(
        xlim =
          c(
            floor(south_east_coast_states_shp_bb$xmin),
            ceiling(south_east_coast_states_shp_bb$xmax)
          ),
        ylim =
          c(
            floor(south_east_coast_states_shp_bb$ymin),
            ceiling(south_east_coast_states_shp_bb$ymax)
          ),
        # with expand = FALSE to prevent expansion beyond the specified limits.
        expand = FALSE
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      scale_fill_manual(labels =
                          c("less", "", "", "", "more"),
                        values = mypalette) +
      theme_bw(base_size = 18) +
      # ggplot2::scale_fill_continuous(name = "",
      #                                # breaks = c(min(nc_round_perc), 'Num of weeks'),
      #                                                                  breaks = c("0.14", "0.29"),
      theme(legend.position = c(0.55, 0.1)) +
      guides(fill = guide_legend(title = "Non-Compliance Color Scale",
                                 nrow = 1))
  })

# individual plots ----

## make map titles ----
permit_regions <-
  c("SA only",
    "GOM and Dual",
    "Gulf"
    )

# Generate plot titles using 'str_glue' to include the 'permit_region'.
perc_plot_titles <-
  permit_regions |>
  purrr::map(\(permit_region) {
    stringr::str_glue(
      "Proportion of Non-compliant, {permit_region} Permitted SEFHIER Vessels by Homeport State"
    )
  })

# Set the names of the 'perc_plot_titles' list to be the same as the 'permit_regions'.
names(perc_plot_titles) <- permit_regions

## save plot to file function ----
# Explanations:
# The code defines a function 'write_png_to_file' to save a ggplot2 map to a PNG file:
# - Takes parameters 'output_file_name' and 'map_plot'.
# - Sets 'png_width'.
# - Uses ggplot2::ggsave() to save the ggplot2 map to a PNG file.
# - Specifies the file path, width, height, and units.

write_png_to_file <- function(output_file_name,
                              map_plot) {

  png_width  <- 31
  # png_height <- 25
  # png_width <- 800
  # png_height <- 600

  ggplot2::ggsave(
      file = output_file_name,
      plot = map_plot,
      device = "png",
      path = file.path(my_paths$outputs,
                       current_project_basename),
      width = png_width,
      # height = png_height,
      units = "cm" # "px"
    )
}

## GOM states ----
permit_region <- "Gulf"

gom_map <-
  shp_file_with_cnts_list_maps$gom_states +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

# gom_map

output_file_name <- str_glue("gom_states_non_compl_by_state_{today()}.png")

write_png_to_file(output_file_name,
                  gom_map)

## GOM and dual ----
permit_region <- "GOM and Dual"

gom_dual_map <-
  shp_file_with_cnts_list_maps$GOM +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <- str_glue("gom_dual_perc_by_state_{today()}.png")

write_png_to_file(output_file_name,
                  gom_dual_map)

## SA only ----
permit_region <- "SA only"
# TODO: make mypalette dynamic by states number and names

sa_only_map <-
  shp_file_with_cnts_list_maps$SA +
  ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <-
  str_glue("sa_only_perc_by_state_{today()}.png")

write_png_to_file(output_file_name,
                  sa_only_map)

# Check no home port vessels ----
## GOM ----
vessels_no_home_port <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$GOM |>
  filter(state_fixed %in% c("NA", "UN") | is.na(state_fixed)) |>
  select(vessel_official_number) |>
  distinct()

nrow(vessels_no_home_port)
# 230

vessels_permits_home_port_22 |>
  select(starts_with("SERO")) |>
  distinct() |>
  filter(trimws(tolower(SERO_OFFICIAL_NUMBER)) %in% trimws(tolower(vessels_no_home_port$vessel_official_number))) |>
  nrow()
# 2

all_get_db_data_result_l$vessels_permits |>
  select(starts_with("SERO")) |>
  distinct() |>
  filter(trimws(tolower(SERO_OFFICIAL_NUMBER)) %in% trimws(tolower(vessels_no_home_port$vessel_official_number))) |>
  glimpse()


compl_err_db_data_metrics_2022_clean_list_short$GOM |>
  filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_no_home_port$vessel_official_number
  ))) |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# 230

compl_err_db_data_metrics_2022_clean_list_short_uniq$GOM |>
  filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_no_home_port$vessel_official_number
  ))) |>
  nrow()
# 230

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$GOM |>
#   filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
#     vessels_no_home_port$vessel_official_number
#   ))) |>
#   View()
