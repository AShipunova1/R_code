#### add-ons 1 ---- 


library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
library(zoo)   # Load the 'zoo' library, which deals with time series data.
library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

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
  cat("\nCount unique values in each column:")

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

# read_rds_or_run <-
#   function(my_file_path,
#            my_data_list_of_dfs,
#            my_function) {
#     # browser()
#
#     if (file.exists(my_file_path)) {
#       # read a binary file saved previously
#       my_df <-
#         readr::read_rds(my_file_path)
#     } else {
#       tic("run the function")
#       my_df <-
#         my_function(my_data_list_of_dfs)
#       toc()
#
#       # write all as binary
#       readr::write_rds(my_df,
#                        my_file_path)
#     }
#
#     return(my_df)
#   }

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
  my_df |>
    # Select columns that do not meet the condition of being entirely NA or entirely NULL using 'select_if' function
    dplyr::select_if(function(x)
      # Check if all values in 'x' are not all NA or not all NULL
      !(all(is.na(x)) | all(is.null(x)))) %>%
    # Return the modified data frame
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



#### add-ons 2 ---- 


# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions definition.
my_paths <- set_work_dir()
# my_paths$inputs <-
#   r"(C:\Users\anna.shipunova\Downloads\input-20231108T141445Z-001\input)"
# my_paths$outputs <- 
#   r"(C:\Users\anna.shipunova\Downloads\input-20231108T141445Z-001\output)"


#### Current file: quantify_compliance_functions.R ----

# quantify_compliance_functions

# Create a list called 'text_sizes' that contains named elements with different font sizes.
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 15,
  axis_text_y_size = 15,
  plot_caption_text_size = 12,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# Define a function named 'get_non_compl_week_counts_percent' that accepts two arguments: 'my_df' (a data frame) and 'vessel_id_col_name' (a column name).
# 
# Start a pipeline to apply subsequent operations to the 'my_df' data frame.
# 
# Count the number of non-compliant weeks per vessel for each 'year_month' using 'count' from the dplyr package. The '!!sym()' function is used to interpret 'vessel_id_col_name' as a symbol.
# 
# Count the occurrence of unique combinations of 'year_month' and 'nc_weeks_per_vessl_m'.
# 
# Pivot the data to have a wide format, creating a column for each value of 'nc_weeks_per_vessl_m' in each 'year_month'.
# 
# Calculate the total number of non-compliant vessels per month by summing columns 2 to 6.
# 
# Reshape the data into a long format, with a row for each 'non_compl_weeks' and its count in a month.
# 
# Calculate the percentage of non-compliant vessels in a month, rounding the result to two decimal places.
# 
# Return the resulting data frame, which represents non-compliant week counts and percentages.
# 

get_non_compl_week_counts_percent <- function(my_df, vessel_id_col_name) {
  # browser()
    my_df %>%
    # Count the number of non-compliant weeks per vessel for each year_month.
    dplyr::count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # Count the occurrence of each unique combination of year_month and nc_weeks_per_vessl_m.
    dplyr::count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    tidyr::pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # Calculate the total number of non-compliant vessels per month.
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%

    # Reshape the data to have a row for each 'non_compl_weeks' and its count in a month.
    tidyr::pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # Calculate the percentage of non-compliant vessels in a month.
    mutate(percent_nc = round(
      100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month,
      digits = 2
    )) %>%
    return()
}

# Define a function called 'perc_plots_by_month' that takes two arguments:
# 1. 'my_df': A data frame.
# 2. 'current_year_month': The specific year_month for which to create a percentage plot.
perc_plots_by_month <-
  function(my_df, current_year_month) {
    # Uncomment the following line to enable debugging using the 'browser()' function.
    # browser()

    # Extract the total number of non-compliant vessels for the specified year_month.
    total_nc_vsl_per_month <-
      my_df %>%
      filter(year_month == current_year_month) %>%
      select(total_nc_vsl_per_month) %>%
      unique()

    # Create a title for the plot indicating the current year_month and total non-compliant vessels.
    month_title <-
      paste0(current_year_month,
             ": ",
             total_nc_vsl_per_month[[1]],
             " total nc vsls")
    
    my_df %>%
      # Filter the data frame to include only rows for the specified year_month.
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc)) +
      
      # Create a column plot with custom fill color.
      geom_col(fill = plot_colors$nc_bucket) +
      
      # Add text labels to the bars with the percentage values.
      geom_text(aes(label = paste0(percent_nc, "%")),
                position = position_dodge(width = 0.9)
                ) +
      
      # Customize the plot's title and axis title text sizes. 
      # Use the values from the 'text_sizes' list to specify the text sizes.
      theme(plot.title = text_sizes$plot_title_text_size,
            axis.title = text_sizes$axis_title_text_size
      ) +

      # Set the y-axis limits to be between 0 and 100.
      ylim(0, 100) +

      # Set plot labels, including the dynamic 'month_title'.
      labs(title = month_title,
           # x-axis label.
           x = "Num of nc weeks",  
           # y-axis label.
           y = "") %>%
      
      return()
  }

# Define a function called 'make_year_permit_label' that takes a single argument, 'curr_year_permit'.
# This function takes a string ('curr_year_permit') and performs a series of text transformations on it:
# It replaces the substring "_dual" with " + dual" using the 'stringr::str_replace' function.
# It replaces underscores ("_") with spaces using 'stringr::str_replace'.
# It converts the entire string to uppercase using 'toupper'.
# The resulting modified string is then returned by the function.

make_year_permit_label <- function(curr_year_permit) {
    curr_year_permit %>%
    
    # Replace "_dual" with " + dual".
    stringr::str_replace("_dual", " + dual") %>%
    
    # Replace underscores ("_") with spaces.
    stringr::str_replace("_", " ") %>%
    
    # Convert the entire string to uppercase.
    toupper() %>%
    
    # Return the resulting modified string.
    return()
}

# Define a function called 'make_one_plot_compl_vs_non_compl' that takes several arguments.
# This function generates a bar plot using ggplot2, where the bars represent compliance percentages. It has several optional parameters, allowing you to customize the appearance of the plot, including the title, color scheme, axis labels, and whether to display a legend. The function also offers the option to display percent labels on the bars.

# This function is designed to create a plot comparing compliant vs. non-compliant data. It takes various arguments for customization:
# 
# 'my_df': The data frame containing the data.
# 'current_title': The title for the plot.
# 'is_compliant': The column name for the 'is_compliant' data.
# 'percent': The column name for the percentage data.
# 'no_legend': A flag to control whether to display a legend.
# 'percent_label_pos': The position of the percentage labels.
# 'default_percent_labels': A flag to control the default display of percentage labels.
# 'geom_text_size': The font size for text labels on the plot.
# The function then creates a ggplot plot, customizes various plot elements, and adds percentage labels to the bars. It returns the resulting plot for further use.
# 

make_one_plot_compl_vs_non_compl <-
  function(my_df,
           current_title = "",
           is_compliant = "is_compliant",
           percent = "percent",
           no_legend = FALSE,
           percent_label_pos = 0.5,
           default_percent_labels = TRUE,
           geom_text_size = text_sizes[["geom_text_size"]]
  ) {
    
    one_plot <-
      my_df %>%
      # ggplot(): Initializes a new ggplot object.
      # aes(): Specifies the aesthetics (aesthetics mapping) for the plot.
      # x = !!sym(is_compliant): Maps the x - axis to a variable specified by the value of is_compliant. The !!sym() syntax is used to unquote is_compliant, allowing it to be evaluated within the context of the ggplot.
      # fill = !!sym(is_compliant): Maps the fill (color) aesthetic to the same variable as the x-axis, based on the compliance status. 
      # Again, !!sym() is used to unquote is_compliant.

      ggplot(aes(
        x = !!sym(is_compliant),
        y = !!sym(percent),
        fill = !!sym(is_compliant)
      )) +
      # Add a column/bar plot
      geom_col() +
      theme(
        axis.text.x =
          element_text(size = text_sizes[["axis_text_x_size"]]),
        axis.text.y =
          element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      # # Add percent numbers on the bars
      #     one_plot <-
      # one_plot + annotate("text", x = 4, y = 25, label = "Some text")
      #
      # geom_text(aes(label =
      #                 paste0(round(!!sym(percent), 1), "%")),
      #           # in the middle of the bar
      #           position =
      #             position_stack(vjust = percent_label_pos)
      #           ) +
      # no x and y titles for individual plots
    
    # Set the plot title and remove x and y axis labels
    labs(title = current_title,
         x = "",
         y = "") +
      
      # Define manual color fill scale
      scale_fill_manual(
        # use custom colors
        values =
          c("compliant" = plot_colors[["compliant"]],
            "non_compliant" = plot_colors[["non_compliant"]]),
        # Legend title
        name = "Is compliant?",
        # Legend labels
        labels = c("Yes", "No")
      ) +
      
      # Define manual x-axis tick labels
      scale_x_discrete(labels = c("Yes", "No")) +
      # scale_y_continuous(limits = c(0, 100), labels = scales::percent)
      # Set the y-axis limits between 0 and 100
      ylim(0, 100)
    # +
    # scale_y_continuous(labels = scales::label_percent(scale = 1))
    
    # Create a 'label_percent' vector by applying the rounding and '%' symbol to 'perc_c_nc' column
    label_percent <- purrr::map(my_df$perc_c_nc,
                         ~ paste0(round(.x, 1), "%"))
    
    # Conditionally add percent numbers to the bars based on 'default_percent_labels'
    if (default_percent_labels) {
      one_plot <-
        one_plot +
        geom_text(
          aes(label =
                paste0(round(!!sym(
                  percent
                ), 1), "%")),
          # in the middle of the bar
          position =
            position_stack(vjust = percent_label_pos),
          size = geom_text_size
        )
      
    } else {
      one_plot <-
        one_plot + annotate("text",
                            x = 1:2,
                            y = 20,
                            label = label_percent)
    }
    
    
    # Conditionally remove the legend from the plot based on 'no_legend'
    # to use with grid arrange multiple plots
    if (no_legend) {
      one_plot <- one_plot +
        theme(legend.position = "none")
    }
    
    # Return the 'one_plot' as the result of the function
    return(one_plot)
  }

# percent buckets
# Define a function named 'get_p_buckets' with two parameters 'my_df' and 'field_name'.
# Depending on the range in which the field_name value falls, the corresponding bucket label is assigned to each row in the percent_n_compl_rank column.
get_p_buckets <- function(my_df, field_name) {

  # Modify 'my_df' by adding a new column 'percent_n_compl_rank' using 'mutate' from dplyr
  # The column values are determined based on the 'field_name' value using 'case_when'
  my_df %>% 
    dplyr::mutate(
      # Create a new column 'percent_n_compl_rank'
      percent_n_compl_rank = 
        dplyr::case_when(
          # Use '!!sym(field_name)' to dynamically refer to 'field_name' within the context of 'my_df'
          # If 'field_name' < 25, set 'percent_n_compl_rank' to '0<= & <25%', etc.
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) & !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) & !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>% 
    # Return the modified data frame
    return()
}

# percent buckets by 50%
# Define a function named 'get_2_buckets' with two parameters 'my_df' and 'field_name'
get_2_buckets <- function(my_df, field_name) {
  # Modify 'my_df' by adding a new column 'percent_non_compl_2_buckets' using 'mutate' from dplyr
  my_df %>%
    # Use 'dplyr::mutate' to create or modify columns in 'my_df'
    dplyr::mutate(

      # Create a new column 'percent_non_compl_2_buckets'
      percent_non_compl_2_buckets =
        dplyr::case_when(
          # Use '!!sym(field_name)' to dynamically refer to 'field_name' within the context of 'my_df'
          # If 'field_name' < 50, set 'percent_non_compl_2_buckets' to '< 50%'       
          !!sym(field_name) < 50 ~ '< 50%',
          # If 'field_name' is greater than or equal to 50, set 'percent_non_compl_2_buckets' to '>= 50%'
          50 <= !!sym(field_name) ~ '>= 50%'
        )) %>%
    # Return the modified data frame
    return()
}

#### Current file: get_data.R ----

# this file is called from quantify_compliance.R

project_dir_name <- "FHIER Compliance"

# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT
# get data from csvs ----
get_data_from_FHIER_csvs <- function() {
  filenames = c(
     # Jenny's Oct 31 2023
    "FHIERCompliance_10_2023.csv",
    "FHIER_Compliance_2023__05_31_2023.csv"
  )

  ## ---- get csv data into variables ----
  csv_names_list <- prepare_csv_names(filenames)

  # View(csv_names_list)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
  # browser()
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
  compl_clean <- compliance_cleaning(csvs_clean1)

  return(compl_clean)
}

# Define a function named 'get_compliance_error_definitions' with no parameters
get_compliance_error_definitions <- function() {
  
  # Create a character vector 'err_desc_filenames' containing the file path
  err_desc_filenames = c(file.path(project_dir_name, "Compliance_Error_Types_03_29_2023.csv"))
  
  # Load the contents of the CSV file specified in 'err_desc_filenames' using 'load_csv_names' and 'my_paths'
  err_desc_csv_contents <- load_csv_names(my_paths, err_desc_filenames)
  
  # Clean the headers of the loaded CSV content using 'clean_headers'
  err_desc_clean_headers_csv_content <-
    clean_headers(err_desc_csv_contents[[1]])
  
  # Convert a specific column ("last_updated") to date format using 'change_to_dates'
  err_desc <-
    change_to_dates(err_desc_clean_headers_csv_content,
                    "last_updated",
                    "%m/%d/%Y %I:%M:%S %p")
  
  # Return the cleaned and processed data frame 'err_desc'
  return(err_desc)
}

get_permit_data_from_PIMS_csv <- function() {
  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"
  
  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  
  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()
  
  # separate columns
# Use the 'separate_wider_delim' function to split the 'permit__' column in the 'active_permits_from_pims_temp1' dataframe
# based on the delimiter "-", creating new columns 'permit_code' and 'permit_num'.
# The 'too_many' argument is set to "merge," which means any excess columns generated during the split will be merged.
active_permits_from_pims_temp2 <- active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",                # Delimiter used for splitting
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%

    # Use the 'separate_wider_regex' function to split the 'vessel_or_dealer' column in the resulting dataframe.
    # This function uses regular expressions to define patterns for creating new columns.
    # In this case, it defines patterns for 'vessel_official_number' and 'vessel_name.'
    # The 'too_few' argument is set to "align_start," which aligns any missing columns at the start.
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",  # Regular expression for vessel official number (more than one alphanumeric character)
        " */* ",                                  # Pattern for separating columns with slashes
        vessel_name = "[A-Za-z0-9]+"              # Regular expression for vessel name (more than one alphanumeric character)
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names with "_date"
  # Use the 'grep' function to find and extract column names from the 'active_permits_from_pims_temp2' dataframe
  # that has "_date".
  ends_with_date_fields <- grep("_date",             # Pattern to search for in column names
                                names(active_permits_from_pims_temp2),  # Names of columns to search within
                                value = TRUE)         # Return matching column names as values in the result.


  # convert to the date format
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    dplyr::select(status_date) %>%                 # Select 'status_date' column
    dplyr::arrange(dplyr::desc(status_date)) %>%   # Arrange in descending order
    dplyr::distinct() %>%                               # Remove duplicate rows
    head()                                       # Retrieve the first few rows
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

get_data_from_csv <- function() {
  # uncomment to run
  compl_clean <- get_data_from_FHIER_csvs()
  # View(compl_clean)
  dim(compl_clean)
  # 208893     21
  
  ## get compliance error definitions from csvs ----
  err_desc <- get_compliance_error_definitions()
  
  ## get permit data from PIMS csv ----
  
  active_permits_from_pims <- get_permit_data_from_PIMS_csv()
  
  compl_clean1 <- additional_clean_up(compl_clean)

  cat("compl_clean_sa_vs_gom_m_int_c")  
  return(compl_clean1)
}

additional_clean_up <- function(compl_clean) {

  # separate SA and GOM permits
  compl_clean_sa_vs_gom <-
    separate_permits_into_3_groups(compl_clean)

  # View(compl_clean_sa_vs_gom)

  # add columns for month and quarter
compl_clean_sa_vs_gom_m <- compl_clean_sa_vs_gom %>%
    # Add a new column 'year_month' by extracting the year and month from the 'week_start' column
    dplyr::mutate(year_month = zoo::as.yearmon(week_start)) %>%
    # Add another new column 'year_quarter' by extracting the year and quarter from the 'week_start' column
    dplyr::mutate(year_quarter = zoo::as.yearqtr(week_start))

  # convert report numbers to numeric
  # Create a new data frame 'compl_clean_sa_vs_gom_m_int' by applying integer conversion to selected columns in 'compl_clean_sa_vs_gom_m'.
  compl_clean_sa_vs_gom_m_int <- compl_clean_sa_vs_gom_m %>%
    dplyr::mutate(
      captainreports__ = as.integer(captainreports__),
      negativereports__ = as.integer(negativereports__),
      gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
    )

  # add year_permit column ----
  # Create a new data frame 'compl_clean_sa_vs_gom_m_int_c' by adding a new column 'year_permit' to 'compl_clean_sa_vs_gom_m_int'. Depending on the values in the 'year' and 'permit_sa_gom' columns, different combinations of 'year' and a descriptive label are created for the 'year_permit' column using the 'paste' function.
  compl_clean_sa_vs_gom_m_int_c <- compl_clean_sa_vs_gom_m_int %>%
    
    # Use the 'mutate' function to add a new column 'year_permit' based on conditional logic using the 'case_when' function.
    dplyr::mutate(
      year_permit =

        # The 'case_when' function is used for conditional transformations.
        dplyr::case_when(

          # When 'year' is "2022" and 'permit_sa_gom' is "gom_only" or "dual," create a string combining 'year' and "gom_dual."
          year == "2022" & (permit_sa_gom == "gom_only" | permit_sa_gom == "dual") ~
            paste(year, "gom_dual"),

          # When 'year' is "2022" and 'permit_sa_gom' is "sa_only," create a string combining 'year' and "sa_only."
          year == "2022" & permit_sa_gom == "sa_only" ~
            paste(year, "sa_only"),

          # When 'year' is "2023" and 'permit_sa_gom' is either "sa_only" or "dual," create a string combining 'year' and "sa_dual."
          year == "2023" & (permit_sa_gom %in% c("sa_only", "dual")) ~
            paste(year, "sa_dual")
        )
    )

  return(compl_clean_sa_vs_gom_m_int_c)
}

# Uncomment and run above functions if using csvs downloaded from FHIER
compl_clean_sa_vs_gom_m_int_c <- get_data_from_csv()

# get data from db ----
# source(file.path(my_paths$git_r, r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

# run_all_get_db_data(): 8.56 sec elapsed                                                                            
# all_sheets_l
# vessels_22_sa
# vessels_to_remove_from_ours
# all_logbooks_db_data_2022_short_p_region


                                                                                                                            


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
  "from_Fhier",
  "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)"
  )

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


#### Current file: get_srhs_vessels.R ----

# get SRHS vessels to exclude ----
# The file is provided by Kenneth Brennan

srhs_vessels_2022 <-
  r"(~\Official documents\srhs_boats\2022_SRHS_Vessels_08_18_2023.xlsx)"

if (!file.exists(srhs_vessels_2022)) {
  srhs_vessels_2022 <-
    file.path(my_paths$inputs,
              "2022_SRHS_Vessels_08_18_2023.xlsx")
}

srhs_vessels_2022_info <-
  read_excel(
    srhs_vessels_2022,
    # add the sheet name if needed and uncomment the next line
    # sheet = sheet_n,
    # use my fix_names function for col names
    .name_repair = fix_names,
    # if guess_max is omitted, the algorithm uses only the few first lines and sometimes guesses it wrong
    guess_max = 21474836,
    # read all columns as text
    col_types = "text"
  )

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


#### add-ons 3 ---- 


# Uses the file.path function to construct a file path. The components used are:
# my_paths$outputs: A variable containing a directory path.
# "quantify_compliance": A directory name to be appended to the path.
# today(): Represents a function used to include the current date, creating a date-specific path.
plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today())
# create dir if doesn_t exists
create_dir_if_not(plot_file_path)

plot_colors <- list("compliant" = "skyblue1",
                    "non_compliant" = "#0570B0",
                    "nc_bucket" = "deepskyblue",
                    "non_compliant_by_month" = "blue")

title_permits <- data.frame(
  # title = c("SA Only", "GOM + Dual", "2023: SA + Dual"),
  title = c("2022: SA Only", "2022: GOM + Dual", "2023: SA + Dual"),
  year_permit = c("2022 sa_only",
                  "2022 gom_dual",
                  "2023 sa_dual"),
  second_part = c("Permitted Vessels",
                  "Permitted Vessels",
                  "Permitted Vessels")
)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int_c |>
  dplyr::filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# remove 2023 gom_only ----
remove_23_gom <- function(my_df) {
  my_df |>
    dplyr::filter(!(year == "2023" & permit_sa_gom == "gom_only")) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered <-
  # from get_data
  remove_23_gom(compl_clean_sa_vs_gom_m_int_1)

# save vsl count for future checks ----
count_all_vessels <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels
count_all_vessels[1]
# 3776

count_not_gom23_vessels <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels
count_not_gom23_vessels[1]
# 3658

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  dplyr::select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  dplyr::count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  dplyr::count(compliant_, year_permit) %>%
  dplyr::arrange(year_permit, compliant_)
# vessels
#  NO         2022 gom_dual   304
#  YES        2022 gom_dual  1482
#  NO         2022 sa_only   1289
#  YES        2022 sa_only   1617
#  NO         2023 sa_dual   1628
#  YES        2023 sa_dual   2125

# metrics
# vessels_compl_or_not_per_y_r_not_gom23
# 1 NO         2022 gom_dual   290
# 2 YES        2022 gom_dual  1298
# 3 NO         2022 sa_only   1263
# 4 YES        2022 sa_only   1602
# 5 NO         2023 sa_dual   1615
# 6 YES        2023 sa_dual   2111


#### Current file: quantify_compliance_from_fhier_year.R ----

# by Year: ----
## year add total ----
# (both compl. and not, a vsl can be in both)

add_total_cnt_in_gr <- function(my_df, group_by_col) {
  my_df %>%
    # group by per year and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate(total_vsl_y =
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered, "year_permit")

compl_clean_sa_vs_gom_m_int_c_cnt_tot <- 
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_c, "year_permit")

# check
compl_clean_sa_vs_gom_m_int_filtered_tot %>%
  select(year_permit, total_vsl_y) %>%
  unique()
#   year_permit   tota_vsl_m
#   <chr>              <int>
# 1 2022 sa_only        2178
# 2 2022 gom_dual       1495
# 3 2023 sa_dual        2236

# 1 2022 sa_only         2145
# 2 2022 gom_dual        1304
# 3 2023 sa_dual         2220

## expired or not? ----
end_of_2022 <- as.Date("12/31/2022", format = "%m/%d/%Y")

expired_or_not <- function(my_df) {
  my_df %>%
    # get difference in days
    dplyr::mutate(exp_w_end_diff_y =
                    as.numeric(as.Date(permitgroupexpiration) -
                                 end_of_2022)) %>%
    # create a column
    dplyr::mutate(
      perm_exp_y =
        dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                         exp_w_end_diff_y > 0 ~ "active")
    ) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_tot)

## count expiration by year, permit ----
count_expiration_by <- function(my_df, group_by_var) {
  my_df %>%
    dplyr::group_by_at(group_by_var) %>%
    # count distinct vessels per group
    dplyr::mutate(exp_y_tot_cnt = n_distinct(vessel_official_number)) %>%
    return()
}

group_by_var <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y,
                      group_by_var)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

## get compl_counts ----
### get compl, no compl, or both per year ----

get_compl_by <- function(my_df, group_by_for_compl) {
  my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = vessel_official_number,
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}

group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short,
               group_by_for_compl)

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot |>
  dplyr::select(
    vessel_official_number,
    year_permit,
    compliant_,
    total_vsl_y
  ) |>
  dplyr::distinct() |>
  get_compl_by(group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide)
# [1]    6 3662

### count compl, no compl, or both per year, permit, active status ----

count_by_cols <- function(my_df,
                          cols_names) {
  my_df %>%
    # turn back to a longer format, vessel ids in one column
    tidyr::pivot_longer(
      # all other columns are vessel ids, use them as names
      cols = !any_of(cols_names),
      values_to = "is_compl_or_both",
      names_to = "vessel_official_number"
    ) %>%
    return()
}

cols_names <-
  c("year_permit", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide,
                cols_names)

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long <- 
  count_by_cols(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide,
                c("year_permit", "total_vsl_y"))

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa <- 
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long |> 
  filter(year_permit == "2022 sa_only") |> 
  select(vessel_official_number, is_compl_or_both) |> 
  dplyr::distinct()

# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa
# 4039    
# vessel_official_number 4039
# is_compl_or_both          4

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa |>
  filter(is_compl_or_both == "NO")

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c)
# 512

### get cnts for compl, no compl, or both per month with exp ----
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      unique() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by onther columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      unique() %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols <- c("year_permit", "perm_exp_y")
cols_to_cnt <- c("year_permit", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] 22  6

#### check counts ----
# print_df_names(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] "year_permit, total_vsl_y, perm_exp_y, exp_y_tot_cnt, is_compl_or_both, compl_or_not_cnt"

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::select(year_permit, total_vsl_y, compl_or_not_cnt, is_compl_or_both) %>%
  dplyr::group_by(year_permit) %>%
  # get sums
  dplyr::mutate(sum_cnts = sum(compl_or_not_cnt)) %>%
  dplyr::filter(!total_vsl_y == sum_cnts) %>%
  dim()
# 0 OK
# unique() %>%
# dplyr::group_by(is_compl_or_both) %>%
# dplyr::mutate(sum_compl_or_not_cnt = sum(compl_or_not_cnt)) %>%
# dplyr::select(is_compl_or_both, sum_compl_or_not_cnt) %>%
# unique() %>%
# dplyr::glimpse()
# $ is_compl_or_both     <chr> "YES", "NO", "NO_YES"
# $ sum_compl_or_not_cnt <int> 890, 562, 727
# 890 + 562 + 727
# [1] 2179
# 0

### One vessel in 2 groups ----
# The number should be the same as the total number we got earlier. It is not, which means One vessel is in 2 perm_exp_y groups, has both expired and not expired permit in 2022.

# TODO: One vessel in 2 perm_exp_y - email
#   year_permit  tota_vsl_m sum_cnts
#   <chr>             <int> <int>
# 1 2022 sa_only       2178 2179
# ...
# https://stackoverflow.com/questions/51848578/how-to-find-values-shared-between-groups-in-a-data-frame
# "Or you can group by val and then check whether the number of distinct exp for that val is equal to the data frame level number of distinct exp"
#

### check if a vessel is compliant and not at the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  dim()
# dplyr::glimpse()
# 0 - OK

# check if a vessel permit is expired and not in the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dim()
# 0

### check total_vsl_y vs. sum_cnts (should be equal, see dbl FL7825PU above) ----
compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1617 YES
# 2       1289 NO
# 1       1602 YES
# 2       1263 NO

compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  # dplyr::group_by(compliant_, perm_exp_y) %>%
  # dplyr::group_by(compliant_) %>%
  dplyr::group_by(perm_exp_y) %>%
  # 1707 + 472
  # [1] 2179
  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1442 YES        active
# 2        887 NO         active
# 3        402 NO         expired
# 4        175 YES        expired
# YES: 1442+175
# [1] 1617
# NO: 887+402
# [1] 1289
# 1617+1289
# 2906

# today()
# [1] "2023-06-19"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1707 YES        active
# 2       1707 NO         active
# 3        472 NO         expired
# 4        472 YES        expired
# 1707 + 472
# 2179

# [1] "2023-08-26"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1694 YES        active
# 2       1694 NO         active
# 3        451 NO         expired
# 4        451 YES        expired
# 1694+451 = 2145

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration

add_total_cnts <-
  function(my_df, group_by_compl_cols, group_by_exp_cols) {
    my_df %>%
      # remove NAs
      dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
      dplyr::mutate(
        compl_or_not =
          dplyr::case_when(is_compl_or_both == "YES" ~
                             "compliant",
                           .default = "non_compliant")
      ) %>%
      dplyr::group_by_at(group_by_compl_cols) %>%
      # add counts by compliant
      dplyr::mutate(cnt_y_p_c = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      # add counts by permit expiration
      dplyr::group_by_at(group_by_exp_cols) %>%
      dplyr::mutate(cnt_y_p_e = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols1 <- c("year_permit", "compl_or_not")
group_by_cols2 <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt,
    group_by_cols1,
    group_by_cols2
  )

# check cnts
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
#   # remove NAs
#   filter(year_permit == '2022 gom_dual' & perm_exp_y == 'expired') %>% View()

## add percents of total ----
add_percents_of_total <-
  function(my_df, select_cols) {
    my_df %>%
      dplyr::select(all_of(select_cols)) %>%
      unique() %>%
      dplyr::mutate(perc_c_nc = cnt_y_p_c * 100 / total_vsl_y) %>%
      return()
  }

select_cols <- c(
  "year_permit",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 11  7

## red/green plots for compl vs. non compl vessels per year ----

# "Permitted SEFHIER Vessels"

gg_all_c_vs_nc_plots <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_permit)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # 1st figure title: "SA Only Permitted Vessels (Total Permitted: 2178; Expired Permits: 472)"
    # 2nd figure title: "GOM + Dual Permitted Vessels (Total Permitted: 1495; Expired Permits: 303)"

    curr_title_permit <-
      title_permits %>%
      filter(year_permit == curr_year_permit)

    current_title <-
      paste(curr_title_permit$title,
             curr_title_permit$second_part)

    # current_title <-
    #   paste0(
    #     curr_title_permit$title,
    #     " ",
    #     curr_title_permit$second_part,
    #     " (Active Permits: ",
    #     active_permits$cnt_y_p_e,
    #     "; Expired Permits: ",
    #     expired_permits$cnt_y_p_e,
    #     ")"
    #   )

    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc")

    return(one_plot)

  })

# 2023 plot
# gg_all_c_vs_nc_plots[[3]]

# 2022
sa_only22 <- gg_all_c_vs_nc_plots[[1]]
# gg_all_c_vs_nc_plots[[2]]

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

# combine plots for 2022
grid.arrange(gg_all_c_vs_nc_plots[[1]],
             # gg_all_c_vs_nc_plots[[2]],
             top = main_title)

grid.arrange(gg_all_c_vs_nc_plots[[2]],
             top = main_title)

grid.arrange(gg_all_c_vs_nc_plots[[3]],
             top = main_title)

# Non compliant only ----

# start with the new data with expiration by year
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_permit_year_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   compliant_,
                   name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

# View(weeks_per_vsl_permit_year_compl_cnt)

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(vessel_official_number == "1000042" &
                  year == "2022") %>%
  dplyr::select(year,
                compliant_,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(year_permit == "2022 sa_only",
                compliant_ == "NO") %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()

head(nc_2022_sa_only_test)
# weeks_per_vessel_per…¹ total_weeks_per_vessel
# 1 VA9236AV 52 52
# 2 VA6784AD 24 24
# 3 VA4480ZY 44 44
# 4 SC9207BX 26 50
# 5 SC8907DF 14 40
# 6 SC8298DH 45 45

weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    year_permit == "2022 sa_only",
    compliant_ == "YES",
    vessel_official_number == "SC8907DF"
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# 26  40

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 185251     32

# test
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", year == "2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178
# 2145

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022",
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1289    Non compliant vsl
  dim()
# [1] 26466 non compliant weeks
# [1] 25662     1

### test 1b ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(
    year,
    permit_sa_gom,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_permit,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

# str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,221 × 7] (S3: tbl_df/tbl/data.frame)
# $ weeks_per_vessel_per_compl: int [1:3221] 52 24 44 26 14 45 5 41 52 27 ...
# $ total_weeks_per_vessel    : int [1:3221] 52 24 44 50 40 45 41 45 52 52 ...
# $ percent_compl             : num [1:3221] 100 100 100 52 35 ...

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
# count in one bucket
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::count(percent_compl, year_permit,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 634
# 615

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::add_count(year_permit,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(year_permit,
                percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  unique() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# $ year_permit         : chr [1:4] "2022 sa_only" "2022 sa_only" "2022 sa_only" "2022 sa_only"
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 398 168 82 615
# $ total_per_y_r       : int [1:4] 1263 1263 1263 1263

# "2022 sa_only"
# 633+85+172+399
# [1] 1289

# $ cnt_v_in_bucket     : int [1:4] 399 171 85 634
# 399 + 171 + 85 + 634
# 1289
# correct

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  # cnt vessels per year, permit and compliance
  dplyr::add_count(year_permit,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1
# 1 0<= & <25%                         31.5
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.49
# 4 75<= & <=100%                      48.7

# 5) blue plots by year ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# "2022: % Non-Compliant GOM + Dual Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this]  "(Total Non-Compliant = 304 Vessels; Active Permits = 1192 Vessels)"
# "2022: % Non-Compliant SA Only Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this] "(Total Non-Compliant = 1289 Vessels; Active Permits = 1707 Vessels)"
# For plot 4:
# "2023: SA + Dual Permitted SEFHIER Vessels (Total Permitted: 2235 ; Total Noncompliant: 1628; Expired Permits: 1)"

blue_year_plot_titles <-
  data.frame(
    year_permit = c("2022 sa_only",
                    "2022 gom_dual",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    curr_title_y_p <- make_year_permit_label(curr_year_permit)

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      filter(year_permit == curr_year_permit)

    y_p_title <-
      paste0(
        curr_blue_year_plot_title$first_part,
        "Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        # " Vessels; Acitve permits = ",
        # active_permits$exp_y_tot_cnt,
        # "; Expired permits: ",
        # expired_permits$exp_y_tot_cnt,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 12))

    return(one_plot)
  })

sa_only_22_gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <- 
gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]]

## plot 2022 ----
ndash <- "\u2013"
super_title <- paste0(
  "2022: % Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

# footnote = textGrob(
#   "X axes is % of missing reports for non-compliant vessels",
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -1.5,
#   just = "right",
#   x = 0.01, y = 0.99,
#   vjust = 1
# )

### common y axes ----
yleft <- textGrob("% per permit region",
                  # rotate
                  rot = 90,
                  gp = gpar(fontsize = 10))

p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  # remove individual x and y labels for each plot
  purrr::map(~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)

## SA23 ----

super_title <- "% of non-compliant vessels (2023)"


gridExtra::grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]],
                        top = super_title
                        )
                        # ,
                        # bottom = footnote

# part time fishery ----
compl_clean_sa_vs_gom_m_int_1 |>
  filter(year_permit == "2022 gom_dual") |>
  summarise(n_distinct(vessel_official_number))

compl_clean_sa_vs_gom_m_int_1 |>
  filter(year_permit == "2022 gom_dual") |>
  filter(gom_permitteddeclarations__ > 0) |>
  summarise(n_distinct(vessel_official_number))

# 808 * 100 / 1304
# 61.96319

# write_csv(compl_clean_sa_vs_gom_m_int_1,
#           "compl_clean_sa_vs_gom_m_int_1.csv")



                                                                                                                            


#### Current file: quantify_compliance_from_fhier_year_100_nc.R ----

# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2022 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 
# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_sa_22' by filtering an existing data frame.
# Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p' to the next operation.
# The filter function is used to select rows where the column year_permit is equal to "2022 sa_only". The result is a filtered data frame for the specified year and permit condition.
count_weeks_per_vsl_permit_year_compl_p_sa_22 <-
  count_weeks_per_vsl_permit_year_compl_p |>
  dplyr::filter(year_permit == "2022 sa_only")

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# by applying a series of operations to the existing data frame
count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100 <-
  
  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Select specific columns from the data frame
  dplyr::select(vessel_official_number,
                compliant_,
                year_permit,
                percent_compl) |>
  
  # Remove duplicate rows
  dplyr::distinct() |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Further filter the data frame to select rows where 'percent_compl' is equal to 100
  dplyr::filter(percent_compl == 100)

dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)
# 487

# All vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p$vessel_official_number))
# 3669

# All SA 2022 vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p_sa_22$vessel_official_number))
# 2152
# in metrics:
# Total Vessels With SA Only
# 2275
# 2275 - 2152 = 123?

# Create a new data frame 'sa_22_non_c_vessels' by applying a series of operations to the existing data frame
sa_22_non_c_vessels <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Select a specific column 'vessel_official_number'
  dplyr::select(vessel_official_number) |>
  
  # Remove duplicate rows
  dplyr::distinct()

# Create a new data frame 'sa_22_vessels' by applying a series of operations to the existing data frame
sa_22_vessels <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Select a specific column 'vessel_official_number'
  dplyr::select(vessel_official_number) |>
  
  # Remove duplicate rows
  dplyr::distinct()

# Calculate the percentage of never compliant entries from all non-compliant entries

# Calculate the total number of non-compliant entries that are never compliant
# by multiplying the number of rows in 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# with 100 and dividing it by the number of rows in 'sa_22_non_c_vessels'
percent_of_never_compl_from_all_non_c <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_non_c_vessels)[[1]]
# [1] 41.87446 %

# Calculate the percentage of never compliant entries from all entries in 2022
# In this code, the variable percent_of_never_compl_from_all_sa_2022 is calculated by dividing the number of rows in the data frame count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100 (representing non-compliant entries that are never compliant) by the number of rows in the data frame sa_22_vessels (representing all entries in the year 2022) and then multiplying the result by 100 to obtain the percentage of never compliant entries from all entries in 2022.

# Calculate the total number of non-compliant entries that are never compliant
# by multiplying the number of rows in 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# with 100 and dividing it by the number of rows in 'sa_22_vessels'
percent_of_never_compl_from_all_sa_2022 <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_vessels)[[1]]
# [1] 22.63011 %

# SA vessels 2022 vessels cnt / percent compl ----

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_short' 
# by applying a series of operations to the existing data frame

# The data frame count_weeks_per_vsl_permit_year_compl_p is passed to the next operation using the pipe operator.
# Specific columns are selected using the select function from dplyr.
# Duplicate rows are removed using the distinct function.
# The result is a data frame with a subset of columns from the original data frame, and duplicate rows are eliminated.

count_weeks_per_vsl_permit_year_compl_p_short <- 
  count_weeks_per_vsl_permit_year_compl_p |>
  dplyr::select(
    vessel_official_number,
    compliant_,
    year_permit,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) |>
  dplyr::distinct()

# data_overview(count_weeks_per_vsl_permit_year_compl_p_short)
# vessel_official_number     3669

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_short_count' 
# by applying a series of operations to the existing data frame 'count_weeks_per_vsl_permit_year_compl_p_short'
count_weeks_per_vsl_permit_year_compl_p_short_count <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_short' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_short |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Further filter the data frame to select rows where 'year_permit' is "2022 sa_only"
  dplyr::filter(year_permit == "2022 sa_only") |>
  
  # Select specific columns 'vessel_official_number' and 'percent_compl'
  dplyr::select(vessel_official_number, percent_compl) |>
  
  # Add a count column 'vessels_cnt' based on the 'percent_compl' values
  dplyr::add_count(percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count, 2)

## add columns ----
# This code performs a series of data manipulation operations on the existing data frame count_weeks_per_vsl_permit_year_compl_p_short_count to create a new data frame, count_weeks_per_vsl_permit_year_compl_p_short_count_perc. Each step is explained as follows:

# The data frame count_weeks_per_vsl_permit_year_compl_p_short_count is passed to the next operation using the pipe operator.
# A new column 'total_vessels' is added to calculate the count of distinct 'vessel_official_number'.
# Columns 'perc_nc_100_gr' and 'perc_nc_100_gr_name' are added based on 'percent_compl' values and a defined interval.
# The data frame is grouped by 'perc_nc_100_gr'.
# A new column 'group_vsl_cnt' is added to calculate the count of distinct 'vessel_official_number' within each group.
# The column 'vessel_official_number' is removed.
# Duplicate rows are removed.
# The 'perc_of_perc' column is calculated based on 'perc_nc_100_gr' values.
# The data frame is ungrouped.
count_weeks_per_vsl_permit_year_compl_p_short_count_perc_sep <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  dplyr::mutate(total_vessels = n_distinct(vessel_official_number)) |>
  # Add columns 'perc_nc_100_gr' and 'perc_nc_100_gr_name' based on 'percent_compl' values
  dplyr::mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100)),
    perc_nc_100_gr_name =
      dplyr::case_when(perc_nc_100_gr == 2 ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |>
  dplyr::group_by(perc_nc_100_gr) |>
  dplyr::mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |> 
  ungroup()

count_weeks_per_vsl_permit_year_compl_p_short_count_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc_sep |>
  dplyr::group_by(perc_nc_100_gr) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  # Calculate the 'perc_of_perc' based on 'perc_nc_100_gr' values
  dplyr::mutate(
    perc_of_perc =
      dplyr::case_when(
        perc_nc_100_gr == 2 ~
          vessels_cnt * 100 / total_vessels,
        perc_nc_100_gr == 1 ~
          sum(vessels_cnt) * 100 / total_vessels
      )
  ) |>
  dplyr::ungroup()

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)

# Create a ggplot object named 'nc_sa_22_100_plot' using a series of dplyr and ggplot functions.

# Use the pipe operator to pass the output of the previous function to the next function.

nc_sa_22_100_plot <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>

  # Select specific columns from the data frame.
  dplyr::select(
    perc_nc_100_gr,
    perc_nc_100_gr_name,
    group_vsl_cnt,
    perc_of_perc
  ) |>

  # Remove duplicate rows based on selected columns.
  dplyr::distinct() |>

  # Create a ggplot object with specified aesthetics.
  ggplot(aes(
    x = perc_nc_100_gr_name,   # X-axis variable
    y = round(perc_of_perc, 0),  # Y-axis variable with rounding
    fill = as.factor(perc_nc_100_gr)  # Fill aesthetic for grouping
  )) +

  # Add a column plot to the ggplot object.
  geom_col() +

  # Manually set the fill colors for different factors.
  scale_fill_manual(
    values = c(
      # "1" = "pink",
      # "2" = "red"
      "1" = "skyblue1",  # Custom color for factor 1
      "2" = "#0570B0"    # Custom color for factor 2
    ),
    name = "Non compliant",  # Legend title
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +

  # Remove the legend.
  theme(legend.position = "none") +

  # Customize text size for y-axis title and labels.
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x = element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y = element_text(size = text_sizes[["axis_text_y_size"]])
  ) +

  # Set the plot titles and y-axis label.
  labs(
    title = stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
    y = "Non compliant in 2022 (%)",
    x = ""  # No label for x-axis
  ) +

  # Set the y-axis limits to be between 0 and 100.
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)
# Add percent numbers on the bars
# In this code, the 'nc_sa_22_100_plot' ggplot object is further modified by adding text labels to the bars. The geom_text function is used to display labels on the plot, and it is configured to show the rounded percentage values on top of the bars. The position_stack function is used to position the labels in the middle of the bars, and the text size is set to a predefined value specified in text_sizes[["geom_text_size"]].

nc_sa_22_100_plot <- nc_sa_22_100_plot +

  # Add text labels to the plot.
  geom_text(
    aes(
      label = paste0(round(perc_of_perc, 0), "%")
    ),
    # Position the text labels in the middle of the bars.
    position = position_stack(vjust = 0.5),
    # Set the text size for the labels.
    size = text_sizes[["geom_text_size"]]
  )

# show the plot
nc_sa_22_100_plot

# Save the ggplot object 'nc_sa_22_100_plot' as a PNG image file using 'ggsave'.

ggsave(
  file = "sa_22_nc_100.png",  # File name
  plot = nc_sa_22_100_plot,   # The ggplot object to be saved
  device = "png",            # File format (PNG)
  path = file.path(my_paths$outputs, r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),  # Output directory
  width = 20,                # Image width in centimeters
  height = 10,               # Image height in centimeters
  units = "cm"               # Unit of measurement for width and height
)

## 100% non compliant, less than 100 and compliant ----

# Calculate the number of distinct vessel official numbers in the data.
# Create a new object named 'total_vessels_c_n_nc' by first filtering the data.
total_vessels_c_n_nc <-
  count_weeks_per_vsl_permit_year_compl_p_short |>
  
  # Select the 'vessel_official_number' column.
  dplyr::select(vessel_official_number) |>

  # Remove duplicate rows based on the selected column and get the dimensions.
  dplyr::distinct() |>
  dim()
# vessel_official_number     3669

nc_sa_22_100_plot <-
  # Calculate the count of weeks per VSL (vessel) permit year for non-compliant permits
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>
  # Select specific columns from the data frame
  dplyr::select(perc_nc_100_gr,
               perc_nc_100_gr_name,
               group_vsl_cnt,
               perc_of_perc) |>

  # Remove duplicate rows from the data frame
  dplyr::distinct() |>
  # Create a ggplot object with specified aesthetics
  # Define the x-axis using 'perc_nc_100_gr_name'
  ggplot(aes(
    x = perc_nc_100_gr_name,
    # Define the y-axis by rounding 'perc_of_perc' to the nearest integer
    y = round(perc_of_perc, 0),
    # Define fill colors based on 'perc_nc_100_gr'
    fill = as.factor(perc_nc_100_gr)
  )) +
  # Add a bar chart (column chart) to the plot
  geom_col() +
  # Manually set fill colors for the chart
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "1" = "skyblue1", # Set color for '1'
        "2" = "#0570B0"   # Set color for '2'
      ),
    # Legend title
    # Set the legend title to "Non compliant"
    name = "Non compliant",
    # Set legend labels
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +
  # Remove the legend from the plot
  theme(legend.position = "none") +
  # Customize the appearance of the plot
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # Set plot titles and labels
  # Set plot title using a formatted string
  # Set y-axis label
  # Set x-axis label to an empty string
  labs(title = 
         stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
       y = "Non compliant in 2022 (%)",
       x = "") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100)

# plot(count_weeks_per_vsl_permit_year_compl_p_short_count)

# 100 % nc check active permits ----
# To check that, I would pull the list of those 23% SA SEFHIER vessels who never reported in 2022. Then I would check that against a list (pulled today) of 2023 permitted SA SEFHIER vessels, to see how many of the 23% of SA SEFHIER vessels who never reported still have a permit in 2023 (as of the date you pull 2023 permit data, anyway). 

count_weeks_per_vsl_permit_year_compl_p_short_count_perc_sep |> glimpse()
# [1] 1163    7

count_weeks_per_vsl_permit_year_compl_p_short_count_perc_sep |>
  filter(perc_nc_100_gr_name == "Never Reported") |>
  select(vessel_official_number, total_vessels, group_vsl_cnt) |>
  distinct() |> 
  glimpse()
 # Length:487             
# total_vessels = 1163
# group_vsl_cnt = 487  

never_reported_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc_sep |>
  filter(perc_nc_100_gr_name == "Never Reported") |>
  select(vessel_official_number) |> 
  distinct()

dim(never_reported_vessels)
# [1] 487   1

# print_df_names(never_reported_vessels)
# print_df_names(vessels_permits_id_clean(vessels_permits))
never_reported_vessels_permits <-
  left_join(never_reported_vessels,
            vessels_permits_id_clean(vessels_permits),
            join_by(vessel_official_number == PERMIT_VESSEL_ID))

# dim(never_reported_vessels_permits)
# [1] 4396   51

never_reported_vessels_permits_exp <-
  never_reported_vessels_permits |>
  select(
    vessel_official_number,
    EFFECTIVE_DATE,
    tidyselect::contains("expir"),
    tidyselect::contains("end")
  ) |>
  distinct()

dim(never_reported_vessels_permits_exp)
# [1] 846   4
# [1] 856   5

# Jeannette's:
  #   req.permit_effective_date <= '2023-12-31'
  # AND req.permit_termination_date >= '2023-01-01'
  # AND ( req.permit_end_date IS NULL
  #       OR req.permit_end_date >= '2023-01-01' )

never_reported_vessels_permits_exp_active23 <-
  never_reported_vessels_permits_exp |>
  filter(EFFECTIVE_DATE <= '2023-12-31') |> 
  mutate(
    last_exp_in_22 =
      case_when(LAST_EXPIRATION_DATE > "2022-12-31" ~ "active23",
                .default = "exp"),
    exp_in_22 =
      case_when(EXPIRATION_DATE > "2022-12-31" ~ "active23",
                .default = "exp"),
    end_in_22 =
      case_when(END_DATE > "2022-12-31" ~ "active23",
                .default = "exp")
  )

never_reported_vessels_permits_exp_active23 |> 
  select(vessel_official_number,
         # exp_in_22) |>
  # [1] 594   2
         last_exp_in_22) |>
  # [1] 496   2
  # end_in_22) |>
  # [1] 565   2
    # tidyselect::ends_with("22")) |> 
# [1] 619   4
  distinct() |> 
  dim()

never_reported_vessels_permits_exp_active23_short <-
  never_reported_vessels_permits_exp_active23 |>
  select(vessel_official_number,
         last_exp_in_22) |>
  distinct()
  
never_reported_vessels_permits_exp_active23_short |> 
  count(last_exp_in_22)
# 1 active23         215
# 2 exp              281

write_csv(
  never_reported_vessels_permits_exp,
  file.path(
    my_paths$outputs,
    "quantify_compliance",
    "never_reported_vessels_permits_expiration.csv"
  )
)

### compare with metric tracking for 2023 ----

inp_phier_csv_path <-
  file.path(
    my_paths$inputs,
    "quantify_compliance",
    "vessels_by_for-hire_only_permit_region_totals_-_raw_data_via_valid_and_renewable_permits_(sero_new_source)_11_17_23.csv"
  )

inp_phier_csv <- read_csv(inp_phier_csv_path)

have_active_permit_2023 <- 
  intersect(never_reported_vessels_permits$vessel_official_number,
        inp_phier_csv$`Vessel Official Number`
        ) |> 
  length()
# 215

  setdiff(never_reported_vessels_permits$vessel_official_number,
        inp_phier_csv$`Vessel Official Number`
        ) |> 
  length()
# 272

#### % ----
# 1 active23         215
# 2 exp              281

all_never_rep <- 
  never_reported_vessels_permits$vessel_official_number |> 
  unique() |> 
  length()

have_active_permit_2023 * 100 / all_never_rep
# 44%

## Less than 100% ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 <- 
  # Filter the data frame based on a condition
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  
  # Keep only rows where 'vessels_cnt' is less than 100
  dplyr::filter(vessels_cnt < 100)

perc_non_compl_plot_less_100 <-
  # Create a ggplot plot using the specified data frame and aesthetics
  ggplot(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
         # Define x-axis using 'vessels_cnt'
         aes(x = vessels_cnt,
             # Define y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add a line to the plot with the specified color
  geom_line(color = "deepskyblue") +
  # Set plot titles and labels
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100", 
       # Set x-axis label
       x = "Vessel count", 
       # Set y-axis label
       y = "% nc vsls") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100) +
  # Customize the size of the individual plot's title
  theme(plot.title =
          element_text(size = 12))

perc_non_compl_plot_less_100 <- 
  # Add additional layers to the existing plot
  perc_non_compl_plot_less_100 +
  # Add points to the plot
  # Display data points on the plot
  geom_point() +
  # text on dots
  # on top
  # Add text labels to the data points
  # Display the 'percent_compl' value rounded to one decimal place as text labels
  # Adjust the vertical justification of the labels
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = -0.3)

# Calculate the maximum value in the 'percent_compl' column of the data frame
max_percent_compl_less_100 <-
  max(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 98.07692

# Calculate the minimum value in the 'percent_compl' column of the data frame
min_percent_compl_less_100 <-
  min(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 1.923077


# Add additional layers to the existing plot
perc_non_compl_plot_less_100_hline <-
  # Add horizontal lines to the plot
  perc_non_compl_plot_less_100 +
  # Add a horizontal line at the 'min_percent_compl_less_100' value
  geom_hline(yintercept = min_percent_compl_less_100,
             # Set the color of the line to red
             color = "red") +
  # Add a horizontal line at the 'max_percent_compl_less_100' value
  geom_hline(yintercept = max_percent_compl_less_100,
             # Set the color of the line to red
             color = "red")

# perc_non_compl_plot_less_100 + geom_smooth(method = "lm", se = FALSE)

perc_non_compl_plot_less_100_ann <- 
  # Add additional layers to the existing plot
  perc_non_compl_plot_less_100_hline +  
  
  # Add text annotations to the plot
  annotate(
    "text",
    label = paste0(round(min_percent_compl_less_100, 1), "%"),  # Display the minimum percentage value as a text label
    x = 53,  # Set the x-coordinate for the label
    y = min_percent_compl_less_100 + 3,  # Set the y-coordinate for the label slightly above the minimum value
    size = 4,  # Set the size of the text
    colour = "red"  # Set the text color to red
  ) +
  
  # Add another text annotation to the plot
  annotate(
    "text",
    label = paste0(round(max_percent_compl_less_100, 1), "%"),  # Display the maximum percentage value as a text label
    x = 53,  # Set the x-coordinate for the label
    y = 100,  # Set the y-coordinate for the label at the top of the plot
    size = 4,  # Set the size of the text
    colour = "red"  # Set the text color to red
  )


perc_non_compl_plot_less_100_ann

# split by group ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 |>
  
  # Calculate the total vessel count
  dplyr::mutate(vessels_cnt_tot = sum(vessels_cnt)) |>
  
  # Create groups for vessel counts
  dplyr::mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6))) |>
  
  # Count the number of entries in each vessel count group
  dplyr::add_count(vessel_cnt_group, wt = vessels_cnt, name = "vessel_cnt_group_num")  |>
  
  # Create descriptive names for the vessel count groups
  dplyr::mutate(vessel_cnt_group_name =
           dplyr::case_when(
             vessel_cnt_group == 1 ~
               paste0("<= 5 vessels (",
                      vessel_cnt_group_num,
                      " v)"),
             .default = paste0("> 5 vessels (",
                               vessel_cnt_group_num,
                               " v)")
           )) |>
  
  # Create groups for compliance percentages
  dplyr::mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 75))) |>
  
  # Count the number of entries in each compliance percentage group
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  
  # Create descriptive names for the compliance percentage groups
  dplyr::mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--75% non compliant ({percent_group_num} v.)"),
        percent_group == 3 ~ str_glue("75--98% non compliant ({percent_group_num} v.)")
      )
  )

# Count the number of entries in each vessel count group and sum the 'vessels_cnt' column
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

# Count the number of entries in each compliance percentage group and sum the 'vessels_cnt' column
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  # Create a ggplot plot with specified aesthetics
  # Define the x-axis using 'vessels_cnt'
  ggplot(aes(x = vessels_cnt, 
             # Define the y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add colored lines to the plot based on the 'vessel_cnt_group' factor
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  # Add data points to the plot with a dark blue color
  geom_point(color = "darkblue") +
  # Add text labels to the data points with a blue color
  # Display 'percent_compl' rounded to one decimal place as text labels
  # Adjust the vertical position of the labels
  # Set the text color to blue
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +
  # Set plot titles and labels
  # Set plot title
  # Set x-axis label
  # Set y-axis label
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # y axes 0 to 100
  ylim(0, 100)
  
ggplot(
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
  # Define the x-axis using 'vessels_cnt'
  aes(x = vessels_cnt)
) +
  # Create a histogram plot with specified bin width of 2
  geom_histogram(binwidth = 2)

# split by group all ----
count_weeks_per_vsl_permit_year_compl_p_short_count_gr <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  
  # Calculate the total count of distinct vessel official numbers
  dplyr::mutate(vessels_cnt_tot = n_distinct(vessel_official_number)) |>
  
  # Select all columns except 'vessel_official_number'
  dplyr::select(-vessel_official_number) |>
  
  # Remove duplicate rows from the data frame
  dplyr::distinct() |>
  
  # Create groups for vessel counts based on specified intervals
  dplyr::mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6, 450))) |>
  
  # Count the number of entries in each vessel count group and sum the 'vessels_cnt' column
  dplyr::add_count(vessel_cnt_group, wt = vessels_cnt, name = "vessel_cnt_group_num") |>
  
  # Create descriptive names for the vessel count groups
  dplyr::mutate(
    vessel_cnt_group_name =
      dplyr::case_when(
        vessel_cnt_group == 1 ~
          str_glue("{vessel_cnt_group}: 1--5 vessels ({vessel_cnt_group_num} v)"),
        vessel_cnt_group == 2 ~
          str_glue(
            "{vessel_cnt_group}: 6--450 vessels ({vessel_cnt_group_num} v)"
          ),
        vessel_cnt_group == 3 ~
          str_glue(
            "{vessel_cnt_group}: 451--500 vessels ({vessel_cnt_group_num} v)"
          )
      )
  ) |>
  
  # Create groups for compliance percentages based on specified intervals
  dplyr::mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 99))) |>
  
  # Count the number of entries in each compliance percentage group and sum the 'vessels_cnt' column
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  
  # Create descriptive names for the compliance percentage groups
  dplyr::mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant  ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--98% non compliant ({percent_group_num} v.)"),
        percent_group == 3 ~ str_glue("99--100% non compliant ({percent_group_num} v.)")
      )
  )

# This code takes the count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr data frame and uses the dplyr::count() function to count the number of entries in each vessel count group (vessel_cnt_group_name). The wt parameter is set to vessels_cnt to sum the 'vessels_cnt' column within each group. The result is a summary of counts for each unique vessel_cnt_group_name along with the sum of 'vessels_cnt' in each group.

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  # Define the x-axis using 'vessels_cnt'
  ggplot(aes(x = vessels_cnt,
             # Define the y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add colored lines based on 'vessel_cnt_group' factor
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  # Add data points in dark blue
  geom_point(color = "darkblue") +

  # Display 'percent_compl' rounded to one decimal place as text labels
  # Adjust the vertical position of the labels
  # Set the text color to blue
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +

  # Set plot title
  # Set x-axis label
  # Set y-axis label
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100)  
  

# facets ----
# p <- ggplot(mtcars, aes(mpg, wt)) +
#   geom_point() +
#   facet_wrap(~ cyl)
# 
# mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
# p + geom_hline(aes(yintercept = wt), mean_wt)
print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr)

# This code defines a variable named labs that contains a set of labels for a plot. The labels specify the plot title, x-axis label, and y-axis label. These labels are intended to be used when customizing the appearance of a plot in ggplot.

labs <- 
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls")

# Define a variable 'p' and create a ggplot plot using the data frame
# 'count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr'.
p <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  # Define the x-axis using 'vessels_cnt'
  # Define the y-axis using 'percent_compl'
  ggplot(aes(x = vessels_cnt,
             y = percent_compl)) +

  # Add data points in dark green
  geom_point(color = "darkgreen") +
  # facet_wrap(vars(vessel_cnt_group_name), scales = "free_x")
  # Create multiple subplots based on 'vessel_cnt_group_num' and 
  # 'percent_group_name' with labels for each subplot.
  facet_wrap(vars(vessel_cnt_group_num, percent_group_name), labeller = "label_both") +
  # Include custom labels previously defined in the 'labs' variable.
  labs

# Define a variable 'p' and create a ggplot plot using the data frame
# 'count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr'.
# Define the x-axis using 'vessels_cnt'
# Define the y-axis using 'percent_compl'
p <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  ggplot(
    aes(x = vessels_cnt,
        y = percent_compl)
  ) +
  # Add data points in dark green
  geom_point(color = "darkgreen") +
  facet_wrap(vars(percent_group_name), scales = "free_x") +
  # Create multiple subplots based on 'percent_group_name' with labels for each subplot.
  # Include custom labels previously defined in the 'labs' variable.
  labs  

# facet plots for all non compliant SA 2022 ----
# A variable labs_all is defined to store custom labels for a ggplot2 plot.
# The labs function is used to set the title, x-axis label, and y-axis label for the plot.
# The title is set to "Number of SA permitted vessels grouped by percent of non-compliant time in 2022".
# The x-axis label is set to "Vessel count".
# The y-axis label is set to "Percent of non-compliant in 2022".
# These custom labels will be used to annotate the ggplot2 plot and provide context and information to the viewer.

labs_all <- 
  labs(title = "Number of SA permitted vessels grouped by percent of non compliant time in 2022",
       x = "Vessel count",
       y = "Percent of non conmpliant in 2022")

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
# All percents ----
# This code is using the pipe operator (|>) to create a new data frame count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot based on the count_weeks_per_vsl_permit_year_compl_p_short_count_gr data frame. Here's what each line of code does:
# 
# count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot <-
# 
# This line initializes a new data frame called count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot.
# count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
# 
# The pipe operator (|>) takes the result of the expression on the left and passes it as the first argument to the function on the right.
# dplyr::group_by(vessel_cnt_group) |>
# 
# This line groups the data by the vessel_cnt_group column using the dplyr::group_by function.
# dplyr::mutate( max_in_vsl_group = max(vessels_cnt), min_in_vsl_group = min(vessels_cnt) ) |>
# 
# Within the grouped data, this line calculates two new columns:
# max_in_vsl_group: Computes the maximum value of the vessels_cnt column within each group.
# min_in_vsl_group: Computes the minimum value of the vessels_cnt column within each group, using the dplyr::mutate function.
# dplyr::ungroup()
# 
# This line ungroups the data, removing the grouping structure created earlier by the dplyr::group_by function.
# So, the code is essentially creating a new data frame that groups the original data by vessel_cnt_group, calculates the maximum and minimum values of vessels_cnt within each group, and then ungroups the data. The resulting data frame is stored in count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot.
# 

count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  dplyr::group_by(vessel_cnt_group) |>
  dplyr::mutate(
    max_in_vsl_group = max(vessels_cnt),
    min_in_vsl_group = min(vessels_cnt)
  ) |>
  dplyr::ungroup()

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot)

## All by vessel count ---

# Create a ggplot visualization using the pipe operator and the dataset.
count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  # Initialize the ggplot object with aesthetics mapping for x, y, and point size.
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  # Add points to the plot with a dark red color.
  geom_point(color = "darkred") +
  # Create facets in the plot based on the levels of the "vessel_cnt_group_name" variable
  # with independent x-axis scales.
  facet_wrap(vars(vessel_cnt_group_name), scales = "free_x") +
  # Add additional labeling and styling to the plot (the specifics of 'labs_all' are not provided).
  labs_all +
  # Customize the x-axis scale with specific tick marks based on data from another dataset.
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(
      max(
        count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
      )
    ))
  ))


## All By percent ----

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
plot_all_by_percent <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  geom_point(color = "darkred") +
  # ggplot2::facet_grid(
  #   cols = vars(percent_group_name),
  #   scales = "free_x",
  #   space = "free_x",
  #   margins = "vessels_cnt"
  # ) +
facet_wrap(vars(percent_group_name),
           scales = "free_x",
           nrow = 1) +
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group)))
  )) +
  labs_all +
  labs(cex = "Vsl num")

plot_all_by_percent

ggsave(
  file = "sa_22_nc_perc_vsl_cnt_by_percent.png",
  plot = plot_all_by_percent,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 40,
  height = 20,
  units = "cm"
)

# 100% non compliant out of total ----
count_weeks_per_vsl_permit_year_compl_p_short_count_tot <- 
  count_weeks_per_vsl_permit_year_compl_p_short |> 
  dplyr::filter(year_permit == "2022 sa_only") |> 
  dplyr::select(vessel_official_number, compliant_, percent_compl) |> 
  dplyr::add_count(compliant_, percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count_tot, 2)
#   vessel_official_number compliant_ percent_compl vessels_cnt
#   <chr>                  <chr>              <dbl>       <int>
# 1 VI5498TB               YES                  100         990
# 2 VA9236AV               NO                   100         487

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot |>
  dplyr::mutate(total_vessels = n_distinct(vessel_official_number)) |> 
  # dplyr::mutate(percent_compl_compl = ) |> 
  dplyr::mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100))) |> 
  # dplyr::group_by(perc_nc_100_gr, compliant_) |> str()
  dplyr::mutate(perc_nc_100_gr_name =
      dplyr::case_when(!!never_reported_filter ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |> 
  dplyr::mutate(group_100_vs_rest =
      dplyr::case_when(!!never_reported_filter ~
                  1,
                .default = 2)
  ) |> 
  dplyr::group_by(perc_nc_100_gr_name) |>
  dplyr::mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  dplyr::mutate(
    perc_of_perc =
          group_vsl_cnt * 100 / total_vessels
  ) |>
  dplyr::ungroup()

glimpse(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
nc_sa_22_tot_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc |>
  dplyr::select(group_100_vs_rest,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  dplyr::distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(group_100_vs_rest))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "2" = "skyblue1",
        "1" = "#0570B0"
      ),
    # Legend title
    name = "Non compliant",
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$perc_nc_100_gr_name)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # no x and y titles for individual plots
  labs(title = 
         stringr::str_glue("Never reported SA vsls in 2022 out of all compliant and non compliant (total vsls = {count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$total_vessels})"),
       y = "",
       # y = "% of All Vessels",
       x = "") +
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
# Add percent numbers on the bars
nc_sa_22_tot_100_plot <-
  nc_sa_22_tot_100_plot +
  geom_text(aes(label =
                  paste0(round(perc_of_perc, 0), "%")),
            # in the middle of the bar
            position =
              position_stack(vjust = 0.5),
            size = text_sizes[["geom_text_size"]])

nc_sa_22_tot_100_plot

ggsave(
  file = "sa_22_tot_100nc_plot.png",
  plot = nc_sa_22_tot_100_plot,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 20,
  height = 10,
  units = "cm"
)



                                                                                                                            


#### Current file: quantify_compliance_from_fhier_month.R ----

# Per month, region ----
# super_title_per_m = "% non-compliant weeks per month for non-compliant vessels by permit type (2022)"

# by Month: ----
## add tot cnts per month, permit ----

compl_clean_sa_vs_gom_m_int_filtered_tot_m <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::group_by(year_permit, year_month) %>%
  # count distinct vessels per group
  dplyr::mutate(total_vsl_m = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

# View(compl_clean_sa_vs_gom_m_int_filtered)

### test tot month ----
compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  dplyr::filter(year == "2022") %>%
  dplyr::select(year_permit, year_month, total_vsl_m) %>%
  dplyr::arrange(year_month, year_permit) %>%
  unique() %>%
  tail()
# 1 2022 gom_dual Oct 2022          1167
# 2 2022 sa_only  Oct 2022          1722
# 3 2022 gom_dual Nov 2022          1152
# 4 2022 sa_only  Nov 2022          1677
# 5 2022 gom_dual Dec 2022          1131
# 6 2022 sa_only  Dec 2022          1657
# numbers are as before, ok
# 1 2022 gom_dual Oct 2022          1144
# 2 2022 sa_only  Oct 2022          1695
# 3 2022 gom_dual Nov 2022          1138
# 4 2022 sa_only  Nov 2022          1656
# 5 2022 gom_dual Dec 2022          1123
# 6 2022 sa_only  Dec 2022          1647


## add the difference between expiration and week_start----

# If we use a week_end, than a vessel which ends near the end of year will have its last week expired.
compl_clean_sa_vs_gom_m_int_c_exp_diff <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  # add a column with difference in days
  dplyr::mutate(exp_w_end_diff =
                  as.numeric(as.Date(permitgroupexpiration) - week_start + 1))

## expired or not? ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff %>%
  # add a column
  dplyr::mutate(perm_exp_m =
                  dplyr::case_when(exp_w_end_diff < 0 ~ "expired",
                            exp_w_end_diff >= 0 ~ "active"))

## Keep active only ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
  filter(perm_exp_m == "active")

dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
# [1] 185251     28

dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp)
# [1] 185199     28

## expired: count if vessel is expired or not by year, permit and month  ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp %>%
  dplyr::group_by(year_permit, year_month, perm_exp_m) %>%
  # add a column counting distinct vessels per group
  dplyr::mutate(exp_m_tot_cnt = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

# check
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  dplyr::filter(year == "2022") %>%
  dplyr::select(year_permit,
                year_month,
                perm_exp_m,
                exp_m_tot_cnt,
                total_vsl_m) %>%
  unique() %>%
  dplyr::arrange(year_permit, year_month) %>%
  tail() |>
  dplyr::glimpse()
# year_permit  year_month perm_exp_m exp_m_tot_cnt total_vsl_m
#   <chr>        <yearmon>  <chr>              <int>       <int>
# 1 2022 sa_only Oct 2022   active              1721        1722
# 2 2022 sa_only Oct 2022   expired                1        1722
# 3 2022 sa_only Nov 2022   active              1676        1677
# 4 2022 sa_only Nov 2022   expired                1        1677
# 5 2022 sa_only Dec 2022   active              1656        1657
# 6 2022 sa_only Dec 2022   expired                1        1657
# compare with the text for tot month above
# rm exp
# $ year_permit   <chr> "2022 sa_only", "2022 sa_only", "2022 sa_only", "2022 sa_only", "…
# $ year_month    <yearmon> Jul 2022, Aug 2022, Sep 2022, Oct 2022, Nov 2022, Dec 2022
# $ perm_exp_m    <chr> "active", "active", "active", "active", "active", "active"
# $ exp_m_tot_cnt <int> 1745, 1755, 1708, 1694, 1655, 1646
# $ total_vsl_m   <int> 1746, 1756, 1709, 1695, 1656, 1647

# from now on use exp_m_tot_cnt instead of total_vsl_m

#### how many are expired ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
  filter(perm_exp_m == "expired") |>
  select(perm_exp_m, exp_m_tot_cnt) |>
  dplyr::distinct()
# 1 expired                1
# 0

compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
  # filter(perm_exp_m == "expired" &
  #          !year_month == "Dec 2022") |>
  # dplyr::glimpse()
  filter(vessel_official_number == "1000164" &
           year_month == "Nov 2022") |>
  dim()
# 0

#### check if expired and active permit is in the same month
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
#   dplyr::group_by(vessel_official_number, year_month) |>
#   mutate(active_or_expired = paste(sort(unique(perm_exp_m)),
#                                    collapse = " & ")) |>
#   filter(grepl("&", active_or_expired)) |>
#   dim()
  # 0

## cnt distinct total vessels per year, permit, month, compl ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  dplyr::group_by(year_permit, year_month, compliant_) %>%
  # add a column
  dplyr::mutate(cnt_vsl_m_compl = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

### test tot cnts per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  dplyr::select(
    year_permit,
    year_month,
    perm_exp_m,
    exp_m_tot_cnt,
    total_vsl_m,
    compliant_,
    cnt_vsl_m_compl
  ) %>%
  unique() %>%
  dplyr::filter(year_month == "Jan 2022") %>%
  dplyr::glimpse()
# toc()
# $ year_month      <yearmon> Jan 2022, Jan 2022, Jan 2022, Jan 2022
# $ perm_exp_m      <chr> "active", "active", "active", "active"
# $ exp_m_tot_cnt   <int> 1635, 1635, 1192, 1192
# $ total_vsl_m     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760 is more than total. Some vessels can be both in a month, if compliance differs by week. For this analysis I used vessels having at least one week in the month  non-compliant.
# If we are going to use "yes only" than redo "yes, no, no_yes" division as for a year above.
# $ cnt_vsl_m_compl <int> 1052, 688, 1004, 42

## add counts of weeks per vessel by month, compl ----
count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  ungroup %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  # select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  # unique() %>%
  filter(year_month == "Dec 2022") %>%
  dplyr::glimpse()
# Rows: 11,031
# $ compliant_                         <chr> "YES", "NO", "YES", "YES",…
# $ total_vsl_m                        <int> 1657, 1657, 1657, 1657, 16…
# $ perm_exp_m                         <chr> "active", "active", "activ…
# $ exp_m_tot_cnt                      <int> 1656, 1656, 1656, 1656, 16…
# $ cnt_vsl_m_compl                    <int> 1282, 434, 1282, 1282, 434…
# $ weeks_per_vessel_per_compl_m       <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  filter(year_permit == "2022 sa_only" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  dplyr::glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

## 1) Month: percent compl weeks per vsl per month ----

count_weeks_per_vsl_permit_year_compl_m_p <-
  count_weeks_per_vsl_permit_year_compl_month %>%
  dplyr::mutate(percent_compl_m =
                  weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_per_compl_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(year_month == "Dec 2022") %>%
  filter(vessel_official_number == "NJ8126HN") %>%
  select(
    vessel_official_number,
    year_month,
    compliant_,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m
  ) %>%
  unique() %>%
  dplyr::arrange(year_month) %>%
  dplyr::glimpse()
# $ compliant_                         <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl_m       <int> 1, 3
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 25, 75

## 2a) Month: Only non-compl and fewer cols ----
# View(count_weeks_per_vsl_permit_year_compl_m_p)
count_weeks_per_vsl_permit_year_compl_m_p_nc <-
  count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    year_month,
    vessel_official_number,
    perm_exp_m,
    exp_m_tot_cnt,
    cnt_vsl_m_compl,
    # total_vsl_m,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc)
## 2b) Month: get percentage "buckets" ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b <-
  # Use F2 to see the function definition
  get_p_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

### check 2, by month ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  filter(percent_n_compl_rank == "75<= & <=100%") %>%
  filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA9236AV") %>%
  dplyr::add_count(percent_compl_m, year_permit,
                   name = "amount_of_occurences") %>%
  # sort in the descending order
  dplyr::arrange(desc(percent_compl_m)) %>%
  # sum
  dplyr::add_count(wt = amount_of_occurences) %>%
  dplyr::glimpse()
# $ amount_of_occurences         <int> 12, 12, 12, 12, 12, 12, 12, 12…
# $ n                            <int> 144, 144, 144, 144, 144, 144, …

### add 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 <-
  # Use F2 to see the function definition
  get_2_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b2)

## 3) Month: count how many in each bucket ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)
# [1] 11489    12
# [1] 11477    12

### 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_non_compl_2_buckets,
                   name = "cnt_v_in_bucket2")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
# check by counting in a different way
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  dplyr::group_by(year_permit,
                  year_month,
                  percent_n_compl_rank) %>%
  dplyr::mutate(cnt_v_in_bucket1 =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::filter(!(cnt_v_in_bucket == cnt_v_in_bucket1)) %>%
  dim()
# 0 - correct, no difference

### tests 3, by month ----
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot)

test_compare_with <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# total 703 nc vsls in "Jan 2022 sa_only"
# tot 1635 in Jan 2022
#
# 45 nc vsls in "Jan 2022 gom_dual"
# 45 * 100 / 1192 = 3.8%

# 688

# still true?
test_res <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
#  703 1

test_compare_with[1] == test_res[1]
# TRUE

## 4) Month: cnt percents of (3) ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### 2 buckets ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_m_b2 = cnt_v_in_bucket2 * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_m_b2, 0), "%"))

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p)

### test 4, by month ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Dec 2022") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
# Dec 2022
# 1 25<= & <50%                         2.30
# 2 50<= & <75%                         4.15
# 3 75<= & <=100%                      93.5

# Jan 2022
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0<= & <25%                          4.69
# 2 25<= & <50%                         4.13
# 3 50<= & <75%                         4.13
# 4 75<= & <=100%                      87.1

# 612*100/703 == 87.05548

## 5) Month plots ----

## 5a) prepare the df for plotting ----
### keep only fields needed to plot ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  dplyr::distinct()

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short <-
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p |> 
    select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  dplyr::distinct()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)

### add column with Month name only (for plotting) ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

# check
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
# [1] 11766    15
# [1] 11489    15
# [1] 11477    15

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)
# [1] 107  12
# [1] 95 12

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)
# [1] 58 10

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)

### split the df by year_permit into a list ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short$year_permit))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2 <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short$year_permit))

### get used year_permits ----
sorted_year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2022 sa_only"  "2023 sa_dual"

### make titles ----
get_year_permit_titles <- function(permit, year) {
  # paste0("The Number of Non-Compliant Vessels Each Month That Were Compliant More Than 50% of a Month in ", year) |> 
        paste0("% of non-compliant ",
             permit,
             " Permitted vessels by month",
             " (", year, ")"
             ) %>%
    # "% of non-compliant ",
    #      permit,
    #      " Permitted vessels by month",
    #      " (",
    #      year,
    #      ")") %>%
    return()
}

year_permit_titles <-
  data.frame(
    super_title_gom = get_year_permit_titles("Gulf + Dual", "2022"),
    super_title_sa = get_year_permit_titles("South Atlantic Only", "2022"),
    super_title_2023 = get_year_permit_titles("South Atlantic + Dual", "2023")
  )

names(year_permit_titles) <- sorted_year_permits

### additional functions for Month plots ----
# TODO: simplify
# returns 0 or number of expired permits
get_expired_permit_numbers <- function(curr_data) {
  # browser()

  exp_filt <- curr_data %>%
    filter(perm_exp_m == "expired") %>%
    unique()

  res <- exp_filt$exp_m_tot_cnt

  # if filter(perm_exp_m == "expired") returned nothing
  if (dim(exp_filt)[1] == 0) {
    res <- 0
  }

  return(res)
}

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)

    curr_month_name <- unique(curr_data$month_only)

    curr_year_permit <- unique(curr_data$year_permit)

    curr_tot_v_per_m_y_r <- unique(curr_data$cnt_vsl_m_compl)

    curr_m_tot_active <- curr_data %>%
      filter(perm_exp_m == "active") %>%
      select(exp_m_tot_cnt) %>%
      unique()

    # see function definition F2
    cnt_expired <- get_expired_permit_numbers(curr_data)

    # curr_title <- paste0(
    #   curr_month_name,
    #   " (",
    #   curr_tot_v_per_m_y_r,
    #   " vsls; ",
    #   curr_m_tot_active$exp_m_tot_cnt,
    #   " act. p.; ",
    #   cnt_expired,
    #   " exp. p.)"
    # )

    # curr_title <- curr_month_name

    curr_title <- paste0(
      curr_month_name,
      " (",
      curr_tot_v_per_m_y_r,
      " total non-compliant vsls)"
      )
    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           # no labels for axes
           x = "",
           y = "") +
      # text on each bar
      geom_text(aes(label = perc_labels),
                # posintion - middle
                position = position_stack(vjust = 0.5)) +
      # Y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 10))

    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_permits %>%
  purrr::map(
    # for each year and permit pull a df from the list
    function(current_year_permit) {
      # browser()
      curr_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[[current_year_permit]]

      curr_year_months <-
        curr_df %>%
        dplyr::select(year_month) %>%
        unique() %>%
        as.data.frame()

      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # run the function for each month
        # see the function definition F2
        purrr::map(~ get_one_plot_by_month(curr_df,
                                            curr_year_month = .))
      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    })

footnote_text <- "In parenthesis are 1) # of non compliant vessels per month; 2) total active permits per month; 3) total expired permits per month;"

# footnote <- textGrob(
#   footnote_text,
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -0.5,
#   # just = "right",
#   x = 0.01,
#   y = 0.99,
#   vjust = 1
# )

### common axes for Months ----
y_left <- textGrob("% per 'bucket'",
                   # angle
                   rot = 90,
                   gp = gpar(fontsize = 10))

x_bottom <-
  textGrob("'buckets' - distibution of % of non compliant weeks per vessel",
           gp = gpar(fontsize = 10))

all_plots_w_titles_list <-
  gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    # get a name
    curr_year_permit <- curr_year_reg_list[[1]]

    # get a title by the name
    curr_super_title <- year_permit_titles[[curr_year_permit]]

    # add a subtitle
    whole_title <- curr_super_title
    # whole_title <-
    #   paste0(curr_super_title,
    #          # new line
    #          "\n",
    #          footnote_text)

    all_plots_per_year_region <-
      gridExtra::arrangeGrob(
        grobs =
          curr_year_reg_list[[2]],
        top = whole_title,
        left = y_left,
        bottom = x_bottom,
        ncol = 3
      )

    # combine the current year_permit and the plots in a list
    res <- list(curr_year_permit,
                all_plots_per_year_region)

    return(res)
  })

# warnings()

# draw one plot to test
gridExtra::grid.arrange(all_plots_w_titles_list[[2]][[2]])

# View(all_plots_w_titles_list)

## all plots per month to files ----
# saves to PNG, PDF etc. depending on an extension in "file_full_name"
save_plots_list_to_files <-
  function(file_full_name,
           plots_list) {
    ggplot2::ggsave(
      file_full_name,
      plots_list,
      width = 30,
      height = 20,
      units = "cm"
    )
  }

# add dir
plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

all_plots_w_titles_list %>%
  # repeat for each element of the list
  purrr::map(function(curr_plot_list) {
    file_name_base <- paste0(curr_plot_list[[1]],
                             "_percent_distribution_per_month",
                             ".png")

    # file.path adds the correct concatenation
    file_full_name <- file.path(plot_file_path_m,
                                file_name_base)

    # see the function definition F2
    save_plots_list_to_files(file_full_name,
                             # plots
                             curr_plot_list[[2]])
  })

# "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance/2023-09-01/per_month/2023 sa_dual_percent_distribution_per_month.png"

# [[1]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\08_26_2023\\per_month/2022 gom_dual_percent_distribution_per_month.png"...



                                                                                                                            


#### Current file: quantify_compliance_from_fhier_vms.R ----

# Above compliance metrics, to assess pre and post VMS requirement or vs increase in VMS ----
# compliance (just Gulf + dual permitted vessels; assess Feb 2022 (=pre-t), March 2022 (VMS implementation), and Sept 2022 (when 80% vessels had a registered VMS))

compl_clean_sa_vs_gom_m_int_filtered |>
  select(year_month) |>
  dplyr::distinct()
# dim(compl_clean_sa_vs_gom_m_int_filtered)

compl_clean_sa_vs_gom_m_int_filtered_vms <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(year_permit == "2022 gom_dual" &
           year_month %in% c("Feb 2022",
                             "Mar 2022",
                             "Sep 2022"))
dim(compl_clean_sa_vs_gom_m_int_filtered_vms)
# [1] 12677    25

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered_vms, "year_month")
# dplyr::glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt %>%
  select(year_month, total_vsl_y) %>%
  unique()
# 1 Sep 2022          1144
# 2 Mar 2022          1031
# 3 Feb 2022          1034

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

# dplyr::glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp)

group_by_var <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp,
                      group_by_var)

# dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    year_month,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short)
# [1] 3319    7

## get compl_counts ----
### get compl, no compl, or both per period ----
group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short,
               group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide)
# [1]    6 1264

### count compl, no compl, or both per period, permit, active status ----
cols_names <-
  c("year_permit", "year_month", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide,
                cols_names)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long)
# [1] 7554    7

### get cnts for compl, no compl, or both per month with exp ----
group_by_cols <- c("year_month", "perm_exp_y")
cols_to_cnt <- c("year_month", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt)
# [1] 23  7

### check if a vessel is compliant and not at the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::glimpse()
# $ year_month             <yearmon> Sep 2022, Mar 2022, Feb 2022
# $ total_vsl_y            <int> 1144, 1031, 1034
# $ perm_exp_y             <chr> "expired", "expired", "expired"
# $ exp_y_tot_cnt          <int> 62, 108, 112
# $ vessel_official_number <chr> "657209", "657209", "657209"
# $ is_compl_or_both       <chr> "YES", "NO_YES", "NO"
# $ shared                 <lgl> TRUE, TRUE, TRUE

  # dim()
# 3
# TODO: fix

### check if a vessel permit is expired and not in the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dim()
# 0 ok

### check total_vsl_y vs. sum_cnts ----
compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1249 YES
# 2        117 NO
# 1249 + 117 = 1366
# TODO: what to compare with?

compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  dplyr::group_by(perm_exp_y) %>%
  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1140 YES        active
# 2        119 YES        expired
# 3       1140 NO         active
# 4        119 NO         expired
# 1140 + 119
# 1259
# TODO: what does it mean?

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration
group_by_compl_cols <- c("year_month", "compl_or_not")
group_by_exp_cols <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt,
    group_by_compl_cols,
    group_by_exp_cols
  )

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot)
# [1] 17 10

## add percents of total ----
select_cols <- c(
  "year_month",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc)
# [1] 12  7

# plots VMS:
gg_all_c_vs_nc_plots_vms <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc$year_month %>%
  unique() %>%
  sort() %>%
  # repeat for each year_month
  purrr::map(function(curr_year_month) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc %>%
      dplyr::filter(year_month == curr_year_month)

    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_month)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # current_title <-
    #   paste0(
    #     "GOM + Dual",
    #     " ",
    #     curr_year_month,
    #     " (Total Permitted: ",
    #     total_vsls,
    #     "; Expired Permits: ",
    #     expired_permits$cnt_y_p_e,
    #     ")"
    #   )

    current_title <-
      paste0(
        "GOM + Dual",
        " ",
        curr_year_month
      )

    # current_title <-
    #   paste0(
    #     "GOM + Dual",
    #     " ",
    #     curr_year_month,
    #     " (Total Permitted: ",
    #     total_vsls,
    #     ")"
    #   )

    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc",
                                       default_percent_labels = FALSE)

    return(one_plot)
  })

main_title_vms <- "Percent Compliant vs. Noncompliant SEFHIER Vessels (VMS)"

# combine plots for 2022
gg_arranged_plots_vms <-
  grid.arrange(
    gg_all_c_vs_nc_plots_vms[[1]],
    gg_all_c_vs_nc_plots_vms[[2]],
    gg_all_c_vs_nc_plots_vms[[3]],
    top = main_title_vms
  )
# class(gg_all_c_vs_nc_plots_vms)

vms_plot_file_path <-
  file.path(plot_file_path, "vms")
create_dir_if_not(vms_plot_file_path)

## save VMS green and red plots ----
save_plots_list_to_files(file.path(vms_plot_file_path,
                                   "vms_3_months.png"),
                         gg_arranged_plots_vms)

# gg_all_c_vs_nc_plots_vms |>
#   purrr::map(function(current_plot) {
#     # create a clean_name
#     # browser()
#     clean_name <-
#       stringr::str_replace_all(current_plot$labels$title,
#                                "[^a_zA-z0-9]+", "_")
#     save_plots_list_to_files(file.path(vms_plot_file_path,
#                                        paste0(clean_name, ".png")),
#                              # plots
#                              current_plot)
#   })


# Non compliant only ----
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_year_month_vms_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt |>
  # compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::add_count(year_month, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year_month, vessel_official_number, name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

dim(weeks_per_vsl_year_month_vms_compl_cnt)
# [1] 12677    31

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
weeks_per_vsl_year_month_vms_compl_cnt_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc)
# [1] 12677    32

# check
weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  filter(vessel_official_number == "FL3327TJ") %>%
  select(
    year_month,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ year_month                 <yearmon> Sep 2022, Sep 2022
# $ compliant_                 <chr> "NO", "YES"
# $ weeks_per_vessel_per_compl <int> 1, 3
# $ total_weeks_per_vessel     <int> 4, 4
# $ percent_compl              <dbl> 25, 75

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

weeks_per_vsl_year_month_vms_compl_cnt_perc_short <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_month,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short)
# [1] 159   7

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts <-
  get_p_buckets(weeks_per_vsl_year_month_vms_compl_cnt_perc_short,
                "percent_compl")

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts)
# [1] 159   8

### test 2 ----
# count in one bucket
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_month == "Mar 2022") %>%
  dplyr::count(percent_compl, year_month,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # dplyr::glimpse()
  # $ percent_compl        <dbl> 100, 75
  # $ year_month           <yearmon> Mar 2022, Mar 2022
  # $ amount_of_occurences <int> 18, 2

  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 55 all
# 20 March

# 3) count how many in each bucket ----

  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b <-
    weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
    dplyr::add_count(year_month,
                     percent_n_compl_rank,
                     name = "cnt_v_in_bucket")

### test 3 ----
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
    # dplyr::filter(year_permit == "2022 sa_only") %>%
    dplyr::select(year_month,
                  percent_n_compl_rank,
                  cnt_v_in_bucket) %>%
    unique() %>%
    dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_period") %>%
    dplyr::arrange(percent_n_compl_rank) %>%
    dplyr::glimpse()
# $ year_month           <yearmon> Sep 2022, Mar 2022, Feb 2022, Sep 2022, Mar 202…
# $ percent_n_compl_rank <chr> "25<= & <50%", "25<= & <50%", "25<= & <50%", "50<= …
# $ cnt_v_in_bucket      <int> 26, 32, 26, 6, 8, 6, 15, 20, 20
# $ total_per_period     <int> 159, 159, 159, 159, 159, 159, 159, 159, 159

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
  # cnt vessels per period and compliance
  dplyr::add_count(year_month,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()

# 1 25<= & <50%                         55.3
# 2 25<= & <50%                         53.3
# 3 25<= & <50%                         50
# 4 50<= & <75%                         12.8
# 5 50<= & <75%                         13.3
# 6 50<= & <75%                         11.5

# 5) blue plots by year ----

blue_year_plot_titles <-
  data.frame(
    first_part = c(
      "GOM + Dual Permitted Vessels\n("
    )
  )

gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc$year_month %>%
  unique() %>%
  sort() %>%
  # repeat for each year_month
  purrr::map(function(curr_year_month) {
    # browser()
    curr_df <-
      weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_month == curr_year_month)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    # curr_title_y_p <- make_year_month_label(curr_year_month)

    # curr_blue_year_plot_title <-
    # blue_year_plot_titles %>%
    # filter(year_month == curr_year_month)

    # y_p_title <-
    #   paste0(
    #     curr_year_month,
    #     " (Total Non-Compliant = ",
    #     total_non_compl_df$vsls_per_y_r,
    #     " Vessels; Acitve permits = ",
    #     active_permits$exp_y_tot_cnt,
    #     "; Expired permits: ",
    #     expired_permits$exp_y_tot_cnt,
    #     " Vessels)"
    #   )

    # y_p_title <-
    #   paste0(
    #     curr_year_month,
    #     " (Total Non-Compliant = ",
    #     total_non_compl_df$vsls_per_y_r,
    #     " Vessels)"
    #   )

    y_p_title <-
      paste0(
        curr_year_month,
        " (Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 10))

    return(one_plot)
  })

# main_blue_title <- "% non compliant vessels per period"
ndash <- "\u2013"
main_blue_title <- paste0(
  "% Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)


grid.arrange(gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[1]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[2]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[3]],
             top = main_blue_title)



#### add-ons 3 ---- 


# Create a read.me file with numbers of total, active and expired ----
## by year ----
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc defined in quantify_compliance_from_fhier_year.R
# Create a new dataset "year_permit_cnts" by performing a series of operations.

year_permit_cnts <-
  # Extract unique "year_permit" values from the specified column and sort them.
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  sort() |>

  # For each unique "year_permit", apply a function using "purrr::map_df".
  purrr::map_df(function(curr_year_permit) {
    # Create a subset "curr_df" of the original dataset for the current "year_permit".
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # Extract unique "total_vsl_y" values for the current "year_permit".
    total_vsls <- unique(curr_df$total_vsl_y)

    # Extract and create a subset of data for "active" permits.
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Extract and create a subset of data for "expired" permits.
    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Create a data frame "out_df" with relevant information.
    out_df <- as.data.frame(c(curr_year_permit, total_vsls, active_permits, expired_permits))
    names(out_df) <- c("year_permit", "total", "active_permits", "expired_permits")

    return(out_df)
  })

# View(year_permit_cnts)

# # 2 ) - not needed, gets non compliant numbers
# count_year1 <-
#   count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
#   unique() %>%
#   sort() %>%
#   # repeat for each year_permit
#   purrr::map_df(function(curr_year_permit) {
#     curr_df <-
#       count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
#       dplyr::filter(year_permit == curr_year_permit)
# 
#     total_non_compl_df <-
#       curr_df %>%
#       dplyr::select(vsls_per_y_r) %>%
#       dplyr::distinct()
#     # browser()
# 
#     active_permits <- curr_df %>%
#       dplyr::filter(perm_exp_y == "active") %>%
#       dplyr::select(exp_y_tot_cnt) |>
#       dplyr::distinct()
# 
#     expired_permits <- curr_df %>%
#       dplyr::filter(perm_exp_y == "expired") %>%
#       dplyr::select(exp_y_tot_cnt) |>
#       dplyr::distinct()
# 
#     out_df <-
#       as.data.frame(c(
#         curr_year_permit,
#         total_non_compl_df,
#         active_permits,
#         expired_permits
#       ))
#     names(out_df) <-
#       c("year_permit", "total", "active_permits", "expired_permits")
# 
#     return(out_df)
#   })

# glimpse(year_permit_cnts)
# glimpse(count_year1)
# all.equal(year_permit_cnts, count_year1)
# [1] "Component “total”: Mean relative difference: 0.4411713"

## 3) by month ----
# View(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
counts_by_month_read_me <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
  dplyr::group_by(year_month, year_permit, perm_exp_m) |>
  dplyr::mutate(permit_cnt_m =
           n_distinct(vessel_official_number)) |>
  dplyr::ungroup() |>
  dplyr::select(year_permit, year_month, total_vsl_m, perm_exp_m, permit_cnt_m) |>
  dplyr::distinct()

# print_df_names(counts_by_month_read_me)

counts_by_month_read_me_clean <-
  counts_by_month_read_me |>
  tidyr::pivot_wider(
    id_cols = c(year_permit, year_month, total_vsl_m),
    names_from = perm_exp_m,
    values_from = permit_cnt_m,
    names_glue = "{perm_exp_m}_permits",
    values_fill = 0
  ) |>
  dplyr::arrange(year_month)

# View(counts_by_month_read_me_clean)

## by year another way ----
# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)

# 38

counts_by_year_read_me_clean <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc |>
  dplyr::select(-perc_c_nc) |>
  dplyr::distinct() |>
  tidyr::pivot_wider(names_from = compl_or_not,
                     values_from = cnt_y_p_c,
                     values_fill = 0) |>
  tidyr::pivot_wider(
    # not needed, by default used all but names and values columns
    # id_cols = -c("perm_exp_y", "perm_exp_y"),
    names_from = perm_exp_y,
    values_from = cnt_y_p_e,
    names_glue = "{perm_exp_y}_permits",
    values_fill = 0
  )

glimpse(counts_by_year_read_me_clean)

