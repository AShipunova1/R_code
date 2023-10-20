

#### Current file: ~/R_code_github/useful_functions_module.r ----

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
  contents <- map_df(myfiles, ~read_excel(
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

## ---- functions to clean FHIER compliance and correspondense reports ----

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
    arrange(group_by_arr[1]) %>%          # Arrange the data by the first column in 'group_by_arr'.
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
    case_when(
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

    # ---- Specific correspondence manipulations ----
    # Perform cleaning and processing specific to correspondence data.
    corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

    ## ---- Specific compliance manipulations ----
    # Extract compliance data from the cleaned CSVs.
    compl_arr <- csvs_clean1[2:length(csvs_clean1)]

    # Clean and process compliance data.
    compl_clean <- compliance_cleaning(compl_arr)

    # Return a list containing cleaned compliance and correspondence data.
    return(list(compl_clean, corresp_arr_contact_cnts_clean))
  }

# ---- specific correspondence manipulations ----
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

## ---- specific compliance manipulations ----
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
#   map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% group_by(test) %>% summarise(sum(elapsed))

# ====
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
# Define a function to create a flat file by combining the contents of multiple files.
make_a_flat_file <- function(flat_file_name, files_to_combine_list) {
  # Redirect the output to the specified 'flat_file_name'.
  sink(flat_file_name)

  # Loop through the list of 'files_to_combine_list'.
  for (i in 1:length(files_to_combine_list)) {
    # Read the contents of the current file.
    current_file <- readLines(files_to_combine_list[i])

    # Print a header indicating the current file being processed.
    cat("\n\n#### Current file:", files_to_combine_list[i], "----\n\n")

    # Print the contents of the current file, separating lines with newline characters.
    cat(current_file, sep = "\n")
  }

  # Restore the default output behavior.
  sink()
}

# ===
# Define a function to append the contents of a single file to an existing flat file.
write_to_1_flat_file <- function(flat_file_name, file_name_to_write) {
  # Redirect the output to the specified 'flat_file_name' and append content.
  sink(flat_file_name, append = TRUE)

  # Read the contents of the current file.
  current_file_text <- readLines(file_name_to_write)

  # Print a header indicating the current file being processed.
  cat("\n\n#### Current file:", file_name_to_write, "----\n\n")

  # Print the contents of the current file, separating lines with newline characters.
  cat(current_file_text, sep = "\n")

  # # Restore the default output behavior.
  # sink()
}

# Function to separate permit groups into three categories based on a specified field
separate_permits_into_3_groups <-
  function(my_df, permit_group_field_name = "permitgroup") {
    my_df %>%
      # Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group
      mutate(permit_sa_gom =
               case_when(
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
#       map_df(function(x) {
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
      coords = c("LONGITUDE", "LATITUDE"),
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



#### Current file: ~/R_code_github/useful_functions_module.r ----

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
  contents <- map_df(myfiles, ~read_excel(
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

## ---- functions to clean FHIER compliance and correspondense reports ----

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
    arrange(group_by_arr[1]) %>%          # Arrange the data by the first column in 'group_by_arr'.
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
    case_when(
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

    # ---- Specific correspondence manipulations ----
    # Perform cleaning and processing specific to correspondence data.
    corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

    ## ---- Specific compliance manipulations ----
    # Extract compliance data from the cleaned CSVs.
    compl_arr <- csvs_clean1[2:length(csvs_clean1)]

    # Clean and process compliance data.
    compl_clean <- compliance_cleaning(compl_arr)

    # Return a list containing cleaned compliance and correspondence data.
    return(list(compl_clean, corresp_arr_contact_cnts_clean))
  }

# ---- specific correspondence manipulations ----
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

## ---- specific compliance manipulations ----
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
#   map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% group_by(test) %>% summarise(sum(elapsed))

# ====
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
# Define a function to create a flat file by combining the contents of multiple files.
make_a_flat_file <- function(flat_file_name, files_to_combine_list) {
  # Redirect the output to the specified 'flat_file_name'.
  sink(flat_file_name)

  # Loop through the list of 'files_to_combine_list'.
  for (i in 1:length(files_to_combine_list)) {
    # Read the contents of the current file.
    current_file <- readLines(files_to_combine_list[i])

    # Print a header indicating the current file being processed.
    cat("\n\n#### Current file:", files_to_combine_list[i], "----\n\n")

    # Print the contents of the current file, separating lines with newline characters.
    cat(current_file, sep = "\n")
  }

  # Restore the default output behavior.
  sink()
}

# ===
# Define a function to append the contents of a single file to an existing flat file.
write_to_1_flat_file <- function(flat_file_name, file_name_to_write) {
  # Redirect the output to the specified 'flat_file_name' and append content.
  sink(flat_file_name, append = TRUE)

  # Read the contents of the current file.
  current_file_text <- readLines(file_name_to_write)

  # Print a header indicating the current file being processed.
  cat("\n\n#### Current file:", file_name_to_write, "----\n\n")

  # Print the contents of the current file, separating lines with newline characters.
  cat(current_file_text, sep = "\n")

  # # Restore the default output behavior.
  # sink()
}

# Function to separate permit groups into three categories based on a specified field
separate_permits_into_3_groups <-
  function(my_df, permit_group_field_name = "permitgroup") {
    my_df %>%
      # Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group
      mutate(permit_sa_gom =
               case_when(
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
#       map_df(function(x) {
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
      coords = c("LONGITUDE", "LATITUDE"),
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



#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_by_permit.R ----

# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

## functions for ten_min ----

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <-
    paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")
  convert_to_decimal_degree(dm_num)
}

get_ten_min_coords <- function(my_df) {
  ten_min_df <-
    my_df |>
    mutate(
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),
      # All lon should be negative, bc we know it is all in America
      ten_min_lon =
        -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )
  return(ten_min_df)
  # distinct(ten_min_df)
}

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good_all_coords <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 97970    17

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short_good_all_coords |>
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])




#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_by_permit.R ----

# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

## functions for ten_min ----

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <-
    paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")
  convert_to_decimal_degree(dm_num)
}

get_ten_min_coords <- function(my_df) {
  ten_min_df <-
    my_df |>
    mutate(
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),
      # All lon should be negative, bc we know it is all in America
      ten_min_lon =
        -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )
  return(ten_min_df)
  # distinct(ten_min_df)
}

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good_all_coords <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 97970    17

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short_good_all_coords |>
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])




#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\prepare_gom_heatmap_func.R ----

# Load the 'tigris' package to access geographic data.
library(tigris)

# Set the 'tigris_use_cache' option to TRUE. This will enable caching of
# data retrieved from the TIGER/Line Shapefiles service, which can help
# improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# plot text sizes ----
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 13,
  axis_text_y_size = 13,
  plot_caption_text_size = 13,
  legend_title_text_size = 10,
  legend_text_text_size = 10,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# read in sa shp ----
# F2 in RStudio will show the function definition, when the cursor is on the name.
# Read a shapefile (geospatial data) from the specified file path and store it in the 'sa_shp' object.
sa_shp <-
  read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    "Florida", # can exclude, if go by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )
)

# Create a data frame 'state_tbl' containing state abbreviations and state names; 2x50.
# - 'state.abb' provides state abbreviations.
# - 'tolower(state.name)' converts state names to lowercase.
# - The resulting data frame has two columns: 'state_abb' and 'state_name'.
state_tbl <- data.frame(state.abb, tolower(state.name))

# Rename the columns in the 'state_tbl' data frame.
# The first column is named 'state_abb', and the second column is named 'state_name'.
names(state_tbl) = c("state_abb", "state_name")

#from the DF, only grab the SA states defined above
sa_state_abb <-
  # a table from above
  state_tbl %>%
  # get only these in our list
  filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  select(state_abb)

# # add regions to the FHIER logbook DF
# fhier_logbooks_content_waves__sa_gom <-
#   fhier_logbooks_content_waves_fl_county %>%
#   # add a new column "end_port_sa_gom" with sa or gom for each state
#   # use fix_name aux function to unify state names (lower case, no spaces etc.)
#   mutate(end_port_sa_gom = case_when(
#     # if a name is in our SA list - "sa", otherwise - "gom"
#     fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
#     .default = "gom"
#   )) %>%
#   # go through the new column again
#   # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
#   # otherwise don't change
#   mutate(end_port_sa_gom = ifelse(
#     tolower(end_port_state) == "fl",
#     end_port_fl_reg,
#     end_port_sa_gom
#   )) %>%
#   # remove this column, we don't need it anymore
#   select(-end_port_fl_reg)

# #### test: check new cols of states and regions ----
# fhier_logbooks_content_waves__sa_gom %>%
#   # look at states and regions
#   select(end_port_state, end_port_sa_gom) %>%
#   unique() %>%
#   glimpse()

## r get Shapefile all waters ----
path_to_federal_state_w <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

file.exists(path_to_federal_state_w)
# T

tic("federal_state_w_sf")
federal_state_w_sf <-
  sf::read_sf(path_to_federal_state_w)
toc()

# rr <-
# federal_state_w_sf |>
#   sf::st_drop_geometry()
#
# rr$Jurisdicti |>
#   cat(sep = ", ")

east_coat_states <- c(
  gom = c("Florida",
          "Texas",
          "Louisiana"),
  sa = c(
    "Alabama",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Mississippi",
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
# nc_sql = sf::st_read(system.file("shape/nc.shp", package="sf"),
#                      query = "SELECT NAME, SID74, FIPS FROM \"nc\" WHERE BIR74 > 20000")

# Create a new data frame 'federal_state_w_sf_east' by filtering the existing data frame 'federal_state_w_sf'.
# Rows are retained if the 'Jurisdicti' column matches any of the values in 'east_coat_states'.
federal_state_w_sf_east <-
  federal_state_w_sf |>
  filter(Jurisdicti %in% east_coat_states)

# mapview(sa_shp)
# [1] 21  7

# coast line SA shp ----
# us_bb <-
#   tigris::counties(filter_by = big_bounding_box, progress_bar = FALSE)

# Create a new data frame 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes.
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE)

# Rows are retained if the 'NAME' column (state name) matches any of the values in 'states_sa'.
sa_s_shp <-
  us_s_shp |>
  filter(NAME %in% states_sa)

# sa_s_shp_plot <-
#   ggplot() +
#   geom_sf(data = sa_s_shp)

# sa_counties_shp <- tigris::counties(states_sa, cb = TRUE)

# gg <- ggplot()
# gg <- gg + geom_sf(
#   data = sa_s_shp,
#   color = "black",
#   # fill = "",
#   size = 0.25
# )
# gg

# library(rnaturalearth) #coastline
# library(maps)
# library(mapdata)


atl_shp_file <-
  file.path(my_paths$inputs,
            r"(shapefiles\ATL_SLA\ATL_SLA.shp)")

# Read a shapefile from the specified file path using the 'sf::read_sf' function.
# The resulting spatial data is stored in the 'atl_shp' object.
atl_shp <- sf::read_sf(atl_shp_file)

# Create a plot using 'ggplot2' with the 'atl_shp' spatial data.
# Use 'geom_sf' to display the shapes from 'atl_shp' with no fill (NA, i.e., transparent).
ggplot() +
  geom_sf(data = atl_shp, fill = NA)

# read in GOM shp ----
# Read a shapefile from the specified file path using 'sf::read_sf'.
# Then, group the resulting data by 'StatZone' and summarize it.
GOMsf <-
  sf::read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>%
  summarise()

# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create 5x5 minute grid ----
# Define a function 'min_grid' that creates a grid of cells within the bounding box of a given spatial data frame.
# - 'my_sf' is the input spatial data frame (default is 'GOMsf').
# - 'minute_num' specifies the grid cell size in minutes.

min_grid <- function(my_sf = GOMsf, minute_num = 1) {
  # Create a grid of cells using 'sf::st_make_grid' within the bounding box of 'my_sf'.
  grid <-
    sf::st_make_grid(x = sf::st_bbox(my_sf),
                     cellsize = 1 / 60 * minute_num) %>%

    # Convert the grid to a spatial data frame using 'sf::st_as_sf'.
    sf::st_as_sf() %>%

    # Add a 'cell_id' column to the grid using 'mutate'.
    mutate(cell_id = 1:nrow(.))

  # Return the created grid.
  return(grid)
}

grid_gom5 <- min_grid(GOMsf, 5)
grid_sa5 <- min_grid(sa_shp, 5)

# Set the aggregate attribute to "constant" for multiple spatial objects.
sf::st_agr(GOMsf) =
  sf::st_agr(sa_shp) =
  sf::st_agr(grid_gom5) =
  sf::st_agr(grid_sa5) =
  "constant"

### remove internal boundaries from the GOM shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- sf::st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## by n min grid ----
# Define a function 'df_join_grid' that joins a data frame with a grid using specified coordinates and CRS.

df_join_grid <- function(my_df, grid, my_crs) {
  # Convert 'my_df' to a spatial data frame with specified coordinates and CRS using 'sf::st_as_sf'.
  my_df |>
    sf::st_as_sf(
      coords = c("LONGITUDE", "LATITUDE"),
      crs = my_crs) |>

  # Join the resulting spatial data frame with the 'grid' using the nearest feature join.
  sf::st_join(grid, join = sf::st_nearest_feature) |>

  # Return the joined data frame.
  return()
}

# Define a function 'crop_by_shape' that crops a spatial object using another spatial object.
# - 'my_sf' is the input spatial object to be cropped.
# - 'my_shp' is the spatial object used for cropping (default is 'GOMsf').

crop_by_shape <- function(my_sf, my_shp = GOMsf) {
  # Join 'my_sf' with 'my_shp' to crop it, leaving only the intersecting geometries.
  my_sf |>
    sf::st_join(my_shp, left = FALSE) %>%

  # Extract the LONGITUDE and LATITUDE coordinates from the joined spatial object.
  dplyr::mutate(LONGITUDE = sf::st_coordinates(.)[, 1],
         LATITUDE = sf::st_coordinates(.)[, 2]) %>%

  # Return the cropped and transformed spatial object.
  return()
}

## count trip ids and vessels by grid cell ----
# Define a function 'add_vsl_and_trip_cnts' that adds vessel and trip counts to a data frame.
# - 'my_df' is the input data frame.
# - 'vessel_id_name' is the name of the column containing vessel IDs (default is "VESSEL_OFFICIAL_NBR").

add_vsl_and_trip_cnts <- function(my_df, vessel_id_name = "VESSEL_OFFICIAL_NBR") {
  # Group the data frame by 'cell_id'.
  my_df |>
    group_by(cell_id) |>

  # Add columns 'vsl_cnt' and 'trip_id_cnt' with counts of distinct vessel and trip IDs.
    # sym() take strings as input and turn them into symbols.
    # The !! (bang-bang or unquote) operator is used to unquote the symbol, allowing it to be used in dplyr verbs like mutate, select, or other functions that accept column names.
    # So, the code !!rlang::sym(vessel_id_name) effectively evaluates to the column name specified by the vessel_id_name variable in the context of a dplyr verb, allowing you to work with the column dynamically based on the variable's value.

    dplyr::mutate(
      vsl_cnt =
        dplyr::n_distinct(!!rlang::sym(vessel_id_name)),
      trip_id_cnt =
        dplyr::n_distinct(TRIP_ID)
    ) |>

  # Ungroup the data frame to remove grouping and return the result.
  dplyr::ungroup() %>%

  # Return the modified data frame.
  return()
}

## make a plot ----
# Define a function 'make_map_trips' to create a ggplot2 heatmap of trip data.
# - 'map_trip_base_data' is the data containing trip information to be mapped.
# - 'shape_data' is the shape data used as a backdrop for mapping.
# - 'total_trips_title' is the title for the total trips legend.
# - 'trip_cnt_name' is the name of the column with trip counts.
# - 'caption_text' is the caption for the plot.
# - 'unit_num' specifies the unit size for the legend.
# - 'print_stat_zone' is an optional argument to include StatZone labels.
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
           unit_num = 1,
           print_stat_zone = NULL
           ) {
    # Calculate the maximum number of trips for legend scaling.
    max_num <- max(map_trip_base_data[[trip_cnt_name]])

    # Create a ggplot2 plot 'map_trips'.
    map_trips <-
      ggplot() +
      # Add a filled heatmap using 'geom_sf'.
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      # Add the shape data with no fill.
      geom_sf(data = shape_data, fill = NA)

    # Check for an optional argument 'print_stat_zone'.
    if (!missing(print_stat_zone)) {
      map_trips <-
        map_trips +
        # Add StatZone labels using 'geom_sf_text'.
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5)
    }

    map_trips <-
        map_trips +
      # Set plot labels and theme settings.
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      theme_bw() +

      # Set fill scale properties.
      scale_fill_gradient2(
        name = total_trips_title,
        labels = scales::comma,
        low = "red",
        mid = "purple",
        high = "blue",
        # trans = "log2",
        trans = "log1p",
        limits = c(1, max_num)
        # ,
        # oob = scales::oob_keep
      ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(unit_num, "npc"),
        legend.title = element_text(size =
                                      text_sizes[["legend_title_text_size"]]),
        legend.text = element_text(size =
                                     text_sizes[["legend_text_text_size"]]), # for charter heatmap use 7
        plot.caption = element_text(hjust = 0,
                                    size = text_sizes[["plot_caption_text_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      # Add a legend guide for fill color.
      guides(fill = guide_colourbar(title.position = "top"))

    # Return the created 'map_trips' plot.
    return(map_trips)
  }


#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_heatmap.R ----

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# data are from "by_permit"

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

library(ggplot2) # a visualization package
library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.

# Heatmap ----

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

## heatmap data ----

# short_example_3_cnts_short |> glimpse()
dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual)
# [1] 41455     8

# glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual)

# for_heatmap_lat_lon_trips_only <-
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
#   select(TRIP_ID, LATITUDE, LONGITUDE) |>
#   distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

# gom
for_heatmap_lat_lon_trips_vessels_gom_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_gom_only)
# Rows: 41,455

# sa
for_heatmap_lat_lon_trips_vessels_sa_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$sa_only |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_sa_only)
# [1] 68122     4

### remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
for_heatmap_lat_lon_trips_vessels_sa_only_rm <-
  for_heatmap_lat_lon_trips_vessels_sa_only |>
  filter(!VESSEL_OFFICIAL_NBR %in% vessels_to_remove_from_ours)

dim(for_heatmap_lat_lon_trips_vessels_sa_only_rm)
# [1] 67983     4

## add the grid ----
# assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl_gom")
# Create a new object 'my_crs' by extracting the coordinate reference system (CRS)
# from a spatial object 'GOMsf'.
my_crs <- sf::st_crs(GOMsf)
# Create a new data frame 'effort_vsl_gom' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_gom_only' and 'grid_gom5' based on a common
# spatial reference system defined by 'my_crs'.
effort_vsl_gom <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_gom_only,
               grid_gom5,
               my_crs)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

tic("effort_vsl_sa")
# Create a new object 'my_crs_sa' by extracting the coordinate reference system (CRS)
# from a spatial object 'sa_shp'.
my_crs_sa <- sf::st_crs(sa_shp)
# sf::st_geometry(grid_sa5)
# Geodetic CRS:  NAD83
# sf::st_geometry(sa_shp)
# Geodetic CRS:  NAD83

# Create a new data frame 'effort_vsl_sa' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_sa_only_rm' and 'grid_sa5' based on a
# spatial reference system defined by 'my_crs_sa'.
effort_vsl_sa <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_sa_only_rm,
               grid_sa5,
               my_crs = my_crs_sa)
toc()
# effort_vsl_sa: 1.22 sec elapsed

## crop by the shape ----
tic("effort_vsl_cropped_gom")
effort_vsl_cropped_gom <- crop_by_shape(effort_vsl_gom)
toc()
# effort_cropped2: 0.44 sec elapsed

dim(effort_vsl_cropped_gom)
# [1] 35822     7

tic("effort_vsl_cropped_sa")
effort_vsl_cropped_sa <- crop_by_shape(effort_vsl_sa, sa_shp)
toc()
# effort_vsl_cropped_sa: 0.54 sec elapsed

str(effort_vsl_cropped_sa)
# [1] 21461     8

## count trip ids and vessels by grid cell ----

effort_vsl_cropped_cnt_l <-
  list(effort_vsl_cropped_gom,
    effort_vsl_cropped_sa) |>
  map(function(effort_vsl_cropped) {
    add_vsl_and_trip_cnts(effort_vsl_cropped)
  })

map(effort_vsl_cropped_cnt_l, dim)
# [[1]]
# [1] 35822     9
#
# [[2]]
# [1] 21461    10

# check
effort_vsl_cropped_cnt_l[[1]] |>
  sf::st_drop_geometry() |>
  filter(cell_id == 1864) |>
  select(vsl_cnt, trip_id_cnt) |>
  distinct() |>
  glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

### no rule3 ----
effort_cropped_short_cnt2_short_l <-
  effort_vsl_cropped_cnt_l |>
  map(function(effort_vsl_cropped_cnt) {
    effort_vsl_cropped_cnt |>
      select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
  })

map(effort_cropped_short_cnt2_short_l, dim)
# [1] 35822     5
# [1] 21461     6

# ### with rule 3 ----
# effort_cropped_short_cnt_rule3_short <-
#   effort_cropped_short_cnt_rule3 |>
#   select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
#
# dim(effort_cropped_short_cnt_rule3_short)
# # [1] 31981     5
#
## join with min grid ----
# ### rule 3 ----
# heat.plt_rule3 <-
#   effort_cropped_short_cnt_rule3_short |>
#   # have to use data.frame, to avoid
#   # Error: y should not have class sf; for spatial joins, use st_join
#   inner_join(data.frame(grid))
# # Joining with `by = join_by(cell_id)`
#
# # mapview(grid)
# dim(heat.plt_rule3)
# # [1] 35828     6
# # [1] 33883     6 (rule3)
# # [1] 31981     6
#
### no rule 3 ----
heat.plt_gom <-
  effort_cropped_short_cnt2_short_l[[1]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_gom5))
# Joining with `by = join_by(cell_id)`

heat.plt_sa <-
  effort_cropped_short_cnt2_short_l[[2]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_sa5))
# Joining with `by = join_by(cell_id)`

## make a plot ----

max_num3_gom <- max(heat.plt_gom$trip_id_cnt)
# 1209

max_num3_sa <- max(heat.plt_sa$trip_id_cnt)
# 590

# map_trips_rule_3 <-
#   make_map_trips(heat.plt,
#            st_union_GOMsf,
#            "total trips",
#            max_num = max_num3,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

# map_trips_no_rule_3
# print_df_names(heat.plt_gom)
map_trips_no_rule_3_gom <-
  make_map_trips(heat.plt_gom,
           st_union_GOMsf,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 1.2)

map_trips_no_rule_3_gom


map_trips_no_rule_3_sa <-
  make_map_trips(heat.plt_sa,
           sa_shp,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 0.8)

map_trips_no_rule_3_sa +
  geom_sf(data = sa_s_shp) +
  geom_sf_text(data = sa_s_shp,
               label = sa_s_shp$NAME,
               size = 3)

# make_map_trips <-
#   function(map_trip_base_data,
#            shape_data,

#            total_trips_title,
#            trip_cnt_name,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
#            unit_num = 1,
#            print_stat_zone = NULL
#            ) {


## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(GOMsf, join = st_nearest_feature)
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf: 75.5 sec elapsed

# all points for GOM permitted
# mapview(for_heatmap_lat_lon_trips_vessels_only_join_gomsf)
# [1] 41455     4

tic("for_heatmap_lat_lon_trips_vessels_only_inters_gomsf")
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf <-
  for_heatmap_lat_lon_trips_vessels_gom_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf_1: 0.5 sec elapsed

# only points in GOM
dim(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf)
# [1] 35822     6
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, StatZone, LONGITUDE, LATITUDE"

### count trip ids and vessels by statZone ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf |>
  # sf::st_drop_geometry() |>
  group_by(StatZone) |>
  mutate(vsl_cnt_stat_zone = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt_stat_zone = n_distinct(TRIP_ID)) |>
  ungroup()

View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt)

# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
#   filter(vsl_cnt_stat_zone < 3) |>
#   write_csv("state_zone_less_2_vsl.csv")

# check
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
    sf::st_drop_geometry() |>
    select(vsl_cnt_stat_zone) |>
    distinct() |>
    arrange(vsl_cnt_stat_zone)

### remove extra columns ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR)) |>
  distinct() |>
  ungroup()

# View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)
# [1] 35822     4

# dots
# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  # filter(vsl_cnt_stat_zone < 10) |>
  # mapview()

#### only vsl > 3 ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_rule3 <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  filter(vsl_cnt_stat_zone > 2)

### combine cnts with statzone geometry ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  sf::st_drop_geometry() |>
  distinct()
# 21

# class(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df)

gomshp_zone_cnt <-
left_join(GOMsf,
          for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df,
         join_by("StatZone"))

dim(gomshp_zone_cnt)
# [1] 21  4

shape_data <- gomshp_zone_cnt


### plot by zone ----
# ggplot(map.df,aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=delta),color="grey20")+

# glimpse(gomshp_zone_cnt)
# mapview(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)

plot_zone_cnt <-
    function(shape_data,
           total_cnt_title,
           cnts_col_name,
           caption_text) {
      # browser()
      max_num <-
        max(shape_data[[cnts_col_name]])
      # max(gomshp_zone_cnt$vsl_cnt_stat_zone)
      # 148
      min_num <-
        min(shape_data[[cnts_col_name]])
      # 2

      map_vsl_zone <-
        ggplot() +
        geom_sf(data = shape_data,
                aes(geometry = geometry,
                    #                   fill = !!sym(trip_cnt_name)),
                    fill = !!sym(cnts_col_name)),
                colour = NA) +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5) +
        labs(
          x = "",
          y = "",
          fill = "",
          caption = caption_text
        ) +
        theme_bw() +
        scale_fill_gradient2(
          name = total_cnt_title,
          labels = scales::comma,
          low = "red",
          mid = "purple",
          high = "blue",
          # trans = "log2",
          trans = "log1p",
          limits = c(min_num, max_num)
          # ,
          # oob = scales::oob_keep
        ) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = unit(0.9, "npc"),
          # legend.key.width = unit(3, "cm"),
          plot.caption = element_text(hjust = 0)
        ) +
        guides(fill = guide_colourbar(title.position = "top"))
    }

# shape_data = gomshp_zone_cnt
# total_cnt_title = "total reporting vessels"
# cnts_col_name = "vsl_cnt_stat_zone"
# caption_text = "Heat map of SEFHIER trips. 2022. GoM permitted vessels."

map_vsls_stat_zone <-
  plot_zone_cnt(
    gomshp_zone_cnt,
    "total reporting vessels",
    "vsl_cnt_stat_zone",
    caption_text = "Heat map of SEFHIER reporting vessels. 2022. GoM permitted only."
  )

# map_vsls_stat_zone

map_trips_stat_zone <-
  plot_zone_cnt(gomshp_zone_cnt,
                "total trips",
                "trip_id_cnt_stat_zone",
                caption_text = "Heat map of SEFHIER trips by stat zones. 2022. GoM permitted vessels only.")

# map_trips_stat_zone
#

# Repeat separately for charter and headboat ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they selected that in the logbook.

## get trip_type data ----

my_vessels_trips <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  select(VESSEL_OFFICIAL_NBR,
         TRIP_ID) |>
  distinct()

str(my_vessels_trips)
# chr [1:626] "FL9312NA" "FL6074MJ" "FL4038KN" "FL0957RW" "FL4521RU" "994360" ...


### create a db query with chunks, otherwise Oracle error ----

my_vessels_ids_u <- unique(my_vessels_trips$VESSEL_OFFICIAL_NBR)

full_length <- length(my_vessels_ids_u)
# 626

max_chunk_num <- 3

# repeat max_chunk_num times
all_ch <-
  lapply(1:max_chunk_num, function(i) {
    # count the current start and end
    one_chunk_length <- ceiling(full_length / max_chunk_num)
    current_end <- one_chunk_length * i
    current_start <- current_end - one_chunk_length

    # pull the chunk from start to end
    my_vessels_ids_u[current_start:current_end] |>
      # and paste in into a comma separated string
      paste0(collapse = "', '")
  })

tail(all_ch[[3]])
tail(my_vessels_ids_u)
# str(all_ch)
# tibble [626 × 1] (S3: tbl_df/tbl/data.frame)

                 # paste0(collapse = "', '")

vsl_id_q_part <-
  all_ch |>
  map(\(one_chunk) str_glue("vessel_official_nbr in ('{one_chunk}')"))

collect_parts <-
  paste(vsl_id_q_part, collapse = ' OR ')

# cat(collect_parts)

get_trip_type_data_from_db <- function() {
  # browser()
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

  "
DECLARE
  str varchar2(32767);
BEGIN
  str := 'Very-very-...-very-very-very-very-very-very long string value';
  update t1 set col1 = str;
END;
/
"

  request_query <-
    paste0(
      "SELECT distinct
      trip_id,
    vessel_official_nbr,
    trip_type_name
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_type in ('H', 'A')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND (
  ",
collect_parts,
")"
    )

  # nchar(request_query)
  # [1] 523009

  # cat(request_query)

  db_data = dbGetQuery(con,
                       request_query)

  dbDisconnect(con)

  return(db_data)
}

tic("trip_type_data_from_db")
trip_type_data_from_db <- get_trip_type_data_from_db()
toc()
# trip_type_data_from_db: 9.46 sec elapsed

# data_overview(trip_type_data_from_db)
# TRIP_ID             47702
# VESSEL_OFFICIAL_NBR 618

# glimpse(my_vessels_trips)

glimpse(trip_type_data_from_db)
# Rows: 47,702

## keep only trips we have in our original data ----
trip_type_data_from_db_by_t_id <-
  trip_type_data_from_db |>
  filter(TRIP_ID %in% my_vessels_trips$TRIP_ID) |>
  distinct()

glimpse(trip_type_data_from_db_by_t_id)
# Rows: 39,977

## add trip_type data to the original data ----
trip_type_data_from_db_by_t_id <-
  mutate(trip_type_data_from_db_by_t_id,
       TRIP_ID = as.character(TRIP_ID))

trip_type_data_from_db_by_t_id_types <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  left_join(trip_type_data_from_db_by_t_id)
# Joining with `by = join_by(TRIP_ID, VESSEL_OFFICIAL_NBR)`

## separate by trip type ----
trip_type_data_from_db_by_t_id_types_l <-
  trip_type_data_from_db_by_t_id_types |>
  split(as.factor(trip_type_data_from_db_by_t_id_types$TRIP_TYPE_NAME)) |>
  # remove extra columns in each df
  map(\(x)
      x |>
        dplyr::select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
        distinct())


# glimpse(trip_type_data_from_db_by_t_id_types)
# List of 2
#  $ CHARTER :'data.frame':	39835 obs. of  3 variables:
#  $ HEADBOAT:'data.frame':	142 obs. of  3 variables:

# str(trip_type_data_from_db_by_t_id_types_l)

## create 5 min heatmaps for both trip types ----
# trip_type_data_from_db_by_t_id_types_l

tic("effort_t_type")
effort_t_type <-
  map(trip_type_data_from_db_by_t_id_types_l, df_join_grid)
toc()
# effort_t_type: 0.7 sec elapsed
# dim(effort_t_type)

tic("effort_t_type_cropped")
effort_t_type_cropped <- map(effort_t_type, crop_by_shape)
toc()
# effort_t_type_cropped: 1.04 sec elapsed

str(effort_t_type_cropped)

effort_t_type_cropped_cnt <- map(effort_t_type_cropped, add_vsl_and_trip_cnts)

map_df(effort_t_type_cropped_cnt, dim)
#   CHARTER HEADBOAT
#     <int>    <int>
# 1   34696       13
# 2       9        9

# data_overview(effort_t_type_cropped_cnt$CHARTER)
# cell_id              2785

# View(grid)

### join with min grid ----
effort_t_type_cropped_cnt_join_grid <-
  map(effort_t_type_cropped_cnt,
      \(x)
      # have to use data.frame, to avoid
      # Error: y should not have class sf; for spatial joins, use st_join
      inner_join(x, data.frame(grid),
                 by = join_by(cell_id)
)
      )

# print_df_names(effort_t_type_cropped_cnt_join_grid$CHARTER)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, cell_id, StatZone, LONGITUDE, LATITUDE, vsl_cnt, trip_id_cnt, x"

# effort_t_type_cropped_cnt_join_grid$CHARTER

map_trips_types <-
  names(effort_t_type_cropped_cnt_join_grid) |>
  map(
    \(charter_headb) make_map_trips(
      effort_t_type_cropped_cnt_join_grid[[charter_headb]],
      shape_data = st_union_GOMsf,
      total_trips_title = "total trips",
      trip_cnt_name = "trip_id_cnt",
      caption_text = str_glue("Heat map of SEFHIER {tolower(charter_headb)} trips (5 min. resolution). 2022. GoM permitted vessels.",
                              unit_num = 0.6)
    )
  )

map_trips_types[[1]]


#### Current file: ~/R_code_github/useful_functions_module.r ----

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
  contents <- map_df(myfiles, ~read_excel(
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

## ---- functions to clean FHIER compliance and correspondense reports ----

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
    arrange(group_by_arr[1]) %>%          # Arrange the data by the first column in 'group_by_arr'.
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
    case_when(
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

    # ---- Specific correspondence manipulations ----
    # Perform cleaning and processing specific to correspondence data.
    corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

    ## ---- Specific compliance manipulations ----
    # Extract compliance data from the cleaned CSVs.
    compl_arr <- csvs_clean1[2:length(csvs_clean1)]

    # Clean and process compliance data.
    compl_clean <- compliance_cleaning(compl_arr)

    # Return a list containing cleaned compliance and correspondence data.
    return(list(compl_clean, corresp_arr_contact_cnts_clean))
  }

# ---- specific correspondence manipulations ----
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

## ---- specific compliance manipulations ----
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
#   map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% group_by(test) %>% summarise(sum(elapsed))

# ====
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
# Define a function to create a flat file by combining the contents of multiple files.
make_a_flat_file <- function(flat_file_name, files_to_combine_list) {
  # Redirect the output to the specified 'flat_file_name'.
  sink(flat_file_name)

  # Loop through the list of 'files_to_combine_list'.
  for (i in 1:length(files_to_combine_list)) {
    # Read the contents of the current file.
    current_file <- readLines(files_to_combine_list[i])

    # Print a header indicating the current file being processed.
    cat("\n\n#### Current file:", files_to_combine_list[i], "----\n\n")

    # Print the contents of the current file, separating lines with newline characters.
    cat(current_file, sep = "\n")
  }

  # Restore the default output behavior.
  sink()
}

# ===
# Define a function to append the contents of a single file to an existing flat file.
write_to_1_flat_file <- function(flat_file_name, file_name_to_write) {
  # Redirect the output to the specified 'flat_file_name' and append content.
  sink(flat_file_name, append = TRUE)

  # Read the contents of the current file.
  current_file_text <- readLines(file_name_to_write)

  # Print a header indicating the current file being processed.
  cat("\n\n#### Current file:", file_name_to_write, "----\n\n")

  # Print the contents of the current file, separating lines with newline characters.
  cat(current_file_text, sep = "\n")

  # # Restore the default output behavior.
  # sink()
}

# Function to separate permit groups into three categories based on a specified field
separate_permits_into_3_groups <-
  function(my_df, permit_group_field_name = "permitgroup") {
    my_df %>%
      # Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group
      mutate(permit_sa_gom =
               case_when(
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
#       map_df(function(x) {
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
      coords = c("LONGITUDE", "LATITUDE"),
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



#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_by_permit.R ----

# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

## functions for ten_min ----

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <-
    paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")
  convert_to_decimal_degree(dm_num)
}

get_ten_min_coords <- function(my_df) {
  ten_min_df <-
    my_df |>
    mutate(
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),
      # All lon should be negative, bc we know it is all in America
      ten_min_lon =
        -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )
  return(ten_min_df)
  # distinct(ten_min_df)
}

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good_all_coords <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 97970    17

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short_good_all_coords |>
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])




#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_by_permit.R ----

# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

## functions for ten_min ----

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <-
    paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")
  convert_to_decimal_degree(dm_num)
}

get_ten_min_coords <- function(my_df) {
  ten_min_df <-
    my_df |>
    mutate(
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),
      # All lon should be negative, bc we know it is all in America
      ten_min_lon =
        -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )
  return(ten_min_df)
  # distinct(ten_min_df)
}

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good_all_coords <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 97970    17

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short_good_all_coords |>
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])




#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\prepare_gom_heatmap_func.R ----

# Load the 'tigris' package to access geographic data.
library(tigris)

# Set the 'tigris_use_cache' option to TRUE. This will enable caching of
# data retrieved from the TIGER/Line Shapefiles service, which can help
# improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# plot text sizes ----
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 13,
  axis_text_y_size = 13,
  plot_caption_text_size = 13,
  legend_title_text_size = 10,
  legend_text_text_size = 10,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# read in sa shp ----
# F2 in RStudio will show the function definition, when the cursor is on the name.
# Read a shapefile (geospatial data) from the specified file path and store it in the 'sa_shp' object.
sa_shp <-
  read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    "Florida", # can exclude, if go by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )
)

# Create a data frame 'state_tbl' containing state abbreviations and state names; 2x50.
# - 'state.abb' provides state abbreviations.
# - 'tolower(state.name)' converts state names to lowercase.
# - The resulting data frame has two columns: 'state_abb' and 'state_name'.
state_tbl <- data.frame(state.abb, tolower(state.name))

# Rename the columns in the 'state_tbl' data frame.
# The first column is named 'state_abb', and the second column is named 'state_name'.
names(state_tbl) = c("state_abb", "state_name")

#from the DF, only grab the SA states defined above
sa_state_abb <-
  # a table from above
  state_tbl %>%
  # get only these in our list
  filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  select(state_abb)

# # add regions to the FHIER logbook DF
# fhier_logbooks_content_waves__sa_gom <-
#   fhier_logbooks_content_waves_fl_county %>%
#   # add a new column "end_port_sa_gom" with sa or gom for each state
#   # use fix_name aux function to unify state names (lower case, no spaces etc.)
#   mutate(end_port_sa_gom = case_when(
#     # if a name is in our SA list - "sa", otherwise - "gom"
#     fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
#     .default = "gom"
#   )) %>%
#   # go through the new column again
#   # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
#   # otherwise don't change
#   mutate(end_port_sa_gom = ifelse(
#     tolower(end_port_state) == "fl",
#     end_port_fl_reg,
#     end_port_sa_gom
#   )) %>%
#   # remove this column, we don't need it anymore
#   select(-end_port_fl_reg)

# #### test: check new cols of states and regions ----
# fhier_logbooks_content_waves__sa_gom %>%
#   # look at states and regions
#   select(end_port_state, end_port_sa_gom) %>%
#   unique() %>%
#   glimpse()

## r get Shapefile all waters ----
path_to_federal_state_w <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

file.exists(path_to_federal_state_w)
# T

tic("federal_state_w_sf")
federal_state_w_sf <-
  sf::read_sf(path_to_federal_state_w)
toc()

# rr <-
# federal_state_w_sf |>
#   sf::st_drop_geometry()
#
# rr$Jurisdicti |>
#   cat(sep = ", ")

east_coat_states <- c(
  gom = c("Florida",
          "Texas",
          "Louisiana"),
  sa = c(
    "Alabama",
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Maine",
    "Maryland",
    "Massachusetts",
    "Mississippi",
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
# nc_sql = sf::st_read(system.file("shape/nc.shp", package="sf"),
#                      query = "SELECT NAME, SID74, FIPS FROM \"nc\" WHERE BIR74 > 20000")

# Create a new data frame 'federal_state_w_sf_east' by filtering the existing data frame 'federal_state_w_sf'.
# Rows are retained if the 'Jurisdicti' column matches any of the values in 'east_coat_states'.
federal_state_w_sf_east <-
  federal_state_w_sf |>
  filter(Jurisdicti %in% east_coat_states)

# mapview(sa_shp)
# [1] 21  7

# coast line SA shp ----
# us_bb <-
#   tigris::counties(filter_by = big_bounding_box, progress_bar = FALSE)

# Create a new data frame 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes.
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE)

# Rows are retained if the 'NAME' column (state name) matches any of the values in 'states_sa'.
sa_s_shp <-
  us_s_shp |>
  filter(NAME %in% states_sa)

# sa_s_shp_plot <-
#   ggplot() +
#   geom_sf(data = sa_s_shp)

# sa_counties_shp <- tigris::counties(states_sa, cb = TRUE)

# gg <- ggplot()
# gg <- gg + geom_sf(
#   data = sa_s_shp,
#   color = "black",
#   # fill = "",
#   size = 0.25
# )
# gg

# library(rnaturalearth) #coastline
# library(maps)
# library(mapdata)


atl_shp_file <-
  file.path(my_paths$inputs,
            r"(shapefiles\ATL_SLA\ATL_SLA.shp)")

# Read a shapefile from the specified file path using the 'sf::read_sf' function.
# The resulting spatial data is stored in the 'atl_shp' object.
atl_shp <- sf::read_sf(atl_shp_file)

# Create a plot using 'ggplot2' with the 'atl_shp' spatial data.
# Use 'geom_sf' to display the shapes from 'atl_shp' with no fill (NA, i.e., transparent).
ggplot() +
  geom_sf(data = atl_shp, fill = NA)

# read in GOM shp ----
# Read a shapefile from the specified file path using 'sf::read_sf'.
# Then, group the resulting data by 'StatZone' and summarize it.
GOMsf <-
  sf::read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>%
  summarise()

# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create 5x5 minute grid ----
# Define a function 'min_grid' that creates a grid of cells within the bounding box of a given spatial data frame.
# - 'my_sf' is the input spatial data frame (default is 'GOMsf').
# - 'minute_num' specifies the grid cell size in minutes.

min_grid <- function(my_sf = GOMsf, minute_num = 1) {
  # Create a grid of cells using 'sf::st_make_grid' within the bounding box of 'my_sf'.
  grid <-
    sf::st_make_grid(x = sf::st_bbox(my_sf),
                     cellsize = 1 / 60 * minute_num) %>%

    # Convert the grid to a spatial data frame using 'sf::st_as_sf'.
    sf::st_as_sf() %>%

    # Add a 'cell_id' column to the grid using 'mutate'.
    mutate(cell_id = 1:nrow(.))

  # Return the created grid.
  return(grid)
}

grid_gom5 <- min_grid(GOMsf, 5)
grid_sa5 <- min_grid(sa_shp, 5)

# Set the aggregate attribute to "constant" for multiple spatial objects.
sf::st_agr(GOMsf) =
  sf::st_agr(sa_shp) =
  sf::st_agr(grid_gom5) =
  sf::st_agr(grid_sa5) =
  "constant"

### remove internal boundaries from the GOM shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- sf::st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## by n min grid ----
# Define a function 'df_join_grid' that joins a data frame with a grid using specified coordinates and CRS.

df_join_grid <- function(my_df, grid, my_crs) {
  # Convert 'my_df' to a spatial data frame with specified coordinates and CRS using 'sf::st_as_sf'.
  my_df_grid <-
    my_df |>
    sf::st_as_sf(
      coords = c("LONGITUDE", "LATITUDE"),
      crs = my_crs) |>

  # Join the resulting spatial data frame with the 'grid' using the nearest feature join.
  sf::st_join(grid, join = sf::st_nearest_feature)

  # Return the joined data frame.
  return(my_df_grid)
}

# Define a function 'crop_by_shape' that crops a spatial object using another spatial object.
# - 'my_sf' is the input spatial object to be cropped.
# - 'my_shp' is the spatial object used for cropping (default is 'GOMsf').

crop_by_shape <- function(my_sf, my_shp = GOMsf) {
  # Join 'my_sf' with 'my_shp' to crop it, leaving only the intersecting geometries.
  my_sf |>
    sf::st_join(my_shp, left = FALSE) %>%

  # Extract the LONGITUDE and LATITUDE coordinates from the joined spatial object.
  dplyr::mutate(LONGITUDE = sf::st_coordinates(.)[, 1],
         LATITUDE = sf::st_coordinates(.)[, 2]) %>%

  # Return the cropped and transformed spatial object.
  return()
}

## count trip ids and vessels by grid cell ----
# Define a function 'add_vsl_and_trip_cnts' that adds vessel and trip counts to a data frame.
# - 'my_df' is the input data frame.
# - 'vessel_id_name' is the name of the column containing vessel IDs (default is "VESSEL_OFFICIAL_NBR").

add_vsl_and_trip_cnts <- function(my_df, vessel_id_name = "VESSEL_OFFICIAL_NBR") {
  # Group the data frame by 'cell_id'.
  my_df |>
    group_by(cell_id) |>

  # Add columns 'vsl_cnt' and 'trip_id_cnt' with counts of distinct vessel and trip IDs.
    # sym() take strings as input and turn them into symbols.
    # The !! (bang-bang or unquote) operator is used to unquote the symbol, allowing it to be used in dplyr verbs like mutate, select, or other functions that accept column names.
    # So, the code !!rlang::sym(vessel_id_name) effectively evaluates to the column name specified by the vessel_id_name variable in the context of a dplyr verb, allowing you to work with the column dynamically based on the variable's value.

    dplyr::mutate(
      vsl_cnt =
        dplyr::n_distinct(!!rlang::sym(vessel_id_name)),
      trip_id_cnt =
        dplyr::n_distinct(TRIP_ID)
    ) |>

  # Ungroup the data frame to remove grouping and return the result.
  dplyr::ungroup() %>%

  # Return the modified data frame.
  return()
}

## make a plot ----
# Define a function 'make_map_trips' to create a ggplot2 heatmap of trip data.
# - 'map_trip_base_data' is the data containing trip information to be mapped.
# - 'shape_data' is the shape data used as a backdrop for mapping.
# - 'total_trips_title' is the title for the total trips legend.
# - 'trip_cnt_name' is the name of the column with trip counts.
# - 'caption_text' is the caption for the plot.
# - 'unit_num' specifies the unit size for the legend.
# - 'print_stat_zone' is an optional argument to include StatZone labels.
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
           unit_num = 1,
           print_stat_zone = NULL
           ) {
    # Calculate the maximum number of trips for legend scaling.
    max_num <- max(map_trip_base_data[[trip_cnt_name]])

    # Create a ggplot2 plot 'map_trips'.
    map_trips <-
      ggplot() +
      # Add a filled heatmap using 'geom_sf'.
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      # Add the shape data with no fill.
      geom_sf(data = shape_data, fill = NA)

    # Check for an optional argument 'print_stat_zone'.
    if (!missing(print_stat_zone)) {
      map_trips <-
        map_trips +
        # Add StatZone labels using 'geom_sf_text'.
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5)
    }

    map_trips <-
        map_trips +
      # Set plot labels and theme settings.
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      theme_bw() +

      # Set fill scale properties.
      scale_fill_gradient2(
        name = total_trips_title,
        labels = scales::comma,
        low = "red",
        mid = "purple",
        high = "blue",
        # trans = "log2",
        trans = "log1p",
        limits = c(1, max_num)
        # ,
        # oob = scales::oob_keep
      ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(unit_num, "npc"),
        legend.title = element_text(size =
                                      text_sizes[["legend_title_text_size"]]),
        legend.text = element_text(size =
                                     text_sizes[["legend_text_text_size"]]), # for charter heatmap use 7
        plot.caption = element_text(hjust = 0,
                                    size = text_sizes[["plot_caption_text_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      # Add a legend guide for fill color.
      guides(fill = guide_colourbar(title.position = "top"))

    # Return the created 'map_trips' plot.
    return(map_trips)
  }


#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location\fishing_effort_location_heatmap.R ----

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# data are from "by_permit"

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

library(ggplot2) # a visualization package
library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.

# Heatmap ----

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

## heatmap data ----

# short_example_3_cnts_short |> glimpse()
dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual)
# [1] 41455     8

# glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual)

# for_heatmap_lat_lon_trips_only <-
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
#   select(TRIP_ID, LATITUDE, LONGITUDE) |>
#   distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

# gom
for_heatmap_lat_lon_trips_vessels_gom_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_gom_only)
# Rows: 41,455

# sa
for_heatmap_lat_lon_trips_vessels_sa_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$sa_only |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_sa_only)
# [1] 68122     4

### remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
for_heatmap_lat_lon_trips_vessels_sa_only_rm <-
  for_heatmap_lat_lon_trips_vessels_sa_only |>
  filter(!VESSEL_OFFICIAL_NBR %in% vessels_to_remove_from_ours)

dim(for_heatmap_lat_lon_trips_vessels_sa_only_rm)
# [1] 67983     4

## add the grid ----
# assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl_gom")
# Create a new object 'my_crs' by extracting the coordinate reference system (CRS)
# from a spatial object 'GOMsf'.
my_crs <- sf::st_crs(GOMsf)
# Create a new data frame 'effort_vsl_gom' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_gom_only' and 'grid_gom5' based on a common
# spatial reference system defined by 'my_crs'.
effort_vsl_gom <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_gom_only,
               grid_gom5,
               my_crs)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

tic("effort_vsl_sa")
# Create a new object 'my_crs_sa' by extracting the coordinate reference system (CRS)
# from a spatial object 'sa_shp'.
my_crs_sa <- sf::st_crs(sa_shp)
# sf::st_geometry(grid_sa5)
# Geodetic CRS:  NAD83
# sf::st_geometry(sa_shp)
# Geodetic CRS:  NAD83

# Create a new data frame 'effort_vsl_sa' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_sa_only_rm' and 'grid_sa5' based on a
# spatial reference system defined by 'my_crs_sa'.
effort_vsl_sa <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_sa_only_rm,
               grid_sa5,
               my_crs = my_crs_sa)
toc()
# effort_vsl_sa: 1.22 sec elapsed

## crop by the shape ----
tic("effort_vsl_cropped_gom")
effort_vsl_cropped_gom <- crop_by_shape(effort_vsl_gom)
toc()
# effort_cropped2: 0.44 sec elapsed

dim(effort_vsl_cropped_gom)
# [1] 35822     7

tic("effort_vsl_cropped_sa")
effort_vsl_cropped_sa <- crop_by_shape(effort_vsl_sa, sa_shp)
toc()
# effort_vsl_cropped_sa: 0.54 sec elapsed

str(effort_vsl_cropped_sa)
# [1] 21461     8

## count trip ids and vessels by grid cell ----

effort_vsl_cropped_cnt_l <-
  list(effort_vsl_cropped_gom,
    effort_vsl_cropped_sa) |>
  map(function(effort_vsl_cropped) {
    add_vsl_and_trip_cnts(effort_vsl_cropped)
  })

map(effort_vsl_cropped_cnt_l, dim)
# [[1]]
# [1] 35822     9
#
# [[2]]
# [1] 21461    10

# check
effort_vsl_cropped_cnt_l[[1]] |>
  sf::st_drop_geometry() |>
  filter(cell_id == 1864) |>
  select(vsl_cnt, trip_id_cnt) |>
  distinct() |>
  glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

### no rule3 ----
effort_cropped_short_cnt2_short_l <-
  effort_vsl_cropped_cnt_l |>
  map(function(effort_vsl_cropped_cnt) {
    effort_vsl_cropped_cnt |>
      select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
  })

map(effort_cropped_short_cnt2_short_l, dim)
# [1] 35822     5
# [1] 21461     6

# ### with rule 3 ----
# effort_cropped_short_cnt_rule3_short <-
#   effort_cropped_short_cnt_rule3 |>
#   select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
#
# dim(effort_cropped_short_cnt_rule3_short)
# # [1] 31981     5
#
## join with min grid ----
# ### rule 3 ----
# heat.plt_rule3 <-
#   effort_cropped_short_cnt_rule3_short |>
#   # have to use data.frame, to avoid
#   # Error: y should not have class sf; for spatial joins, use st_join
#   inner_join(data.frame(grid))
# # Joining with `by = join_by(cell_id)`
#
# # mapview(grid)
# dim(heat.plt_rule3)
# # [1] 35828     6
# # [1] 33883     6 (rule3)
# # [1] 31981     6
#
### no rule 3 ----
heat.plt_gom <-
  effort_cropped_short_cnt2_short_l[[1]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_gom5))
# Joining with `by = join_by(cell_id)`

heat.plt_sa <-
  effort_cropped_short_cnt2_short_l[[2]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_sa5))
# Joining with `by = join_by(cell_id)`

## make a plot ----

max_num3_gom <- max(heat.plt_gom$trip_id_cnt)
# 1209

max_num3_sa <- max(heat.plt_sa$trip_id_cnt)
# 590

# map_trips_rule_3 <-
#   make_map_trips(heat.plt,
#            st_union_GOMsf,
#            "total trips",
#            max_num = max_num3,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

# map_trips_no_rule_3
# print_df_names(heat.plt_gom)
map_trips_no_rule_3_gom <-
  make_map_trips(heat.plt_gom,
           st_union_GOMsf,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 1.2)

map_trips_no_rule_3_gom


map_trips_no_rule_3_sa <-
  make_map_trips(heat.plt_sa,
           sa_shp,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 0.8)

map_trips_no_rule_3_sa +
  geom_sf(data = sa_s_shp) +
  geom_sf_text(data = sa_s_shp,
               label = sa_s_shp$NAME,
               size = 3)

# make_map_trips <-
#   function(map_trip_base_data,
#            shape_data,

#            total_trips_title,
#            trip_cnt_name,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
#            unit_num = 1,
#            print_stat_zone = NULL
#            ) {


## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(GOMsf, join = st_nearest_feature)
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf: 75.5 sec elapsed

# all points for GOM permitted
# mapview(for_heatmap_lat_lon_trips_vessels_only_join_gomsf)
# [1] 41455     4

tic("for_heatmap_lat_lon_trips_vessels_only_inters_gomsf")
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf <-
  for_heatmap_lat_lon_trips_vessels_gom_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf_1: 0.5 sec elapsed

# only points in GOM
dim(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf)
# [1] 35822     6
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, StatZone, LONGITUDE, LATITUDE"

### count trip ids and vessels by statZone ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf |>
  # sf::st_drop_geometry() |>
  group_by(StatZone) |>
  mutate(vsl_cnt_stat_zone = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt_stat_zone = n_distinct(TRIP_ID)) |>
  ungroup()

View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt)

# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
#   filter(vsl_cnt_stat_zone < 3) |>
#   write_csv("state_zone_less_2_vsl.csv")

# check
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
    sf::st_drop_geometry() |>
    select(vsl_cnt_stat_zone) |>
    distinct() |>
    arrange(vsl_cnt_stat_zone)

### remove extra columns ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR)) |>
  distinct() |>
  ungroup()

# View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)
# [1] 35822     4

# dots
# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  # filter(vsl_cnt_stat_zone < 10) |>
  # mapview()

#### only vsl > 3 ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_rule3 <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  filter(vsl_cnt_stat_zone > 2)

### combine cnts with statzone geometry ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  sf::st_drop_geometry() |>
  distinct()
# 21

# class(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df)

gomshp_zone_cnt <-
left_join(GOMsf,
          for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df,
         join_by("StatZone"))

dim(gomshp_zone_cnt)
# [1] 21  4

shape_data <- gomshp_zone_cnt


### plot by zone ----
# ggplot(map.df,aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=delta),color="grey20")+

# glimpse(gomshp_zone_cnt)
# mapview(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)

plot_zone_cnt <-
    function(shape_data,
           total_cnt_title,
           cnts_col_name,
           caption_text) {
      # browser()
      max_num <-
        max(shape_data[[cnts_col_name]])
      # max(gomshp_zone_cnt$vsl_cnt_stat_zone)
      # 148
      min_num <-
        min(shape_data[[cnts_col_name]])
      # 2

      map_vsl_zone <-
        ggplot() +
        geom_sf(data = shape_data,
                aes(geometry = geometry,
                    #                   fill = !!sym(trip_cnt_name)),
                    fill = !!sym(cnts_col_name)),
                colour = NA) +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5) +
        labs(
          x = "",
          y = "",
          fill = "",
          caption = caption_text
        ) +
        theme_bw() +
        scale_fill_gradient2(
          name = total_cnt_title,
          labels = scales::comma,
          low = "red",
          mid = "purple",
          high = "blue",
          # trans = "log2",
          trans = "log1p",
          limits = c(min_num, max_num)
          # ,
          # oob = scales::oob_keep
        ) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = unit(0.9, "npc"),
          # legend.key.width = unit(3, "cm"),
          plot.caption = element_text(hjust = 0)
        ) +
        guides(fill = guide_colourbar(title.position = "top"))
    }

# shape_data = gomshp_zone_cnt
# total_cnt_title = "total reporting vessels"
# cnts_col_name = "vsl_cnt_stat_zone"
# caption_text = "Heat map of SEFHIER trips. 2022. GoM permitted vessels."

map_vsls_stat_zone <-
  plot_zone_cnt(
    gomshp_zone_cnt,
    "total reporting vessels",
    "vsl_cnt_stat_zone",
    caption_text = "Heat map of SEFHIER reporting vessels. 2022. GoM permitted only."
  )

# map_vsls_stat_zone

map_trips_stat_zone <-
  plot_zone_cnt(gomshp_zone_cnt,
                "total trips",
                "trip_id_cnt_stat_zone",
                caption_text = "Heat map of SEFHIER trips by stat zones. 2022. GoM permitted vessels only.")

# map_trips_stat_zone
#

# Repeat separately for charter and headboat ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they selected that in the logbook.

## get trip_type data ----

my_vessels_trips <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  select(VESSEL_OFFICIAL_NBR,
         TRIP_ID) |>
  distinct()

str(my_vessels_trips)
# chr [1:626] "FL9312NA" "FL6074MJ" "FL4038KN" "FL0957RW" "FL4521RU" "994360" ...


### create a db query with chunks, otherwise Oracle error ----

my_vessels_ids_u <- unique(my_vessels_trips$VESSEL_OFFICIAL_NBR)

full_length <- length(my_vessels_ids_u)
# 626

max_chunk_num <- 3

# repeat max_chunk_num times
all_ch <-
  lapply(1:max_chunk_num, function(i) {
    # count the current start and end
    one_chunk_length <- ceiling(full_length / max_chunk_num)
    current_end <- one_chunk_length * i
    current_start <- current_end - one_chunk_length

    # pull the chunk from start to end
    my_vessels_ids_u[current_start:current_end] |>
      # and paste in into a comma separated string
      paste0(collapse = "', '")
  })

tail(all_ch[[3]])
tail(my_vessels_ids_u)
# str(all_ch)
# tibble [626 × 1] (S3: tbl_df/tbl/data.frame)

                 # paste0(collapse = "', '")

vsl_id_q_part <-
  all_ch |>
  map(\(one_chunk) str_glue("vessel_official_nbr in ('{one_chunk}')"))

collect_parts <-
  paste(vsl_id_q_part, collapse = ' OR ')

# cat(collect_parts)

get_trip_type_data_from_db <- function() {
  # browser()
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

  "
DECLARE
  str varchar2(32767);
BEGIN
  str := 'Very-very-...-very-very-very-very-very-very long string value';
  update t1 set col1 = str;
END;
/
"

  request_query <-
    paste0(
      "SELECT distinct
      trip_id,
    vessel_official_nbr,
    trip_type_name
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_type in ('H', 'A')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND (
  ",
collect_parts,
")"
    )

  # nchar(request_query)
  # [1] 523009

  # cat(request_query)

  db_data = dbGetQuery(con,
                       request_query)

  dbDisconnect(con)

  return(db_data)
}

tic("trip_type_data_from_db")
trip_type_data_from_db <- get_trip_type_data_from_db()
toc()
# trip_type_data_from_db: 9.46 sec elapsed

# data_overview(trip_type_data_from_db)
# TRIP_ID             47702
# VESSEL_OFFICIAL_NBR 618

# glimpse(my_vessels_trips)

glimpse(trip_type_data_from_db)
# Rows: 47,702

## keep only trips we have in our original data ----
trip_type_data_from_db_by_t_id <-
  trip_type_data_from_db |>
  filter(TRIP_ID %in% my_vessels_trips$TRIP_ID) |>
  distinct()

glimpse(trip_type_data_from_db_by_t_id)
# Rows: 39,977

## add trip_type data to the original data ----
trip_type_data_from_db_by_t_id <-
  mutate(trip_type_data_from_db_by_t_id,
       TRIP_ID = as.character(TRIP_ID))

trip_type_data_from_db_by_t_id_types <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  left_join(trip_type_data_from_db_by_t_id)
# Joining with `by = join_by(TRIP_ID, VESSEL_OFFICIAL_NBR)`

## separate by trip type ----
trip_type_data_from_db_by_t_id_types_l <-
  trip_type_data_from_db_by_t_id_types |>
  split(as.factor(trip_type_data_from_db_by_t_id_types$TRIP_TYPE_NAME)) |>
  # remove extra columns in each df
  map(\(x)
      x |>
        dplyr::select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
        distinct())


# glimpse(trip_type_data_from_db_by_t_id_types)
# List of 2
#  $ CHARTER :'data.frame':	39835 obs. of  3 variables:
#  $ HEADBOAT:'data.frame':	142 obs. of  3 variables:

# str(trip_type_data_from_db_by_t_id_types_l)

## create 5 min heatmaps for both trip types ----
# trip_type_data_from_db_by_t_id_types_l

tic("effort_t_type")
effort_t_type <-
  map(trip_type_data_from_db_by_t_id_types_l, df_join_grid)
toc()
# effort_t_type: 0.7 sec elapsed
# dim(effort_t_type)

tic("effort_t_type_cropped")
effort_t_type_cropped <- map(effort_t_type, crop_by_shape)
toc()
# effort_t_type_cropped: 1.04 sec elapsed

str(effort_t_type_cropped)

effort_t_type_cropped_cnt <- map(effort_t_type_cropped, add_vsl_and_trip_cnts)

map_df(effort_t_type_cropped_cnt, dim)
#   CHARTER HEADBOAT
#     <int>    <int>
# 1   34696       13
# 2       9        9

# data_overview(effort_t_type_cropped_cnt$CHARTER)
# cell_id              2785

# View(grid)

### join with min grid ----
effort_t_type_cropped_cnt_join_grid <-
  map(effort_t_type_cropped_cnt,
      \(x)
      # have to use data.frame, to avoid
      # Error: y should not have class sf; for spatial joins, use st_join
      inner_join(x, data.frame(grid),
                 by = join_by(cell_id)
)
      )

# print_df_names(effort_t_type_cropped_cnt_join_grid$CHARTER)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, cell_id, StatZone, LONGITUDE, LATITUDE, vsl_cnt, trip_id_cnt, x"

# effort_t_type_cropped_cnt_join_grid$CHARTER

map_trips_types <-
  names(effort_t_type_cropped_cnt_join_grid) |>
  map(
    \(charter_headb) make_map_trips(
      effort_t_type_cropped_cnt_join_grid[[charter_headb]],
      shape_data = st_union_GOMsf,
      total_trips_title = "total trips",
      trip_cnt_name = "trip_id_cnt",
      caption_text = str_glue("Heat map of SEFHIER {tolower(charter_headb)} trips (5 min. resolution). 2022. GoM permitted vessels.",
                              unit_num = 0.6)
    )
  )

map_trips_types[[1]]
