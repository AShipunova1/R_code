
#### add-ons 1 ---- 


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


[1mindexing[0m [34museful_functions_module.r[0m [=========================] [32m2.15GB/s[0m, eta: [36m 0s[0m
                                                                                                                    

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
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)


[1mindexing[0m [34mmisc_info.R[0m [=====================================] [32m233.15MB/s[0m, eta: [36m 0s[0m
                                                                                                                    

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
# NB. "Monroe" is in borh regions
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

[1mindexing[0m [34mboats_number_get_data.R[0m [=========================] [32m489.24MB/s[0m, eta: [36m 0s[0m
                                                                                                                    

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

# source(file.path(my_paths$git_r, r"(get_data\get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()

# get logbooks 2022 only ----
## logbooks data----
# for qmd use  #' {{< include .qmd >}} instead of # source()

# 2a)
# use all logbooks from https://drive.google.com/drive/folders/1HipnxawNsDjrsMc4dXgFwdRPQ3X6-x3n
processed_logb_path <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs\SEFHIER_usable_logbooks_2022.rds)")

# file.exists(processed_logb_path)

processed_logbooks <-
  read_rds(processed_logb_path)

processed_logbooks_clean_names <-
  clean_headers(processed_logbooks)

# 2b)
# dim(all_get_db_data_result_l$mv_safis_trip_download)
# [1] 735666    149

# intersect(ordered(names(processed_logbooks_clean_names)),
#           ordered(names(all_logbooks_db_data_2022_short_p_region)))
# 70

# setdiff(ordered(names(processed_logbooks_clean_names)),
#           ordered(names(all_logbooks_db_data_2022_short_p_region)))
# 85

# setdiff(ordered(names(all_logbooks_db_data_2022_short_p_region)),
#         ordered(names(processed_logbooks_clean_names)))
# [1] "vessel_official_number" "vessel_name"         "notif_cancel_flag"
# 3

## input data names ----
input_data_df_names <-
  c("all_get_db_data_result_l",
    "processed_logbooks_clean_names")

title_message_print(input_data_df_names)

# all_logbooks_db_data_2022_short_p_region

# diffdf::diffdf(all_logbooks_db_data_2022_short_p_region,
#                processed_logbooks)

# print_df_names(all_logbooks_db_data_2022_short_p_region)
# [1] "trip_id, trip_type_name, vessel_id, vessel_official_number, vessel_name, trip_start_date, trip_end_date, state, state_name, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, activity_type_name, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit, vendor_app_name, vendor_platform, trip_de, trip_ue, trip_dc, trip_uc, area_code, sub_area_code, distance_code, distance_code_name, local_area_code, latitude, longitude, effort_de, effort_ue, effort_dc, effort_uc, catch_uc, user_app, notif_seq, notif_type, notif_accsp_system_id, notif_accsp_permit_id, notif_trip_type, notif_trip_type_name, notif_trip_start_date, notif_trip_start_time, notif_trip_end_date, notif_trip_end_time, notif_start_port, notif_start_port_name, notif_start_port_county, notif_start_port_state, notif_end_port, notif_end_port_name, notif_end_port_county, notif_end_port_state, notif_cancel_flag, notif_email_sent, notif_intended_fishing_flag, notif_gear_type, notif_landing_location, notif_landing_location_name, notif_landing_location_city, notif_landing_location_county, notif_landing_location_state, notif_stat_zone, notif_ue, notif_de, notif_uc, notif_dc, permit_region"


[1mindexing[0m [34mwaters_shape_prep.R[0m [=============================] [32m514.40MB/s[0m, eta: [36m 0s[0m
                                                                                                                    

#### Current file: waters_shape_prep.R ----

# setup ----
# maps:
library(mapview)
library(sf)
## Load the 'tigris' package to access geographic data.
library(tigris)

## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
waters_project_dir_name <- this.path::this.dir()

waters_project_basename <-
  basename(waters_project_dir_name)

waters_output_path <- file.path(my_paths$outputs,
                         waters_project_basename)

my_crs = 4326
## state and county lists ----
misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
# source(misc_info_path)

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

# str(GOMsf)

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

# tic("shp_4326_list map")
# shp_4326_list_map <-
#   map(my_dfs_to_transform,
#          function(x) st_transform(x, my_crs))
# toc()
# shp_4326_list map: 13.97 sec elapsed

names(shp_4326_list) <- my_dfs_to_transform_names

# misc ----
# all waters
# install.packages("ggOceanMaps")
# library(ggOceanMaps)
#
# shapefile_list(name = "all")
#
# basemap(data = lat_lon_gom_state_cnt_sf, bathymetry = TRUE)

# (dd_rbathy)

# Results ----
result_names <- c("GOMsf",
             "world_state_and_fed_waters_path",
             "fl_state_w_counties_shp",
             "GOM_s_fl_state_waters_only",
             "shp_4326_list: ",
             my_dfs_to_transform_names)
title_message_print(result_names)


[1mindexing[0m [34mboats_number.R[0m [====================================] [32m2.15GB/s[0m, eta: [36m 0s[0m
                                                                                                                    

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

# setup current project ----
# source("~/R_code_github/boats_number/boats_number_sources.R")

## processed_logbooks ----

# names(processed_logbooks_clean_names) |>
#   sort() |>
#   cat(sep = ", ")

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

# grep("state", names(processed_logbooks_clean_names), value = T, ignore.case = T)
processed_logbooks_short <-
  processed_logbooks_clean_names |>
  dplyr::select(dplyr::all_of(port_fields_short)) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short)
# [1] 3011    7
# [1] 2475    6 (no overridden)
# [1] 66641     8 with start and end dates
# [1] 72246    14 with lat/long
# [1] 73368    15 with trip_id

### add date related columns ----
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
# processed_logbooks_short_dates: 2.94 sec elapsed
# processed_logbooks_short_dates: 4.28 sec elapsed

## Prepare home_port data ----
# all_get_db_data_result_l |>
#   print_df_names()

# all_get_db_data_result_l$vessels_permits |>
  # print_df_names()

vessel_permit_port_info <-
  all_get_db_data_result_l$vessels_permits |>
  # active permits in 2022
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2022-12-31" |
      END_DATE > "2022-12-31" |
      EXPIRATION_DATE > "2022-12-31"
  )

dim(vessel_permit_port_info)
# [1] 68113    51
# SERO_OFFICIAL_NUMBER  5220
# SERO_HOME_PORT_CITY    809
# SERO_HOME_PORT_COUNTY   320
# SERO_HOME_PORT_STATE     28

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
# VESSEL_VESSEL_ID      5220

vessel_permit_port_info_short_clean <-
  clean_headers(vessel_permit_port_info_short)

## add vessel_permit home port information to trip (logbook) information ----

# print_df_names(processed_logbooks_short_port_states_fl_reg_one_marker)
# print_df_names(vessel_permit_port_info_short_clean)

join_trip_and_vessel <-
  dplyr::left_join(
    processed_logbooks_short_dates,
    vessel_permit_port_info_short_clean,
    dplyr::join_by(vessel_official_number ==
                     sero_official_number)
  )

dim(join_trip_and_vessel)
# [1] 3011   20
# [1] 2475   19 (processed logbooks)
# [1] 66641    35
# [1] 72246    29 with lat/long
# [1] 73368    30 with trip_id

# with overridden
# vessel_id             1876
# vessel_official_number   1876
# permit_sa_gom            4
# SERO_OFFICIAL_NUMBER  1785

# w/o overridden
# vessel_official_number 1629
# permit_region             2
# PERMIT_VESSEL_ID        1562
# VESSEL_VESSEL_ID        1562
# SERO_HOME_PORT_CITY      361
# SERO_HOME_PORT_COUNTY    145
# SERO_HOME_PORT_STATE      20
# latitude                52887
# longitude               53423

# remove trailing spaces

join_trip_and_vessel_trim <-
  join_trip_and_vessel |>
  mutate_if(is.character, str_trim)

diffdf::diffdf(join_trip_and_vessel,
               join_trip_and_vessel_trim)

## Lower case of all data ----
join_trip_and_vessel_low <-
  join_trip_and_vessel_trim |>
  mutate_if(is.character, tolower)

# remove not a-z in strings (e.g. "St. John" or
# "miami dade" vs. "miami-dade")

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
      rlang::quo(!!port_county_column %in% tolower(fl_counties$gom))

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

# TODO: Why there is no home port?
join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(contains("port")) |>
  distinct() |>
  glimpse()

# Add port state regions ----
# ?? Use a start port state instead of filter(!is.na(sero_home_port_state))
tic("join_trip_and_vessel_clean_state_regions")
join_trip_and_vessel_clean_state_regions <-
  join_trip_and_vessel_clean |>
  filter(!is.na(sero_home_port_state)) |>
  add_region_to_state("sero_home") |>
  add_region_to_state("end")
toc()
# join_trip_and_vessel_clean_state_regions: 33.63 sec elapsed

# Split by home port regions ----
join_trip_and_vessel_clean_state_regions_l <-
  join_trip_and_vessel_clean_state_regions |>
  split(as.factor(
    join_trip_and_vessel_clean_state_regions$sero_home_state_region
  ))

map(join_trip_and_vessel_clean_state_regions_l,
    count_uniq_by_column)

# $gom
# vessel_official_number    944
# $sa
# vessel_official_number    617

# dim(join_trip_and_vessel_clean_state_regions_l$gom)
# [1] 53582    32

## Short GOM df ----
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

## count different counties ----
# start_end_county_diff |> print_df_names()

start_end_county_diff_gom_num <-
  start_end_county_diff_gom |>
  add_count(permit_region, sero_home_port_county,
            end_port_county, trip_end_year_quarter,
            name = "cnt_diff_county")

dim(start_end_county_diff_gom_num)
# [1] 250   5
# [1] 575  11
# [1] 291  13 gom only

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

# join_trip_and_vessel_clean |>
#   filter(sero_home_port_county == "santa rosa" &
#            end_port_county == "escambia") |>
#   select(trip_end_year_quarter, vessel_official_number) |>
#   distinct() |>
#   arrange(trip_end_year_quarter, vessel_official_number) |>
#   View()

# vessels by quarter, correct
start_end_county_diff_gom_num |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  arrange(trip_end_quarter_num) |>
  glimpse()
# $ trip_end_quarter_num    <chr> "1", "2", "2", "3"
# $ cnt_diff_county         <int> 1, 2, 2, 1

## Result table for GOM permit region (and the state region) ----
start_end_county_diff_gom_num_gom_permit_only <-
  start_end_county_diff_gom_num |>
  filter(permit_region == "gom")

# check
dim(start_end_county_diff_gom_num_gom_permit_only)
# [1] 270  13

# state == gom, permit gom or sa
start_end_county_diff_gom_num |>
# select(sero_home_port_state) |>
# distinct() |> # 5 states
dim()
# [1] 291  13

#### make the result table ----
# print_df_names(start_end_county_diff_gom_num_gom_permit_only)

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
# 47 tot Q4
# 2 for Brazoria - Galveston

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


# join_trip_and_vessel_clean |>
#   filter(
#     sero_home_port_county == "pinellas" &
#       end_port_county == "brunswick"
#   ) |>
#   select(vessel_official_number, trip_end_year_quarter) |>
# View()
# 1 vessel, correct

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
start_end_county_diff_gom_num_gom_permit_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_county_num_of_vessels_tot")

head(start_end_county_diff_gom_num_gom_permit_only_res_quarter)
#   trip_end_year_quarter diff_county_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                           24
# 2 2022 Q2                                          103
# 3 2022 Q3                                           96
# 4 2022 Q4                                           47

# How many SEFHIER vessels have a different start port state than end port state? ----

## different start and end states in one trip ----

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

# View(start_end_state_diff_num_gom_only_res)

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
# 2 2022 Q2                                           30
# 3 2022 Q3                                           26
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

## save results to csv ----
start_end_state_diff_num_gom_only_res |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  ungroup() |>
  write_csv(file.path(curr_proj_output_path,
                      "start_end_state_diff_num_gom_only_res.csv"),)


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

start_end_state_diff_num_gom_only_res_cnts_by_home_sum <-
  start_end_state_diff_num_gom_only_res_cnts_by_home |>
  add_count(trip_end_year_quarter,
            sero_home_port_state,
            wt = states_cnt,
            name = "sum_by_q_and_home")

start_end_state_diff_num_gom_only_res_cnts_by_home_sum |>
  write_csv(
    file.path(
      curr_proj_output_path,
      "start_end_state_diff_num_gom_only_res_cnts_by_home_sum.csv"
    )
  )

## State to state by state and quarter res table ----
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

write_csv(
  start_end_state_diff_num_gom_only_res_home,
  file.path(curr_proj_output_path,
            "start_end_state_diff_num_gom_only_res_home.csv"),
)


# How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)? ----

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

# result for state region to region
# 3 in the whole year (for GOM home port, and GOM permit)
#   trip_end_year_quarter diff_port_regions_num_of_vessels_tot
#   <yearqtr> <int>
# 1 2022 Q1   1
# 2 2022 Q2   1
# 3 2022 Q4   1

#' Nothing to show, only 1 vessel has trips starting in Fl and ending in North Carolina in Q2 and 1 vessel in Q4.
#' Plus 1 vessel in Q1 from Sarasota, Florida to Duval, Florida

# Quantify the # of vessels who fish in both the gulf and S Atl ----
# GOM permit
# retain Monroe
# Create 2 dfs fished in GOM or in SA using lat and lon for area fished
# grouping by vessel ID and quarter, check if unique vessel fishing in GOM and in SA

## prep fishing locations ----
# [1] "vessel_official_number, end_port_name, end_port_state, end_port_county, end_port, permit_region, start_port_name, start_port_state, start_port_county, start_port, trip_id, trip_end_date, trip_start_date, latitude, longitude, trip_start_week_num, trip_end_week_num, trip_start_y, trip_end_y, trip_start_m, trip_end_m, trip_start_year_quarter, trip_start_quarter_num, trip_end_year_quarter, trip_end_quarter_num, permit_vessel_id, vessel_vessel_id, sero_home_port_city, sero_home_port_county, sero_home_port_state, sero_home_state_region, end_state_region"

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

# dim(lat_lon_gom_state)
# [1] 46181     5

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

# View(lat_lon_gom_state_cnt)
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
# [1] 36173     7

# all points
# mapview(lat_lon_gom_state_cnt_sf)

## Join 'my_sf' with 'my_shp' to crop it, leaving only the intersecting geometries.
## extract the longitude and latitude coordinates from the joined spatial object.
## Return the cropped and transformed spatial object.
crop_by_shape <- function(my_sf, my_shp = GOMsf) {
  my_sf |>
    sf::st_join(my_shp, left = FALSE) %>%

  dplyr::mutate(longitude = sf::st_coordinates(.)[, 1],
         latitude = sf::st_coordinates(.)[, 2]) %>%

  return()
}

# lat_lon_gom_state_cnt_sf |>
#   mapview(
#     cex = "cnt_v_coords_by_y",
#     alpha = 0.3,
#     col.regions = viridisLite::turbo,
#     # legend = FALSE
#     layer.name = "GOM permit trips"
#   )

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

## split by region using shape files ----
intersect_waters_and_points <-
  function(my_shp,
           my_points) {
    intersect_result <-
      st_intersection(my_shp,
                      my_points)
    return(intersect_result)
  }

subtract_waters_from_points <-
  function(my_shp,
           my_points) {
    difference_result <-
      st_difference(my_points,
                    my_shp)
    return(difference_result)
  }

### fishing in GOM  ----
gom_lat_lon_gom_state_cnt_sf_fed_w_file_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "gom_lat_lon_gom_state_cnt_sf_fed_w.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

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

### fewer points, to speed up ----
sa_bb <- st_bbox(shp_4326_list$sa_shp)
sa_bb_points <- st_crop(lat_lon_gom_state_cnt_sf, sa_bb)

# dim(sa_bb_points)
# [1] 10293     7

#### state waters, Monroe in both regions ----
# mapview(east_coast_sa_state_waters_shp)

sa_state_waters_points_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "sa_all_state_waters_points.rds")

# file.exists(sa_state_waters_points_path)
# unlink(sa_state_waters_points_path)

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
# tic("sa_state_waters_points_no_gom")
# sa_state_waters_points_no_gom <-
#   st_difference(sa_state_waters_points,
#                 gom_lat_lon_gom_state_cnt_sf_fed_w)
# toc()
# had to restart Windows
# st_intersects

# View(sa_state_waters_points)
# > names(sa_fed_waters_points) |>
# + setdiff(names(sa_state_waters_points))
# [1] "Id"       "AreaName"
# > setdiff(names(sa_state_waters_points), names(sa_fed_waters_points))
# [1] "Jurisdicti" "area_mi2"   "area_km2"   "area_nm2"   "Shape_Leng" "Shape_Area"

# names(gom_lat_lon_gom_state_cnt_sf_fed_w)
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

sa_state_waters_points_short_df <-
  st_drop_geometry(sa_state_waters_points_short)

gom_lat_lon_gom_state_cnt_sf_fed_w_short_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w_short)

sa_state_waters_points_short_df_no_gom <-
  anti_join(sa_state_waters_points_short_df,
            gom_lat_lon_gom_state_cnt_sf_fed_w_short_df)

# mapview(sa_state_waters_points_short_df_no_gom,
#         xcol = "longitude",
#         ycol = "latitude",
#         crs = my_crs)

# Remove not sa counties ---
# dim(sa_state_waters_points_short_df_no_gom)
# 560 6

# print_df_names(sa_state_waters_points_short_df_no_gom)

logbooks_w_county <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(sero_home_port_county,
    any_of(names(sa_state_waters_points_short_df_no_gom))) |>
  distinct()

dim(logbooks_w_county)
# [1] 46424     5

sa_state_waters_points_short_df_no_gom_counties <-
  sa_state_waters_points_short_df_no_gom |>
  left_join(logbooks_w_county)
# Joining with `by = join_by(vessel_official_number, latitude, longitude,
# trip_end_year_quarter)`


sa_state_waters_points_short_df_no_gom_counties_sa <-
  sa_state_waters_points_short_df_no_gom_counties |>
  filter(sero_home_port_county %in% tolower(fl_counties$sa))

dim(sa_state_waters_points_short_df_no_gom_counties_sa)
# [1] 544   7

# Check counties ----
# sa_state_waters_points_short_df_no_gom_counties_sa |>
#   select(sero_home_port_county) |>
#   distinct()
# monroe

# mapview(sa_state_waters_points_short_df_no_gom_counties_sa,
#         xcol = "longitude",
#         ycol = "latitude",
#         crs = my_crs)

## join by vessel ----
### back to dfs for join ----
gom_lat_lon_gom_state_cnt_fed_w_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w)
# str(gom_lat_lon_gom_state_cnt_fed_w_df)

sa_lat_lon_gom_state_cnt_sf_fed_w_df <-
  st_drop_geometry(sa_fed_waters_points)
# str(sa_lat_lon_gom_state_cnt_sf_fed_w_df)

# sa_state_waters_points_short_df_no_gom_counties_sa_ok

### join point data frames ----
keep_sa_fields <-
  intersect(
    names(sa_lat_lon_gom_state_cnt_sf_fed_w_df),
    names(sa_state_waters_points_short_df_no_gom_counties_sa)
  )

all_points_sa <-
  list(sa_lat_lon_gom_state_cnt_sf_fed_w_df,
       sa_state_waters_points_short_df_no_gom_counties) |>
  map_df(\(curr_df) {
    curr_df |>
      select(all_of(keep_sa_fields))
  })

dim(all_points_sa)
# [1] 1013    6
# check 994360 in state waters
# all_points_sa_l |>
#   filter(vessel_official_number == "994360") |>
#   View()

gom_lat_lon_gom_state_cnt_fed_w_df_short <-
  gom_lat_lon_gom_state_cnt_fed_w_df |>
  select(all_of(keep_sa_fields))

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
all_fish_points_reg_y <-
  all_fish_points |>
  group_by(vessel_official_number) |>
  mutate(has_gom_point_y =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_y =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

all_fish_points_reg_both_y <-
  all_fish_points_reg_y |>
  filter(has_gom_point_y & has_sa_point_y)

dim(all_fish_points_reg_both_y)
# [1] 8524   18
# [1] 4479   12 gom permit only
# vessel_official_number   76 all permits
# n_distinct(all_fish_points_reg_both_y$vessel_official_number)
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
# [1] 7145   18
# [1] 3940   12 gom permit only

n_distinct(all_fish_points_reg_both_q$vessel_official_number)
# [1] 30

all_fish_points_reg_both_q |>
  select(trip_end_year_quarter, vessel_official_number) |>
  distinct() |>
  count(trip_end_year_quarter)
#   trip_end_year_quarter     n
#   <yearqtr>             <int>
# 1 2022 Q1                  35
# 2 2022 Q2                  31
# 3 2022 Q3                  35
# 4 2022 Q4                  24

# no SA permit
# 1 2022 Q1                  13
# 2 2022 Q2                  14
# 3 2022 Q3                  15
# 4 2022 Q4                  13

## map all_fish_points_reg_both_q ----

# View(all_fish_points_reg_both_q)

all_fish_points_reg_both_q_gom_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.gom", "latitude.gom"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".sa")) |>
  rename(geometry_gom = geometry)

all_fish_points_reg_both_q_sa_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.sa", "latitude.sa"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".gom")) |>
  rename(geometry_sa = geometry)

all_sa_gom_map <-
  mapview(all_fish_points_reg_both_q_sa_sf,
          col.regions = "blue") +
  mapview(all_fish_points_reg_both_q_gom_sf,
          col.regions = "green")

# all_sa_gom_map

### sa and gom map by q ----

all_fish_points_reg_both_q_sf_quaters <-
  list(all_fish_points_reg_both_q_sa_sf,
       all_fish_points_reg_both_q_gom_sf) |>
  map(\(curr_df) {
    list_by_reg_q <-
      curr_df |>
      split(as.factor(curr_df$trip_end_year_quarter))

    return(list_by_reg_q)
  })

names(all_fish_points_reg_both_q_sf_quaters) <-
  c("sa",
    "gom")

# View(all_fish_points_reg_both_q_sf_quaters)

all_quarters_list <-
  names(all_fish_points_reg_both_q_sf_quaters$sa)

# mapview(all_fish_points_reg_both_q_sf_quaters$sa$`2022 Q1`,
        # col.regions = "green") +
# mapview(all_fish_points_reg_both_q_sf_quaters$gom$`2022 Q1`,
        # col.regions = "blue")

all_maps_by_q <-
  all_quarters_list |>
  map(\(curr_quarter) {
    mapview(all_fish_points_reg_both_q_sf_quaters$sa[[curr_quarter]],
            col.regions = "green") +
      mapview(all_fish_points_reg_both_q_sf_quaters$gom[[curr_quarter]],
              col.regions = "blue")
  })

names(all_maps_by_q) <- all_quarters_list


# View(all_fish_points_reg_both_q_sf_quaters$sa)
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
