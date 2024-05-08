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

# Load the 'openxlsx' library, used for reading and writing Excel (.xlsx) files.
library(openxlsx)

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

my_read_xlsx <- function(file_path, sheet_n, start_row = 1) {
  res_df <-
    read.xlsx(
      file_path,
      sheet_n,
      startRow = start_row,
      detectDates = TRUE,
      colNames = TRUE,
      sep.names = "_"
    ) |>
    clean_headers()

  return(res_df)
}

# ===
# The function load_xls_names returns the concatenated data frame containing data from all Excel files. This allows you to work with the combined data more easily.
load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {

  # Extract the 'inputs' directory path from 'my_paths' and store it in 'my_inputs'
  my_inputs <- my_paths$inputs

  # Use 'lapply' to prepend 'my_inputs' directory path to each Excel file name in 'xls_names_list'
  myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))

  # Read Excel files listed in 'myfiles' into one data frame using 'map_df'
  contents <-
    map_df(myfiles,
           ~ my_read_xlsx(.x, # File path
                          sheet_n, # Sheet number to read)
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

## functions to clean FHIER compliance and correspondence reports ----

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
# cleaning, regularly done for csvs downloaded from FHIER
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
#
# benchmarking to insert inside a function
# time_for_appl <<- benchmark(replications=rep(10, 3),
                            # lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE),
                            # sapply(myfiles, read.csv, skipNul = TRUE, header = TRUE, simplify = TRUE)
                            # ,
                            # columns = c('test', 'elapsed', 'relative')
# )

# write.csv(time_for_appl, "time_for_appl.csv")

# or
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

# ===
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

pretty_print <- function(my_text, my_title,
                         the_end = "---") {
  # Print out to console
  title_message_print(my_title)
  cat(c(my_text, the_end),
      sep = "\n")
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

# The read_rds_or_run function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section

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

remove_0_cols <- function(my_df) {
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

# ===
# Explanations:
# The function 'read_an_answer' performs the following operations:
# 1. Reads user input with the provided prompt using 'readline'.
# 2. Checks if the first character of the input is 'n'. If true, sets 'ANSWER' to "no"; otherwise, sets it to "yes".
# 3. Returns the processed answer.
read_an_answer <- function(my_prompt) {
  ANSWER <- readline(my_prompt)
  if (tolower(substr(ANSWER, 1, 1)) == "n")
    ANSWER = "no"
  else
    ANSWER = "yes"
}
# USE:
# if (interactive()) read_an_answer(my_prompt)

# make it "NO_YES" if both compliant and not compliant
# Not tested with overridden
get_compl_by <-
  function(my_df,
           group_by_for_compl =
             vars(-c("vessel_official_number", "compliant_", "overridden_")),
           names_from_list = c("vessel_official_number")) {
    browser()
    my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = all_of(names_from_list),
      values_from = c("compliant_", "overridden_"),
        # compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(unique(sort(.x)), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}

# Example:
# all columns except...
group_by_for_compl <-
  vars(-c("vessel_official_number", "compliant_"))

# ---
# Explanations:
# The function 'compl__back_to_longer_format' performs the following operations:
# 1. Turns the data frame back to a longer format with vessel IDs in one column.
# 2. Specifies the columns to pivot. All columns except those specified in 'cols_names' are treated as vessel IDs.
# 3. Sets the values to the column 'is_compl_or_both'.
# 4. Sets the names to the column 'vessel_official_number'.
# 5. Returns the modified data frame.

# Usage example:
# cols_names are all names except vessel_official_numbers
# cols_names <-
#   c("year",
#     "permit_sa_gom_dual",
#     "total_vsl_y_by_year_perm",
#     "year_permit_sa_gom_dual"
#     )
#
# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both <-
#   compl__back_to_longer_format(
#     compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both,
#     cols_names
#   )
compl__back_to_longer_format <-
  function(my_df,
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

# ===
# Explanations:
# The function 'add_cnt_in_gr' performs the following operations:
# 1. Groups the data frame by the specified columns using `group_by_at(group_by_col)`. `group_by_col` is defined above.
# 2. Adds a new column named 'cnt_col_name' representing the count of distinct vessel official numbers in each group using `mutate({cnt_col_name} := n_distinct(vessel_official_number))`.
# The syntax `{cnt_col_name} :=` is used to create a new column dynamically with the name provided in the `cnt_col_name` argument.
# 3. Removes the grouping to return the data to its original structure with `ungroup()`.
# 4. Returns the modified data frame.
add_cnt_in_gr <-
  function(my_df,
           group_by_col,
           cnt_col_name = "total_vsl_m_by_year_perm") {
    my_df %>%
      # group by per month and permit
      group_by_at(group_by_col) %>%
      # cnt distinct vessels in each group
      mutate({
        {
          cnt_col_name
        }
      } :=
        n_distinct(vessel_official_number)) %>%
      ungroup() %>%
      return()
  }

# ---
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

# ---
# combine columns into one
# Usage:
# db_participants_address__needed_short__phone2 <-
#   db_participants_address__needed_short__phone0 |>
#   group_by(official_number) |>
#   mutate(db_phone = pmap(across(ends_with("_phone")),
#                          ~ list_sort_uniq(.))) |>
#   ungroup()
# ---
# back to chr
# summarise(db_mailing_state1 =
              # paste(sort(unique(str_trim(flatten(db_mailing_state)))), collapse = ", ")) |>

# flatten(list(sort(unique(str_trim(my_lists)))))
list_sort_uniq <- function(my_lists) {
  # browser()
  res <-
    my_lists |>
    str_trim() |>
    unique() |>
    sort() |>
    list() |>
    flatten()
  return(res)
}

# ---
# another usage example
# also uses mutate within a loop to create multiple new columns and binds each one back to the original df.
# db_participants_address__needed_short__erv_erb_combined <-
#   col_part_names |>
#   map(\(curr_col_part)  {
#     new_col_name <- str_glue("db_{curr_col_part}")
#     # cat(new_col_name, sep = "\n")
#
#     db_participants_address__needed_short__phone0 |>
#       group_by(official_number) |>
#       mutate(!!new_col_name :=
#                pmap(across(ends_with(curr_col_part)),
#                     ~ list_sort_uniq(.)),
#              .keep = "none") |>
#       ungroup() |>
#       select(-official_number)
#
#   }) |>
#   bind_cols(db_participants_address__needed_short__phone0, .)

# ===
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

# package.skeleton(
#   name = "auxfunctions",
#   path = ".."
# )
