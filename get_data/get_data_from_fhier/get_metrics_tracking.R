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
