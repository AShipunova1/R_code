## fhier_reports_metrics_tracking ----

library(tidyverse)

# help functions
# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

fix_names <- function(x) {
  x %>%
    # remove dots
    stringr::str_replace_all("\\.", "") %>%
    # all not letters and numbers to underscores
    stringr::str_replace_all("[^A-z0-9]", "_") %>%
    # letters only in the beginning
    stringr::str_replace_all("^(_*)(.+)", "\\2\\1") %>%
    # tolower
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
  r"(~\R_files_local\my_inputs\from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source))"

# save all file names to a list
fhier_reports_metrics_tracking_file_path <-
  purrr::map(fhier_reports_metrics_tracking_file_names,
      ~ file.path(common_dir,
                  .x))

# test
purrr::map(fhier_reports_metrics_tracking_file_path,
    file.exists)
# T

# read each csv in a list of dfs
fhier_reports_metrics_tracking_list <-
  purrr::map(
    fhier_reports_metrics_tracking_file_path,
    ~ readr::read_csv(
      .x,
      # read as character
      col_types = cols(.default = 'c'),
      name_repair = fix_names
    )
  )

# check how many in diff years ----
dplyr::setdiff(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# [1] 669

dplyr::setdiff(fhier_reports_metrics_tracking_list[[2]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[1]]$vessel_official_number) |>
  length()
# [1] 493

# in both years
dplyr::intersect(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# 2965
