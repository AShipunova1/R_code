## fhier_reports_metrics_tracking ----

# files to have:
# from "Logbook Processing (Do this before all Logbook Analyses)" google drive
# https://drive.google.com/drive/folders/18ociLUchXpLxrhb3-gJRuIV_0PaQGUFy?usp=sharing
# 1) "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv"
# 2) "2023SRHSvessels.csv"

fhier_reports_metrics_tracking_file_path <-
  file.path(
    my_paths$inputs,
    r"(\from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_2023.csv)"
  )

# file.exists(fhier_reports_metrics_tracking_file_path)
# T

# read each csv in a list of dfs
# Use the purrr::map function to read multiple CSV files into a list of data frames.
fhier_reports_metrics_tracking_list <- purrr::map(
  c(fhier_reports_metrics_tracking_file_path),
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
  c("2023")

# View(fhier_reports_metrics_tracking_list)

# check how many in diff years if both ----
check_diff_years <-
  function(variables) {
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
  }

# uncomment if needed
# check_diff_years()
