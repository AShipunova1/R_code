get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

get_metrics_tracking_path <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_metrics_tracking.R")
source(get_metrics_tracking_path)
# View(fhier_reports_metrics_tracking_list)

get_srhs_vessels_path <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_srhs_vessels.R")
source(get_srhs_vessels_path)

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
# [1] 2981    1 2022
# [1] 3387    1 2023

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

# [1] "2024-02-01"
# $`2023`
# [1] 3513   13

purrr::map(fhier_reports_metrics_tracking_not_srhs_ids_list, dim)
# [[1]]
# [1] 3571    1
#
# [[2]]
# [1] 3399    1

# [1] "2024-02-01"
# $`2023`
# [1] 3513   13

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
