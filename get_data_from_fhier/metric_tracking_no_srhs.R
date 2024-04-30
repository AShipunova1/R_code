source(file.path(my_paths$git_r,
                 "get_data_from_fhier",
                 "get_metrics_tracking.R"))

source(file.path(my_paths$git_r,
                 "get_data_from_fhier",
                 "get_srhs_vessels.R"))

## exclude srhs vessels from metric traking ----
fhier_reports_metrics_tracking_not_srhs_ids <-
  map_df(
    fhier_reports_metrics_tracking_list,
    ~ .x |>
      filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__)
  ) |>
  select(vessel_official_number) |>
  distinct()

dim(fhier_reports_metrics_tracking_not_srhs_ids)
# [1] 2981    1
# browser()
fhier_reports_metrics_tracking_not_srhs_ids_list <-
  map(
    fhier_reports_metrics_tracking_list,
    ~ .x |>
      filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__) |>
      select(vessel_official_number) |>
      distinct()
  )

# check
map(fhier_reports_metrics_tracking_list, dim)
# [[1]]
# [1] 3634   13
# 
# [[2]]
# [1] 3460   13

map(fhier_reports_metrics_tracking_not_srhs_ids_list, dim)
# [[1]]
# [1] 3571    1
# 
# [[2]]
# [1] 3399    1
