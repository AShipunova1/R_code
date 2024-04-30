## fhier_reports_metrics_tracking ----

fhier_reports_metrics_tracking_file_names <-
  c("Detail_Report_12312021_12312022__08_23_2023.csv",
    "Detail_Report_12312022_12312023__08_23_2023.csv")

common_dir <-
  r"(~\R_files_local\my_inputs\from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source))"

fhier_reports_metrics_tracking_file_path <-
  map(fhier_reports_metrics_tracking_file_names,
      ~ file.path(common_dir,
                  .x))

# test
map(fhier_reports_metrics_tracking_file_path,
    file.exists)
# T

fhier_reports_metrics_tracking_list <-
  map(
    fhier_reports_metrics_tracking_file_path,
    ~ read_csv(
      .x,
      # read as character
      col_types = cols(.default = 'c'),
      name_repair = fix_names
    )
  )

# check how many in diff years ----
setdiff(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# [1] 669

setdiff(fhier_reports_metrics_tracking_list[[2]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[1]]$vessel_official_number) |>
  length()
# [1] 493

intersect(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# 2965
