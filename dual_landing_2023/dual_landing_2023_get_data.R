# this file is called from dual_landing_2023_start.R

processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# 1) Processed Logbooks
# SEFHIER_permitted_vessels_nonSRHS_{my_year}
# "~\R_files_local\my_inputs\processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2022.rds"

# 2) vessels_with_zero_logbooks_{my_year}

# get processed logbook data ----
# For all years
processed_logbooks_file_names <-
  list.files(
    path = processed_input_data_path,
    pattern =
      str_glue("SEFHIER_processed_Logbooks_{my_year2}"),
    recursive = TRUE,
    full.names = TRUE
  )

processed_logbooks <-
  map_df(processed_logbooks_file_names,
         read_rds)

names(processed_logbooks) <-
  names(processed_logbooks) |>
  tolower()

# get vessels with no logbooks ----
# For all years
vessels_no_logbooks_file_names <-
  list.files(
    path = processed_input_data_path,
    pattern =
      str_glue("vessels_with_zero_logbooks_{my_year2}"),
    recursive = TRUE,
    full.names = TRUE
  )

vessels_no_logbooks <-
  map_df(vessels_no_logbooks_file_names,
         read_rds) |> 
  distinct()

names(vessels_no_logbooks) <-
  names(vessels_no_logbooks) |>
  tolower()

# results:
# all_get_db_data_result_l
# processed_metrics_tracking_permits
# processed_logbooks
# vessels_no_logbooks
