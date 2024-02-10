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

results <-
  c("processed_logbooks")

cat(results, sep = "\n")
