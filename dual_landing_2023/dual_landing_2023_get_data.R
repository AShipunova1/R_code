# this file is called from dual_landing_2023_start.R

processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# Upload
# 1) Processed Logbooks
# SEFHIER_permitted_vessels_nonSRHS_{my_year}
# "~\R_files_local\my_inputs\processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2022.rds"

# 2) maps

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

# waters_shape_prep ----
waters_shape_prep_path <- 
  file.path(my_paths$git_r,
            "get_data",
            "waters_shape_prep.R")
# file.exists(waters_shape_prep_path)
# T

source(waters_shape_prep_path)

result_names <-
  c("processed_logbooks",
    "south_east_coast_states",
    "east_coast_states",
    "sa_council_states",
    "south_atlantic_states",
    "fl_counties",
    "my_state_abb",
    "my_state_name",
    "GOMsf",
    "world_state_and_fed_waters_path",
    "fl_state_w_counties_shp",
    "GOM_s_fl_state_waters_only",
    "shp_4326_list: ",
    "east_coast_sa_state_waters_shp",
    "gom_fl_state_w_counties_shp",
    "sa_fl_state_w_counties_shp",
    "sa_shp",
    "gom_states_shp",
    "sa_states_shp")

title_message_print(result_names)

