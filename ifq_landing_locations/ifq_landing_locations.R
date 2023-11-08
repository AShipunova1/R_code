# setup for ifq_landing_locations ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

# get data for ifq_landing_locations ----
# 1) convert addresses from the original csv to coordinates with ARCgis
# 2) upload the result to R
input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use_geocoded.csv)")

input_data <- readr::read_csv(input_data_file_path)

# problems(input_data)
# # A tibble: 2 × 5
#     row   col expected actual file                                                      
#   <int> <int> <chr>    <chr>  <chr>                                                     
# 1  1264    68 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…
# 2  1264    78 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…

# data_overview(input_data)
# print_df_names(input_data)

# fill in empty ----
input_data |>
  filter(USER_FK_LANDING_LOCATION_ID == "2128") |>
  select(USER_LATITUDE,
         USER_LONGITUDE) |> 
  distinct() |>
  glimpse()


res <-
  strsplit("27° 54.478'",
           "\\D+")

# str(res)
# List of 1

# North West coords only
convert_dms_to_dd_nw <- 
  function(one_dms_coord) {
    res <-
      strsplit("27° 54.478'",
               "(\\d+)\D+(\\d+)\\.(\\d+)\D+")
    
  }

USER_FK_LANDING_LOCATION_ID
# 2128	27° 54.478' N	97° 07.991' W
# 27° 54' 478"
# = 27° + 54'/60 + 478"/3600
# = 28.032778°
# 
# 97° 07' 991"
# = 97° + 07'/60 + 991"/3600
# = 97.391944°

# check if given lat/lon is different from geocoded ----
# USER_LATITUDE, USER_LONGITUDE, x, y, OID_, USER_FK_LANDING_LOCATION_ID
