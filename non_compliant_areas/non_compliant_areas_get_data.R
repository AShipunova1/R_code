db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")

source(db_data_path)

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 8.86 sec elapsed

compl_err_db_data <- 
  all_get_db_data_result_l$compl_err_db_data

# use metricks only vessels ----
source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")

# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_err_db_data_metrics <-
  compl_err_db_data |>
  filter(
    vessel_official_nbr %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# divide by permit region ----
compl_err_db_data_metrics_permit_reg <-
  compl_err_db_data_metrics |> 
  filter(comp_week_start_dt < '2023-01-01' &
           comp_week_end_dt >= '2022-01-01') |> 
  separate_permits_into_3_groups(permit_group_field_name = "permit_group") |>
  # [1] 26391    39
  remove_empty_cols() |> 
  distinct()

dim(compl_err_db_data_metrics_permit_reg)
# [1] 26391    29

# split into separate dfs by permit region ----
compl_err_db_data_metrics_permit_reg_list <- 
  compl_err_db_data_metrics_permit_reg |> 
  split(as.factor(compl_err_db_data_metrics_permit_reg$permit_sa_gom))

map(compl_err_db_data_metrics_permit_reg_list, dim)
# $dual
# [1] 1317   29
# 
# $gom_only
# [1] 1358   29
# 
# $sa_only
# [1] 23716    29

### remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
compl_err_db_data_metrics_permit_reg_sa_only <-
  compl_err_db_data_metrics_permit_reg_list$sa_only |>
  filter(!vessel_official_nbr %in% vessels_to_remove_from_ours)

dim(compl_err_db_data_metrics_permit_reg_sa_only)
# [1] 22228    29

# prepare vessel_permit_data ----
## 2022 permits ----
vessels_permits_home_port_22 <-
  all_get_db_data_result_l$vessels_permits |>
  filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |> 
  filter(EFFECTIVE_DATE < "2021-12-31") |> 
  remove_empty_cols()
  
## add permit region ----
vessels_permits_home_port_22_reg <-
  vessels_permits_home_port_22 |>
  mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") 
## shorten permit_vessel ----
vessels_permits_home_port <-
  vessels_permits_home_port_22_reg |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  distinct()
# View(vessels_permits_home_port)
# [1] 5029    5

# Create a new data frame 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes. ----
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

misc_info_path <- file.path(my_paths$git_r,
               r"(get_data\misc_info.R)")

source(misc_info_path)

## Rows are retained if the 'NAME' column (state name) matches any of the values in 'south_east_coast_states'.
south_east_coast_states_shp <-
  us_s_shp |>
  filter(NAME %in% south_east_coast_states)

tigris_crs <- sf::st_crs(south_east_coast_states_shp)
# User input: NAD83 
# ID["EPSG",4269]]
crs4326 <- 4326

# run once, gives vessels_permits_home_port_c_st_fixed ----

fix_ports_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_fix_lat_lon.R")

source(fix_ports_file_path)

glimpse(vessels_permits_home_port_c_st_fixed)

## shorten fixed
vessels_permits_home_port_c_st_fixed_short <-
  vessels_permits_home_port_c_st_fixed |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         city_fixed,
         state_fixed) |>
  distinct() |>
  remove_empty_cols()
  
View(vessels_permits_home_port_c_st_fixed)
# [1] 5029    8
dim(vessels_permits_home_port_c_st_fixed_short)
# [1] 5029    4

# add dual permit_region ----
glimpse(vessels_permits_home_port_c_st_fixed_short)
# TODO: why all dual?
  # vessels_permits_home_port_c_st_fixed_short

# add lat long to fixed ports ----

my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_dir_name,
            paste0(current_project_dir_name, "_no_county_fixed.rds"))

file.exists(my_file_path_lat_lon)

get_lat_lon_no_county <-
  function(vessels_permits_home_port_c_st_fixed) {
    vessels_permits_home_port_c_st_fixed_lat_longs <-
      vessels_permits_home_port_c_st_fixed |>
    select(SERO_OFFICIAL_NUMBER,
           permit_sa_gom,
           city_fixed,
           state_fixed) |>
      tidygeocoder::geocode(city = "city_fixed",
                            state = "state_fixed")
    return(vessels_permits_home_port_c_st_fixed_lat_longs)
  }

vessels_permits_home_port_lat_longs_nc <-
  read_rds_or_run(my_file_path_lat_lon,
                  my_data =
                    as.data.frame(vessels_permits_home_port_c_st_fixed),
                  get_lat_lon_no_county)
# 2023-11-27 run for non_compliant_areas_no_county_fixed.rds: 632.64 sec elapsed
# Warning message:
# In query_api(api_url, api_query_parameters, method = method) :
#   Internal Server Error (HTTP 500).

dim(vessels_permits_home_port_lat_longs_nc)
# [1] 5029    5

# get data for all trips ----
## get all trips
## add weeks
## mark compl/non compl by week from compl_err_db_data
## add home port for all vessels
## add counts

## get all vessels with 2022 permits ----
vessels_permits_home_port_lat_longs_nc |> 
  dim()
# [1] 5029    5

# [1] 4729    5
# SERO_OFFICIAL_NUMBER 4729
# lat                   547



#   select(TRIP_ID,
#          TRIP_START_DATE,
#          TRIP_END_DATE,
#          VESSEL_ID)
# all_get_db_data_result_l$trip_neg_2022 |> 
#   
# all_get_db_data_result_l$trips_notifications_2022
## add weeks
## mark compl/non compl by week from compl_err_db_data
## add home port for all vessels
## add counts

cat("Data:",
  "all_sheets_l",
  "vessels_22_sa",
  "vessels_to_remove_from_ours",
  "all_get_db_data_result_l",
  "vessels_permits_home_port_lat_longs_nc",
  sep = "\n"
)
