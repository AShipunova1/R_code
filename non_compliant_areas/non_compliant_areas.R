# identifying any particular areas of high non-compliance to help focus future outreach efforts. 
# do this as a map
# home port from the permit as an area
# source the usual setup 
# get data
# remove not in metrics
# separate by permit region
# remove not in Jeannette's SA list
# remove not in Jeannette's GOM list
# add home port

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)

if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")
  library(tidygeocoder)
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_path <- this.path::this.dir()

current_project_dir_name <- basename(current_project_dir_path)

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
vessels_permits_home_port <-
  all_get_db_data_result_l$vessels_permits |>
  filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |> 
  filter(EFFECTIVE_DATE < "2021-12-31") |> 
  select(SERO_OFFICIAL_NUMBER, starts_with("SERO_HOME")) |> 
  distinct()

# print_df_names(vessels_permits_home_port)
# [1] 6762    4
# [1] 4729    4 date filters

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

## add lat/lon ----
my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_dir_name,
            paste0(current_project_dir_name, ".rds"))

file.exists(my_file_path_lat_lon)

get_lat_lon <-
  function(vessels_permits_home_port) {
    vessels_permits_home_port_lat_longs <-
      vessels_permits_home_port |>
      tidygeocoder::geocode(city = "SERO_HOME_PORT_CITY",
                            state = "SERO_HOME_PORT_STATE",
                            county = "SERO_HOME_PORT_COUNTY")
    return(vessels_permits_home_port_lat_longs)
  }

read_rds_or_run(my_file_path_lat_lon,
                            my_data = as.data.frame(vessels_permits_home_port_lat_longs),
                            get_lat_lon)


tic("vessels_permits_home_port_lat_longs")
vessels_permits_home_port_lat_longs <- 
  vessels_permits_home_port |> 
  tidygeocoder::geocode(
    city = "SERO_HOME_PORT_CITY",
    state = "SERO_HOME_PORT_STATE",
    county = "SERO_HOME_PORT_COUNTY"
    # ,
    # return_addresses = TRUE
  )
toc()
# vessels_permits_home_port_lat_longs: 758.14 sec elapsed

readr::write_rds(vessels_permits_home_port_lat_longs,
                 my_file_path_lat_lon)
#### check ----
vessels_permits_home_port_lat_longs |> 
  filter(!tolower(trimws(SERO_HOME_PORT_CITY)) == tolower(trimws(city))) |> 
  dim()
# 0

vessels_permits_home_port_lat_longs |> 
  filter(!tolower(trimws(SERO_HOME_PORT_COUNTY)) == tolower(trimws(county))) |> 
  dim()
# 0

vessels_permits_home_port_lat_longs |> 
  filter(!tolower(trimws(SERO_HOME_PORT_STATE)) == tolower(trimws(state))) |> 
  dim()
# 0

vessels_permits_home_port_lat_longs_sh <-
  vessels_permits_home_port_lat_longs |>
  select(-c(city, county, state))

dim(vessels_permits_home_port_lat_longs_sh)
# [1] 4729    6

# join compl and home port ----

names(compl_err_db_data_metrics_permit_reg_list)
# [1] "dual"     "gom_only" "sa_only" 

permit_regions <- names(compl_err_db_data_metrics_permit_reg_list)

compl_err_db_data_metrics_permit_reg_list_home_port <- 
 permit_regions |>
  map(\(permit_reg) {
    compl_err_db_data_metrics_permit_reg_list[[permit_reg]] |>
      left_join(
        vessels_permits_home_port_lat_longs_sh,
        join_by(vessel_official_nbr == SERO_OFFICIAL_NUMBER)
      ) |> 
      remove_empty_cols()
  })

names(compl_err_db_data_metrics_permit_reg_list_home_port) <- 
  permit_regions
  
map(compl_err_db_data_metrics_permit_reg_list_home_port, dim)
# $dual
# [1] 1317   34
# 
# $gom_only
# [1] 1358   32
# 
# $sa_only
# [1] 23716    31


# convert to sf ----
compl_err_db_data_metrics_permit_reg_list_home_port_sf <- 
  compl_err_db_data_metrics_permit_reg_list_home_port |>
  map(\(reg_df) {
    reg_df |>
      filter(!is.na(long) &
               !is.na(lat)) |> 
      sf::st_as_sf(
        # Specify the field names to use as coordinates
        coords = c("long", "lat"),
        # Use the provided CRS (Coordinate Reference System), default to sa_shp's CRS
        crs = crs4326,
        # Keep the LATITUDE and LONGITUDE columns in the resulting sf object
        remove = FALSE
      )
  })

map(compl_err_db_data_metrics_permit_reg_list_home_port_sf, dim)
# fewer, because no coords:
# $dual
# [1] 1078   35
# 
# $gom_only
# [1] 1219   33
# 
# $sa_only
# [1] 18604    32

# crop points by south_east_coast_states_shp ----
tic("st_crop")
compl_err_db_data_metrics_permit_reg_list_home_port_sf_cropped <-
  map(compl_err_db_data_metrics_permit_reg_list_home_port_sf,
      \(permit_region) {
        sf::st_crop(permit_region,
                    south_east_coast_states_shp)
      })
toc()

# map sa ----
mapview::mapview(compl_err_db_data_metrics_permit_reg_list_home_port_sf$sa_only) +
  south_east_coast_states_shp

