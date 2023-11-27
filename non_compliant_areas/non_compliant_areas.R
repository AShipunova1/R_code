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

# run once, gives vessels_permits_home_port_c_st_fixed ----

fix_ports_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_fix_lat_lon.R")


# source(fix_ports_file_path)

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

# join compl and home port ----

names(compl_err_db_data_metrics_permit_reg_list)
# [1] "dual"     "gom_only" "sa_only" 

permit_regions <- names(compl_err_db_data_metrics_permit_reg_list)

compl_err_db_data_metrics_permit_reg_list_home_port <- 
 permit_regions |>
  map(\(permit_reg) {
    compl_err_db_data_metrics_permit_reg_list[[permit_reg]] |>
      left_join(
        vessels_permits_home_port_lat_longs_nc,
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

# count home ports by vessel ----
compl_err_db_data_metrics_permit_reg_list_home_port_cnt <- 
  compl_err_db_data_metrics_permit_reg_list_home_port |>
  map(\(curr_df) {
    curr_df |> 
      count(vessel_official_nbr, is_comp)
  })

glimpse(compl_err_db_data_metrics_permit_reg_list_home_port)

## check comp (all non comp) ----
compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  select(vessel_official_nbr, is_comp) |>
  distinct() |> 
  count(is_comp)
# is_comp    n
# 1       0 1227

## filter by permit expiration ----
compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  filter(comp_week_start_dt < prm_grp_exp_date) |>
  select(vessel_official_nbr, is_comp) |>
  distinct() |>
  count(is_comp)
#   is_comp    n
# 1       0 1227

compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  filter(comp_week_start_dt > prm_grp_exp_date) |>
  # glimpse()
# Rows: 30
  select(vessel_official_nbr, is_comp_override) |> 
  count(is_comp_override)
#   is_comp_override  n
# 1                1 30
# All with exp permit are overridden!

## check overrides ----
compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  select(vessel_official_nbr, is_comp_override) |> 
  distinct() |> 
  count(is_comp_override) |> 
#   is_comp_override     n
# 1                0 872
# 2                1 469
  count(wt = n) 
# 1341 > 1227 (some vessels are sometimes overridden and some times not, so  they are in both categories)

# compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |> 
#   data_overview()
# SERO_HOME_PORT_CITY      249
# lat                      206
# is_comp                    1
# prm_grp_exp_date          63
# vessel_official_nbr     1227

# count vessels by home ports ----

compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  select(
    vessel_official_nbr,
    city_fixed,
    state_fixed
  ) |>
  mutate(city_fixed = trimws(city_fixed),
         state_fixed = trimws(state_fixed)) |>
  count(city_fixed,
        state_fixed)
#        SERO_HOME_PORT_CITY SERO_HOME_PORT_STATE    n
# 1                      0                   UN   40
# 2                     11                   AK    6
# 3              AMELIA IS                   FL    6
# 4          AMELIA ISLAND                   FL    6
# ...
# 229           YANKEETOWN          FL    7
# 230                 <NA>        <NA> 3934

compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  select(vessel_official_nbr,
         lat,
         long) |> 
  count(lat,
         long)
         # lat       long    n
# 1   24.55483  -81.80207 1132
# 2   24.56720  -81.74085   12
# 3   24.65572  -81.41368    3
# 210       NA         NA 4074

compl_err_db_data_metrics_permit_reg_list_home_port_cnt <-
  compl_err_db_data_metrics_permit_reg_list_home_port |>
  map(\(curr_df) {
    curr_df |>
      add_count(lat,
                long, name = "coord_cnt")
  })

## shorten before count ----
compl_err_db_data_metrics_permit_reg_list_home_port_short <-
  compl_err_db_data_metrics_permit_reg_list_home_port |>
  map(\(curr_df) {
    curr_df |>
      select(vessel_official_nbr,
             permit_sa_gom,
             city_fixed,
             state_fixed,
             lat,
             long) |>
      distinct()
  })

map(compl_err_db_data_metrics_permit_reg_list_home_port_short, dim)
# $dual
# [1] 121   6
# 
# $gom_only
# [1] 185   6
# 
# $sa_only
# [1] 1227    6

# add vessel count by place ----
compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt <-
  compl_err_db_data_metrics_permit_reg_list_home_port_short |>
  map(\(curr_df) {
    curr_df |>
      add_count(lat,
                long, name = "coord_cnt")
    
  })

# View(compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt)

# # write home ports to csv ----
# names(compl_err_db_data_metrics_permit_reg_list_home_port) |>
#   map(\(curr_permit_reg_name) {
#     compl_err_db_data_metrics_permit_reg_list_home_port[[curr_permit_reg_name]] |>
#       select(
#         vessel_official_nbr,
#         SERO_HOME_PORT_CITY,
#         SERO_HOME_PORT_COUNTY,
#         SERO_HOME_PORT_STATE
#       ) |>
#       distinct() |> 
#       mutate(
#         SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
#         SERO_HOME_PORT_COUNTY = trimws(SERO_HOME_PORT_COUNTY),
#         SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
#       ) |>
#       arrange(SERO_HOME_PORT_STATE,
#               SERO_HOME_PORT_COUNTY,
#               SERO_HOME_PORT_CITY) |>
#       write_csv(file = 
#                   file.path(
#                     my_paths$outputs,
#                     current_project_dir_name,
#                     stringr::str_glue("{current_project_dir_name}_{curr_permit_reg_name}.csv")
#                   ))
#   })

# convert to sf ----
compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf <- 
  compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt |>
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
  }) |> 
  invisible()

map(compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf, dim)
# $dual
# [1] 107   8
# 
# $gom_only
# [1] 160   8
# 
# $sa_only
# [1] 1017    8

# crop points by south_east_coast_states_shp ----
# tic("st_crop")
# compl_err_db_data_metrics_permit_reg_list_home_port_sf_cropped <-
#   map(compl_err_db_data_metrics_permit_reg_list_home_port_sf,
#       \(permit_region) {
#         sf::st_crop(permit_region,
#                     south_east_coast_states_shp)
#       })
# toc()

# map sa ----
print_df_names(compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf$sa_only)

## prepare add columns ----
compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf$sa_only |>
  mutate(my_label =
           str_glue("{city_fixed} {state_fixed}; # = {coord_cnt}")) |>
  mapview::mapview(
    zcol = "my_label",
    cex = "coord_cnt",
    alpha = 0.3,
    col.regions = viridisLite::turbo,
    legend = FALSE,
    layer.name = 'Vessel count by home port coordinates'
  ) +
  south_east_coast_states_shp

# check cnts on map ----
jupiter_cnt_1 <-
  compl_err_db_data_metrics_permit_reg_list_home_port$sa_only |>
  filter(city_fixed == "JUPITER") |>
  # dim()
  # [1] 351  30
  select(vessel_official_nbr) |>
  distinct() |>
  dim()
# [1] 13  1

jupiter_cnt_2 <- 
  compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf$sa_only |>
  filter(city_fixed == "JUPITER") |>
  dim()
# [1] 13  8

jupiter_cnt_1[[1]] == jupiter_cnt_2[[1]]
T

# BAYOU LA BATRE
mult_names <- "AMELIA"

compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf |>
  map(\(curr_df)
      {
        curr_df |>
          filter(grepl(mult_names, city_fixed)) |>
          glimpse()
  })

compl_err_db_data_metrics_permit_reg_list_home_port_short_cnt_sf$sa_only |>
  select(city_fixed,
         state_fixed,
         coord_cnt) |>
  mutate(my_label =
           str_glue("{city_fixed} {state_fixed}; # = {coord_cnt}")) |>  distinct() |>
  filter(state_fixed == "FL") |> 
  arrange(city_fixed)
