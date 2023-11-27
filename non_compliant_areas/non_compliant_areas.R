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

# join compl and home port ----

names(compl_err_db_data_metrics_permit_reg_list)
# [1] "dual"     "gom_only" "sa_only" 

permit_regions <- names(compl_err_db_data_metrics_permit_reg_list)

compl_err_db_data_metrics_permit_reg_list_home_port <- 
 permit_regions |>
  map(\(permit_reg) {
    compl_err_db_data_metrics_permit_reg_list[[permit_reg]] |>
      left_join(
        vessels_permits_home_port_lat_longs,
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
# All are overridden!

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
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_STATE
  ) |>
  mutate(SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
         SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)) |> 
  count(SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_STATE)
#        SERO_HOME_PORT_CITY SERO_HOME_PORT_STATE    n
# 1                      0                   UN   40
# 2                     11                   AK    6
# 3              AMELIA IS                   FL    6
# 4          AMELIA ISLAND                   FL    6
# 26        CAROLINA BEACH                   NC  263
# 27        CAROLINA BEACH                   UN   40
# 57         FERNADINA BCH                   FL   17
# 58      FERNANDINA BEACH                   FL   80
# FORT LAUDERDALE	BROWARD
# FT LAUDERDALE	BROWARD

# 70            GEORGETOWN                   SC   46
# 71            GEORGRTOWN                   SC   12
# 78           HILTON HEAD                   SC   58
# 79    HILTON HEAD ISLAND                   SC   91
# 88            ISLAMORADA                   FL  772
# 89            ISLAMORADA                   UN   36
# 91          JACKSONVILLE                   FL  356
# 92    JACKSONVILLE BEACH                   FL  106
# 93                   JAX                   FL    6
# 100             KEY WEST                   FL 1114
# 102              KEYWEST                   FL   18
# 105               KODIAK                   AK    5
# 112         LITTLE RIVER                   SC  222
# 113 LITTLE RIVERNHV1N4WH                   SC    7
# 125                MIAMI                   FL  461
# 126          MIAMI BEACH                   FL   18
# 132       MOUNT PLEASANT                   SC   35
# 133          MT PLEASANT                   SC   32
# 134         MT. PLEASANT                   SC   26
# 135       MURRELLS INLET                   SC  375
# 136        MURRELS INLET                   SC   15
# 142      NEW SMYMA BEACH                   FL   18
# 143     NEW SMYRNA BEACH                   FL  276
# 148                NORTH                   FL   10
# 149          NORTH MIAMI                   FL   40
# 150    NORTH MIAMI BEACH                   FL   43
# 155           OCEAN CITY                   MD 1073
# 158          OCEEAN CITY                   MD    8
# 164           PALM BEACH                   FL  192
# 165   PALM BEACH GARDENS                   FL   30
# 181   PORT OF PALM BEACH                   FL    8
# 166            PALM CITY                   FL    6
# 171       POINT PLEASANT                   NJ   20
# 172 POINT PLEASANT BEACH                   NJ    2
# 173    POINT PLEASANT NJ                   NJ    5
# 192        RIVERIA BEACH                   FL   49
# 193        RIVIERA BEACH                   FL   10
# 197              SAVANAH                   GA   10
# 198             SAVANNAH                   GA  100
# 195      SAINT AUGUSTINE                   FL   29
# 210         ST AUGUSTINE                   FL  453
# 213        ST. AUGUSTINE                   FL   47
# 224            TAVERNIER                   FL  191
# 225        TAVERNIER KEY                   FL   27
# 238     WRIGHTSVILLE BCH                   NC   36
# 239   WRIGHTSVILLE BEACH                   NC  174
# 241                 <NA>                   UN   49
# 242                 <NA>                 <NA> 3934

# write home ports to csv ----
names(compl_err_db_data_metrics_permit_reg_list_home_port) |>
  map(\(curr_permit_reg_name) {
    compl_err_db_data_metrics_permit_reg_list_home_port[[curr_permit_reg_name]] |>
      select(
        vessel_official_nbr,
        SERO_HOME_PORT_CITY,
        SERO_HOME_PORT_COUNTY,
        SERO_HOME_PORT_STATE
      ) |>
      distinct() |> 
      mutate(
        SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
        SERO_HOME_PORT_COUNTY = trimws(SERO_HOME_PORT_COUNTY),
        SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
      ) |>
      arrange(SERO_HOME_PORT_STATE,
              SERO_HOME_PORT_COUNTY,
              SERO_HOME_PORT_CITY) |>
      write_csv(file = 
                  file.path(
                    my_paths$outputs,
                    current_project_dir_name,
                    stringr::str_glue("{current_project_dir_name}_{curr_permit_reg_name}.csv")
                  ))
  })

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
  }) |> 
  invisible()

map(compl_err_db_data_metrics_permit_reg_list_home_port_sf, dim)
# fewer, because some have no coords:
# $dual
# [1] 1078   35
# 
# $gom_only
# [1] 1219   33
# 
# $sa_only
# [1] 18604    32

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
compl_err_db_data_metrics_permit_reg_list_home_port_sf$sa_only |>
    mutate(my_label =
             str_glue("{SERO_HOME_PORT_CITY} {SERO_HOME_PORT_STATE}; # = {total_place_cnt}")) |>
mapview::mapview(
 zcol = "my_label",
      cex = "total_place_cnt",
      alpha = 0.3,
      col.regions = viridisLite::turbo,
      legend = FALSE,
      layer.name = 'Counts by year and place'
                 ) +
  south_east_coast_states_shp

