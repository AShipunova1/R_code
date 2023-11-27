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

get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_get_data.R")

source(get_data_file_path)
# all_get_db_data_result_l
# vessels_permits_home_port_lat_longs_nc

# vessels_permits_home_port_lat_longs_nc |> print_df_names() 
#   group_by(SERO_OFFICIAL_NUMBER) |> 
  
# join compl and home port ----
# vessels_permits_home_port_lat_longs_nc |> 
#   distinct() |> 
#   group_by(SERO_OFFICIAL_NUMBER) %>% 
#   filter(n() > 1) |> 
#   glimpse()

compl_err_db_data_metrics_permit_reg_short <-
  compl_err_db_data_metrics_permit_reg |>
  select(vessel_official_nbr,
         is_comp,
         # comp_year,
         # comp_week,
         permit_sa_gom) |> 
  distinct() |> 
  filter(is_comp == 0)

dim(compl_err_db_data_metrics_permit_reg_short)
# [1] 1514    3

vessels_permits_home_port_lat_longs_nc_comp_err <-
  vessels_permits_home_port_lat_longs_nc |>
  distinct() |>
  left_join(
    compl_err_db_data_metrics_permit_reg_short,
    join_by(SERO_OFFICIAL_NUMBER == vessel_official_nbr)
  )

dim(vessels_permits_home_port_lat_longs_nc_comp_err)
# [1] 25587    33
# [1] 4741    7 distinct
# [1] 4729    7 no comp only

dim(vessels_permits_home_port_lat_longs_nc_comp_err)
# SERO_OFFICIAL_NUMBER 4729
# lat                   547


# count home_port by compliance ----
vessels_permits_home_port_lat_longs_nc_comp_err |> 
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  # glimpse()
  # [1] 845   6
  count(lat, long, is_comp) |> 
  arrange(desc(n)) |> 
    head()
# 1  NA    NA        NA    16
# 2  NA    NA         0     8
# 3  25.9 -81.7       0     3
# 4  26.1 -81.8       0     3
# 5  26.5 -81.9       0     3
# 6  26.5 -81.9      NA     3

vessels_permits_home_port_lat_longs_nc_comp_err |> 
  # dim()
  # [1] 4729    7
  count(lat, long, is_comp) |> 
  arrange(desc(n)) |> 
    head()
#     lat  long is_comp     n
#   <dbl> <dbl>   <int> <int>
# 1  25.9 -97.5      NA   116
# 2  24.6 -81.8      NA   112
# 3  30.4 -88.2      NA   103
# 4  30.0 -90.1      NA    86
# 5  30.4 -88.9      NA    86
# 6  30.3 -87.6      NA    82
# tail()
# 1  43.0  -87.9       0     1
# 2  43.1  -70.8       0     1
# 3  43.9  -70.0       0     1
# 4  46.2 -124.       NA     1
# 5  46.3 -124.       NA     1
# 6  57.8 -152.        0     1

vessels_permits_home_port_lat_longs_nc_comp_err_cnt <- 
  vessels_permits_home_port_lat_longs_nc_comp_err |> 
  add_count(lat, long, is_comp,
            name = "coord_by_comp") |>
  distinct() |> 
  arrange(desc(coord_by_comp)) 

vessels_permits_home_port_lat_longs_nc_comp_err_cnt |> 
  filter(city_fixed == "SEBASTIAN") |> 
  data_overview()
# SERO_OFFICIAL_NUMBER 27
# is_comp               2

vessels_permits_home_port_lat_longs_nc_comp_err_cnt |> 
  filter(city_fixed == "SEBASTIAN") |> 
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  glimpse()
# is_comp       <int> 0, NA
# coord_by_comp <int> 15, 12

# keep only_cnts ----
vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short <-
  vessels_permits_home_port_lat_longs_nc_comp_err_cnt |>
  select(-c(SERO_OFFICIAL_NUMBER, permit_sa_gom)) |>
  distinct()

# add percentage ----
vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short |>
  add_count(city_fixed, 
            state_fixed,
            wt = coord_by_comp,
            name = "tot_cnt100") |> 
  # group_by(is_comp) |> 
  # mutate(tot_cnt100 = sum(coord_by_comp)) |> 
  # test
  filter(city_fixed == "KEY WEST")

vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short_perc <-
  vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short |>
  add_count(city_fixed,
            state_fixed,
            wt = coord_by_comp,
            name = "tot_cnt100") |>
  mutate(is_comp_perc =
           coord_by_comp * 100 / tot_cnt100,
         is_comp_perc_round =
           round(is_comp_perc, 1))

vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short_perc_sf <- 
  vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short_perc |>
  filter(!is.na(long) &
           !is.na(lat)) |>
  sf::st_as_sf(# Specify the field names to use as coordinates
    coords = c("long", "lat"),
    # Use the provided CRS (Coordinate Reference System), default to sa_shp's CRS
    crs = crs4326
    )

# glimpse(vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short_perc_sf)

vessels_permits_home_port_lat_longs_nc_comp_err_cnt_short_perc_sf |>
  filter(is_comp == 0) |> 
  mutate(my_label =
           str_glue("{city_fixed} {state_fixed}; # = {is_comp_perc_round}")) |>
  mapview::mapview(
    zcol = "my_label",
    cex = "is_comp_perc_round",
    alpha = 0.3,
    col.regions = viridisLite::turbo,
    legend = FALSE,
    layer.name = 'Vessel count by home port coordinates'
  ) +
  south_east_coast_states_shp


# old
# check counts ----
## count home ports by vessel ----
compl_err_db_data_metrics_permit_reg_list_home_port_cnt <- 
  compl_err_db_data_metrics_permit_reg_list_home_port |>
  map(\(curr_df) {
    curr_df |> 
      count(vessel_official_nbr, is_comp)
  })

glimpse(compl_err_db_data_metrics_permit_reg_list_home_port)

vessels_permits_home_port_lat_longs_nc_comp_err |> 
  count(SERO_OFFICIAL_NUMBER, permit_sa_gom) |> 
  glimpse()

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
#        city_fixed state_fixed    n
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
# [1] 1018    8

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
# 7   CAPE CANAVERAL          FL        14  CAPE CANAVERAL FL; # = 14
# 8  CAPE CANAVERAL           FL        14 CAPE CANAVERAL  FL; # = 14
# fixed