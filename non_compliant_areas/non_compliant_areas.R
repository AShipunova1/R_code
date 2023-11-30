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
library(leafpop)

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

# prepare data ----
get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_get_data.R")

source(get_data_file_path)
# all_get_db_data_result_l
# vessels_permits_home_port_lat_longs_city_state

# vessels_permits_home_port_lat_longs_city_state |> dim()
# [1] 4729    6

## check for duplicate vessels ----
vessels_permits_home_port_lat_longs_city_state |>
  distinct() |>
  group_by(SERO_OFFICIAL_NUMBER) %>%
  filter(n() > 1) |>
  dim()
# 0

vessels_permits_home_port_lat_longs_city_state |>
  distinct() |>
#   group_by(permit_sa_gom, lat, long) %>%
# [1] 4393    6
  group_by(lat, long) %>%
  filter(n() > 1) |>
  dim()
# [1] 4505    6

# add counts to vessel_permit ----
# [1] "SERO_OFFICIAL_NUMBER, permit_sa_gom, city_fixed, state_fixed, lat, long"

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port <-
  vessels_permits_home_port_lat_longs_city_state |>
  add_count(permit_sa_gom, 
            lat, 
            long, 
            name = "cnt_vsl_by_permit_n_port_coord")

### check counts ----
vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         lat,
         long) |>
  count(permit_sa_gom,
        lat,
        long) |>
  filter(permit_sa_gom == "sa_only") |>
  arrange(desc(n)) |>
  head()

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  filter(permit_sa_gom == "sa_only") |>
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  arrange(desc(cnt_vsl_by_permit_n_port_coord)) |>
  head()

# Join home port and compliance info by vessel ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance <-
  vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  filter(permit_sa_gom == "sa_only") |> 
  mutate(
    is_compliant_in_22 = case_when(
      SERO_OFFICIAL_NUMBER %in% compl_err_db_data_metrics_permit_reg_sa_only_vsl$vessel_official_nbr ~ "NO",
      .default = "YES"
    )
  )

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance)
# [1] 25587    33
# [1] 4741    7 distinct
# [1] 4729    7 no comp only
# [1] 3388    8 sa only


# data_overview(vessels_permits_home_port_lat_longs_city_state_sa_compliance)
# all permit regions
# SERO_OFFICIAL_NUMBER 4729
# lat                   547
# is_compliant_in_22                2

# sa_only
# SERO_OFFICIAL_NUMBER           3388
# lat                             450
# is_compliant_in_22                2

# count vessels by home_port and compliance ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt <- 
  vessels_permits_home_port_lat_longs_city_state_sa_compliance |> 
  add_count(lat, 
            long, 
            is_compliant_in_22,
            name = "cnt_sa_vsl_by_port_coord_n_compl")

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt |> 
  filter(cnt_vsl_by_permit_n_port_coord < cnt_sa_vsl_by_port_coord_n_compl) |> 
  dim()
0
# correct

# Percent of (non)compliant by port ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc <- 
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt |>
  group_by(is_compliant_in_22) |>
  mutate(
    non_comp_perc =
      cnt_sa_vsl_by_port_coord_n_compl * 100 /
      cnt_vsl_by_permit_n_port_coord,
    is_comp_perc_round =
      round(non_comp_perc)
  ) |>
  ungroup()

glimpse(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc)
# [1] 3388   11

# spot test counts ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |> 
  filter(city_fixed == "SEBASTIAN") |> 
  data_overview()
# SERO_OFFICIAL_NUMBER 27
# is_comp               2

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |> 
  filter(city_fixed == "SEBASTIAN") |> 
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  glimpse()
# $ is_compliant_in_22               <chr> "NO", "YES"
# $ cnt_sa_vsl_by_port_coord_n_compl <int> 15, 12
# $ is_comp_perc_round               <dbl> 55.6, 44.4

# mult_names <- "AMELIA"
# BAYOU LA BATRE
mult_names <- "BATRE"
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  filter(grepl(mult_names, city_fixed)) |>
  select(-SERO_OFFICIAL_NUMBER) |> 
  distinct() |>
  glimpse()
# $ SERO_OFFICIAL_NUMBER             <chr> "938369", "FL3307AE"
# $ city_fixed                       <chr> "AMELIA IS", "AMELIA ISLAND"
# $ cnt_vsl_by_permit_n_port_coord   <int> 2, 2


# $ city_fixed                       <chr> "BAYOU LA BATRE", "BAYOU  LA BATRE"…
# $ state_fixed                      <chr> "AL", "AL", "LA"
# $ cnt_vsl_by_permit_n_port_coord   <int> 103, 103, 103


# convert to sf ----
## how many don't have coords ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  filter(is.na(long) &
           is.na(lat)) |>
  dim()
# 11

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  filter(!is.na(long) &
           !is.na(lat)) |>
  sf::st_as_sf(# Specify the field names to use as coordinates
    coords = c("long", "lat"),
    # Use the same CRS (Coordinate Reference System) as the us states map
    crs = tigris_crs)

all_home_ports <-
  mapview(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf)

## crop all home ports by us state south map ----
# mapview(south_east_coast_states_shp)

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf |>
  sf::st_crop(south_east_coast_states_shp_bb)  # Bounding box used for cropping
# although coordinates are longitude/latitude, st_intersection assumes that they
# are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south)
# [1] 3106   10

# Prepare for mapping: non compl only, add labels ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south |>
  filter(is_compliant_in_22 == "NO") |>
  distinct() |> 
  mutate(my_label =
           str_glue("{city_fixed} {state_fixed}; {is_comp_perc_round}% non-compl")) |> 
  mutate(perc_nc_bin = cut_number(is_comp_perc_round, n = 5))

# View(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)
# [1] 832  12

uniq_color_num <-
  length(
    unique(
      vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab$is_comp_perc_round
    )
  )
# uniq_color_num
# 53

# popupTable(x, zcol, row.numbers = TRUE, feature.id = TRUE, className = NULL)

markers_info <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  rename(total_vsls = cnt_vsl_by_permit_n_port_coord,
         non_compliant = cnt_sa_vsl_by_port_coord_n_compl,) |>
  leafpop::popupTable(
    feature.id = FALSE,
    row.numbers = FALSE,
    zcol = c(
      "total_vsls",
      "non_compliant",
      "perc_nc_bin"
    )
  )

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  mapview::mapview(
    label = "my_label",
    # zcol = "is_comp_perc_round",
    zcol = "perc_nc_bin",
    cex = "cnt_vsl_by_permit_n_port_coord",
    alpha = 0.3,
    col.regions =
      viridisLite::mako(uniq_color_num, direction = -1),
    legend = FALSE,
    layer.name = '% non compliant SA permitted vessels (2022) by home port coordinates',
    popup = markers_info
    # burst = TRUE
  )
# ) +
  # south_east_coast_states_shp

print_df_names(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)
# old ----


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
