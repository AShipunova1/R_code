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
    # direction = -1: the order of colors is reversed
    col.regions =
      viridisLite::mako(uniq_color_num, direction = -1),
    legend = FALSE,
    layer.name = '% non compliant SA permitted vessels (2022) by home port coordinates',
    popup = markers_info
    # burst = TRUE
  )
# ) +
  # south_east_coast_states_shp

