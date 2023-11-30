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

## check how many coords have more than one vessel ----
vessels_permits_home_port_lat_longs_city_state |>
  distinct() |>
#   group_by(permit_sa_gom, lat, long) %>%
# [1] 4393    6
  group_by(lat, long) %>%
  filter(n() > 1) |>
  dim()
# [1] num of SERO_OFFICIAL_NUMBER    6

# add counts to vessel_permit ----
# Adding a count column with num of SERO_OFFICIAL_NUMBER based on permit type, latitude, and longitude to the data frame.

vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port <-
  vessels_permits_home_port_lat_longs_city_state |>
  dplyr::add_count(permit_sa_gom,
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

# This code filters and modifies the data based on the permit type and compliance status for the year 2022. Here's the breakdown of the comments:
# 
# 1. Filtering and modifying data based on permit type and compliance status.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port` to the subsequent functions.
# 
# 3. Filtering rows where the permit type is "sa_only" using the `filter` function.
# 
# 4. Adding a new column named 'is_compliant_in_22' using the `mutate` function and conditional logic (`case_when`):
#    - If the vessel's official number is in a specific list (`compl_err_db_data_metrics_permit_reg_sa_only_vsl$vessel_official_nbr`), set compliance status to "NO".
#    - For other vessels, default compliance status to "YES".

vessels_permits_home_port_lat_longs_city_state_sa_compliance <-
  vessels_permits_home_port_lat_longs_city_state_cnt_vsl_by_port |>
  dplyr::filter(permit_sa_gom == "sa_only") |>
  dplyr::mutate(
    is_compliant_in_22 = 
      dplyr::case_when(
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
# Adding a count column named cnt_sa_vsl_by_port_coord_n_compl based on the variables lat, long, and is_compliant_in_22 using the dplyr::add_count function.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance |>
  dplyr::add_count(lat,
                   long,
                   is_compliant_in_22,
                   name = "cnt_sa_vsl_by_port_coord_n_compl")

## check if total vessel num is always grater than non compliant vessels ----
vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt |> 
  dplyr::filter(cnt_vsl_by_permit_n_port_coord < cnt_sa_vsl_by_port_coord_n_compl) |> 
  dim()
0
# correct

# Percent of (non)compliant by port ----
# Adding new columns to the data frame:
# 
# non_comp_perc: Non-compliance percentage calculated as the ratio of non-compliant vessels to total vessels.
# is_comp_perc_round: Rounded percentage of non-compliance. No digits after the decimal point.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt |>
  dplyr::group_by(is_compliant_in_22) |>
  dplyr::mutate(
    non_comp_perc =
      cnt_sa_vsl_by_port_coord_n_compl * 100 /
      cnt_vsl_by_permit_n_port_coord,
    is_comp_perc_round =
      round(non_comp_perc)
  ) |>
  dplyr::ungroup()

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

## check multiple names for the same cooordinates ---- 
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

## Preparing spatial data for map visualization ----
# Filtering rows with non-missing longitude and latitude values using the filter function.
# 
# Converting the data frame to a spatial object using the sf::st_as_sf function.
# 
# Specifying the field names ("long", "lat") to be used as coordinates.
# 
# Setting the Coordinate Reference System (CRS) for the spatial object to match the US states map using the crs parameter with the tigris_crs value.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc |>
  dplyr::filter(!is.na(long) &
           !is.na(lat)) |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = tigris_crs)

all_home_ports <-
  mapview(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf)

## crop all home ports by us state south map ----
# Creating a subset of data for the southern and eastern coast states
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

# The following code modifies the data frame for map visualization by filtering, selecting distinct rows, and creating new columns. Here's the breakdown of the comments:
# 
# 1. Modifying the data frame for map visualization.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south` to the subsequent functions.
# 
# 3. Filtering rows where the compliance status in 2022 is "NO" using the `filter` function.
# 
# 4. Keeping only distinct rows in the data frame using the `distinct` function.
# 
# 5. Adding a new column named 'my_label' using string interpolation with the `str_glue` function. This column combines the city, state, and non-compliance percentage for labeling on the map.
# 
# 6. Adding a new column named 'perc_nc_bin' using the `cut_number` function. This column represents the non-compliance percentage binned into 5 levels for color representation on the map.

vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south |>
  dplyr::filter(is_compliant_in_22 == "NO") |>
  dplyr::distinct() |> 
  dplyr::mutate(my_label =
           stringr::str_glue("{city_fixed} {state_fixed}; {is_comp_perc_round}% non-compl")) |> 
  dplyr::mutate(perc_nc_bin = 
                  ggplot2::cut_number(is_comp_perc_round, n = 5))

# View(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)

dim(vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab)
# [1] 832  12

# Calculating the number of unique colors based on a specific column
uniq_color_num <-
  function(zcol_name) {
    length(
      unique(
        vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab[[zcol_name]]
      )
    ) %>%
      return()
  }
# uniq_color_num("is_comp_perc_round")
# 43

# The following code creates pop-up marker information for the map by renaming columns and generating a pop-up table. Here's the breakdown of the comments:
# 
# 1. Creating pop-up marker information for the map using the `leafpop::popupTable` function.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab` to the subsequent functions.
# 
# 3. Renaming columns in the data frame for clarity in pop-up information. The column `cnt_vsl_by_permit_n_port_coord` is renamed to `total_vsls`, and `cnt_sa_vsl_by_port_coord_n_compl` is renamed to `non_compliant`.
# 
# 4. Generating a pop-up table using the specified columns (`total_vsls`, `non_compliant`, and `perc_nc_bin`) with the `leafpop::popupTable` function.
# 
# 5. Disabling the display of feature ID in the pop-up table by setting `feature.id` to FALSE.
# 
# 6. Disabling the display of row numbers in the pop-up table by setting `row.numbers` to FALSE.
# 
# 7. Specifying the columns to be displayed in the pop-up table using the `zcol` parameter.
markers_info <-
  vessels_permits_home_port_lat_longs_city_state_sa_compliance_cnt_perc_sf_south_lab |>
  dplyr::rename(total_vsls = cnt_vsl_by_permit_n_port_coord,
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
    # Specifying the column to be used for coloring the map
    zcol = "perc_nc_bin",
    # Scaling factor for the size of the markers
    cex = "cnt_vsl_by_permit_n_port_coord",
    # Setting the transparency level of the markers
    alpha = 0.3,
    # Specifying the color palette for the map
    # The direction parameter is set to -1 to reverse the order of colors.
    col.regions =
      viridisLite::mako(uniq_color_num("perc_nc_bin"), direction = -1),
    # legend = FALSE,
    layer.name = '% non compliant SA permitted vessels (2022) by home port coordinates',
    popup = markers_info
    # Enabling burst mode for each bin to be a layer
    # burst = TRUE 
  # )
) +
  south_east_coast_states_shp

