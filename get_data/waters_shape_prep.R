# setup ----
# maps:
library(mapview)
library(sf)
## Load the 'tigris' package to access geographic data.
library(tigris)

## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
waters_project_dir_name <- this.path::this.dir()

waters_project_basename <-
  basename(waters_project_dir_name)

waters_output_path <- file.path(my_paths$outputs,
                         waters_project_basename)

my_crs = 4326
## state and county lists ----
misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
source(misc_info_path)

# shape files ----

## GOM state and fed ----
GOM_400fm_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOM_400fm\GOM_400fm.shp)")

# file.exists(GOM_400fm_path)
# T

GOMsf <-
  sf::read_sf(GOM_400fm_path)
# mapview(GOMsf)

# str(GOMsf)

## SA federal waters ----
sa_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# file.exists(sa_path)

sa_shp <- sf::read_sf(sa_path)

# mapview(sa_shp)

## world waters ----
world_state_and_fed_waters_path <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

# file.exists(state_and_fed_waters_path)

world_state_and_fed_waters_shp <-
  sf::read_sf(world_state_and_fed_waters_path)

# mapview(state_and_fed_waters_shp)

## SA state waters ----

east_coast_sa_state_waters_shp <-
  world_state_and_fed_waters_shp |>
  filter(Jurisdicti %in% east_coast_states$sa)

# mapview(east_coast_sa_state_waters_shp)

## Florida state waters ----
# bc FL is in both regions

### fl_state_w_counties ----
fl_state_w_counties_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

# file.exists(fl_state_w_counties_path)

fl_state_w_counties_shp <-
  sf::read_sf(fl_state_w_counties_path)

# mapview(fl_state_w_counties_shp)

### fl sa ----
sa_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$sa)

# mapview(sa_fl_state_w_counties_shp)

### fl gom ----

gom_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$gom)

## GOM South Florida state_waters_only ----
GOM_s_fl_state_waters_only <-
  GOMsf |>
  filter(Jurisdict == "State" &
           StatZone %in% c(1, 2, 3)) |>
  select(-c(DepZone,
            Activity,
            Shape_Area)) |>
  distinct()


# mapview(fl_state_w_counties_shp,
#         col.regions = "green") +
#   mapview(GOM_s_fl_state_waters_only)

# When getting points in SA Monroe
# Have to do it by steps, otherwise it takes too long
# 1) get all points not in GOM_s_fl_state_waters_only

## all US states ----
## The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_state_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

sa_states_shp <-
  us_state_shp |>
  filter(NAME %in% south_atlantic_states)

gom_states_shp <-
  us_state_shp |>
  filter(NAME %in% east_coast_states$gom)

## Convert to common crs ----
st_crs(east_coast_sa_state_waters_shp)
    # ID["EPSG",3395]]

st_crs(gom_fl_state_w_counties_shp)
    # ID["EPSG",4269]]

st_crs(GOMsf)
    # ID["EPSG",4326]]

st_crs(sa_shp)
    # ID["EPSG",4269]]

st_crs(us_state_shp)
    # ID["EPSG",4269]]

my_dfs_to_transform_names <-
  list("east_coast_sa_state_waters_shp",
       "gom_fl_state_w_counties_shp",
       "sa_fl_state_w_counties_shp",
       "sa_shp",
       "gom_states_shp",
       "us_state_shp"
       )

shp_4326_list <-
  my_dfs_to_transform_names |>
  map(\(my_df_name) {
    browser()
    my_df_4326 <-
      !!sym(my_df_name) |>
      st_transform(my_crs)
    return()
  })

east_coast_sa_state_waters_shp_4326 <-
  st_transform(east_coast_sa_state_waters_shp, my_crs)

gom_fl_state_w_counties_shp_4326 <-
  st_transform(gom_fl_state_w_counties_shp, my_crs)

sa_fl_state_w_counties_shp_4326 <-
  st_transform(sa_fl_state_w_counties_shp, my_crs)

sa_shp_4326 <-
  st_transform(sa_shp, my_crs)

sa_states_shp_4326 <-
  us_state_shp |>
  filter(NAME %in% south_atlantic_states)

gom_states_shp <-
  us_state_shp |>
  filter(NAME %in% east_coast_states$gom)

# misc ----
# all waters
# install.packages("ggOceanMaps")
# library(ggOceanMaps)
#
# shapefile_list(name = "all")
#
# basemap(data = lat_lon_gom_state_cnt_sf, bathymetry = TRUE)

# (dd_rbathy)

# Results ----
result_names <- c("GOMsf",
             "sa_shp_4326",
             "world_state_and_fed_waters_path",
             "east_coast_sa_state_waters_shp_4326",
             "fl_state_w_counties_shp",
             "sa_fl_state_w_counties_shp_4326",
             "gom_fl_state_w_counties_shp_4326",
             "GOM_s_fl_state_waters_only",
             "sa_states_shp",
             "gom_states_shp")
title_message_print(result_names)

