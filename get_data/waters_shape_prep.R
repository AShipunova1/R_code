# setup ----
# maps:
library(mapview)
library(sf)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
waters_project_dir_name <- this.path::this.dir()

waters_project_basename <-
  basename(waters_project_dir_name)

output_path <- file.path(my_paths$outputs,
                         waters_project_basename)

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

# SA fed ---
sa_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# file.exists(sa_path)

sa_shp <- sf::read_sf(sa_path)

# mapview(sa_shp)

## fl_state_w_counties ----
fl_state_w_counties_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

# file.exists(fl_state_w_counties_path)

fl_state_w_counties_shp <- sf::read_sf(fl_state_w_counties_path)

# mapview(fl_state_w_counties_shp)

## fl sa ----
sa_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$sa)

## fl gom ----

gom_fl_state_w_counties_shp <-
  fl_state_w_counties_shp |>
  filter(county_nam %in% fl_counties$gom)

mapview(gom_fl_state_w_counties_shp)

## SA state waters ----
state_and_fed_waters_path <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

# file.exists(state_and_fed_waters_path)

state_and_fed_waters_shp <-
  sf::read_sf(state_and_fed_waters_path)

str(state_and_fed_waters_shp)

sa_state_waters_shp <-
  state_and_fed_waters_shp |>
  filter(Jurisdicti %in% east_coast_states$sa)

# mapview(sa_state_waters_shp)

# sa_fl_state_waters ----
sa_state_waters_shp

sa_fl_state_waters_shp <-
  st_intersection(sa_fl_state_w_counties_shp,
                  sa_state_waters_shp)


# misc ----
# all waters
# install.packages("ggOceanMaps")
# library(ggOceanMaps)
#
# shapefile_list(name = "all")
#
# basemap(data = lat_lon_gom_state_cnt_sf, bathymetry = TRUE)

# (dd_rbathy)
