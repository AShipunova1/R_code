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

# mapview(gom_fl_state_w_counties_shp)

## SA state waters ----
state_and_fed_waters_path <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

# file.exists(state_and_fed_waters_path)

state_and_fed_waters_shp <-
  sf::read_sf(state_and_fed_waters_path)

# str(state_and_fed_waters_shp)

sa_state_waters_shp <-
  state_and_fed_waters_shp |>
  filter(Jurisdicti %in% east_coast_states$sa)

# mapview(sa_state_waters_shp)

# Convert to common crs ----
st_crs(sa_state_waters_shp)
    # ID["EPSG",3395]]

st_crs(gom_fl_state_w_counties_shp)
    # ID["EPSG",4269]]

st_crs(GOMsf)
    # ID["EPSG",4326]]

st_crs(sa_shp)
    # ID["EPSG",4269]]

sa_state_waters_shp_4326 <-
  st_transform(sa_state_waters_shp, my_crs)

gom_fl_state_w_counties_shp_4326 <-
  st_transform(gom_fl_state_w_counties_shp, my_crs)

sa_fl_state_w_counties_shp_4326 <-
  st_transform(sa_fl_state_w_counties_shp, my_crs)

sa_shp_4326 <-
  st_transform(sa_shp, my_crs)

# Get fl sa only state waters ----
# tic("fl_sa_only state waters")
# sa_only_fl_state_waters_shp <-
#   st_intersection(sa_state_waters_shp_4326,
#                   sa_fl_state_w_counties_shp_4326)
# toc()
# fl_sa_only state waters: 0.72 sec elapsed

get_sa_only_fl_state_waters_shp <-
  function(sa_state_waters_shp_4326,
                  sa_fl_state_w_counties_shp_4326) {
    sa_only_fl_state_waters_shp <-
      st_intersection(sa_state_waters_shp_4326,
                  sa_fl_state_w_counties_shp_4326)
    return(sa_only_fl_state_waters_shp)
  }
# sa_lat_lon_gom_state_cnt_sf_fed_w: 84.14 sec elapsed

sa_only_fl_state_waters_shp_path <-
  file.path(waters_output_path,
            "sa_only_fl_state_waters_shp.rds")

# readr::write_rds(sa_lat_lon_gom_state_cnt_sf_fed_w,
#                  sa_lat_lon_gom_state_cnt_sf_fed_w_file_path)

sa_only_fl_state_waters_shp <-
  read_rds_or_run_no_db(
  sa_only_fl_state_waters_shp_path,
  list(sa_state_waters_shp_4326,
                  sa_fl_state_w_counties_shp_4326),
  get_sa_only_fl_state_waters_shp

)

# mapview(sa_only_fl_state_waters_shp)

# misc ----
# all waters
# install.packages("ggOceanMaps")
# library(ggOceanMaps)
#
# shapefile_list(name = "all")
#
# basemap(data = lat_lon_gom_state_cnt_sf, bathymetry = TRUE)

# (dd_rbathy)
