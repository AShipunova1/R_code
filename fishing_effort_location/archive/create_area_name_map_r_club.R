# setup ----
library(tidyverse)
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively

# set working directories
  # change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()

  # for others
  add_dir <- ""
  # for Anna's computer
  if (get_username() == "anna.shipunova") {
    add_dir <- "R_files_local/test_dir"
  }

  # add an empty or Anna's folder in front
  main_r_dir <- file.path(add_dir, "SEFHIER/R code")

  in_dir <- "Inputs"
  # file.path instead of paste, because it provides correct concatenation, "\" or "/" etc.
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "Outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir)
  return(my_paths)
}

set_work_dir_local <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"

  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  git_r_dir <- "R_code_github"
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

if (get_username() == "anna.shipunova") {
  set_work_dir <- set_work_dir_local
}

my_paths <- set_work_dir()

current_project_name <- "fishing_effort_location"

read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- sf::read_sf(shapefile_file_name)
  return(x)
}

# see the function above, F2 in RStudio will show the function definition, when the cursor is on the name.
sa_shp <-
  read_shapefile(r"(shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# see the function above
gom_reef_shp <-
  read_shapefile(r"(gom\ReefFish_EFH_GOM\ReefFish_EFH_GOM.shp)")

clean_lat_long <- function(my_lat_long_df, my_limit = 1000) {
  my_lat_long_df %>%
    unique() %>%
    # we can limit the amount of points to show on the map
    head(my_limit) %>%
    # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove coords outside off requested borders
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71)) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# shape files ----
m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

m_g_r <- mapview(
  gom_reef_shp,
  col.regions = "lightblue",
  alpha.regions = 0.2,
  layer.name = "GOM Reef Fish EFH",
  legend = FALSE
)

# get data ----
db_data_w_area_file_path <-
  file.path(my_paths$inputs,
            "fishing_effort_locations/db_data_w_area.csv")
db_data_w_area <- readr::read_csv(db_data_w_area_file_path)

lat_long_area <- db_data_w_area %>%
  dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  unique()


# clean coordinates and cut by coordinate boundaries ----
how_many_points_to_draw <- 10000

lat_long_area_clean <- clean_lat_long(lat_long_area, how_many_points_to_draw)

# exclude GOM "region" ----
lat_long_area_clean_no_gom <-
  lat_long_area_clean %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

# create sf ----
lat_long_area_clean_sf <-
  lat_long_area_clean_no_gom %>%
  mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_CODE,
      DISTANCE_CODE_NAME,
      REGION,
      sep = ", "
    )
  ) %>%
  my_to_sf()

## a mapview with default options ----
mapview(lat_long_area_clean_sf)

## a mapview with custom options ----
lat_long_area_clean_map <-
  lat_long_area_clean_sf %>%
  mapview(
    # colors according a chosen column
    zcol = "AREA_NAME",
    # palette to choose colors from
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # transparency
    alpha = 0.3,
    legend = FALSE
  )

## show the maps ----
m_s + m_g_r + lat_long_area_clean_map

# remove points falling into GOM reef polygon ----
lat_long_area_clean_sf_no_gom_reef_sf <-
  sf::st_difference(lat_long_area_clean_sf, gom_reef_shp)

## a mapview with custom options ----
lat_long_area_clean_sf_no_gom_reef_sf_map <-
  lat_long_area_clean_sf_no_gom_reef_sf %>%
  mapview(
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    alpha = 0.3,
    legend = FALSE
  )

## show the maps ----
m_s + m_g_r + lat_long_area_clean_sf_no_gom_reef_sf_map
