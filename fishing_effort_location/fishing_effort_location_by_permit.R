# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

# Load the 'zoo' package for date manipulations
library(zoo)

# Load the 'sf' package to create sf (simple features) objects for working with spatial data
library(sf)

# Load the 'mapview' package to view spatial objects interactively
library(mapview)

# Load the 'leaflet' package for creating interactive web maps
library(leaflet)

# Load the 'tictoc' package for benchmarking and measuring code execution time
library(tictoc)

# Load the 'stringi' package for manipulating and working with character strings
library(stringi)

# Load the 'htmltools' package for working with HTML content in R
library(htmltools)

# Load the 'htmlwidgets' package, which is not present in the code but may be used to add JavaScript functionality to HTML widgets
# library(htmlwidgets)

# Source an external R script that contains useful functions.
source("~/R_code_github/useful_functions_module.r")

# Set 'my_paths' by calling the 'set_work_dir' function to define working directory paths.
my_paths <- set_work_dir()

# Define the name of the current project as "fishing_effort_location."
current_project_name <- "fishing_effort_location"

# Source another external R script using 'file.path' to construct the full file path.
get_data_path <-
  file.path(# Path to the git repository directory
    my_paths$git_r,
    # Subdirectory for the current project
    current_project_name,
    # Name of the R script to source
    "fishing_effort_locations_get_data.R")

source(get_data_path)

# Define an R function 'my_to_sf' for converting a data frame to an sf object.
my_to_sf <- function(my_df) {
  my_df %>%
    # Convert the data frame to an sf object using st_as_sf from the 'sf' package
    sf::st_as_sf(
      # Specify the columns containing longitude and latitude as coordinates
      coords = c("longitude", "latitude"),
      # Set the coordinate reference system (CRS) using the 'sa_shp' object
      crs = sf::st_crs(sa_shp),
      # Keep the original LATITUDE and LONGITUDE columns in the resulting sf object
      remove = FALSE
    ) %>%
    # Return the resulting sf object
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# Define an R function 'with_st_intersection' for calculating the intersection between two spatial objects.
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()  # Uncomment this line to enable debugging using 'browser()'

  # Get the parameter names for the input spatial objects
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # Start measuring the execution time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))

  # Calculate the intersection between 'points_sf' and 'polygons_sf'
  res <- sf::st_intersection(points_sf, polygons_sf)

  # Print the execution time
  toc()

  # Return the result of the intersection calculation
  return(res)
}

# run st_difference with benchmark
# Define an R function 'with_st_difference' for calculating the difference between two spatial objects.
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()  # Uncomment this line to enable debugging using 'browser()'

  # Get the parameter names for the input spatial objects
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # Start measuring the execution time with a message
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))

  # Calculate the difference between 'points_sf' and 'polygons_sf'
  res <- sf::st_difference(points_sf, polygons_sf)

  # Print the execution time
  toc()

  # Return the result of the difference calculation
  return(res)
}

## functions for ten_min ----

# Define an R function 'get_degree' to extract degrees from geographical coordinates.
get_degree <- function(gis_coord) {
  # Take the absolute value and round down to the nearest integer to get degrees.
  floor(abs(gis_coord))
}

# These R functions are used for converting between different formats of geographical coordinates.
# Define an R function 'get_minute' to extract minutes from geographical coordinates.
get_minute <- function(gis_coord) {
  # Take the absolute value, find the fractional part, and convert to minutes.
  dd <- abs(gis_coord) %% 1
  minutes <- floor(dd * 60)
  return(minutes)
}

# Define an R function 'convert_to_ten_min' to convert minutes to the nearest multiple of ten.
convert_to_ten_min <- function(minute) {
  # Divide minutes by 10, round down to the nearest multiple of 10.
  floor(minute/10) * 10
}

# Define an R function 'convert_to_decimal_degree' to convert degrees and minutes to decimal degrees.
convert_to_decimal_degree <- function(dm_num) {
  # Extract the degrees part (first two positions) and convert it to a numeric value.
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))

  # Extract the minutes part (characters in positions 3 and 4), divide by 60, and convert to a numeric value.
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60

  # Calculate the decimal degrees by adding degrees and minutes.
  degree + dd
}

# Define an R function 'get_lat_ten_min' for rounding latitude coordinates to the nearest ten minutes and converting to decimal degrees.
get_lat_ten_min <- function(gis_lat) {
  # Get the degrees component from latitude
  deg <- get_degree(gis_lat)

  # Get the minutes component from latitude
  minute <- get_minute(gis_lat)

  # Round minutes to the nearest ten minutes
  ten_min_num <- convert_to_ten_min(minute)

  # Create a string representing degrees and ten minutes (e.g., "DDMM")
  dm_num <- paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")

  # Convert the string representation to decimal degrees
  convert_to_decimal_degree(dm_num)
}

# All lon should be negative, bc we know it is all in America
# Define an R function 'get_ten_min_coords' for rounding coordinates to the nearest ten minutes.
get_ten_min_coords <- function(my_df) {
  # Create a new data frame 'ten_min_df' based on the input data frame 'my_df'
  ten_min_df <-
    my_df |>
    dplyr::mutate(
      # Create a new column 'ten_min_lat' by calling 'get_lat_ten_min' on the 'LATITUDE' column
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),

      # Create a new column 'ten_min_lon' by rounding and negating 'LONGITUDE' to ensure it's negative
      ten_min_lon = -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )

  # Return the resulting 'ten_min_df'
  return(ten_min_df)
}

## Data from db as in FHIER ----
# print_df_names(all_logbooks_db_data_2022_short_p_region)
# [1] 94471    73

# Create a new data frame by processing the data.

coord_data_2022_short_good_all_coords <-
  all_logbooks_db_data_2022_short_p_region |>

  # Convert 'longitude' and 'latitude' columns to numeric data types
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude)) |>

  # Ensure that all 'longitude' values are negative by taking the absolute value and negating them
  dplyr::mutate(longitude = -abs(longitude)) |>

  # Keep only distinct rows in the data frame
  distinct()

dim(coord_data_2022_short_good_all_coords)
# [1] 94471    73

## From FHIER ----
# View(coord_data_2022_short)
#
# coord_data_2022_short_good_all_coords_fhier <-
#   # Convert 'longitude' and 'latitude' columns to numeric data types
#   dplyr::mutate(longitude = as.numeric(longitude),
#                 latitude = as.numeric(latitude)) |>
#
#   # Ensure that all 'longitude' values are negative by taking the absolute value and negating them
#   dplyr::mutate(longitude = -abs(longitude)) |>
#
#   # Keep only distinct rows in the data frame
#   distinct()
#
# dim(coord_data_2022_short_good_all_coords)
# [1] 97970    17

# Keep only full sets of coordinates ----
# Create a new data frame 'coord_data_2022_short_good' by filtering and keeping only distinct rows.
coord_data_2022_short_good <-
  coord_data_2022_short_good_all_coords |>

  # Use the filter function to keep rows where either 'longitude' or 'latitude' is not NA
  dplyr::filter(!is.na(longitude) | !is.na(latitude)) |>

  # Keep only distinct rows in the data frame
  distinct()

dim(coord_data_2022_short_good_all_coords)
# [1] 97970    17 FHIER
# [1] 97943    16 db

dim(coord_data_2022_short_good)
# [1] 97547    17
# [1] 93450    73 db

### convert to sf ----
coord_data_2022_short_good_sf <-
  my_to_sf(coord_data_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

# Define a bounding box represented as a vector named 'big_bounding_box'.
big_bounding_box <- c(
   xmin = -97.79954,  # Minimum longitude (western boundary)
   ymin = 21.521757,  # Minimum latitude (southern boundary, Cuba)
   xmax = -64.790337,  # Maximum longitude (eastern boundary, Bermuda)
   ymax = 49  # Maximum latitude (northern boundary, Canada)
)

tic("coord_data_2022_short_good_sf_crop_big")
# Create a new spatial object 'coord_data_2022_short_good_sf_crop_big'
# by cropping 'coord_data_2022_short_good_sf' to the 'big_bounding_box'.

coord_data_2022_short_good_sf_crop_big <-
  sf::st_crop(coord_data_2022_short_good_sf,  # Spatial object to be cropped
              big_bounding_box)  # Bounding box used for cropping
toc()
# coord_data_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(coord_data_2022_short_good_sf_crop_big)
# [1] 95720    18
# [1] 91735    74

# convert back to df ----
# The sf::st_drop_geometry function is applied to the spatial object to remove its geometry, resulting in a data frame that contains only the non-geometric attributes or columns.

coord_data_2022_short_good_sf_crop_big_df <-
  coord_data_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(coord_data_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

## remove ids not in fhier_reports_metrics_tracking_not_srhs_ids ----
# Create a new data frame 'coord_data_2022_short_good_sf_crop_big_df_in_metricks'
# by filtering 'coord_data_2022_short_good_sf_crop_big_df' based on a condition.

coord_data_2022_short_good_sf_crop_big_df_in_metricks <-
  coord_data_2022_short_good_sf_crop_big_df |>

  # Use the 'filter' function to retain rows meeting a specific condition.
  dplyr::filter(
    # Keep rows where the 'vessel_official_nbr' values are present in the fhier_reports_metrics_tracking_not_srhs_ids
    vessel_official_nbr %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(coord_data_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17
# [1] 90838    73 from mv

