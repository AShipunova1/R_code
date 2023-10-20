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
source(
  file.path(
    my_paths$git_r,                    # Path to the git repository directory
    current_project_name,              # Subdirectory for the current project
    "fishing_effort_locations_get_data.R"  # Name of the R script to source
  )
)

# Define an R function 'my_to_sf' for converting a data frame to an sf object.
my_to_sf <- function(my_df) {
  my_df %>%
    # Convert the data frame to an sf object using st_as_sf from the 'sf' package
    sf::st_as_sf(
      # Specify the columns containing longitude and latitude as coordinates
      coords = c("LONGITUDE", "LATITUDE"),
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
  minute <- floor(dd * 60)
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

# Create a new data frame 'safis_efforts_extended_2022_short_good_all_coords' by processing the data.

safis_efforts_extended_2022_short_good_all_coords <-
  all_logbooks_db_data_2022_short_p_region |>

  # Convert 'LONGITUDE' and 'LATITUDE' columns to numeric data types
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>

  # Ensure that all 'LONGITUDE' values are negative by taking the absolute value and negating them
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>

  # Keep only distinct rows in the data frame
  distinct()

dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 94471    73

## From FHIER ----
# View(safis_efforts_extended_2022_short)
#
# safis_efforts_extended_2022_short_good_all_coords_fhier <-
#   # Convert 'LONGITUDE' and 'LATITUDE' columns to numeric data types
#   dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
#                 LATITUDE = as.numeric(LATITUDE)) |>
#
#   # Ensure that all 'LONGITUDE' values are negative by taking the absolute value and negating them
#   dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
#
#   # Keep only distinct rows in the data frame
#   distinct()
#
# dim(safis_efforts_extended_2022_short_good_all_coords)
# [1] 97970    17

# Keep only full sets of coordinates ----
# Create a new data frame 'safis_efforts_extended_2022_short_good' by filtering and keeping only distinct rows.
safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short_good_all_coords |>

  # Use the filter function to keep rows where either 'LONGITUDE' or 'LATITUDE' is not NA
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>

  # Keep only distinct rows in the data frame
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17 FHIER
# [1] 97943    16 db

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17
# [1] 93450    73 db

### convert to sf ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

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

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
# Create a new spatial object 'safis_efforts_extended_2022_short_good_sf_crop_big'
# by cropping 'safis_efforts_extended_2022_short_good_sf' to the 'big_bounding_box'.

safis_efforts_extended_2022_short_good_sf_crop_big <-
  sf::st_crop(safis_efforts_extended_2022_short_good_sf,  # Spatial object to be cropped
              big_bounding_box)  # Bounding box used for cropping
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18
# [1] 91735    74

# convert back to df ----
# The sf::st_drop_geometry function is applied to the spatial object to remove its geometry, resulting in a data frame that contains only the non-geometric attributes or columns.

safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

## remove ids not in fhier_reports_metrics_tracking_not_srhs_ids ----
# Create a new data frame 'safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks'
# by filtering 'safis_efforts_extended_2022_short_good_sf_crop_big_df' based on a condition.

safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>

  # Use the 'filter' function to retain rows meeting a specific condition.
  dplyr::filter(
    # Keep rows where the 'VESSEL_OFFICIAL_NBR' values are present in the fhier_reports_metrics_tracking_not_srhs_ids
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17
# [1] 90838    73


### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])


