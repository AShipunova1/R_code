library(tidyverse)
library(ROracle)
library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively

#### Current file: ~/R_code_github/fishing_effort_location/read.me.R ----

# https://github.com/AShipunova1/R_code/tree/main/fishing_effort_location

# All of 2022 would work for us
# Trips filtered to charter trips only.
# fishing charter trips
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  all
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name
# ---

# I was wondering if it would be possible to get access to the for hire reports.  We are working on a management strategy evaluation and need information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
#
# I got approval to send you the data, per your request. To ensure I get what you need, can you please respond to the following:
#
# You are looking for just depth, area fished (lats/longs?) and dates of those trips (start and end fields), correct?
# The lawyer deemed these non-confidential data fields, but just making sure you weren't expecting anything else.  No problem.  I do have access to confidential data through the SEFSC and ACCSP, and adrain has been approved by the SEFSC as well.  Are there additional steps I need to take for the future to be able to access landings data?  I don't think so, but we can cross that bridge when needed.
# Do you want just for-hire intended fishing trips that have recorded an "area fished" on their trip report that falls within SA waters?
# Are there any lat/long or state limits to apply?
# I think filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.  ok
#
# The other possibilities are instead sending any for-hire trip taken by a SA federal for-hire permitted vessel (either for just SA or dual Gulf/SA permitted vessels), and/or filtering by start and/or end port of the trip.
# Is there a date range?
# I would not suggest using 2021 data, given the program was so new. We tend to run any analyses from Jan 1, 2022+
# All of 2022 would work for us ok
#
# What is the deadline that you would need this data request fulfilled by?
# June 1 ok
#
# We collect 3 depth fields in the SEFHIER logbook: min and max depth, and primary fishing depth.
# Do you want all 3 fields, or one in particular?
# Primary depth is fine.  However if the other fields are more commonly filled out than primary depth, please provide those fields instead.  ok
#
# We also have lat/long data fields, collected in the logbook. A summary of logbook fields of potential interest are here:
# Trip type (commercial, charter, recreational, headboat) - these would be for-hire permitted vessels, telling us which trip type they intend to make:  Trips filtered to charter trips only.  We have access to commercial and headboat  ok (here headboat is a for-hire vessel operating as a headboat, but that is not in the SRHS program). So, I will include charter and headboat (which excludes SRHS data)
# Trip start and trip end date and time fields  :  Trip start and end date  ok
# Start Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  all  ok
# End Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  code only  ok
# Trip Activity Type: options are trips with effort; intended fishing but unable to fish (e.g. weather); trip with no intention of fishing (e.g. chartered sunset or dolphin cruise): fishing charter trips  ok
# Longitude and Latitude fields:  Lat and long as specific as possible  ok
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name (these are ACCSP fields, so I or you would need to get the code definitions) all.  I have access in ACCSP  ok
#
#
# **Some data caveats to be aware of:
#
# this is preliminary SEFHIER reported data, and the SEFHIER data is not yet calibrated or certified.  This information will only be used to consider relative effort.  Are there issues with potential bias by area?  This was more of a note, to just say please be careful when using it for relative effort as this may not be representative.  We have not looked into what the biases are, but based alone on the lack of reporting it just may not be representative
# to-date; we still have only about 60% compliance month to month in the SA, for vessels reporting as required. Therefore, interpretation of the data should take that into consideration
# SRHS data are not included in the SEFHIER data (I do not have access to that program's data).  I can access this data   ok
# SEFHIER logbooks don’t collect depth at the set level. So, a species (or complex level) analysis probably wouldn’t be very useful given how the fishery operates. We understand.  It is a hard one to define especially with some of the captains drifting instead of anchoring.  Average for the trip is fine.      ok
# We collect “average” gear depth (essentially fishing depth for the whole trip, where a trip could be across many depths - depending how and where they moved for that trip, and they could have targeted many species throughout the trip- depending on weather and what was biting).
# We also collect min and max depth in the logbooks, but again those are trip level fields
#
#

# setup ----
set_work_dir <- function() {
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

my_paths <- set_work_dir()

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>%
    as.data.frame()
}

data_overview <- function(my_df) {
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  count_uniq_by_column(my_df)
}


#### Current file: ~/R_code_github/fishing_effort_location/fishing_effort_locations_get_data.R ----

con = ROracle::dbConnect(
  DBI::dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR",
                              keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====
# fishing charter trips only
# 2022
# sero_vessel_permit

request_query <- "SELECT
  trip_start_date,
  trip_end_date,
  start_port,
  start_port_name,
  start_port_county,
  start_port_state,
  end_port,
  vendor_app_name,
  area_code,
  sub_area_code,
  distance_code_name,
  local_area_code,
  latitude,
  longitude,
  minimum_bottom_depth,
  maximum_bottom_depth,
  fishing_gear_depth,
  depth
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
    trip_de >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_END_DATE <= TO_DATE('31-DEC-22', 'dd-mon-yy')
  AND trip_type_name = 'CHARTER'
  AND sero_vessel_permit IS NOT NULL"

db_data = ROracle::dbGetQuery(con,
                     request_query)


# get area data ----
area_data_query <-
  "select * from SAFIS.AREAS_FISHED@secapxdv_dblk.sfsc.noaa.gov
  where state in ('FL', 'US')
"

db_area_data = ROracle::dbGetQuery(con,
                     area_data_query)

ROracle::dbDisconnect(con)

## get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- sf::read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

# see the function above, F2 in RStudio will show the function definition, when the cursor is on the name.
sa_shp <- read_shapefile(r"(shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

# see the function above
gom_reef_shp <- read_shapefile(r"(gom\ReefFish_EFH_GOM\ReefFish_EFH_GOM.shp)")

### fl_state_w_counties ----
# see the function above
fl_state_w_counties_shp <- read_shapefile(r"(GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

#### Current file: ~/R_code_github/fishing_effort_location/fishing_effort_location.R ----

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
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth

# OK boundaries ----
# lat 23 : 28
# lon -71 : -83

clean_lat_long <- function(my_lat_long_df, my_limit = 1000) {

  my_lat_long_df %>%
    unique() %>%
    # we can limit the amount of points to show on the map
    head(my_limit) %>%
    # all LONG should be negative
    dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove coords outside off requested borders
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71)) %>%
    return()
}

# combine with additional area data ----
db_data_w_area <- dplyr::full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`

all_points <- dim(db_data_w_area)[1]
# 254689

# View(db_data_w_area)

# write_csv(
#   db_data_w_area,
#   file.path(
#     my_paths$outputs,
#     "fishing_effort_location",
#     "db_data_w_area.csv"
#   )
# )

to_report <-
  db_data_w_area %>%
  dplyr::select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    START_PORT,
    START_PORT_NAME,
    START_PORT_COUNTY,
    START_PORT_STATE,
    END_PORT,
    LATITUDE,
    LONGITUDE,
    MINIMUM_BOTTOM_DEPTH,
    MAXIMUM_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH,
    DEPTH,
    REGION,
    AREA_NAME
  ) %>%
  unique()

dim(to_report)
# 75549

#### Current file: ~/R_code_github/fishing_effort_location/fishing_effort_location_viz.R ----

# shape files maps ----
m_s <- mapview(
  sa_shp,
  layer.name = "South Altlantic",
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  legend = FALSE
)

m_fl_state_w_counties <- mapview(
  fl_state_w_counties,
  layer.name = "FL counties and state waters",
  # col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  legend = FALSE
)

# OK boundaries ----
# lat 23 : 28
# lon -71 : -83

to_sf <- function(my_df) {
  my_df %>%
    sf::st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = sf::st_crs(sa_shp)) %>%
    return()
}

## no gom by month ----

lat_long_month_depth_report <-
  to_report %>%
  # exclude GOM
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # labels are a month only
  dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  # get avg bottom depth for labels
  dplyr::mutate(AVG_DEPTH = mean(
    c(
      MINIMUM_BOTTOM_DEPTH,
      MAXIMUM_BOTTOM_DEPTH,
      FISHING_GEAR_DEPTH
    ),
    na.rm = TRUE
  )) %>%
  # return to the default colwise operations
  dplyr::ungroup() %>%
  # combine a label
  dplyr::mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      paste0("avg_depth: ", AVG_DEPTH),
      paste0("area_name: ", AREA_NAME),
      START_PORT_NAME,
      sep = ", "
    ))

all_points <- dim(lat_long_month_depth_report)[1]
# 75598

# see the function above, F2 in RStudio will show the function definition.
lat_long_month_depth_clean <-
  clean_lat_long(lat_long_month_depth_report, all_points)
dim(lat_long_month_depth_clean)[1]
# 28032

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# remove points which belong to gom_reef_shp
# that works for several minutes before producing a result
lat_long_month_depth_minus_gom_sf <-
  sf::st_difference(to_sf(lat_long_month_depth_clean), gom_reef_shp)
# str(gom_reef_shp)
# to avoid errors when print into file
mapview::mapviewOptions(fgb = FALSE)

lat_long_month_no_gom_map <-
  lat_long_month_depth_minus_gom_sf %>%
  mapview(
    # colors
    zcol = "TRIP_START_M",
    # color palette
    col.regions = viridisLite::turbo,
    layer.name = 'Month',
    # size
    cex = "FISHING_GEAR_DEPTH",
    # transparency
    alpha = 0.3,
    legend = TRUE
  )

# combine mapviews
res_map <- m_s + m_fl_state_w_counties + lat_long_month_no_gom_map

res_map

# safe as a png
# png_fl <- "res_map.png"
# mapview::mapshot(res_map, file = png_fl)
# # open the file
# utils::browseURL(png_fl)
