# Version May 30 2023 ASh

# setup ----

library(tidyverse)
library(magrittr)
library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(tictoc) #benchmarking

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

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

if (get_username() == "anna.shipunova") {
  set_work_dir <- set_work_dir_local
}

my_paths <- set_work_dir()

current_project_name <- "fishing_effort_location"

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

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  # find an intersection of points (coordinates) and polygons (shapefiles)
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  # find points (coordinates) not within polygons (shapefiles)
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# all_points <- dim(db_data_w_area)[1]

# required fields/columns
fields_list <-
  c("TRIP_START_DATE",
"TRIP_END_DATE",
"START_PORT",
"START_PORT_NAME",
"START_PORT_COUNTY",
"START_PORT_STATE",
"END_PORT",
"END_PORT_NAME",
"END_PORT_COUNTY",
"END_PORT_STATE",
"ACTIVITY_TYPE_NAME",
"TRIP_TYPE_NAME",
"AREA_CODE",
"SUB_AREA_CODE",
"DISTANCE_CODE_NAME",
"LATITUDE",
"LONGITUDE",
"FISHING_GEAR_DEPTH")

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
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

#### Current file: ~/R_code_github/fishing_effort_location/fishing_effort_locations_get_data.R ----

# get area data ----
## From DB ====
data_from_db <- function() {
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

## From DB ====
  # fishing charter trips only
  # 2022
  # sero_vessel_permit

  request_query <- "SELECT distinct
    trip_start_date,
    trip_end_date,
    start_port,
    start_port_name,
    start_port_county,
    start_port_state,
    end_port,
    end_port_name,
    end_port_county,
    end_port_state,
    activity_type_name,
    trip_type_name,
    area_code,
    sub_area_code,
    distance_code_name,
    latitude,
    longitude,
    fishing_gear_depth

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

  # data_overview(db_data)

# get area data ----
  area_data_query <-
    "select * from SAFIS.AREAS_FISHED@secapxdv_dblk.sfsc.noaa.gov
  where state in ('FL', 'US')
"

db_area_data = ROracle::dbGetQuery(con,
                            area_data_query)

ROracle::dbDisconnect(con)

  db_data_w_area <- full_join(db_area_data, db_data)
  # Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
  # LOCAL_AREA_CODE)`

  return(db_data_w_area)
}

# Uncomment to run
# tic("data_from_db()")
# db_data_w_area <- data_from_db()
# toc()

db_data_w_area_file_path <-
  file.path(my_paths$inputs,
            "fishing_effort_locations/db_data_w_area_more_fields.csv")

# write_csv(db_data_w_area,
          # db_data_w_area_file_path)

# or get data from the saved csv ----
db_data_w_area <- readr::read_csv(db_data_w_area_file_path)

## ---- get other geographical data ----
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

# Filter out maximum by data ----
db_data_w_area_no_mex <-
  db_data_w_area %>%
  dplyr::filter(!(grepl("MEX", AREA_NAME))) %>%
  dplyr::filter(!(grepl("GOM", AREA_NAME))) %>%
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) %>%
  # south and north by SA shp
  dplyr::filter(between(LATITUDE, 23.81794, 36.55028)) %>%
  # west and east by SA shp
  dplyr::filter(between(LONGITUDE, -83, -71.37133))

# dim(db_data_w_area_no_mex)

# st_geometry(sa_shp)
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -71.37133 ymax: 36.55028

db_data_w_area_no_mex_uniq <-
  db_data_w_area_no_mex %>%
  unique()

# keep fewer columns ----
db_data_w_area_report_short <-
  db_data_w_area_no_mex_uniq %>%
  dplyr::select(all_of(fields_list))

# dim(db_data_w_area_report_short)

# SA EEZ for all ----
## get only the points inside the SA EEZ ----
db_data_w_area_report_sf <-
  db_data_w_area_report_short %>%
  unique() %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

# dim(db_data_w_area_report_sf)
# [1] 45264    19

### with st_intersection (slow) ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf_more_fields.csv")

db_data_w_area_report_sa_eez_sf <-
  sf::read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  # see above
  my_to_sf()

#### save sa_eez_data ----
readr::write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

# dim(db_data_w_area_report_sa_eez_sf)

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report_short %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

## state waters sa ----
# search in fl_state_w_counties_shp shapefile names
# if it is in the SA list - keep it
get_state_waters_sa_sf <- function() {
  fl_counties_sa <- c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "Saint Johns",
    "Saint Lucie",
    "Volusia",
    "Monroe" #has GOM too, remove separately
  ) 

  fl_state_w_counties_names <- fl_state_w_counties_shp$gnis_name

  fl_state_w_counties_names_df <-
    as.data.frame(fl_state_w_counties_names)

  sa_fl_state_w_counties_names <-
    as.data.frame(fl_counties_sa)[[1]] %>%
    # for each fl_county
    purrr::map_df(function(fl_county) {
      sa_county <-
        fl_state_w_counties_names_df %>%
        dplyr::filter(grepl(
          fl_county,
          fl_state_w_counties_names_df$fl_state_w_counties_names
        ))

      return(sa_county)
    })

  fl_state_w_counties_sa <- filter(
    fl_state_w_counties_shp,
    gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names
  )

  return(fl_state_w_counties_sa)
}

# run it
fl_state_w_counties_sa_sf <- get_state_waters_sa_sf()

# mapview(fl_state_w_counties_sa_sf)

### get only state and inner waters by intersection ----
db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_28_s_sf,
                      fl_state_w_counties_sa_sf)


# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
db_data_w_area_report_28_s_sa_counties_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  sf::read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  # see above
  my_to_sf()

readr::write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

## For Monroe exclude GOM ----

### using sf::st_difference ----
# slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  # see above
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)

# or read csv
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  sf::read_sf(sa_counties_no_gom_sf_filename) %>%
  # see above
  my_to_sf()

# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

readr::write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# Report csv ----
my_sf_to_df <- function(my_sf) {
  my_df <-
    my_sf %>%
    # convert to data frame from an SF object
    sf::st_drop_geometry() %>%
    # keep only the fields from fields_list
    dplyr::select(dplyr::all_of(fields_list)
    ) %>%
    unique()

  return(my_df)
}

my_sf_to_csv <- function(my_sf, file_name) {
  # see above
  my_df <- my_sf_to_df(my_sf)

  readr::write_csv(
    my_df,
    file.path(my_paths$outputs,
              current_project_name,
              "report",
              paste0(file_name, ".csv"))
  )
}

# see above (F2)
my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")

my_sf_to_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf, "south_of_28_state_w")

# all maps together ----

# to keep the light basemaps
mapviewOptions(basemaps.color.shuffle = FALSE)

## south of 28 map ----
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  # transparency
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

m_sa_eez <-
  mapview(
    db_data_w_area_report_sa_eez_sf,
    layer.name = 'SA EEZ'
  )

# combine maps
all_maps <-
  m_s +
  m_g_r +
  m_sa_eez +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf

# to see it
all_maps
