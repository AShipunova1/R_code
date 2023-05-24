# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
source(file.path(my_paths$git_r, "fishing_effort_location", "fishing_effort_locations_get_data.R"))

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

# south of 28N - all SA
# north of 28N - EEZ only

# south of 28N - all SA ----
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

# combine with additional area data ----
db_data_w_area <- full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`

all_points <- dim(db_data_w_area)[1]
# 254689

# View(db_data_w_area)

db_data_w_area_lat_lon <- 
  db_data_w_area %>%
      # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71))
# 92949    

dim(db_data_w_area_lat_lon)

db_data_w_area_lat_lon_reg <-  
  db_data_w_area_lat_lon %>% 
  filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
  filter(!grepl("TAMPA", AREA_NAME)) %>% 
  filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
  filter(!grepl("GULF OF MEXICO", REGION))

dim(db_data_w_area_lat_lon_reg)
# 88796

# View(db_data_w_area_lat_lon_reg)

# to_report <-
#   db_data_w_area_lat_lon_reg %>%
#   select(
#     TRIP_START_DATE,
#     TRIP_END_DATE,
#     START_PORT,
#     START_PORT_NAME,
#     START_PORT_COUNTY,
#     START_PORT_STATE,
#     END_PORT,
#     LATITUDE,
#     LONGITUDE,
#     MINIMUM_BOTTOM_DEPTH,
#     MAXIMUM_BOTTOM_DEPTH,
#     FISHING_GEAR_DEPTH,
#     DEPTH
#   ) %>%
#   unique() #?

# dim(to_report)
# 27077

# add counts to unique?
  db_data_w_area_lat_lon_reg %>%
  select(
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
    DEPTH
  ) %>%
data_overview()

# make a flat file ----
dir_to_comb <- file.path(my_paths$git_r, "fishing_effort_location")

files_to_combine <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R"),
    file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat.R")

# run as needed
make_a_flat_file(flat_file_name,
                 files_to_combine_list)

# SA EEZ only ----
db_data_w_area_report <-
  db_data_w_area %>%
  # all LONG should be negative
  mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    START_PORT,
    START_PORT_NAME,
    START_PORT_COUNTY,
    START_PORT_STATE,
    END_PORT,
    LATITUDE,
    LONGITUDE,
    # MINIMUM_BOTTOM_DEPTH,
    # MAXIMUM_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH
  )

dim(db_data_w_area_report)
# 254689     
db_data_w_area_report_short <-
  db_data_w_area_report %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
# 253142
dim(db_data_w_area_report_short)
# data_overview(db_data_w_area_report)

db_data_w_area_report_sf <- sf::st_as_sf(
  db_data_w_area_report_short,
  coords = c("LONGITUDE",
             "LATITUDE"),
  crs = sf::st_crs(sa_shp)
)

db_data_w_area_report_sa_eez <-
  sf::st_intersection(db_data_w_area_report_sf, sa_shp)

# cc <- sf::st_coordinates(db_data_w_area_report_sa_eez)
# 
# str(db_data_w_area_report_sa_eez)

m_db_data_w_area_report_sa_eez <- mapview(db_data_w_area_report_sa_eez,
        layer.name = 'SA EEZ')

# db_data_w_area_report_sa_eez %>%
#   select(-c(Id, AreaName)) %>%
#   write_csv(
#   file.path(
#     my_paths$outputs,
#     "fishing_effort_location",
#     "db_data_w_area_report_sa_eez.csv"
#   ))
# 
#   

# to_sf

str(sa_shp)
s1 <- filter(sa_shp,
             AreaName == "Off FL")

mapview(s1)

# state waters sa ----
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
  "Monroe"
)

names(fl_state_w_counties)
fl_state_w_counties$gnis_name %>% paste0(collapse = ", ")

fl_state_w_counties_names <- fl_state_w_counties$gnis_name
length(fl_state_w_counties_names)
# 67

grep("Monroe", fl_state_w_counties_names, value = T)

str(fl_counties_sa)
# 12 + Monroe

fl_state_w_counties_names_df <- as.data.frame(fl_state_w_counties_names)
# str(fl_state_w_counties_names_df)
# fl_state_w_counties_names) %>%

sa_fl_state_w_counties_names <-
  as.data.frame(fl_counties_sa)[[1]] %>%
  map_df(function(fl_county) {
    # browser()
    sa_county <-
      fl_state_w_counties_names_df %>%
      filter(grepl(
        fl_county,
        fl_state_w_counties_names_df$fl_state_w_counties_names
      ))
    
    return(sa_county)
  })


fl_state_w_counties_sa <- filter(fl_state_w_counties,
             gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names)

mapview(fl_state_w_counties_sa)

names(fl_state_w_counties_sa)

db_data_w_area_report_sa_counties <-
  sf::st_intersection(db_data_w_area_report_sf, fl_state_w_counties_sa)

## exclude GOM ----
db_data_w_area_report_sa_counties_no_gom <- st_difference(db_data_w_area_report_sa_counties, gom_reef_shp)


### map ----
m_db_data_w_area_report_sa_counties_no_gom <-
  mapview(
  db_data_w_area_report_sa_counties_no_gom,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

m_db_data_w_area_report_sa_eez + m_db_data_w_area_report_sa_counties_no_gom

### state w south of 28 ----
db_data_w_area_report_sf_28_s <-
  db_data_w_area_report_short %>%
  filter(between(LATITUDE, 23, 28) &
           between(LONGITUDE, -83, -71)) %>%
  sf::st_as_sf(coords = c("LONGITUDE",
                          "LATITUDE"),
               crs = sf::st_crs(sa_shp))
# dim(db_data_w_area_report_sf_28_s)
# 92949    

## exclude GOM ----
  db_data_w_area_report_sa_counties_no_gom <-
    sf::st_difference(db_data_w_area_report_sa_counties, gom_reef_shp)

### Below 28: remove GOM at sea points ----
db_data_w_area_report_28_s_no_gom_reef <-
  sf::st_difference(db_data_w_area_report_sf_28_s, gom_reef_shp)

### Below 28: keep only SA counties ----
db_data_w_area_report_28_s_no_gom_reef_state_w_sf <-
  sf::st_intersection(db_data_w_area_report_28_s_no_gom_reef,
                      fl_state_w_counties_sa)


m_db_data_w_area_report_28_s_no_gom_reef_state_w_sf <-
  mapview(
  db_data_w_area_report_28_s_no_gom_reef_state_w_sf,
  col.regions = "green",
  layer.name = 'State and inner waters south of 28N'
)

m_db_data_w_area_report_sa_eez + m_db_data_w_area_report_28_s_no_gom_reef_state_w_sf

# mapview(db_data_w_area_report_28_s_no_gom_reef) + m_s

# grey points in SA outside of EEZ

# - sa_eez
db_data_w_area_report_28_s_no_gom_reef_minus_sa_eez <-
  sf::st_difference(db_data_w_area_report_28_s_no_gom_reef, sa_shp)

mapview(db_data_w_area_report_28_s_no_gom_reef_minus_sa_eez)

# - state_w
db_data_w_area_report_28_s_no_gom_reef_minus_sa_eez_minus_state <-
  sf::st_difference(db_data_w_area_report_28_s_no_gom_reef_minus_sa_eez,
                  fl_state_w_counties)
