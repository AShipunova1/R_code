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
# ? how to get the boundary?

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

names(db_data_w_area_lat_lon)

db_data_w_area_lat_lon_reg <-  
  db_data_w_area_lat_lon %>% 
  filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
  filter(!grepl("TAMPA", AREA_NAME)) %>% 
  filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
  filter(!grepl("GULF OF MEXICO", REGION))

dim(db_data_w_area_lat_lon_reg)
# 88796

View(db_data_w_area_lat_lon_reg)

to_report <-
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
  unique() #?

dim(to_report)
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

# # write to file
# files_to_combine_list <- files_to_combine
# # sink()
# for (i in 1:length(files_to_combine_list)) {
#     # browser()
#     current_file = readLines(files_to_combine_list[i])
#   cat("\n\n#### Current file:", files_to_combine_list[i], "\n\n")
#   # cat(current_file, sep = "\n")
# }

# run as needed
make_a_flat_file(flat_file_name,
                 files_to_combine_list)
