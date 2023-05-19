# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

library(zoo) #date manipulations

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
    head(my_limit) %>%
    # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71)) %>%
    # remove all entries with missing coords
    filter(complete.cases(.)) %>%
    return()
}


# --- with dates ----
# View(db_data)
lat_long3 <- db_data %>%
  mutate(ROW_ID = row_number()) %>%
  mutate(TRIP_START_DAY_M =
           format(TRIP_START_DATE, "%m")) %>%
  select(LATITUDE, LONGITUDE, ROW_ID, TRIP_START_DAY_M)

# str(lat_long3)

## with depth ----

lat_long_dat_dep <-
  db_data %>%
  # labels are a month only
  mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  # compute on a data frame a row-at-a-time
  rowwise() %>%
  # get avg bottom depth
  mutate(AVG_BOTTOM_DEPTH = mean(c(
    MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH
  ), na.rm = T)) %>%
  ungroup() %>%
  # choose the first not na
  # gives the same result as AVG_BOTTOM_DEPTH
  mutate(AVG_DEPTH = coalesce(AVG_BOTTOM_DEPTH,
                              FISHING_GEAR_DEPTH,
                              DEPTH)) %>%
  # 0 instead of NA
  mutate(AVG_DEPTH = replace_na(AVG_DEPTH, 0))

clean_lat_long_subset <-
  lat_long_dat_dep %>%
  select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH) %>%
  clean_lat_long(points_num)


lat_long_dat_dep_q_list <-
  split(lat_long_dat_dep_q,
        as.factor(lat_long_dat_dep_q$YEAR_QUARTER))

mapview_q <- function(my_df, points_num, q_name) {
  clean_lat_long_subset <-
    my_df %>%
    select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH,
           YEAR_QUARTER) %>%
    clean_lat_long(points_num)
  
  n_map <-
    clean_lat_long_subset %>%
    mutate(POINT = paste(LATITUDE, LONGITUDE, YEAR_QUARTER,
                         sep = ", ")) %>%
    to_sf() %>%
    mapview(
      zcol = "TRIP_START_M",
      col.regions = viridisLite::turbo,
      layer.name = 'Month',
      cex = "AVG_DEPTH",
      alpha = 0.3,
      legend = T
    )
  
  # browser()
  # title in bold
  title_html <- paste0("<b>", q_name, "</b>")
  
  # change in place
  n_map@map %<>%
    # add the title
    addControl(title_html,
               position = "bottomright")
  
  return(n_map)
}

# lat long and area ----
dim(db_area_data)[1]
# 83 FL
# 291
dim(db_data)[1]
# 254283     
# 254283 + 83 = 254366

db_data_w_area <- full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`
dim(db_data_w_area)[1]
# 254444
# 254689

lat_long_area <-
  db_data_w_area %>%
  # labels are a month only
  mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  select(
    LATITUDE,
    LONGITUDE,
    TRIP_START_M,
    AREA_CODE,
    DISTANCE_CODE_NAME,
    AREA_NAME,
    SUB_AREA_NAME, 
    REGION
  )

all_points <- dim(lat_long_area)[1]
# 254444
# 254689

lat_long_area_clean <- clean_lat_long(lat_long_area, all_points)
dim(lat_long_area_clean)[1]
# 6359

lat_long_area_clean %>%
  select(AREA_NAME, REGION) %>% 
  filter(grepl("MEX", AREA_NAME) | grepl("GOM", AREA_NAME)) %>% 
  unique() 

lat_long_area_clean_no_gom <-
  lat_long_area_clean %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

lat_long_area_clean_map <-
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
  to_sf() %>%
  mapview(
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # cex = "DISTANCE_CODE_NAME",
    alpha = 0.3,
    legend = F,
    clusterOptions = markerClusterOptions()
  )

lat_long_area_clean_map

# SpatialPointsDataFrame(coordinates(gadmCHE), 
#                                       data = gadmCHE@data, 
#                                       proj4string = CRS(proj4string(gadmCHE)))
# 
# lat_long_area_clean_map + sa_shp

lat_long_area_for_leaflet <-
  clean_lat_long(lat_long_area, all_points) %>%
  mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_CODE,
      DISTANCE_CODE_NAME,
      sep = ", "
    )
  ) 

lat_long_area_leaflet_w_clusters <-
  lat_long_area_for_leaflet %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(
    label = paste(
      lat_long_area_for_leaflet$LATITUDE,
      lat_long_area_for_leaflet$LONGITUDE,
      lat_long_area_for_leaflet$AREA_NAME,
      sep = ", "
    ),
    # lat_long_area_clean$POINT,
    # labelOptions = labelOptions(noHide = T),
    clusterOptions = markerClusterOptions()
  )

# doesn't show clusters
# lat_long_area_for_leaflet %>%
#   to_sf() %>%
# mapview(clusterOptions = markerClusterOptions())
# 
# + 
#   m_s

lat_long_area_leaflet_w_clusters
# ===
lat_long_area_clean %>%
  select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE,
         DISTANCE_CODE_NAME) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  glimpse()
# Rows: 85

lat_long_area_clean %>%
  select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# Rows: 47

lat_long_area_clean %>%
  select(AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# 26

lat_long_area_clean %>%
  select(AREA_NAME,
         AREA_CODE) %>%
  filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
  filter(!grepl("TAMPA", AREA_NAME)) %>% 
  filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
  # write_csv("area_code_name.csv")

View(db_data)

# separate SA only ----
# see v_safis_trip_download
"SELECT
  distinct region
FROM
safis.areas_fished@secapxdv_dblk.sfsc.noaa.gov"
# null, unknown?
# ===
# SOUTH ATLANTIC
# SOUTH ATLANIC
# MID ATLANTIC
# CARIBBEAN
#  
# 
# GULF OF MEXICO
# SOUTH ATLANTIC 
# NEW ENGLAND
# RHODE ISLAND
# UNKNOWN