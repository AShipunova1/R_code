# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
library(ROracle)
# library(sp) # vector data
# library(rgdal) # input/output, projections
library(sf)
library(mapview)
library(leaflet)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

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
  avg_depth_in_fathoms,
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

db_data = dbGetQuery(con,
                     request_query)

# dbDisconnect(con)

data_overview(db_data)
# str(db_data)
# 'data.frame':	306261 obs. of  19 variables:

a <- db_data %>% 
  count(VENDOR_APP_NAME)
# 1 BLUEFIN DATA ACCSP SDK  33560
# 2          ETRIPS ONLINE   1185
# 3        ETRIPS/MOBILE 2  82207
# 4                   VESL 186854
# 5                   <NA>   2455
sum(a$n)
# 306261

# LATITUDE      NA's   :1198            
# TODO: check the sign
# LATITUDE        LONGITUDE  
# Min.   :-87.30   Min.   :-117.25
# Max.   : 90.00   Max.   : 137.59  
lat_long <- db_data %>%
  select(LATITUDE, LONGITUDE, TRIP_START_DATE)

lat_long2 <- db_data %>%
  mutate(row_id = row_number()) %>%
  select(LATITUDE, LONGITUDE, row_id)

## ---- get geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- read_sf(shapefile_file_name)
  return(x)
}
sa_shp <- read_shapefile("osa_n_gom/SA_EEZ_off_states.shp")
gom_shp <- read_shapefile("osa_n_gom/ReefFish_EFH_GOM.shp")
plot(sa_shp)
plot(gom_shp)
str(sa_shp)

plot(sa_shp$geometry)

lat_long_sf <-
  lat_long %>%
  filter(complete.cases(.)) %>%
    # mutate(latitude = jitter(latitude, factor = jitter_factor)) %>%
    # mutate(longitude = jitter(longitude, factor = jitter_factor)) %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = 4326)
str(lat_long_sf)
plot(lat_long_sf)
plot(lat_long_sf$geometry)

# m1 <- mapview(lat_long_sf,
#           zcol = "TRIP_START_DATE"
#           # ,
#           # cex = "CATCH_CNT",
#           # alpha = 0.3,
#           # col.regions = viridisLite::turbo,
#           # legend = FALSE,
#           # layer.name = mrip_fhier_by_state_df$common_name[1]
#           ) 
  # %>%
  # addStaticLabels(label = mrip_fhier_by_state_df$name_cnts,
                  # noHide = TRUE,
                  # direction = 'top',
                  # textOnly = TRUE,
                  # textsize = "10px")

mapview(lat_long_sf)
# Error in dispatch(map, "fitBounds", leaflet = { : Invalid map parameter

lat_long2_sf <- lat_long2 %>%
  filter(complete.cases(.)) %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = 4326)

plot(lat_long2_sf)
mapview(lat_long2_sf)

m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)
m_g <- mapview(gom_shp,
               layer.name = "Gulf of Mexico",
               legend = FALSE)
# 20 points ----

lat_long2_sf_20 <- 
  lat_long2 %>%
  unique() %>%
  head(20) %>%
  mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  filter(complete.cases(.)) %>%
  st_as_sf(coords = c("LONGITUDE",
                      "LATITUDE"),
           crs = 4326)

View(lat_long2_sf_20)

m1 <- mapview(lat_long2_sf_20)
m1 + m_g + m_s

# --- OK boundaries ----
# lat 23 : 36
# lon -71 : -98

clean_lat_long <- function(my_lat_long_df, my_limit) {
  my_lat_long_df %>%
    unique() %>%
    head(my_limit) %>%
    # all should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 37) &
             between(LONGITUDE, -98, -71)) %>%
    # remove all entryes with missing coords
    filter(complete.cases(.)) %>%
    return()
}

to_sf <- function(my_df) {
  my_df %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = 4326) %>%
    return()
}

# --- using first n ----
n100 <-
  lat_long2 %>% 
  clean_lat_long(100) %>%
  to_sf() %>%
  mapview(grid = TRUE)

n100 + m_g + m_s
# --- with labels ----
points_num <- 1000
clean_lat_long_subset <-
  lat_long2 %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset %>%
  mutate(point = paste(LATITUDE, LONGITUDE)) %>%
  to_sf() %>%
  mapview(zcol = "point",
          col.regions = viridisLite::turbo(n = dim(clean_lat_long_subset)[1],
                                           direction = -1),
          legend = FALSE)

n_map + m_g + m_s

#   mapview(x_sf,
  #         zcol = "name_cnts",
  #         cex = "CATCH_CNT",
  #         alpha = 0.3,
  #         col.regions = viridisLite::turbo,
  #         legend = FALSE,
  #         layer.name = mrip_fhier_by_state_df$common_name[1]
  #         ) %>%
  # addStaticLabels(label = mrip_fhier_by_state_df$name_cnts,
  #                 # noHide = TRUE,
  #                 direction = 'top',
  #                 # textOnly = TRUE,
  #                 textsize = "10px")
