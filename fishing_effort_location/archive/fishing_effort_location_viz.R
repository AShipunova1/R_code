# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

# library(zoo) #date manipulations
# # maps
# library(sf)
# library(mapview)
# library(leaflet)
# 
# source("~/R_code_github/useful_functions_module.r")
# my_paths <- set_work_dir()
# source(
#   file.path(
#     my_paths$git_r,
#     "fishing_effort_location",
#     "fishing_effort_locations_get_data.R"
#   )
# )

# SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
names(db_data)
# get 
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth

db_data %>%
  dplyr::select(AREA_CODE) %>%
# 
#     dplyr::select(AREA_CODE, SUB_AREA_CODE, LOCAL_AREA_CODE, DISTANCE_CODE_NAME) %>%
  unique()
  # View()

# VENDOR_APP_NAME ----
a <- db_data %>% 
  count(VENDOR_APP_NAME)
# 1 BLUEFIN DATA ACCSP SDK  33560
# 2          ETRIPS ONLINE   1185
# 3        ETRIPS/MOBILE 2  82207
# 4                   VESL 186854
# 5                   <NA>   2455
sum(a$n)
# 306261
# 253941

lat_long <- db_data %>%
  dplyr::select(LATITUDE, LONGITUDE, TRIP_START_DATE)

# plot(sa_shp)
# plot(gom_shp)
# str(sa_shp)

plot(sa_shp$geometry)

lat_long_sf <-
  lat_long %>%
  filter(complete.cases(.)) %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = 4326)

# str(lat_long_sf)
# plot(lat_long_sf)
# plot(lat_long_sf$geometry)

# shape files maps ----
m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

# m_g <- mapview(gom_shp,
#                layer.name = "Gulf of Mexico",
#                legend = FALSE)


# OK boundaries ----
# lat 23 : 28
# lon -71 : -83

# defined in the main
# clean_lat_long <- function(my_lat_long_df, my_limit = 1000) {
#   my_lat_long_df %>%
#     unique() %>%
#     head(my_limit) %>%
#     # all LONG should be negative
#     dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
#     # remove wrong coords
#     filter(between(LATITUDE, 23, 28) &
#              between(LONGITUDE, -83, -71)) %>%
#     # remove all entries with missing coords
#     # filter(complete.cases(.)) %>%
#     return()
# }

# to_sf <- function(my_df) {
#   my_df %>%
#     st_as_sf(coords = c("LONGITUDE",
#                         "LATITUDE"),
#              crs = 4326) %>%
#     return()
# }

to_sf <- function(my_df) {
  my_df %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = st_crs(sa_shp)) %>%
    return()
}

# --- with labels ----

# my_colors <-
#   # number of colors = number of unique points
#   viridisLite::turbo(n = dim(clean_lat_long_subset)[1],
#                      # reverse direction
#                      direction = -1)

# n_map <-
#   clean_lat_long_subset %>%
#   dplyr::mutate(point = paste(LATITUDE, LONGITUDE)) %>%
#   to_sf() %>%
#   mapview(zcol = "point",
#           # col.regions = my_colors,
#           col.regions = viridisLite::turbo,
#           legend = FALSE)

# n_map + m_g + m_s

# --- with dates ----
# View(db_data)
lat_long3 <- db_data %>%
  dplyr::mutate(ROW_ID = row_number()) %>%
  dplyr::mutate(TRIP_START_DAY_M =
           format(TRIP_START_DATE, "%m")) %>%
  dplyr::select(LATITUDE, LONGITUDE, ROW_ID, TRIP_START_DAY_M)

# str(lat_long3)

points_num <- 100
clean_lat_long_subset3 <-
  lat_long3 %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset3 %>%
  # dplyr::mutate(point = paste(LATITUDE, LONGITUDE, TRIP_START_DAY_M)) %>%
  dplyr::mutate(point = TRIP_START_DAY_M) %>%
  to_sf() %>%
  mapview(zcol = "point",
          col.regions = viridisLite::turbo,
          layer.name = 'Month',
          cex = "FISHING_GEAR_DEPTH",
          alpha = 0.3,
          legend = T)

### show my map + shape files ----
n_map + m_s

## with depth ----

# db_data %>% glimpse()

lat_long_dat_dep <-
  db_data %>%
  # labels are a month only
  dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  # compute on a data frame a row-at-a-time
  rowwise() %>%
  # get avg bottom depth
  dplyr::mutate(AVG_BOTTOM_DEPTH = mean(c(
    MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH
  ), na.rm = T)) %>%
  dplyr::ungroup() %>%
  # choose the first not na
  # gives the same result as AVG_BOTTOM_DEPTH
  dplyr::mutate(AVG_DEPTH = coalesce(AVG_BOTTOM_DEPTH,
                              FISHING_GEAR_DEPTH,
                              DEPTH)) %>%
  # 0 instead of NA
  dplyr::mutate(AVG_DEPTH = replace_na(AVG_DEPTH, 0))

points_num <- 1000

clean_lat_long_subset <-
  lat_long_dat_dep %>%
  dplyr::select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH) %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset %>%
  # save info to show on the map
  dplyr::mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
  # convert to sf
  # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
  to_sf() %>%
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

n_map + m_s

# for quarters ----

lat_long_dat_dep_q <-
  lat_long_dat_dep %>%
  # add quarter
  dplyr::mutate(YEAR_QUARTER = as.yearqtr(TRIP_START_DATE)) %>%
  dplyr::mutate(QUARTER = format(YEAR_QUARTER, "%q"))

lat_long_dat_dep_q_list <-
  split(lat_long_dat_dep_q,
        as.factor(lat_long_dat_dep_q$YEAR_QUARTER))

mapview_q <- function(my_df, points_num, q_name) {
  clean_lat_long_subset <-
    my_df %>%
    dplyr::select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH,
           YEAR_QUARTER) %>%
    clean_lat_long(points_num)
  
  n_map <-
    clean_lat_long_subset %>%
    dplyr::mutate(POINT = paste(LATITUDE, LONGITUDE, YEAR_QUARTER,
                         sep = ", ")) %>%
    to_sf() %>%
    mapview(
      zcol = "TRIP_START_M",
      col.regions = viridisLite::turbo,
      layer.name = 'Month',
      cex = "FISHING_GEAR_DEPTH",
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

points_num <- 1000

maps_q <-
  map(names(lat_long_dat_dep_q_list),
      function(q_name) {
        # browser()
        m_n <- mapview_q(lat_long_dat_dep_q_list[[q_name]],
                         points_num,
                         q_name)
        return(m_n + m_s)
      })

maps_q[[4]]

# map(lat_long_dat_dep_q_list,
#     function(x) {dim(x)})
# $`2022 Q1`
# [1] 35207    24
# 
# $`2022 Q2`
# [1] 91786    24
# 
# $`2022 Q3`
# [1] 89717    24
# 
# $`2022 Q4`
# [1] 37148    24

map(lat_long_dat_dep_q_list,
    function(x) {
      x %>%
        dplyr::select(LATITUDE, LONGITUDE) %>%
        filter(complete.cases(.)) %>%
        unique() %>%
        dim()
    })
# $`2022 Q1`
# [1] 9219    2
# 
# $`2022 Q2`
# [1] 23792     2
# 
# $`2022 Q3`
# [1] 22052     2
# 
# $`2022 Q4`
# [1] 8950    2

# View(lat_long_dat_dep_q)
# check depth ----
lat_long_dat_dep_q %>% 
 filter(!(AVG_BOTTOM_DEPTH == AVG_DEPTH)) %>% dim()
# 0
# TODO: remove AVG_DEPTH
# TODO: create lat_long_dat_dep_q independently

# lat long and area ----
dim(db_area_data)[1]
# 83 FL
# 291
dim(db_data)[1]
# 254283     
# 254283 + 83 = 254366
# 254528

db_data_w_area <- full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`
dim(db_data_w_area)[1]
# 254444
# 254689

lat_long_area <-
  db_data_w_area %>%
  # labels are a month only
  dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  dplyr::select(
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
  dplyr::select(AREA_NAME, REGION) %>% 
  filter(grepl("MEX", AREA_NAME) | grepl("GOM", AREA_NAME)) %>% 
  unique() 

# exclude GOM
lat_long_area_clean_no_gom <-
  lat_long_area_clean %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

lat_long_area_clean_sf <-
  lat_long_area_clean_no_gom %>%
  dplyr::mutate(
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
  to_sf()

lat_long_area_clean_map <-
  lat_long_area_clean_sf %>%
  mapview(
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # cex = "DISTANCE_CODE_NAME",
    alpha = 0.3,
    legend = F,
    clusterOptions = markerClusterOptions()
  )

# lat_long_area_clean_map

# SpatialPointsDataFrame(coordinates(gadmCHE), 
#                                       data = gadmCHE@data, 
#                                       proj4string = CRS(proj4string(gadmCHE)))
# 
# minus_sa <- st_difference(corrected_data_sf, sa_shp)
# sa_areas_only <- st_difference((lat_long_area_clean_sf + sa_shp), gom_reef_shp)

 # to avoide 
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf_use_s2(FALSE)

sa_areas_minus_gom <- st_difference(lat_long_area_clean_sf, gom_reef_shp)
# although coordinates are longitude/latitude, st_difference assumes
# that they are planar

sa_areas_minus_gom %>%
    mapview(
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # cex = "DISTANCE_CODE_NAME",
    alpha = 0.3,
    legend = F,
    clusterOptions = markerClusterOptions()
  )

## no gom by month ----
# names(db_data)
lat_long_month_depth <-
  db_data_w_area %>%
  # labels are a month only
  dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  # compute on a data frame a row-at-a-time
  rowwise() %>%
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
      AVG_DEPTH,
      AREA_NAME,
      START_PORT_NAME,
      sep = ", "
    )) %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

all_points <- dim(lat_long_month_depth)[1]
# 254503

lat_long_month_depth_clean <- clean_lat_long(lat_long_month_depth, all_points)
dim(lat_long_month_depth_clean)[1]
# 28032
# %>%
#   to_sf(lat_long_month_depth_clean)

names(lat_long_month_depth_minus_gom)

lat_long_month_depth_minus_gom_sf <- st_difference(to_sf(lat_long_month_depth_clean), gom_reef_shp)

mapviewOptions(fgb = FALSE)
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

res_map <- lat_long_month_no_gom_map + m_s
res_map1 <- lat_long_month_no_gom_map + m_s + mapview(atl_state_waters)


# install.packages("webshot2")
# library(webshot2)
# mapviewOptions(fgb = FALSE)
# remotes::install_github("r-spatial/mapview")
png_fl <- "res_map.png"
mapview::mapshot(res_map, file = png_fl)
# open the file
browseURL(png_fl)

## clusters ----
lat_long_area_for_leaflet <-
  clean_lat_long(lat_long_area, all_points) %>%
  dplyr::mutate(
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
## check different area options ----
lat_long_area_clean %>%
  dplyr::select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE,
         DISTANCE_CODE_NAME) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  glimpse()
# Rows: 85

lat_long_area_clean %>%
  dplyr::select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# Rows: 47

lat_long_area_clean %>%
  dplyr::select(AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# 26

lat_long_area_clean %>%
  dplyr::select(AREA_NAME,
         AREA_CODE) %>%
  filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
  filter(!grepl("TAMPA", AREA_NAME)) %>% 
  filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
  # write_csv("area_code_name.csv")

# View(db_data)

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
