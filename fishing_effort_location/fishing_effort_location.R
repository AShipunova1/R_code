# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98

library(zoo) #date manipulations
# maps
library(sf)
library(mapview)
library(leaflet)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
source(file.path(my_paths$git_r, "fishing_effort_location", "fishing_effort_locations_get_data.R"))

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
  select(LATITUDE, LONGITUDE, TRIP_START_DATE)

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
m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)
m_g <- mapview(gom_shp,
               layer.name = "Gulf of Mexico",
               legend = FALSE)

g_d <- mapview(gom_depth_shp,
               layer.name = "Gulf of Mexico depth",
               legend = TRUE)

g_d_5_100 <- mapview(gom_depth_shp5_100,
               layer.name = "Depth 5-100 m",
               color = "yellow",
               # col.regions = list("red", "green"),
               legend = TRUE)


g_d_100_1000 <- mapview(gom_depth_shp100_1000,
               layer.name = "Depth 100-1000 m",
               color = "lightgreen",
               # col.regions = list("red", "green"),
               legend = TRUE)

g_d_500_4000 <- mapview(gom_depth_shp500_4000,
               layer.name = "Depth 500-4000 m",
               color = "darkgreen",
               legend = TRUE)

  
# OK boundaries ----
# lat 23 : 28
# lon -71 : -98

clean_lat_long <- function(my_lat_long_df, my_limit) {
  my_lat_long_df %>%
    unique() %>%
    head(my_limit) %>%
    # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -98, -71)) %>%
    # remove all entries with missing coords
    filter(complete.cases(.)) %>%
    return()
}

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
#   mutate(point = paste(LATITUDE, LONGITUDE)) %>%
#   to_sf() %>%
#   mapview(zcol = "point",
#           # col.regions = my_colors,
#           col.regions = viridisLite::turbo,
#           legend = FALSE)

# n_map + m_g + m_s

# --- with dates ----
# View(db_data)
lat_long3 <- db_data %>%
  mutate(ROW_ID = row_number()) %>%
  mutate(TRIP_START_DAY_M =
           format(TRIP_START_DATE, "%m")) %>%
  select(LATITUDE, LONGITUDE, ROW_ID, TRIP_START_DAY_M)

# str(lat_long3)

points_num <- 1000
clean_lat_long_subset3 <-
  lat_long3 %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset3 %>%
  # mutate(point = paste(LATITUDE, LONGITUDE, TRIP_START_DAY_M)) %>%
  mutate(point = TRIP_START_DAY_M) %>%
  to_sf() %>%
  mapview(zcol = "point",
          col.regions = viridisLite::turbo,
          layer.name = 'Month',
          cex = "CATCH_CNT",
          alpha = 0.3,
          legend = T)

### show my map + shape files ----
n_map + m_g + m_s

## with depth ----

# db_data %>% glimpse()

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

points_num <- 1000

clean_lat_long_subset <-
  lat_long_dat_dep %>%
  select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH) %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset %>%
  # save info to show on the map
  mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
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
    cex = "AVG_DEPTH",
    # transparency
    alpha = 0.3,
    legend = TRUE
  )

n_map + m_g + m_s
# with depth lines
n_map + m_g + m_s + g_d_5_100 + g_d_100_1000 + g_d_500_4000

# for quarters ----

lat_long_dat_dep_q <-
  lat_long_dat_dep %>%
  # add quarter
  mutate(YEAR_QUARTER = as.yearqtr(TRIP_START_DATE)) %>%
  mutate(QUARTER = format(YEAR_QUARTER, "%q"))

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

points_num <- 3000

maps_q <-
  map(names(lat_long_dat_dep_q_list),
      function(q_name) {
        # browser()
        m_n <- mapview_q(lat_long_dat_dep_q_list[[q_name]],
                         points_num,
                         q_name)
        return(m_n + m_g + m_s)
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
        select(LATITUDE, LONGITUDE) %>%
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

