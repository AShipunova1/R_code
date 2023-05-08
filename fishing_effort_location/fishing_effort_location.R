# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
library(ROracle)
library(zoo)
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

# LATITUDE      NA's   :1198            
# LATITUDE        LONGITUDE  
# Min.   :-87.30   Min.   :-117.25
# Max.   : 90.00   Max.   : 137.59  
lat_long <- db_data %>%
  select(LATITUDE, LONGITUDE, TRIP_START_DATE)

lat_long2 <- db_data %>%
  mutate(row_id = row_number()) %>%
  select(LATITUDE, LONGITUDE, row_id)

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

# mapview(lat_long_sf)
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

my_colors <-
  # number of colors = number of unique points
  viridisLite::turbo(n = dim(clean_lat_long_subset)[1],
                     # reverse direction
                     direction = -1)

n_map <-
  clean_lat_long_subset %>%
  mutate(point = paste(LATITUDE, LONGITUDE)) %>%
  to_sf() %>%
  mapview(zcol = "point",
          # col.regions = my_colors,
          col.regions = viridisLite::turbo,
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

# --- with dates ----
# View(db_data)
lat_long3 <- db_data %>%
  mutate(ROW_ID = row_number()) %>%
  mutate(TRIP_START_DAY_M =
           format(TRIP_START_DATE, "%m")) %>%
           # format(as.Date(TRIP_START_DATE,
           #                # 2022-12-10 23:00:00"
           #                format = "%d/%m/%Y"), "%m/%d")) %>%
  select(LATITUDE, LONGITUDE, ROW_ID, TRIP_START_DAY_M)
str(lat_long3)

points_num <- 100
clean_lat_long_subset <-
  lat_long3 %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset %>%
  # mutate(point = paste(LATITUDE, LONGITUDE, TRIP_START_DAY_M)) %>%
  mutate(point = TRIP_START_DAY_M) %>%
  to_sf() %>%
  mapview(zcol = "point",
          # col.regions = my_colors,
          col.regions = viridisLite::turbo,
          layer.name = 'Month',
          cex = "CATCH_CNT",
          alpha = 0.3,
          legend = T)

n_map + m_g + m_s

# --- with depth ----

db_data %>% glimpse()

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
  mutate(AVG_DEPTH = coalesce(
    AVG_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH,
    DEPTH
    # ,
    # AVG_DEPTH_IN_FATHOMS
  )) %>%
  # MINIMUM_BOTTOM_DEPTH,  MAXIMUM_BOTTOM_DEPTH,  AVG_DEPTH_IN_FATHOMS,  FISHING_GEAR_DEPTH,  DEPTH,
  mutate(AVG_DEPTH = replace_na(AVG_DEPTH, 0))

# %>%
# str()

points_num <- 1000
clean_lat_long_subset <-
  lat_long_dat_dep %>%
  select(LATITUDE, LONGITUDE, TRIP_START_M, AVG_DEPTH) %>%
  clean_lat_long(points_num)

n_map <-
  clean_lat_long_subset %>%
  mutate(POINT = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
  to_sf() %>%
  mapview(zcol = "TRIP_START_M",
          col.regions = viridisLite::turbo,
          layer.name = 'Month',
          cex = "AVG_DEPTH",
          alpha = 0.3,
          legend = T)

n_map + m_g + m_s

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
lat_long_dat_dep_q %>% 
 filter(!(AVG_BOTTOM_DEPTH == AVG_DEPTH)) %>% dim()
# 0
# TODO: remove AVG_DEPTH
# TODO: create lat_long_dat_dep_q independently

