# errors in geo data ----
# 1) a sign
# 2) on land
# 3) outside boundaries
# 3a) if 28 north - see the wrong long?
# 4) wrong depth
# 5) missing coords
# filter(complete.cases(.)) %>%
# 6) area codes?
# AREA_CODE               91
# SUB_AREA_CODE           56
# DISTANCE_CODE_NAME       5
# LOCAL_AREA_CODE         46

# ===
# fathom, old English measure of length, now standardized at 6 feet (1.83 metre), which has long been used as a nautical unit of depth.

# ===
# LATITUDE      NA's   :1198
# LATITUDE        LONGITUDE
# Min.   :-87.30   Min.   :-117.25
# Max.   : 90.00   Max.   : 137.59

# ===
data_overview(db_data)

lat_long_to_map <- function(my_df, my_title) {
  my_df %>%
  # save info to show on the map
  mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
  # convert to sf
  # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
  to_sf() %>%
  mapview(
    col.regions = viridisLite::turbo,
    layer.name = my_title,
    legend = TRUE
  ) %>% return()
}

# 1) a sign ----
## positive_long ---- 
positive_long <-
  db_data %>%
  filter(LONGITUDE > 0)

dim(positive_long)
# 49777

#     LATITUDE        LONGITUDE      
# Min.   :-87.30   Min.   :-117.25  
# 1st Qu.: 26.10   1st Qu.: -83.57  
# Median : 29.00   Median : -81.01  
# Mean   : 28.88   Mean   : -50.88  
# 3rd Qu.: 30.16   3rd Qu.: -75.85  
# Max.   : 90.00   Max.   : 137.59  
# NA's   :1277     NA's   :1277     

## negative latitude ----
negative_lat <-
  db_data %>%
  filter(LATITUDE <= 0)

dim(negative_lat)
# 120  

negative_lat %>% select(LATITUDE, LONGITUDE) %>% unique()
glimpse(negative_lat)
negative_lat %>% count(LATITUDE, LONGITUDE)
# LATITUDE LONGITUDE  n
# 1 -87.30000  29.30000  2
# 2 -83.97695  29.15705 11
# 3 -76.76466  55.33196  3
# 4 -41.40274 -14.40965  2
# 5 -30.00000  86.00000 93
# 6 -27.85917 137.58650  4
# 7   0.00000   0.00000  5

negative_lat_c_subset <-
  negative_lat %>%
  select(LATITUDE, LONGITUDE)

lat_long_to_map <- function(my_df, my_title) {
  my_df %>%
  # save info to show on the map
  mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
  # convert to sf
  # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
  to_sf() %>%
  mapview(
    col.regions = viridisLite::turbo,
    layer.name = my_title,
    legend = TRUE
  ) %>% return()
}

# neg_lat_map <-
  # negative_lat_c_subset %>%
  # # save info to show on the map
  # mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
  # # convert to sf
  # # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
  # to_sf() %>%
  # mapview(
  #   col.regions = viridisLite::turbo,
  #   layer.name = 'Negative latitude',
  #   legend = TRUE
  # )

## numbers are off the chart
coords_off_boundaries <-
  db_data %>%
  filter(!between(abs(LATITUDE), 23, 37) |
           !between(abs(LONGITUDE), 71, 98))

