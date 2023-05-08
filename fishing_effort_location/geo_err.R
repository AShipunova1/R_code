library(sp)
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
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = st_crs(sa_shp)) %>%
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

neg_lat_map <-
  lat_long_to_map(negative_lat_c_subset, 'Negative latitude')

## 3) outside boundaries ----
coords_off_boundaries <-
  db_data %>%
  filter(!between(abs(LATITUDE), 23, 37) |
           !between(abs(LONGITUDE), 71, 98))

coords_off_boundaries %>% 
  count(LATITUDE, LONGITUDE) %>%
  arrange(n) %>% tail() 
     # LATITUDE LONGITUDE   n
# 2253 38.61667 -74.48333 107
# 2254 83.00000  27.00000 125
# 2255 41.08637 -71.75213 140
# 2256 87.00000  30.00000 217
# 2257 39.81874 -74.07497 326
# 2258 41.58291 -70.38649 410

coords_off_boundaries %>% 
  # dim()
  # 11357    
  select(LATITUDE, LONGITUDE) %>% unique() %>%
  lat_long_to_map('coords_off_boundaries')

## 2) on land ----
corrected_data <-
  db_data %>%
  unique() %>%
  # all LONG should be negative
  mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # remove wrong coords
  filter(between(LATITUDE, 23, 28) &
           between(LONGITUDE,-98,-71)) %>%
  # remove all entries with missing coords
  filter(complete.cases(.))

corrected_data_sf <- to_sf(corrected_data) 
# corrected_data_map <- corrected_data_sf %>% mapview()
# View(corrected_data_map)
# 11998

# shape files maps ----
m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)
m_g <- mapview(gom_shp,
               layer.name = "Gulf of Mexico",
               legend = FALSE)

st_crs(m_s)
# Coordinate Reference System: NA
st_crs(corrected_data_sf)
# Coordinate Reference System:
#   User input: EPSG:4326 
#   wkt:
# ...
  # st_crs(x) == st_crs(y) is not TRUE

  # corrected_data_sf - m_s - m_g
# Error in `-.POSIXt`(left, right) : 
#   can only subtract numbers from "POSIXt" objects


# st_crs(m_s)
# Coordinate Reference System: NA
st_crs(corrected_data_sf)
# Coordinate Reference System:
#   User input: EPSG:4326 

# municipalities_31370 <- st_transform(municipalities, "EPSG:31370")
# sa_shp_4326 <- st_transform(sa_shp, "EPSG:4326")
# m_s <- CRS("+init=epsg:4326")
View(sa_shp_4326)
st_crs(sa_shp)
st_crs(gom_shp)
# Coordinate Reference System:
#   User input: NAD83
    # ID["EPSG",4269]]



# m_s@map$x$options$crs
# $crsClass
# [1] "L.CRS.EPSG3857"

# proj4string(m_s)

# read_sf 

# m_s <- mapview(sa_shp,
#                layer.name = "South Altlantic",
#                legend = FALSE)
# m_g <- mapview(gom_shp,
#                layer.name = "Gulf of Mexico",
#                legend = FALSE)

in_sa <- st_intersection(corrected_data_sf, sa_shp)
# attribute variables are assumed to be spatially constant throughout all geometries 

# st_crs(corrected_data_sf)
# st_crs(sa_shp)

mapview(minus_map)
# subset
mapview(corrected_data_sf[sa_shp, ])
# lat_lon_data[shapefile_data, ]

minus_sa <- st_difference(corrected_data_sf, sa_shp)
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

mapview(minus_sa)
