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
# data_overview(db_data)

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

# corrected_data_sf <- to_sf(corrected_data) 
corrected_data_sf <- 
  corrected_data %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = st_crs(sa_shp))
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

# st_crs(m_s)
# Coordinate Reference System: NA
st_crs(corrected_data_sf)
# Coordinate Reference System:
    # ID["EPSG",4269]]

st_crs(sa_shp)
st_crs(gom_shp)
# Coordinate Reference System:
#   User input: NAD83
    # ID["EPSG",4269]]

identical(st_crs(sa_shp),
st_crs(gom_shp)
)
# TRUE
# m_s@map$x$options$crs
# $crsClass
# [1] "L.CRS.EPSG3857"

# in_sa <- st_intersection(corrected_data_sf, sa_shp)
# attribute variables are assumed to be spatially constant throughout all geometries 

# st_crs(corrected_data_sf)
# st_crs(sa_shp)

# mapview(minus_map)
# subset
# mapview(corrected_data_sf[sa_shp, ])
# lat_lon_data[shapefile_data, ]

minus_sa <- st_difference(corrected_data_sf, sa_shp)
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

mapview(minus_sa)

sf_use_s2(FALSE)
# Spherical geometry (s2) switched off

# minus_gom <- st_difference(corrected_data_sf, gom_shp)
# although coordinates are longitude/latitude, st_difference
# assumes that they are planar
# in_gom <- st_intersection(corrected_data_sf, gom_shp)

dim(minus_sa)
# [1] 44023    20

# minus_sa_gom <- st_difference(gom_shp, minus_sa)
minus_sa_gom2 <- st_difference(minus_sa, gom_shp)

# p <- poly2nb(st_make_valid(shp))

mm <- mapview(minus_sa_gom2, color = "green")

View(minus_sa_gom2)

mm + m_g + m_s
# m_s is still resent?

# A helper function that erases all of y from x: ----
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

names(corrected_data) %>% paste0(collapse = ", ")

corrected_data_short_sf <-
  corrected_data %>%
  select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    LATITUDE,
    LONGITUDE,
    MINIMUM_BOTTOM_DEPTH,
    MAXIMUM_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH
  ) %>%
  st_as_sf(coords = c("LONGITUDE",
                      "LATITUDE"),
           crs = st_crs(sa_shp))

# str(corrected_data_short_sf)
# Classes ‘sf’ and 'data.frame':	11998 obs. of  6 variables:

# st_difference(corrected_data_short_sf, 
#               st_union(st_combine(c(gom_shp, sa_shp))))

union_shp <- st_union(gom_shp, sa_shp)
# although coordinates are longitude/latitude, st_union assumes that they are planar
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 

plot(union_shp)

# st_difference(corrected_data_short_sf, 
#               st_union(st_combine(c(gom_shp, sa_shp))))

corrected_data_short_minus_union_shp <-
  st_difference(corrected_data_short_sf, union_shp)
# although coordinates are longitude/latitude, st_difference
# assumes that they are planar

# write_csv(corrected_data_short_minus_union_shp, "short_minus_sa_gom.csv")

m_minus <- mapview(corrected_data_short_minus_union_shp, color = "green")
# View(corrected_data_short_minus_union_shp)

m_minus + mapview(union_shp)

# minus sa again? ----

all_minus_sa <-
  st_difference(corrected_data_short_minus_union_shp, sa_shp)

write_csv(all_minus_sa, "all_minus_sa.csv")

m_all_minus_sa <-
  mapview(
    all_minus_sa,
    col.regions = "green",
    layer.name = 'Not in GOM or SA',
    alpha = 0.3,
    cex = 1
  )

m_all_minus_sa + union_shp
# ===

# FL ---
# https://catalog.data.gov/dataset/tiger-line-shapefile-2019-state-florida-current-place-state-based/resource/fcf74536-aeab-4ed1-a9df-06daf29a527b

fl_shp <- read_shapefile("tl_2019_12_place_FL/tl_2019_12_place.shp")

st_crs(fl_shp)
    # ID["EPSG",4269]]

on_land <- st_intersection(corrected_data_short_sf, fl_shp)
dim(on_land)
# [1] 3660   22

m_l <- mapview(on_land)
