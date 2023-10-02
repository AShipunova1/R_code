# setup ----
library(mapview)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <- get_current_file_directory()

# get data ----
# "C:\Users\anna.shipunova\Documents\R_code_github\get_db_data\get_db_data.R"
source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 2.27 sec elapsed (from csv)

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols()

# statistics ----

dim(trip_coord_info)
# [1] 141350     39

# data_overview(trip_coord_info)

coordInfo_summ <- summary(trip_coord_info) |>
  as.data.frame()

coordInfo_summ_compact <-
  coordInfo_summ |>
  select(-Var1) |>
  tidyr::pivot_wider(names_from = Var2,
              # number of vessels
              values_from = Freq,
              values_fn = list)

# coordInfo_summ_compact$FISHING_HOURS

# str_split_fixed(coordInfo_summ$Freq, " *: *", 2) |> head()

coordInfo_summ1 <-
  coordInfo_summ |>
  tidyr::separate(Freq, c("stat_name", "sta_val"), " *: *",
                  extra = "merge")

# View(coordInfo_summ1)

coordInfo_summ2 <-
  coordInfo_summ1 |>
  distinct() |>
  pivot_wider(names_from = stat_name,
              id_cols = Var2,
              values_from = sta_val) |>
  # mutate_if(is.character, trimws) |>
  mutate(field_name = trimws(Var2),
         .before = 1,
         .keep = "unused")

names(coordInfo_summ2) <- fix_names(names(coordInfo_summ2))

# print_df_names(coordInfo_summ2)

coordInfo_summ2_short <-
  coordInfo_summ2 |>
  filter(is.na(mode)) |>
  select(field_name, min, max, na_s) |>
  distinct()

# View(coordInfo_summ2_short)

# coordInfo_summ2_short$field_name |> cat(sep = ", ")
# TRIP_ID, FISHING_HOURS, LATITUDE, LONGITUDE, AVG_DEPTH_IN_FATHOMS, E_DE, E_DC, DEPTH, MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH, FISHING_GEAR_DEPTH, DAYS_AT_SEA, T_DE, T_DC, VESSEL_ID, CF_PERMIT_ID, TRIP_START_DATE, TRIP_END_DATE, ACTIVITY_TYPE, SERO_VESSEL_PERMIT

weird_stats <-
  coordInfo_summ2_short |>
  filter(field_name %in%
           c("FISHING_HOURS", "LATITUDE", "LONGITUDE", "TRIP_END_DATE"))

# head(weird_stats)
#   field_name    min                        max                        na_s
# 1 FISHING_HOURS "0.000  "                  "4920.640  "                NA
# 2 LATITUDE      "-87.30  "                 "90.00  "                  "580  "
# 3 LONGITUDE     "-117.25  "                "137.59  "                 "580  "
# 4 TRIP_END_DATE "1969-08-17 00:00:00.00  " "2023-09-19 00:00:00.00  "  NA

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

lat_long_to_map <- function(my_df, my_title = "my_title") {
  my_df %>%
    # save info to show on the map
    dplyr::mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
    # convert to sf
    # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
    sf::st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE")) %>%
    mapview::mapview(
      col.regions = viridisLite::turbo,
      layer.name = my_title,
      legend = TRUE
    ) %>% return()
}

sa_shp <-
  sf::read_sf(r"(~\R_files_local\my_inputs\shapefiles\osa_n_gom\SA_EEZ_off_states.shp)")

trip_coord_info_map <-
  trip_coord_info |>
  select(LONGITUDE, LATITUDE) |>
  distinct() |>
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  mutate(label_lat_lon = paste(round(LATITUDE, 0),
                       round(LONGITUDE, 0)
                       )
  ) |>
  # head() |>
#   dim()
# [1] 116246      2
  sf::st_as_sf(coords = c("LONGITUDE",
                          "LATITUDE"),
               crs = sf::st_crs(sa_shp))

mapview::mapview(trip_coord_info_map,
                     # colors according a chosen column
    zcol = "label_lat_lon",
    # palette to choose colors from
    col.regions = viridisLite::turbo,
    layer.name = 'label_lat_lon',
    # transparency
    alpha = 0.3,
    legend = FALSE
)

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
# ====
world_coast <-
rnaturalearth::ne_coastline()
# worldmap <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
plot(world_coast)

ggplot() +
  ggplot2::geom_sf(data = world_coast) +
  ggplot2::geom_sf(data = trip_coord_info_sf_out,
                   colour = "blue",
                   transpa)


library(maps)
world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
ggplot() + geom_sf(data = world1) +
    ggplot2::geom_sf(data = trip_coord_info_sf_out,
                   colour = "blue",
                   fill = "lightblue")


bbxmin <- big_bounding_box$xmin
bbxmax <- big_bounding_box$xmax
bbymin <- big_bounding_box$ymin
bbymax <- big_bounding_box$ymax


ggplot() +
  ggplot2::geom_sf(data = world_coast) +
  ggplot2::geom_sf(data = trip_coord_info_sf_out,
                       color = "blue") +
  geom_rect(
    aes(
      xmin = big_bounding_box[["xmin"]],
      xmax = big_bounding_box[["xmax"]],
      ymin = big_bounding_box[["ymin"]],
      ymax = big_bounding_box[["ymax"]]
    ),
    color = "red",
    fill = NA
  )

ggplot() +
  ggplot2::geom_sf(data = world_coast) +
  ggplot2::geom_sf(data = trip_coord_info_map_data,
                       color = "blue")

ggplot() +
  ggplot2::geom_sf(data = world_coast) +
  ggplot2::geom_sf(data = trip_coord_info_map_data,
                       color = "blue")
# +
  # geom_sf_label(data = trip_coord_info_map_data,
  #               mapping = aes(label = label_lat_lon))
