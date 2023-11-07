# setup ----
library(mapview)
library(ggplot2)
library(ggmap)
library(leaflet)
library(tigris)
tigris_use_cache = TRUE

library(rnaturalearth) #coastline
library(knitr)
library(maps)
library(mapdata)
library(sf)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  # get_current_file_directory()
  file.path(my_paths$git_r, "geo_errors")

# get data ----
source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc(log = TRUE, quiet = TRUE)
# run_all_get_db_data(): 2.27 sec elapsed (from csv)

# View(all_get_db_data_result_l)

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols()

world_coast <-
  rnaturalearth::ne_coastline(returnclass = "sf")
# class(world_coast)

# statistics ----

# dim(trip_coord_info)
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
  dplyr::distinct() |>
  pivot_wider(names_from = stat_name,
              id_cols = Var2,
              values_from = sta_val) |>
  # dplyr::mutate_if(is.character, trimws) |>
  dplyr::mutate(field_name = trimws(Var2),
         .before = 1,
         .keep = "unused")

names(coordInfo_summ2) <- fix_names(names(coordInfo_summ2))

# print_df_names(coordInfo_summ2)

coordInfo_summ2_short <-
  coordInfo_summ2 |>
  filter(is.na(mode)) |>
  select(field_name, min, max, na_s) |>
  dplyr::distinct()

# View(coordInfo_summ2_short)

# coordInfo_summ2_short$field_name |> cat(sep = ", ")
# TRIP_ID, FISHING_HOURS, LATITUDE, LONGITUDE, AVG_DEPTH_IN_FATHOMS, E_DE, E_DC, DEPTH, MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH, FISHING_GEAR_DEPTH, DAYS_AT_SEA, T_DE, T_DC, VESSEL_ID, CF_PERMIT_ID, TRIP_START_DATE, TRIP_END_DATE, ACTIVITY_TYPE, SERO_VESSEL_PERMIT

weird_stats <-
  coordInfo_summ2_short |>
  filter(field_name %in%
           c("FISHING_HOURS", "LATITUDE", "LONGITUDE", "TRIP_END_DATE"))

head(weird_stats) |>
    kable(caption = "weird_stats")

#   field_name    min                        max                        na_s
# 1 FISHING_HOURS "0.000  "                  "4920.640  "                NA
# 2 LATITUDE      "-87.30  "                 "90.00  "                  "580  "
# 3 LONGITUDE     "-117.25  "                "137.59  "                 "580  "
# 4 TRIP_END_DATE "1969-08-17 00:00:00.00  " "2023-09-19 00:00:00.00  "  NA

# r prepare vendor columns ----
# t_UE, E_UE
# trip_coord_info |>
#   filter(!trimws(T_UE) == trimws(E_UE)) |>
#   glimpse()
# Rows: 119,735

trip_coord_info_vendors <-
  trip_coord_info |>
  dplyr::group_by(LATITUDE, LONGITUDE) |>
  # dplyr::mutate(all_permits = toString(unique(TOP))) |>
  dplyr::mutate(vendor_trip = toString(unique(T_UE)),
         vendor_effort = toString(unique(E_UE))) |>
  dplyr::ungroup()

# trip_coord_info_vendors |>
#   select(vendor_trip, vendor_effort) |>
#   dplyr::distinct() |>
#   dim()
# [1] 971   2

trip_coord_info_trip_vendors_cnt <-
  trip_coord_info_vendors |>
  select(vendor_trip) |>
  dplyr::mutate(vendor_trip = trimws(tolower(vendor_trip))) |>
  add_count(vendor_trip) |>
  dplyr::distinct()

# dim(trip_coord_info_trip_vendors_cnt)
# 678

trip_coord_info_trip_vendors_cnt |>
  dplyr::arrange(desc(n)) |>
  head() |>
  kable(caption = "trip_coord_info_trip_vendors_cnt")

# vesl	77275
# vms	15160
# ray rosher	1032
# captgreg9982	653

trip_coord_info_effort_vendors_cnt <-
  trip_coord_info_vendors |>
  select(vendor_effort) |>
  dplyr::mutate(vendor_effort = trimws(tolower(vendor_effort))) |>
  add_count(vendor_effort) |>
  dplyr::distinct()

trip_coord_info_effort_vendors_cnt |>
  dplyr::arrange(desc(n)) |>
  head() |>
  kable(caption = "trip_coord_info_effort_vendors_cnt")
# safis	119734
# vms	15160
# vesl	1880
# fishizzle75	283

#dim [1] 263   2

tic("trip_coord_info_vendors3_trip")
trip_coord_info_vendors3_trip <-
  trip_coord_info |>
  dplyr::group_by(LATITUDE, LONGITUDE) |>
  dplyr::mutate(vendor_trip_cat = case_when(
    trimws(tolower(T_UE)) == "vms" ~ "vms",
    trimws(tolower(T_UE)) %in% c("vesl", "bluefin") ~ "vesl",
    .default = "etrips"
  )) |>
  dplyr::ungroup()
toc(log = TRUE, quiet = TRUE)

# r map functions ----
crs4326 <- 4326
my_crs <- crs4326

lat_long_to_map <-
  function(my_df, my_title = "my_title", legend = TRUE,
           zcol_name = "") {

    my_sf <-
      my_df %>%
      # save info to show on the map
      dplyr::mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
      # convert to sf
      # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
      sf::st_as_sf(coords = c("LONGITUDE",
                              "LATITUDE"),
                   crs = my_crs)

    if (length(zcol_name) > 0) {
      my_map <-
        my_sf |>
        mapview::mapview(
          col.regions = viridisLite::turbo,
          layer.name = my_title,
          legend = legend,
          zcol = zcol_name
        )

    } else {
      my_map <-
        my_sf |>
        mapview::mapview(
          col.regions = viridisLite::turbo,
          layer.name = my_title,
          legend = legend
        )

    }

    return(my_map)
  }

world_coast <-
  rnaturalearth::ne_coastline(returnclass = 'sf')

lat_long_to_map_plot <-
  function(my_df,
           my_title = "my_title",
           coast_map = world_coast,
           label_column = NA) {
    # browser()

    my_crs <- sf::st_crs(coast_map)
    my_df_sf <-
      my_df %>%
      # save info to show on the map
      dplyr::mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
      # convert to sf
      # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
      sf::st_as_sf(coords = c("LONGITUDE",
                              "LATITUDE"),
                   crs = my_crs)

    map_plot <-
      ggplot() +
      ggplot2::geom_sf(data = coast_map) +
      ggplot2::geom_sf(data = my_df_sf,
                       color = "blue") +
      ggplot2::ggtitle(my_title)

    if (!is.na(label_column)) {
      map_plot <-
        map_plot +
        ggplot2::geom_sf_text(data = my_df_sf,
                              aes(label = !!sym(label_column)))
    }

    return(map_plot)

  }

crop_by_shape <-
  function(my_sf, shape) {
    my_sf |>
      sf::st_join(shape, left = FALSE) %>%
      dplyr::mutate(
        LONGITUDE = sf::st_coordinates(.)[, 1],
        LATITUDE = sf::st_coordinates(.)[, 2]
      ) %>%
      return()
  }

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

red_bounding_box <-
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
# get land map ----
ne_10m_land_sf <-
  sf::read_sf(r"(my_inputs\shapefiles\ne_10m_land\ne_10m_land.shp)")

sf::st_geometry(ne_10m_land_sf)
# Geodetic CRS:  WGS 84
# Geometry type: MULTIPOLYGON

ne_10m_land_sf_bb <-
  sf::st_crop(ne_10m_land_sf,
              big_bounding_box)
# plot(ne_10m_land_sf_bb)

# get ocean map ne ----
ne_10m_ocean_sf <-
  sf::read_sf(r"(my_inputs\shapefiles\ne_10m_ocean\ne_10m_ocean.shp)")

ne_10m_ocean_sf_bb <-
  sf::st_crop(ne_10m_ocean_sf,
              big_bounding_box)

# sf::st_crs(ne_10m_ocean_sf_bb)
    # ID["EPSG",4326]]

# Import country data ----
country <- ne_countries(scale = "medium", returnclass = "sf")

# r map all available points ----
trip_coord_info_map_data <-
  trip_coord_info |>
  select(LONGITUDE, LATITUDE) |>
  dplyr::distinct() |>
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  dplyr::mutate(label_lat_lon = paste(round(LATITUDE, 0),
                               round(LONGITUDE, 0)))
  # dim()
  # [1] 116246      3

trip_coord_info_plot <-
  lat_long_to_map_plot(trip_coord_info_map_data,
                       my_title = "All coordinates")
trip_coord_info_plot

# r count vendors ----

trip_coord_info_vendors3 <-
  trip_coord_info_vendors3_trip |>
  dplyr::mutate(year_start = year(TRIP_START_DATE))

trip_coord_info_vendors3 |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  count(vendor_trip_cat) |>
  kable(caption = "ALL: count(vendor_trip_cat)")

trip_coord_info_vendors3 |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat, year_start) |>
  count(vendor_trip_cat, year_start) |>
  kable(caption = "ALL: count(vendor_trip_cat, year_start)")

# all vendors
# etrips	47731
# vesl	77352
# vms	16267

# 1) a sign ----
## positive_long ----
positive_long <-
  trip_coord_info_vendors3 %>%
  filter(LONGITUDE > 0)

# dim(positive_long)
# [1] 15418    41

#     LATITUDE        LONGITUDE
# Min.   :-87.30   Min.   :-117.25
# 1st Qu.: 26.10   1st Qu.: -83.57
# Median : 29.00   Median : -81.01
# Mean   : 28.88   Mean   : -50.88
# 3rd Qu.: 30.16   3rd Qu.: -75.85
# Max.   : 90.00   Max.   : 137.59
# NA's   :1277     NA's   :1277

positive_long_map <-
  positive_long |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  lat_long_to_map_plot(my_title = 'Positive longitude')

# label_column = "vendor_trip_cat"

positive_long_map

## r Positive longitude, corrected ----

positive_long_corrected_map <-
  positive_long |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) |>
  lat_long_to_map_plot(my_title = 'Positive longitude, corrected')

# label_column = "vendor_trip_cat"

positive_long_corrected_map +
  red_bounding_box

### r positive longitude table ----

positive_long |>
    select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  count(vendor_trip_cat) |>
  dplyr::distinct() |>
  kable(caption = "positive_long: vendor_trip_cat")

# count vendor_trip_cat
# etrips	89
# vesl	15329

# count(vendor_trip_cat, year_start)
positive_long |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat, year_start) |>
  count(vendor_trip_cat, year_start) |>
  dplyr::distinct() |>
  kable(caption = "positive_long: count(vendor_trip_cat, year_start)")

### r positive longitude VESL only ----

positive_longitude_report <- function() {

  positive_long_vesl <-
    trip_coord_info_vendors3 |>
    filter(LONGITUDE > 0) |>
    filter(vendor_trip_cat == "vesl")

  # positive_long_vesl_map <-
  #   positive_long_vesl |>
  #   select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  #   dplyr::distinct() |>
  #   lat_long_to_map_plot(my_title = 'Positive longitude (VESL)')
  #
  # ggsave("positive_long_vesl_map.png",
  #   plot = positive_long_vesl_map)
  #
  # write_csv(positive_long_vesl, "positive_long_vesl.csv")

  return(positive_long_vesl)
}

positive_long_vesl <- positive_longitude_report()

# r count vessels with bad vesl coords ----
positive_long_vesl_ll <-
  positive_long_vesl |>
    select(LATITUDE, LONGITUDE) |>
    dplyr::distinct()

dim(positive_long_vesl_ll)
# [1] 4517    2

# how many positive lon entries for each vsl ----
positive_long_vesl_cnts <-
  positive_long_vesl |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  add_count(VESSEL_ID, name = "vesl_bad_vsl_cnt") |>
  dplyr::distinct() |>
  dplyr::arrange(desc(vesl_bad_vsl_cnt))

# cnt all entries for the same vessels ----
all_vesl_cnts <-
  trip_coord_info |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  filter(VESSEL_ID %in% positive_long_vesl_cnts$VESSEL_ID) |>
  add_count(VESSEL_ID, name = "all_bad_vsl_cnt") |>
  dplyr::distinct() |>
  dplyr::arrange(desc(all_bad_vsl_cnt))

positive_long_vesl_cnts_short <-
  positive_long_vesl_cnts |>
  select(VESSEL_ID, vesl_bad_vsl_cnt) |>
  dplyr::distinct()

all_vesl_cnts_short <-
  all_vesl_cnts |>
  select(VESSEL_ID, all_bad_vsl_cnt) |>
  dplyr::distinct()

# join both cnts ----
join_vesl_cnts <-
  left_join(all_vesl_cnts_short,
            positive_long_vesl_cnts_short)

dim(join_vesl_cnts)
  # [1] 336   3

join_vesl_cnts |>
  filter(vesl_bad_vsl_cnt > 3) |>
  dim()
# [1] 231   3

# check the difference between total and positive lon cnts ----
join_vesl_cnts_no_diff_all <-
  join_vesl_cnts |>
  dplyr::mutate(cnt_diff = abs(all_bad_vsl_cnt - vesl_bad_vsl_cnt))

join_vesl_cnts_no_diff <-
 join_vesl_cnts_no_diff_all |>
  filter(cnt_diff < 3)
glimpse(join_vesl_cnts_no_diff)
# 161

ggplot(join_vesl_cnts,
       aes(factor(all_bad_vsl_cnt),
           vesl_bad_vsl_cnt)) +
  geom_boxplot() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "lightblue",
    aes(group = 1)
  ) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    color = "purple",
    aes(group = 1)
  ) +

  labs(title = "Positive lon entries vs total entries, by vessel",
       x = "")
# +
#   theme(# turn x text
#     axis.text.x = element_text(angle = 45))

# find the model ----
sim1_mod <- lm(vesl_bad_vsl_cnt ~ all_bad_vsl_cnt, data = join_vesl_cnts)
best <- coef(sim1_mod)
# (Intercept) vesl_bad_vsl_cnt
# 42.9004745        0.7912089
# best[[1]]
# best <- optim(c(0, 0), measure_distance, data = sim1)
# best$par
#> [1] 4.222248 2.051204

ggplot(join_vesl_cnts,
       aes(all_bad_vsl_cnt,
           vesl_bad_vsl_cnt)) +
  geom_point(size = 2, colour = "grey30") +
  geom_abline(intercept = best[[1]],
              slope = best[[2]],
              color = "green")

#  is bad depends on total? ----
# https://statsandr.com/blog/chi-square-test-of-independence-in-r/
ggplot(join_vesl_cnts) +
  aes(x = all_bad_vsl_cnt,
      fill = factor(vesl_bad_vsl_cnt)) +
  geom_bar() +
  theme(legend.position = "none")

# plot the difference ----
join_vesl_cnts_no_diff_all |>
  dplyr::arrange(cnt_diff) |>
  glimpse()

ggplot(join_vesl_cnts_no_diff_all) +
  aes(x = cnt_diff) +
  geom_bar() +
  theme(legend.position = "none")

# get the distribution of cnt differences ----
join_vesl_cnts_no_diff_all_freq <-
  join_vesl_cnts_no_diff_all |>
  dplyr::group_by(cnt_diff) %>%
  dplyr::mutate(cnt_diff_freq = n()) %>%
  dplyr::ungroup() |>
  dplyr::arrange(desc(cnt_diff_freq))

join_vesl_cnts_no_diff_all_freq |>
  select(cnt_diff, cnt_diff_freq) |>
  dplyr::distinct() |>
  glimpse()
# Rows: 99
# cnt_diff      <int> 0, 1, 2, 3, 4, 7, 16, 10, 9, 6, 12, 47, 34,…
# $ cnt_diff_freq <int> 124, 23, 14, 9, 7, 6, 5, 4, 4, 4, 4, 3, 3,

# summary(join_vesl_cnts_no_diff_all_freq)

# result: 124 vessels have all they reported coordinates wrong (vesl, positive longitude)

# test independence ----
test <-
  chisq.test(
    join_vesl_cnts_no_diff_all$all_bad_vsl_cnt,
    join_vesl_cnts_no_diff_all$vesl_bad_vsl_cnt
  )
test
# X-squared = 24982, df = 18975, p-value < 0.00000000000000022
test$observed |> glimpse()
test$expected |> glimpse()

test1 <-
  stats::chisq.test(
    join_vesl_cnts_no_diff_all$vesl_bad_vsl_cnt,
    join_vesl_cnts_no_diff_all$all_bad_vsl_cnt
  )
# Warning message:
# In stats::chisq.test(join_vesl_cnts_no_diff_all$vesl_bad_vsl_cnt,  :
#   Chi-squared approximation may be incorrect
test1
# same as test




# plot all_bad_vsl_cnt vs. vesl_bad_vsl_cnt ----
ggplot(join_vesl_cnts,
       aes(x = all_bad_vsl_cnt,
           y = vesl_bad_vsl_cnt)) +
  # geom_col(position = "dodge")
  geom_point()

# plot how many vessels have positive long ----

## r convert join_vesl_cnts_no_diff_ll coords ----

join_vesl_cnts_no_diff_ll <-
  trip_coord_info |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  filter(VESSEL_ID %in% join_vesl_cnts_no_diff$VESSEL_ID) |>
  dplyr::distinct()

# write_csv(join_vesl_cnts_no_diff_ll,
#           "join_vesl_cnts_no_diff_ll.csv")

join_vesl_cnts_no_diff_ll |>
  # head(20) |>
  glimpse()

vesl_no_diff_bounding_box <-
  geom_rect(
    aes(
      xmin = min(join_vesl_cnts_no_diff_ll$LONGITUDE),
      xmax = max(join_vesl_cnts_no_diff_ll$LONGITUDE),
      ymin = min(join_vesl_cnts_no_diff_ll$LATITUDE),
      ymax = max(join_vesl_cnts_no_diff_ll$LATITUDE)
    ),
    color = "purple",
    fill = NA
  )

join_vesl_cnts_no_diff_ll_sf <-
  join_vesl_cnts_no_diff_ll |>
    sf::st_as_sf(coords = c("LONGITUDE",
                          "LATITUDE"),
               crs = my_crs,
               remove = FALSE)

join_vesl_cnts_no_diff_ll_sf_l <-
  join_vesl_cnts_no_diff_ll_sf |>
  dplyr::mutate(labl = paste(LATITUDE, LONGITUDE))

glimpse(join_vesl_cnts_no_diff_ll_sf_l)

## r map join_vesl_cnts_no_diff_ll ----

join_vesl_cnts_no_diff_ll_sf_l |>
  leaflet() %>%
  addTiles() %>%
  addMarkers(
    label = paste(
      join_vesl_cnts_no_diff_ll_sf_l$LATITUDE,
      join_vesl_cnts_no_diff_ll_sf_l$LONGITUDE,
      sep = ", "
    ),
    # lat_long_area_clean$POINT,
    # labelOptions = labelOptions(noHide = T),
    clusterOptions = markerClusterOptions()
  )

map_plot <-
  ggplot() +
  ggplot2::geom_sf(data = world_coast) +
  vesl_no_diff_bounding_box +
  ggplot2::ggtitle("join_vesl_cnts_no_diff_ll")

# map_plot

# join_vesl_cnts_no_diff_ll_sf

# get vessel ids for "all entries are wrong" ----
join_vesl_cnts_no_diff_all_wrong <-
  join_vesl_cnts_no_diff_all |>
  filter(cnt_diff == 0)

glimpse(join_vesl_cnts_no_diff_all_wrong)
# $ VESSEL_ID        <int> 295040, 249248, 329371, 169199, 84633, 172960, 170137,…
# $ all_bad_vsl_cnt  <int> 514, 235, 227, 214, 199, 193, 186, 183, 179, 175, 173,…

# one vessel coords ----
coords_295040 <-
  positive_long_vesl |>
  filter(VESSEL_ID == "295040")

View(coords_295040)

coords_295040$E_DE |>
  format(format = '%m/%d/%Y') |>
  unique() |>
  glimpse()
 # chr [1:28] "07/02/2022" "03/14/2022" "04/15/2022" "05/02/2022" "05/04/2022" ...


join_vesl_cnts_no_diff_all_wrong_vsls <-
  positive_long_vesl |>
  filter(VESSEL_ID %in% join_vesl_cnts_no_diff_all_wrong$VESSEL_ID)
dim(join_vesl_cnts_no_diff_all_wrong_vsls)
# [1] 8996   41

join_vesl_cnts_no_diff_all_wrong_vsls_short <-
  join_vesl_cnts_no_diff_all_wrong_vsls |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  dplyr::distinct()
# A tibble: 3,512 × 3

# join_vesl_cnts_no_diff_all_wrong_vsls_short lon to negative ----
join_vesl_cnts_no_diff_all_wrong_vsls_short_fix <-
  join_vesl_cnts_no_diff_all_wrong_vsls_short |>
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE))

# check vessel_ids ----

## plot join_vesl_cnts_no_diff_all_wrong_vsls ----

join_vesl_cnts_no_diff_all_wrong_vsls_short_sf <-
  join_vesl_cnts_no_diff_all_wrong_vsls_short |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = crs4326)

positive_lon_points_map <-
  mapview(join_vesl_cnts_no_diff_all_wrong_vsls_short_sf,
        zcol = "VESSEL_ID")

data_overview(join_vesl_cnts_no_diff_all_wrong_vsls_short)
 #    LATITUDE        LONGITUDE        VESSEL_ID
 # Min.   :-83.98   Min.   :  1.47   Min.   : 84633
 # 1st Qu.: 27.14   1st Qu.: 81.44   1st Qu.:247428
 # Median : 29.48   Median : 85.51   Median :326607
 # Mean   : 27.79   Mean   : 74.19   Mean   :291283
 # 3rd Qu.: 30.08   3rd Qu.: 86.35   3rd Qu.:329344
 # Max.   : 88.00   Max.   :100.00   Max.   :398142

join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf <-
  join_vesl_cnts_no_diff_all_wrong_vsls_short_fix |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = crs4326)

positive_lon_points_fix_map <-
  mapview(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf,
          zcol = "VESSEL_ID")

# mapview add the big box ----
big_box_map <- sf::st_bbox(
  c(
    xmin = big_bounding_box[["xmin"]],
    xmax = big_bounding_box[["xmax"]],
    ymin = big_bounding_box[["ymin"]],
    ymax = big_bounding_box[["ymax"]]
  ),
  crs = crs4326
)

# positive_lon_points_fix_map +
#   big_box_map


# all crs ----
# https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/
proj_db <- system.file("proj/proj.db", package = "sf")
  # For dynamically linked PROJ, provide the path to proj.db yourself:
  # if (proj_db == "") proj_db <- proj_db_path
crs_table <- sf::read_sf(proj_db, "crs_view") # extracts the "crs_view" table
subset(crs_table, grepl("Belg|Ostend", name) & auth_name == "EPSG")[2:5]

subset(crs_table, auth_name == "EPSG")[2:5]
# # A tibble: 7,344 × 4
#    auth_name code  name      type
#    <chr>     <chr> <chr>     <chr>
#  1 EPSG      3819  HD1909    geographic 2D
#  2 EPSG      3821  TWD67     geographic 2D

# check the vessels ----
vessel_ids_124 <-
  join_vesl_cnts_no_diff_all_wrong_vsls_short_fix$VESSEL_ID |>
  unique()

length(vessel_ids_124)
# 124

trip_coord_info |>
  filter(VESSEL_ID %in% vessel_ids_124) |>
  glimpse()
head(vessel_ids_124)

## get vessel o number  ----
vessel_permits_info <-
  all_get_db_data_result_l[["vessels_permits"]] |>
  remove_empty_cols()

vessel_permits_info_124 <-
  vessel_permits_info |>
  filter(VESSEL_VESSEL_ID %in% vessel_ids_124)

dim(vessel_permits_info_124)
# [1] 1878   50

data_overview(vessel_permits_info_124)
# PERMIT_VESSEL_ID      123

setdiff(vessel_ids_124,
        vessel_permits_info_124$VESSEL_VESSEL_ID)
# [1] 328366
# TODO: why there is a trip info, but not vessel/permit?

View(vessel_permits_info_124)

vessel_permits_info_124 |>
  select(SERO_HOME_PORT_STATE) |>
  dplyr::distinct() |>
  glimpse()

# Rows: 10
# $ SERO_HOME_PORT_STATE <chr> "FL", "AL", "NJ", "MS", "NC", "GA", "TX", "LA", …
# how many coords per vessel ----
vessels_124_coord_freq <-
  join_vesl_cnts_no_diff_all_wrong_vsls_short_fix |>
  add_count(VESSEL_ID, name = "coord_freq") |>
  dplyr::arrange(desc(coord_freq)) |> glimpse()
# 124
# $ VESSEL_ID <int> 249248, 329344, 169199, 328889, 170137, 248214, 329371, 247…
# $ n         <int> 188, 172, 169, 137, 134, 130, 126, 123, 118, 103, 103, 100,…

## add vessel o number ----

vessels_124_coord_freq_von <-
  vessel_permits_info_124 |>
  select(VESSEL_VESSEL_ID,
         PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  right_join(vessels_124_coord_freq,
             join_by(VESSEL_VESSEL_ID == VESSEL_ID),
             relationship = "many-to-many")

# View(vessels_124_coord_freq_von)

### map vessels_124_coord_freq_von ----
# TODO: add total trip counts
vessels_124_coord_freq_von_sf <-
  vessels_124_coord_freq_von |>
  sf::st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
                 crs = crs4326)


vessels_124_coord_freq_von_sf |>
  mapview(zcol = "coord_freq")

sf::st_geometry(vessels_124_coord_freq_von_sf)

# TODO: histogram err freq by vessel
# separate correctable positive lon ----

join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf_in_box <-
  sf::st_crop(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf, big_bounding_box)

mapview(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf_in_box)

on_land_pos_lon_fixed <-
  crop_by_shape(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf_in_box,
                ne_10m_land_sf_bb)

# sf::st_crs(ne_10m_land_sf_bb)
#     ID["EPSG",4326]]

dim(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf_in_box)
mapview(on_land_pos_lon_fixed)

## at see ----

at_see_pos_lon_fixed <-
  crop_by_shape(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix_sf_in_box,
                ne_10m_ocean_sf_bb)

# mapview(at_see_pos_lon_fixed)
str(at_see_pos_lon_fixed)
# sf [2,598 × 7] (S3: sf/tbl_df/tbl/data.frame)

corrected_coords_good <-
  sf::st_drop_geometry(at_see_pos_lon_fixed) |>
  select(VESSEL_ID, LONGITUDE, LATITUDE)

corrected_bad <-
  dplyr::setdiff(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix,
          corrected_coords_good)

dim(join_vesl_cnts_no_diff_all_wrong_vsls_short_fix)
# [1] 3512    3

dim(corrected_coords_good)
# [1] 2598    3

dim(corrected_bad)
# [1] 914   3

# correct

both_bad_and_good_vsls <-
  dplyr::intersect(corrected_bad$VESSEL_ID,
            corrected_coords_good$VESSEL_ID)

length(both_bad_and_good_vsls)
# 64

dplyr::setdiff(corrected_bad$VESSEL_ID,
            corrected_coords_good$VESSEL_ID) |>
  length()
# 13

corrected_coords_good_only_id <-
  dplyr::setdiff(corrected_coords_good$VESSEL_ID,
               corrected_bad$VESSEL_ID)

length(corrected_coords_good_only_id)
# 47

both_bad_and_good_vsls_p_v_info <-
  vessel_permits_info |>
  filter(VESSEL_VESSEL_ID %in% both_bad_and_good_vsls)

both_bad_and_good_vsls_p_v_ids <-
  both_bad_and_good_vsls_p_v_info |>
  select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  dplyr::distinct()

## get coords for both_bad_and_good_vsls ----
# print_df_names(both_bad_and_good_vsls_p_v_ids_all_info)
# [1] "PERMIT_VESSEL_ID, VESSEL_ID, LATITUDE, LONGITUDE"

both_bad_and_good_vsls_p_v_ids_all_info <-
  trip_coord_info |>
  right_join(both_bad_and_good_vsls_p_v_ids,
             join_by(VESSEL_ID ==
                       VESSEL_VESSEL_ID)) |>
  select(PERMIT_VESSEL_ID, VESSEL_ID,
         LATITUDE, LONGITUDE)

glimpse(both_bad_and_good_vsls_p_v_ids_all_info)
# [1] 5672    4

both_bad_and_good_vsls_p_v_ids_all_info_sf <-
  both_bad_and_good_vsls_p_v_ids_all_info |>
  sf::st_as_sf(coords = c("LONGITUDE",
                          "LATITUDE"),
               crs = crs4326)


# lat_long_to_map(both_bad_and_good_vsls_p_v_ids_all_info)

mapview(both_bad_and_good_vsls_p_v_ids_all_info_sf,
        zcol = "PERMIT_VESSEL_ID",
        legend = FALSE)

# pull all info by vessel_ids ----
info_by_vsl_ids <- function(vsl_id_lists) {

  p_v_info <-
    vessel_permits_info |>
    filter(VESSEL_VESSEL_ID %in% vsl_id_lists)

  p_v_ids <-
    p_v_info |>
    select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
    dplyr::distinct()

  # get coords for both_bad_and_good_vsls

  p_v_ids_all_info <-
    trip_coord_info |>
    right_join(p_v_ids,
               join_by(VESSEL_ID ==
                         VESSEL_VESSEL_ID)) |>
    select(PERMIT_VESSEL_ID, VESSEL_ID,
           LATITUDE, LONGITUDE)

  return(p_v_ids_all_info)
}

# map good ----
corrected_coords_good_only_id_all_info <-
  info_by_vsl_ids(corrected_coords_good_only_id) |>
  add_count(PERMIT_VESSEL_ID, VESSEL_ID,
            name = "coord_by_vsl_cnt") |>
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE))

# lat_long_to_map(corrected_coords_good_only_id_all_info,
#                 zcol_name = "PERMIT_VESSEL_ID")

beatCol <-
  colorFactor(palette = 'plasma',
              factor(corrected_coords_good_only_id_all_info$PERMIT_VESSEL_ID))

# radius = ~sqrt(BeatHome*50)
dim(corrected_coords_good_only_id_all_info)
# [1] 2361    5

corrected_coords_good_only_id_all_info_u <-
  corrected_coords_good_only_id_all_info |>
  dplyr::distinct()

dim(corrected_coords_good_only_id_all_info_u)
# [1] 417   5

good_fix_map <-
  leaflet(data = corrected_coords_good_only_id_all_info_u) %>%
  addTiles() %>%
  addCircleMarkers(
    data = corrected_coords_good_only_id_all_info_u,
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    label = ~ paste(PERMIT_VESSEL_ID, coord_by_vsl_cnt,
                     sep = ": "),
    color = ~ beatCol(PERMIT_VESSEL_ID),
    radius = ~ coord_by_vsl_cnt * 0.05,
    markerClusterOptions()
    # ,
    # labelOptions = labelOptions(noHide = T)
  )

# good_fix_map

# report ----
  # good, bad, both
  # vsls, trips, locations, owners

