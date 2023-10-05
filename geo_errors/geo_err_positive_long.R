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
  group_by(LATITUDE, LONGITUDE) |>
  # mutate(all_permits = toString(unique(TOP))) |>
  mutate(vendor_trip = toString(unique(T_UE)),
         vendor_effort = toString(unique(E_UE))) |>
  ungroup()

# trip_coord_info_vendors |>
#   select(vendor_trip, vendor_effort) |>
#   distinct() |>
#   dim()
# [1] 971   2

trip_coord_info_trip_vendors_cnt <-
  trip_coord_info_vendors |>
  select(vendor_trip) |>
  mutate(vendor_trip = trimws(tolower(vendor_trip))) |>
  add_count(vendor_trip) |>
  distinct()

# dim(trip_coord_info_trip_vendors_cnt)
# 678

trip_coord_info_trip_vendors_cnt |>
  arrange(desc(n)) |>
  head() |>
  kable(caption = "trip_coord_info_trip_vendors_cnt")

# vesl	77275
# vms	15160
# ray rosher	1032
# captgreg9982	653

trip_coord_info_effort_vendors_cnt <-
  trip_coord_info_vendors |>
  select(vendor_effort) |>
  mutate(vendor_effort = trimws(tolower(vendor_effort))) |>
  add_count(vendor_effort) |>
  distinct()

trip_coord_info_effort_vendors_cnt |>
  arrange(desc(n)) |>
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
  group_by(LATITUDE, LONGITUDE) |>
  mutate(vendor_trip_cat = case_when(
    trimws(tolower(T_UE)) == "vms" ~ "vms",
    trimws(tolower(T_UE)) %in% c("vesl", "bluefin") ~ "vesl",
    .default = "etrips"
  )) |>
  ungroup()
toc(log = TRUE, quiet = TRUE)

# r map functions ----
lat_long_to_map <-
  function(my_df, my_title = "my_title", legend = TRUE) {
  my_df %>%
    # save info to show on the map
    dplyr::mutate(point = paste(LATITUDE, LONGITUDE, sep = ", ")) %>%
    # convert to sf
    # an sf object is a collection of simple features that includes attributes and geometries in the form of a data frame.
    sf::st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
                 crs = my_crs) %>%
    mapview::mapview(
      col.regions = viridisLite::turbo,
      layer.name = my_title,
      legend = legend
    ) %>% return()
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

# r map all available points ----
trip_coord_info_map_data <-
  trip_coord_info |>
  select(LONGITUDE, LATITUDE) |>
  distinct() |>
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  mutate(label_lat_lon = paste(round(LATITUDE, 0),
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
  mutate(year_start = year(TRIP_START_DATE))

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
  mutate(LONGITUDE = -abs(LONGITUDE)) |>
  lat_long_to_map_plot(my_title = 'Positive longitude, corrected')

# label_column = "vendor_trip_cat"

positive_long_corrected_map +
  red_bounding_box

### r positive longitude table ----

positive_long |>
    select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  count(vendor_trip_cat) |>
  distinct() |>
  kable(caption = "positive_long: vendor_trip_cat")

# count vendor_trip_cat
# etrips	89
# vesl	15329

# count(vendor_trip_cat, year_start)
positive_long |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat, year_start) |>
  count(vendor_trip_cat, year_start) |>
  distinct() |>
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
  #   distinct() |>
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
    distinct()

dim(positive_long_vesl_ll)
# [1] 4517    2

positive_long_vesl_cnts <-
  positive_long_vesl |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  add_count(VESSEL_ID, name = "vesl_bad_vsl_cnt") |>
  distinct() |>
  arrange(desc(vesl_bad_vsl_cnt))

all_vesl_cnts <-
  trip_coord_info |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  filter(VESSEL_ID %in% positive_long_vesl_cnts$VESSEL_ID) |>
  add_count(VESSEL_ID, name = "all_bad_vsl_cnt") |>
  distinct() |>
  arrange(desc(all_bad_vsl_cnt))

positive_long_vesl_cnts_short <-
  positive_long_vesl_cnts |>
  select(VESSEL_ID, vesl_bad_vsl_cnt) |>
  distinct()

all_vesl_cnts_short <-
  all_vesl_cnts |>
  select(VESSEL_ID, all_bad_vsl_cnt) |>
  distinct()

join_vesl_cnts <-
  left_join(all_vesl_cnts_short,
            positive_long_vesl_cnts_short)

dim(join_vesl_cnts)
  # [1] 336   3

join_vesl_cnts |>
  filter(vesl_bad_vsl_cnt > 3) |>
  dim()
# [1] 231   3

join_vesl_cnts_no_diff <-
  join_vesl_cnts |>
  mutate(cnt_diff = abs(all_bad_vsl_cnt - vesl_bad_vsl_cnt)) |>
  filter(cnt_diff < 3)
glimpse(join_vesl_cnts_no_diff)
# 161

## r convert join_vesl_cnts_no_diff_ll coords ----

join_vesl_cnts_no_diff_ll <-
  trip_coord_info |>
  select(LATITUDE, LONGITUDE, VESSEL_ID) |>
  filter(VESSEL_ID %in% join_vesl_cnts_no_diff$VESSEL_ID) |>
  distinct()

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
  mutate(labl = paste(LATITUDE, LONGITUDE))

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

map_plot

# join_vesl_cnts_no_diff_ll_sf

