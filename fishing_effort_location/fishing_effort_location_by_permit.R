# Requirements ----
# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally.
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)
# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.
# fields to get
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# - Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth
# south of 28N - all SA
# OK boundaries
# lat 23 : 28
# lon -71 : -83

# north of 28N - EEZ only

# setup ----

library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# library(htmlwidgets) # add js script to leaflets
library(stringi) # add characters
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

# convert to sf shortcut
my_to_sf <- function(my_df) {
  my_df %>%
    # convert to sf
    sf::st_as_sf(
      # field names to use
      coords = c("LONGITUDE",
                 "LATITUDE"),
      # use crs from sa_shp
      crs = sf::st_crs(sa_shp),
      # keep LAT/LONG, to save in a file
      remove = FALSE
    ) %>%
    return()
}

# to avoid this error:
#   Loop 0 is not valid: Edge 57478 has duplicate vertex with edge 57482
sf::sf_use_s2(FALSE)

# run st_intersection with benchmark
with_st_intersection <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_intersection(", par1, ", ", par2, ")"))
  res <- sf::st_intersection(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)

  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

## functions for ten_min ----

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <-
    paste(deg, stringi::stri_pad_left(ten_min_num, 2, 0), sep = "")
  convert_to_decimal_degree(dm_num)
}

get_ten_min_coords <- function(my_df) {
  ten_min_df <-
    my_df |>
    mutate(
      ten_min_lat = get_lat_ten_min(as.numeric(my_df$LATITUDE)),
      # All lon should be negative, bc we know it is all in America
      ten_min_lon =
        -1 * abs(get_lat_ten_min(as.numeric(my_df$LONGITUDE)))
    )
  return(ten_min_df)
  # distinct(ten_min_df)
}

## From FHIER ----
# View(safis_efforts_extended_2022_short)

safis_efforts_extended_2022_short_good <-
  safis_efforts_extended_2022_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_2022_short)
# [1] 97970    17

dim(safis_efforts_extended_2022_short_good)
# [1] 97547    17

### convert to sf from FHIER ----
safis_efforts_extended_2022_short_good_sf <-
  my_to_sf(safis_efforts_extended_2022_short_good)

# show all boundaries ----

# subset by Big box ----
# Michelle: I think we need to allow trips that occur anywhere in the GOM, with the eastern lat border being like a big line down the Atlantic Ocean at Bermuda. Does that make sense? Southern Border could be at Cuba. The Northern Border needs to extend up through Maine - since we require reporting no matter where they fish. Basically just a big box, regardless of Council jurisdiction.
# Jessica: I like the big box without council jurisdiction and then I am going to assume we will just plot those trips for vessels with GOM permits?  This should show the Council how many GOM vessels also fish in other regions as well as where they are fishing in the Gulf.

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

tic("safis_efforts_extended_2022_short_good_sf_crop_big")
safis_efforts_extended_2022_short_good_sf_crop_big <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          big_bounding_box)
toc()
# safis_efforts_extended_2022_short_good_sf_crop_big: 0.89 sec elapsed

dim(safis_efforts_extended_2022_short_good_sf_crop_big)
# [1] 95720    18

# convert back to df ----
safis_efforts_extended_2022_short_good_sf_crop_big_df <-
  safis_efforts_extended_2022_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df)
# [1] 95720     17

# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_2022_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks)
# [1] 93581    17

# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  filter(!VESSEL_NAME == vessel_name) |>
  select(VESSEL_OFFICIAL_NBR, VESSEL_NAME, vessel_name) |>
  distinct() |>
  head()
#   <chr>               <chr>        <chr>
# 1 1212782             NO DOUBT     "NO DOUBT 2"
# 2 614579              L & H        "L "
# 3 FL2570PG            C&D BOATS 09 "C"
# 4 FL3143RA            F-N-OFF II   "F"
# 5 FL0334RY            REEL-AXING   "REEL"
# 6 1162015             REEL-ALITY   "REEL"

### shorten ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom |>
  select(
    -c(
      VESSEL_NAME,
      TRIP_START_DATE,
      EFFORT_SEQ,
      AREA_CODE,
      SUB_AREA_CODE,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_REGION,
      AREA_STATE,
      DISTANCE_CODE,
      DISTANCE_NAME,
      LOCAL_AREA_CODE,
      LOCAL_AREA_NAME,
      permit_code,
      permit_num,
      reqmit_id,
      type,
      request_type,
      status,
      vessel_name,
      status_date,
      effective_date,
      expiration_date,
      end_date,
      term_date
    )
  ) |>
  distinct()

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
  mutate(
    permit_region =
      case_when(
        permit_sa_gom == "gom_only"
        | permit_sa_gom == "dual" ~
          "gom_dual",
        permit_sa_gom == "sa_only" ~
          "sa_only"
      )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(
    function(permit_df) {
      permit_df |>
      select(-c(LATITUDE, LONGITUDE)) |>
      count(ten_min_lat, ten_min_lon) |>
      arrange(desc(n)) |>
      head(2)
    }
  )
# $gom_dual
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        30.2       -87.5  2122
# 2        30.3       -86.5  1245
#
# sa_only
# # A tibble: 6 × 3
#   ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        24.8       -80.5  2863
# 2        24.5       -81.7  2701

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
  map(function(permit_df) {
    permit_df |>
      select(-c(permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts") |>
      group_by(ten_min_lat, ten_min_lon) |>
      mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
      ungroup()
  })

# View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare df ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual |>
  mutate(cnt_label =
           paste0("loc: ", location_cnts_u,
                  "; trips: ",  trip_ids_cnts)) |>
  mutate(
    ten_min_lbl =
      paste0(
        round(ten_min_lat, 1),
        ", ",
        round(ten_min_lon, 1),
        "; ",
        "trips: ",
        trip_ids_cnts,
        "; loc: ",
        location_cnts_u
      )
  )

dim(gom_vessels)
# [1] 1369    6

head(gom_vessels, 10)
  # ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
# 4        27.5       -83.2           121             119
# 5        27.8       -82.8           475             454
# 6        27.7       -82.7           839             562

# max(gom_vessels$location_cnts_u)
# [1] 1846

# max(gom_vessels$trip_ids_cnts)
# [1] 2122

dim(gom_vessels)
# [1] 1369    6

gom_vessels |>
  filter(gom_vessels$trip_ids_cnts > 2) |>
  dim()
# [1] 770   6

# example with lat long vs ten min ----
# ~Saint Petersburg
gom_vessels_example_3loc <-
  gom_vessels |>
  filter(trip_ids_cnts %in% c("475", "839", "961"))

short_example_3loc <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lat,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_example_3loc$ten_min_lon,
            digits = 1)
  )

dim(short_example_3loc)
# 740 8

short_example_3_cnts <-
  short_example_3loc |>
  dplyr::add_count(ten_min_lat, ten_min_lon,
                   name = "trip_ids_cnts") |>
  group_by(ten_min_lat, ten_min_lon) |>
  mutate(location_cnts_u = (n_distinct(LATITUDE, LONGITUDE))) |>
  ungroup()

short_example_3_cnts |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740

# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   10

glimpse(short_example_3_cnts)

short_example_3_cnts_short <-
  short_example_3_cnts |>
  select(-c(VESSEL_OFFICIAL_NBR,
            permit_sa_gom,
            permit_region,
            TRIP_ID)) |>
  distinct()

dim(short_example_3_cnts_short)
# [1] 564   5

short_example_3_cnts_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  distinct() |>
  arrange(trip_ids_cnts)
# ok
#   ten_min_lat ten_min_lon trip_ids_cnts location_cnts_u
#         <dbl>       <dbl>         <int>           <int>
# 1        27.8       -82.8           142             142
# 2        27.7       -82.7           279             211
# 3        27.8       -82.7           319             211

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             "; ",
             round(trip_ids_cnts, 1),
             " trps; ",
             round(location_cnts_u, 1),
             " loc"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    clusterOptions = markerClusterOptions()) |>
  addMarkers(
    short_example_3_cnts_short,
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    labelOptions = labelOptions(noHide = T)
  ) |>
  addGraticule(
    interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1)) |>
    # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
    setView(-82.75, 27.8, zoom = 11) |>
    addControl(rr, position = "bottomleft")

# uncomment to run
# map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\small_example\map_leaflet_short_example.html)")
# https://drive.google.com/file/d/1lO9a3nbH1g8AZu41yXdyCNHYhsCe58sZ/view?usp=drive_link

# all points ----
## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
    gom_clusters_shape_base <-
      leaflet(data = lat_lon_data) |>
      addTiles()
    return(gom_clusters_shape_base)
  }

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

# small test map
map_base_gom_vessels_15 <-
  gom_vessels |>
  head(15) |>
  image_with_clusters_base()

## markers for map_base_gom_vessels ----

marker_js_gom_vessels_green <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

cnts_sum_marker_js <- JS(
  "function(cluster) {
    var markers = cluster.getAllChildMarkers();
    var sum = 0;
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.location_cnts_u);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# Where are the data
# View(map_base_gom_vessels_15)
# environment(map_base_gom_vessels_15[["preRenderHook"]])[["data"]][["location_cnts_u"]]

# small working test ----
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ cnt_label,
    # label = ~ location_cnts_u,
    labelOptions = labelOptions(noHide = T),
    options =
      markerOptions(trip_ids_cnts = ~trip_ids_cnts,
                    location_cnts_u = ~location_cnts_u),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))


## texts on map ----
my_text_all_points <-
  "Numbers on green circles show an amount of unique locations in this cluster.</br>
On mouse hover it will show the clustered area.</br>
Blue circles are points on the ten minute grid.</br>
On mouse hover rectangular labels show coordinates on the ten minute grid,
# of trips and # of unique locations in this ten minute square."

# print_df_names(gom_vessels)
# [1] "ten_min_lat, ten_min_lon, trip_ids_cnts, location_cnts_u, cnt_label, ten_min_lbl"

my_text_all_points_html <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                                             my_text_all_points,
                                             '</span>')))

# font-size: 0.875em; /* 14px/16=0.875em */
my_title_all_points <-
  '<span style=font-size: LARGE>
Fishing locations rounded to ten minutes for GOM and dual permitted vessels in 2022</span><br>
<span style=font-size: small>
<strong>NB</strong>.
Not all trips has valid coordinates, hence not shown here</span>'

tag_map_title <- tags$style(HTML(
  ".leaflet-control.comment {
    font-size: small;
  }
  .leaflet-control.map-title {
    //transform: translate(-50%, 20%);
    //position: fixed !important;
    //left: 50%;
    //text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    //font-weight: bold;
    font-size: Large;
  }
"))

my_title_all_points_html <- tags$div(tag_map_title, HTML(my_title_all_points))

map_base_gom_vessels_w_markers <-
  map_base_gom_vessels |>
  addCircleMarkers(
    # get data from gom_vessels df
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    label = ~ ten_min_lbl,
    options =
      # put data from gom_vessels df in the options to use with JS
      pathOptions(
        trip_ids_cnts = ~ trip_ids_cnts,
        location_cnts_u = ~ location_cnts_u
      ),
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  fitBounds( ~ min(ten_min_lon),
             ~ min(ten_min_lat),
             ~ max(ten_min_lon),
             ~ max(ten_min_lat)) |>
  setView(
    lng = mean(gom_vessels$ten_min_lon),
    lat = mean(gom_vessels$ten_min_lat),
    zoom = 4
  ) |>
  addRectangles(
    lng1 = big_bounding_box[["xmin"]],
    lat1 = big_bounding_box[["ymin"]],
    lng2 = big_bounding_box[["xmax"]],
    lat2 = big_bounding_box[["ymax"]],
    fill = FALSE,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

# map_base_gom_vessels_w_markers

map_base_gom_vessels_w_markers_with_text <-
  map_base_gom_vessels_w_markers |>
  # add the explanation text at the bottom
  addControl(my_text_all_points_html,
             position = "bottomleft") |>
  # add title
  addControl(my_title_all_points_html,
             position = "topright") |>
  # big bounding box
  addPopups(big_bounding_box[["xmax"]],
            big_bounding_box[["ymax"]],
            "Allowed coordinates")

map_base_gom_vessels_w_markers_with_text

  # add ten min grid
  # addGraticule(interval = 1 / 60 * 10,
  #              style = list(color = "grey", weight = 1)) |>
  # flyToBounds(-82.9, 27.65, -82.6, 27.85) |>
  # setView(-82.75, 27.8, zoom = 11) |>
# addControl(my_title_all_points_html,
  #            position = "topright",
  #            className = "map-title")


# uncomment to run
# htmlwidgets::saveWidget(map_base_gom_vessels_w_markers,
#                         file =
#                           r"(my_outputs\fishing_effort_location\09_2023\map_leaflet_gom_permit_all.html)")

# big_bounding_box <- c(
#    xmin = -97.79954,
#    ymin = 21.521757, #Cuba
#    xmax = -64.790337, #Bermuda
#    ymax = 49 #Canada
#  )

# str(big_bounding_box["xmin"])


# Heatmap ----

# read in GOM trip ticket grid
GOMsf = read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>% summarise()
# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create GOM 5x5 minute grid
min_grid <-
  function(minute_num = 1) {
    grid <-
      sf::st_make_grid(x = st_bbox(GOMsf), cellsize = 1 / 60 * minute_num) %>%
      st_as_sf() %>%
      mutate(cell_id = 1:nrow(.))

    return(grid)
  }

grid <- min_grid(5)

st_agr(GOMsf) = st_agr(grid) = "constant"

### remove internal boundaries from the shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## heatmap data ----

# short_example_3_cnts_short |> glimpse()
glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual)

# glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual)

# for_heatmap_lat_lon_trips_only <-
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
#   select(TRIP_ID, LATITUDE, LONGITUDE) |>
#   distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

for_heatmap_lat_lon_trips_vessels_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

# dim(for_heatmap_lat_lon_trips_vessels_only)
# Rows: 41,455

#### assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips ####

## by n min grid ----
df_join_grid <-
  function(my_df) {
    my_df |>
      # join n minute grid
      st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(grid, join = st_nearest_feature) %>%
          return()
        }

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl")
effort_vsl <- df_join_grid(for_heatmap_lat_lon_trips_vessels_only)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

## crop by the shape ----
# effort_cropped <-
#   with_st_intersection(effort,
#           GOMsf)
# effort_cropped: 81.51 sec elapsed

tic("effort_vsl_cropped")
effort_vsl_cropped <-
  effort_vsl |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# effort_cropped2: 0.44 sec elapsed

# class(effort_cropped)

# print_df_names(effort_vsl_cropped)

## count trip ids and vessels by grid cell ----
effort_vsl_cropped_cnt2 <-
  effort_vsl_cropped |>
  # sf::st_drop_geometry() |>
  group_by(cell_id) |>
  mutate(vsl_cnt = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt = n_distinct(TRIP_ID)) |>
  ungroup()
# [1] 35822     8

# check
effort_vsl_cropped_cnt2 |>
  sf::st_drop_geometry() |>
  filter(cell_id == 1864) |>
  select(vsl_cnt, trip_id_cnt) |>
  distinct() |>
  glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

dim(effort_vsl_cropped_cnt2)
# [1] 35822     9

effort_cropped_short_cnt_rule3 <-
  effort_vsl_cropped_cnt2 |>
  filter(vsl_cnt > 2)

dim(effort_cropped_short_cnt_rule3)
# [1] 31981     9

## remove extra columns ----
print_df_names(effort_cropped_short_cnt_rule3)

effort_cropped_short_cnt_rule3_short <-
  effort_cropped_short_cnt_rule3 |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))

dim(effort_cropped_short_cnt_rule3_short)
# [1] 31981     5

## join with min grid ----
heat.plt <-
  effort_cropped_short_cnt_rule3_short |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid))
# Joining with `by = join_by(cell_id)`

# mapview(grid)
dim(heat.plt)
# [1] 35828     6
# [1] 33883     6 (rule3)
# [1] 31981     6

## make a plot ----
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).") {

    max_num <- max(map_trip_base_data[[trip_cnt_name]])
    map_trips <-
      ggplot() +
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      geom_sf(data = shape_data, fill = NA) +
      geom_sf_text(data = shape_data,
                   aes(geometry = geometry,
                       label = StatZone),
                   size = 3.5) +
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      theme_bw() +
      scale_fill_gradient2(
        name = total_trips_title,
        labels = scales::comma,
        low = "red",
        mid = "purple",
        high = "blue",
        # trans = "log2",
        trans = "log1p",
        limits = c(1, max_num)
        # ,
        # oob = scales::oob_keep
      ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(0.9, "npc"),
        # legend.key.width = unit(3, "cm"),
        plot.caption = element_text(hjust = 0)
      ) +
      guides(fill = guide_colourbar(title.position = "top"))

    return(map_trips)
  }

max_num3 <- max(heat.plt$trip_id_cnt)
map_trips_rule_3 <-
  make_map_trips(heat.plt,
           st_union_GOMsf,
           "total trips",
           max_num = max_num3,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

# map_trips_rule_3

map_trips_no_rule_3 <-
  make_map_trips(heat.plt_no_rule3_all_dots,
           st_union_GOMsf,
           "total trips")


## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(GOMsf, join = st_nearest_feature)
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf: 75.5 sec elapsed

# all points for GOM permitted
# mapview(for_heatmap_lat_lon_trips_vessels_only_join_gomsf)
# [1] 41455     4

tic("for_heatmap_lat_lon_trips_vessels_only_inters_gomsf")
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf <-
  for_heatmap_lat_lon_trips_vessels_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf_1: 0.5 sec elapsed

# only points in GOM
dim(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf)
# [1] 35822     6
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, StatZone, LONGITUDE, LATITUDE"

### count trip ids and vessels by statZone ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf |>
  # sf::st_drop_geometry() |>
  group_by(StatZone) |>
  mutate(vsl_cnt_stat_zone = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt_stat_zone = n_distinct(TRIP_ID)) |>
  ungroup()

View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt)

for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  filter(vsl_cnt_stat_zone < 3) |>
  write_csv("state_zone_less_2_vsl.csv")

# check
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
    sf::st_drop_geometry() |>
    select(vsl_cnt_stat_zone) |>
    distinct() |>
    arrange(vsl_cnt_stat_zone)

### remove extra columns ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR)) |>
  distinct() |>
  ungroup()

# View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)
# [1] 35822     4

# dots
# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  # filter(vsl_cnt_stat_zone < 10) |>
  # mapview()

#### only vsl > 3 ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_rule3 <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  filter(vsl_cnt_stat_zone > 2)

### combine cnts with statzone geometry ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  sf::st_drop_geometry() |>
  distinct()
# 21

# class(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df)

gomshp_zone_cnt <-
left_join(GOMsf,
          for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df,
         join_by("StatZone"))

dim(gomshp_zone_cnt)
# [1] 21  4

shape_data <- gomshp_zone_cnt


### plot by zone ----
# ggplot(map.df,aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=delta),color="grey20")+

# glimpse(gomshp_zone_cnt)
# mapview(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)

plot_zone_cnt <-
    function(shape_data,
           total_cnt_title,
           cnts_col_name,
           caption_text) {
      # browser()
      max_num <-
        max(shape_data[[cnts_col_name]])
      # max(gomshp_zone_cnt$vsl_cnt_stat_zone)
      # 148
      min_num <-
        min(shape_data[[cnts_col_name]])
      # 2

      map_vsl_zone <-
        ggplot() +
        geom_sf(data = shape_data,
                aes(geometry = geometry,
                    #                   fill = !!sym(trip_cnt_name)),
                    fill = !!sym(cnts_col_name)),
                colour = NA) +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5) +
        labs(
          x = "",
          y = "",
          fill = "",
          caption = caption_text
        ) +
        theme_bw() +
        scale_fill_gradient2(
          name = total_cnt_title,
          labels = scales::comma,
          low = "red",
          mid = "purple",
          high = "blue",
          # trans = "log2",
          trans = "log1p",
          limits = c(1, max_num)
          # ,
          # oob = scales::oob_keep
        ) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = unit(0.9, "npc"),
          # legend.key.width = unit(3, "cm"),
          plot.caption = element_text(hjust = 0)
        ) +
        guides(fill = guide_colourbar(title.position = "top"))
    }

for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  # dim()
# [1] 35822     4

  distinct() |>
  dim()
  # [1] 29230     4


shape_data = gomshp_zone_cnt
total_cnt_title = "total reporting vessels"
cnts_col_name = "vsl_cnt_stat_zone"
caption_text = "Heat map of SEFHIER trips. 2022. GoM permitted vessels."


map_trips_stat_zone <-
  plot_zone_cnt(
    gomshp_zone_cnt,
    "total reporting vessels",
    "vsl_cnt_stat_zone",
    caption_text = "Heat map of SEFHIER reporting vessels. 2022. GoM permitted only."
  )

map_trips_stat_zone
#

