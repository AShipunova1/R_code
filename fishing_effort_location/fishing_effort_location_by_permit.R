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

# use metriks only vessels not in SRHS ----
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
      select(-c(LATITUDE, LONGITUDE,
                permit_sa_gom,
                permit_region)) |>
      dplyr::add_count(ten_min_lat, ten_min_lon,
                       name = "trip_ids_cnts")
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2 <-
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

View(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     10
# ok (== safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations  ----

# print_df_names(
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR)) |>
      distinct()
  })

safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

# GOM permits / vessels ----

## prepare sf ----
gom_vessels <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u$gom_dual

dim(gom_vessels)
# [1] 1369    3

# head(gom_vessels, 2)
#   ten_min_lat ten_min_lon location_cnts
#         <dbl>       <dbl>         <int>
# 1        27.7       -83.2           135
# 2        27.5       -83.3            83

# names(gom_vessels) <-
#   c("LATITUDE", "LONGITUDE",
#     "location_cnts")

## base map gom vessels ----
image_with_clusters_base <-
  function(lat_lon_data) {
  gom_clusters_shape_base <-
    leaflet(data = lat_lon_data) |>
    addTiles()
  return(gom_clusters_shape_base)
}

map_base_gom_vessels <- image_with_clusters_base(gom_vessels)

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
      sum += Number(markers[i].options.location_cnts);
    }
  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + sum + '</div><span>'
  return new L.DivIcon({html: html, className: 'marker-cluster'});
  }"
)

# environment(map_base_gom_vessels_sf[["preRenderHook"]])[["data"]][["location_cnts"]]

# label = ~htmlEscape(Name)
# works
View(map_base_gom_vessels_15)
map_base_gom_vessels_15 |>
  addMarkers(
    lat = ~ ten_min_lat,
    lng = ~ ten_min_lon,
    options =
      markerOptions(location_cnts = ~ location_cnts),
    clusterOptions = markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  )

# works with circle markers
map_base_gom_vessels_sf_15 |>
  addCircleMarkers(
    options = pathOptions(location_cnts = ~ location_cnts),
    label = ~ location_cnts,
    # labels always on
    labelOptions = labelOptions(noHide = T),
    # sum of cnts on green circles
    clusterOptions =
      markerClusterOptions(iconCreateFunction = cnts_sum_marker_js)
  ) |>
  # ten min grid
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "grey", weight = 1))

# example with lat long vs ten min ----

gom_vessels_sf_example3 <-
  gom_vessels_sf |>
  filter(location_cnts %in% c("475", "839", "961"))

print_df_names(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual)

short_example_3 <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  filter(
    round(ten_min_lat, digits = 1) ==
      round(gom_vessels_sf_example3$LATITUDE,
            digits = 1) &
      round(ten_min_lon, digits = 1) ==
      round(gom_vessels_sf_example3$LONGITUDE,
            digits = 1)
  )

dim(short_example_3)
# 740

short_example_3_cnts <-
  short_example_3 |>
    group_by(ten_min_lat, ten_min_lon) |>
    add_count(name = "ten_min_cnts")

short_example_3 |>
  dplyr::count(ten_min_lat, ten_min_lon) |>
  dplyr::count(wt = n)
# 740

short_example_3 |>
  select(TRIP_ID) |>
  distinct() |>
  dim()
# 740 (distinct and w/o)

short_example_3 |>
  select(LATITUDE, LONGITUDE) |>
  dim()
# 740
# 142+319+279
# [1] 740
  # distinct() |>
# [1] 564   2

dim(short_example_3_cnts)
# [1] 740   9

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
  distinct()
# ok
#     ten_min_lat ten_min_lon     n
#         <dbl>       <dbl> <int>
# 1        27.7       -82.7   279
# 2        27.8       -82.8   142
# 3        27.8       -82.7   319

str(short_example_3_cnts_short)

short_example_3_cnts_short_lbl <-
  short_example_3_cnts_short |>
  # add coordinates labels
  mutate(ten_min_lbl =
           paste0(
             round(ten_min_lat, 1),
             ", ",
             round(ten_min_lon, 1),
             ": ",
             round(ten_min_cnts, 1),
             " trips"
           ))

my_text <-
  "Numbers on round circles show an amount of unique locations in this cluster.</br>
Clicking on one circle will zoom in and show individual fishing locations.
</br>
Rectangular labels show coordinates on the ten minute grid and an amount of trips in this square."

rr <-
  htmltools::tags$div(htmltools::HTML(paste0('<span>',
                       my_text,
                       '</span>')))

# str(short_example_3_cnts_short_tm_sf)
map_leaflet_short_example <-
  leaflet(short_example_3_cnts_short_lbl) |>
  addTiles() |>
  addCircleMarkers(clusterOptions = markerClusterOptions()) |>
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

map_leaflet_short_example

# uncomment to run
# htmlwidgets::saveWidget(map_leaflet_short_example,
#                         file =
#                           r"(my_outputs\fishing_trips_GOM_2022\map_leaflet_short_example.html)")

# mapview(short_example_3_cnts_short_tm_sf)
leaflet(data = short_example_3_cnts_short_tm_sf) |>
  addTiles() |>
    addCircleMarkers(short_example_3_cnts_short_tm_sf,
                     radius = 10,
                     color = "blue",
                     label = ten_min_cnts,
                     clusterOptions = markerClusterOptions())


### all points ----
map_base_gom_vessels_sf |>
addCircleMarkers(
  options =
    pathOptions(loc_cnts = ~location_cnts),
  clusterOptions =
    markerClusterOptions(iconCreateFunction = cnts_marker_js)
)

## add ten minute grid gom vessels ----
map_base |>
 addGraticule(interval = 1 / 60 * 10,
              style = list(color = "grey", weight = 1))
# red
              # style = list(color = "#FF0000", weight = 1))


# GOM area ----
## get gom boundaries ----
all_gom_sf_bbox <-
  st_bbox(all_gom_sf)

## subset data by all_gom_sf_bounding_box ----
dim(safis_efforts_extended_2022_short_good_sf)
# [1] 97547    18

tic("st_crop(safis_efforts_extended_2022_short_good_sf")
gom_safis_efforts_extended_2022_short_good_sf_crop <-
  st_crop(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf_bbox)
toc()
# st_crop(safis_efforts_extended_2022_short_good_sf: 0.5 sec elapsed

# mapview(gom_safis_efforts_extended_2022_short_good_sf)

dim(gom_safis_efforts_extended_2022_short_good_sf_crop)
# [1] 68190    18

## get only points on GOM maps ----

tic("gom_safis_efforts_extended_2022_short_good_sf_crop_inters")
gom_safis_efforts_extended_2022_short_good_sf_crop_inters <-
  with_st_intersection(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf)
toc()
# gom_safis_efforts_extended_2022_short_good_sf_crop_inters: 722.62 sec elapsed
# 722.62/60 = 722.62 m

# str(safis_efforts_extended_2022_short_good_sf)
# sfc_POINT of length 97547
# str(all_gom_sf)
#   ..- attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"

st_intersection_faster <- function(x, y, ...) {
  #faster replacement for st_intersection(x, y,...)

  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {
      y[., ]
    }

  st_intersection(x, y_subset, ...)
}

tic("gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1")
gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1 <-
  st_intersection_faster(safis_efforts_extended_2022_short_good_sf,
          all_gom_sf)
toc()
# gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1: 695.34 sec elapsed
# 695.34/60 = 11.589m

dim(gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1)
# [1] 42866    26

tic("gom_points_n_shape")
gom_points_n_shape <-
  mapview(
    all_gom_sf,
    col.regions = "#F4E3FF",
    alpha.regions = 0.2,
    layer.name = "Gulf of Mexico",
    legend = FALSE
  ) +
  mapview(gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1,
          legend = F,
          clusterOptions = markerClusterOptions())
toc()
# gom_points_n_shape: 13.22 sec elapsed

## image with clusters -----
lat_lon_data <- gom_safis_efforts_extended_2022_short_good_sf_crop_inters_1

image_with_clusters_base <- function(lat_lon_data) {
  tic("gom_clusters_shape")
  gom_clusters_shape_base <-
    leaflet(data = lat_lon_data) |>
    addTiles() |>
    addPolygons(data = all_gom_sf,
                weight = 5,
                col = "#F4E3FF") |>
    flyToBounds(-97.8, 23.8, -80.4, 31.1) |>
    setView(-89, 29, zoom = 5)
  toc()
  return(gom_clusters_shape_base)
}

map_base <- image_with_clusters_base(lat_lon_data)

marker_js <- JS(
  "function(cluster) {
                  var html = '<div style=\"background-color:rgba(144, 238, 144)\"><span>' + cluster.getChildCount() + '</div><span>'
                  return new L.DivIcon({html: html, className: 'marker-cluster'});
}"
)

map_base |>
    addCircleMarkers(clusterOptions =
                         markerClusterOptions(
          iconCreateFunction = marker_js
                       ))

# add ten minute grid ----
map_base |>
 addGraticule(interval = 1 / 60 * 10,
              style = list(color = "grey", weight = 1))
              # style = list(color = "#FF0000", weight = 1))


# baselayerchange
map_base |>
  htmlwidgets::onRender("
    function(e, x) {
      var myMap = this;

			this._shownPolygon = new L.Polygon(e.layer.getConvexHull(), this.options.polygonOptions);
			myMap.addLayer(this._shownPolygon);


    }")

js_cluster_polygons <-
  function(cluster) {
    # JS(
    "
# function(cluster) {
        cluster._showCoverage({ layer: cluster })
        //var coverages = new L.LayerGroup();
        // coverages.clearLayers();
        //coverages.addLayer(L.polygon(cluster.getConvexHull()));

          // getConvexHull())
          //	var latLngBounds = cluster.getBounds();
        	var latLngConvex = cluster.getConvexHull();
          // L.addLayer(L.polygon(cluster.getConvexHull()));


          var sum = 55;
          var children = cluster.getAllChildMarkers();
         return new L.DivIcon({
              html: '<div style=\"background-color: rgb(111,198,204); opacity : 0.7\"><span>' + JSON.stringify(coverages) + '</div><span>',
              className: 'marker-cluster'
                                               });
                                           # }
    "
    # )
  }

image_with_clusters()
# JSON.stringify(coverages)


l <- leaflet() %>% setView(0,0,3)

esri <- grep("^Esri", providers, value = TRUE)

for (provider in esri) {
  l <- l %>% addProviderTiles(provider, group = provider)
}

l %>%
  addLayersControl(baseGroups = names(esri),
    options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
    position = "bottomleft") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")

lat_lon_data_short <-
  lat_lon_data |>
  sf::st_drop_geometry() |>
  select(LATITUDE,
         LONGITUDE,
         TRIP_ID)
# |>
  # distinct()

lat_lon_data_coord <-
  lat_lon_data |>
  sf::st_drop_geometry() |>
  # na.omit() |>
  select(LATITUDE,
         LONGITUDE)
# |>
  # distinct()

# dim(lat_lon_data_uniq_coord)
# [1] 35762     2

dim(lat_lon_data_short)
# [1] 42865     3
# LATITUDE  31996
# LONGITUDE 32220

lat_lon_data_uniq_coord_ten_min <-
  lat_lon_data_coord |>
  # head(10) |>
  get_ten_min_coords()

lat_lon_data_uniq_coord_ten_min_short <-
  lat_lon_data_uniq_coord_ten_min |>
  select(ten_min_lat, ten_min_lon)
# |>
  # distinct() |>
  # dim()
# [1] 1295    2

lat_lon_data_uniq_coord_ten_min_short <-
  lat_lon_data_uniq_coord_ten_min_short |>
  rename(LATITUDE = ten_min_lat,
         LONGITUDE = ten_min_lon)

map_base <-
  image_with_clusters_base(lat_lon_data_uniq_coord_ten_min_short)

map_base |>
  addCircleMarkers(clusterOptions =
                     markerClusterOptions(iconCreateFunction =                                            marker_js)) |>
  addGraticule(interval = 1 / 60 * 10,
               style = list(color = "#FF0000", weight = 1))


# SA ----
### with st_intersection ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec
# 174.95 sec
# 193.08 /60 ~3
# 196.25 sec

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf_more_fields.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

dim(db_data_w_area_report_sa_eez_sf)
# 18989 13
# [1] 18967    13
# [1] 18970    21

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# 92882   11
# 27979   19   unique

## state waters sa ----
get_state_waters_sa_sf <- function() {
  fl_counties_sa <- c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "Saint Johns",
    "Saint Lucie",
    "Volusia",
    "Monroe"
  ) #has GOM too, remove separately

  # mapview(fl_state_w_counties)
  # fl_state_w_counties$gnis_name %>% paste0(collapse = ", ")

  fl_state_w_counties_names <- fl_state_w_counties_shp$gnis_name

  # length(fl_state_w_counties_names)
  # 67

  # grep("Monroe", fl_state_w_counties_names, value = T)

  # length(fl_counties_sa)
  # 12 + Monroe

  fl_state_w_counties_names_df <-
    as.data.frame(fl_state_w_counties_names)
  # str(fl_state_w_counties_names_df)
  # fl_state_w_counties_names) %>%

  # View(as.data.frame(fl_counties_sa))

  sa_fl_state_w_counties_names <-
    as.data.frame(fl_counties_sa)[[1]] %>%
    map_df(function(fl_county) {
      # browser()
      sa_county <-
        fl_state_w_counties_names_df %>%
        filter(grepl(
          fl_county,
          fl_state_w_counties_names_df$fl_state_w_counties_names
        ))

      return(sa_county)
    })

  fl_state_w_counties_sa <- filter(
    fl_state_w_counties_shp,
    gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names
  )

  return(fl_state_w_counties_sa)
}

fl_state_w_counties_sa_sf <- get_state_waters_sa_sf()

# mapview(db_data_w_area_report_28_s_sf)

### get only state and inner waters by intersection ----
db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_28_s_sf,
                      fl_state_w_counties_sa_sf)
# 0.37 sec
# 3.56 sec

dim(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    38

# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
db_data_w_area_report_28_s_sa_counties_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30
# 10761    38

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

### using sf::st_difference ----
# slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec
# 195.96 sec ~3 m


# or read csv
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903 40

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

# Report csv ----
my_sf_to_df <- function(my_sf) {
  my_df <-
    my_sf %>%
    sf::st_drop_geometry() %>%
    select(all_of(fields_list)
           # )
    # select(
    #   TRIP_START_DATE,
    #   TRIP_END_DATE,
    #   START_PORT,
    #   START_PORT_NAME,
    #   START_PORT_COUNTY,
    #   START_PORT_STATE,
    #   END_PORT,
    #   LATITUDE,
    #   LONGITUDE,
    #   FISHING_GEAR_DEPTH
    ) %>%
    unique()

  return(my_df)
}

my_sf_to_csv <- function(my_sf, file_name) {
  my_df <- my_sf_to_df(my_sf)

  write_csv(
    my_df,
    file.path(my_paths$outputs,
              current_project_name,
              "report",
              paste0(file_name, ".csv"))
  )
}

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   40

my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")
# names(db_data_w_area_report_sa_eez_sf)
# 18989
# [1] 18970    21


my_sf_to_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf, "south_of_28_state_w")
# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903

# all maps together ----
## south of 28 map ----
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

m_g_r <- mapview(
  gom_reef_shp,
  col.regions = "lightblue",
  alpha.regions = 0.2,
  layer.name = "GOM Reef Fish EFH",
  legend = FALSE
)

m_sa_eez <-
  mapview(
    db_data_w_area_report_sa_eez_sf,
    layer.name = 'SA EEZ'
  )

all_maps <-
  m_s +
  m_g_r +
  m_sa_eez +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf


# make a flat file ----
dir_to_comb <- file.path(my_paths$git_r, current_project_name)

files_to_combine_list <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R")
    # ,
    # file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat_05_30.R")

# run as needed
# make_a_flat_file(flat_file_name,
                 # files_to_combine_list)

# The relative would be looking by depth, area, and seasonally. ----
## by area: ----
# db_data_w_area_report_sa_eez_sf
# db_data_w_area_report_28_s_sa_counties_no_gom_sf

## by depth, state ----
db_data_w_area_report_sa_eez_sf %>%
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

db_data_w_area_report_28_s_sa_counties_no_gom_sf %>%
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

# by end_port, depth, month ---
db_data_w_area_report %>%
    dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  count(FISHING_GEAR_DEPTH, END_PORT, TRIP_START_M) %>% glimpse()
  # View()

# SA only ----

# Filter out maximum by data ----
db_data_w_area_no_mex <-
  db_data_w_area %>%
  # [1] 254689     32
  dplyr::filter(!(grepl("MEX", AREA_NAME))) %>%
    # 254503
  dplyr::filter(!(grepl("GOM", AREA_NAME))) %>%
  dplyr::filter(!REGION %in% c("GULF OF MEXICO")) %>%
  # all LONG should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) %>%
  # [1] 253003     32
  # south and north by SA shp
  dplyr::filter(between(LATITUDE, 23.81794, 36.55028)) %>%
  # [1] 241183     32
  # 133889
  # west and east by SA shp
  dplyr::filter(between(LONGITUDE, -83, -71.37133))
  # [1] 140177     32

# st_geometry(sa_shp)
# Bounding box:  xmin: -83 ymin: 23.81794 xmax: -71.37133 ymax: 36.55028

db_data_w_area_no_mex_uniq <-
  db_data_w_area_no_mex %>%
  unique()

# keep fewer columns ----
db_data_w_area_report_short <-
  db_data_w_area_no_mex_uniq %>%
  select(all_of(fields_list))

dim(db_data_w_area_report_short)
# 45315 10
# [1] 45264    18

# keep fewer rows by removing duplicates ----
db_data_w_area_report <-
  db_data_w_area_report_short %>% unique()
# [1] 45261    10

# SA EEZ for all ----
## get only the points inside the SA EEZ ----
db_data_w_area_report_sf <-
  db_data_w_area_report_short %>%
  unique() %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_sf)
# [1] 45264    19

### with st_intersection ----
# get only the points inside the SA EEZ by intersection
db_data_w_area_report_sa_eez_sf <-
  with_st_intersection(db_data_w_area_report_sf, sa_shp)
# 594.35 sec
# 63.44 sec (uniq)
# 65.1 sec
# 174.95 sec
# 193.08 /60 ~3
# 196.25 sec

### or read it ----
db_data_w_area_report_sa_eez_file_name <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_sa_eez_sf_more_fields.csv")

db_data_w_area_report_sa_eez_sf <-
  read_sf(db_data_w_area_report_sa_eez_file_name) %>%
  my_to_sf()

#### save sa_eez_data ----
write_csv(db_data_w_area_report_sa_eez_sf, db_data_w_area_report_sa_eez_file_name)

dim(db_data_w_area_report_sa_eez_sf)
# 18989 13
# [1] 18967    13
# [1] 18970    21

# South of 28N - all SA ----
db_data_w_area_report_28_s_sf <-
  db_data_w_area_report %>%
  filter(between(LATITUDE, 23, 28)) %>%
  # convert to sf, see fun above (use F2)
  my_to_sf()

dim(db_data_w_area_report_28_s_sf)
# 92882   11
# 27979   19   unique

## state waters sa ----
get_state_waters_sa_sf <- function() {
  fl_counties_sa <- c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "Saint Johns",
    "Saint Lucie",
    "Volusia",
    "Monroe"
  ) #has GOM too, remove separately

  # mapview(fl_state_w_counties)
  # fl_state_w_counties$gnis_name %>% paste0(collapse = ", ")

  fl_state_w_counties_names <- fl_state_w_counties_shp$gnis_name

  # length(fl_state_w_counties_names)
  # 67

  # grep("Monroe", fl_state_w_counties_names, value = T)

  # length(fl_counties_sa)
  # 12 + Monroe

  fl_state_w_counties_names_df <-
    as.data.frame(fl_state_w_counties_names)
  # str(fl_state_w_counties_names_df)
  # fl_state_w_counties_names) %>%

  # View(as.data.frame(fl_counties_sa))

  sa_fl_state_w_counties_names <-
    as.data.frame(fl_counties_sa)[[1]] %>%
    map_df(function(fl_county) {
      # browser()
      sa_county <-
        fl_state_w_counties_names_df %>%
        filter(grepl(
          fl_county,
          fl_state_w_counties_names_df$fl_state_w_counties_names
        ))

      return(sa_county)
    })

  fl_state_w_counties_sa <- filter(
    fl_state_w_counties_shp,
    gnis_name %in% sa_fl_state_w_counties_names$fl_state_w_counties_names
  )

  return(fl_state_w_counties_sa)
}

fl_state_w_counties_sa_sf <- get_state_waters_sa_sf()

# mapview(db_data_w_area_report_28_s_sf)

### get only state and inner waters by intersection ----
db_data_w_area_report_28_s_sa_counties_sf <-
  with_st_intersection(db_data_w_area_report_28_s_sf,
                      fl_state_w_counties_sa_sf)
# 0.37 sec
# 3.56 sec

dim(db_data_w_area_report_28_s_sa_counties_sf)
# [1] 10761    38

# mapview(db_data_w_area_report_28_s_sa_counties_sf)

### or read csv ----
db_data_w_area_report_28_s_sa_counties_file_name <-
file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_sf_u_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_sf <-
  read_sf(db_data_w_area_report_28_s_sa_counties_file_name) %>%
  my_to_sf()

write_csv(
  db_data_w_area_report_28_s_sa_counties_sf,
  db_data_w_area_report_28_s_sa_counties_file_name
)

dim(db_data_w_area_report_28_s_sa_counties_sf)
# 30392    30
# 10761    38

## For Monroe exclude GOM ----

# View(fl_state_w_counties_monroe)

### using sf::st_difference ----
# slow
db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  with_st_difference(db_data_w_area_report_28_s_sa_counties_sf, gom_reef_shp)
# 188.69 sec
# 195.96 sec ~3 m


# or read csv
sa_counties_no_gom_sf_filename <- file.path(my_paths$outputs, current_project_name, "db_data_w_area_report_28_s_sa_counties_no_gom_sf_more_fields.csv")

db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  read_sf(sa_counties_no_gom_sf_filename) %>%
  my_to_sf()

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 23519    32
# 8903 40

write_csv(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  sa_counties_no_gom_sf_filename
)

# mapview(db_data_w_area_report_28_s_sa_counties_no_gom_sf)

# Report csv ----
# convert sf to a df
my_sf_to_df <- function(my_sf) {
  my_df <-
    my_sf %>%
    sf::st_drop_geometry() %>%
    select(all_of(fields_list)
           # )
    # select(
    #   TRIP_START_DATE,
    #   TRIP_END_DATE,
    #   START_PORT,
    #   START_PORT_NAME,
    #   START_PORT_COUNTY,
    #   START_PORT_STATE,
    #   END_PORT,
    #   LATITUDE,
    #   LONGITUDE,
    #   FISHING_GEAR_DEPTH
    ) %>%
    unique()

  return(my_df)
}

my_sf_to_csv <- function(my_sf, file_name) {
  my_df <- my_sf_to_df(my_sf)

  write_csv(
    my_df,
    file.path(my_paths$outputs,
              current_project_name,
              "report",
              paste0(file_name, ".csv"))
  )
}

dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903   40

my_sf_to_csv(db_data_w_area_report_sa_eez_sf, "sa_eez_all")
# names(db_data_w_area_report_sa_eez_sf)
# 18989
# [1] 18970    21


my_sf_to_csv(db_data_w_area_report_28_s_sa_counties_no_gom_sf, "south_of_28_state_w")
# dim(db_data_w_area_report_28_s_sa_counties_no_gom_sf)
# 8903

# all maps together ----
## south of 28 map ----
m_db_data_w_area_report_28_s_sa_counties_no_gom_sf <-
  mapview(
  db_data_w_area_report_28_s_sa_counties_no_gom_sf,
  col.regions = "green",
  layer.name = 'State and inner waters'
)

m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

m_g_r <- mapview(
  gom_reef_shp,
  col.regions = "lightblue",
  alpha.regions = 0.2,
  layer.name = "GOM Reef Fish EFH",
  legend = FALSE
)

m_sa_eez <-
  mapview(
    db_data_w_area_report_sa_eez_sf,
    layer.name = 'SA EEZ'
  )

all_maps <-
  m_s +
  m_g_r +
  m_sa_eez +
  m_db_data_w_area_report_28_s_sa_counties_no_gom_sf


# make a flat file ----
dir_to_comb <- file.path(my_paths$git_r, current_project_name)

files_to_combine_list <-
  c(
    file.path(my_paths$git_r, "useful_functions_module.r"),
    file.path(dir_to_comb, "read.me.R"),
    file.path(dir_to_comb, "fishing_effort_locations_get_data.R"),
    file.path(dir_to_comb, "fishing_effort_location.R")
    # ,
    # file.path(dir_to_comb, "fishing_effort_location_viz.R")
  )

flat_file_name = file.path(dir_to_comb, "fishing_effort_location_flat_05_30.R")

# run as needed
# make_a_flat_file(flat_file_name,
                 # files_to_combine_list)

# The relative would be looking by depth, area, and seasonally. ----
## by area: ----
# db_data_w_area_report_sa_eez_sf
# db_data_w_area_report_28_s_sa_counties_no_gom_sf

## by depth, state ----
db_data_w_area_report_sa_eez_sf %>%
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

db_data_w_area_report_28_s_sa_counties_no_gom_sf %>%
  my_sf_to_df() %>%
  count(FISHING_GEAR_DEPTH, START_PORT_STATE) %>%
  View()

# by end_port, depth, month ---
db_data_w_area_report %>%
    dplyr::mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  count(FISHING_GEAR_DEPTH, END_PORT, TRIP_START_M) %>% glimpse()
  # View()

## seasonally ----

