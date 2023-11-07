## prepare trip coord data ----
# print_df_names(all_get_db_data_result_l)
# mv_sero_fh_permits_his, trips_info, trip_coord_info, trip_neg_2022, trips_notifications_2022, vessels_permits, dates_2022, compl_err_db_data

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols() |>
  filter(TRIP_TYPE %in% c("A", "H"))

View(trip_coord_info)
# [1] 139504     39
# TRIP_ID, AREA_CODE, SUB_AREA_CODE, DISTANCE_CODE, FISHING_HOURS, LATITUDE, LONGITUDE, LOCAL_AREA_CODE, IN_STATE, AVG_DEPTH_IN_FATHOMS, E_DE, E_UE, E_DC, E_UC, ANYTHING_CAUGHT_FLAG, DEPTH, MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH, FISHING_GEAR_DEPTH, TRIP_TYPE, SUPPLIER_TRIP_ID, DAYS_AT_SEA, T_DE, T_UE, T_DC, T_UC, VESSEL_ID, CF_PERMIT_ID, TRIP_START_DATE, PORT, STATE, TRIP_END_DATE, TRIP_END_TIME, TRIP_START_TIME, SUBMIT_METHOD, ACTIVITY_TYPE, END_PORT, START_PORT, SERO_VESSEL_PERMIT

# clean coordinates ----
trip_coord_info_good <-
  trip_coord_info |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LON should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  dplyr::distinct()

dim(trip_coord_info_good)
# [1] 138923     39

### convert to sf from FHIER ----
tic("get GOMsf")
GOMsf <-
  sf::read_sf(r"(GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>%
  summarise()
toc()

my_crs <- sf::st_crs(GOMsf)

trip_coord_info_good_sf <-
  my_to_sf(trip_coord_info_good, my_crs)

dim(trip_coord_info_good_sf)
# [1] 138923     40

# subset by grays sanctuary ----
# Sanctuary Boundaries (DD)
sanctuary_bounding_box <- c(
   xmin = -80.921200,
   ymin = 31.362732,
   xmax = -80.828145,
   ymax = 31.421064
 )

tic("trip_coord_info_good_sf_sanct")
trip_coord_info_good_sf_sanct <-
  st_crop(trip_coord_info_good_sf,
          sanctuary_bounding_box)
toc()
# trip_coord_info_good_sf_sanct: 0.26 sec elapsed

glimpse(trip_coord_info_good_sf_sanct)
# 1

# Research Area Boundaries (DD)
research_area_bounding_box <- c(
   xmin = -80.921200,
   ymin = 31.362732,
   xmax = -80.828145,
   ymax = 31.384444
 )

# mapview(safis_efforts_extended_all_short_good_sf_sanct)
leaflet_trip_coord_info_good_sf_sanct <-
  trip_coord_info_good_sf_sanct |>
  leaflet() |>
  addTiles() |>
  addMarkers(
    lat = ~ LATITUDE,
    lng = ~ LONGITUDE,
    label = ~ paste(LATITUDE, LONGITUDE)
  ) |>
  setView(
    lng = mean(safis_efforts_extended_all_short_good_sf_sanct$LONGITUDE),
    lat = mean(safis_efforts_extended_all_short_good_sf_sanct$LATITUDE),
    zoom = 10
  ) |>
  # addRectangles(
  #   lng1 = sanctuary_bounding_box[["xmin"]],
  #   lat1 = sanctuary_bounding_box[["ymin"]],
  #   lng2 = sanctuary_bounding_box[["xmax"]],
  #   lat2 = sanctuary_bounding_box[["ymax"]],
  #   fill = FALSE,
  #   dashArray = 10,
  #   1,
  #   10,
  #   weight = 0.7
  # ) |>
    addRectangles(
    lng1 = research_area_bounding_box[["xmin"]],
    lat1 = research_area_bounding_box[["ymin"]],
    lng2 = research_area_bounding_box[["xmax"]],
    lat2 = research_area_bounding_box[["ymax"]],
    fillColor = "#ffcccb",
    fillOpacity = 0.2,
    dashArray = 10,
    1,
    10,
    weight = 0.7
  )

leaflet_trip_coord_info_good_sf_sanct

