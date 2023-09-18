# combine dfs ----
safis_efforts_extended_all <- rbind(safis_efforts_extended_2022,
                                    safis_efforts_extended_2023)

dim(safis_efforts_extended_all)
# [1] 140348     42

# rm extra columns ----
rm_columns <- c("ANYTHING_CAUGHT_FLAG",
"COMMON_NAME",
"DC",
"DE",
"FISHING_HOURS",
"GEAR_CATEGORY_CODE",
"GEAR_CATEGORY_NAME",
"GEAR_CODE",
"GEAR_DESC",
"GEAR_NAME",
"GEAR_SIZE",
"GEAR_TYPE_CODE",
"GEAR_TYPE_NAME",
"GEARS_FISHING",
"HOURS_DAYS_FLAG",
"IN_STATE",
"LMA_CODE",
"MESH_RING_LENGTH",
"MESH_RING_WIDTH",
"RIG_CODE",
"SPECIES_ITIS",
"STRETCH_SIZE",
"SUPPLIER_EFFCAT_ID",
"UC",
"UE"
)

safis_efforts_extended_all_short <-
  safis_efforts_extended_all |>
  select(-any_of(rm_columns)) |>
  distinct()

dim(safis_efforts_extended_all_short)
# [1] 140348     17

# clean coordinates ----
safis_efforts_extended_all_short_good <-
  safis_efforts_extended_all_short |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE)) |>
  # all LON should be negative
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE)) %>%
  # keep only full sets of coordinates
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

dim(safis_efforts_extended_all_short)
# [1] 140348     17

dim(safis_efforts_extended_all_short_good)
# [1] 139771     17

### convert to sf from FHIER ----
safis_efforts_extended_all_short_good_sf <-
  my_to_sf(safis_efforts_extended_all_short_good)

# subset by grays sanctuary ----
# Sanctuary Boundaries (DD)
sanctuary_bounding_box <- c(
   xmin = -80.921200,
   ymin = 31.362732,
   xmax = -80.828145,
   ymax = 31.421064
 )
tic("safis_efforts_extended_all_short_good_sf_sanct")
safis_efforts_extended_all_short_good_sf_sanct <-
  st_crop(safis_efforts_extended_all_short_good_sf,
          sanctuary_bounding_box)
toc()
# safis_efforts_extended_all_short_good_sf_sanct: 0.26 sec elapsed

glimpse(safis_efforts_extended_all_short_good_sf_sanct)
# 1

# Research Area Boundaries (DD)
research_area_bounding_box <- c(
   xmin = -80.921200,
   ymin = 31.362732,
   xmax = -80.828145,
   ymax = 31.384444
 )

# mapview(safis_efforts_extended_all_short_good_sf_sanct)
leaflet_safis_efforts_extended_all_short_good_sf_sanct <-
  safis_efforts_extended_all_short_good_sf_sanct |>
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

leaflet_safis_efforts_extended_all_short_good_sf_sanct

# here - old ----

# convert back to df ----
safis_efforts_extended_all_short_good_sf_crop_big_df <-
  safis_efforts_extended_all_short_good_sf_crop_big |>
  sf::st_drop_geometry()

dim(safis_efforts_extended_all_short_good_sf_crop_big_df)
# [1] 137761     17


# use metrics only vessels not in SRHS ----
source(r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
safis_efforts_extended_all_short_good_sf_crop_big_df_in_metricks <-
  safis_efforts_extended_all_short_good_sf_crop_big_df |>
  filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(safis_efforts_extended_all_short_good_sf_crop_big_df_in_metricks)
# [1] 134918     17

# add permit info ----
## prepare permit info ----
# Reports / SERO Permits
permit_file_path = r"(my_inputs\from_Fhier\SERO Permits_09_14_2023.csv)"

permits_from_fhier <-
  read_csv(permit_file_path,
           name_repair = fix_names)
# Rows: 138917 Columns: 19

### keep only permits not expired before 2022 - FILTER ----
# View(permits_from_fhier)
permits_from_fhier_active <-
  permits_from_fhier |>
  filter(expiration_date > '01/01/2022' |
           end_date > '01/01/2022')

dim(permits_from_fhier_active)
# [1] 138917     19

## add permits to coordinates ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_all_short_good_sf_crop_big_df_in_metricks,
    permits_from_fhier_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits |>
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
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30

### check names ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom |>
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
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_short <-
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom |>
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

dim(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] 111716      5
# [1] 109577      5

# print_df_names(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_short)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE, permit_sa_gom"

# convert to ten_min ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min <-
  get_ten_min_coords(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_short)

dim(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min)
# [1] 95720     5
# [1] 284785     32 (with permit)
# [1] 111716      7 (fewer columns)
# [1] 109577      7

# split by permit ----
## add permit_name_col ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm <-
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min |>
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

safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list <-
  split(
    safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm,
    as.factor(
      safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm$permit_region
    )
  )

safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total <-
  dim(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm)[[1]]
# [1] 109577      8

map_df(
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
# 2        8       8

# rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# save counts ----
## check ----
safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
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

# View(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list)

safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts <-
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list |>
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

# View(safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts2$gom_dual)

map_df(
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts,
  dim
)
#   gom_dual sa_only
#      <int>   <int>
# 1    41455   68122
 # |> rowSums()
# [1] 109577     16
# ok (== safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_total)

# remove duplicate locations (shorten) ----

# print_df_names(
#   safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts[[1]]
# )
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, ten_min_lat, ten_min_lon, location_cnts"

safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u <-
  safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts |>
  map(function(permit_df) {
    permit_df |>
      select(-c(TRIP_ID, VESSEL_OFFICIAL_NBR,
                LATITUDE, LONGITUDE)) |>
      distinct()
  })

safis_efforts_extended_all_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts_u |>
  map_df(dim)
#   gom_dual sa_only
#      <int>   <int>
# 1     1369    2344

