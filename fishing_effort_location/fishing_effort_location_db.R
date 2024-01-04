# basename(rstudioapi::getSourceEditorContext()$path)
# setup (fishing_effort_location_db) ----
# TODO: separate and source functions from
# "R_code_github\fishing_effort_location\fishing_effort_location_by_permit.R"

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

# prepare data ----
## prepare trip coord data ----
# print_df_names(all_get_db_data_result_l)
# mv_sero_fh_permits_his, trips_info, trip_coord_info, trip_neg_2022, trips_notifications_2022, vessels_permits, dates_2022, compl_err_db_data

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols() |>
  filter(TRIP_TYPE %in% c("A", "H"))

dim(trip_coord_info)
# [1] 139504     39
# TRIP_ID, AREA_CODE, SUB_AREA_CODE, DISTANCE_CODE, FISHING_HOURS, LATITUDE, LONGITUDE, LOCAL_AREA_CODE, IN_STATE, AVG_DEPTH_IN_FATHOMS, E_DE, E_UE, E_DC, E_UC, ANYTHING_CAUGHT_FLAG, DEPTH, MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH, FISHING_GEAR_DEPTH, TRIP_TYPE, SUPPLIER_TRIP_ID, DAYS_AT_SEA, T_DE, T_UE, T_DC, T_UC, VESSEL_ID, CF_PERMIT_ID, TRIP_START_DATE, PORT, STATE, TRIP_END_DATE, TRIP_END_TIME, TRIP_START_TIME, SUBMIT_METHOD, ACTIVITY_TYPE, END_PORT, START_PORT, SERO_VESSEL_PERMIT

## prepare permit info ----
### remove empty cols ----
vessels_permits_clean <-
  all_get_db_data_result_l[["vessels_permits"]] |>
  remove_empty_cols()

dim(all_get_db_data_result_l[["vessels_permits"]])
# [1] 78438    51

# View(vessels_permits_clean)
# [1] 78438    50

### keep only group 7 permits ----
vessels_permits_clean_gr7 <-
  vessels_permits_clean |>
  filter(PERMIT_GROUP == 7)

dim(vessels_permits_clean_gr7)
# [1] 61153    50

### permits effective in 2022 ----
vessels_permits_clean_gr7_2022 <-
  vessels_permits_clean_gr7 |>
  filter(EFFECTIVE_DATE <=
           as.Date('2022-12-31'))

dim(vessels_permits_clean_gr7_2022)
# [1] 47973    50

## add permit regions ----
### combine tops  ----
vessels_permits_clean_gr7_2022_all_permits <-
  vessels_permits_clean_gr7_2022 |>
  dplyr::group_by(VESSEL_VESSEL_ID) |>
  dplyr::mutate(all_permits = toString(unique(TOP))) |>
  dplyr::ungroup() |>
  dplyr::select(VESSEL_VESSEL_ID, all_permits) |>
  dplyr::distinct()

# data_overview(vessels_permits_clean_gr7_2022_all_permits)
# VESSEL_VESSEL_ID 4493
# all_permits       56

### add permit region ----
vessels_permits_clean_gr7_2022_region <-
  vessels_permits_clean_gr7_2022_all_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits")

dim(vessels_permits_clean_gr7_2022_region)
# [1] 4493    3

data_overview(vessels_permits_clean_gr7_2022_region)
# VESSEL_VESSEL_ID 4493
# all_permits        56
# permit_sa_gom       3

### combine permits into 2 groups; for 2022 gom+dual, sa_only ----
vessels_permits_clean_gr7_2022_region2 <-
  vessels_permits_clean_gr7_2022_region |>
  dplyr::mutate(permit_22 =
           dplyr::case_when(permit_sa_gom == "sa_only" ~ "sa_only",
                     .default = "gom_dual")) |>
  dplyr::select(-permit_sa_gom) |>
  dplyr::distinct()

dim(vessels_permits_clean_gr7_2022_region2)
# [1] 4493    3

# Heatmaps for charter and headboat separately ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they dplyr::selected that in the logbook.

## filter 2022 ----
trip_coord_info_2022 <-
  trip_coord_info |>
  filter(TRIP_START_DATE > "2021-12-31" &
           TRIP_START_DATE  <= "2022-12-31")

dim(trip_coord_info_2022)
# [1] 96821    39

# data_overview(trip_coord_info_2022)
# TRIP_ID             96702
# VESSEL_ID            1939

## shorten ----
rm_cols <- c("TRIP_ID",
"LATITUDE",
"LONGITUDE",
"TRIP_TYPE",
"VESSEL_ID",
"TRIP_START_DATE",
"TRIP_END_DATE")

trip_coord_info_2022_short <-
  trip_coord_info_2022 |>
  dplyr::select(all_of(rm_cols)) |>
  dplyr::distinct()

dim(trip_coord_info_2022_short)
# [1] 96785     7

# write_csv(trip_coord_info_2022_short, "trip_coord_info_2022_short.csv")

## only good lat long ----
trip_coord_info_2022_short_coord <-
  trip_coord_info_2022_short |>
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  dplyr::distinct()

## rename trip_types to names ----
trip_coord_info_2022_short_coord_t_names <-
  trip_coord_info_2022_short_coord |>
  dplyr::mutate(TRIP_TYPE = ifelse(TRIP_TYPE == "A",
                            "CHARTER",
                            "HEADBOAT"))

# write_csv(trip_coord_info_2022_short_coord_t_names, "trip_coord_info_2022_short_coord_t_names.csv")

## Join permits with effort ----
trip_coord_info_2022_short_vessels_permits_region <-
  trip_coord_info_2022_short_coord_t_names |>
  left_join(vessels_permits_clean_gr7_2022_region2,
            join_by(VESSEL_ID == VESSEL_VESSEL_ID))

dim(trip_coord_info_2022_short_vessels_permits_region)
# [1] 96383     9

### filter permit effective by trip start ? ----
# not used until answered
# trip_coord_info_2022_short_vessels_permits_eff_p <-
#   trip_coord_info_2022_short_vessels_permits |>
#   filter(TRIP_START_DATE >= EFFECTIVE_DATE)
#
# dim(trip_coord_info_2022_short_vessels_permits_eff_p)
# [1] 1181296      56

### remove extra permit and vessel columns ----
# print_df_names(trip_coord_info_2022_short_vessels_permits)
# [1] "TRIP_ID, LATITUDE, LONGITUDE, TRIP_TYPE, VESSEL_ID, TRIP_START_DATE, TRIP_END_DATE, all_permits, permit_22"

trip_coord_info_2022_short_vessels_permits_region_short <-
  trip_coord_info_2022_short_vessels_permits_region |>
  dplyr::select(all_of(
    c(
      "TRIP_ID",
      "LATITUDE",
      "LONGITUDE",
      "TRIP_TYPE",
      "VESSEL_ID",
      "permit_22"
    )
  )) |>
  dplyr::distinct()

dim(trip_coord_info_2022_short_vessels_permits_region_short)
# [1] 96383     6

## separate by trip and permit_region type ----
trip_coord_info_2022_short_vessels_permits_region_short__l <-
  trip_coord_info_2022_short_vessels_permits_region_short |>
  split(
    list(
      trip_coord_info_2022_short_vessels_permits_region_short$TRIP_TYPE,
      trip_coord_info_2022_short_vessels_permits_region_short$permit_22
    )
  ) |>
  # remove extra columns in each df
  purrr::map(
    \(x)
    x |>
      dplyr::select(TRIP_ID, VESSEL_ID, LATITUDE, LONGITUDE) |>
      dplyr::distinct()
  )

# str(trip_coord_info_2022_short_vessels_permits_region_short_trip_type_l)
# List of 2
#  $ A :'data.frame':	94962 obs. of  4 variables:
#  $ H:'data.frame':	1823  obs. of  4 variables:

all_dfs_dim <-
  purrr::map_df(trip_coord_info_2022_short_vessels_permits_region_short__l, dim)

glimpse(all_dfs_dim)
# $ CHARTER.gom_dual  <int> 50280, 4
# $ HEADBOAT.gom_dual <int> 500, 4
# $ CHARTER.sa_only   <int> 43306, 4
# $ HEADBOAT.sa_only  <int> 1194, 4

sum(all_dfs_dim[1,])
# 96785 # the same as before, ok if separate gual, gom and sa
# 95280 # gom + dual vs sa_only

## create 5 min heatmaps for both trip types ----
# get functions and GOMsf ----
source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_heatmap_func.R)"
  )
)
# st_union(GOMsf): 21.5 sec elapsed

tic("effort_t_type")
effort_t_type <-
  purrr::map(trip_coord_info_2022_short_vessels_permits_region_short__l, df_join_grid)
toc()
# effort_t_type: 1.66 sec elapsed

tic("effort_t_type_cropped")
effort_t_type_cropped <- purrr::map(effort_t_type, crop_by_shape)
toc()
# effort_t_type_cropped: 1.04 sec elapsed

# str(effort_t_type_cropped)

effort_t_type_cropped_cnt <-
  effort_t_type_cropped |>
  purrr::map(
    \(x)
    x |>
      dplyr::group_by(cell_id) |>
      dplyr::mutate(
        vsl_cnt = n_distinct(VESSEL_ID),
        trip_id_cnt = n_distinct(TRIP_ID)
      ) |>
      dplyr::ungroup()
  )

map_df(effort_t_type_cropped_cnt, dim) |>
  dplyr::glimpse()
# $ CHARTER.gom_dual  <int> 34315, 9
# $ HEADBOAT.gom_dual <int> 285, 9
# $ CHARTER.sa_only   <int> 1781, 9
# $ HEADBOAT.sa_only  <int> 0, 9

# purrr::map_df(effort_t_type_cropped_cnt, data_overview)

# data_overview(effort_t_type_cropped_cnt$A)
# TRIP_ID     35777
# VESSEL_ID     819
# cell_id      3110
# data_overview(effort_t_type_cropped_cnt$H)
# TRIP_ID     663
# VESSEL_ID   263
# cell_id     359
# data_overview(effort_t_type_cropped_cnt$CHARTER.gom_dual)
# TRIP_ID     34307
# VESSEL_ID     709
# cell_id      3069
# data_overview(effort_t_type_cropped_cnt$HEADBOAT.gom_dual)
# TRIP_ID     285
# VESSEL_ID    21
# cell_id      67

# View(grid)

# purrr::map(names(effort_t_type_cropped_cnt),
#     \(type_reg) {
#       effort_t_type_cropped_cnt[[type_reg]] |>
#         sf::st_drop_geometry() |>
#         write_csv(paste0(type_reg, ".csv"))
#     })

### join with min grid ----
effort_t_type_cropped_cnt_join_grid <-
  purrr::map(effort_t_type_cropped_cnt,
      \(x)
      # have to use data.frame, to avoid
      # Error: y should not have class sf; for spatial joins, use st_join
      inner_join(x, data.frame(grid),
                 by = join_by(cell_id)))

# print_df_names(effort_t_type_cropped_cnt_join_grid$A)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, cell_id, StatZone, LONGITUDE, LATITUDE, vsl_cnt, trip_id_cnt, x"

# effort_t_type_cropped_cnt_join_grid$CHARTER
map_df(effort_t_type_cropped_cnt_join_grid, dim) |>
  dplyr::glimpse()

data_overview(effort_t_type_cropped_cnt_join_grid$HEADBOAT.gom_dual)

effort_t_type_cropped_cnt_join_grid$HEADBOAT.gom_dual |>
  dplyr::select(LATITUDE, LONGITUDE) |>
  dplyr::distinct()

map_trips_types <-
  names(effort_t_type_cropped_cnt_join_grid) |>
  purrr::map(
    function(charter_headb) {
      # browser()
      trip_type_name_0 <- stringr::str_split(charter_headb, "\\.")
      trip_type_name <- tolower(trip_type_name_0[[1]][[1]])
      make_map_trips(
      effort_t_type_cropped_cnt_join_grid[[charter_headb]],
      shape_data = st_union_GOMsf,
      total_trips_title = "total trips",
      trip_cnt_name = "trip_id_cnt",
      caption_text = str_glue("Heat map of SEFHIER {trip_type_name} trips (5 min. resolution).\n 2022. GoM and dual permitted vessels"),
      unit_num = 0.95
    )}
  )

map_trips_types[[1]]

