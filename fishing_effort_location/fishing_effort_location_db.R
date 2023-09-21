# setup ----
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
# print_df_names(all_get_db_data_result_l)
# mv_sero_fh_permits_his, trips_info, trip_coord_info, trip_neg_2022, trips_notifications_2022, vessels_permits, dates_2022, compl_err_db_data

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols() |>
  filter(TRIP_TYPE %in% c("A", "H"))

dim(trip_coord_info)
# [1] 139504     39
# TRIP_ID, AREA_CODE, SUB_AREA_CODE, DISTANCE_CODE, FISHING_HOURS, LATITUDE, LONGITUDE, LOCAL_AREA_CODE, IN_STATE, AVG_DEPTH_IN_FATHOMS, E_DE, E_UE, E_DC, E_UC, ANYTHING_CAUGHT_FLAG, DEPTH, MINIMUM_BOTTOM_DEPTH, MAXIMUM_BOTTOM_DEPTH, FISHING_GEAR_DEPTH, TRIP_TYPE, SUPPLIER_TRIP_ID, DAYS_AT_SEA, T_DE, T_UE, T_DC, T_UC, VESSEL_ID, CF_PERMIT_ID, TRIP_START_DATE, PORT, STATE, TRIP_END_DATE, TRIP_END_TIME, TRIP_START_TIME, SUBMIT_METHOD, ACTIVITY_TYPE, END_PORT, START_PORT, SERO_VESSEL_PERMIT

# Heatmaps for charter and headboat separately ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they selected that in the logbook.

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
  select(all_of(rm_cols)) |>
  distinct()

dim(trip_coord_info_2022_short)
# [1] 96785     7

## only good lat long ----
trip_coord_info_2022_short_coord <-
  trip_coord_info_2022_short |>
  dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) |>
  distinct()

## add permit info ----
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

### join with effort ----
trip_coord_info_2022_short_vessels_permits <-
  trip_coord_info_2022_short |>
  left_join(vessels_permits_clean_gr7,
            join_by(VESSEL_ID == VESSEL_VESSEL_ID),
            relationship = "many-to-many")


View(trip_coord_info_2022_short_vessels_permits)

## separate by trip type ----
trip_coord_info_2022_short_types_l <-
  trip_coord_info_2022_short_coord |>
  split(as.factor(trip_coord_info_2022_short$TRIP_TYPE)) |>
  # remove extra columns in each df
  map(\(x)
      x |>
        dplyr::select(TRIP_ID, VESSEL_ID, LATITUDE, LONGITUDE) |>
        distinct())

str(trip_coord_info_2022_short_types_l)
# List of 2
#  $ A :'data.frame':	94560 obs. of  4 variables:
#  $ H:'data.frame':	1823  obs. of  4 variables:

## create 5 min heatmaps for both trip types ----
# trip_type_data_from_db_by_t_id_types_l

tic("effort_t_type")
effort_t_type <-
  map(trip_coord_info_2022_short_types_l, df_join_grid)
toc()
# effort_t_type: 1.66 sec elapsed

tic("effort_t_type_cropped")
effort_t_type_cropped <- map(effort_t_type, crop_by_shape)
toc()
# effort_t_type_cropped: 1.04 sec elapsed

# str(effort_t_type_cropped)

effort_t_type_cropped_cnt <-
  effort_t_type_cropped |>
  map(
    \(x)
    x |>
      group_by(cell_id) |>
      mutate(
        vsl_cnt = n_distinct(VESSEL_ID),
        trip_id_cnt = n_distinct(TRIP_ID)
      ) |>
      ungroup()
  )

map_df(effort_t_type_cropped_cnt, dim)
#   CHARTER HEADBOAT
#       A     H
# 1 35786   663
# 2     9     9

# map_df(effort_t_type_cropped_cnt, data_overview)

data_overview(effort_t_type_cropped_cnt$A)
# TRIP_ID     35777
# VESSEL_ID     819
# cell_id      3110
data_overview(effort_t_type_cropped_cnt$H)
# TRIP_ID     663
# VESSEL_ID   263
# cell_id     359

# View(grid)

### join with min grid ----
effort_t_type_cropped_cnt_join_grid <-
  map(effort_t_type_cropped_cnt,
      \(x)
      # have to use data.frame, to avoid
      # Error: y should not have class sf; for spatial joins, use st_join
      inner_join(x, data.frame(grid),
                 by = join_by(cell_id)
)
      )

# print_df_names(effort_t_type_cropped_cnt_join_grid$A)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, cell_id, StatZone, LONGITUDE, LATITUDE, vsl_cnt, trip_id_cnt, x"

# effort_t_type_cropped_cnt_join_grid$CHARTER

map_trips_types <-
  names(effort_t_type_cropped_cnt_join_grid) |>
  map(
    \(charter_headb) make_map_trips(
      effort_t_type_cropped_cnt_join_grid[[charter_headb]],
      shape_data = st_union_GOMsf,
      total_trips_title = "total trips",
      trip_cnt_name = "trip_id_cnt",
      caption_text = str_glue("Heat map of SEFHIER {tolower(charter_headb)} trips (5 min. resolution). 2022. GoM permitted vessels.")
    )
  )

map_trips_types[[1]]

