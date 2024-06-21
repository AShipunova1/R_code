# setup ----

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "get_db_data"
input_path <- file.path(my_paths$inputs, current_project_name)

# err msg if no connection, but keep running
try(con <- connect_to_secpr())
# con <- connect_to_secpr()

# get data from db ----
# RDS (R Data Serialization) files are a common format for saving R objects in RStudio, and they allow you to preserve the state of an object between R sessions.

## permit ----
file_name_permits <-
  file.path(input_path, "permit_info.rds")

mv_sero_fh_permits_his_query <-
  "select * from
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
"

permit_info_fun <-
  function(mv_sero_fh_permits_his_query) {
    dbGetQuery(con,
               mv_sero_fh_permits_his_query) %>%
      return()

  }

get_permit_info <-
  function() {
      read_rds_or_run(file_name_permits,
                      mv_sero_fh_permits_his_query,
                      permit_info_fun,
                      force_from_db)
  }
# 2023-09-20 run the function: 40.74 sec elapsed

### permit + vessel from db ----
# permit_vessel_query_exp21_query <-
# "select * from srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
# join safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
# ON (v.sero_official_number = p.vessel_id)
# where expiration_date > TO_DATE('01-JAN-21')
# "
# and top in ('CDW', 'CHS', 'SC')

# permit_vessel_query_exp21 <- dbGetQuery(con,
                          # permit_vessel_query_exp21_query)

# View(permit_vessel_query_exp21)

# add sa gom field

# names(permit_vessel_query_exp21) <-
  # make.unique(names(permit_vessel_query_exp21), sep = "_")

# print_df_names(permit_vessel_query_exp21)

# permit_vessel_query_exp21 %>%
  # filter(!(VESSEL_ID == SERO_OFFICIAL_NUMBER)) %>%
  # dim()
# 0

# Logbooks
# get trips info ----

trips_file_name <-
    file.path(input_path, "trips.rds")

trips_query <-
  "SELECT
  *
FROM
  safis.trips@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND CURRENT_DATE
  )
ORDER BY
  trip_end_date DESC
"

trips_fun <- function(trips_query) {
  return(dbGetQuery(con,
             trips_query))
}

get_trips_info <-
  function() {
      read_rds_or_run(trips_file_name,
                      trips_query,
                      trips_fun,
                      force_from_db)
  }
# 2023-09-20 run the function: 33.02 sec elapsed

# grep("long", names(trips_info), ignore.case = T, value = T)
# 0

# latitude/longitude ----
# select * from safis.EFFORTS@secapxdv_dblk.sfsc.noaa.gov;

trip_coord_query <- "
  SELECT
  trip_id,
  area_code,
  sub_area_code,
  distance_code,
  fishing_hours,
  latitude,
  longitude,
  local_area_code,
  in_state,
  avg_depth_in_fathoms,
  e.de e_de,
  e.ue e_ue,
  e.dc e_dc,
  e.uc e_uc,
  anything_caught_flag,
  depth,
  minimum_bottom_depth,
  maximum_bottom_depth,
  fishing_gear_depth,
  ten_minute_square_list,
  trip_type,
  supplier_trip_id,
  days_at_sea,
  t.de t_de,
  t.ue t_ue,
  t.dc t_dc,
  t.uc t_uc,
  vessel_id,
  cf_permit_id,
  trip_start_date,
  port,
  state,
  trip_end_date,
  trip_end_time,
  trip_start_time,
  submit_method,
  activity_type,
  end_port,
  start_port,
  sero_vessel_permit,
  sea_time
FROM
       safis.efforts@secapxdv_dblk.sfsc.noaa.gov e
  JOIN safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  USING ( trip_id )
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND CURRENT_DATE
  )
"

trip_coord_file_name <-
    file.path(input_path, "trip_coord.rds")

trip_coord_fun <- function(trip_coord_query) {
  return(dbGetQuery(con,
             trip_coord_query))
}

get_trip_coord_info <-
  function() {
      read_rds_or_run(trip_coord_file_name,
                      trip_coord_query,
                      trip_coord_fun,
                      force_from_db)
  }

# 2023-09-20 run the function: 30.94 sec elapsed

# DNF reports
# get trip neg ----

trip_neg_2022_file_path <-
  file.path(input_path, "trip_neg_2022.rds")

trip_neg_2022_query <-
  "SELECT *
  FROM
    safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
  , 'dd-mon-yy') )"
  # WHERE
  #   trip_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  #   OR trip_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')"
# 1495929

trip_neg_2022_fun <-
  function(trip_neg_2022_query) {
    return(dbGetQuery(con, trip_neg_2022_query))
  }

# trip_neg_query_2022: 201.21 sec elapsed
# trip_neg_query_2022: 60.06 sec elapsed
# trip_neg_query_2022: 89.38 sec elapsed

get_trip_neg_2022 <-
  function() {
    read_rds_or_run(trip_neg_2022_file_path,
                    trip_neg_2022_query,
                    trip_neg_2022_fun,
                    force_from_db)
  }
# run the function: 98.23 sec elapsed

# Declarations
# trips_notifications ----
trips_notifications_2022_query <-
  "SELECT
 *
FROM
  safis.trip_notifications@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
  , 'dd-mon-yy') )
  OR ( trip_end_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
  , 'dd-mon-yy') )
"

trips_notifications_2022_file_path <-
  file.path(input_path, "trips_notifications_2022.rds")

trips_notifications_2022_fun <-
  function(trips_notifications_2022_query) {
    return(dbGetQuery(con, trips_notifications_2022_query))
  }
# trips_notifications_query: 52.08 sec elapsed
# 97279
# trips_notifications_query: 7.65 sec elapsed

get_trips_notifications_2022 <-
  function() {
    read_rds_or_run(
      trips_notifications_2022_file_path,
      trips_notifications_2022_query,
      trips_notifications_2022_fun,
      force_from_db
    )
  }
# 2023-07-15 run the function: 13.41 sec elapsed

# get_vessels with permits 2021-- ----

dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
  AND effective_date <= CURRENT_DATE
"

vessels_permits_query <-
  str_glue("SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = sero_official_number )
WHERE {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = coast_guard_nbr )
WHERE
  {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = state_reg_nbr )
WHERE
{dates_filter}
")

vessels_permits_file_path <- file.path(input_path, "vessels_permits.rds")

vessels_permits_fun <-
  function(vessels_permits_query) {
    return(dbGetQuery(con,
                      vessels_permits_query))
  }

get_vessels_permits <-
  function() {
    read_rds_or_run(vessels_permits_file_path,
                    vessels_permits_query,
                    vessels_permits_fun,
                    force_from_db) |>
      vessels_permits_id_clean()
  }
# 2023-09-20 run the function: 14.08 sec elapsed

# find \\0 column ----
# get vessels
# can't because of "\\0"
# use:
# replace(VESSEL_NAME, chr(0), '') VESSEL_NAME,

#
# field_names <-
#   c("VESSEL_ID",
#     "COUNTY_CODE",
#     "STATE_CODE",
#     "ENTRY_DATE",
#     "SUPPLIER_VESSEL_ID",
#     "PORT_CODE",
#     "HULL_ID_NBR",
#     "COAST_GUARD_NBR",
#     "STATE_REG_NBR",
#     "REGISTERING_STATE",
#     # "VESSEL_NAME",
#     "PASSENGER_CAPACITY",
#     "VESSEL_TYPE",
#     "YEAR_BUILT",
#     "UPDATE_DATE",
#     "PRIMARY_GEAR",
#     "OWNER_ID",
#     "EVENT_ID",
#     "DE",
#     "UE",
#     "DC",
#     "UC",
#     "STATUS",
#     "SER_ID",
#     "UPDATED_FLAG",
#     "SERO_HOME_PORT_CITY",
#     "SERO_HOME_PORT_COUNTY",
#     "SERO_HOME_PORT_STATE",
#     "SERO_OFFICIAL_NUMBER")
#
# vessels_zero_query <-
#   "select
#   distinct {field_name}
#   from
#   safis.vessels@secapxdv_dblk.sfsc.noaa.gov"
#
# rr <-
#   map(field_names,
#     function(field_name) {
#       print(str_glue("field_name = {field_name}"))
#       q <- str_glue(vessels_zero_query)
#       tic("vessels_all1")
#       vessels_all <- dbGetQuery(con,
#                                 q)
#       toc()
#       return(dim(vessels_all))
#     }
# )
#
# # \\0 err:
# #   field_name = VESSEL_NAME
#
#
# # distinct vessel_id ok
# tic("vessels_all1")
# vessels_all <- dbGetQuery(con,
#                           vessels_zero_query)
# toc()
#
#
#
# dim(permit_info)
# dim(trip_neg_2022)
# dim(trips_notifications_2022)
# dim(trips_info_2022)
# dim(vessels_all)

# dates_2022 ----
dates_2022_query <-
  "SELECT
  dd.year,
  dd.month_of_year,
  dd.week_of_year,
  dd.complete_date
FROM
  srh.dim_dates@secapxdv_dblk.sfsc.noaa.gov dd
WHERE
  dd.complete_date BETWEEN '01-DEC-2021' AND '31-JAN-2023'
"

dates_2022_file_path <- file.path(input_path, "dates_2022.rds")

dates_2022_fun <-
  function(dates_2022_query) {
    return(dbGetQuery(con,
                      dates_2022_query))
  }

get_dates_2022 <- function() {
  read_rds_or_run(dates_2022_file_path,
                  dates_2022_query,
                  dates_2022_fun,
                  force_from_db)
}

# get override data ----
compl_err_query <-
    "SELECT
  *
FROM
       srh.srfh_vessel_comp_err@secapxdv_dblk.sfsc.noaa.gov
  INNER JOIN srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
  USING ( srh_vessel_comp_id )
WHERE
  comp_year > '2020'
"
# common fields
#   SRH_VESSEL_COMP_ID
# CREATED_DT
# CREATED_USER_ID
# LU_DT
# LU_USER_ID

get_compl_err_data_from_db <-
  function(compl_err_query) {
    compl_err_db_data_0 =
      ROracle::dbGetQuery(con,                                       compl_err_query)

    compl_err_db_data_1 <-
      compl_err_db_data_0 %>%
      # remove duplicated columns
      select(-c(CREATED_DT,
                CREATED_USER_ID,
                LU_DT,
                LU_USER_ID))

    return(compl_err_db_data_1)
  }

file_name_overr <-
  file.path(input_path, "compl_err_db_data_raw.rds")

get_compl_err_db_data <- function() {
  compl_err_db_data_raw <-
    read_rds_or_run(file_name_overr,
                    compl_err_query,
                    get_compl_err_data_from_db,
                    force_from_db)
  # 2023-09-20 run the function: 14.99 sec elapsed

  compl_err_db_data <- clean_headers(compl_err_db_data_raw)

  return(compl_err_db_data)
}

# get metric_tracking_no_srhs ----
source(file.path(my_paths$git_r,
                 "get_data_from_fhier",
                 "metric_tracking_no_srhs.R"))

# dim(fhier_reports_metrics_tracking_not_srhs_ids)
# 4063

# --- main ----
run_all_get_db_data <-
  function() {
    # browser()
    result_l = list()

    mv_sero_fh_permits_his <- get_permit_info()
    result_l[["mv_sero_fh_permits_his"]] <- mv_sero_fh_permits_his
    # dim(mv_sero_fh_permits_his)
    # [1] 183204     22

    trips_info <- get_trips_info()
    result_l[["trips_info"]] <- trips_info
    # dim(trips_info)
    # [1] 98528    72 2022
    # [1] 142037     72 2021--

    trip_coord_info <- get_trip_coord_info()
    result_l[["trip_coord_info"]] <- trip_coord_info
    # dim(trip_coord_info)
    # [1] 141350     41

    trip_neg_2022 <- get_trip_neg_2022()
    result_l[["trip_neg_2022"]] <- trip_neg_2022
    # dim(trip_neg_2022)
    # Rows: 1,495,929
    # [1] 746087     12
    # [1] 747173     12

    trips_notifications_2022 <- get_trips_notifications_2022()
    result_l[["trips_notifications_2022"]] <- trips_notifications_2022
    # dim(trips_notifications_2022)
    # Rows: 129,701
    # [1] 70056    33

    vessels_permits <- get_vessels_permits()
    result_l[["vessels_permits"]] <- vessels_permits
    # dim(vessels_permits)
    # [1] 78438    51

    dates_2022 <- get_dates_2022()
    result_l[["dates_2022"]] <- dates_2022
    # dim(dates_2022)
    # 427 4

    compl_err_db_data <- get_compl_err_db_data()
    result_l[["compl_err_db_data"]] <- compl_err_db_data
    # dim(compl_err_db_data)
    # [1] 99832    38

    return(result_l)
  }

force_from_db <- NULL
# force_from_db <- "YES"
# tic("run_all_get_db_data()")
# all_get_db_data_result_l <- run_all_get_db_data()
# toc()
# reading RDS
# run_all_get_db_data(): 1.69 sec elapsed
# reading from db
# run_all_get_db_data(): 259.81 sec elapsed ~ 4 min

# str(all_get_db_data_result_l[["compl_err_db_data"]])
# 'data.frame':	99832 obs. of  38 variables:

### check ----
# names(all_get_db_data_result_l) |>
#   map(\(df_name) {
#     c(df_name, dim(all_get_db_data_result_l[[df_name]]))
#   })

# force_from_db <- "NULL"
# dates_2022 <- get_dates_2022()


# close the db connection ----
try(ROracle::dbDisconnect(con))

