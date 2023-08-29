# get data ----
# input_path <- file.path(my_paths$inputs, current_project_name)
input_path <- file.path(my_paths$inputs, "db_compliance")

## permit ----
file_name_permits <-
  file.path(input_path, "permit_info.rds")
  # r"(my_outputs\from_db\mv_sero_fh_permits_his.rds)"

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

mv_sero_fh_permits_his <-
  read_rds_or_run(file_name_permits,
                  mv_sero_fh_permits_his_query,
                  permit_info_fun
                  )

# mv_sero_fh_permits_his <-
  # read_csv(file_name_permits)
# Rows: 181032 Columns: 22
# dim(mv_sero_fh_permits_his)
# [1] 181689     22

### the same from another db table (to compare) ----

# udp_v_sero_oth_prm_period_his_query <-
#   "select * from
#   udp.v_sero_oth_prm_period_his@secpr_dblk
# "
#
# permit_info_udp <- dbGetQuery(con,
#                           udp_v_sero_oth_prm_period_his_query)
#
# write all as binary
# readr::write_rds(permit_info_udp,
#                  file.path(input_path, "permit_info_udp.rds"))

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
# get trips info for 2022 ----

trips_22_file_name <-
    file.path(input_path, "trips_22.rds")

trips_22_query <-
  "SELECT
  *
FROM
  safis.trips@secapxdv_dblk.sfsc.noaa.gov
WHERE
  ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
  , 'dd-mon-yy') )
  OR ( trip_end_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
  , 'dd-mon-yy') )
ORDER BY
  trip_end_date DESC
"

trips_22_fun <- function(trips_22_query) {
  return(dbGetQuery(con,
             trips_22_query))
}

trips_info_2022 <-
  read_rds_or_run(trips_22_file_name,
                  trips_22_query,
                  trips_22_fun
                  )


# trips_info_2022: 92.95 sec elapsed
# trips_info_2022: 39.97 sec elapsed
# run the function: 35.42 sec elapsed

dim(trips_info_2022)
# Rows: 205,772
# [1] 98449    72
# [1] 98528    72

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
    return(dbGetQuery(con, trip_neg_query_2022))
  }

# trip_neg_query_2022: 201.21 sec elapsed
# trip_neg_query_2022: 60.06 sec elapsed
# trip_neg_query_2022: 89.38 sec elapsed

trip_neg_2022 <-
  read_rds_or_run(trip_neg_2022_file_path,
                  trip_neg_2022_query,
                  trip_neg_2022_fun
                  )
# run the function: 98.23 sec elapsed


dim(trip_neg_2022)
# Rows: 1,495,929
# [1] 746087     12
# [1] 747173     12

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

trips_notifications_2022_fun <- function(trips_notifications_2022_query) {
  return(dbGetQuery(con, trips_notifications_2022_query))
}
# trips_notifications_query: 52.08 sec elapsed
# 97279
# trips_notifications_query: 7.65 sec elapsed

trips_notifications_2022 <-
  read_rds_or_run(
    trips_notifications_2022_file_path,
    trips_notifications_2022_query,
    trips_notifications_2022_fun
  )
# 2023-07-15 run the function: 13.41 sec elapsed

dim(trips_notifications_2022)
# Rows: 129,701
# [1] 70056    33

# get_vessels wtih permits 2022 ----
vessels_permits_2022_query <-
  "SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = sero_official_number )
WHERE
  ( end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
  AND effective_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = coast_guard_nbr )
WHERE
  ( end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
  AND effective_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = state_reg_nbr )
WHERE
  ( end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
  AND effective_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')"

vessels_permits_2022_file_path <- file.path(input_path, "vessels_permits_2022.rds")

vessels_permits_2022_fun <-
  function(vessels_permits_2022_query) {
    return(dbGetQuery(con,
                      vessels_permits_2022_query))
  }

vessels_permits_2022 <-
  read_rds_or_run(
    vessels_permits_2022_file_path,
    vessels_permits_2022_query,
    vessels_permits_2022_fun
  )
# 2023-07-15 run the function: 13.33 sec elapsed

# get vessels ----
# can't because of "\\0"
# use:
# replace(VESSEL_NAME, chr(0), '') VESSEL_NAME,

# # get vessels and trips 2022 ----
# # to have vessel official number
# vessels_trips_22_file_name <-
#     file.path(input_path, "vessels_trips_22.rds")
#
# vessels_trips_22_query <-
#   "SELECT
#   *
# FROM
#   safis.trips@secapxdv_dblk.sfsc.noaa.gov
#   join
#   safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   using(VESSEL_ID)
# WHERE
#   ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
#   , 'dd-mon-yy') )
#   OR ( trip_end_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
#   , 'dd-mon-yy') )
# ORDER BY
#   trip_end_date DESC
# "
# vessels_trips_22_fun <- function(vessels_trips_22_query) {
#   return(dbGetQuery(con,
#              vessels_trips_22_query))
# }
#
# vessels_trips_info_2022 <-
#   read_rds_or_run(vessels_trips_22_file_name,
#                   vessels_trips_22_query,
#                   vessels_trips_22_fun
#                   )
# # 2023-07-24 run the function: 39.3 sec elapsed
#
# # get trip neg and vessels 2022 ----
# # to have vessel official number
# vessels_trip_neg_2022_file_name <-
#     file.path(input_path, "vessels_trip_neg_2022.rds")
#
# vessels_trip_neg_2022_query <-
#   "SELECT
#   tne.*,
#   COUNTY_CODE,
#   STATE_CODE,
#   ENTRY_DATE,
#   SUPPLIER_VESSEL_ID,
#   PORT_CODE,
#   HULL_ID_NBR,
#   COAST_GUARD_NBR,
#   STATE_REG_NBR,
#   REGISTERING_STATE,
#   replace(VESSEL_NAME, chr(0), '') VESSEL_NAME,
#   PASSENGER_CAPACITY,
#   VESSEL_TYPE,
#   YEAR_BUILT,
#   UPDATE_DATE,
#   PRIMARY_GEAR,
#   OWNER_ID,
#   v.EVENT_ID as EVENT_ID_V,
#   v.DE as DE_V,
#   v.UE as UE_V,
#   v.DC as DC_V,
#   v.UC as UC_V,
#   v.STATUS as STATUS_V,
#   SER_ID,
#   UPDATED_FLAG,
#   SERO_HOME_PORT_CITY,
#   SERO_HOME_PORT_COUNTY,
#   SERO_HOME_PORT_STATE,
#   SERO_OFFICIAL_NUMBER
# FROM
#        safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
#   on ( tne.vessel_id = v.vessel_id )
# WHERE
#   ( trip_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22',
#   'dd-mon-yy') )"
#
# vessels_trip_neg_2022_fun <- function(vessels_trip_neg_2022_query) {
#   return(dbGetQuery(con,
#              vessels_trip_neg_2022_query))
# }
#
# vessels_trip_neg_2022 <-
#   read_rds_or_run(vessels_trip_neg_2022_file_name,
#                   vessels_trip_neg_2022_query,
#                   vessels_trip_neg_2022_fun
#                   )
# # 2023-07-24 run the function: 125.96 sec elapsed
#
# # get vessels and trip notifications 2022 ----
# # to have vessel official number
# vessels_trips_notifications__file_name <-
#     file.path(input_path, "vessels_trips_notifications_.rds")
#
# vessels_trips_notifications__query <-
#   "SELECT
#   *
# FROM
#   safis.trips_notifications@secapxdv_dblk.sfsc.noaa.gov
#   join
#   safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   using(VESSEL_ID)
# WHERE
#   ( trip_start_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
#   , 'dd-mon-yy') )
#   OR ( trip_end_date BETWEEN TO_DATE('01-JAN-22', 'dd-mon-yy') AND TO_DATE('31-DEC-22'
#   , 'dd-mon-yy') )
# ORDER BY
#   trip_end_date DESC
# "
# vessels_trips_notifications__fun <-
#   function(vessels_trips_notifications__query) {
#     return(dbGetQuery(con,
#                       vessels_trips_notifications__query))
#   }
#
# vessels_trips_notifications_2022 <-
#   read_rds_or_run(vessels_trips_notifications__file_name,
#                   vessels_trips_notifications__query,
#                   vessels_trips_notifications__fun
#                   )
# # 2023-07-24 run the function: 12.61 sec elapsed
#
#
# # vessels_all <-
# # Error in .oci.GetQuery(conn, statement, data = data, prefetch = prefetch,  :
# #   Error in try({ : embedded nul in string: '\0'
#
# # > dbGetQuery(cnx, "select replace(s, chr(0), '') s from test_00")
# # dat <- dbGetQuery(myConnection,"SELECT REPLACE(COLUMN_NAME, CHR(0), ' ') AS NEW_COLUMN
# #                                 FROM MY_TABLE")
#
# # tic("vessels_all")
# # vessels_all <- dbGetQuery(con,
# #                           vessels_query)
# # toc()
# # Error in try({ : embedded nul in string: '\0'
#
# # dim(vessels_all)
# # vessels_all_file_path <- file.path(input_path, "vessels.csv")
#
# # vessels_all <- read_csv(vessels_all_file_path)
# # Rows: 140405 Columns: 29
# # ── Column specification ─────────────────────────────────────────────────────────
# # Delimiter: ","
# # chr (22): COUNTY_CODE, STATE_CODE, ENTRY_DATE, SUPPLIER_VESSEL_ID, PORT_CODE,...
# # dbl  (6): VESSEL_ID, PASSENGER_CAPACITY, YEAR_BUILT, OWNER_ID, SER_ID, UPDATE...
# # lgl  (1): VESSEL_TYPE
#
# # Where is \\0 : vessel_name ----
# # tic("vessels_zero")
# # print_df_names(vessels_trips_notifications_2022 )
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
# # dim(permit_info)
# # dim(trip_neg_2022)
# # dim(trips_notifications_2022)
# # dim(trips_info_2022)
# # dim(vessels_all)
#
# dates_2022 ----
# "SELECT * FROM
#   srh.dim_dates@secapxdv_dblk.sfsc.noaa.gov
# WHERE
#   complete_date BETWEEN '01-jan-2022' AND '31-DEC-2022'
# "

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
# dd.complete_date BETWEEN '01-DEC-2022' AND '31-JAN-2023'
# TO_DATE('01-JAN-22', 'dd-mon-yy')

dates_2022_file_path <- file.path(input_path, "dates_2022.rds")

dates_2022_fun <-
  function(dates_2022_query) {
    return(dbGetQuery(con,
                      dates_2022_query))
  }

dates_2022 <-
  read_rds_or_run(
    dates_2022_file_path,
    dates_2022_query,
    dates_2022_fun
  )


# tic("dates_2022_query")
# dates_2022 <- dbGetQuery(con,
#                          dates_2022_query)
# toc()

glimpse(dates_2022)
# Rows: 427

# write_rds(dates_2022,
#           file.path(input_path, "dates_2022.rds"))

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
    
    # ROracle::dbDisconnect(con)
    
    return(compl_err_db_data_1)
  }

# tic("get_compl_err_data_from_db()")
# compl_err_db_data_raw <- get_compl_err_data_from_db()
# toc()

file_name_overr <-
  file.path(input_path, "compl_err_db_data_raw.rds")

compl_err_db_data_raw <-
  read_rds_or_run(file_name_overr,
                  compl_err_query,
                  get_compl_err_data_from_db)
# 2023-08-08 run the function: 19.05 sec elapsed
# 2023-08-08 run the function: 22.67 sec elapsed

compl_err_db_data <- clean_headers(compl_err_db_data_raw)

# get metric_tracking_no_srhs
source(file.path(my_paths$git_r,
                 "get_data_from_fhier",
                 "metric_tracking_no_srhs.R"))

# Clean vessels ----
metricks_not_srhs_ids_2022 <-
  fhier_reports_metrics_tracking_not_srhs_ids_list[[1]]

dim(metricks_not_srhs_ids_2022)
# [1] 3571    1

## weird headers ----
# print_df_names(vessels_permits_2022)
vessels_permits_2022_c <-
  vessels_permits_2022 |>
  rename("PERMIT_VESSEL_ID" = "QCSJ_C000000000300000") |>
  rename("VESSEL_VESSEL_ID" = "QCSJ_C000000000300001")

dim(vessels_permits_2022_c)
# [1] 40474    51
 
## fhier_metrics vessels only ----
vessels_permits_2022_c_me <-
  vessels_permits_2022_c |>
  filter(
    PERMIT_VESSEL_ID %in% metricks_not_srhs_ids_2022$vessel_official_number
  )

dim(vessels_permits_2022_c_me)
# [1] 29656    51
