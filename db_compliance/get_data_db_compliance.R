# get data ----
input_path <- file.path(my_paths$inputs, current_project_name)

## permit ----
file_name_permits <- r"(my_outputs\from_db\mv_sero_fh_permits_his.csv)"

mv_sero_fh_permits_his <-
  read_csv(file_name_permits)
# Rows: 181032 Columns: 22
# ── Column specification ─────────────────────────────────────
# Delimiter: ","
# chr (14): VESSEL_ID, EXPIRATION_DATE, TOP, PERMIT, EFFECT...

# View(mv_sero_fh_permits_his)

### the same from db ----

mv_sero_fh_permits_his_query <-
  "select * from
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
"

permit_info <- dbGetQuery(con,
                          mv_sero_fh_permits_his_query)

# View(permit_info)
write_csv(permit_info, file.path(input_path, "permit_info.csv"))

### the same from another db table (to compare) ----

udp_v_sero_oth_prm_period_his_query <-
  "select * from
  udp.v_sero_oth_prm_period_his@secpr_dblk
"

permit_info_udp <- dbGetQuery(con,
                          udp_v_sero_oth_prm_period_his_query)

write_csv(permit_info_udp, file.path(input_path, "permit_info.csv"))

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

# get trips info for 2022 ----

trips_22_query <-
  "SELECT
 *
FROM
  safis.trips@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_start_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  OR trip_end_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
"

tic("trips_info_2022")
trips_info_2022 <- dbGetQuery(con,
                          trips_22_query)
toc()
# trips_info_2022: 92.95 sec elapsed
# glimpse(trips_info_2022)
# Rows: 205,772

write_csv(trips_info_2022, file.path(input_path, "trips_info_2022.csv"))

# trip_notifications ----
trip_notifications_query <-
  "SELECT
 *
FROM
  safis.trip_notifications@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_start_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  OR trip_end_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
"
# 97279

tic("trip_notifications_query")
trip_notifications_2022 <- dbGetQuery(con,
                          trip_notifications_query)
toc()
# trip_notifications_query: 52.08 sec elapsed

glimpse(trip_notifications_2022)
# Rows: 129,701

write_csv(trip_notifications_2022, 
          file.path(input_path, "trip_notifications_2022.csv"))

# get trip neg from db ----
trip_neg_query_2022 <-
  "SELECT *
  FROM
    safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov
  WHERE
    trip_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
    OR trip_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')"
# 1495929  

tic("trip_neg_query_2022")
trip_neg_2022 <- dbGetQuery(con,
                          trip_neg_query_2022)
toc()
# trip_neg_query_2022: 201.21 sec elapsed

write_csv(trip_neg_2022, 
          file.path(input_path, "trip_neg_2022.csv"))

# glimpse(trip_neg_2022)
# Rows: 1,495,929

# trip_notifications ----
trip_notifications_query <-
  "SELECT
 *
FROM
  safis.trip_notifications@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_start_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  OR trip_end_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
"
# 97279

tic("trip_notifications_query")
trip_notifications_2022 <- dbGetQuery(con,
                          trip_notifications_query)
toc()
# trip_notifications_query: 52.08 sec elapsed

# glimpse(trip_notifications_2022)
# Rows: 129,701

write_csv(trip_notifications_2022, 
          file.path(input_path, "trip_notifications_2022.csv"))

# get vessels from db ----
vessels_query <-
  "SELECT *
  FROM
    safis.vessels@secapxdv_dblk.sfsc.noaa.gov"
# Error in .oci.GetQuery(conn, statement, data = data, prefetch = prefetch,  : 
#   Error in try({ : embedded nul in string: '\0'

# > dbGetQuery(cnx, "select replace(s, chr(0), '') s from test_00")
# dat <- dbGetQuery(myConnection,"SELECT REPLACE(COLUMN_NAME, CHR(0), ' ') AS NEW_COLUMN
#                                 FROM MY_TABLE")

# tic("vessels_all")
# vessels_all <- dbGetQuery(con,
#                           vessels_query)
# toc()
# Error in try({ : embedded nul in string: '\0'

# dim(vessels_all)
vessels_all_file_path <- file.path(input_path, "vessels.csv")
# write_csv(vessels_all, vessels_all_file_path)

vessels_all <- read_csv(vessels_all_file_path)
# Rows: 140405 Columns: 29                                                                                            
# ── Column specification ─────────────────────────────────────────────────────────
# Delimiter: ","
# chr (22): COUNTY_CODE, STATE_CODE, ENTRY_DATE, SUPPLIER_VESSEL_ID, PORT_CODE,...
# dbl  (6): VESSEL_ID, PASSENGER_CAPACITY, YEAR_BUILT, OWNER_ID, SER_ID, UPDATE...
# lgl  (1): VESSEL_TYPE

# dim(permit_info)
# dim(trip_neg_2022)
# dim(trip_notifications_2022)
# dim(trips_info_2022)
# dim(vessels_all)
