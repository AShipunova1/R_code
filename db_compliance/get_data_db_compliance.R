# get data ----
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

### the same from another db table (to compare) ----

udp_v_sero_oth_prm_period_his_query <-
  "select * from
  udp.v_sero_oth_prm_period_his@secpr_dblk
"

permit_info_udp <- dbGetQuery(con,
                          udp_v_sero_oth_prm_period_his_query)


### permit + vessel from db ----
permit_vessel_query_exp21_query <-
"select * from srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
join safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
ON (v.sero_official_number = p.vessel_id)
where expiration_date > TO_DATE('01-JAN-21')
"
# and top in ('CDW', 'CHS', 'SC')

permit_vessel_query_exp21 <- dbGetQuery(con,
                          permit_vessel_query_exp21_query)

View(permit_vessel_query_exp21)

# add sa gom field

names(permit_vessel_query_exp21) <-
  make.unique(names(permit_vessel_query_exp21), sep = "_")

# print_df_names(permit_vessel_query_exp21)

permit_vessel_query_exp21 %>%
  filter(!(VESSEL_ID == SERO_OFFICIAL_NUMBER)) %>%
  dim()
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

  

trips_info_2022 <- dbGetQuery(con,
                          trips_22_query)

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
trip_notifications_query_2022 <- dbGetQuery(con,
                          trip_notifications_query)
toc()
trip_notifications_query: 52.08 sec elapsed
