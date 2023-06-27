#  Questions ----
# FHIER / Reports
# TRIP NOTIFICATIONS (HAIL-OUT) EXTENDED -- REPORT OF DATA FOUND IN THE TRIP NOTIFICATIONS TABLE WITH LIST OF VALUE ITEMS EXTENDED UPON TO SHOW ADDITIONAL INFORMATION.
# or
# TRIP NOTIFICATIONS (HAIL-OUTS) -- REPORT OF DATA FOUND IN THE TRIP NOTIFICATIONS TABLE.
# or
# SRHS TRIP NOTIFICATIONS (HAIL-OUT) EXTENDED -- REPORT OF DATA FOUND IN THE TRIP NOTIFICATIONS TABLE FOR VESSELS IN THE HEADBOAT SURVEY WITH LIST OF VALUE ITEMS EXTENDED UPON TO SHOW ADDITIONAL INFORMATION.
# or
# GOM TRIP NOTIFICATIONS BY ARRIVAL PORT STATE -- GOM TRIP NOTIFICATIONS BY ARRIVAL PORT WHERE THE TRIP NOTIFICATION WAS NOT CANCELLED.

# data base questions
# udp.v_sero_oth_prm_period_his@secpr_dblk
# 1) what are end_date, expiration_date, last_expiration_date
# 2) select distinct permit_status from
          # udp.v_sero_oth_prm_period_his@secpr_dblk;
# -- MV_SAFIS_GOM_VESSELS
# select distinct permit_status
# "PERMIT_STATUS"
# "EXPIRED"
# "TERMINATED"
# "TRANSFERRED"
# "PENDING VESSEL SOLD"
# "RENEWED"
# "DUPLICATED"
# "VESSEL SOLD"
# "UPDATED"
# "VESSEL LEASED"
# "MAILED"
# "SURRENDERED"


# Read.me ----
# Ken Brennan has requested the 2022 number of fishing intended trips with effort for just trips landing at a GOM state (west coast only of FL), by state, for GOM permitted SEFHIER vessels. 
# 
# I think you can just use the FHIER report "Trip notifications (hail-outs) extended (declaration data from SAFIS). From there, you'd want to filter out any non-GOM permitted vessels, parse for just fishing intended charter and headboat trips (A & H), and then filter those declarations into state bins to count (using the landing location info) - to get the total # for just 2022.
# 
# Taking a quick look at that csv file (trip notifications (hail-out) extended), a lot of the state abbreviations appear to be missing.  So, I think finding numbers by state will be the most complicated part of this data request. Perhaps parsing by County and then matching county -> corresponding state would be the best bet. This would make just grabbing west coast FL landing locations possible as well. 
# 
# I told Ken we'd get this back to him by COB Friday, but let me know if you need more time. You can also pull from the DB, if that is easiest for you. 

# But don't forget to filter out any non-GOM permitted vessels from the pulled data. There'd need to be some comparison to the 2022 permitted vessels list, and an innerjoin() of something to exclude those in the report that are not in fact permitted. I'm sure you know this, but just putting it here in case. ;)

# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# get data ----
# 01/01/2022 - 12/31/2022

data_from_fhier_extended <- read_csv(
r"(my_inputs\fishing_trips_GOM_2022\Trip Notifications (Hail-Outs) Extended.csv)")
# Rows: 65004 Columns: 31                                                                               
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr (22): NOTIFICATION_TYPE_NAME, SYSTEM_ID, TRIP_TYPE, TRIP_TY...
# dbl  (7): NOTIFICATION_SEQ, NOTIFICATION_TYPE_ID, ACCSP_PERMIT_...
# lgl  (2): CANCEL_FLAG, TRIP_START_DATE_TIME

# ---
# FHIER / Reports / GOM TRIP NOTIFICATIONS BY ARRIVAL PORT STATE
# 51641
# Filters in FHIER
# INTENDED_FISHING_FLAG = 'YES'
# TRIP_TYPE in 'CHARTER, HEADBOAT'
data_from_fhier_GOM <-
  read_csv(r"(my_inputs\fishing_trips_GOM_2022\Trip Notifications Detail.csv)")
# Rows: 51641 Columns: 18                                                                               
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr (16): NOTIFICATION_TYPE, VESSEL_OFFICIAL_NUMBER, VESSEL_NAM...
# dbl  (2): NOTIFICATION_SEQ, LANDING_LOCATION

print_df_names(data_from_fhier_GOM)

# data from db ----
con <- connect_to_secpr()

q_file_name <- r"(my_inputs\fishing_trips_GOM_2022\MV_SAFIS_GOM_VESSELS.sql)"

MV_SAFIS_GOM_VESSELS_w_permit_status_query <- 
  readr::read_file(q_file_name)

data_from_db1 <-
  dbGetQuery(con, MV_SAFIS_GOM_VESSELS_w_permit_status_query)

# dim(data_from_db1)
# 19451     
data_from_db1 %>%
  # filter(EFFECTIVE_DATE < '2022-01-01') %>% # 14
  filter(EFFECTIVE_DATE >= '2022-01-01') %>% # 2004
  filter(END_DATE > '2022-12-31') %>% # [1] 2018 vessels
  dim()

data_from_db1

# "ACTIVITY_TYPE"
# 81
# null ?
# 3 ?
# 0
# 80

# V_SAFIS_TRIP_DOWNLOAD.sql
# DECODE(t.activity_type, 0, 'TRIP WITH EFFORT', 80, 'TRIP UNABLE TO FISH', 81, 'TRIP NO INTENTION OF FISHING') AS activity_type_name,

q_file_name_all_info <- r"(my_inputs\fishing_trips_GOM_2022\gom_landing_2022.sql)"
# 962 in db

gom_landing_2022_query <- 
  readr::read_file(q_file_name_all_info)

# remove semicolon at the end of the query!
data_from_db2 <-
  dbGetQuery(con, gom_landing_2022_query)

dim(data_from_db2)
# 963  

# head(sort(unique(data_from_db1$SAFIS_VESSEL_ID)))
# head(sort(unique(data_from_db_2$SAFIS_VESSEL_ID)))

data_from_db1_2022 <-
  data_from_db1 %>% 
  filter(END_DATE > '2022-12-31')

SAFIS_VESSEL_ID_1_only <-
setdiff(unique(data_from_db1_2022$SAFIS_VESSEL_ID),
        unique(data_from_db_2$SAFIS_VESSEL_ID))

SAFIS_VESSEL_ID_1_only %>% 
  as.data.frame() %>% 
  write_csv("vessel_ids_to_check.csv")
  # length()

# 876

SAFIS_VESSEL_ID_1_only_str <- 
  paste0(SAFIS_VESSEL_ID_1_only, collapse = ', ')

# str(SAFIS_VESSEL_ID_1_only_str)
# check missing ones
missing_vessels_query <-
  paste0(
    "SELECT
  vessel_id, coast_guard_nbr, state_reg_nbr, vessel_name, sero_official_number
FROM
  safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  where vessel_id in (
",
SAFIS_VESSEL_ID_1_only_str,
")"
  )

# str(missing_vessels_query)

data_from_db_missing_vessels <-
  dbGetQuery(con, missing_vessels_query)

dim(data_from_db_missing_vessels)
# 876