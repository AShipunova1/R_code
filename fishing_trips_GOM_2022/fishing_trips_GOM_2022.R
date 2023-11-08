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
# 2) dplyr::select distinct permit_status from
          # udp.v_sero_oth_prm_period_his@secpr_dblk;
# -- MV_SAFIS_GOM_VESSELS
# dplyr::select distinct permit_status
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
library(tictoc)
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# get data ----
# 01/01/2022 - 12/31/2022

gom_state_abbr <- c("AL", "FL", "LA", "MS", "TX")

## data from fhier ----
### GOM Trip Notifications by Arrival Port State ----
file_name_summ <- r"(my_inputs\fishing_trips_GOM_2022\gom_trip_notifications_by_arrival_port_state_summary.csv)"
  
gom_trip_notifications_by_arrival_port_state_summary <-
  read_csv(file_name_summ)
# Rows: 1 Columns: 11                                                                                 
# ── Column specification ─────────────────────────────────────────
# Delimiter: ","
# dbl (6): Rounded AL Pct, Rounded FL Pct, Rounded LA Pct, Tota...
# num (5): Total With GOM Arrival Port, Total AL, Total FL, Tot...

### Trip Notifications (Hail-Outs) Extended ---- 
data_from_fhier_extended <- read_csv(
r"(my_inputs\fishing_trips_GOM_2022\Trip Notifications (Hail-Outs) Extended.csv)")
# Rows: 65004 Columns: 31                                                                               
# ── Column specification ───────────────────────────────────────────
# Delimiter: ","
# chr (22): NOTIFICATION_TYPE_NAME, SYSTEM_ID, TRIP_TYPE, TRIP_TY...
# dbl  (7): NOTIFICATION_SEQ, NOTIFICATION_TYPE_ID, ACCSP_PERMIT_...
# lgl  (2): CANCEL_FLAG, TRIP_START_DATE_TIME

### FHIER / Reports / GOM TRIP NOTIFICATIONS BY ARRIVAL PORT STATE ----
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

# print_df_names(data_from_fhier_GOM)

#### check data_from_fhier_GOM ----
# Florida counties by region (from the Internet)
fl_counties <- list(
  "SA" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "GOM" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

data_from_fhier_GOM_fl_counties <-
  data_from_fhier_GOM %>% 
  filter(ARRIVAL_PORT_STATE == "FL") %>% 
  dplyr::select(ARRIVAL_PORT_COUNTY) %>% 
  unique()

str(data_from_fhier_GOM_fl_counties)

sa_fl_counties_in_fhier_data <-
  intersect(
    toupper(fl_counties$SA),
    toupper(data_from_fhier_GOM_fl_counties$ARRIVAL_PORT_COUNTY)
  )


## data from db ----
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
# 1) 963 in db

gom_landing_2022_query <- 
  readr::read_file(q_file_name_all_info)

# remove semicolon at the end of the query!
data_from_db4 <-
  dbGetQuery(con, gom_landing_2022_query)

# dim(data_from_db2)
# 1104

data_from_db2 %>% 
    dplyr::select(ACTIVITY_TYPE_NAME) %>% unique()
# 1    TRIP WITH EFFORT
# 2 TRIP UNABLE TO FISH

# head(sort(unique(data_from_db1$SAFIS_VESSEL_ID)))
# head(sort(unique(data_from_db_2$SAFIS_VESSEL_ID)))

data_from_db1_2022 <-
  data_from_db1 %>% 
  filter(END_DATE > '2022-12-31')

SAFIS_VESSEL_ID_1_only <-
setdiff(unique(data_from_db1_2022$SAFIS_VESSEL_ID),
        unique(data_from_db2$SAFIS_VESSEL_ID))

SAFIS_VESSEL_ID_1_only %>% 
  # as.data.frame() %>% 
  # write_csv("vessel_ids_to_check.csv")
  length()

# 873

SAFIS_VESSEL_ID_1_only_str <- 
  paste0(SAFIS_VESSEL_ID_1_only, collapse = ', ')

SAFIS_VESSEL_ID_1_only <-
setdiff(unique(data_from_db1_2022$SAFIS_VESSEL_ID),
        unique(data_from_db2$SAFIS_VESSEL_ID))


# ---

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

# 876

data_from_db_missing_vessels %>% 
  dplyr::select(SERO_OFFICIAL_NUMBER) %>% 
  unique() %>% 
  write_csv("data_from_db_missing_vessels_sero_off_nbr.csv")
  # paste0(collapse = ", ") %>% cat()

### compare with fhier ----
setdiff(unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER),
        unique(data_from_db2$SERO_OFFICIAL_NUMBER))
# 312
# FL3059CY
# no trip info in FHIER / vessel dashboard
data_from_fhier_GOM %>% 
  filter(VESSEL_OFFICIAL_NUMBER == 'FL3059CY') %>% 
  View()

## check gom_landing_2022 ----
# View(data_from_db2)

# where safis_vessel_id = '328952'
#     order by end_date desc;
#     328952	DOUBLE O	16-MAY-22	31-OCT-23	MAILED
# 328952	DOUBLE O	01-OCT-21	12-MAY-22	TRANSFERRED

# dplyr::select DISTINCT
# t.sero_vessel_permit,
# gom.end_date
# from gom_landing_2022

# "SERO_VESSEL_PERMIT","END_DATE"
# ,30-SEP-19
# ,12-MAY-22
# ,30-SEP-18
# ,30-SEP-21

## data from db without sero_vessel_permit is not null ----
data_overview(data_from_db3)
# 2318  
# SAFIS_VESSEL_ID               819
# SERO_OFFICIAL_NUMBER          818

# SAFIS_VESSEL_ID_1_only <-
setdiff(unique(data_from_db1_2022$SAFIS_VESSEL_ID),
        unique(data_from_db3$SAFIS_VESSEL_ID)) %>% 
  length()
# 654

in_fhier_only <-
  setdiff(
    unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER),
    unique(data_from_db3$SERO_OFFICIAL_NUMBER)
  )
# %>% 
#   length()
# 39

in_db_only3 <-
  setdiff(
    unique(data_from_db3$SERO_OFFICIAL_NUMBER),
    unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER)
  )
length(in_db_only)
# 28

# TODO: why the difference?

in_db_only4 <-
  setdiff(
    unique(data_from_db4$SERO_OFFICIAL_NUMBER),
    unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER)
  )
length(in_db_only4)
# 82

head(in_db_only)
# FL8519NA - has trip

head(in_fhier_only)
# FL2947LJ - twice in the vessel dashboard

data_from_fhier_GOM %>% 
  filter(VESSEL_OFFICIAL_NUMBER %in% in_fhier_only) %>% 
  View()

# SIX SHOOTER

in_fhier_only_name <-
  setdiff(
    unique(data_from_fhier_GOM$VESSEL_NAME),
    unique(data_from_db3$VESSEL_NAME)
  )
# print_df_names(data_from_db3)
length(in_fhier_only_name)
# 33
head(in_fhier_only_name)

# "SELECT
#   *
# FROM
#        safis.trips@secapxdv_dblk.sfsc.noaa.gov
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   USING ( vessel_id )
# WHERE
#   vessel_name = 'SIX PACKED'"
# 0

### check in_fhier_only_name ----
in_fhier_only_name_query0 <- 
  paste0(
"SELECT
  *
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  USING ( vessel_id )
WHERE
  vessel_name in (",
in_fhier_only_name,
")"
  )
# [30] "SELECT\n  *\nFROM\n       safis.trips@secapxdv_dblk.sfsc.noaa.gov\n  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov\n  USING ( vessel_id )\nWHERE\n  vessel_name in (DANIELLE)"         

# in_fhier_only_name_query

in_fhier_only_name_flat <-
  in_fhier_only_name %>%
  paste0(collapse = "', '")

in_fhier_only_name_query1 <- 
  paste0(
    "SELECT
  *
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  USING ( vessel_id )
WHERE
  vessel_name in ('",
in_fhier_only_name_flat,
"')",
" AND trunc(trip_start_date) BETWEEN TO_DATE('01-JAN-22') and
   TO_DATE('31-DEC-22')
"
  )

# in_fhier_only_name_query0

check_missing_vsls1 <-
  dbGetQuery(con, in_fhier_only_name_query1)

dim(check_missing_vsls)
# [1] 716 100

# TRIP_START_DATE                 
# Min.   :2021-01-06 23:00:00.00  
# 1st Qu.:2021-07-11 18:00:00.00  
# Median :2022-05-15 00:00:00.00  
# Mean   :2022-03-21 19:42:34.19  
# 3rd Qu.:2022-10-08 06:00:00.00  
# Max.   :2023-06-26 00:00:00.00  

# START_PORT       
#  Length:716

str(check_missing_vsls1)
# 'data.frame':	275 obs. of  100 variables:

check_missing_vsls1 %>%
  dplyr::select(
    SERO_OFFICIAL_NUMBER,
    COAST_GUARD_NBR,
    STATE_REG_NBR,
    VESSEL_NAME,
    END_PORT,
    ACTIVITY_TYPE,
    REPORTING_SOURCE,
    TRIP_TYPE
  ) %>%
  unique() %>%
  View()
# 14

# SERO_OFFICIAL_NUMBER 7
# $ TRIP_TYPE <chr> "A", "R", "U"
# $ ACTIVITY_TYPE <dbl> 80, 0, NA

# FL9013MV - only STATE_REG_NBR, no SERO_OFFICIAL_NUMBER!
#     SERO_OFFICIAL_NUMBER    VESSEL_NAME
# 1               FL0018NB     WATER BEAR
# 19                658805  FISHER OF MEN
# 210               590873 BLIND SQUIRREL
# 240              1251931         AMBUSH
# 242               934561      ANTHONY C
# 274                 <NA>   HEATHER LYNN
# 275             FL2597MN SALT CRACKER V

### check permits for fhier only ----
vessel_ids0 <- 'FL9013MV'
vessel_ids <- check_missing_vsls1 %>%
  dplyr::select(
    SERO_OFFICIAL_NUMBER
  ) %>%
  unique() %>% 
  {paste0(.$SERO_OFFICIAL_NUMBER, collapse = "', '")}

check_permits_fo_query <-
  paste0(
"SELECT
distinct vessel_id, top
FROM
udp.v_sero_oth_prm_period_his@secpr_dblk
WHERE
vessel_id in ('",
vessel_ids,
"')
order by vessel_id
")

# FL9013MV_permits <-
#   dbGetQuery(con, check_permits_fo_query)
# 0

missing_vessels_permits <-
  dbGetQuery(con, check_permits_fo_query)

grep("G", missing_vessels_permits$TOP, value = T, ignore.case = T)
# [1] "CHG" "RCG"

missing_vessels_permits %>% 
  filter(grepl("G", missing_vessels_permits$TOP))
#   VESSEL_ID TOP
# 1  FL2597MN CHG
# 2  FL2597MN RCG
# MAILED

# Why not from db?
  # FL9013MV 'HEATHER LYNN' only STATE_REG_NBR, no SERO_OFFICIAL_NUMBER!
# hull XKH31047E585
# in FHIER 1st 'HEATHER LYNN' vessel_id = 694424
# 694424.................. HEATHER LYNN - DAVID ALAN LEVERE            (727) 4170877
# no G permits in 2022
# in FHIER 2nd hull XKH310290581 (638363:FL6359PX......... SUSAN T - HEATHER LYNN BROOKS            (727) 6146104)


  # FL2597MN trip_start_date 2023
# In db end_port = 110935
# vessel_id = '386533'
# trip_start 21-AUG-22
# --  srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov

### check data_from_fhier_GOM ---- 
data_from_fhier_GOM %>%
  filter(VESSEL_NAME %in% in_fhier_only_name)

data_from_fhier_GOM %>% 
  filter(VESSEL_NAME %in% in_fhier_only_name) %>% 
  dplyr::select(VESSEL_OFFICIAL_NUMBER) %>% 
  unique()

## rm AND trunc(t.trip_start_date) BETWEEN trunc(gom.effective_date) AND trunc(gom.end_date) ----
# WHERE
# t.activity_type IN (0, 80)
# AND
# t.trip_type IN ('A', 'H')
# AND trunc(t.trip_start_date) BETWEEN  TO_DATE('01-JAN-22') and
# TO_DATE('31-DEC-22')


dim(data_from_db4)
# 16342    

in_fhier_only_name <-
  setdiff(
    unique(data_from_fhier_GOM$VESSEL_NAME),
    unique(data_from_db4$VESSEL_NAME)
  )

length(in_fhier_only_name)
# 30

data_overview(data_from_fhier_GOM)
# VESSEL_NAME              732
# TRIP_TYPE                  2
# LANDING_LOCATION         432
# ARRIVAL_PORT_STATE         5

### check dates ----
data_from_fhier_GOM %>% 
    dplyr::select(TRIP_START_DATE) %>% 
    unique() %>% 
    dplyr::arrange(TRIP_START_DATE) %>% 
    tail()
# 6 12/31/2022     
# head()
# ok

head(in_fhier_only_name)

## get data from db with the same fields as in fhier ----

# print_df_names(data_from_fhier_GOM)

q_file_name <- r"(my_inputs\fishing_trips_GOM_2022\gom_landing_2022_more_fields.sql)"

gom_landing_2022_more_fields_query <- 
  readr::read_file(q_file_name)

# if a csv exists, read it, it is faster than from db
db_csv_data_gom_22_path <- 
  file.path(my_paths$inputs, 
r"(fishing_trips_GOM_2022\gom_landing_2022_more_fields.csv)")

if (file.exists(db_csv_data_gom_22_path)) {
  data_from_db_more_fields <-
    read_csv(db_csv_data_gom_22_path)
} else {
  tic("data_from_db_more_fields")
  data_from_db_more_fields <-
    dbGetQuery(con, gom_landing_2022_more_fields_query)
  toc()
  # 731.83 sec elapsed
  # 455.661 in sql dev
  # data_from_db_more_fields: 837.41 sec elapsed
}

### save data_from_db_more_fields on disc ----
# write_csv(data_from_db_more_fields,
          # "gom_landing_2022_more_fields.csv")

### compare again with FHIER ----
## in_fhier_only_names_diff5 ----
in_fhier_only_names_diff5 <-
setdiff(unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER),
        unique(data_from_db_more_fields$SERO_OFFICIAL_NUMBER))

length(in_fhier_only_names_diff5)
# 33

in_db_only_names_diff5 <-
setdiff(unique(data_from_db_more_fields$SERO_OFFICIAL_NUMBER),
        unique(data_from_fhier_GOM$VESSEL_OFFICIAL_NUMBER))

length(in_db_only_names_diff5)
# 83


# count landing locations by trip_id ----
## from DB

# str(data_from_db4)
# 'data.frame':	16342 obs. of  22 variables:

data_from_db4 %>%
  dplyr::select(SERO_OFFICIAL_NUMBER, END_PORT_STATE) %>%
  dplyr::count(END_PORT_STATE)
#    END_PORT_STATE     n
# 1              AL  2074
# 2              DE    39
# 3              FL 11449
# 4              LA   654
# 5              ME     8
# 6              MS   323
# 7              NC   149
# 8              NJ    51
# 9              NY     5
# 10             RI    44
# 11             SC    15
# 12             TX  1531

# in FHIER / GOM Trip Notifications by Arrival Port State
# Total AL 8,714
# Total FL 41,066
# Total LA 1,557
# Total MS 970
# Total TX 4,809

# View(data_from_db4)
# View(data_from_fhier_GOM)
# View(data_from_db_more_fields)

data_from_db_more_fields %>% 
  dplyr::select(TRIP_ID, END_PORT_STATE) %>%
  unique() %>% 
  dplyr::count(END_PORT_STATE)

# View(gom_trip_notifications_by_arrival_port_state_summary)

data_from_db_more_fields_end_p_s_by_trip <-
  data_from_db_more_fields %>%
  filter(END_PORT_STATE %in% gom_state_abbr) %>%
  dplyr::select(TRIP_ID, END_PORT_STATE) %>%
  unique()

dim(data_from_db_more_fields_end_p_s_by_trip)
# 53504

data_from_db_more_fields_end_p_s_by_trip %>% 
  dplyr::count(END_PORT_STATE)
#   END_PORT_STATE     n
# 1             AL  8398
# 2             FL 38180
# 3             LA  1640
# 4             MS   991
# 5             TX  4295

data_from_db_more_fields %>% 
  filter(NOTIF_LANDING_LOCATION_STATE %in% gom_state_abbr) %>%
  dplyr::select(TRIP_ID, NOTIF_LANDING_LOCATION_STATE) %>%
  unique() %>% 
  dplyr::count(NOTIF_LANDING_LOCATION_STATE)
  # NOTIF_LANDING_LOCATION_STATE     n
# 1                           AL  7322
# 2                           FL 22471
# 3                           LA  1265
# 4                           MS   893
# 5                           TX  2940
# even less (in compar. w GOM Trip Notifications by Arrival Port State)

data_from_db_more_fields_end_p_s_by_trip_p <-
  data_from_db_more_fields_end_p_s_by_trip %>%
  dplyr::add_count(END_PORT_STATE, name = "trip_by_state_num") %>%
  dplyr::select(END_PORT_STATE, trip_by_state_num) %>% 
  unique() %>% 
  dplyr::mutate(perc_st = trip_by_state_num * 100 / sum(trip_by_state_num))

# View(data_from_db_more_fields_end_p_s_by_trip_p)
# percentage is very close

# data_from_db_more_fields_end_p_s_by_trip_p

gom_fhier <-
  t(gom_trip_notifications_by_arrival_port_state_summary) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

str(gom_fhier)

gom_fhier1 <-
  gom_trip_notifications_by_arrival_port_state_summary %>%
  as.data.frame()

# %>% 

# purrr::map(rlang::set_names(c("trip_by_state_num", "perc_st")),
#            ~ dplyr::select(df, starts_with(.x)))

# gom_state_abbr %>% 
names(gom_fhier1) %>% 
  purrr::map(
    function(curr_name) {
      gom_fhier1      
    
           # ~ dplyr::select(gom_fhier1, grepl(.x)))
})

names(gom_fhier1) %>% 
  gsub(".x$|.y$", "", name)


# https://stackoverflow.com/questions/51297089/how-to-split-data-frame-by-column-names-in-r
# split by column names int percent and total
gom_fhier1_list <-
  purrr::map(rlang::set_names(c("Total", "Round")),
             ~ dplyr::select(gom_fhier1,
                             tidyselect::starts_with(.x)))
str(gom_fhier1_list)
# List of 2

gom_fhier1_list$Total %>%
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()
  

gom_fhier1_list_t <- purrr::map(gom_fhier1_list,
  # transpose
  ~ t(.x) %>% 
  as.data.frame() %>% 
  # var: Name of column to use for rownames.
  tibble::rownames_to_column(var = "header_state")
)

glimpse(gom_fhier1_list_t$Round)

gom_fhier1_list_t <-
  purrr::map(gom_fhier1_list_t,
             ~ dplyr::mutate(.x,
                      # gsub(pattern, replacement, x, ignore.case = FALSE...)
                      # remove any of:
                      state_abbr = gsub("Total |Rounded | Pct",
                                        "", 
                                        header_state, 
                                        ignore.case = T
                                        )
                      )
             )

# View(gom_fhier1_list_t)

compare_perc_db_fhier0 <-
  data_from_db_more_fields_end_p_s_by_trip_p %>%
  dplyr::mutate(rounded_percent_db = round(perc_st)) %>%
  dplyr::inner_join(gom_fhier1_list_t$Round,
             dplyr::join_by(END_PORT_STATE == state_abbr))

compare_perc_db_fhier1 <-
  dplyr::inner_join(compare_perc_db_fhier0,
                    gom_fhier1_list_t$Total,
             dplyr::join_by(END_PORT_STATE == state_abbr))

# View(compare_perc_db_fhier)
# print_df_names(compare_perc_db_fhier)
# [1] "END_PORT_STATE, trip_by_state_num, perc_st, rounded_percent_db, header_state.x, V1.x, header_state.y, V1.y"

compare_perc_db_fhier <-
  compare_perc_db_fhier1 %>%
  dplyr::select(-starts_with("header_state"))

names(compare_perc_db_fhier) <-
  c("END_PORT_STATE", "trip_by_state_num", "perc_st", "rounded_percent_db", "rounded_percent_fhier", "total_fhier")

# View(compare_perc_db_fhier)

compare_perc_db_fhier %>%
  dplyr::count(wt = trip_by_state_num)
#       n
# 1 53504
compare_perc_db_fhier %>%
  dplyr::count(wt = total_fhier)
#       n
# 1 57116

# write_csv(compare_perc_db_fhier,
          # "lending_compare_perc_db_fhier.csv")

dbDisconnect(con)

## Separate Florida counties by region ----
# And just send Marion County as GOM (keys), since it’s too hard to break up by region. Unless you have a quick solution. This is just a best guess, and doesn’t need to be perfect.

# print_df_names(data_from_db_more_fields)

data_from_db_more_fields_gom0 <-
  data_from_db_more_fields %>%
  filter(END_PORT_STATE %in% gom_state_abbr) %>%
  dplyr::mutate(fl_county_gom = dplyr::case_when(
    toupper(END_PORT_COUNTY) %in% toupper(fl_counties$GOM) ~ "fl_county_gom",
    .default = 'not_gom'
  ))
  
data_from_db_more_fields_gom0 %>% 
  dplyr::select(END_PORT_COUNTY, END_PORT_STATE, fl_county_gom) %>% 
  filter(END_PORT_STATE == "FL" & fl_county_gom == 'not_gom') %>% 
  unique() %>% 
  head()
#   END_PORT_COUNTY END_PORT_STATE fl_county_gom
# 1      PALM BEACH             FL       not_gom
# 2        ST LUCIE             FL       not_gom
# 8         BREVARD             FL       not_gom
# 9           DUVAL             FL       not_gom

filter_florida <-
  quo(END_PORT_STATE == 'FL' & fl_county_gom == 'fl_county_gom')

data_from_db_more_fields_gom1 <-
  data_from_db_more_fields_gom0 %>%
  filter(!!filter_florida | !(END_PORT_STATE == "FL"))

dim(data_from_db_more_fields_gom1)
# 564416

data_from_db_more_fields_gom1_p <-
  data_from_db_more_fields_gom1 %>%
  dplyr::select(TRIP_ID, END_PORT_STATE) %>%
  unique() %>% 
  dplyr::add_count(END_PORT_STATE, name = "trip_by_state_num") %>%
  dplyr::select(END_PORT_STATE, trip_by_state_num) %>% 
  unique() %>% 
  dplyr::mutate(perc_st = trip_by_state_num * 100 / sum(trip_by_state_num))

# write_csv(data_from_db_more_fields_gom1_p,
#          "data_from_db_more_fields_gom1_p.csv")
