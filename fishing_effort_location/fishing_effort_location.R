# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 

source("~/R_code_github/useful_functions_module.r")
library(ROracle)

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

request_query <- "SELECT
  trip_start_date,
  trip_end_date,
  start_port,
  start_port_name,
  start_port_county,
  start_port_state,
  end_port,
  vendor_app_name,
  area_code,
  sub_area_code,
  distance_code_name,
  local_area_code,
  latitude,
  longitude,
  minimum_bottom_depth,
  maximum_bottom_depth,
  avg_depth_in_fathoms,
  fishing_gear_depth,
  depth
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
    trip_de >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND trip_type_name = 'CHARTER'
  AND sero_vessel_permit IS NOT NULL"

db_data = dbGetQuery(con,
                     request_query)

# dbDisconnect(con)

# str(db_data)
# 'data.frame':	306261 obs. of  19 variables:

a <- db_data %>% 
  count(VENDOR_APP_NAME)
# 1 BLUEFIN DATA ACCSP SDK  33560
# 2          ETRIPS ONLINE   1185
# 3        ETRIPS/MOBILE 2  82207
# 4                   VESL 186854
# 5                   <NA>   2455
sum(a$n)
# 306261
  # data_overview()
