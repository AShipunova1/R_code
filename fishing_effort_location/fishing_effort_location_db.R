# get data ----
get_trip_type_data_from_db <- function() {
  # browser()
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

  request_query <-
    paste0(
    "SELECT DISTINCT
    trip_id,
    vessel_official_nbr,
    trip_start_date,
    trip_end_date,
    trip_type_name,
    latitude,
    longitude
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_type in ('H', 'A')
  AND trip_de >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_END_DATE <= TO_DATE('31-DEC-22', 'dd-mon-yy')
  AND sero_vessel_permit IS NOT NULL"
)

  # nchar(request_query)
  # [1] 523009

  # cat(request_query)

  db_data = dbGetQuery(con,
                       request_query)

  dbDisconnect(con)

  return(db_data)
}

tic("get_trip_type_data_from_db")
fishing_data_from_db <- get_trip_type_data_from_db()
toc()
# get_trip_type_data_from_db: 12.8 sec elapsed

# prepare data ----
fishing_data_from_db
