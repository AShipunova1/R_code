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
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_END_DATE <= TO_DATE('31-DEC-22', 'dd-mon-yy')
  AND trip_type_name = 'CHARTER'
  AND sero_vessel_permit IS NOT NULL"

db_data = dbGetQuery(con,
                     request_query)

dbDisconnect(con)

data_overview(db_data)
# str(db_data)
# 'data.frame':	306261 obs. of  19 variables

## ---- get geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- read_sf(shapefile_file_name)
  return(x)
}
sa_shp <- read_shapefile("osa_n_gom/SA_EEZ_off_states.shp")
gom_shp <- read_shapefile("osa_n_gom/ReefFish_EFH_GOM.shp")

gom_depth_shp <- read_shapefile("w98e78n31s18_isobath_selected_5-4000m/w98e78n31s18_isobath_selected_5-4000m.shp")
plot(gom_depth_shp)

gom_depth_shp5_100 <- read_shapefile("w98e78n31s18_isobath_5-100m/w98e78n31s18_isobath_5-100m.shp")
plot(gom_depth_shp5_100)

gom_depth_shp100_1000 <- read_shapefile("w98e78n31s18_isobath_100-1000m/w98e78n31s18_isobath_100-1000m.shp")
# plot(gom_depth_shp100_1000)

gom_depth_shp500_4000 <- read_shapefile("w98e78n31s18_isobath_500-4000m/w98e78n31s18_isobath_500-4000m.shp")

