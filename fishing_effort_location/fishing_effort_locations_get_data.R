con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====
# fishing charter trips only
# 2022
# sero_vessel_permit

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

## ---- get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

sa_shp <- read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

mapview(atmx_eez_shp, legend = F) + mapview(sa_shp)
# doesn't work:
# gom_only <- st_difference(atmx_eez_shp, sa_shp)
# gom_only <- st_difference(atmx_eez_shp, sa_shp$geometry)
# Warning message:
# attribute variables are assumed to be spatially constant throughout all geometries 
# mapview(gom_only,
        # legend = F)

# gom_bath_shp <- read_shapefile(r"(gom\bathyc\bathyc.shp)")
# plot(gom_bath_shp)
# mapview(gom_bath_shp)

# Protraction Diagram Polygons for the Gulf of Mexico OCS
# gom_protrac_shp <- read_shapefile(r"(gom\protrac_nad83\protrac_nad83.shp)")
# mapview(gom_protrac_shp)

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
# works Atlantic + GOM:
atmx_eez_shp <- read_shapefile(r"(atmx_eez/atmx_eez.shp)")
mapview(atmx_eez_shp, legend = F)

# gom_depth_shp <- read_shapefile("gom/w98e78n31s18_isobath_selected_5-4000m/w98e78n31s18_isobath_selected_5-4000m.shp")
# plot(gom_depth_shp)

# useez <- read_shapefile("Downloads/useez.shp")

# gom_depth_shp5_100 <- read_shapefile("gom/w98e78n31s18_isobath_5-100m/w98e78n31s18_isobath_5-100m.shp")
# plot(gom_depth_shp5_100)

# gom_depth_shp100_1000 <- read_shapefile("gom/w98e78n31s18_isobath_100-1000m/w98e78n31s18_isobath_100-1000m.shp")
# plot(gom_depth_shp100_1000)

# gom_depth_shp500_4000 <- read_shapefile("gom/w98e78n31s18_isobath_500-4000m/w98e78n31s18_isobath_500-4000m.shp")

# ===
fl_state_land_waters <- read_shapefile("Florida_State_Waters_and_Land_Boundary/Florida_State_Waters_and_Land_Boundary.shp")

mapview(fl_state_land_waters)

# ====
# https://catalog.data.gov/dataset/outer-continental-shelf-submerged-lands-act-boundary-atlantic-region-nad83
# Outer Continental Shelf Submerged Lands Act Boundary - Atlantic Region NAD83

atl_state_waters <- read_shapefile("ATL_SLA/ATL_SLA.shp")
mapview(atl_state_waters)

# ====

fl_state_w_counties <- read_shapefile(r"(GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

mapview(fl_state_w_counties)
