# get area data ----
## From DB ====
data_from_db <- function() {
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )
  
  # fishing charter trips only
  # 2022
  # sero_vessel_permit
  
  request_query <- "SELECT distinct
    trip_start_date,
    trip_end_date,
    start_port,
    start_port_name,
    start_port_county,
    start_port_state,
    end_port,
    end_port_name,
    end_port_county,
    end_port_state,
    activity_type_name,
    trip_type_name,
    area_code,
    sub_area_code,
    distance_code_name,
    latitude,
    longitude,
    fishing_gear_depth

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
  
  # data_overview(db_data)
  
  area_data_query <-
    "select * from SAFIS.AREAS_FISHED@secapxdv_dblk.sfsc.noaa.gov
  where state in ('FL', 'US')
"
  
  db_area_data = dbGetQuery(con,
                            area_data_query)
  
  dbDisconnect(con)
  
  db_data_w_area <- full_join(db_area_data, db_data)
  # Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
  # LOCAL_AREA_CODE)`
  
  return(db_data_w_area)
}

# tic("data_from_db()")
# db_data_w_area <- data_from_db()
# toc()
# 110.58 sec

# 
# dim(db_data_w_area)
# 'data.frame':	306261 obs. of  19 variables
# [1] 254689     32  (May 25)

db_data_w_area_file_path <-
  file.path(my_paths$inputs,
            "fishing_effort_locations/db_data_w_area_more_fields.csv")

# write_csv(db_data_w_area,
          # db_data_w_area_file_path)

# or get data from the saved csv ----

db_data_w_area <- read_csv(db_data_w_area_file_path)

## ---- get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

sa_shp <- read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

gom_reef_shp <- read_shapefile(r"(gom\ReefFish_EFH_GOM\ReefFish_EFH_GOM.shp)")

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

# Sys.setenv(SHAPE_RESTORE_SHX = "YES")
# works Atlantic + GOM:
# atmx_eez_shp <- read_shapefile(r"(atmx_eez/atmx_eez.shp)")
# mapview(atmx_eez_shp, legend = F)
# mapview(atmx_eez_shp, legend = F) +
  # mapview(sa_shp) +
  # mapview(gom_reef_shp)
# gom_depth_shp <- read_shapefile("gom/w98e78n31s18_isobath_selected_5-4000m/w98e78n31s18_isobath_selected_5-4000m.shp")
# plot(gom_depth_shp)

# useez <- read_shapefile("Downloads/useez.shp")

# gom_depth_shp5_100 <- read_shapefile("gom/w98e78n31s18_isobath_5-100m/w98e78n31s18_isobath_5-100m.shp")
# plot(gom_depth_shp5_100)

# gom_depth_shp100_1000 <- read_shapefile("gom/w98e78n31s18_isobath_100-1000m/w98e78n31s18_isobath_100-1000m.shp")
# plot(gom_depth_shp100_1000)

# gom_depth_shp500_4000 <- read_shapefile("gom/w98e78n31s18_isobath_500-4000m/w98e78n31s18_isobath_500-4000m.shp")

# ===
# fl_state_land_waters <- read_shapefile("Florida_State_Waters_and_Land_Boundary/Florida_State_Waters_and_Land_Boundary.shp")

# mapview(fl_state_land_waters)

### atl_state_waters ----
# https://catalog.data.gov/dataset/outer-continental-shelf-submerged-lands-act-boundary-atlantic-region-nad83
# Outer Continental Shelf Submerged Lands Act Boundary - Atlantic Region NAD83

# atl_state_waters <- read_shapefile("ATL_SLA/ATL_SLA.shp")
# mapview(atl_state_waters)

### fl_state_w_counties ----
fl_state_w_counties_shp <- read_shapefile(r"(GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

# mapview(fl_state_w_counties)
