library(mapview)
library(sf)

# get area data ----
rm_columns <- c("ACTIVITY_TYPE",
"ANYTHING_CAUGHT_FLAG",
"APP_VERSION",
"APPROVAL_DATE",
"APPROVED_BY",
"AVG_DEPTH_IN_FATHOMS",
"CAPT_NAME_FIRST",
"CAPT_NAME_LAST",
"CATCH_DC",
"CATCH_DE",
"CATCH_SEQ",
"CATCH_SOURCE_NAME",
"CATCH_SOURCE",
"CATCH_SPECIES_ITIS",
"CATCH_UE",
"COMMON_NAME",
"CONFIRMATION_SIGNATURE",
"DC",
"DE",
"DEA_PERMIT_ID",
"DEPTH",
"DISPOSITION_CODE",
"DISPOSITION_NAME",
"EFFORT_SEQ",
"EFFORT_TARGET_COMMON_NAMES",
"EFFORT_TARGET_SPECIES_LIST",
"EVENT_ID",
"FISHING_GEAR_DEPTH",
"FISHING_HOURS",
"FORM_VERSION",
"FUEL_DIESEL_GALLON_PRICE",
"FUEL_DIESEL_GALLONS",
"FUEL_GALLON_PRICE",
"FUEL_GALLONS",
"FUEL_GAS_GALLON_PRICE",
"FUEL_GAS_GALLONS",
"GEAR_CATEGORY_CODE",
"GEAR_CATEGORY_NAME",
"GEAR_CODE",
"GEAR_DESC",
"GEAR_NAME",
"GEAR_QUANTITY",
"GEAR_SIZE",
"GEAR_TYPE_CODE",
"GEAR_TYPE_NAME",
"GEARS_FISHING",
"GRADE_CODE",
"GRADE_NAME",
"HOOKS_PER_LINE",
"HOURS_DAYS_FLAG",
"IN_STATE",
"LANDING_SEQ",
"LMA_CODE",
"MARKET_CATEGORY_CODE",
"MARKET_CATEGORY_NAME",
"MAXIMUM_BOTTOM_DEPTH",
"MESH_RING_LENGTH",
"MESH_RING_WIDTH",
"MINIMUM_BOTTOM_DEPTH",
"NBR_OF_CREW",
"NBR_PAYING_PASSENGERS",
"NUM_ANGLERS",
"REPORTED_QUANTITY",
"REPORTING_SOURCE",
"RIG_CODE",
"SPECIES_ITIS",
"SPLIT_TRIP",
"STRETCH_SIZE",
"SUB_TRIP_TYPE",
"SUBMIT_METHOD",
"SUBMITTED_BY_CORPORATE_NAME",
"SUBMITTED_BY_FIRST_NAME",
"SUBMITTED_BY_LAST_NAME",
"SUBMITTED_BY_MIDDEL_NAME",
"SUBMITTED_BY_PARTICIPANT",
"SUPPLIER_EFFCAT_ID",
"SUPPLIER_TRIP_ID",
"TICKET_TYPE",
"TRANSMISSION_DATE",
"TRIP_END_TIME",
"TRIP_FEE",
"TRIP_NBR",
"TRIP_START_TIME",
"TRIP_TYPE",
"UC",
"UE",
"UNIT_MEASURE")

# From DB ====

# file.exists(file.path(my_paths$git_r,
#                       r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

source(file.path(my_paths$git_r, r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

# Data from FHIER ----
## Reports / SAFIS Efforts Extended ----

upload_effort_files <- function(add_path) {
  full_path_to_files <-
    file.path(my_paths$inputs,
              add_path)

  csv_names_list <-
    list.files(path = full_path_to_files,
               pattern = "SAFIS EFFORTS EXTENDED *",
               full.names = T)

  efforts_extended <-
    load_csv_names_in_one_df(NULL, csv_names_list) |>
    dplyr::distinct()

  return(efforts_extended)
}

### 2022 ----
add_path <- r"(from_Fhier\SAFIS Efforts Extended\2022__09_08_2023)"
safis_efforts_extended_2022 <- upload_effort_files(add_path)

dim(safis_efforts_extended_2022)
# [1] 101038     42
# [1] 97970    42 dplyr::distinct()

# safis_efforts_extended_2022 |> dplyr::select(LOCAL_AREA_NAME) |> dplyr::distinct()

# data_overview(safis_efforts_extended_2022)
# TRIP_ID              97848
# VESSEL_OFFICIAL_NBR   1943
# LATITUDE             69913
# LONGITUDE            70682
# LOCAL_AREA_CODE         49

### 2023 ----
add_path <- r"(from_Fhier\SAFIS Efforts Extended\2023__09_13_2023)"
safis_efforts_extended_2023 <- upload_effort_files(add_path)

dim(safis_efforts_extended_2023)
# [1] 42378    42

### clean fhier effort data ----
# safis_efforts_extended_2022_short0 <-
#   safis_efforts_extended_2022 |>
#   dplyr::select(-all_of(names(empty_cols)))
# dim(safis_efforts_extended_2022_short0)

safis_efforts_extended_2022_short <-
  safis_efforts_extended_2022 |>
  dplyr::select(-any_of(rm_columns)) |>
  dplyr::distinct()
dim(safis_efforts_extended_2022_short)
# [1] 97970    17

# get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- sf::read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

sa_shp <- read_shapefile(r"(shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

# all GOM ----
gom_reef_shp <- read_shapefile(r"(gom\ReefFish_EFH_GOM\ReefFish_EFH_GOM.shp)")

gom_fed <- read_shapefile(r"(gom\GOM_FedWatersBoundary\MSA_FMC_GOM_FedWaters.shp)")

# plot(gom_fed)
# mapview(gom_fed)

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
# mapview(atmx_eez_shp, legend = F)

tic("all_atlantic_n_gom_map")
all_atlantic_n_gom_map <-
  mapview(atmx_eez_shp, legend = F) +
  mapview(sa_shp) +
  mapview(gom_reef_shp)
toc()

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

# mapview(fl_state_w_counties_shp)
