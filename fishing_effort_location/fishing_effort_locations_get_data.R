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

source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()

# dim(all_get_db_data_result_l$mv_safis_trip_download)
# [1] 735666    149

# get 2022 only ----
# Create a new variable 'all_logbooks_db_data_2022' using the pipe operator.
# This variable will store the filtered data for the year 2022.
all_logbooks_db_data_2022 <-
  # Take the data from 'all_get_db_data_result_l$mv_safis_trip_download'
  all_get_db_data_result_l$mv_safis_trip_download |>

  # Use the dplyr::filter function to filter rows based on a condition
  dplyr::filter(
    # Check if the 'TRIP_START_DATE' is between "2022-01-01" and "2022-12-31"
    dplyr::between(
      TRIP_START_DATE,                 # Column to check
      as.Date("2022-01-01"),          # Start date for the range
      as.Date("2022-12-31")           # End date for the range
    )
  )

dim(all_logbooks_db_data_2022)
# [1] 326670    149


# ### Remove Not in Jeannette's list SA 2022 vessels ----
# # Build the path to the R script 'vessel_permit_corrected_list.R' by
# # combining the base path 'my_paths$git_r' and the script name.
# script_path <-
#   file.path(my_paths$git_r,
#             "vessel_permit_list/vessel_permit_corrected_list.R")
#
# # Source (run) the R script using the constructed script path.
# source(script_path)
#
# # Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# # 'vessels_to_remove_from_ours' vector.
# all_logbooks_db_data_rm <-
#   all_get_db_data_result_l$mv_safis_trip_download |>
#   filter(!VESSEL_OFFICIAL_NBR %in% vessels_to_remove_from_ours)
#
# dim(all_logbooks_db_data_rm)
# # [1] 733585    149

## Remove unused columns ----

# Create a new variable 'all_logbooks_db_data_2022_short' by further processing the 'all_logbooks_db_data_2022' data.
all_logbooks_db_data_2022_short <-
  # Take the data from 'all_logbooks_db_data_2022'
  all_logbooks_db_data_2022 |>

  # Use dplyr::select to remove columns specified in 'rm_columns'
  dplyr::select(-any_of(rm_columns)) |>

  # Use dplyr::distinct to retain only distinct rows
  dplyr::distinct()

dim(all_logbooks_db_data_2022_short)
# [1] 326670  134

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
    distinct()

  return(efforts_extended)
}

### 2022 ----
add_path <- r"(from_Fhier\SAFIS Efforts Extended\2022__09_08_2023)"
safis_efforts_extended_2022 <- upload_effort_files(add_path)

dim(safis_efforts_extended_2022)
# [1] 101038     42
# [1] 97970    42 distinct()

# safis_efforts_extended_2022 |> select(LOCAL_AREA_NAME) |> distinct()

data_overview(safis_efforts_extended_2022)
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
#   select(-all_of(names(empty_cols)))
# dim(safis_efforts_extended_2022_short0)

safis_efforts_extended_2022_short <-
  safis_efforts_extended_2022 |>
  select(-any_of(rm_columns)) |>
  distinct()
dim(safis_efforts_extended_2022_short)
# [1] 97970    17

# get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

sa_shp <- read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
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
