# Prepare all_logbooks_db_data_2022_short_p_region
# 1) download all db data
# 2) use "all logbooks = mv_safis_trip_download
# 3) Filter 2022 only
# 4) Remove unused columns
# 5) Mark sa_only vs. gom and dual for 2022 using vessel list from Jeanette’s comparison

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

source(file.path(my_paths$git_r, r"(get_data\get_db_data\get_db_data.R)"))

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

# Remove unused columns ----

# Create a new variable 'all_logbooks_db_data_2022_short' by further processing the 'all_logbooks_db_data_2022' data.
all_logbooks_db_data_2022_short <-
  # Take the data from 'all_logbooks_db_data_2022'
  all_logbooks_db_data_2022 |>

  # Use dplyr::select to remove columns specified in 'rm_columns'
  dplyr::select(-any_of(rm_columns)) |>

  # Use dplyr::distinct to retain only distinct rows
  dplyr::distinct()

dim(all_logbooks_db_data_2022_short)
# [1] 94471    72

# Mark sa_only vs. gom and dual for 2022 ----
# Get vessel list from Jeanette’s comparison
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to keep only vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_22_sa' vector.
all_logbooks_db_data_2022_short_p_region <-
  all_logbooks_db_data_2022_short |>
  # Use the dplyr::mutate function to add a new column 'permit_region' to the dataset
  dplyr::mutate(
    permit_region =
      # Use the case_when function to conditionally assign values to 'permit_region'
      # If vessel number is in 'vessels_22_sa', set to "sa_only"
      dplyr::case_when(VESSEL_OFFICIAL_NBR %in% vessels_22_sa$vessel_official_number ~ "sa_only",
                # For all other cases, set to "gom_and_dual"
                .default = "gom_and_dual")
  )

dim(all_logbooks_db_data_2022_short_p_region)
# [1] 94471    73

names(all_logbooks_db_data_2022_short_p_region) <-
  names(all_logbooks_db_data_2022_short_p_region) |>
  my_headers_case_function()

# Output the object list. cat performs much less conversion than print.
data_list <- c("all_get_db_data_result_l",
               "all_logbooks_db_data_2022_short_p_region")

cat(data_list,
    sep = '\n')
