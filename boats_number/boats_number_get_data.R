# Prepare all_logbooks_db_data_2022_short_p_region
# 1) download all db data
# 2) a) use processed logbooks
#    b) or "all logbooks = mv_safis_trip_download
# 3) Filter 2022 only
# 4) Remove unused columns
# 5) Don't do this until further notice: Mark sa_only vs. gom and dual for 2022 using vessel list from Jeanette’s comparison

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

# get logbooks 2022 only ----
## logbooks data----
# for qmd use  #' {{< include .qmd >}} instead of source()

# 2a)
# use all logbooks from https://drive.google.com/drive/folders/1HipnxawNsDjrsMc4dXgFwdRPQ3X6-x3n
processed_logb_path <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs\SEFHIER_usable_logbooks_2022.rds)")

# file.exists(processed_logb_path)

processed_logbooks <-
  read_rds(processed_logb_path)

processed_logbooks_clean_names <-
  clean_headers(processed_logbooks)

# 2b)
# dim(all_get_db_data_result_l$mv_safis_trip_download)
# [1] 735666    149

# intersect(ordered(names(processed_logbooks_clean_names)),
#           ordered(names(all_logbooks_db_data_2022_short_p_region)))
# 70

# setdiff(ordered(names(processed_logbooks_clean_names)),
#           ordered(names(all_logbooks_db_data_2022_short_p_region)))
# 85

# setdiff(ordered(names(all_logbooks_db_data_2022_short_p_region)),
#         ordered(names(processed_logbooks_clean_names)))
# [1] "vessel_official_number" "vessel_name"         "notif_cancel_flag"
# 3

## input data names ----
input_data_df_names <-
  c("all_get_db_data_result_l",
    "processed_logbooks_clean_names")

title_message_print(input_data_df_names)

# all_logbooks_db_data_2022_short_p_region

# diffdf::diffdf(all_logbooks_db_data_2022_short_p_region,
#                processed_logbooks)

# print_df_names(all_logbooks_db_data_2022_short_p_region)
# [1] "trip_id, trip_type_name, vessel_id, vessel_official_number, vessel_name, trip_start_date, trip_end_date, state, state_name, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, activity_type_name, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit, vendor_app_name, vendor_platform, trip_de, trip_ue, trip_dc, trip_uc, area_code, sub_area_code, distance_code, distance_code_name, local_area_code, latitude, longitude, effort_de, effort_ue, effort_dc, effort_uc, catch_uc, user_app, notif_seq, notif_type, notif_accsp_system_id, notif_accsp_permit_id, notif_trip_type, notif_trip_type_name, notif_trip_start_date, notif_trip_start_time, notif_trip_end_date, notif_trip_end_time, notif_start_port, notif_start_port_name, notif_start_port_county, notif_start_port_state, notif_end_port, notif_end_port_name, notif_end_port_county, notif_end_port_state, notif_cancel_flag, notif_email_sent, notif_intended_fishing_flag, notif_gear_type, notif_landing_location, notif_landing_location_name, notif_landing_location_city, notif_landing_location_county, notif_landing_location_state, notif_stat_zone, notif_ue, notif_de, notif_uc, notif_dc, permit_region"

