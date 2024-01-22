# compare this 4 files (permits)
# 1) compliance report downloaded from FHIER (= complaince module)
# 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types)
# 3) Metrics tracking from FHIER
# 4) joined permit and vessel tables from the Oracle db
# check transformed permits

# setup ----
source("~/R_code_github/useful_functions_module.r")

library(tidyverse)

# Determine the path of the executing script
library(this.path)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

# get data ----
## 1) compliance report downloaded from FHIER (= complaince module) ----

compliance_file_name <- "FHIER Compliance.csv"

compliance_file_path <-
  file.path(curr_proj_input_path,
            compliance_file_name)

compliance_from_fhier <-
  read_csv(compliance_file_path)

dim(compliance_from_fhier)
# [1] 148375     17

## 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types) ----

db_logbooks_query <-
  "SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_start_date >= '01-JAN-2022'
  AND trip_start_date <= '31-DEC-2022'
"

db_logbooks_file_name <-
  file.path(curr_proj_input_path,
                      "logbooks22.rds")

db_logbooks_fun <-
  function(db_logbooks_query) {
    return(dbGetQuery(con,
                      db_logbooks_query))
  }

try(con <- connect_to_secpr())

get_db_logbooks <-
  function() {
    read_rds_or_run(db_logbooks_file_name,
                    db_logbooks_query,
                    db_logbooks_fun)
  }

db_logbooks <- get_db_logbooks()
# 2024-01-19 run for logbooks22.rds: 147.9 sec elapsed
# File: logbooks22.rds modified 2024-01-19 17:14:31.881392

dim(db_logbooks)
# [1] 327869    149

## 3) Metrics tracking from FHIER ----
metrics_report_file_name <-
  r"(Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source).csv)"

metrics_report_file_path <-
  file.path(curr_proj_input_path,
            metrics_report_file_name)

file.exists(metrics_report_file_path)
# T

metrics_report <- read_csv(metrics_report_file_path)

dim(metrics_report)
# [1] 3606   13

## permit table from the Oracle db ----

# ## 4) joined permit and vessel tables from the Oracle db ----

# # Need both tables to get the vessel official numbers
# # get_vessels with permits 2021+ ----
#
# dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
#     OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
#   AND effective_date <= CURRENT_DATE
# "
# # Use that "dates_filter" in all parts of the union below.
#
# # The 3 part union is needed because while the permit table has only one vessel id, the vessel table has 3 different columns for that (sero_official_number, coast_guard_nbr and state_reg_nbr) and we want to join tables by all 3 in turn.
# # stringr::str_glue is a function that allows you to create strings with placeholders for variable values. It works by using curly braces {} to enclose variable names within a string.
# vessels_permits_query <-
#   stringr::str_glue("SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = sero_official_number )
# WHERE {dates_filter}
# UNION ALL
# SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = coast_guard_nbr )
# WHERE
#   {dates_filter}
# UNION ALL
# SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = state_reg_nbr )
# WHERE
# {dates_filter}
# ")
#
# vessels_permits_file_path <-
#   file.path(my_paths$inputs, "get_db_data", "vessels_permits.rds")
#
# vessels_permits_fun <-
#   function(vessels_permits_query) {
#     return(dbGetQuery(con,
#                       vessels_permits_query))
#   }
#
# get_vessels_permits <-
#   function() {
#     read_rds_or_run(vessels_permits_file_path,
#                     vessels_permits_query,
#                     vessels_permits_fun) |>
#       vessels_permits_id_clean()
#   }
#
# vessels_permits <- get_vessels_permits()
# # File: vessels_permits.rds modified 2024-01-02 10:27:22.824263

dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
  AND effective_date <= CURRENT_DATE
"

mv_sero_fh_permits_his_query_file_path <-
  file.path(my_paths$inputs, "get_db_data", "permit_info.rds")

# file.exists(file_name_permits)
# T

mv_sero_fh_permits_his_query <-
  str_glue(
    "SELECT * FROM
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE {dates_filter}
"
  )

mv_sero_fh_permits_his_query_fun <- function(mv_sero_fh_permits_his_query) {
  result <- dbGetQuery(con, mv_sero_fh_permits_his_query)
  return(result)
}

get_permit_info <-
  function() {
    read_rds_or_run(mv_sero_fh_permits_his_query_file_path,
                    mv_sero_fh_permits_his_query,
                    mv_sero_fh_permits_his_query_fun)
  }

permit_info <- get_permit_info()
# File: permit_info.rds modified 2023-10-19 09:34:47.729631

dim(permit_info)
# [1] 183855     22

## all 4 dataframes ----
all_4_dfs <-
  Hmisc::llist(compliance_from_fhier,
    db_logbooks,
    metrics_report,
    permit_info)

# str(all_4_dfs$db_logbooks)
all_4_df_names <- names(all_4_dfs)

# prepare data for comparison ----
## clean_headers ----
all_4_dfs1 <- map(all_4_dfs, clean_headers)
# View(all_4_dfs1)

## keep only vessel_id and permit columns ----
names_to_keep <-
  function(my_df) {
    my_df_names <- names(my_df)
    grep("vessel|permit|exp|effect|end_date",
         my_df_names,
         value = TRUE,
         ignore.case = TRUE)
  }

col_names_to_keep <- map(all_4_dfs1, names_to_keep)
# View(col_names_to_keep)


all_4_dfs2[[1]]

a <-
  all_4_dfs1[[1]] |>
  select(col_names_to_keep[[1]])

View(a)

# 1)
print_df_names(all_4_dfs1[[1]])
# "vessel_official_number, name, permitgroup, permit_groupexpiration, year, week, gom_permitteddeclarations__, captainreports__, negativereports__, complianceerrors__, compliant_, set_permits_on_hold_, overridden_, override_date, override_by, contactedwithin_48_hours_, submittedpower_down_"

all_4_dfs1[[1]] <-
  all_4_dfs1[[1]] |>
  select(vessel_official_number,
         permitgroup, permit_groupexpiration)


# 2)
print_df_names(all_4_dfs1[[2]])
# [1] "trip_id, supplier_trip_id, trip_type, trip_type_name, vessel_id, vessel_official_nbr, vessel_name, capt_name_first, capt_name_last, trip_start_date, trip_start_time, trip_end_date, trip_end_time, state, state_name, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, num_anglers, nbr_of_crew, activity_type, activity_type_name, nbr_paying_passengers, trip_fee, fuel_gas_gallons, fuel_gas_gallon_price, fuel_diesel_gallons, fuel_diesel_gallon_price, fuel_gallons, fuel_gallon_price, trip_nbr, split_trip, form_version, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit, submitted_by_participant, submitted_by_corporate_name, submitted_by_first_name, submitted_by_middel_name, submitted_by_last_name, approved_by, approval_date, dea_permit_id, submit_method, ticket_type, event_id, sub_trip_type, reporting_source, transmission_date, confirmation_signature, vendor_app_name, app_version, vendor_platform, trip_de, trip_ue, trip_dc, trip_uc, effort_seq, supplier_effcat_id, area_code, sub_area_code, distance_code, distance_code_name, local_area_code, gear_code, gear_name, gear_quantity, fishing_hours, hours_days_flag, latitude, longitude, minimum_bottom_depth, maximum_bottom_depth, avg_depth_in_fathoms, fishing_gear_depth, depth, anything_caught_flag, effort_target_species_list, effort_target_common_names, gears_fishing, gear_size, rig_code, mesh_ring_length, mesh_ring_width, stretch_size, in_state, lma_code, hooks_per_line, effort_de, effort_ue, effort_dc, effort_uc, catch_seq, landing_seq"

all_4_dfs1[[2]] <-
  all_4_dfs1[[2]] |>
  select(vessel_id, vessel_official_nbr, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit)



# get pairs ----

# combn(my_col_names, 3) |>
# str(all_4_dfs)
