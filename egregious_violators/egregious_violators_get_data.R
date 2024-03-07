# get data for egregious violators

# 1) compliance data
# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT
# FHIER_Compliance_2022__02_05_2024.csv
# FHIER_Compliance_2023__01_24_2024.csv

# 2) correspondence data
# Download files from FHIER / Home / Correspondence
# Actions / Download
# "~\my_inputs\from_Fhier\Correspondence\Correspondence_2024_02_15.csv"

# 3) Previous results (from google drive)
# ~\R_files_local\my_inputs\egregious_violators\egregious violators for investigation_2023-01-24_to_2023-08-01_OLEAction(green).xlsx"

# 4) Physical Address List from FHIER
# Downloaded from REPORTS / For-hire Primary Physical Address List
# "For-hire Primary Physical Address List.csv"

# 5) vessel and permit information from Oracle db
# "vessels_permits_participants.rds"

# 6) home port processed city and state from PIMS
# "~\R_files_local\my_outputs\home_ports\vessels_from_pims_ports.csv"

# FHIER ----

# Compliance
# Correspondence
# permit info from processed metrics tracking

# Download from FHIER first
all_csv_names_list = c("Correspondence_2024_02_15.csv",
                         r"(2024_02_15\FHIER_Compliance_2023__02_15_2024.csv)",
                         r"(2024_02_15\FHIER_Compliance_2024__02_15_2024.csv)")

## ---- get csv data into variables ----
from_fhier_data_path <-
  file.path(my_paths$inputs)

temp_var <-
  get_compl_and_corresp_data(from_fhier_data_path, all_csv_names_list)

compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

names(compl_clean_list) <- c(my_year1, my_year2)

# check
map(compl_clean_list, dim)
# $`2023`
# [1] 149025     20
# 
# $`2024`
# [1] 19715    20

# combine years in one df
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

dim(compl_clean)
# [1] 168740     20

dim(corresp_contact_cnts_clean0)
# [1] 31038    20


# get previous results ---
prev_result_path <- 
  file.path(my_paths$inputs,
            current_project_basename,
            "egregious violators for investigation_2023-01-24_to_2023-08-01_OLEAction(green).xlsx")

# file.exists(prev_result_path)
prev_result <-
  read_xlsx(prev_result_path) |> 
  remove_empty_cols() |> 
  clean_headers()

glimpse(prev_result)
# [1] 96 18

## get permits from FHIER Metric Tracking ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# file names for all years
processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

dim(processed_metrics_tracking_permits)
# [1] 6822    9

## Physical Address List from FHIER ----
# REPORTS / For-hire Primary Physical Address List

fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from PIMS\address\For-hire Primary Physical Address List_02_21_2024.csv)"
  )

# file.exists(fhier_addresses_path)

fhier_addresses <-
  read_csv(fhier_addresses_path,
           # read all as characters
           col_types = cols(.default = 'c'),
           name_repair = fix_names)

# View(fhier_addresses)

# PIMS ----
# ## Get vessels from PIMS ----
# # hailing port
# get_vessel_data_pims <-
#   function(vessel_names_file_path,
#            to_skip = 0,
#            my_sheet = "Sheet 1") {
#     # file.exists(vessel_names_file_path)
#     
#     vessels_from_pims_raw <-
#       read_xlsx(vessel_names_file_path,
#                 sheet = my_sheet,
#                 skip = to_skip)
#     
#     # clean_headers
#     vessels_from_pims <-
#       vessels_from_pims_raw %>%
#       clean_headers()
#     
#     return(vessels_from_pims)
#   }
# 
# vessel_data_pims_double_address <-
#     file.path(my_paths$inputs,
#               r"(non_compliant_areas\vessels_permit_hailng_port_double_name.xlsx)")
# 
# vessel_names_file_path <- 
#     file.path(my_paths$inputs,
#               r"(non_compliant_areas\Vessels - 2024-02-12_1633.xlsx)")
# 
# vessels_from_pims <- get_vessel_data_pims(vessel_names_file_path)
# 
# dim(vessels_from_pims)
# # [1] 23059     8
# 
# vessels_from_pims_double <- 
#   get_vessel_data_pims(vessel_data_pims_double_address,
#                        to_skip = 0)
# 
# dim(vessels_from_pims_double)
# # [1] 652   3
# 
## get home port processed city and state ----

processed_pims_home_ports_path <-
  file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports.csv")

processed_pims_home_ports <- 
  read_csv(processed_pims_home_ports_path)

# View(processed_pims_home_ports)
# View(vessels_from_pims) - more fields

# Oracle db ----
## get owners addresses ----
db_participants_asddress_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@Secapxdv_Dblk.sfsc.noaa.gov
"

db_participants_asddress_file_path <-
  file.path(all_inputs,
            current_project_name,
            "db_participants_asddress.rds")
 
# dir.exists(file.path(all_inputs,
#             current_project_name))

# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- connect_to_secpr())
}

db_participants_asddress_fun <-
  function(db_participants_asddress) {
    # browser()
    return(dbGetQuery(con,
                      db_participants_asddress))
  }

db_participants_asddress <-
  read_rds_or_run(
    db_participants_asddress_file_path,
    db_participants_asddress_query,
    db_participants_asddress_fun
    # force_from_db = "yes"
  ) |>
  remove_empty_cols() |>
  clean_headers()

dim(db_participants_asddress)
# [1] 55113    41
# [1] 55113    37 remove_empty_cols

# 2024-03-01 run for db_participants_asddress.rds: 35.25 sec elapsed

## get_vessels with permits and sero_home_port ----
vessel_permit_where_part <-
  "
    p.permit_status <> 'REVOKED'
      AND p.top IN ( 'CHG', 'HCHG', 'HRCG', 'RCG', 'CHS',
                     'SC', 'CDW' )
      AND ( p.expiration_date >= ( sysdate - ( 365 / 2 ) )
            OR p.end_date >= ( sysdate - ( 365 / 2 ) ) )
      AND nvl(p.end_date, p.expiration_date) IS NOT NULL
"

vessel_permit_fields_part <-
  "   v.sero_home_port_city,
      v.sero_home_port_county,
      v.sero_home_port_state,
      v.sero_official_number,
      v.coast_guard_nbr,
      v.event_id,
      v.hull_id_nbr,
      v.owner_id,
      v.state_reg_nbr,
      v.status v_status,
      v.supplier_vessel_id,
      v.ue,
      v.vessel_id v_vessel_id,
      v.vessel_name,
      p.effective_date,
      p.end_date,
      p.entity_id,
      p.expiration_date,
      p.new_owner,
      p.permit,
      p.permit_status,
      p.prior_owner,
      p.vessel_alt_num,
      p.vessel_id p_vessel_id
"

vessels_permits_from_part <-
"FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p,
  safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
"

vessels_permits_query <-
  str_glue(
  "SELECT
  {vessel_permit_fields_part}
  {vessels_permits_from_part}
  WHERE
    ( p.vessel_id = sero_official_number
  OR
    p.vessel_id = state_reg_nbr
  OR 
    p.vessel_id = coast_guard_nbr )
  AND
  {vessel_permit_where_part}
  ")

vessels_permits_participants_query <-
  paste0(
  "SELECT
  v_p.*,

  f_p.first_name,
  f_p.middle_name,
  f_p.last_name,
  f_p.name_suffix,
  f_p.address_1,
  f_p.address_2,
  f_p.state,
  f_p.postal_code,
  f_p.phone_nbr,
  f_p.email,
  f_p.license_nbr,
  f_p.participant_id,
  f_p.permit_id,
  f_p.status f_p_status
FROM
       safis.full_participant@secapxdv_dblk.sfsc.noaa.gov f_p
  JOIN (",
  vessels_permits_query,
  ") v_p
  ON ( to_char(license_nbr) = to_char(entity_id) )"
  )

cat(
  vessels_permits_participants_query,
  file =
    file.path(
      all_inputs,
      current_project_name,
      "vessels_permits_participants_query.sql"
    )
)

vessels_permits_participants_file_path <-
  file.path(all_inputs,
            current_project_name,
            "vessels_permits_participants.rds")
 
# dir.exists(file.path(all_inputs,
#             current_project_name))

vessels_permits_participants_fun <-
  function(vessels_permits_participants) {
    # browser()
    return(dbGetQuery(con,
                      vessels_permits_participants))
  }

vessels_permits_participants <-
  read_rds_or_run(
    vessels_permits_participants_file_path,
    vessels_permits_participants_query,
    vessels_permits_participants_fun
    # force_from_db = "yes"
  )
# 2023-08-14 run the function: 12.84 sec elapsed
# 2024-02-16 run for vessels_permits_participants.rds: 5.81 sec elapsed

dim(vessels_permits_participants)
# [1] 63928    38
# [1] 31942    38
# [1] "2024-02-16"
# [1] 30511    38

# Results ----
results <-
  c("db_participants_asddress",
    "vessels_permits_participants",
    "compl_clean",
    "corresp_contact_cnts_clean0",
    "processed_pims_home_ports"
)

cat(c("Data are in:",
      results),
    sep = "\n")
