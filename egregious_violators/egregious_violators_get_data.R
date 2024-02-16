# get data for egregious violators

# 1) compliance data
# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT
# FHIER_Compliance_2022__02_05_2024.csv
# FHIER_Compliance_2023__01_24_2024.csv

# 2) correspondence data
# Download files from FHIER / Home / Correspondence
# Actions / Download
# "~\my_inputs\from_Fhier\Correspondence\Correspondence_2023_02_15.csv"

# All other files are from Processed data google folder:
# https://drive.google.com/drive/folders/1ZObq0pd7yr7caYGXjZfgRFa9BqN00OKv

# 2) Processed metrics_tracking
# SEFHIER_permitted_vessels_nonSRHS_2022.rds
# SEFHIER_permitted_vessels_nonSRHS_2023.rds

# 3) Processed Logbooks
# SEFHIER_processed_Logbooks_2022.rds
# SEFHIER_processed_Logbooks_2023.rds

# 4) Vessels with no logbooks
# vessels_with_zero_logbooks_2022.rds
# vessels_with_zero_logbooks_2023.rds

# library(lubridate)
# library(tictoc)
# library(stringr)

# source(file.path(current_project_path,
#                  "db_functions.R"))

# Compliance
# Correspondence
# permit info from processed metrics tracking

# Download from FHIER first
# "~\R_files_local\my_inputs\from_Fhier\Correspondence\Correspondence_2023_02_15.csv"
# "~\R_files_local\my_inputs\from_Fhier\FHIER Compliance\2024_02_05\FHIER_Compliance_2022__02_05_2024.csv"
# "~\R_files_local\my_inputs\from_Fhier\FHIER Compliance\2024_01_24\FHIER_Compliance_2023__01_24_2024.csv"
csv_names_list_22_23 = c("Correspondence_2023_02_15.csv",
                         r"(2024_02_05\FHIER_Compliance_2022__02_05_2024.csv)",
                         r"(2024_01_24\FHIER_Compliance_2023__01_24_2024.csv)")

data_file_date <- today()

## ---- get csv data into variables ----
from_fhier_data_path <-
  file.path(my_paths$inputs, "from_Fhier")

temp_var <- get_compl_and_corresp_data(from_fhier_data_path, csv_names_list_22_23)

compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

names(compl_clean_list) <- c(my_year1, my_year2)

# check
map(compl_clean_list, dim)
# 
# 2022
# [1] 147338     20
# 
# 2023
# [1] 148956     20

# combine years in one df <- 
compl_clean <- rbind(compl_clean_list$`2022`, compl_clean_list$`2023`)
dim(compl_clean)
# [1] 296294     20

dim(corresp_contact_cnts_clean0)
# [1] 31038    20

# get permits from FHIER Metric Tracking ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# file names for all years
processed_metrics_tracking_file_names <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

dim(processed_metrics_tracking_permits)
# [1] 6822    9

# get vessels, permits and participants info from the db ----

# get_vessels with permits and participants ----
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

con <- connect_to_secpr()
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

results <-
  c("vessels_permits_participants",
    "compl_clean",
    "corresp_contact_cnts_clean0"
)

cat(c("Data are in:",
      results),
    sep = "\n")
