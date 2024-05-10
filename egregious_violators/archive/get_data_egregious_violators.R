library(lubridate)
library(tictoc)
library(stringr)

source(file.path(current_project_path,
                 "db_functions.R"))

# get data for egregious violators
# Download from FHIER first
csv_names_list_22_23 = c("Correspondence__08_01_2023.csv",
                         r"(FHIER_Compliance_2022__08_01_2023.csv)",
                         r"(FHIER_Compliance_2023__08_01_2023.csv)")

data_file_date <- today()
  # lubridate::mdy("06_22_2023")

## ---- get csv data into variables ----
all_inputs <- my_paths$inputs
my_paths$inputs <- file.path(my_paths$inputs, "from_Fhier")
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_inputs"

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\from_Fhier\Correspondence\Correspondence_22_23__06_22_2023.csv"
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

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
            # current_project_name))

# vessels_permits_participants <-
#   dbGetQuery(con,
#              vessels_permits_participants_query)

# dim(vessels_permits_participants)
# [1] 63928    38

# View(vessels_permits_participants)
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
  )
# 2023-08-14 run the function: 12.84 sec elapsed

dim(vessels_permits_participants)
# [1] 63928    38
# [1] 31942    38

