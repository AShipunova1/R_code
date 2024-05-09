# get data for egregious violators
# use from egregious_violators_start.R

# 1) compliance data
# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT 
# For the last 6 month
# FHIER_Compliance_...csv

# 2) correspondence data
# Download files from FHIER / Home / Correspondence
# Actions / Download 
# For the whole period, starting 01/01/2021
# "~\my_inputs\from_Fhier\Correspondence\Correspondence_2024_02_15.csv"

# 3) processed Metrics tracking
# For the last 6 month
# SEFHIER_permitted_vessels_nonSRHS_YEAR.csv

# 4) Physical Address List from FHIER
# Downloaded from REPORTS / For-hire Primary Physical Address List
# For the whole period, starting 01/01/2021
# "For-hire Primary Physical Address List.csv"

# 5) home port processed city and state from PIMS
# "~\R_files_local\my_outputs\home_ports\vessels_from_pims_ports.csv"

# 6) address information from Oracle db
# "db_participants_address.rds"

# 7) Previous results (from google drive)
# ~\R_files_local\my_inputs\egregious_violators\egregious violators for investigation_DATES...xlsx"

# FHIER ----

# Compliance
# Correspondence
# permit info from processed metrics tracking

# Download from FHIER first
all_csv_names_list = c("Correspondence_2024_04_09.csv",
                         r"(2024_04_09\FHIER_Compliance_2023__04_09_2024.csv)",
                         r"(2024_04_09\FHIER_Compliance_2024__04_09_2024.csv)")

## ---- get compliance and correspondence csv data into variables ----
from_fhier_data_path <-
  file.path(my_paths$inputs)

temp_var <-
  get_compl_and_corresp_data(from_fhier_data_path, all_csv_names_list)

compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

names(compl_clean_list) <- c(my_year1, my_year2)

# check
purrr::map(compl_clean_list, dim)

# combine years in one df
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

dim(compl_clean)

dim(corresp_contact_cnts_clean0)

## get Metric Tracking (permits from FHIER) ----
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

# exclude links
processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

# read the rest
processed_metrics_tracking_permits <-
  purrr::map_df(processed_metrics_tracking_file_names,
         readr::read_rds)

# lower names case
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

dim(processed_metrics_tracking_permits)

## get Physical Address List from FHIER ----
# REPORTS / For-hire Primary Physical Address List

fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from_Fhier\address\For-hire Primary Physical Address List_04_09_2024.csv)"
  )

file.exists(fhier_addresses_path)

fhier_addresses <-
  readr::read_csv(fhier_addresses_path,
           # read all as characters
           col_types = readr::cols(.default = 'c'),
           name_repair = fix_names)

# View(fhier_addresses)

# PIMS ----
## get home port processed city and state ----

processed_pims_home_ports_path <-
  file.path(my_paths$outputs,
              "home_ports",
              "vessels_from_pims_ports.csv")

processed_pims_home_ports <- 
  readr::read_csv(processed_pims_home_ports_path)

# View(processed_pims_home_ports)
# View(vessels_from_pims) - more fields

# Oracle db ----
## get owners addresses ----
db_participants_address_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@secapxdv_dblk
"

db_participants_address_file_path <-
  file.path(my_paths$inputs,
            current_project_name,
            "db_participants_address.rds")
 
# dir.exists(file.path(my_paths$inputs,
#             current_project_name))

# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- connect_to_secpr())
}

db_participants_address_fun <-
  function(db_participants_address) {
    # browser()
    return(dbGetQuery(con,
                      db_participants_address))
  }

db_participants_address <-
  read_rds_or_run(
    db_participants_address_file_path,
    db_participants_address_query,
    db_participants_address_fun
    # force_from_db = "yes"
  ) |>
  remove_empty_cols() |>
  clean_headers()
# 2024-04-09 run for db_participants_address.rds: 52.22 sec elapsed

dim(db_participants_address)

# Data from the previous results of "egregious violators for investigation" ----
# Download first as .xlsx

# get previous results ---
prev_result_path <- 
  file.path(my_paths$inputs,
            current_project_basename,
            "egregious violators for investigation_2023-08-15_to_2024-02-13_OLE.xlsx")

file.exists(prev_result_path)

prev_result <-
  my_read_xlsx(prev_result_path) |> 
  remove_empty_cols() |>
  clean_headers()

dim(prev_result)

# Results ----
results <-
  c(
    "compl_clean",
    "corresp_contact_cnts_clean0",
    "prev_result",
    "processed_metrics_tracking_permits",
    "fhier_addresses",
    "processed_pims_home_ports",
    "db_participants_address"
  )

cat(c("Data are in:",
      results),
    sep = "\n")

