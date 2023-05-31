# this file is called from quantify_compliance.R

library(tictoc)

project_dir_name <- "FHIER Compliance"

filenames = c("FHIER_Compliance_2022__05_31_2023.csv",
              "FHIER_Compliance_2023__05_31_2023.csv")

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\FHIER Compliance\05_31_2023\FHIER_Compliance_2023__05_31_2023.csv"

## ---- get csv data into variables ----
csv_names_list <- prepare_csv_names(filenames)

View(csv_names_list)
# read all csv files
csv_contents <- load_csv_names(my_paths, csv_names_list)
# browser()
# unify headers, trim vesselofficialnumber, just in case
csvs_clean1 <- clean_all_csvs(csv_contents)
str(csvs_clean1)
# browser()
compl_clean <- compliance_cleaning(csvs_clean1)

View(compl_clean)
dim(compl_clean)
# 208893     21

## ---- get compliance error definitions ----

err_desc_filenames = c(file.path(project_dir_name, "Compliance_Error_Types_03_29_2023.csv"))

err_desc_csv_contents <-
  load_csv_names(my_paths, err_desc_filenames)

err_desc_clean_headers_csv_content <-
  clean_headers(err_desc_csv_contents[[1]])
err_desc <-
  change_to_dates(err_desc_clean_headers_csv_content,
                  "last_updated",
                  "%m/%d/%Y %I:%M:%S %p")

## ---- get permit data from PIMS ----
get_permit_data_from_PIMS_csv <- function() {
  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"
  
  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])
  
  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()
  
  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9]+"
      ),
      too_few = "align_start"
    )
  
  # correct dates format
  
  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)
  
  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")
  
  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  str(active_permits_from_pims)
  
  return(active_permits_from_pims)
}

# uncomment to run
active_permits_from_pims <- get_permit_data_from_PIMS_csv()

## get permit data from db ----

get_permit_data_from_db <- function() {
  # run once
  con <- connect_to_secpr()
  
  permit_query <-
    "SELECT DISTINCT
  permit,
  top,
  permit_status,
  vessel_id,
  vessel_alt_num,
  effective_date,
  expiration_date,
  end_date,
  top_name
FROM
  srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE
  effective_date > TO_DATE('01-JAN-20')
"
  
  permit_db_data = ROracle::dbGetQuery(con,
                                       permit_query)
  
  ROracle::dbDisconnect(con)
  
  return(permit_db_data)
}

# to run
permit_db_data <- get_permit_data_from_db()

str(permit_db_data)
# 37187 
# old csv 23888

# get compliance err data from db ----

get_compl_err_data_from_db <- function() {
  # run once
  con <- connect_to_secpr()

  compl_err_query <-
  "SELECT
  srh_vessel_comp_id,
  safis_vessel_id,
  supplier_vessel_id, 
  coast_guard_nbr, 
  state_reg_nbr,
  comp_year,
  comp_week,
  comp_error_type_cd,
  error_type_wo_desc,
  is_overridable,
  for_hire_trip_type,
  activity_dt,
  activity_time,
  is_past_grace_period,
  lateness
FROM
  srh.v_comp_srfh_comp_err_detail@secapxdv_dblk.sfsc.noaa.gov
  join
  SAFIS.VESSELS@secapxdv_dblk.sfsc.noaa.gov
  on(safis_vessel_id = vessel_id)
WHERE
  comp_year > '2020'
"
  compl_err_db_data = ROracle::dbGetQuery(con,
                                       compl_err_query)
  
  ROracle::dbDisconnect(con)
  
  return(compl_err_db_data)
}

tic("get_compl_err_data_from_db()")
compl_err_db_data_raw <- get_compl_err_data_from_db()
toc()
# 16.46 sec

compl_err_db_data <- clean_headers(compl_err_db_data_raw)
names(compl_err_db_data)
