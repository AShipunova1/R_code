# this file is called from quantify_compliance.R

library(tictoc)

project_dir_name <- "FHIER Compliance"

# get data from csvs ----
get_data_from_FHIER_csvs <- function() {
  filenames = c(
    "FHIER_Compliance_2022__05_31_2023.csv",
    "FHIER_Compliance_2023__05_31_2023.csv"
  )

  # "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\FHIER Compliance\05_31_2023\FHIER_Compliance_2023__05_31_2023.csv"

  ## ---- get csv data into variables ----
  csv_names_list <- prepare_csv_names(filenames)

  # View(csv_names_list)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
  # browser()
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
#  str(csvs_clean1)
  # browser()
  compl_clean <- compliance_cleaning(csvs_clean1)

  return(compl_clean)
}

get_compliance_error_definitions <- function() {
  err_desc_filenames = c(file.path(project_dir_name, "Compliance_Error_Types_03_29_2023.csv"))

  err_desc_csv_contents <-
    load_csv_names(my_paths, err_desc_filenames)

  err_desc_clean_headers_csv_content <-
    clean_headers(err_desc_csv_contents[[1]])
  err_desc <-
    change_to_dates(err_desc_clean_headers_csv_content,
                    "last_updated",
                    "%m/%d/%Y %I:%M:%S %p")

  return(err_desc)
}

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
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

get_data_from_csv <- function() {

# uncomment to run
compl_clean <- get_data_from_FHIER_csvs()
# View(compl_clean)
dim(compl_clean)
# 208893     21

## get compliance error definitions from csvs ----
err_desc <- get_compliance_error_definitions()

## get permit data from PIMS csv ----

active_permits_from_pims <- get_permit_data_from_PIMS_csv()

compl_clean1 <- additional_clean_up(compl_clean)

return(compl_clean1)
}

additional_clean_up <- function(compl_clean) {

  # ---- separate SA and GOM permits ----
  compl_clean_sa_vs_gom <-
    separate_permits_into_3_groups(compl_clean)

  # View(compl_clean_sa_vs_gom)

  # ---- add columns for month and quarter ----
  compl_clean_sa_vs_gom_m <-
    compl_clean_sa_vs_gom %>%
    # add month
    mutate(year_month = as.yearmon(week_start)) %>%
    # add quarter
    mutate(year_quarter = as.yearqtr(week_start))

  # ---- convert report numbers to numeric ----
  compl_clean_sa_vs_gom_m_int <-
    compl_clean_sa_vs_gom_m %>%
    mutate(
      captainreports__ = as.integer(captainreports__),
      negativereports__ = as.integer(negativereports__),
      gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
    )

  # add year_permit column ----
  compl_clean_sa_vs_gom_m_int_c <-
    compl_clean_sa_vs_gom_m_int %>%
    mutate(
      year_permit =
        case_when(
          year == "2022" & (permit_sa_gom == "gom_only"
                            | permit_sa_gom =="dual") ~
            paste(year, "gom_dual"),
          year == "2022" & permit_sa_gom == "sa_only" ~
            paste(year, "sa_only"),
          year == "2023" & (permit_sa_gom %in% c("sa_only", "dual")) ~
            paste(year, "sa_dual")
        )
    )


  return(compl_clean_sa_vs_gom_m_int_c)
}

# get data from db ----
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

get_compl_err_data_from_db <- function() {
  # run once
  con <- connect_to_secpr()

  compl_err_query <-
    "SELECT
  *
FROM
       srh.srfh_vessel_comp_err@secapxdv_dblk.sfsc.noaa.gov
  INNER JOIN srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
  USING ( srh_vessel_comp_id )
WHERE
  comp_year > '2021'
"
# common fields
#   SRH_VESSEL_COMP_ID
# CREATED_DT
# CREATED_USER_ID
# LU_DT
# LU_USER_ID

    compl_err_db_data_0 = ROracle::dbGetQuery(con,
                                       compl_err_query)

    compl_err_db_data_1 <-
      compl_err_db_data_0 %>%
      # remove duplicated columns
      select(-c(CREATED_DT,
                CREATED_USER_ID,
                LU_DT,
                LU_USER_ID))

  ROracle::dbDisconnect(con)

  return(compl_err_db_data_1)
}

get_data_from_db <- function() {

## get permit data from db ----
# to run
permit_db_data <- get_permit_data_from_db()

# str(permit_db_data)
# 37187
# old csv 23888

# get compliance err data from db ----

# uses an inner_join, keeps only entries with compl errors.
# To get all use FULL OUTER JOIN

tic("get_compl_err_data_from_db()")
compl_err_db_data_raw <- get_compl_err_data_from_db()
toc()

# get_compl_err_data_from_db(): 47.5 sec elapsed
# get_compl_err_data_from_db(): 22.23 sec elapsed

# test for unique() fields
all_names_len <- names(compl_err_db_data_raw) %>% length()
uniq_names_len <-
  names(compl_err_db_data_raw) %>% unique() %>% length()
identical(all_names_len, uniq_names_len)

# names(compl_err_db_data_raw) %>%
  # unique() %>%
#   # 42
  # 38
  # length()
# 46
# 38

compl_err_db_data <- clean_headers(compl_err_db_data_raw)
names(compl_err_db_data)

# dim(compl_err_db_data)
# [1] 87925    15
# [1] 44662    38 2021+

# override comments ----
compl_err_db_data_raw %>% select(OVERRIDE_CMT, COMP_OVERRIDE_CMT) %>% unique()
}

if (exists("get_data_from_param")) {
  if (get_data_from_param == "db") {
    get_data_from_db()
  }
} else {
  compl_clean_sa_vs_gom_m_int <- get_data_from_csv()
}
