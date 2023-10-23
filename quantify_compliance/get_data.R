# this file is called from quantify_compliance.R

library(tictoc)

project_dir_name <- "FHIER Compliance"

# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT

# get data from csvs ----
get_data_from_FHIER_csvs <- function() {
  filenames = c(
    "FHIER_Compliance_2022__05_31_2023.csv",
    "FHIER_Compliance_2023__05_31_2023.csv"
  )

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
source(file.path(my_paths$git_r, r"(get_db_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

