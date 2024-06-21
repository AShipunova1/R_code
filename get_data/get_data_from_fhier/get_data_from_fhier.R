# devtools::install_github("AShipunova1/R_code/auxfunctions@development")

library(auxfunctions)

#' 1) compliance data
#' 
#' Download files from FHIER / Reports / FHIER COMPLIANCE REPORT 
#' 
#' For the last 6 month
#' 
#' FHIER_Compliance_...csv
#' 

#' 2) correspondence data
#' 
#' Download files from FHIER / Home / Correspondence
#' 
#' Actions / Download 
#' 
#' For the whole period, starting 01/01/2021
#' 
#' "~\my_inputs\from_Fhier\Correspondence\Correspondence_2024_02_15.csv"
#' 

#' 3) Physical Address List from FHIER
#' 
#' Downloaded from REPORTS / For-hire Primary Physical Address List
#' 
#' For the whole period, starting 01/01/2021
#' 
#' "For-hire Primary Physical Address List.csv"
#' 

#' 4) processed Metrics tracking
#' 
#' For the last 6 month
#' 
#' SEFHIER_permitted_vessels_nonSRHS_YEAR.csv
#' 
# ===
#' Compliance & Correspondence
#' 
#' Download from FHIER first
all_csv_names_list = c("Correspondence_2024_06_17.csv",
                         r"(2024_06_17\FHIER_Compliance_2023__06_17_2024.csv)",
                         r"(2024_06_17\FHIER_Compliance_2024__06_17_2024.csv)")

## get compliance and correspondence csv data into variables ----
my_paths <- auxfunctions::set_work_dir()

from_fhier_data_path <-
  file.path(my_paths$inputs)

temp_var <-
  auxfunctions::get_compl_and_corresp_data(from_fhier_data_path, all_csv_names_list)

compl_clean_list <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

my_year1 <- "2023"
my_year2 <- "2024"

names(compl_clean_list) <- c(my_year1, my_year2)

#' check
purrr::map(compl_clean_list, dim)

#' combine years in one df
compl_clean <-
  rbind(compl_clean_list[[my_year1]], compl_clean_list[[my_year2]])

dim(compl_clean)
# [1] 221081     20

dim(corresp_contact_cnts_clean0)
# [1] 34549    22

## get Physical Address List from FHIER ----
#' REPORTS / For-hire Primary Physical Address List

fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from_Fhier\address\For-hire Primary Physical Address List_06_17_2024.csv)"
  )

file.exists(fhier_addresses_path)

fhier_addresses <-
  readr::read_csv(fhier_addresses_path,
           # read all as characters
           col_types = readr::cols(.default = 'c'),
           name_repair = auxfunctions::fix_names)

dim(fhier_addresses)
# [1] 3386   16

## get processed Metric Tracking (permits from FHIER) ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

#' file names for all years
processed_metrics_tracking_file_names_all <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

#' exclude links
processed_metrics_tracking_file_names <-
  grep(
    processed_metrics_tracking_file_names_all,
    pattern = "Shortcut.lnk",
    invert = TRUE,
    value = TRUE
  )

#' read the rest
processed_metrics_tracking_permits <-
  purrr::map_df(processed_metrics_tracking_file_names,
         readr::read_rds)

#' lower names case
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"

dim(processed_metrics_tracking_permits)
# [1] 9971    9

