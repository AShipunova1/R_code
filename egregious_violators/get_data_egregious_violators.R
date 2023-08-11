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
my_paths$inputs <- file.path(my_paths$inputs, "from_Fhier")
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_inputs"

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\from_Fhier\Correspondence\Correspondence_22_23__06_22_2023.csv"
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

# get vessels and permit info from the db ----

number_of_weeks_for_non_compliancy = 27
days_in_27_weeks <- number_of_weeks_for_non_compliancy*7

half_year_ago <-
  data_file_date - days_in_27_weeks

# half_year_ago
# [1] "2023-01-24"
stringr::str_glue("TO_DATE({half_year_ago}, 'yyyy-mm-dd')")
# TO_DATE(2023-01-24, 'yyyy-mm-dd')

# get_vessels wtih permits 2022 ----
vessels_permits_2022_query <-
  str_glue(
  "SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = sero_official_number )
WHERE
  end_date >= to_date({half_year_ago}, 'yyyy-mm-dd')
    OR expiration_date >= TO_DATE({half_year_ago}, 'yyyy-mm-dd'
)
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = coast_guard_nbr )
WHERE
   end_date >= TO_DATE({half_year_ago}, 'yyyy-mm-dd')
    OR expiration_date >= TO_DATE({half_year_ago}, 'yyyy-mm-dd') UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = state_reg_nbr )
WHERE
  end_date >= TO_DATE({half_year_ago}, 'yyyy-mm-dd')
  OR expiration_date >= TO_DATE({half_year_ago}, 'yyyy-mm-dd')
  "
)

vessels_permits_2022_file_path <- file.path(input_path, "vessels_permits_2022.rds")

vessels_permits_2022_fun <-
  function(vessels_permits_2022_query) {
    return(dbGetQuery(con,
                      vessels_permits_2022_query))
  }

vessels_permits_2022 <-
  read_rds_or_run(
    vessels_permits_2022_file_path,
    vessels_permits_2022_query,
    vessels_permits_2022_fun
  )
# 2023-07-15 run the function: 13.33 sec elapsed

#
# vessels ----
vessels_query <-
  "SELECT
 *
  FROM
    safis.vessels@secapxdv_dblk.sfsc.noaa.gov
"

vessels_file_path <- 
  file.path(my_paths$inputs, 
            "../egregious_violators",
            "vessels.rds")

con <- connect_to_secpr()

vessels_fun <- function(vessels_query) {
  return(dbGetQuery(con, vessels_query))
}

vessels <-
  read_rds_or_run(
    vessels_file_path,
    vessels_query,
    vessels_fun
  )

dim(vessels)
