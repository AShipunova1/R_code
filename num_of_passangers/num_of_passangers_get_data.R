##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false

source("~/R_code_github/compare_catch/compare_catch_data_preparation.R")

source("~/R_code_github/useful_functions_module.r")
library(ROracle)

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

# from db ----
query1 <- "SELECT DISTINCT
  trip_id,
  -- coalesce(state_reg_nbr, coast_guard_nbr)
  state_reg_nbr, coast_guard_nbr
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  USING ( vessel_id )
WHERE
--  t.sero_vessel_permit IS NOT NULL
--  AND
    t.vendor_app_name <> 'VMS'
  AND t.de <= '27-FEB-23'
  AND t.de >= '01-JAN-22'
"
# t.sero_vessel_permit IS NOT NULL
# AND t.de <= '26-FEB-23' - doesn't pick up 26-FEB-23

trip_id_vessel_from_db <- dbGetQuery(con, query1)

### rename columns to be the same ----
paste0(names(trip_id_vessel_from_db), collapse = ", ")
# "TRIP_ID, COALESCE(STATE_REG_NBR,COAST_GUARD_NBR)"
# [1] "TRIP_ID, STATE_REG_NBR, COAST_GUARD_NBR"

# names(trip_id_vessel_from_db) <- c("trip_id", "vessel_official_number")

names(trip_id_vessel_from_db) <-
  c("trip_id", "state_reg_nbr", "coast_guard_nbr")

# with state names and supplier_trip_id
query_w_state <- "SELECT
--  *
  trip_id,
  supplier_trip_id,
  state_reg_nbr,
  coast_guard_nbr,
  t.de,
  vendor_app_name,
--  ft.srfh_for_hire_name
  s.state_name
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  USING ( vessel_id )
--  JOIN srh.srfh_for_hire_type@secapxdv_dblk.sfsc.noaa.gov ft
--  ON ( ft.safis_trip_type = t.trip_type )
  JOIN safis.state@secapxdv_dblk.sfsc.noaa.gov   s
  ON ( s.state_code = t.state )
WHERE
--    coast_guard_nbr = '556499'
--    t.vendor_app_name <> 'VMS'
--  AND 
--  t.de <= '27-FEB-23'
--  AND 
  t.de >= '31-DEC-21'
  and t.trip_start_date <= '27-FEB-23'
  AND t.trip_start_date >= '31-DEC-21'
ORDER BY
  t.de DESC
"

trip_id_vessel_st_from_db <- dbGetQuery(con, query_w_state)
trip_id_vessel_st_from_db <- clean_headers(trip_id_vessel_st_from_db)

# download Reports / FHIER All Logbooks 01/01/2022-02/26/2023

logbooks_downloaded_from_fhier_22_23_feb_file_name <-
  file.path(my_paths$inputs,
            r"(logbooks_from_fhier\SRFH All Logbooks.csv)")

logbooks_downloaded_from_fhier <- read_csv(logbooks_downloaded_from_fhier_22_23_feb_file_name)

names(logbooks_downloaded_from_fhier)

logbooks_downloaded_from_fhier_trip_vsl <-
  logbooks_downloaded_from_fhier %>%
  select(View, `Vessel Official Number`)

names(logbooks_downloaded_from_fhier_trip_vsl) <-
  c("trip_id", "vessel_official_nbr")

str(logbooks_downloaded_from_fhier_trip_vsl)

logbooks_downloaded_from_fhier <- clean_headers(logbooks_downloaded_from_fhier)

# === additional downloads from fhier ----

logbooks_downloaded_from_fhier_22_23_feb_file_name <-
  file.path(my_paths$inputs,
            r"(logbooks_from_fhier\SRFH All Logbooks.csv)")

logbooks_downloaded_from_fhier <- read_csv(logbooks_downloaded_from_fhier_22_23_feb_file_name)

file_list <-
  list(
    "SRFH All Logbooks.csv",
    "SRFH All Logbooks2.csv",
    "SRFH All Logbooks3.csv",
    "SRFH All Logbooks4.csv",
    "SRFH All Logbooks5.csv",
    "SRFH All Logbooks6.csv"
  )

new_logb <-
  bind_rows(lapply(file_list, function(f) {
    f_name <- file.path(my_paths$inputs,
                        "logbooks_from_fhier", f)
    read.csv(f_name)
  }))

# dim(new_logb)
# 100824     

new_logb <- clean_headers(new_logb)
