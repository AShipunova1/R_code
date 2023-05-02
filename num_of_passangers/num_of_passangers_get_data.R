##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)
library(ROracle)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

query_passengers <- "SELECT DISTINCT
  trip_id,
  supplier_trip_id,
  trip_start_date,
  trip_end_date,
  nbr_of_crew,
  num_anglers,
  nbr_paying_passengers,
  sero_vessel_permit,
  vendor_app_name,
  coast_guard_nbr,
  state_reg_nbr,
  vessel_name,
  passenger_capacity
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  USING ( vessel_id )
WHERE
    t.vendor_app_name <> 'VMS'
  AND t.de <= '27-FEB-23'
  AND t.de >= '01-JAN-22'
  AND passenger_capacity < nbr_paying_passengers
"
passengers_from_db <- dbGetQuery(con, query_passengers)

# trip id, vessel nbr from db ----
query1 <- "SELECT DISTINCT
  trip_id,
  state_reg_nbr, coast_guard_nbr
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  USING ( vessel_id )
WHERE
    t.vendor_app_name <> 'VMS'
  AND t.de <= '27-FEB-23'
  AND t.de >= '01-JAN-22'
"
trip_id_vessel_from_db <- dbGetQuery(con, query1)

### rename columns to be the same ----
paste0(names(trip_id_vessel_from_db), collapse = ", ")

names(trip_id_vessel_from_db) <-
  c("trip_id", "state_reg_nbr", "coast_guard_nbr")

# with state names and supplier_trip_id
query_w_state <- "SELECT
  trip_id,
  supplier_trip_id,
  state_reg_nbr,
  coast_guard_nbr,
  t.de,
  vendor_app_name,
  s.state_name
FROM
       safis.trips@secapxdv_dblk.sfsc.noaa.gov t
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  USING ( vessel_id )
  JOIN safis.state@secapxdv_dblk.sfsc.noaa.gov   s
  ON ( s.state_code = t.state )
WHERE
  t.de >= '31-DEC-21'
  AND t.trip_start_date >= '31-DEC-21'
  AND t.trip_start_date <= '27-FEB-23'
ORDER BY
  t.de DESC
"

trip_id_vessel_st_from_db <- dbGetQuery(con, query_w_state)
trip_id_vessel_st_from_db <- clean_headers(trip_id_vessel_st_from_db)

# download Reports / FHIER All Logbooks 01/01/2022-02/26/2023
# by month, otherwise it is truncated

file_list <-
  dir(file.path(my_paths$inputs, "logbooks_from_fhier"), pattern = "^SRFH All Logbooks \\d\\d \\d\\d.csv", full.names = TRUE, ignore.case = TRUE)

# read each csv and combine the result into one df
new_logb <-
  bind_rows(lapply(file_list, function(f) {
    read.csv(f)
  }))

dim(new_logb)
new_logb <- clean_headers(new_logb)

