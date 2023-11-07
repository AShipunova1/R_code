source("~/R_code_github/useful_functions_module.r")
library(ROracle)
library(zoo)

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

dat_pending = dbGetQuery(
  con,
  "SELECT
  *
FROM
       srh.v_val_srfh_pending@secapxdv_dblk.sfsc.noaa.gov
  JOIN srh.val_param@secapxdv_dblk USING (
  VAL_PARAM_ID,
  VAL_PARAM_TABLE,
  VAL_PARAM_NAME
  )
WHERE
  departure_date >= '01-JAN-2022'"
)

str(dat_pending)

### Clean up db data ====
dat_pending_date <-
  dat_pending %>%
  clean_headers() %>%
  mutate(
    # year only
    arr_year = format(arrival_date, format = "%Y"),
    # year and month
    arr_year_month = as.yearmon(arrival_date),
    # a column with "overridden" or "pending" text
    overridden = dplyr::case_when(ovr_flag == 1 ~ "overridden",
                           ovr_flag == 0 ~ "pending",
                           .default = "unknown"),
    # trim white spaces
    vessel_name = trimws(vessel_name),
    official_number = trimws(official_number)
  )

## From FHIER ====
# download separately overridden and not
f_name_y <-
  r"(~\R_files_local\my_inputs\validation_errors\Errors assigned to Others and-or Unassigned21_y.csv)"

f_name_n <-
  r"(~\R_files_local\my_inputs\validation_errors\Errors assigned to Others and-or Unassigned21_n.csv)"

# read both csvs to R
from_fhier <-
  c(f_name_y, f_name_n) %>%
  map_df(~ read_csv(.x, col_types = cols(.default = "c")))

# dim(from_fhier_y)
# [1] 4697   18
# dim(from_fhier_n)
# [1] 353  18
dim(from_fhier)
# 5050

# View(from_fhier)

from_fhier %>% clean_headers() %>% dplyr::glimpse()
# Arrival, Edit.Trip, Overridden
# arrival, edit_trip, overridden

from_fhier %>% data_overview()
# Edit Trip            4949
# VesselOfficialNumber  982

### Clean up from FHIER ====
date_format = "%m/%d/%Y"
from_fhier_data <-
  from_fhier %>%
  clean_headers() %>%
  mutate(
    # convert to a data format
    arrival = as.POSIXct(arrival,
                         format = date_format),
    departure = as.POSIXct(departure,
                           format = date_format),
    # year only column
    arr_year = format(arrival, format = "%Y"),
    # get year and month column
    arr_year_month = as.yearmon(arrival),
    # add a column with "overridden" and "pending" as text
    overridden1 = dplyr::case_when(
      tolower(overridden) == "y" ~ "overridden",
      tolower(overridden) == "n" ~ "pending",
      .default = "unknown"
    ),
    # trim white spaces
    vessel_name = trimws(vessel_name),
    vesselofficialnumber = trimws(vesselofficialnumber)
  )

# keep 2022 plus only
from_fhier_data_22 <-
  from_fhier_data %>%
  filter(arr_year_month >= "Jan 2022")

