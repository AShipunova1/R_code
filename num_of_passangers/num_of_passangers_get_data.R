source("~/R_code_github/useful_functions_module.r")
library(ROracle)

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====

num_of_pass_db = dbGetQuery(
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
