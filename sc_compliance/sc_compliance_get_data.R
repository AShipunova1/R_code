# Load the 'ROracle' library, which provides an interface for working with Oracle databases in R.
library(ROracle)

# Import and prep compliance/override data ----

## Import compliance/override data ----
# Prepare variables to use as parameters for read_rds_or_run_query()
### 1) Use file.path to construct the path to a file from components. ----
# It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(processed_data_path,
            "Raw_Oracle_Downloaded_compliance_2021_plus.rds")

# Check if the file path is correct, optional
file.exists(compl_override_data_file_path)

### 2) Create a variable with a query to call data from Orace db ----
# Define year >= 2021 because of when the program started or between 2 years defined above
compl_override_data_query <-
  stringr::str_glue(
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '{db_year_1}' AND comp_year <= '{db_year_2}'")

### 3) create a function to pull data from the db ----
compl_override_data_fun <-
  function(compl_override_data_query) {
    return(ROracle::dbGetQuery(con,
                      compl_override_data_query))
  }

### 4) make and run the function ----
try(con <- connect_to_secpr())

get_compl_override_data <-
  function() {
    auxfunctions::read_rds_or_run(my_file_path = compl_override_data_file_path,
                                  compl_override_data_query,
                                  my_function = compl_override_data_fun)
  }

compl_override_data <- get_compl_override_data()
# File: Raw_Oracle_Downloaded_compliance_2021_plus.rds modified 2024-06-03 11:38:05.936556

compl_override_data$COMP_WEEK_START_DT |> min()
# [1] "2023-01-01 23:00:00 EST"
compl_override_data$COMP_WEEK_START_DT |> max()
# [1] "2024-12-22 23:00:00 EST"
