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

## prep the compliance/override data ----

### Change column names for consistency with other datasets ----
compl_override_data__renamed <-
  compl_override_data |>
  dplyr::rename(vessel_official_number =
                  "VESSEL_OFFICIAL_NBR",
                overridden = "IS_COMP_OVERRIDE") |>
  clean_headers()

### change data type of this column if needed ----
if (!class(compl_override_data__renamed$vessel_official_number) == "character") {
  compl_override_data__renamed$vessel_official_number <-
    as.character(compl_override_data__renamed$vessel_official_number)
}

#### Add both months for straddle weeks ----

# Explanations:
# 1. 'compl_override_data__renamed_interv' is created from 'compl_override_data__renamed'.
# 2. It groups the data by the start and end dates of the compliance weeks.
# 3. It calculates the minimum month value between the 'comp_week_start_dt' and 'comp_week_end_dt' dates, storing it in a new column called 'comp_month_min'.
# 4. It calculates the maximum month value between the 'comp_week_start_dt' and 'comp_week_end_dt' dates, storing it in a new column called 'comp_month_max'.
# 5. The data is ungrouped after the calculations are done.

compl_override_data__renamed_interv <-
  compl_override_data__renamed |>
  dplyr::group_by(comp_week_start_dt, comp_week_end_dt) |>
  dplyr::mutate(comp_month_min =
           min(lubridate::month(comp_week_start_dt),
               lubridate::month(comp_week_end_dt))) |>
  dplyr::mutate(comp_month_max =
           max(lubridate::month(comp_week_start_dt),
               lubridate::month(comp_week_end_dt))) |>
  dplyr::ungroup()

# check
compl_override_data__renamed_interv |>
  dplyr::filter(!comp_month_min == comp_month_max) |>
  dplyr::select(
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    comp_month_min,
    comp_month_max
  ) |>
  dplyr::distinct() |>
  dplyr::glimpse()

## keep fewer fields from compliance info ----
# Keep only entries for "my_year" defined earlier and the previous year. Could be changed depending on the data provided by SC.

compl_override_data__renamed_m_short <-
  compl_override_data__renamed_interv |>
  select(
    vessel_official_number,
    permit_group,
    prm_grp_exp_date,
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    comp_month_min,
    comp_month_max
  ) |>
  distinct() |>
  filter(comp_year %in% c(my_year, as.integer(my_year) - 1))

dim(compl_override_data__renamed_m_short)
# [1] 279379     11

## get comp/overridden ----
tictoc::tic("get comp/overridden")
compl_override_data__renamed_m_short__compl_overr_by_week <-
  auxfunctions::add_compliant_after_override(compl_override_data__renamed_m_short)
tictoc::toc()
# get comp/overridden: 58.25 sec elapsed

# check all is_comp and overridden combinations
compl_override_data__renamed_m_short__compl_overr_by_week |>
    select(is_comp, overridden, compliant_after_override) |>
    distinct()
#   is_comp overridden compliant_after_override
# 1       1          0 yes
# 2       0          0 no
# 3       0          1 yes
# 4       1          1 yes
