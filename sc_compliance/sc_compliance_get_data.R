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

## Combine weekly compliance to create monthly compliance ----

# Explanations:
# - This code snippet combines weekly compliance data to create monthly compliance data for each vessel.
# - The process is timed using `tic()` and `toc()` to measure its execution time.
# - The `compl_override_data__renamed_m_short__compl_overr_by_week` dataframe is used as input.
# - The data is first grouped by `vessel_official_number`, `comp_year`, and `comp_month_min` (the month of the compliance week start).
# - For each group, the unique compliant statuses after override are calculated and concatenated into a string (`all_m_comp_min`).
# - Monthly compliance (`month_comp_min`) is determined based on the concatenated compliant statuses.
# - The process is repeated for `comp_month_max` to get the monthly compliance for the month of the end of the compliance week.
# - Finally, the `compl_override_data__renamed_m_short__m_compl__both_months` dataframe is created with both `comp_month_min` and `comp_month_max` data.
tictoc::tic("get month_comp")
compl_override_data__renamed_m_short__m_compl__both_months <-
  compl_override_data__renamed_m_short__compl_overr_by_week |>
  dplyr::group_by(vessel_official_number, comp_year, comp_month_min) |>
  dplyr::mutate(all_m_comp_min =
                  toString(unique(sort(
                    compliant_after_override
                  )))) |>
  dplyr::mutate(month_comp_min =
                  dplyr::case_when(all_m_comp_min %in% c(c("no, yes"), "no") ~ "non_compl",
                                   .default = "compl")) |>
  dplyr::ungroup() |>
  dplyr::group_by(vessel_official_number, comp_year, comp_month_max) |>
  dplyr::mutate(all_m_comp_max =
                  toString(unique(sort(
                    compliant_after_override
                  )))) |>
  dplyr::mutate(month_comp_max =
                  dplyr::case_when(all_m_comp_max %in% c(c("no, yes"), "no") ~ "non_compl",
                                   .default = "compl")) |>
  dplyr::ungroup()
tictoc::toc()
# get month_comp: 31.7 sec elapsed

### combine month compliance for each week ----

# Explanations:
# 1. 'compl_override_data__renamed_m_short__m_compl' is created from 'compl_override_data__renamed_m_short__m_compl__both_months'.
# 2. It groups the data by the vessel official number, compliance year, compliance week, start date of the compliance week, and end date of the compliance week.
# 3. It calculates a new column 'common_month_compliance' based on conditions:
#    a. If the minimum and maximum month values of compliance are both 'compl', then the 'common_month_compliance' is set to 'compl'.
#    b. For all other cases, it is set to 'non_compl'. Meaning if a week that overlaps these 2 months is non_compl, both months are non-compliant.
# 4. The data is ungrouped after the calculations are done.

tictoc::tic("min_max_compl")
compl_override_data__renamed_m_short__m_compl <-
  compl_override_data__renamed_m_short__m_compl__both_months |>
  dplyr::group_by(vessel_official_number,
           comp_year,
           comp_week,
           comp_week_start_dt,
           comp_week_end_dt) |>
  dplyr::mutate(
    common_month_compliance =
      dplyr::case_when(
        month_comp_min == month_comp_max &
          month_comp_min == "compl" ~
          month_comp_min,
        .default = "non_compl"
      )
  ) |>
  dplyr::ungroup()
tictoc::toc()
# min_max_compl: 35.58 sec elapsed

# check
compl_override_data__renamed_m_short__m_compl |>
  select(contains("month")) |>
  # filter(!month_comp_min == month_comp_max) |>
  filter(comp_month_min == 4) |>
  distinct() |>
  glimpse()

# get processed logbooks ----

# set the path to processed logbook data
logbook_file_path <-
  file.path(processed_data_path,
            stringr::str_glue("SEFHIER_processed_Logbooks_{my_year}.rds"))

file.exists(logbook_file_path)

# read in logbook data, clean up headers
logbooks <-
  readr::read_rds(logbook_file_path) |>
  auxfunctions::clean_headers()

# checks dimensions of the dataframe
dim(logbooks)
# [1] 42605   179

logbooks |>
  dplyr::filter(lubridate::month(comp_week_end_dt) == 4) |>
  glimpse()

# get dnfs ----

# set the path to processed DNF data
dnf_file_path <-
  file.path(processed_data_path,
            stringr::str_glue("SEFHIER_processed_dnfs_{my_year}.rds"))

# read in DNF data, clean up headers
dnfs <-
  readr::read_rds(dnf_file_path) |>
  auxfunctions::clean_headers()

# checks dimensions of the dataframe
dim(dnfs)
# [1] 168354     37

# get srhs vessels ----

# read csv and clean headers
srhs_2024 <-
  readr::read_csv(srhs_2024_file_path,
                  col_types = readr::cols(.default = 'c')) |>
  auxfunctions::clean_headers()

# glimpse(srhs_2024)

# read sc permitted data ----

# read xlsx and clean headers
SC_permittedVessels <-
  auxfunctions::my_read_xlsx(sc_file_path) |>
  auxfunctions::clean_headers()

# glimpse(SC_permittedVessels)

## fix dates in headers ----
# TODO: grab just the month of interest (depends on the SC file)

# grep for numbers only in the names and convert them

# save the not digit headers
not_date_names <-
  grep("^\\D+$", names(SC_permittedVessels), value = T)

# find all digit headers
date_names_raw <-
  grep("^\\d+$", names(SC_permittedVessels), value = T)

# Explanations:
# 1. Take the data frame 'date_names_raw'.
# 2. Use the function 'convertToDate()' to convert the Excel integer values to Date objects.
# 3. Use the 'format()' function to format the dates as "%m-%y", which represents month and year.
# 4. Assign the result to 'date_names_ok'.
date_names_ok <-
  date_names_raw |>
  openxlsx::convertToDate() |>
  format("%m-%y")

# combine the saved non-digit headers and the newly converted ones
all_names <- c(not_date_names, date_names_ok)

# check
all_names

# apply the names to the DF
names(SC_permittedVessels) <-
  all_names

# check
names(SC_permittedVessels)

dim(SC_permittedVessels)
# [1] 228  18

## change sc data format ----

# glimpse(SC_permittedVessels)

# Explanations:
# 1. 'SC_permittedVessels_longer' is created by reshaping the data frame 'SC_permittedVessels'.
# 2. The 'pivot_longer()' function from the 'tidyr' package is used to reshape the data from wide to long format.
# 3. All columns except those specified in "!c()" are pivoted.
# 4. The 'names_to' parameter specifies the name of the new column that will store the names of the original columns. Here, it's set to "month_year".
# 5. The 'values_to' parameter specifies the name of the new column that will store the values from the pivoted columns. Here, it's set to "delinquent_month".

SC_permittedVessels_longer <-
  SC_permittedVessels |>
  tidyr::pivot_longer(
    !c(
      "vessel_reg_uscg_",
      "vessel_name",
      "reports_to_srhs",
      "federal_for_hire_permit_expiration",
      "marked_as_federally_permitted_in_vesl",
      "delinquent"
    ),
    names_to = "month_year",
    values_to = "delinquent_month"
  )

dplyr::glimpse(SC_permittedVessels_longer)

# Explanations:
# 1. 'SC_permittedVessels_longer_m_y' is created by processing the 'SC_permittedVessels_longer' data frame.
# 2. The 'separate_wider_delim()' function from the 'tidyr' package is used to separate the 'month_year' column into two separate columns ('month_sc' and 'year_sc') based on the specified delimiter '-'.
# 3. The 'names' parameter specifies the names for the new columns created by separation.
# 4. The 'delim' parameter specifies the delimiter used to separate the 'month_year' column.
# 5. The 'mutate()' function is used to modify the 'year_sc' column by pasting "20" in front of each entry to ensure it's in a four-digit format. This is done, because we have the four-digit ("2024") format for a year in the compliance report.
# 6. The 'mutate()' function is applied across all columns containing month and year information ('month_sc' and 'year_sc') to convert them to numeric format. Again, for compatibility with the other data sets.
# 7. The 'distinct()' function is used to remove duplicate rows from the data frame.

SC_permittedVessels_longer_m_y <-
  SC_permittedVessels_longer |>
  tidyr::separate_wider_delim(cols = month_year,
                       delim = "-",
                       names = c("month_sc", "year_sc")) |>
  dplyr::mutate(year_sc = paste0("20", year_sc)) |>
  dplyr::mutate(dplyr::across(tidyselect::all_of(c("month_sc", "year_sc")), as.numeric)) |>
  dplyr::distinct()

dplyr::glimpse(SC_permittedVessels_longer_m_y)

# SRHS: check and remove reports_to_srhs ----

# Join the SC data with the SRHS list by vessel
sc__srhs_join <-
  dplyr::full_join(SC_permittedVessels_longer_m_y,
            srhs_2024,
            dplyr::join_by(vessel_reg_uscg_ == uscg__))

dplyr::glimpse(sc__srhs_join)

# Get all the combinations of SC and SRHS lists.
# In this results we have:
# 1               0 NA
# Both are not SRHS
# 2               1 Y
# Both are SRHS
# 3              NA Y
# The vessel is not in the SC list, which is expected.

# For this SC entry file there are no discrepancies, so we can simply remove all the vessels marked as reports_to_srhs from the future analysis. We don't have compliance information for them.

sc__srhs_join |>
  dplyr::select(reports_to_srhs, is_insurvey) |>
  dplyr::distinct()

## Keep only non-SRHS vessels ----
SC_permittedVessels_longer_m_y_no_srhs <-
  SC_permittedVessels_longer_m_y |>
  dplyr::filter(reports_to_srhs == 0)

dim(SC_permittedVessels_longer_m_y)
# [1] 2736    9
dim(SC_permittedVessels_longer_m_y_no_srhs)
# [1] 2640    9

# combine data ----

# Join the SC data with the compliance data we prepared, by vessel, month and year.

# Explanations:
# 1. 'sc__fhier_compl__join_w_month' is created by left joining two data frames: 'SC_permittedVessels_longer_m_y_no_srhs' and 'compl_override_data__renamed_m_short__m_compl'.
# 2. The join is performed based on the following conditions:
#    a. The vessel registration number from 'SC_permittedVessels_longer_m_y_no_srhs' matches the vessel official number from 'compl_override_data__renamed_m_short__m_compl'.
#    b. The month_sc column (representing the month in 'SC_permittedVessels_longer_m_y_no_srhs') falls within the range of months (comp_month_min to comp_month_max) in 'compl_override_data__renamed_m_short__m_compl'.
#    c. The year_sc column (representing the year in 'SC_permittedVessels_longer_m_y_no_srhs') matches the comp_year column in 'compl_override_data__renamed_m_short__m_compl'.

# SC_permittedVessels_longer_m_y_no_srhs |> print_df_names()
#   select(contains("month")) |> head()

# compl_override_data__renamed_m_short__m_compl |>
#   select(contains("month")) |> glimpse()
# by <- join_by(id, closest(sale_date >= promo_date), sale_date_lower <= promo_date)
# full_join(sales, promos, by)

sc__fhier_compl__join_w_month <-
  dplyr::left_join(
    SC_permittedVessels_longer_m_y_no_srhs,
    compl_override_data__renamed_m_short__m_compl,
    dplyr::join_by(
      vessel_reg_uscg_ == vessel_official_number,
      month_sc >= comp_month_min,
      month_sc <= comp_month_max,
      year_sc == comp_year,
    )
  )

sc__fhier_compl__join_w_month |>
  filter(year_sc == my_year &
           month_sc %in% last_2_m) |> View()

# check
dim(SC_permittedVessels)
# [1] 228  18
dim(SC_permittedVessels_longer_m_y_no_srhs)
# [1] 2640    9
dim(sc__fhier_compl__join_w_month)
# [1] 10512    24
n_distinct(SC_permittedVessels)
# 228
n_distinct(sc__fhier_compl__join_w_month$vessel_reg_uscg_)
# 220 (rm SRHS)
# glimpse(sc__fhier_compl__join_w_month)

sc__fhier_compl__join_w_month |>
  select(contains("month")) |>
  distinct() |>
  filter(!month_comp_min == month_comp_max) |>
  glimpse()
# 46

# Get only the 2 last months ----
month_now <- lubridate::month(lubridate::today())
last_2_m <- c(month_now - 1, month_now - 2)

sc__fhier_compl__join_w_month_last2 <-
  sc__fhier_compl__join_w_month |>
  dplyr::filter(year_sc == my_year &
                  month_sc %in% last_2_m)

dim(sc__fhier_compl__join_w_month_last2)
# [1] 1906   24
#
