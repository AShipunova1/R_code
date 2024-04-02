# processing_logbook_data

# TODO: add comments

# General set up ----

# load required packages (install first if needed)
library(ROracle)
library(tidyverse)
# see the list of packages: tidyverse_packages()

library(tictoc) # Functions for timing
library(crayon) # Colored terminal output

# set working and output directory - where do you keep the data and analysis folder on your computer?
michelles_path <-
  "C:/Users/michelle.masi/Documents/SEFHIER/R code/DNF related analyses/DNF Processing (Do this before all DNF Analyses)/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingDNFData/"
# r"(C:\Users\jenny.ostroff\Desktop\Backups\Rcode\ProcessingDNFData)"

# Input files are the same here
annas_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data/)"

# !! Change to your path !!
# Change to use another path instead:
# Path <- michelles_path
Path <- annas_path

Inputs <- "Inputs"
Outputs <- "Outputs"

output_file_path <-
  file.path(Path, Outputs)

# Set the date ranges for the DNF and compliance data you are pulling
# this is the year to assign to the output file name
# my_year <- "2022"
# my_year <- "2023"
my_year <- "2024"

my_date_beg <- str_glue("01-JAN-{my_year}")
my_date_end <- str_glue("31-DEC-{my_year}")

# years range for srfh_vessel_comp db download, see below
db_year_1 <- "2021"
db_year_2 <- "2024"

# Auxiliary methods ----
if (Path == annas_path) {
  annas_git_path <-
    r"(~\R_code_github\get_data)"

  auxiliary_methods_file_path <-
    file.path(annas_git_path,
              "processing_auxiliary_methods.R")
} else {
  auxiliary_methods_file_path <-
    file.path(Path,
              "processing_auxiliary_methods.R")
}

# file.exists(auxiliary_methods_file_path)

source(auxiliary_methods_file_path)

# Start the log ----
my_tee(date(),
       my_title = str_glue("Start DNF processing for {my_year}"))

# Get data ----

# This section is needed to pull data from Oracle database
# to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

# set up an Oracle connection
# Sys.getenv("ORA_SDTZ")

# You have to set up the same timezones for ROracle and the system. Because by default they use different ones.
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

tic("try_connection")
try(con <- connect_to_secpr())
toc()

## Import and prep compliance/override data ----

### Import compliance/override data ----
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(Path,
            Outputs,
            str_glue("Raw_Oracle_Downloaded_compliance_2021_plus.rds"))
# File: Raw_Oracle_Downloaded_compliance_2021_plus.rds modified 2024-02-05 09:52:06.996529

# Check if the file path is correct, optional
# file.exists(compl_override_data_file_path)

# 2) Create a variable with a table name to call data from, define year.
# >= 2021 because of when the program started or between 2 years defined above
compl_err_query <-
  str_glue(
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '{db_year_1}' AND comp_year <= '{db_year_2}'")

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

# use to force the DB download
# force_from_db = T

compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query
                        )

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data__renamed <-
  compl_override_data |>
  dplyr::rename(VESSEL_OFFICIAL_NUMBER =
                  "VESSEL_OFFICIAL_NBR",
                OVERRIDDEN = "IS_COMP_OVERRIDE")

# stats
my_stats(compl_override_data__renamed)
# rows: 460724
# columns: 23
# Unique vessels: 4390

# stats
min(compl_override_data__renamed$COMP_WEEK_START_DT)
# [1] "2021-01-04 EST"

# keep only year of analysis, including the week 52 of the previous year if needed
compl_override_data__renamed__this_year <-
  compl_override_data__renamed |>
  filter(
    COMP_WEEK_END_DT >= as.Date(my_date_beg, "%d-%b-%Y",
                                tz = Sys.timezone()) &
      COMP_WEEK_START_DT <= as.Date(my_date_end, "%d-%b-%Y",
                                    tz = Sys.timezone())
  )
# check
# That's the week 52 of the previous year (my_year - 1):
min(compl_override_data__renamed__this_year$COMP_WEEK_START_DT)
# [1] "2021-12-27 EST" # this might contain the last week in the year before my_year, to account for a compliance week that overlaps last week of the year and first week of my_year
min(compl_override_data__renamed__this_year$COMP_WEEK_END_DT)
# [1] "2022-01-02 EST" # this should be the last day of the first week in my_year

# change data type of this column if needed
if (!class(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER) == "character") {
  compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER <-
    as.character(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER)
}

## Import the permit data ----
processed_metrics_tracking_path <-
  file.path(Path,
            Outputs,
            str_glue("SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds"))
# some may make this file path the Inputs file, because you are inputting this file into this script
# it doesn’t matter as long as your file location on your computer matches what you say here.

# optional
# file.exists(processed_metrics_tracking_path)

processed_metrics_tracking <-
  read_rds(processed_metrics_tracking_path)

## Import and prep the logbook data ----

# Import the logbook data from file or database
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) create a variable with file path to read or write the logbook file

logbooks_file_path <-
  file.path(Path,
            Outputs,
            str_glue("Raw_Oracle_Downloaded_logbook_{my_date_beg}__{my_date_end}.rds"))

# 2) create a variable with an SQL query to call data from the database

# stringr::str_glue:
# Interpolation with glue to include variable names

logbooks_download_query <-
  str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_start_date >= '{my_date_beg}'
  AND trip_start_date <= '{my_date_end}'
")

# Use 'read_rds_or_run_query' defined above to either read logbook information from an RDS file or execute a query to obtain it and write a file for future use.
Logbooks <-
  read_rds_or_run_query(logbooks_file_path,
                        logbooks_download_query,
                        force_from_db = NULL
                        )
# 2024-02-05 run for Raw_Oracle_Downloaded_logbook_01-JAN-2022__31-DEC-2022.rds: 122.37 sec elapsed

# Rename column to be consistent with other dataframes
Logbooks <-
  rename(Logbooks,
         VESSEL_OFFICIAL_NUMBER =
           "VESSEL_OFFICIAL_NBR")

# stats
my_stats(Logbooks, "Logbooks from the db")
# 2022
# rows: 327847
# columns: 149
# Unique vessels: 1885
# Unique trips (logbooks): 94737

# rows: 484413
# columns: 149
# Unique vessels: 2218
# Unique trips (logbooks): 143456

# reformat trip start/end date
# Explanation:
#
# 1. **Create New Dataframe:**
#    - `Logbooks <- Logbooks |> ...`: Create a new dataframe 'Logbooks' by using the pipe operator '|>' on the existing 'Logbooks'.
#
# 2. **Use 'mutate' to Convert Columns:**
#    - `mutate(across(..., as.Date))`: Utilize the 'mutate' function with 'across' to apply a transformation to multiple columns.
#
# 3. **Column Selection with 'across':**
#    - `c(!where(is.Date) & ends_with("_DATE"))`: Select columns that meet the specified conditions:
#      - `!where(is.Date)`: Columns that are not already of type 'Date'.
#      - `ends_with("_DATE")`: Columns whose names end with "_DATE".
#
# 4. **Convert Columns to Date:**
#    - `as.Date`: Use the 'as.Date' function to convert the selected columns to the 'Date' format.

Logbooks <-
  Logbooks |>
  mutate(across(c(!where(is.Date) & ends_with("_DATE")),
                as.Date))

# Check
# Logbooks$TRIP_START_DATE |>
#   class()
# before
# "POSIXct" "POSIXt"
# after
# "Date"

# Was:
# Logbooks$TRIP_START_DATE |>
#   head(1)
# "2022-07-07 01:00:00 EDT"

# Now:
# Logbooks$TRIP_START_DATE |>
#   head(1)
# "2022-07-07"

# reformat trip start/end time
time_col_names <-
  c("TRIP_START_TIME",
    "TRIP_END_TIME")

# convert time columns to numeric,
# then format to 4 digits as a string (some time entries were like "800")
# (from help: sprintf returns a character vector)

# Explanation:
#
# 1. **Create New Dataframe:**
#    - `Logbooks <- Logbooks |> ...`: Create a new dataframe 'Logbooks' by using the pipe operator '|>' on the existing 'Logbooks'.
#
# 2. **Use 'mutate' to Convert Columns to Numeric:**
#    - `mutate(across(..., as.numeric))`: Utilize the 'mutate' function with 'across' to apply a transformation to specific non-numeric columns, converting them to numeric using 'as.numeric'.
#
# 3. **Column Selection with 'across':**
#    - `c(!where(is.numeric) & all_of(time_col_names))`: Select columns that meet the specified conditions:
#      - `!where(is.numeric)`: Columns that are not already of type 'numeric'.
#      - `all_of(time_col_names)`: Columns specified by 'time_col_names'.
#
# 4. **Convert Columns to Numeric:**
#    - `as.numeric`: Use the 'as.numeric' function to convert the selected columns to numeric.
#
# 5. **Use 'mutate' to Format Columns with Leading Zeros:**
#    - `mutate(across(..., ~ sprintf("%04d", .x)))`: Utilize 'mutate' with 'across' to format specific columns specified by 'time_col_names' with leading zeros using the 'sprintf' function.
#
# 6. **Column Selection with 'across':**
#    - `all_of(time_col_names)`: Select columns specified by 'time_col_names'.
#
# 7. **Format Columns with Leading Zeros:**
#    - `~ sprintf("%04d", .x)`: Format each column value with leading zeros using 'sprintf("%04d", .x)'.

Logbooks <-
  Logbooks |>
  mutate(across(c(
    !where(is.numeric) & all_of(time_col_names)
  ),
  as.numeric)) |>
  mutate(across(all_of(time_col_names),
                ~ sprintf("%04d", .x)))

### Filter out just my analysis year logbook entries ----

# check
min(Logbooks$TRIP_START_DATE)
# [1] "2022-01-01"
# [1] "2023-01-01"
max(Logbooks$TRIP_START_DATE)
# [1] "2022-12-31"
# [1] "2023-12-31"

# TODO: rename
Logbooks <-
  Logbooks |>
  filter(TRIP_START_DATE >= as.Date(my_date_beg, "%d-%b-%Y") &
           TRIP_START_DATE <= as.Date(my_date_end, "%d-%b-%Y"))

# stats, to compare with the end result
logbooks_stat_correct_dates_before_filtering <-
  c(dim(Logbooks),
    n_distinct(Logbooks$VESSEL_OFFICIAL_NUMBER),
    n_distinct(Logbooks$TRIP_ID)
  )

my_stats(Logbooks, "Logbooks after filtering by dates")
# rows: 327773
# columns: 149
# Unique vessels: 1882
# Unique trips (logbooks): 94714

# check
min(Logbooks$TRIP_START_DATE)
# [1] "2022-01-01"
max(Logbooks$TRIP_START_DATE)
# [1] "2022-12-31"

min(Logbooks$TRIP_END_DATE)
# [1] "2018-06-04"
max(Logbooks$TRIP_END_DATE)
# [1] "2023-05-26"

# create column for start date & time
tic("format time")
Logbooks$STARTDATETIME <-
  as.POSIXct(paste(Logbooks$TRIP_START_DATE,                                           Logbooks$TRIP_START_TIME),
             format = "%Y-%m-%d %H%M")
toc()
# format time: 4.38 sec elapsed

# check
Logbooks$STARTDATETIME |>
  head(1)
# "2022-07-07 08:00:00 EDT"

# create column for end date & time
Logbooks$ENDDATETIME <-
  as.POSIXct(paste(Logbooks$TRIP_END_DATE,                                         Logbooks$TRIP_END_TIME),
             format = "%Y-%m-%d %H%M")

Logbooks |> filter(is.na(VESSEL_OFFICIAL_NUMBER))
# 0 OK

### Prepare data to determine what weeks were overridden, so we can exclude logbooks from those weeks later ----

# not needed for processing
compl_override_data__renamed__this_year |>
  select(COMP_YEAR,
         COMP_WEEK_END_DT,
         COMP_WEEK) |>
  distinct() |>
  arrange(COMP_WEEK_END_DT) |>
  head(3)
# 2022
#   COMP_YEAR COMP_WEEK_END_DT COMP_WEEK
# 1      2021       2022-01-02        52
# 2      2022       2022-01-09         1
# 3      2022       2022-01-16         2

# 2023
#   COMP_YEAR COMP_WEEK_END_DT COMP_WEEK
# 1      2022       2023-01-01        52
# 2      2023       2023-01-08         1
# 3      2023       2023-01-15         2

# Needed to adjust for week 52 of the previous year and use in joins
Logbooks <-
  Logbooks |>
  mutate(TRIP_END_WEEK = isoweek(TRIP_END_DATE), # puts it in week num
         TRIP_END_YEAR = isoyear(TRIP_END_DATE)) # adds a year


# Adding flags or filtering the logbook data ----

## Filter out vessels not in Metrics tracking ----
SEFHIER_compl_override_data__renamed__this_year <-
  compl_override_data__renamed__this_year |>
  filter(VESSEL_OFFICIAL_NUMBER %in% processed_metrics_tracking$VESSEL_OFFICIAL_NUMBER)

my_stats(compl_override_data__renamed__this_year)
my_stats(SEFHIER_compl_override_data__renamed__this_year)

vessels_not_in_metrics <-
  n_distinct(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER)

vessels_not_in_metrics
# 289 2022

# TODO: fix my_tee
# my_tee(vessels_not_in_metrics,
#        "Vessels removed if a vessel is not in Metrics tracking")
# 1556 (2022)

logbooks_not_in_metrics <-
  n_distinct(compl_override_data__renamed__this_year$TRIP_ID) -
  n_distinct(SEFHIER_compl_override_data__renamed__this_year$TRIP_ID)
# 0

# my_tee(dnfs_not_in_metrics,
#        "DNFs removed if a vessel is not in Metrics tracking")
# 356497 (2022)
# 446859 (2023)

## add compliance/override data to logbooks ----
my_stats(compl_override_data__renamed__this_year,
         "Compliance and override data from the db")
# 2022
# rows: 150029
# columns: 23
# Unique vessels: 3626

### TODO: check if logbooks and compliance data have the same week dates

### join the data frames ----
# Logbooks |> names() |> cat('", "')
# intersect(sort(names(Logbooks)),
#           sort(names(
#             SEFHIER_compl_override_data__renamed__this_year
#           )))

# grep("year", names(Logbooks), ignore.case = T, value = T)
# grep("week", names(Logbooks), ignore.case = T, value = T)

logbooks_join_overr <-
  full_join(
    Logbooks,
    SEFHIER_compl_override_data__renamed__this_year,
    join_by(TRIP_END_YEAR == COMP_YEAR,
            VESSEL_OFFICIAL_NUMBER,
            TRIP_END_WEEK == COMP_WEEK),
    relationship = "many-to-many"
  )

# check the difference
in_compl_not_in_logbooks <-
  logbooks_join_overr |>
  filter(is.na(TRIP_ID)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_compl_not_in_logbooks)
# 3320

in_logbooks_not_in_compl <-
  logbooks_join_overr |>
  filter(is.na(IS_COMP)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_logbooks_not_in_compl)
# 140

# TODO: validate in_compl_not_in_dnfs and in_dnfs_not_in_compl
# my_year
# getwd()
# write_rds(as_tibble(in_logbooks_not_in_compl),
#           file.path(output_file_path,
#             "in_dnfs_not_in_compl.rds"))

# stats
my_stats(SEFHIER_compl_override_data__renamed__this_year)
# 2022
# rows: 440307
# columns: 8
# Unique vessels: 2020
# Unique trips: 440307
my_stats(logbooks_join_overr)
# 2022
# rows: 441000
# columns: 28
# Unique vessels: 2020
# Unique trips: 440307

### Remove rows with NA logbooks and entries in Compiance ----
logbooks_join_overr__all_logbooks <-
  logbooks_join_overr |>
  filter(!is.na(TRIP_ID))

dim(logbooks_join_overr)
dim(logbooks_join_overr__all_logbooks)

### Add a compliant_after_override column ----
tic("Add a compliant_after_override column")
logbooks_join_overr__compl <-
  logbooks_join_overr__all_logbooks |>
  rowwise() |>
  mutate(
    compliant_after_override =
      case_when(IS_COMP == 0 & OVERRIDDEN == 0  ~ "no",
                IS_COMP == 1 ~ "yes",
                OVERRIDDEN == 1 ~ "yes",
                is.na(IS_COMP) ~ NA,
                .default = toString(IS_COMP))
  ) |>
  ungroup()
toc()
# Add a compliant_after_override column: 91.81 sec elapsed

logbooks_join_overr__all_logbooks |>
  select(IS_COMP,
         OVERRIDDEN) |>
  distinct()
#   IS_COMP OVERRIDDEN
# 1       1          0
# 2       1          1
# 3      NA         NA
# 4       0          1
# 5       0          0

logbooks_join_overr |>
  filter(IS_COMP == 1 & OVERRIDDEN == 1) |>
  select(TRIP_ID) |>
  distinct() |>
  dim()
# 401

logbooks_join_overr__compl |>
  select(compliant_after_override,
         IS_COMP,
         OVERRIDDEN) |>
  distinct()
#   compliant_after_override IS_COMP OVERRIDDEN
# 1 yes                            1          0
# 2 yes                            1          1
# 3 NA                            NA         NA
# 4 yes                            0          1
# 5 no                             0          0

## Start date/time is after end date/time ----
# check logbook records for cases where start date/time is after end date/time, delete these records

# the Time Stamp Error is true if start date/time is greater than or equal to end date/time, false if not
logbooks_join_overr__compl['time_stamp_error'] <-
  ifelse(
    logbooks_join_overr__compl$STARTDATETIME >= logbooks_join_overr__compl$ENDDATETIME,
    TRUE,
    FALSE
  )

### Filter: only keep the rows where there is no error between start & end date & time ----
logbooks_join_overr__compl__start_end_ok <-
  logbooks_join_overr__compl |>
  filter(time_stamp_error == FALSE)

# stats
my_stats(logbooks_join_overr__compl__start_end_ok)
# rows: 310832
# columns: 153
# Unique vessels: 1825
# Unique trips (logbooks): 89797

# stats
thrown_by_time_stamp_error <-
  logbooks_join_overr__compl |>
  filter(time_stamp_error == TRUE)

# my_tee(n_distinct(thrown_by_time_stamp_error$TRIP_ID),
#        "Thrown away by time_stamp_error (logbooks num)")
# 550

## Delete logbooks for trips lasting more than 10 days ----

# The assumption is there is an error in either start or end date and time and the trip didn't really last that long.

logbooks_join_overr__compl__start_end_ok['trip_length'] <-
  as.numeric(
    difftime(
      logbooks_join_overr__compl__start_end_ok$ENDDATETIME,
      logbooks_join_overr__compl__start_end_ok$STARTDATETIME,
      units = "hours"
    )
  )

### Filter: only keep trips with a length less than or equal to 10 days (240 hours) ----

logbooks_join_overr__compl__start_end_ok__trip_len_ok <-
  logbooks_join_overr__compl__start_end_ok |>
  filter(trip_length <= 240)

# stats
my_stats(logbooks_join_overr__compl__start_end_ok__trip_len_ok)
# rows: 310741
# columns: 154
# Unique vessels: 1822
# Unique trips (logbooks): 89762

# Output trips with length > 240 into a data frame (for stats)
logbooks_too_long <-
  logbooks_join_overr__compl__start_end_ok |>
  filter(trip_length > 240)

# my_tee(n_distinct(logbooks_too_long$TRIP_ID),
#        "Thrown away by trip_more_10_days (logbooks num)")
# trip_ids: 35

my_tee(n_distinct(logbooks_too_long$VESSEL_ID),
       "Thrown away by trip_more_10_days (vessels num)")
# 30

## Mark all trips that were received > 30 days after the trip end date, by using compliance data and time of submission ----
### add the threshold date ----
logbooks_join_overr_e <-
  logbooks_join_overr__compl__start_end_ok |>
  mutate(TRIP_END_DATE_E =
           ymd_hms(TRIP_END_DATE,
                   truncated = 3,
                   tz = Sys.timezone()))

# add a date 30 days later with a time
# logbooks_notoverridden <-
#   logbooks_notoverridden |>
logbooks_join_overr_e_usable_date <-
  logbooks_join_overr_e |>
  mutate(USABLE_DATE_TIME =
           TRIP_END_DATE_E + days(30)) |>
  mutate(USABLE_DATE_TIME =
           `hour<-`(USABLE_DATE_TIME, 23)) |>
  mutate(USABLE_DATE_TIME =
           `minute<-`(USABLE_DATE_TIME, 59)) |>
  mutate(USABLE_DATE_TIME =
           `second<-`(USABLE_DATE_TIME, 59))


# subtract the usable date from the date of submission
# value is true if the logbook was submitted within 30 days, false if the logbook was not

late_submission_filter_stats <-
  function(my_df) {
    # stats
    my_stats(my_df)
    # rows: 271479
    # columns: 155
    # Unique vessels: 1629
    # Unique trips (logbooks): 73313

    late_submission <-
      my_df |>
      filter(MORE_THAN_30_DAYS_LATE == FALSE)

    # my_tee(n_distinct(late_submission$TRIP_ID),
           # "Count late_submission (logbooks num)")
    # trip_ids: 16449

    # my_tee(
      # n_distinct(late_submission$VESSEL_OFFICIAL_NUMBER),
      # "Count late_submission (vessels num)"
    # )
    # 1064

    # check
    min(my_df$TRIP_START_DATE)
    # [1] "2022-01-01"
    max(my_df$TRIP_START_DATE)
    # [1] "2022-12-31"
    min(my_df$TRIP_END_DATE)
    # [1] "2022-01-01"
    max(my_df$TRIP_END_DATE)
    # [1] "2022-12-31"

  }

late_submission_filter <-
  function(my_df) {

    logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp <-
      my_df |>
      mutate(MORE_THAN_30_DAYS_LATE =
               ifelse(USABLE_DATE_TIME >= TRIP_DE, TRUE, FALSE))

    late_submission_filter_stats(logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp)

    # late_submissions_flag = "_no_late_submissions"
    return(logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp)
  }

### Filter (mark only): data frame of logbooks that were usable ----
SEFHIER_logbooks_processed <-
  late_submission_filter(logbooks_join_overr_e_usable_date)

# Separate permit regions to GOM only, SA only or dual using PERMIT_GROUP ----
# Revisit after
# fixing metrics tracking for transferred permits
# Reason: Metrics tracking may not be tracking permit status change over the year (e.g. transferred permits)

# Data example:
# SEFHIER_logbooks_processed |>
#   select(PERMIT_GROUP) |>
#   distinct() |>
#   tail(3)
# PERMIT_GROUP
# (CDW)CDW, (CHG)1615, (CHS)CHS, (SC)SC
# (CHG)1034, (RCG)982
# (CHG)589, (RCG)567

# Auxiliary: how to find the column name
#
# grep("permit",
#      names(SEFHIER_logbooks_processed),
#      value = TRUE,
#      ignore.case = TRUE)

# Explanation:
#
# 1. **Create New Dataframe:**
#    - `SEFHIER_logbooks_processed_regions <- SEFHIER_logbooks_processed |> ...`: Create a new dataframe 'SEFHIER_logbooks_processed_regions' based on the 'SEFHIER_logbooks_processed' dataframe.
#
# 2. **Use 'mutate' to Add Column:**
#    - `mutate(permit_sa_gom = dplyr::case_when(...))`: Utilize the 'mutate' function to add a new column 'permit_sa_gom' with values determined by the conditions specified in the 'case_when' function.
#
# 3. **Conditions with 'case_when':**
#    - `!grepl("RCG|HRCG|CHG|HCHG", PERMIT_GROUP) ~ "sa_only"`: If 'PERMIT_GROUP' does not contain the specified patterns, assign "sa_only".
#    - `!grepl("CDW|CHS|SC", PERMIT_GROUP) ~ "gom_only"`: If 'PERMIT_GROUP' does not contain the specified patterns, assign "gom_only".
#    - `.default = "dual"`: For any other case, assign "dual".
#
# 4. **'dplyr::' Prefix:**
#    - `dplyr::case_when(...)`: Prefix 'dplyr::' is used to explicitly specify that the 'case_when' function is from the 'dplyr' package, ensuring there is no ambiguity if other packages also have a 'case_when' function.

SEFHIER_logbooks_processed_p_regions <-
  SEFHIER_logbooks_processed |>
  mutate(
    permit_sa_gom =
      dplyr::case_when(
        !grepl("RCG|HRCG|CHG|HCHG", PERMIT_GROUP) ~
          "sa_only",
        !grepl("CDW|CHS|SC", PERMIT_GROUP) ~ "gom_only",
        .default = "dual"
      )
  )

# stats
my_stats(SEFHIER_logbooks_processed)

logbooks_before_filtering <-
  n_distinct(Logbooks$TRIP_ID)

my_tee(logbooks_before_filtering,
        "Logbooks before filtering")
# 94737 2022
# 52393 2023

logbooks_after_filtering <-
  n_distinct(SEFHIER_logbooks_processed$TRIP_ID)

my_tee(logbooks_after_filtering,
        "Logbooks after filtering")
# [1] 73313
# 51340 2023
# 89621 2022

percent_of_removed_logbooks <-
  (logbooks_before_filtering - logbooks_after_filtering) * 100 / logbooks_before_filtering

cat(percent_of_removed_logbooks, sep = "\n")
# 22.59539
# 2.00981 (with late submission)
# 5.400213 2022
# 0.7146099 if keep overridden

# removed_vessels
vessels_before_filtering <-
  n_distinct(Logbooks$VESSEL_OFFICIAL_NUMBER)

cat(vessels_before_filtering)
# 1885 2022
# 1646 2023

vessels_after_filtering <-
  n_distinct(SEFHIER_logbooks_processed$VESSEL_OFFICIAL_NUMBER)

cat(vessels_after_filtering)
# 1823 2022
# 1597 2023

removed_vessels <-
  vessels_before_filtering - vessels_after_filtering
# 253
# 49
# 3

percent_of_removed_vessels <-
  (vessels_before_filtering - vessels_after_filtering) * 100 / vessels_before_filtering
# [1] 13.44315
# [1] 2.976914

removed_logbooks_and_vessels_text <- c(
  crayon::blue("percent_of_removed_logbooks"),
  str_glue("{round(percent_of_removed_logbooks)}%"),
  crayon::blue("removed_vessels"),
  removed_vessels,
  crayon::blue("percent_of_removed_vessels"),
  str_glue("{round(percent_of_removed_vessels)}%")
)

my_tee(removed_logbooks_and_vessels_text,
       "\nRemoved logbooks and vessels stats")

# Export processed logbooks ----
SEFHIER_processed_Logbooks_file_name <-
  str_glue("SEFHIER_processed_Logbooks_{my_year}.rds")

annas_file_path <-
  file.path(Path,
            "Outputs",
            SEFHIER_processed_Logbooks_file_name)

jennys_file_path <-
  file.path(Path,
            Outputs,
            SEFHIER_processed_Logbooks_file_name)

michelles_file_path <-
  file.path(Path,
            Outputs,
            SEFHIER_processed_Logbooks_file_name)

# !! Change to the correct path !!
output_file_path <-
  annas_file_path

write_rds(
  SEFHIER_logbooks_processed,
  file = output_file_path
)

