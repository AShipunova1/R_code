# processing_logbook_data

# Creates:
# 1) The result will be in
# SEFHIER_processed_Logbooks_{my_year}.rds
# 2)
# vessels_with_zero_logbooks_{my_year}.rds

# Files to read or create:
# 1) Raw_Oracle_Downloaded_compliance_2021_plus.rds
# 2) Raw_Oracle_Downloaded_logbook_{my_date_beg}__{my_date_end}.rds
# 3) SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds

# This code processes logbook data from Oracle database,
# then cleans it up, so that we can use it in any logbook data analysis:
# (1) (a) pull all logbook and compliance/override data from Oracle database
#     (b) get processed Metrics Tracking
# (2) clean up logbook data set
#   (a) remove records from SRHS vessels
#   (b) remove records where start date/time is after end date/time
#   (c) remove records for trips lasting more than 10 days
# (3) remove all trips that were received > 30 days after trip end date, by using compliance data and time of submission
#   (a) remove all overridden data, because the submission date is unknown
# (4) Mark late submission data
# (5) Add permit region information (GOM, SA, or dual), using permit names (optional)

# For 2022 we don't keep trips starting in 2021 and ending in 2022. We only keep trips starting in 2022.

# Caveats:
# 1) The way COMP_WEEK is calculated could get messed up depending on a given year time frame. It's due to something
# internal called the ISO number and how the function calculates the start of a week. If you are running this on a
# new data set, check your weeks to make sure it's calculating correctly.

# Running the code
# To run the file as a whole, you can type this in the console: source('Processing Logbook Data.R') and hit enter.
# Pressing F2 when the custom function name is under the cursor will show the function definition.
# Pressing F1 when the R function name is under the cursor will show the function definition
# and examples in the help panel.

# General set up ----

# load required packages
library(ROracle)
library(xlsx)
library(tidyverse)
# see the list of packages: tidyverse_packages()

library(tictoc) # Functions for timing
library(crayon) # Colored terminal output

# set working and output directory - where do you keep the data and analysis folder on your computer?
michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Logbook related analyses/Logbook Processing (Do this before all Logbook Analyses)/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingLogbookData/"

annas_path <-
  r"(C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\processing_logbook_data/)"

# Change to use another path instead:
# Path <- michelles_path
Path <- annas_path

Inputs <- "Inputs/"
Outputs <- "Outputs/"

# Set the date ranges for the logbook and compliance data you are pulling
# this is the year to assign to the output file name
# my_year <- "2022"
# my_year <- "2023"
my_year <- '2024'

my_date_beg <- str_glue("01-JAN-{my_year}")
my_date_end <- str_glue("31-DEC-{my_year}")

# Auxiliary methods ----
annas_git_path <-
r"(~\R_code_github\get_data)"

if (Path == annas_path) {
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
       my_title = str_glue("Start logbooks processing for {my_year}"))

# Get data ----

# This section is needed to pull data from Oracle database
# to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

# set up an Oracle connection
tic("try_connection")
try(con <- connect_to_secpr())
toc()
# try_connection: 21.7 sec elapsed if no connection
# try_connection: 1.63 sec elapsed if works normally

## Import and prep compliance/override data ----

### Import compliance/override data ----
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# compl_override_data_file_path, e.g.
#   "//ser-fs1/sf/LAPP-DM Documents\\Ostroff\\SEFHIER\\Rcode\\ProcessingLogbookData\\Inputs\\compl_err_db_data_raw.rds"

# 1) Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(Path,
            Outputs,
            str_glue("Raw_Oracle_Downloaded_compliance_2021_plus.rds"))

# 2) Create a variable with a table name to call data from, define year.
# >= 2021 because of when the program started
compl_err_query <-
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '2021'"

# Check if the file path is correct, optional
# file.exists(compl_override_data_file_path)

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query)
# 2024-02-05 run for Raw_Oracle_Downloaded_compliance_2021_plus.rds: 104.5 sec elapsed
# File: Raw_Oracle_Downloaded_compliance_2021_plus.rds modified Mon Feb  5 09:52:06 2024

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data <-
  compl_override_data |>
  dplyr::rename(VESSEL_OFFICIAL_NUMBER =
                  "VESSEL_OFFICIAL_NBR",
                OVERRIDDEN = "IS_COMP_OVERRIDE")

# stats
my_stats(compl_override_data)
# rows: 460724
# columns: 23
# Unique vessels: 4390

# stats
min(compl_override_data$COMP_WEEK_START_DT)
# [1] "2021-01-04 EST"

# keep only year of analysis, including the week 52 of the previous year
compl_override_data_this_year <-
  compl_override_data |>
  filter(COMP_WEEK_END_DT >= as.Date(my_date_beg, "%d-%b-%Y") &
           COMP_WEEK_START_DT <= as.Date(my_date_end, "%d-%b-%Y"))

# check
# That's the week 52 of my_year-1:
min(compl_override_data_this_year$COMP_WEEK_START_DT)
# [1] "2021-12-27 EST" #this should be the last week in the year before my_year, to account for compliance week that overlaps last week of the year and first week of my_year
min(compl_override_data_this_year$COMP_WEEK_END_DT)
# [1] "2022-01-02 EST" #this should be the 1st date in my_year

# change data type of this column if needed
if (!class(compl_override_data_this_year$VESSEL_OFFICIAL_NUMBER) == "character") {
  compl_override_data_this_year$VESSEL_OFFICIAL_NUMBER <-
    as.character(compl_override_data_this_year$VESSEL_OFFICIAL_NUMBER)
}

## Import the permit data ----
processed_metrics_tracking_path <-
  file.path(Path,
            Outputs,
            str_glue("SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds"))

# file.exists(processed_metrics_tracking_path)

SEFHIER_permit_info_short_this_year <-
  read_rds(processed_metrics_tracking_path)

## Import and prep the logbook data ####

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
                        logbooks_download_query)
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

### Prepare data to determine what weeks were overridden, so we can exclude logbooks from those weeks later ----

# assign each logbook a week designation (first day of the reporting week is a Monday)
# use the end date to calculate this, it won't matter for most trips, but for some trips that
# happen overnight on a Sunday, it might affect what week they are assigned to
#https://stackoverflow.com/questions/60475358/convert-daily-data-into-weekly-data-in-r

# Calculate the ISO week number for each date in the 'TRIP_END_DATE2' column.
# lubridate package has following methods:
# week() returns the number of complete seven day periods that have occurred between the date and January 1st, plus one.
#
# isoweek() returns the week as it would appear in the ISO 8601 system, which uses a reoccurring leap week.
#
# epiweek() is the US CDC version of epidemiological week. It follows same rules as isoweek() but starts on Sunday. In other parts of the world the convention is to start epidemiological weeks on Monday, which is the same as isoweek.
#

# Needed to adjust for week 52 of the previous year
Logbooks <-
  Logbooks |>
  mutate(COMP_WEEK = isoweek(TRIP_END_DATE), # puts it in week num
         TRIP_END_YEAR = isoyear(TRIP_END_DATE)) # adds a year

# to see the respective data in compl_override_data_this_year, note the last week of 2021
# not needed for processing
compl_override_data_this_year |>
  select(COMP_YEAR,
         COMP_WEEK_END_DT,
         COMP_WEEK) |>
  distinct() |>
  arrange(COMP_WEEK_END_DT) |>
  head(3)
#   COMP_YEAR COMP_WEEK_END_DT COMP_WEEK
# 1      2021       2022-01-02        52
# 2      2022       2022-01-09         1
# 3      2022       2022-01-16         2

## add override data to logbooks ----
my_stats(compl_override_data_this_year,
         "Compl/override data from the db")
# rows: 151515
# columns: 19
# Unique vessels: 3740

# We need 'relationship = "many-to-many"' because
# Each row represents a catch ID within the effort ID within the Trip ID, for a given vessel. So there are many rows that will match the same vessel and year/week.

logbooks_join_overr <-
  left_join(Logbooks,
            compl_override_data_this_year,
            join_by(TRIP_END_YEAR == COMP_YEAR,
                    VESSEL_OFFICIAL_NUMBER,
                    COMP_WEEK),
            relationship = "many-to-many"
  )

# stats
my_stats(Logbooks)
my_stats(logbooks_join_overr)
# rows: 327818
# columns: 169
# Unique vessels: 1882
# Unique trips (logbooks): 94714

# Make lists of overridden or not vessels
# If a week for a vessel was overridden (compl_override_data_this_year), remove the trip reports from the corresponding week in the logbook data
# We have to remove logbooks for weeks that were overridden because we don't have a timestamp for when the logbook was submitted to the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate that time laps.
# We can't differentiate between turning a logbook in on time in the app, and it taking two months to get it vs turning in a logbook two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and SAFIS) in March, 2022 (At permit renewal)... user submitted on time in app (VESL) but we may not get that report in SAFIS for months later (when its found as a "missing report" and then requeued for transmission)

logbooks_overridden <-
  filter(logbooks_join_overr, OVERRIDDEN == 1) #data frame of logbooks that were overridden

# stats
my_stats(logbooks_overridden)
# rows: 10136
# columns: 169
# Unique vessels: 286
# Unique trips (logbooks): 2905

logbooks_notoverridden <-
  filter(logbooks_join_overr, OVERRIDDEN == 0) #data frame of logbooks that weren't overridden

# stats
my_stats(logbooks_notoverridden)
# rows: 317002
# columns: 169
# Unique vessels: 1869
# Unique trips (logbooks): 91688

logbooks_NA <-
  filter(logbooks_join_overr, is.na(OVERRIDDEN)) #logbooks with an Overridden value of NA, because they were
# 1) submitted by a vessel that is missing from the Compliance report and therefore has no associated override data, or
# 2) submitted by a vessel during a period in which the permit was inactive, and the report was not required

# stats
my_stats(logbooks_NA)
# rows: 680
# columns: 169
# Unique vessels: 18
# Unique trips (logbooks): 142

## Add vessels missing from the Compliance report ----
# SEFHIER vessels missing from the Compliance report

# Finds vessels in the permit data that are missing from the compliance data
# So if a vessel is in the Metrics tracking report, but not in the compliance report we want to add them back.

vessels_missing <-
  setdiff(
    SEFHIER_permit_info_short_this_year$VESSEL_OFFICIAL_NUMBER,
    compl_override_data_this_year$VESSEL_OFFICIAL_NUMBER
  )

# stats
my_tee(n_distinct(vessels_missing),
       "vessels_missing")
# vessels_missing 8

# SEFHIER logbooks from vessels missing from the Compliance report
vessels_missing_logbooks <-
  logbooks_NA |>
  filter(VESSEL_OFFICIAL_NUMBER %in% vessels_missing)

# add missing logbooks back to the not overridden data frame
logbooks_notoverridden <-
  rbind(logbooks_notoverridden,
        vessels_missing_logbooks) |>
  distinct()

my_stats(logbooks_notoverridden)
# rows: 317390
# columns: 169
# Unique vessels: 1870
# Unique trips (logbooks): 91733

# remove missing logbooks from NA dataset, the NA dataset is now only those that were submitted when not needed

my_stats(logbooks_NA)
# Unique vessels: 18
# Unique trips (logbooks): 142

# Subset the logbooks_NA dataframe by excluding rows with VESSEL_OFFICIAL_NUMBER
# present in the vessels_missing vector.
logbooks_NA__rm_missing_vsls <- logbooks_NA |>
  filter(!VESSEL_OFFICIAL_NUMBER %in% vessels_missing)

my_stats(logbooks_NA__rm_missing_vsls,
         "logbooks_NA after removing missing logbooks")
# Unique vessels: 17
# Unique trips (logbooks): 97

# We have decided to throw out logbooks that were submitted when the permit was inactive, the logic
# being we shouldn't include logbooks that weren't required in the first place. Alternatively,
# deciding to keep in the NAs means we would be keeping reports that were submitted by a vessel
# during a period in which the permit was inactive, and the report was not required.
# rbind(logbooks_notoverridden, logbooks_NA) this is the alternative

# Use trip end date to calculate the usable date 30 days later

# Add a correct timezone to TRIP_END_DATE (EST vs. EDT)
logbooks_notoverridden <-
  logbooks_notoverridden |>
  mutate(TRIP_END_DATE_E =
           ymd_hms(TRIP_END_DATE,
                   truncated = 3,
                   tz = Sys.timezone()))

# add a date 30 days later with a time
logbooks_notoverridden <-
  logbooks_notoverridden |>
  mutate(USABLE_DATE_TIME =
           TRIP_END_DATE_E +
           days(30) +
           hours(23) +
           minutes(59) +
           seconds(59))

# format the submission date (TRIP_DE)
logbooks_notoverridden <-
  logbooks_notoverridden |>
  mutate(TRIP_DE =
           as.POSIXct(TRIP_DE, format = "%Y-%m-%d %H:%M:%S"))

# Drop empty columns
logbooks_notoverridden <-
  logbooks_notoverridden |>
  select(where(not_all_na))

# diffdf::diffdf(logbooks_notoverridden,
#                logbooks_notoverridden1)
# 26 columns dropped, bc they were all NAs

### stats ----
uniq_vessels_num_was <-
  n_distinct(Logbooks[["VESSEL_OFFICIAL_NUMBER"]])
uniq_vessels_num_now <-
  n_distinct(logbooks_notoverridden[["VESSEL_OFFICIAL_NUMBER"]])

uniq_trips_num_was <- n_distinct(Logbooks[["TRIP_ID"]])
uniq_trips_num_now <-
  n_distinct(logbooks_notoverridden[["TRIP_ID"]])

uniq_vessels_lost_by_overr <-
  uniq_vessels_num_was - uniq_vessels_num_now
# 12

uniq_trips_lost_by_overr <-
  uniq_trips_num_was - uniq_trips_num_now
# 2981

my_tee(uniq_vessels_lost_by_overr,
       "Thrown away vessels by overridden weeks")

my_tee(uniq_trips_lost_by_overr,
       "Thrown away trips by overridden weeks")

# Filtering logbook data ----
# Use logbooks_notoverridden from the previous section

## Filter out vessels not in Metrics tracking ----
SEFHIER_logbooks_notoverridden <-
  left_join(SEFHIER_permit_info_short_this_year,
            logbooks_notoverridden,
            join_by(VESSEL_OFFICIAL_NUMBER),
            suffix = c("_metrics", "_logbooks"))

glimpse(SEFHIER_logbooks_notoverridden)

### Save vessels with no logbooks ----
vessels_with_zero_logbooks <-
  SEFHIER_logbooks_notoverridden |>
  filter(is.na(TRIP_ID)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

vessels_with_zero_logbooks_file_path <-
  file.path(Path,
            Outputs,
            str_glue("vessels_with_zero_logbooks_{my_year}.rds"))

write_rds(vessels_with_zero_logbooks,
          vessels_with_zero_logbooks_file_path)

## stats ----
# We have to keep both vessel names, bc they are different some times in metrics vs. logbooks.
SEFHIER_logbooks_notoverridden |>
  select(starts_with("vessel")) |>
  distinct() |>
  filter(!VESSEL_NAME_metrics == VESSEL_NAME_logbooks) |>
  nrow()
# 48

my_stats(logbooks_notoverridden)
my_stats(SEFHIER_logbooks_notoverridden)

vessels_not_in_metrics <-
  n_distinct(logbooks_notoverridden$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_logbooks_notoverridden$VESSEL_OFFICIAL_NUMBER)

my_tee(vessels_not_in_metrics,
       "Removed if a vessel is not in Metrics tracking")

## Start date/time is after end date/time ----
# check logbook records for cases where start date/time is after end date/time, delete these records

# the Time Stamp Error is true if start date/time is greater than or equal to end date/time, false if not
SEFHIER_logbooks_notoverridden['time_stamp_error'] <-
  ifelse(
    SEFHIER_logbooks_notoverridden$STARTDATETIME >= SEFHIER_logbooks_notoverridden$ENDDATETIME,
    TRUE,
    FALSE
  )

### Filter: only keep the rows where there is no error between start & end date & time ----
SEFHIER_logbooks_notoverridden__start_end_ok <-
  SEFHIER_logbooks_notoverridden |>
  filter(time_stamp_error == FALSE)

# stats
my_stats(SEFHIER_logbooks_notoverridden__start_end_ok)
# rows: 310832
# columns: 153
# Unique vessels: 1825
# Unique trips (logbooks): 89797

# stats
thrown_by_time_stamp_error <-
  SEFHIER_logbooks_notoverridden |>
  filter(time_stamp_error == TRUE)

my_tee(n_distinct(thrown_by_time_stamp_error$TRIP_ID),
       "Thrown away by time_stamp_error (logbooks num)")
# 550

## Delete logbooks for trips lasting more than 10 days ----

# The assumption is there is an error in either start or end date and time and the trip didn't really last that long.

SEFHIER_logbooks_notoverridden__start_end_ok['trip_length'] <-
  as.numeric(
    difftime(
      SEFHIER_logbooks_notoverridden__start_end_ok$ENDDATETIME,
      SEFHIER_logbooks_notoverridden__start_end_ok$STARTDATETIME,
      units = "hours"
    )
  )

### Filter: only keep trips with a length less than or equal to 10 days (240 hours) ----

SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok <-
  SEFHIER_logbooks_notoverridden__start_end_ok |>
  filter(trip_length <= 240)

# stats
my_stats(SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok)
# rows: 310741
# columns: 154
# Unique vessels: 1822
# Unique trips (logbooks): 89762

# Output trips with length > 240 into a data frame (for stats)
logbooks_too_long <-
  SEFHIER_logbooks_notoverridden__start_end_ok |>
  filter(trip_length > 240)

my_tee(n_distinct(logbooks_too_long$TRIP_ID),
       "Thrown away by trip_more_10_days (logbooks num)")
# trip_ids: 35

my_tee(n_distinct(logbooks_too_long$VESSEL_ID),
       "Thrown away by trip_more_10_days (vessels num)")
# 30

## Mark all trips that were received > 30 days after the trip end date, by using compliance data and time of submission ----

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

    my_tee(n_distinct(late_submission$TRIP_ID),
           "Count late_submission (logbooks num)")
    # trip_ids: 16449

    my_tee(
      n_distinct(late_submission$VESSEL_OFFICIAL_NUMBER),
      "Count late_submission (vessels num)"
    )
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
  function() {
    SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok_temp <-
      SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok |>
      mutate(MORE_THAN_30_DAYS_LATE =
               ifelse(USABLE_DATE_TIME >= TRIP_DE, TRUE, FALSE))

    late_submission_filter_stats(SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok_temp)

    # late_submissions_flag = "_no_late_submissions"
    return(SEFHIER_logbooks_notoverridden__start_end_ok__trip_len_ok_temp)
  }

### Filter (mark only): data frame of logbooks that were usable ----
SEFHIER_logbooks_processed <- late_submission_filter()

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
