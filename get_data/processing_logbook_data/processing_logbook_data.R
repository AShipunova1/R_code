# processing_logbook_data

# Creates:
# 1) The result will be in
# SEFHIER_processed_Logbooks_{my_year}.rds

# Files to read in:
# 1) SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds
       # use processing_metrics_tracking.R to create this file or download it from Google Drive
       # before running this code
# 2) processing_auxiliary_methods.R
       # get from Google Drive R code folder, put in path directory with this code

# Files this code will pull from Oracle:
# 1) Raw_Oracle_Downloaded_compliance_2021_plus.rds
# 2) Raw_Oracle_Downloaded_logbook_{my_date_beg}__{my_date_end}.rds

# This code pulls in logbook data from Oracle database,
# then cleans it up, so that we can use it in any logbook data analysis:
# (1) (a) pull all logbook and compliance/override data from Oracle database
#     (b) get processed Metrics Tracking file
# (2) clean up logbook data set
#   (a) remove records from SRHS vessels
#   (b) remove records where start date/time is after end date/time
#   (c) remove records for trips lasting more than 10 days
# (3) mark all trips that were received > 30 days after trip end date, by using compliance data and time of submission
#   (a) mark all trips where the week was overridden, because the submission date is unknown, more notes further in the code
# (4) Add permit region information (GOM, SA, or dual), using permit names (optional)

# For 2022 we don't keep trips starting in 2021 and ending in 2022. We only keep trips starting in 2022, because 2021 was the first year of the program and we don’t think it’s very reliable data

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

# load required packages (install first if needed)
library(ROracle)
library(tidyverse)
# see the list of packages: tidyverse_packages()
library(tictoc) # Functions for timing
library(crayon) # Colored terminal output

# set working and output directory - where do you keep the data and analysis folder on your computer?
michelles_path <-
  "C:/Users/michelle.masi/Documents/SEFHIER/R code/Processed data/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessinglogbookData/"
# r"(C:\Users\jenny.ostroff\Desktop\Backups\Rcode\ProcessinglogbookData)"

# Input files are the same here
annas_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data/)"

# !! Change to your path !!
# Change to use another path instead:
# Path <- michelles_path
Path <- annas_path

#create these paths in your directory first
Inputs <- "Inputs"
Outputs <- "Outputs"

output_file_path <-
  file.path(Path, Outputs)

# dir.exists(output_file_path)

# Set the date ranges for the logbook and compliance data you are pulling
# this is the year to assign to the output file name
my_year <- "2022"
# my_year <- "2023"
# my_year <- "2024"

# TODO: find the fringe weeks

my_calendar_date_beg <- str_glue("01-JAN-{my_year}")
my_calendar_date_end <- str_glue("31-DEC-{my_year}")

# floor_date(x, 'weeks')
#[1] "2019-12-29"

# ceiling_date(x, 'weeks') - 1
#[1] "2020-01-04"

my_compliance_date_beg <-
  dmy(my_calendar_date_beg) |>
  floor_date('weeks')

wday(my_compliance_date_beg,
     label = TRUE,
     week_start = 1)

my_compliance_date_end <-
    dmy(my_calendar_date_end) |>
  ceiling_date('weeks') - 1

wday(my_compliance_date_end,
     label = TRUE,
     week_start = 1)


# years range for srfh_vessel_comp db download, see below
# this should at least run the year before my_year to the year after my_year
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

# Create a log of stats for the processing file, various parameters are tallied throughout the code, and saved here. ----

my_tee(date(),
       my_title = str_glue("Start logbook processing for {my_year}"))

# Get data ----

# This section is needed to pull data from Oracle database
# to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

# set up an Oracle connection
# Sys.getenv("ORA_SDTZ")

# You have to set up the same time zones for ROracle and RStudio. By default, they use different ones, and this difference causes dates and times to round up in R Studio, pushing some date timestamps to the next day, and making them incorrect.
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

# test if you are connected to VPN
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
# force_from_db = TRUE
compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query,
                        force_from_db = NULL
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
# compl_override_data__renamed
# rows: 460724
# columns: 23
# Unique vessels: 4390


# stats (check the min date in the data)
min(compl_override_data__renamed$COMP_WEEK_START_DT)
# [1] "2021-01-04 EST"

# keep only year of analysis, including the week 52 of the previous year if needed

# Explanations:
# 1. 'compl_override_data__renamed__this_year' is a new data frame created from 'compl_override_data__renamed'.
# 2. In this data frame, only rows meeting specific date criteria are retained.
# 3. The 'filter' function is used to subset rows based on the following conditions:
#    a. 'COMP_WEEK_START_DT' should be greater than or equal to 'my_date_beg' (start date).
#    b. 'COMP_WEEK_START_DT' should be less than or equal to 'my_date_end' (end date).
# 4. Dates are converted using 'as.Date' with the appropriate format ("%d-%b-%Y") and time zone.
# 5. 'Sys.timezone()' retrieves the current system's time zone.
# 6. The filtered data frame 'compl_override_data__renamed__this_year' contains only rows within the specified date range.
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
# [1] "2022-01-02 EST" # this might contain the last week in the year before my_year, to account for a compliance week that overlaps last week of the previous year and first week of my_year

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

#reads the file in the path into a data frame
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
    trip_end_date >= '{my_date_beg}'
  AND trip_start_date <= '{my_date_end}'
")

# Use 'read_rds_or_run_query' defined above to either read logbook information from an RDS file or execute a query to obtain it and write a file for future use.
# Change "force_from_db = NULL" to "force_from_db = TRUE" for force db downloading (must be on VPN)

Logbooks_raw <-
  read_rds_or_run_query(logbooks_file_path,
                        logbooks_download_query,
                        force_from_db = NULL)

# Rename column to be consistent with other dataframes
Logbooks_raw_renamed <-
  rename(Logbooks_raw,
         VESSEL_OFFICIAL_NUMBER =
           "VESSEL_OFFICIAL_NBR")

# stats
my_stats(Logbooks_raw_renamed, "Logbooks from the db")
# 2022
# rows: 327847
# columns: 149
# Unique vessels: 1885
# Unique trips (logbooks): 94737

### reformat trip start/end date ----
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
Logbooks_raw_renamed__to_date <-
  Logbooks_raw_renamed |>
  mutate(across(c(!where(is.Date) & ends_with("_DATE")),
                as.Date))

# (check) Was:
Logbooks_raw$TRIP_START_DATE |>
  head(1)
# "2022-07-07 01:00:00 EDT"

# (check time was removed from date column) Now:
Logbooks_raw_renamed__to_date$TRIP_START_DATE |>
  head(1)
# "2022-07-07"
#this also changes the format from POSIX to DATE

### reformat trip start/end time ----

# convert time columns to numeric,
# then format to 4 digits as a string (some time entries were like "800") —-
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

#create list of column headers, to be used in the next piece of code
time_col_names <-
  c("TRIP_START_TIME",
    "TRIP_END_TIME")

Logbooks_raw_renamed__to_date_time4 <-
  Logbooks_raw_renamed__to_date |>
  mutate(across(c(
    !where(is.numeric) & all_of(time_col_names)
  ),
  as.numeric)) |>
  mutate(across(all_of(time_col_names),
                ~ sprintf("%04d", .x)))

### Filter out just my analysis year logbook entries ----

# first check the min and max start date in the DF
min(Logbooks_raw_renamed__to_date_time4$TRIP_START_DATE)
# [1] "2022-01-01"
# [1] "2023-01-01"
max(Logbooks_raw_renamed__to_date_time4$TRIP_START_DATE)
# [1] "2022-12-31"
# [1] "2023-12-31"

# Now filter for just my analysis year
# this will keep all rows where trip start and/or end date falls in my_year
# so if all rows meet this criteria, it may not remove any rows, that is ok
Logbooks_raw_renamed__to_date_time4__my_year <-
  Logbooks_raw_renamed__to_date_time4 |>
  filter(
    TRIP_END_DATE >=
      as.Date(my_date_beg, "%d-%b-%Y",
              tz = Sys.timezone()) &
      TRIP_START_DATE <=
      as.Date(my_date_end, "%d-%b-%Y",
              tz = Sys.timezone())
  )

# stats, to compare with the end result —-
logbooks_stat_correct_dates_before_filtering <-
  c(dim(Logbooks_raw_renamed__to_date_time4__my_year),
    n_distinct(Logbooks_raw_renamed__to_date_time4__my_year$VESSEL_OFFICIAL_NUMBER),
    n_distinct(Logbooks_raw_renamed__to_date_time4__my_year$TRIP_ID)
  )
#[1] 327823    149   1885  94733

#check the number of logbooks after filtering by my analysis year
my_stats(Logbooks_raw_renamed__to_date_time4__my_year,
         "Logbooks after filtering by dates")
# Logbooks after filtering by dates
# rows: 327823
# columns: 149
# Unique vessels: 1885
# Unique trips: 94733


# check the min and max start dates, after filtering DF to just my analysis year
min(Logbooks_raw_renamed__to_date_time4__my_year$TRIP_START_DATE)
# [1] "2022-01-01"
max(Logbooks_raw_renamed__to_date_time4__my_year$TRIP_START_DATE)
# [1] "2022-12-31"

# check the min and max end dates, after filtering DF to just my analysis year
min(Logbooks_raw_renamed__to_date_time4__my_year$TRIP_END_DATE)
# [1] "2018-06-04"
max(Logbooks_raw_renamed__to_date_time4__my_year$TRIP_END_DATE)
# [1] "2023-05-26"

# create column for start and end date & time —--
# Used in "the Time Stamp Error" and "the trip is too long".

# Explanations:
# 1. 'Logbooks_raw_renamed__to_date_time4__my_year__format_time' is a modified version of 'Logbooks_raw_renamed__to_date_time4__my_year'.
# 2. Two new columns, 'STARTDATETIME' and 'ENDDATETIME', are added to this data frame.
# 3. The values for these columns are obtained by combining 'TRIP_START_DATE' with 'TRIP_START_TIME' for 'STARTDATETIME'
#    and 'TRIP_END_DATE' with 'TRIP_END_TIME' for 'ENDDATETIME'.
# 4. 'paste' function is used to concatenate date and time strings.
# 5. The resulting combined strings are then converted to POSIXct objects using 'as.POSIXct'.
# 6. 'format' parameter specifies the format of the input strings ("%Y-%m-%d %H%M").
# 7. 'tz' parameter specifies the time zone to be used for the conversion, obtained from 'Sys.timezone()'.
# 8. This ensures that the date and time values are correctly parsed and stored as datetime objects in the specified time zone.

Logbooks_raw_renamed__to_date_time4__my_year__format_time <-
  Logbooks_raw_renamed__to_date_time4__my_year |>
  mutate(STARTDATETIME =
           as.POSIXct(paste(TRIP_START_DATE,                                           TRIP_START_TIME),
                      format = "%Y-%m-%d %H%M",
                      tz = Sys.timezone())) |>
  mutate(ENDDATETIME =
           as.POSIXct(paste(TRIP_END_DATE,                                           TRIP_END_TIME),
                      format = "%Y-%m-%d %H%M",
                      tz = Sys.timezone()))

# check date and time values are correctly parsed and stored as datetime objects in the specified time zone
Logbooks_raw_renamed__to_date_time4__my_year__format_time$ENDDATETIME |>
  head(1)
# "2022-07-07 08:00:00 EDT"

### Check if all vessels have ids ----
Logbooks_raw_renamed__to_date_time4__my_year__format_time |>
  filter(is.na(VESSEL_OFFICIAL_NUMBER)) |>
  nrow()
# 0 OK, all vessel have a VESSEL_OFFICIAL_NUMBER

### Prepare data to determine what weeks were overridden, so we can mark logbooks from those weeks later ----

# not needed for processing, just checking that our compliance weeks are correct
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
# Explanations:
# 1. 'Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso' is an extension of 'Logbooks_raw_renamed__to_date_time4__my_year__format_time'.
# 2. Two new columns, 'TRIP_END_WEEK' and 'TRIP_END_YEAR', are added to this data frame.
# 3. 'isoweek' function is applied to the 'TRIP_END_DATE' column to extract the ISO week number of the trip end date.
# 4. This ISO week number represents the week of the year in which the trip ends.
# 5. Similarly, 'isoyear' function is applied to the 'TRIP_END_DATE' column to extract the ISO year of the trip end date.
# 6. This ISO year represents the year to which the trip end date belongs.
# 7. These columns provide additional temporal information about the trips, such as the week number and year according to the ISO 8601 standard.

Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso <-
  Logbooks_raw_renamed__to_date_time4__my_year__format_time |>
  mutate(
    TRIP_END_WEEK = isoweek(TRIP_END_DATE),
    TRIP_END_YEAR = isoyear(TRIP_END_DATE)
  )

# Adding flags for filtering the logbook data ----

## Filter out vessels not in Metrics tracking from compliance info ----
SEFHIER_compl_override_data__renamed__this_year <-
  compl_override_data__renamed__this_year |>
  filter(VESSEL_OFFICIAL_NUMBER %in% processed_metrics_tracking$VESSEL_OFFICIAL_NUMBER)

# Check the number of records (rows), vessels and trips before and after filtering out vessels not in Metrics Tracking
my_stats(compl_override_data__renamed__this_year)
my_stats(SEFHIER_compl_override_data__renamed__this_year)

# Total number of vessels in Oracle raw data but not in Metrics Tracking
vessels_not_in_metrics <-
  n_distinct(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER)

#check if there are any vessels not in metrics tracking
vessels_not_in_metrics #[1] 244

# Total number of vessels in Oracle raw data but not in Metrics Tracking
my_tee(vessels_not_in_metrics,
       "Vessels removed if a vessel is not in Metrics tracking")
#244 (2022)


## add compliance/override data to logbooks ----
# We add data from the compliance module to the DNF data frame to associate weeks where compliance was overridden with the corresponding DNFs.
# Depending on the analysis question, we may want to remove DNFs for weeks that
# were overridden because we don't have a timestamp for when the DNF was submitted to
#the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate between
#turning a DNF in on time- in the app, and it then taking two months to get it into FHIER vs
#turning in a DNF two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and
#SAFIS) in March, 2022 (at permit renewal)... user submitted on time in app (VESL) but we
#may not get that report in SAFIS for months later (when it’s found as a "missing report" and
#then requeued for transmission)
my_stats(SEFHIER_compl_override_data__renamed__this_year,
         "Compliance and override data from the db")
# Compliance and override data from the db
# rows: 141557
# columns: 23
# Unique vessels: 3382


### join the data frames ----

#### find field names ----
grep("year", names(Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso), ignore.case = T, value = T)
#[1] "TRIP_END_YEAR"
grep("week", names(Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso), ignore.case = T, value = T)
#[1] "TRIP_END_WEEK"

# join logbooks and compliance
logbooks_join_overr <-
  full_join(
    Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso,
    SEFHIER_compl_override_data__renamed__this_year,
    join_by(TRIP_END_YEAR == COMP_YEAR,
            VESSEL_OFFICIAL_NUMBER,
            TRIP_END_WEEK == COMP_WEEK),
    relationship = "many-to-many"
  )
# We need the “many-to-many” relationship. There will be many logbooks that match a compliance week, because a logbook is submitted for any day in the week.
# to see the many-to-many relationship see find_duplicates_in_compl.R

# check the difference
#This list of vessels is not concerning, it’s just an extra step in providing a thorough analysis. It's possible that a vessel submitted a DNF for every week they were permitted, so that they remained compliant but never submitted a logbook.
in_compl_not_in_logbooks <-
  logbooks_join_overr |>
  filter(is.na(TRIP_ID)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

# check the number of rows in DF
nrow(in_compl_not_in_logbooks)

##This list of vessels is not concerning, it’s just an extra step in providing a thorough analysis.
#This tells us that these vessels submitted a logbook for a week when they were not permitted. It can happen when a captain is unaware that his permit has expired, but reports anyway.
in_logbooks_not_in_compl <-
  logbooks_join_overr |>
  filter(is.na(IS_COMP)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

# check the number of rows in DF
nrow(in_logbooks_not_in_compl)


### Remove rows with NA logbooks and entries in Compliance ----
# this just means that no logbooks were submitted for that compliance week, i.e. the vessel submitted a DNF instead

logbooks_join_overr__all_logbooks <-
  logbooks_join_overr |>
  filter(!is.na(TRIP_ID))

#check the dimensions of the DFs before and after removing rows with NA logbooks dim(logbooks_join_overr)
dim(logbooks_join_overr__all_logbooks)

# Total # of records, vessels and trips, before and after joining logbook and compliance data
my_stats(Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso)

my_stats(logbooks_join_overr_all_logbooks)
# logbooks_join_overr_all_logbooks
# rows: 435911
# columns: 173
# Unique vessels: 3426
# Unique trips: 94734


### Add a compliant_after_override column ----
#This is needed so that we can easily filter out compliant or non-compliant vessels in the dataset, by adding an extra column that states yes or no regarding compliance. The NA in IS_COMP represents one of two possible scenarios: 1) a logbook was submitted for a vessel that is missing from the compliance module but is in metrics tracking, or 2) a logbook was submitted for a week when the vessel was not permitted. It is not simple to determine which. Deciding what to do with these DNFs will depend on the individual analysis question, and so is not addressed here, but simply left as NA.


# Explanations:
# 1. 'logbooks_join_overr__compl' is created as an extension of 'logbooks_join_overr__all_logbooks'.
# 2. This operation is performed row-wise.
# 3. A new column 'compliant_after_override' is added to the data frame.
# 4. Within the 'compliant_after_override' column, values are assigned based on conditions using the 'case_when' function.
# 5. If 'IS_COMP' (compliance indicator) is 0 and 'OVERRIDDEN' (override indicator) is 0, the value is set to "no".
# 6. If 'IS_COMP' is 1, indicating compliance, the value is set to "yes".
# 7. If 'OVERRIDDEN' is 1, indicating that compliance was overridden, the value is set to "yes".
# 8. If 'IS_COMP' is NA, indicating missing data, the value is set to NA.
# 9. For all other cases, the value is set to a string representation of 'IS_COMP'.
# 10. Finally, the data frame is ungrouped to revert to its original structure.

# This shows the different scenarios we should account for in the case_when statement below. If the results differ from what is shown in the comments here, we will need to adapt the code.
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

## NOTE: IF “Is_Overriden == 1 & is_Comp == 0, then the vessel should be considered compliant in any compliance analyses


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


# check the number of distinct trips that are compliant and overridden
logbooks_join_overr |>
  filter(IS_COMP == 1 & OVERRIDDEN == 1) |>
  select(TRIP_ID) |>
  distinct() |>
  dim()
# 401

#check the distinct values in new DF for ‘compliant after override’, ‘is_comp’ and ‘overridden’ columns
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
# logbooks_join_overr__compl__start_end_ok
# rows: 325649
# columns: 175
# Unique vessels: 1882
# Unique trips: 94060

# create a tibble of all trips with time_stamp_Error
thrown_by_time_stamp_error <-
  logbooks_join_overr__compl |>
  filter(time_stamp_error == TRUE)

# check the number of logbooks thrown out due to time_stamp_error
my_tee(n_distinct(thrown_by_time_stamp_error$TRIP_ID),
       "Thrown away by time_stamp_error (logbooks num)")

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

trip_length_threshold <- 240

#filter out logbooks that exceed the trip length threshold, as defined above
logbooks_join_overr__compl__start_end_ok__trip_len_ok <-
  logbooks_join_overr__compl__start_end_ok |>
  filter(trip_length < trip_length_threshold)

# Number of vessels and unique trips, after removing trips that exceed the length threshold
my_stats(logbooks_join_overr__compl__start_end_ok__trip_len_ok)

# Output trips with length > trip_length_threshold (240) into a data frame (for stats)
logbooks_too_long <-
  logbooks_join_overr__compl__start_end_ok |>
  filter(trip_length > trip_length_threshold)

#number of trips thrown out because they exceed 10 days
my_tee(n_distinct(logbooks_too_long$TRIP_ID),
       "Thrown away by trip_more_10_days (logbooks num)")
#number of vessels impacted by tossing logbooks that exceed 10 day threshold
my_tee(n_distinct(logbooks_too_long$VESSEL_ID),
       "Thrown away by trip_more_10_days (vessels num)")

## Mark all trips that were received > 30 days after the trip end date, by using compliance data and time of submission ----

### add the threshold date ----
# Add a date 30 days after the trip and set a time to the last minute of that day

# Explanations:
# 1. 'logbooks_join_overr_e_usable_date' is created based on the 'logbooks_join_overr__compl__start_end_ok' data frame.
# 2. The 'mutate' function is used to add a new column named 'USABLE_DATE_TIME'.
# 3. 'USABLE_DATE_TIME' is calculated as 30 days after 'TRIP_END_DATE' using the 'days' function from lubridate.
# 4. Subsequent 'mutate' calls adjust the 'USABLE_DATE_TIME' to have the time set to 23:59:59 on the same date.

logbooks_join_overr_e_usable_date <-
  logbooks_join_overr__compl__start_end_ok__trip_len_ok |>
  mutate(USABLE_DATE_TIME =
           TRIP_END_DATE + days(30)) |>
  mutate(USABLE_DATE_TIME =
           `hour<-`(USABLE_DATE_TIME, 23)) |>
  mutate(USABLE_DATE_TIME =
           `minute<-`(USABLE_DATE_TIME, 59)) |>
  mutate(USABLE_DATE_TIME =
           `second<-`(USABLE_DATE_TIME, 59))

# subtract the usable date from the date of submission —
# value is true if the logbook was submitted within 30 days, false if the logbook was not

# Print out statistics

# # create a function to produce some stats for the next analysis step, assessing late submission —-

# 1. 'late_submission_filter_stats' is a function that takes a dataframe 'my_df' as input.
# 2. It first calls another function 'my_stats' to compute and print statistics about 'my_df'.
# 3. It filters 'my_df' to create a subset called 'late_submission' where 'MORE_THAN_30_DAYS_LATE' is FALSE.
# 4. It then calculates and prints the number of distinct trip IDs and vessel official numbers in the 'late_submission' subset.
# 5. Lastly, it checks and prints the minimum and maximum values of 'TRIP_START_DATE' and 'TRIP_END_DATE' in 'my_df'.

late_submission_filter_stats <-
  function(my_df) {
    my_stats(my_df)

    late_submission <-
      my_df |>
      filter(MORE_THAN_30_DAYS_LATE == FALSE)

    my_tee(n_distinct(late_submission$TRIP_ID),
    "Count late_submission (logbooks num)")

    my_tee(
    n_distinct(late_submission$VESSEL_OFFICIAL_NUMBER),
    "Count late_submission (vessels num)"
    )

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

### Add a column with late submission ----
# Function Explanations:
# 1. 'late_submission_filter' is a function designed to filter late submissions from a given data frame.
# 2. The function takes 'my_df' as input, which represents the data frame to be processed.
# 3. Inside the function, a new data frame 'logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp' is created by modifying 'my_df'.
# 4. Within 'logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp', a new column 'MORE_THAN_30_DAYS_LATE' is created based on a conditional statement.
# 5. The conditional statement checks if the 'USABLE_DATE_TIME' column is greater than or equal to the 'TRIP_DE' column. If true, it assigns TRUE to 'MORE_THAN_30_DAYS_LATE', otherwise FALSE.
# 6. The 'late_submission_filter_stats' function is called to generate statistics on the filtered data frame. This function is provided above.
# 7. Finally, the modified data frame 'logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp' is returned from the function.

late_submission_filter <-
  function(my_df) {
    logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp <-
      my_df |>
      mutate(IS_MORE_THAN_30_DAYS_LATE =
               case_when(TRIP_DE >= USABLE_DATE_TIME ~ TRUE,
                         .default = FALSE))

    late_submission_filter_stats(logbooks_join_overr__compl__start_end_ok__trip_len_ok_temp)

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

# All stats ----
my_stats(SEFHIER_logbooks_processed)
# SEFHIER_logbooks_processed
# rows: 325649
# columns: 178
# Unique vessels: 1882
# Unique trips: 94060

#number of logbooks from Oracle, before any processing done
logbooks_before_filtering <-
  n_distinct(Logbooks_raw$TRIP_ID)  #94737 (2022)
#call out to console the # of logbooks before filtering
my_tee(logbooks_before_filtering,
        "Logbooks before filtering")

#number of logbooks from Oracle, after processing is done
logbooks_after_filtering <-
  n_distinct(SEFHIER_logbooks_processed$TRIP_ID)  #94060 (2022)
#call out to console the # of logbooks before filtering
my_tee(logbooks_after_filtering,
        "Logbooks after filtering")
#total % of logbooks removed after processing
percent_of_removed_logbooks <-
  (logbooks_before_filtering - logbooks_after_filtering) * 100 / logbooks_before_filtering
#call out to console the % of logbooks before filtering
cat(percent_of_removed_logbooks, sep = "\n")
# 5.400213 2022
# 0.7146099 if keep overridden

# number removed_vessels, after processing the logbook data from Oracle
vessels_before_filtering <-
  n_distinct(Logbooks_raw_renamed$VESSEL_OFFICIAL_NUMBER)
#call out to console the # of vessels removed
cat(vessels_before_filtering)

#number of vessels in the processed logbook file
vessels_after_filtering <-
  n_distinct(Logbooks_raw_renamed__to_date_time4__my_year__format_time__iso$VESSEL_OFFICIAL_NUMBER)
#call out the # of vessels to the console
cat(vessels_after_filtering)


#number of vessels that were removed in the processing steps of this code
removed_vessels <-
  vessels_before_filtering - vessels_after_filtering

#percent of vessels that were removed in the processing steps of this code
percent_of_removed_vessels <-
  (vessels_before_filtering - vessels_after_filtering) * 100 / vessels_before_filtering

#establish text color and format to call to console
removed_logbooks_and_vessels_text <- c(
  crayon::blue("percent_of_removed_logbooks"),
  str_glue("{round(percent_of_removed_logbooks)}%"),
  crayon::blue("removed_vessels"),
  removed_vessels,
  crayon::blue("percent_of_removed_vessels"),
  str_glue("{round(percent_of_removed_vessels)}%")
)

#call to the consolve the text established above
my_tee(removed_logbooks_and_vessels_text,
       "\nRemoved logbooks and vessels stats")
# Removed logbooks and vessels stats
# percent_of_removed_logbooks
# 1%
# removed_vessels
# 0
# percent_of_removed_vessels
# 0%
my_tee(removed_logbooks_and_vessels_text,
       "\nRemoved logbooks and vessels stats")

# Export processed logbooks ----

my_calendar_date_beg <- str_glue("01-JAN-{my_year}")
my_calendar_date_end <- str_glue("31-DEC-{my_year}")

# TODO: create 2 different dfs by calendar and compliance date separately

SEFHIER_processed_Logbooks_file_name <-
  str_glue("SEFHIER_processed_Logbooks_{my_year}.rds")

write_rds(
  SEFHIER_logbooks_processed,
  file = file.path(output_file_path, SEFHIER_processed_Logbooks_file_name)
)


