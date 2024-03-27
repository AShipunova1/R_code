# processing_DNF_data

# The result will be in
# SEFHIER_processed_dnfs_{my_year}.rds

# For this analysis, a week starts on Monday and ends on Sunday.

# Files to read or create:
# 1) Raw_Oracle_Downloaded_compliance_2021_plus.rds
# 2) Raw_Oracle_Downloaded_dnf_{my_date_beg}__{my_date_end}.rds
# 3) SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds
       # use processing_metrics_tracking.R to create file #3 before running this script
#4) processing_auxiliary_methods.R
       # get from Google Drive R code folder (R code / Processed data, https://docs.google.com/document/d/1U5cYuEGITcHrwDLgkHtyMvoUsS7nZeONyHt2yelpbNA/edit?usp=drive_link), put in path directory with this script

# This code processes DNF data from Oracle database ready for FHIER,
# then cleans it up, so that we can use it in any DNF data analysis:
# (1) (a) pull all DNF and compliance/override data from Oracle database
#     (b) get Metrics Tracking from FHIER
# (2) clean up DNF data set
#     (a) remove records from SRHS vessels
# (3) remove all overridden data.
# We have to remove dnfs for weeks that were overridden because we don't have a timestamp for when the dnf was submitted to the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate that time laps.
# We can't differentiate between turning a dnf in on time in the app, and it taking two months to get it vs turning in a dnf two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and SAFIS) in March, 2022 (At permit renewal)... user submitted on time in app (VESL) but we may not get that report in SAFIS for months later (when its found as a "missing report" and then requeued for transmission)
# (4) mark all trips neg that were received > 30 days after trip end date, by using time of submission

# For 2022 we don't keep trips neg starting in 2021 and ending in 2022. We only keep trips neg starting in 2022.

# Caveats:
# 1) The way COMP_WEEK is calculated could get messed up depending on a given year time frame. It's due to something
# internal called the ISO number and how the function calculates the start of a week. If you are running this on a
# new data set, check your weeks to make sure it's calculating correctly.

# Running the code
# To run the file as a whole, you can type this in the console: source('processing_dnf_data.R') and hit enter.
# Pressing F2 when the custom function name is under the cursor will show the function definition.
# Pressing F1 when the R function name is under the cursor will show the function definition and examples in the help panel.

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

Inputs <- "Inputs/"
Outputs <- "Outputs/"

output_file_path <-
  Path

# Set the date ranges for the DNF and compliance data you are pulling
# this is the year to assign to the output file name
# my_year <- "2022"
my_year <- "2023"
# my_year <- "2024"

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
# try_connection: 21.7 sec elapsed if no connection (i.e. not on VPN)
# try_connection: 1.63 sec elapsed if works normally

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

compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query)

# check a week start day
compl_override_data |>
  filter(COMP_YEAR == '2023' &
         COMP_WEEK == '50') |>
  select(COMP_WEEK_START_DT) |>
  distinct() |> str()
# $ COMP_WEEK_START_DT: POSIXct, format: "2023-12-11".
# Monday - correct (https://calendar.online/calendar-weeks/2023/50)

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
  filter(COMP_WEEK_END_DT >= as.Date(my_date_beg, "%d-%b-%Y",
                                     tz = Sys.timezone()) &
           COMP_WEEK_START_DT <= as.Date(my_date_end, "%d-%b-%Y",
                                         tz = Sys.timezone()))

# check
# That's the week 52 of (my_year - 1):
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

## Import and prep the dnf data ----

# Import the dnf data from file or database
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) create a variable with file path to read or write the dnf file

dnfs_file_path <-
  file.path(Path,
            Outputs,
            str_glue("Raw_Oracle_Downloaded_dnf_{my_date_beg}__{my_date_end}.rds"))
# Was "SAFIS_TripsDownload_"

# 2) create a variable with an SQL query to call data from the database

# stringr::str_glue:
# Interpolation with glue to include variable names

# Need to join safis.trips_neg to safis.vessels because the vessel_ID in safis.trips_neg is not the official number

dnfs_download_query <-
  str_glue(
    "SELECT
  trip_id,
  trip_date,
  tn.vessel_id vessel_id,
  tn.de,
  tn.ue,
  coast_guard_nbr,
  state_reg_nbr,
  sero_official_number vessel_official_number
FROM
       safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tn
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  ON ( tn.vessel_id = v.vessel_id )
WHERE
    trip_date BETWEEN TO_DATE('{my_date_beg}', 'dd-mon-yy') AND
TO_DATE('{my_date_end}', 'dd-mon-yy')
"
  )

# Use 'read_rds_or_run_query' defined above to either read dnf information from an RDS file or execute a query to obtain it and write a file for future use.
dnfs <-
  read_rds_or_run_query(dnfs_file_path,
                        dnfs_download_query)
# from scratch
# 2024-03-25 run for Raw_Oracle_Downloaded_dnf_01-JAN-2022__31-DEC-2022.rds: 120.43 sec elapsed
# 2024-03-25 run for Raw_Oracle_Downloaded_dnf_01-JAN-2023__31-DEC-2023.rds: 125.17 sec elapsed

### add COAST_GUARD_NBR or STATE_REG_NBR if no VESSEL_OFFICIAL_NUMBER ----
# Explanations:
# 1. Use 'mutate' to create or modify a column named 'VESSEL_OFFICIAL_NUMBER'.
# 2. Use 'case_when' to conditionally assign values to the column based on conditions.
# 3. Check if 'VESSEL_OFFICIAL_NUMBER' is NA (missing).
#    - If true, use 'coalesce' to select the first non-missing value among 'COAST_GUARD_NBR' and 'STATE_REG_NBR'.
#    - If 'VESSEL_OFFICIAL_NUMBER' is not missing, keep its original value.
# 4. The resulting DataFrame will have the 'VESSEL_OFFICIAL_NUMBER' column filled with non-missing values from 'COAST_GUARD_NBR' or 'STATE_REG_NBR'.

dnfs_na_vessel <-
  dnfs |>
  filter(is.na(VESSEL_OFFICIAL_NUMBER)) |>
  select(COAST_GUARD_NBR, STATE_REG_NBR) |>
  distinct()
# dim()

dnfs_na_vessel |>
  left_join(processed_metrics_tracking,
            join_by(COAST_GUARD_NBR == VESSEL_OFFICIAL_NUMBER)) |>
  filter(!is.na(PERMITS)) |> dim()
# 0

dnfs_na_vessel |>
  left_join(processed_metrics_tracking,
            join_by(STATE_REG_NBR == VESSEL_OFFICIAL_NUMBER)) |>
  filter(!is.na(PERMITS)) |> glimpse()
# 2

dnfs_v_all_ids <-
  dnfs |>
  mutate(VESSEL_OFFICIAL_NUMBER =
           case_when(
             is.na(VESSEL_OFFICIAL_NUMBER) ~
               coalesce(COAST_GUARD_NBR, STATE_REG_NBR),
             .default = VESSEL_OFFICIAL_NUMBER
           ))

# check if all vessels have ids now
# dnfs_v_all_ids |>
#   filter(is.na(VESSEL_OFFICIAL_NUMBER)) |>
#   distinct() |>
#   nrow()
# 0 - OK

### Fewer columns ----
dnfs_short <-
  dnfs_v_all_ids |>
  select(-c(STATE_REG_NBR, COAST_GUARD_NBR))

# stats
my_stats(dnfs_short, "dnfs from the db")
# 2022
# rows: 796804
# columns: 6
# Unique vessels: 3576
# Unique trips: 796804

# 2023 with ue
# rows: 948977
# columns: 6
# Unique vessels: 3860
# Unique trips: 948977

### Prepare data to determine what weeks were overridden, so we can exclude dnfs from those weeks later ----

# assign each dnf a week designation (first day of the reporting week is a Monday)
# use the end date to calculate this, it won't matter for most trips, but for some trips neg that
# happen overnight on a Sunday, it might affect what week they are assigned to
#https://stackoverflow.com/questions/60475358/convert-daily-data-into-weekly-data-in-r

# Calculate the ISO week number for each date in the 'TRIP_DATE2' column.
# lubridate package has following methods:
# week() returns the number of complete seven day periods that have occurred between the date and January 1st, plus one.
#
# isoweek() returns the week as it would appear in the ISO 8601 system, which uses a reoccurring leap week.
#
# epiweek() is the US CDC version of epidemiological week. It follows same rules as isoweek() but starts on Sunday. In other parts of the world the convention is to start epidemiological weeks on Monday, which is the same as isoweek.

# Needed to adjust for week 52 of the previous year
dnfs_short_date__iso <-
  dnfs_short |>
  mutate(TRIP_DATE_WEEK = isoweek(TRIP_DATE), # puts it in week num
         TRIP_DATE_YEAR = isoyear(TRIP_DATE)) # adds a year

# to see the respective data in compl_override_data__renamed__this_year, note the last week of 2021
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

# Adding flags to the dnf data ----

## Filter out vessels not in Metrics tracking ----
SEFHIER_dnfs_short_date__iso <-
  dnfs_short_date__iso |>
  filter(VESSEL_OFFICIAL_NUMBER %in% processed_metrics_tracking$VESSEL_OFFICIAL_NUMBER)

my_stats(dnfs_short_date__iso)
my_stats(SEFHIER_dnfs_short_date__iso)

vessels_not_in_metrics <-
  n_distinct(dnfs_short_date__iso$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_dnfs_short_date__iso$VESSEL_OFFICIAL_NUMBER)

my_tee(vessels_not_in_metrics,
       "Vessels removed if a vessel is not in Metrics tracking")
# 1556 (2022)

dnfs_not_in_metrics <-
  n_distinct(dnfs_short_date__iso$TRIP_ID) -
  n_distinct(SEFHIER_dnfs_short_date__iso$TRIP_ID)

my_tee(dnfs_not_in_metrics,
       "DNFs removed if a vessel is not in Metrics tracking")
# 356497 (2022)
# 446859 (2023)

## add override data to dnfs ----
my_stats(compl_override_data__renamed__this_year,
         "Compliance and override data from the db")
# 2022
# rows: 150029
# columns: 23
# Unique vessels: 3626

### check if dnfs and compliance data have the same week dates ----
trip_date_1 <-
  SEFHIER_dnfs_short_date__iso |>
  filter(TRIP_DATE_WEEK == 1,
         TRIP_DATE_YEAR == my_year) |>
  select(TRIP_DATE)

dnfs_first_week_my_year <-
  tibble(min1 = as.Date(min(trip_date_1$TRIP_DATE),
                        tz = Sys.timezone()),
    # [1] "2022-01-03 23:00:00 EST"
    max1 = as.Date(max(trip_date_1$TRIP_DATE),
                   tz = Sys.timezone())
    # [1] "2022-01-09 23:00:00 EST"
    )

# Explanations:
# 1. Use 'filter' to select rows where 'COMP_WEEK' is equal to 1 and 'COMP_YEAR' is equal to "2022".
# 2. Use the pipe operator ('|>') to pass the resulting DataFrame to the next operation.
# 3. Use 'select' to keep only the columns that start with "COMP_WEEK_".
# 4. Use 'distinct' to keep only unique rows after selecting columns.
# 5. The resulting DataFrame will contain only the columns that start with "COMP_WEEK_" from the filtered rows.
trip_date_2 <-
  compl_override_data__renamed__this_year |>
  filter(COMP_WEEK == 1,
         COMP_YEAR == my_year) |>
  select(starts_with("COMP_WEEK_")) |>
  distinct()

compl_first_week_my_year <-
  tibble(min1 = as.Date(min(trip_date_2$COMP_WEEK_START_DT),
                        tz = Sys.timezone()),
    # [1] "2022-01-03 EST"
    max1 = as.Date(max(trip_date_2$COMP_WEEK_END_DT),
                   tz = Sys.timezone()))
    # [1] "2022-01-09 EST"

diffdf::diffdf(dnfs_first_week_my_year, compl_first_week_my_year)
# same as above, ok

### join the dfs ----
dnfs_join_overr <-
  full_join(SEFHIER_dnfs_short_date__iso,
            compl_override_data__renamed__this_year,
            join_by(TRIP_DATE_YEAR == COMP_YEAR,
                    VESSEL_OFFICIAL_NUMBER,
                    TRIP_DATE_WEEK == COMP_WEEK),
            relationship = "many-to-many"
  )

# The below section is an example of the many to many relationship, using 2023 data (to check, remove 'relationship = "many-to-many"' from above.)
# We need 'relationship = "many-to-many"':
# ℹ Row 46609 of `x` matches multiple rows in `y`.
in_x <- SEFHIER_dnfs_short_date__iso[46609, ]

compl_override_data__renamed__this_year |>
  filter(VESSEL_OFFICIAL_NUMBER ==
           in_x$VESSEL_OFFICIAL_NUMBER &
           COMP_YEAR == in_x$TRIP_DATE_YEAR &
           COMP_WEEK == in_x$TRIP_DATE_WEEK) |>
  glimpse()
# An error in the compliance report, duplicate records.

# ℹ Row 22748 of `y` matches multiple rows in `x`.
# We need the many to many relationship because the DNFs represent a single day in a 7 day week, while the compliance represents a single week. So the relationship between DNFs to Compliance is 7 to 1.

in_compl_not_in_dnfs <-
  dnfs_join_overr |>
  filter(is.na(TRIP_ID)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_compl_not_in_dnfs)

in_dnfs_not_in_compl <-
  dnfs_join_overr |>
  filter(is.na(IS_COMP)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_dnfs_not_in_compl)

# TODO: validate in_compl_not_in_dnfs and in_dnfs_not_in_compl
# my_year
# View(in_dnfs_not_in_compl)

# stats
my_stats(SEFHIER_dnfs_short_date__iso)
# 2022
# rows: 440307
# columns: 8
# Unique vessels: 2020
# Unique trips: 440307
my_stats(dnfs_join_overr)
# 2022
# rows: 441000
# columns: 28
# Unique vessels: 2020
# Unique trips: 440307

### Add a compliance column dnfs_join_overr

## Flag trips neg that were received > 30 days after the trip date, by using compliance data and time of submission ----

### Use trip end date to calculate the usable date 30 days later ----

# Add a correct timezone to TRIP_DATE (EST vs. EDT)

dnfs_notoverridden__w_missing__timezone <-
  dnfs_notoverridden__w_missing |>
  mutate(TRIP_DATE_E =
           ymd_hms(TRIP_DATE,
                   truncated = 3,
                   tz = Sys.timezone()))

# add a date 30 days later and set time to 23:59:59

# Explanations:
# 1. Use 'mutate' to create a new column named 'USABLE_DATE_TIME' by adding 30 days to the 'TRIP_DATE_E' column.
# 2. Use the `hour<-` function from lubridate package to set the hour component of 'USABLE_DATE_TIME' to 23.
# 3. Use 'mutate' to update 'USABLE_DATE_TIME' with the hour modification.
# 4. Use the `minute<-` function from lubridate package to set the minute component of 'USABLE_DATE_TIME' to 59.
# 5. Use 'mutate' to update 'USABLE_DATE_TIME' with the minute modification.
# 6. Use the `second<-` function from lubridate package to set the second component of 'USABLE_DATE_TIME' to 59.
# 7. Use 'mutate' to update 'USABLE_DATE_TIME' with the second modification.
dnfs_notoverridden_all <-
  dnfs_notoverridden__w_missing__timezone |>
  mutate(USABLE_DATE_TIME =
           TRIP_DATE_E + days(30)) |>
  mutate(USABLE_DATE_TIME =
           `hour<-`(USABLE_DATE_TIME, 23)) |>
  mutate(USABLE_DATE_TIME =
           `minute<-`(USABLE_DATE_TIME, 59)) |>
  mutate(USABLE_DATE_TIME =
           `second<-`(USABLE_DATE_TIME, 59))

# dnfs_notoverridden_all |> glimpse()
# $ TRIP_DATE_E            <dttm> 2022-02-04 23:00:00
# $ USABLE_DATE_TIME       <dttm> 2022-03-06 23:59:59

# Drop empty columns
dnfs_notoverridden_ok <-
  dnfs_notoverridden_all |>
  select(where(not_all_na))

# dropped, bc they were all NAs:
# SRH_VESSEL_ID
# COMP_OVERRIDE_DT
# COMP_OVERRIDE_USER_ID
# COMP_OVERRIDE_CMT
# SRFH_ASSIGNMENT_ID

# subtract the usable date from the date of submission
# value is true if the dnf was submitted within 30 days, false if the dnf was not

late_submission_filter_stats <-
  function(my_df) {
    my_stats(my_df)

    late_submission <-
      my_df |>
      filter(MORE_THAN_30_DAYS_LATE == TRUE)

    my_tee(n_distinct(late_submission$TRIP_ID),
           "Count late_submission (dnfs num)")
    # trip_ids: 16449

    my_tee(
      n_distinct(late_submission$VESSEL_OFFICIAL_NUMBER),
      "Count late_submission (vessels num)"
    )
    # 1064

    # check
    min(my_df$TRIP_DATE)
    # [1] "2022-01-01"
    max(my_df$TRIP_DATE)
    # [1] "2022-12-31"
  }

# The logic should be that when the DE is less than the USABLE_DATE_TIME, the answer to the question MORE_THAN_30_DAYS_LATE should be No.

# Explanations:
# 1. Use 'mutate' to create a new column named 'MORE_THAN_30_DAYS_LATE'.
# 2. Use 'case_when' to conditionally assign values to the new column based on the comparison of 'DE' and 'USABLE_DATE_TIME'.
# 3. If 'DE' is less than or equal to 'USABLE_DATE_TIME', assign FALSE to 'MORE_THAN_30_DAYS_LATE'.
# 4. If the condition in step 3 is not met (i.e., 'DE' is greater than 'USABLE_DATE_TIME'), assign TRUE to 'MORE_THAN_30_DAYS_LATE'.

late_submission_filter <-
  function(dnf_df) {
    SEFHIER_dnfs_notoverridden__temp <-
      dnf_df |>
      mutate(MORE_THAN_30_DAYS_LATE =
               case_when(DE <= USABLE_DATE_TIME ~ FALSE,
                         .default = TRUE))

    late_submission_filter_stats(SEFHIER_dnfs_notoverridden__temp)

    return(SEFHIER_dnfs_notoverridden__temp)
  }

### Flag: data frame of dnfs that were usable ----
SEFHIER_processed_dnfs__late_subm <-
  late_submission_filter(dnfs_notoverridden_ok)
# rows: 369816
# columns: 26
# Unique vessels: 1991
# Unique trips (logbooks): 369667
# ---
# Count late_submission (dnfs num)
# 272217
# ---
# Count late_submission (vessels num)
# 1926

# Add all columns from processed metrics tracking to obtain the Permit region ----

SEFHIER_processed_dnfs__late_subm__metrics <-
  left_join(SEFHIER_processed_dnfs__late_subm,
            processed_metrics_tracking)

# stats
my_stats(SEFHIER_processed_dnfs__late_subm__metrics)
# rows: 369816
# columns: 34
# Unique vessels: 1991
# Unique trips: 369667

# stats total ----

dnfs_before_filtering_out_overridden <-
  n_distinct(SEFHIER_dnfs_short_date__iso$TRIP_ID)

my_tee(dnfs_before_filtering_out_overridden,
        "dnfs before filtering out overridden")
# 440307 2022
# 52393 2023

dnfs_after_filtering_out_overridden <-
  n_distinct(dnfs_notoverridden_ok$TRIP_ID)

my_tee(dnfs_after_filtering_out_overridden,
        "dnfs after filtering out overridden")
# 369667 2022
# 51340 2023

percent_of_removed_dnfs <-
  (dnfs_before_filtering_out_overridden - dnfs_after_filtering_out_overridden) * 100 / dnfs_before_filtering_out_overridden

cat(percent_of_removed_dnfs, sep = "\n")
# 16.04335 2022

# removed_vessels
vessels_before_filtering_out_overridden <-
  n_distinct(SEFHIER_dnfs_short_date__iso$VESSEL_OFFICIAL_NUMBER)

cat(vessels_before_filtering_out_overridden)
# 2020 2022
# 1646 2023

vessels_after_filtering_out_overridden <-
  n_distinct(dnfs_notoverridden_ok$VESSEL_OFFICIAL_NUMBER)

cat(vessels_after_filtering_out_overridden)
# 1991 2022
# 1597 2023

removed_vessels <-
  vessels_before_filtering_out_overridden - vessels_after_filtering_out_overridden
# 29

percent_of_removed_vessels <-
  (vessels_before_filtering_out_overridden - vessels_after_filtering_out_overridden) * 100 / vessels_before_filtering_out_overridden

removed_dnfs_and_vessels_text <-
  c(
    crayon::blue("percent_of_removed_dnfs"),
    str_glue("{round(percent_of_removed_dnfs)}%"),
    crayon::blue("removed_vessels"),
    removed_vessels,
    crayon::blue("percent_of_removed_vessels"),
    str_glue("{round(percent_of_removed_vessels)}%")
  )

my_tee(removed_dnfs_and_vessels_text,
       "\nRemoved dnfs and vessels stats. For vessels in Metrics tracking only.")
# 2022
# dnfs before filtering out overridden
# 440307
# dnfs after filtering out overridden
# 369667

# Removed dnfs and vessels stats. For vessels in Metrics tracking only.
# percent_of_removed_dnfs
# 16%
# removed_vessels
# 29
# percent_of_removed_vessels
# 1%

# 2023
# dnfs after filtering out overridden
# 458906
# Removed dnfs and vessels stats. For vessels in Metrics tracking only.
# percent_of_removed_dnfs
# 9%
# removed_vessels
# 12
# percent_of_removed_vessels
# 1%

# Export usable dnfs ----

SEFHIER_processed_dnfs_file_name <-
  str_glue("SEFHIER_processed_dnfs_{my_year}.rds")

annas_file_path <-
  file.path(Path,
            "Outputs",
            SEFHIER_processed_dnfs_file_name)

jennys_file_path <-
  file.path(Path,
            Outputs,
            SEFHIER_processed_dnfs_file_name)

michelles_file_path <-
  file.path(Path,
            Outputs,
            SEFHIER_processed_dnfs_file_name)

write_rds(
  SEFHIER_processed_dnfs__late_subm__metrics,
  file = output_file_path
)

