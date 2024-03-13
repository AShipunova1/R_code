# processing_DNF_data

# The result will be in
# SEFHIER_processed_dnfs_{my_year}.rds

# Files to read or create:
# 1) Raw_Oracle_Downloaded_compliance_2021_plus.rds
# 2) Raw_Oracle_Downloaded_dnf_{my_date_beg}__{my_date_end}.rds
# 3) SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds
       # use processing_metrics_tracking.R to create file #3 before running this script
#4) processing_auxiliary_methods.R
       # get from Google Drive R code folder, put in path directory with this script

# This code processes DNF data from Oracle database ready for FHIER,
# then cleans it up, so that we can use it in any DNF data analysis:
# (1) (a) pull all DNF and compliance/override data from Oracle database
#      (b) get Metrics Tracking from FHIER
# (2) clean up DNF data set
#      (a) remove records from SRHS vessels
# (3) mark all trips neg that were received > 30 days after trip end date, by using time of #submission
# (4) remove all overridden data, because the submission date is unknown
# (5) Add permit region information (GOM, SA, or dual), using permit names (optional)

# For 2022 we don't keep trips neg starting in 2021 and ending in 2022. We only keep trips neg starting in 2022.

# Caveats:
# 1) The way COMP_WEEK is calculated could get messed up depending on a given year time frame. It's due to something
# internal called the ISO number and how the function calculates the start of a week. If you are running this on a
# new data set, check your weeks to make sure it's calculating correctly.

# Running the code
# To run the file as a whole, you can type this in the console: source('Processing DNF Data.R') and hit enter.
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
michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/DNF related analyses/DNF Processing (Do this before all DNF Analyses)/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingDNFData/"
# r"(C:\Users\jenny.ostroff\Desktop\Backups\Rcode\ProcessingDNFData)"

# Input files are the same here
annas_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data/)"

# Change to use another path instead:
# Path <- michelles_path
Path <- annas_path

Inputs <- "Inputs/"
Outputs <- "Outputs/"

# Set the date ranges for the DNF and compliance data you are pulling
# this is the year to assign to the output file name
# my_year <- "2022"
# my_date_beg <- '01-JAN-2022'
# my_date_end <- '31-DEC-2022'

my_year <- "2023"
my_date_beg <- '01-JAN-2023'
my_date_end <- '31-DEC-2023'

# years range for srfh_vessel_comp db download
db_year_1 <- "2021"
db_year_2 <- "2023"

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
       my_title = str_glue("Start DNF processing for {my_year}"))

# Get data ----

# This section is needed to pull data from Oracle database
# to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

# set up an Oracle connection
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

# 2) Create a variable with a table name to call data from, define year.
# >= 2021 because of when the program started
compl_err_query <-
  str_glue(
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '{db_year_1}' AND comp_year <= '{db_year_2}'")

# Check if the file path is correct, optional
# file.exists(compl_override_data_file_path)

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query)
# 2024-02-05 run for Compliance_raw_data_2021_plus.rds: 104.5 sec elapsed
# File: Compliance_raw_data_2021_plus.rds modified Mon Feb  5 09:52:06 2024

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data__renamed <-
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
  compl_override_data__renamed |>
  filter(COMP_WEEK_END_DT >= as.Date(my_date_beg, "%d-%b-%Y") &
           COMP_WEEK_START_DT <= as.Date(my_date_end, "%d-%b-%Y"))

# check
# That's the week 52 of my_year-1:
min(compl_override_data_this_year$COMP_WEEK_START_DT)
# [1] "2021-12-27 EST" #this might contain the last week in the year before my_year, to account for a compliance week that overlaps last week of the year and first week of my_year
min(compl_override_data_this_year$COMP_WEEK_END_DT)
# [1] "2022-01-02 EST" #this should be the last day of the first week in my_year

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
#some may make this file path the Inputs file, because you are inputting this file into this script
#it doesn’t matter as long as your file location on your computer matches what you say here

# file.exists(processed_metrics_tracking_path)

SEFHIER_permit_info_short_this_year <-
  read_rds(processed_metrics_tracking_path)

## Import and prep the dnf data ####

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
# 2024-02-05 run for Raw_Oracle_Downloaded_dnf_01-JAN-2022__31-DEC-2022.rds: 104.7 sec elapsed

# check
get_dnfs_check_ids <- function(dnfs) {
  dnfs_check_ids <-
    dnfs |> filter(!is.na(COAST_GUARD_NBR) & !is.na(STATE_REG_NBR)) |>
    select(COAST_GUARD_NBR, STATE_REG_NBR, VESSEL_OFFICIAL_NUMBER) |>
    distinct()
}

# dnfs_check_ids <- get_dnfs_check_ids(dnfs)
# View(dnfs_check_ids)
# 116

### Fewer columns ----
# names(dnfs)
dnfs_short <-
  dnfs |>
  select(-c(STATE_REG_NBR, COAST_GUARD_NBR))

# stats
my_stats(dnfs_short, "dnfs from the db")
# 2022
# rows: 790839
# columns: 5
# Unique vessels: 2241
# Unique trips neg (dnfs): 790839

### reformat trip date ----
# Explanation:
#
# 1. **Create New Dataframe:**
#    - `dnfs <- dnfs |> ...`: Create a new dataframe 'dnfs' by using the pipe operator '|>' on the existing 'dnfs'.
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

dnfs_short_date <-
  dnfs_short |>
  mutate(TRIP_DATE = as.Date(TRIP_DATE))

# Check
# dnfs_short$TRIP_DATE |>
#   class()
# dnfs_short_date$TRIP_DATE |>
#   class()
# before
# "POSIXct" "POSIXt"
# after
# "Date"

# Now:
# dnfs_short_date$TRIP_DATE |>
#   head(1)
# [1] "2022-09-19"

### Filter out just my_analysis_year dnf entries ----

# check
min(dnfs$TRIP_DATE)
# [1] "2022-01-01"
# [1] "2023-01-01"
max(dnfs$TRIP_DATE)
# [1] "2022-12-31"
# [1] "2023-12-31"

dnfs_short_date__in_range <-
  dnfs_short_date |>
  filter(TRIP_DATE >= as.Date(my_date_beg, "%d-%b-%Y") &
           TRIP_DATE <= as.Date(my_date_end, "%d-%b-%Y"))

# stats, to compare with the end result
dnfs_stat_correct_dates_before_filtering <-
  c(dim(dnfs_short_date__in_range),
    n_distinct(dnfs_short_date__in_range$VESSEL_OFFICIAL_NUMBER),
    n_distinct(dnfs_short_date__in_range$TRIP_ID)
  )

my_stats(dnfs_short_date__in_range, "dnfs after filtering by dates")
# rows: 790839
# columns: 5
# Unique vessels: 2241
# Unique trips neg (dnfs): 790839

# check
min(dnfs_short_date__in_range$TRIP_DATE)
# [1] "2022-01-01"
max(dnfs_short_date__in_range$TRIP_DATE)
# [1] "2022-12-31"

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
#

# Needed to adjust for week 52 of the previous year
dnfs_short_date__in_range__iso <-
  dnfs_short_date__in_range |>
  mutate(COMP_WEEK = isoweek(TRIP_DATE), # puts it in week num
         TRIP_END_YEAR = isoyear(TRIP_DATE)) # adds a year

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

## add override data to dnfs ----
my_stats(compl_override_data_this_year,
         "Compl/override data from the db")
# rows: 150029
# columns: 23
# Unique vessels: 3626

# We need 'relationship = "many-to-many"' because
# TODO: 1)
# 2) 1 row of `y` matches multiple rows in `x`:
# We need the many to many relationship because the DNFs represent a single day in a 7 day week, while the compliance represents a single week. So the relationship between DNFs to Compliance is 7 to 1.

dnfs_join_overr <-
  left_join(dnfs_short_date__in_range__iso,
            compl_override_data_this_year,
            join_by(TRIP_END_YEAR == COMP_YEAR,
                    VESSEL_OFFICIAL_NUMBER,
                    COMP_WEEK),
            relationship = "many-to-many"
  )

# the below section of 25 lines is an example of the many to many relationship, using 2022 data
# ℹ Row 104686 of `x` matches multiple rows in `y`.

# dnfs_short_date__in_range__iso[104686, ] |> glimpse()
# 1242820

#compl_override_data_this_year |>
#  filter(VESSEL_OFFICIAL_NUMBER == "1242820" &
#           COMP_WEEK == 32 &
#           COMP_YEAR == 2022) |>
#  View()

# FL9558PU

# dnfs_short_date__in_range__iso |>
#   filter(VESSEL_OFFICIAL_NUMBER == "FL9558PU")
# 0

# ℹ Row 43081 of `y` matches multiple rows in `x`.

#compl_override_data_this_year[43081, ] |> glimpse()
# 1228073

# dnfs_short_date__in_range__iso |>
#   filter(VESSEL_OFFICIAL_NUMBER == "1228073" &
#            TRIP_END_YEAR == 2022 &
#            COMP_WEEK == 20) |>
  # View()


# stats
my_stats(dnfs_short_date__in_range__iso)
my_stats(dnfs_join_overr)
# dnfs_short_date__in_range__iso
# rows: 790839
# columns: 7
# Unique vessels: 2241
# Unique trips neg (dnfs): 790839
# ---
# dnfs_join_overr
# rows: 791534
# columns: 27
# Unique vessels: 2241
# Unique trips neg (dnfs): 790839

# Make lists of overridden or not vessels
# If a week for a vessel was overridden (compl_override_data_this_year), remove the trip reports from the corresponding week in the dnf data
# We have to remove dnfs for weeks that were overridden because we don't have a timestamp for when the dnf was submitted to the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate that time laps.
# We can't differentiate between turning a dnf in on time in the app, and it taking two months to get it vs turning in a dnf two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and SAFIS) in March, 2022 (At permit renewal)... user submitted on time in app (VESL) but we may not get that report in SAFIS for months later (when its found as a "missing report" and then requeued for transmission)

dnfs_overridden <-
  filter(dnfs_join_overr, OVERRIDDEN == 1) #data frame of dnfs that were overridden

# stats
my_stats(dnfs_overridden)
# rows: 17642
# columns: 27
# Unique vessels: 379
# Unique trips neg (dnfs): 17432

dnfs_notoverridden <-
  filter(dnfs_join_overr, OVERRIDDEN == 0) #data frame of dnfs that weren't overridden

# stats
my_stats(dnfs_notoverridden)
# rows: 369465
# columns: 27
# Unique vessels: 2005
# Unique trips neg (dnfs): 369316

dnfs_NA <-
  filter(dnfs_join_overr, is.na(OVERRIDDEN)) # dnfs with an Overridden value of NA, because they were
# 1) submitted by a vessel that is missing from the Compliance report and therefore has no associated override data, or
# 2) submitted by a vessel during a period in which the permit was inactive, and the report was not required

# stats
my_stats(dnfs_NA)
# rows: 404427
# columns: 27
# Unique vessels: 1103
# Unique trips neg (dnfs): 404427

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
# 61

# SEFHIER dnfs from vessels missing from the Compliance report
vessels_missing_dnfs <-
  dnfs_NA |>
  filter(VESSEL_OFFICIAL_NUMBER %in% vessels_missing)

# add missing dnfs back to the not overridden data frame
dnfs_notoverridden <-
  rbind(dnfs_notoverridden,
        vessels_missing_dnfs) |>
  distinct()

my_stats(dnfs_notoverridden)
# rows: 371755
# columns: 27
# Unique vessels: 2018
# Unique trips neg (dnfs): 371606

# remove missing dnfs from NA dataset, the NA dataset is now only those that were submitted when not needed

my_stats(dnfs_NA)
# Unique vessels: 1103
# Unique trips neg (dnfs): 404427

# Subset the dnfs_NA dataframe by excluding rows with VESSEL_OFFICIAL_NUMBER
# present in the vessels_missing vector.
dnfs_NA__rm_missing_vsls <- dnfs_NA |>
  filter(!VESSEL_OFFICIAL_NUMBER %in% vessels_missing)

my_stats(dnfs_NA__rm_missing_vsls,
         "dnfs_NA after removing missing dnfs")
# rows: 402137
# columns: 27
# Unique vessels: 1090
# Unique trips neg (dnfs): 402137

# We have decided to throw out dnfs that were submitted when the permit was inactive, the logic
# being we shouldn't include dnfs that weren't required in the first place. Alternatively,
# deciding to keep in the NAs means we would be keeping reports that were submitted by a vessel
# during a period in which the permit was inactive, and the report was not required.
# rbind(dnfs_notoverridden, dnfs_NA) this is the alternative

# Use trip end date to calculate the usable date 30 days later

# Add a correct timezone to TRIP_DATE (EST vs. EDT)
dnfs_notoverridden <-
  dnfs_notoverridden |>
  mutate(TRIP_DATE_E =
           ymd_hms(TRIP_DATE,
                   truncated = 3,
                   tz = Sys.timezone()))

# add a date 30 days later with a time
dnfs_notoverridden <-
  dnfs_notoverridden |>
  mutate(USABLE_DATE_TIME =
           TRIP_DATE_E +
           days(30) +
           hours(23) +
           minutes(59) +
           seconds(59))

# format the submission date (DE)
dnfs_notoverridden_all <-
  dnfs_notoverridden |>
  mutate(DE =
           as.POSIXct(DE, format = "%Y-%m-%d %H:%M:%S"))

# Drop empty columns
dnfs_notoverridden <-
  dnfs_notoverridden_all |>
  select(where(not_all_na))

# diffdf::diffdf(dnfs_notoverridden,
#                dnfs_notoverridden_all)
# dropped, bc they were all NAs:
# SRH_VESSEL_ID
# COMP_OVERRIDE_DT
# COMP_OVERRIDE_USER_ID
# COMP_OVERRIDE_CMT
# SRFH_ASSIGNMENT_ID

# stats, what was lost by excluding the overridden dnfs
uniq_vessels_num_was <-
  n_distinct(dnfs_short_date__in_range__iso[["VESSEL_OFFICIAL_NUMBER"]])
uniq_vessels_num_now <-
  n_distinct(dnfs_notoverridden[["VESSEL_OFFICIAL_NUMBER"]])

uniq_trips_num_was <- n_distinct(dnfs_short_date__in_range__iso[["TRIP_ID"]])
uniq_trips_num_now <-
  n_distinct(dnfs_notoverridden[["TRIP_ID"]])

uniq_vessels_lost_by_overr <-
  uniq_vessels_num_was - uniq_vessels_num_now
# 12

uniq_trips_lost_by_overr <-
  uniq_trips_num_was - uniq_trips_num_now
# 2981

my_tee(uniq_vessels_lost_by_overr,
       "Thrown away vessels by overridden weeks")
# 223

my_tee(uniq_trips_lost_by_overr,
       "Thrown away trips neg by overridden weeks")
# 419233

# Filtering dnf data ----
# Use dnfs_notoverridden from the previous section

## Filter out vessels not in Metrics tracking ----
SEFHIER_dnfs_notoverridden <-
  dnfs_notoverridden |>
  filter(VESSEL_OFFICIAL_NUMBER %in% SEFHIER_permit_info_short_this_year$VESSEL_OFFICIAL_NUMBER)

my_stats(dnfs_notoverridden)
my_stats(SEFHIER_dnfs_notoverridden)

vessels_not_in_metrics <-
  n_distinct(dnfs_notoverridden$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_dnfs_notoverridden$VESSEL_OFFICIAL_NUMBER)

my_tee(vessels_not_in_metrics,
       "Removed if a vessel is not in Metrics tracking")
# 47

## Mark all trips neg that were received > 30 days after the trip date, by using compliance data and time of submission ----

# subtract the usable date from the date of submission
# value is true if the dnf was submitted within 30 days, false if the dnf was not

late_submission_filter_stats <-
  function(my_df) {
    my_stats(my_df)

    late_submission <-
      my_df |>
      filter(MORE_THAN_30_DAYS_LATE == FALSE)

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

late_submission_filter <-
  function() {
    SEFHIER_dnfs_notoverridden__temp <-
      SEFHIER_dnfs_notoverridden |>
      mutate(MORE_THAN_30_DAYS_LATE =
               case_when(DE <= USABLE_DATE_TIME ~ FALSE,
                         .default = TRUE))
    late_submission_filter_stats(SEFHIER_dnfs_notoverridden__temp)

    return(SEFHIER_dnfs_notoverridden__temp)
  }

### Filter (mark only): data frame of dnfs that were usable ----
SEFHIER_processed_dnfs__late_subm <- late_submission_filter()
# rows: 366565
# columns: 25
# Unique vessels: 1971
# Unique trips neg (dnfs): 366417
# ---
# Count late_submission (dnfs num)
# 268497
# ---
# Count late_submission (vessels num)
# 1904

# Add all columns from processed metrics tracking to obtain the Permit region.

SEFHIER_processed_dnfs <-
  left_join(SEFHIER_processed_dnfs__late_subm,
            SEFHIER_permit_info_short_this_year)

# > my_stats(SEFHIER_processed_dnfs)
# SEFHIER_processed_dnfs
# rows: 366565
# columns: 26
# Unique vessels: 1971
# Unique trips neg (dnfs): 366416
# ---
# > my_stats(SEFHIER_processed_dnfs_1)
# SEFHIER_processed_dnfs_1
# rows: 368037
# columns: 33
# Unique vessels: 3443
# Unique trips neg (dnfs): 366417

# stats
my_stats(SEFHIER_processed_dnfs)

dnfs_before_filtering <-
  n_distinct(dnfs_short_date__in_range__iso$TRIP_ID)

my_tee(dnfs_before_filtering,
        "dnfs before filtering")
# 790839 2022
# 52393 2023

dnfs_after_filtering <-
  n_distinct(SEFHIER_processed_dnfs$TRIP_ID)

my_tee(dnfs_after_filtering,
        "dnfs after filtering")
# 51340 2023
# 366416 2022

percent_of_removed_dnfs <-
  (dnfs_before_filtering - dnfs_after_filtering) * 100 / dnfs_before_filtering
 cat(percent_of_removed_dnfs, sep = "\n")
# 53.66743 2022

# removed_vessels
vessels_before_filtering <-
  n_distinct(dnfs_short_date__in_range__iso$VESSEL_OFFICIAL_NUMBER)
 cat(vessels_before_filtering)
# 1646 2023
# 2241 2022

vessels_after_filtering <-
  n_distinct(SEFHIER_processed_dnfs$VESSEL_OFFICIAL_NUMBER)
 cat(vessels_after_filtering)
# 1597 2023
# 1971 2022

removed_vessels <-
  vessels_before_filtering - vessels_after_filtering
# 270

percent_of_removed_vessels <-
  (vessels_before_filtering - vessels_after_filtering) * 100 / vessels_before_filtering

removed_dnfs_and_vessels_text <- c(
  crayon::blue("percent_of_removed_dnfs"),
  str_glue("{round(percent_of_removed_dnfs)}%"),
  crayon::blue("removed_vessels"),
  removed_vessels,
  crayon::blue("percent_of_removed_vessels"),
  str_glue("{round(percent_of_removed_vessels)}%")
)

my_tee(removed_dnfs_and_vessels_text,
       "\nRemoved dnfs and vessels stats")

# Removed dnfs and vessels stats
# percent_of_removed_dnfs
# 54%
# removed_vessels
# 270
# percent_of_removed_vessels
# 12%

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

# !! Change to the correct path !!
output_file_path <-
  annas_file_path

write_rds(
  SEFHIER_processed_dnfs,
  file = output_file_path
)

