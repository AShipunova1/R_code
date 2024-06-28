# processing_DNF_data

# Files to create prior to running this script:
# 1) SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds
       # use processing_metrics_tracking.R to create this file before running this script

# Files this script will create:
# 1) Raw_Oracle_Downloaded_compliance_2021_plus.rds
# 2) Raw_Oracle_Downloaded_dnf_{my_compliance_date_beg}__{my_compliance_date_end}.rds
# 3) SEFHIER_processed_dnfs__compliance_weeks_{my_year}.rds
# 4) SEFHIER_processed_dnfs_calendar_{my_year}.rds
# this is the final processed data set for the calendar year that you can then use in further analyses
# 5) SEFHIER_processed_dnfs_{my_year}.rds

# This code processes DNF data from SEFSC’s Oracle database,
# then cleans it up, so that we can use it in any DNF data analysis:
# (1) (a) pull all DNF and compliance/override data from Oracle database
#     (b) get Metrics Tracking from FHIER
# (2) clean up DNF data set
#     (a) remove records from SRHS vessels
# (3) flag all overridden data.
       # Depending on the analysis question, we may want to remove DNFs for weeks that
       # were overridden because we don't have a timestamp for when the DNF was submitted to
       # the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate between
       # turning a DNF in on time- in the app, and it then taking two months to get it into FHIER vs
       # turning in a DNF two months late.
       # E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and
       # SAFIS) in March, 2022 (at permit renewal)... user submitted on time in app (VESL) but we
       # may not get that report in SAFIS for months later (when its found as a "missing report" and
       # then requeued for transmission)
# (4) flag all DNFs (trips neg) that were received > 30 days after trip end date, by using time of
       # submission
       # Depending on the analysis question, we may want to remove DNF’s that were more than
       # 30 days late. Reports submitted more than 30 days late are often not used due to recall
       # bias.

# Notes:
# For this analysis, a fishing week starts on Monday and ends on Sunday (following the regs).
# For 2022 we don't keep DNFs starting in 2021 and ending in 2022. We only keep DNFs starting in 2022.

# Caveats:
# 1) The way COMP_WEEK is calculated could get messed up depending on a given year's time frame.
  # It's due to something internal called the ISO number and how the function calculates the start of a week.
  # If you are running this on a new data set, check your weeks to make sure it's calculating correctly.

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
  "C:/Users/michelle.masi/Documents/SEFHIER/R code/Processed data/"

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

# years range for srfh_vessel_comp db download, see below
db_year_1 <- "2023"
db_year_2 <- "2024"

# ---
# Explanations:
# - This function, `get_the_dates`, generates a list of date strings and date objects based on a specified year and a start day for weeks.
# - It takes two parameters: `my_year`, which defaults to "2023", and `week_start_day`, which defaults to "Monday".
# - The function returns a list (`lst`) of the start and end dates for the calendar year, as well as the start and end dates for compliance based on week boundaries. lst is used to keep entries names.
# The compliance dates include the "fringe" weeks if needed.
#
# 1. **Generating Calendar Dates**:
#     - The function first creates two strings representing the start and end dates of the calendar year based on the provided `my_year` parameter.
#     - `my_calendar_date_beg` is set to "01-JAN-{my_year}" and `my_calendar_date_end` is set to "31-DEC-{my_year}" using string interpolation (`str_glue`).
#
# 2. **Calculating Compliance Date Boundaries**:
#     - The function calculates the compliance start and end dates based on the provided `week_start_day` option.
#     - It uses `lubridate` functions to convert the calendar date strings to date objects (`dmy`) and then adjust them to the nearest week boundaries.
#     - `my_compliance_date_beg` is calculated as the beginning of the week containing the calendar start date.
#     - `my_compliance_date_end` is calculated as the end of the week containing the calendar end date, minus one day.
#     - The `getOption` function is used to ensure the start of the week is set according to `week_start_day`.
#
# 3. **Creating the List of Dates**:
#     - The function combines the four calculated dates (`my_calendar_date_beg`, `my_calendar_date_end`, `my_compliance_date_beg`, and `my_compliance_date_end`) into a list using the `lst` function.
#
week_start_day = "Monday"

get_the_dates <-
  function(my_year = "2023",
           week_start_day = "Monday") {
    my_calendar_date_beg <- str_glue("01-JAN-{my_year}")
    my_calendar_date_end <- str_glue("31-DEC-{my_year}")
    my_compliance_date_beg <-
      dmy(my_calendar_date_beg) |>
      floor_date('weeks', week_start = getOption("lubridate.week.start", week_start_day))
    my_compliance_date_end <-
      dmy(my_calendar_date_end) |>
      ceiling_date('weeks',
                   week_start = getOption("lubridate.week.start", week_start_day)) - 1

    my_dates <- lst(
      my_calendar_date_beg,
      my_calendar_date_end,
      my_compliance_date_beg,
      my_compliance_date_end
    )

    return(my_dates)
  }

curr_dates <- get_the_dates(my_year)
my_compliance_date_beg <- curr_dates$my_compliance_date_beg
my_compliance_date_end <- curr_dates$my_compliance_date_end

# ---
# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

# Define a helper function 'title_message_print' to print the title message in blue.
title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg), sep = "\n")
}

# Define a helper function 'my_tee' to print the message to the console and a file.
my_tee <- function(my_text,
                   my_title = NA,
                   stat_log_file_path = NA,
                   date_range = my_year) {

  the_end = "---"

  # Print out to console
  title_message_print(my_title)
  cat(c(my_text, the_end),
      sep = "\n")

  # Create a new file every day
  if (is.na(stat_log_file_path)) {
    stat_log_file_path <-
      file.path(Path,
                Outputs,
                str_glue("processing_stats_{date_range}_run_{today()}.log"))
  }

  # Write to a log file
  cat(c(my_title, my_text, the_end),
      file = stat_log_file_path,
      sep = "\n",
      append = TRUE)
}

# ---
# A function to print out stats.
# Usage: my_stats(Logbooks)

# Explanation:
#
# 1. **Define Function with Optional Parameter:**
#    - `my_stats <- function(my_df, title_msg = NA) { ... }`: Define a function named 'my_stats' that takes a dataframe 'my_df' as input and an optional 'title_msg' parameter with a default value of NA.
#
# 2. **Check and Assign Default Title Message:**
#    - `if (is.na(title_msg))  { ... }`: Check if 'title_msg' is NA, and if so, assign the dataframe name as the default title message using 'deparse(substitute(my_df))'.
#
# 3. **Extract Statistics:**
#    - `rows_n_columns <- dim(my_df)`: Extract the number of rows and columns in the dataframe.
#    - `uniq_vessels_num <- n_distinct(my_df[["VESSEL_OFFICIAL_NUMBER"]])`: Count the number of distinct vessel numbers.
#    - `uniq_trips_num <- n_distinct(my_df[["TRIP_ID"]])`: Count the number of distinct trip IDs.
#
# 4. **Create Formatted Text with Statistics:**
#    - `stat_text <- str_glue("rows: {rows_n_columns[[1]]} ... Unique trips (logbooks): {uniq_trips_num}")`: Use 'str_glue' to format the statistics into a text string.
#
# 5. **Print Title Message and Statistics to Console:**
#    - `title_message_print(title_msg)`: Use the helper function 'title_message_print' to print the title message in blue.
#    - `print(stat_text)`: Print the formatted statistics to the console.
#
# 6. **Write Statistics to Log File:**
#    - `stat_log_file_path <- file.path(Path, Outputs, str_glue("stat_info_{today()}.log"))`: Define the file path for the log file, including the date.
#    - `cat(c(title_msg, stat_text), file = stat_log_file_path, sep = "\n", append = TRUE)`: Write the title message and formatted statistics to the log file, appending to the file if it already exists.

my_stats <- function(my_df, title_msg = NA) {

  # A title
  if (is.na(title_msg))  {
    df_name = deparse(substitute(my_df))
    title_msg <- df_name
  }

  # Extract statistics
  rows_n_columns <- dim(my_df)
  uniq_vessels_num <- n_distinct(my_df[["VESSEL_OFFICIAL_NUMBER"]])
  uniq_trips_num <- n_distinct(my_df[["TRIP_ID"]])

  # Create a formatted text with statistics
  # include trips, only if > 0
  trip_cnts <-
    if (uniq_trips_num > 0) {
      str_glue("Unique trips: {uniq_trips_num}")
    }
  else {
    ""
  }

  stat_text <- str_glue(
    "
rows: {rows_n_columns[[1]]}
columns: {rows_n_columns[[2]]}
Unique vessels: {uniq_vessels_num}
{trip_cnts}
"
  )

  # Print out to console and to the log file
  my_tee(stat_text,
         my_title = title_msg,
         stat_log_file_path = NA)
}

# Create DNF processing start date and time (for version control tracking) ----
my_tee(date(),
       my_title = str_glue("Start DNF processing for {my_year}"))

# Get data ----

# This section is needed to pull data from Oracle database
# to get this to work, you first need to add in Oracle username and PW into the Windows credential manager
# for me instructions: https://docs.google.com/document/d/1qSVoqKV0YPhNZAZA-XBi_c6BesnH_i2XcLDfNpG9InM/edit#

# set up an Oracle connection
# Sys.getenv("ORA_SDTZ")

# You have to set up the same time zones for ROracle and RStudio. By default, they use different ones, and this difference causes dates and times to round up in R Studio, pushing some date timestamps to the next day, and making them incorrect.
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

# Define a function named 'connect_to_secpr'.
# It returns the established database connection (con), which can be used to interact with the "SECPR" database in R.
# Usage:
# con <- connect_to_secpr()
# or
# try(con <- connect_to_secpr())

connect_to_secpr <- function() {
  # Retrieve the username associated with the "SECPR" database from the keyring.
  my_username <- keyring::key_list("SECPR")[1, 2]

  # Use 'dbConnect' to establish a database connection with the specified credentials.
  con <- dbConnect(
    dbDriver("Oracle"),
    # Use the Oracle database driver.
    username = my_username,
    # Use the retrieved username.
    password = keyring::key_get("SECPR", my_username),
    # Retrieve the password from the keyring.
    dbname = "SECPR"  # Specify the name of the database as "SECPR."
  )

  # Return the established database connection.
  return(con)
}

# test if you are connected to VPN
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

# ---
# A function to use every time we want to read a ready file or query the database if no files exist.

# The read_rds_or_run_query function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run_query <- function(my_file_path,
                                  my_query,
                                  force_from_db = TRUE) {

  if (file.exists(my_file_path)) {
    modif_time <- file.info(my_file_path)$mtime
  }

    # Check if the file specified by 'my_file_path' exists and 'force_from_db' is not set.
    if (file.exists(my_file_path) &
        is.null(force_from_db)) {
        # If the file exists and 'force_from_db' is not set, read the data from the RDS file.

        function_message_print("File already exists, reading.")

        my_result <- readr::read_rds(my_file_path)

    } else {

      # If the file doesn't exist or 'force_from_db' is set, perform the following steps:

      # 0. Print this message.
      function_message_print(c(
        "File",
        my_file_path,
        "doesn't exists, pulling data from database.",
        "Must be on VPN."
      ))

      # 1. Generate a message indicating the date and the purpose of the run for "tic".
      msg_text <-
        paste(today(), "run for", basename(my_file_path))
      tictoc::tic(msg_text)  # Start timing the operation.

      # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result. I.e. download data from the Oracle database. Must be on VPN.

      my_result <- dbGetQuery(con, my_query)
      # my_result <- my_function(my_data)

      tictoc::toc()  # Stop timing the operation.

      # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
      # try is a wrapper to run an expression that might fail and allow the user's code to handle error-recovery.

      # 4. Print this message.
      function_message_print(c("Saving new data into a file here: ",
                       my_file_path))

      try(readr::write_rds(my_result,
                           my_file_path))

      modif_time <- date()
    }

  # Print out the formatted string with the file name ('my_file_name') and the modification time ('modif_time') to keep track of when the data were downloaded.
  my_file_name <- basename(my_file_path)
  function_message_print(
    str_glue("File: {my_file_name} modified {modif_time}"))

    # Return the generated or read data.
    return(my_result)
}

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use
# Use force_from_db = TRUE to pull again from the DB
compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query,
                        force_from_db = TRUE)

# check a week start day, should be Monday
compl_override_data |>
  filter(COMP_YEAR == '2023' &
         COMP_WEEK == '50') |>
  select(COMP_WEEK_START_DT) |>
  distinct() |>
  mutate(week_day = weekdays(COMP_WEEK_START_DT)) |>
  str()
# $ COMP_WEEK_START_DT: POSIXct, format: "2023-12-11".
# $ week_day          : chr "Monday"
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

# Check min date in DNF data
min(compl_override_data__renamed$COMP_WEEK_START_DT)
# [1] "2021-01-04 EST"

# keep only my_year of analysis, including week 52 of the previous year if needed
compl_override_data__renamed__this_year <-
  compl_override_data__renamed |>
  filter(COMP_WEEK_END_DT >= as.Date(my_compliance_date_beg, "%d-%b-%Y",
                                     tz = Sys.timezone()) &
           COMP_WEEK_START_DT <= as.Date(my_compliance_date_end, "%d-%b-%Y",
                                         tz = Sys.timezone()))

# check
# That's the week 52 of the previous year (my_year - 1):
min(compl_override_data__renamed__this_year$COMP_WEEK_START_DT)
# [1] "2021-12-27 EST" # this might contain the last week in the year before my_year, to account for a compliance week that overlaps last week of the previous year and first week of my_year
min(compl_override_data__renamed__this_year$COMP_WEEK_END_DT)
# [1] "2022-01-02 EST" # this might contain the last week in the year before my_year, to account for a compliance week that overlaps last week of the previous year and first week of my_year


# change data type of this column if needed
if (!class(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER) == "character") {
  compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER <-
    as.character(compl_override_data__renamed__this_year$VESSEL_OFFICIAL_NUMBER)
}

## Import the permit data ----
# Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
processed_metrics_tracking_path <-
  file.path(Path,
            Outputs,
            str_glue("SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds"))

# optional
file.exists(processed_metrics_tracking_path)

# reads the file in the path into a data frame
processed_metrics_tracking <-
  read_rds(processed_metrics_tracking_path)

## Import and prep the DNF data ----

# Import the DNF data from file or database
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) create a variable with file path to read or write the DNF file
dnfs_file_path <-
  file.path(Path,
            Outputs,
            str_glue("Raw_Oracle_Downloaded_dnf_{my_compliance_date_beg}__{my_compliance_date_end}.rds"))
# Was "SAFIS_TripsDownload_"

# 2) create a variable with an SQL query to call data from the database

# Explanation:
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
    trip_date BETWEEN TO_DATE('{my_compliance_date_beg}', 'yyyy-mm-dd') AND
TO_DATE('{my_compliance_date_end}', 'yyyy-mm-dd')
"
  )

# Use 'read_rds_or_run_query' defined above to either read DNF information from an RDS file or execute a query to obtain it and write a file for future use.
# Use force_from_db = TRUE to pull again from the DB
dnfs <-
  read_rds_or_run_query(dnfs_file_path,
                        dnfs_download_query,
                        force_from_db = TRUE)

# from scratch (with the parameter "force_from_db = TRUE")
# 2024-03-25 run for Raw_Oracle_Downloaded_dnf_01-JAN-2022__31-DEC-2022.rds: 120.43 sec elapsed
# 2024-03-25 run for Raw_Oracle_Downloaded_dnf_01-JAN-2023__31-DEC-2023.rds: 125.17 sec elapsed

### add COAST_GUARD_NBR or STATE_REG_NBR, if no VESSEL_OFFICIAL_NUMBER ----


# Explanations:
# 1. Use 'mutate' to create or modify a column named 'VESSEL_OFFICIAL_NUMBER'.
# 2. Use 'case_when' to conditionally assign values to the column based on conditions.
# 3. Check if 'VESSEL_OFFICIAL_NUMBER' is NA (missing).
#    - If true, use 'coalesce' to select the first non-missing value among 'COAST_GUARD_NBR' and 'STATE_REG_NBR'.
#    - If 'VESSEL_OFFICIAL_NUMBER' is not missing, keep its original value.
# 4. The resulting DataFrame will have the 'VESSEL_OFFICIAL_NUMBER' column filled with non-missing values from 'COAST_GUARD_NBR' or 'STATE_REG_NBR'.
dnfs_v_all_ids <-
  dnfs |>
  mutate(VESSEL_OFFICIAL_NUMBER =
           case_when(
             is.na(VESSEL_OFFICIAL_NUMBER) ~
               coalesce(COAST_GUARD_NBR, STATE_REG_NBR), # first grab coast guard number and then state reg (if coast guard numbers is not available)
             .default = VESSEL_OFFICIAL_NUMBER
           ))

# check if all vessels have ids now
# dnfs_v_all_ids |>
#   filter(is.na(VESSEL_OFFICIAL_NUMBER)) |>
#   distinct() |>
#   nrow()
# 0 - OK, it means that all NA vessel official numbers have been replaced with either a Coast Guard or State Registration number

### Remove State Reg # and Coast Guard # fields, as no longer needed ----
dnfs_short <-
  dnfs_v_all_ids |>
  select(-c(STATE_REG_NBR, COAST_GUARD_NBR))

# check # of records (rows) and unique vessels/trips, after replacing NAs above
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

### Prepare data to determine what weeks were overridden, so we can flag DNFs from those weeks later ----

# assign each DNF a week designation (first day of the reporting week is a Monday)
# use the end date to calculate this, it won't matter for most trips, but for some trips neg that
# happen overnight on a Sunday, it might affect what week they are assigned to
# https://stackoverflow.com/questions/60475358/convert-daily-data-into-weekly-data-in-r

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

# Adding flags to the DNF data ----

## Filter out vessels not in Metrics tracking ----
SEFHIER_dnfs_short_date__iso <-
  dnfs_short_date__iso |>
  filter(VESSEL_OFFICIAL_NUMBER %in% processed_metrics_tracking$VESSEL_OFFICIAL_NUMBER)

# Check the number of records (rows), vessels and trips after filtering vessels not in Metrics Tracking
my_stats(SEFHIER_dnfs_short_date__iso)
# compare numbers with DF prior to filtering out non-SEFHIER permitted vessels
my_stats(dnfs_short_date__iso)

# Create DF of vessels not in Metrics Tracking
vessels_not_in_metrics <-
  n_distinct(dnfs_short_date__iso$VESSEL_OFFICIAL_NUMBER) -
  n_distinct(SEFHIER_dnfs_short_date__iso$VESSEL_OFFICIAL_NUMBER)

# Total number of vessels in Oracle raw data but not in Metrics Tracking
my_tee(vessels_not_in_metrics,
       "Vessels removed if a vessel is not in Metrics tracking")
# 1556 (2022)
# 1857 (2023)

# Create DF of DNFs that get excluded, because the vessel is not in Metrics Tracking (submitted by non-SEFHIER permitted vessels)
dnfs_not_in_metrics <-
  n_distinct(dnfs_short_date__iso$TRIP_ID) -
  n_distinct(SEFHIER_dnfs_short_date__iso$TRIP_ID)

# Total DNFs that get excluded, after removing non-SEFHIER permitted vessels
my_tee(dnfs_not_in_metrics,
       "DNFs removed if a vessel is not in Metrics tracking")
# 356497 (2022)
# 446859 (2023)

## add compliance/override data to dnfs ----
# We add data from the compliance module to the DNF data frame to associate weeks where compliance was overridden with the corresponding DNFs.
# Depending on the analysis question, we may want to remove DNFs for weeks that
# were overridden because we don't have a timestamp for when the DNF was submitted to
# the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate between
# turning a DNF in on time- in the app, and it then taking two months to get it into FHIER vs
# turning in a DNF two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and
# SAFIS) in March, 2022 (at permit renewal)... user submitted on time in app (VESL) but we
# may not get that report in SAFIS for months later (when its found as a "missing report" and
# then requeued for transmission)
my_stats(compl_override_data__renamed__this_year,
         "Compliance and override data from the db")
# 2022
# rows: 150029
# columns: 23
# Unique vessels: 3626

### check if DNFs and compliance data have the same week dates ----
# Both datasets should define a week as Mon-Sun, and place each date in the correct week.
# The first two functions define a week for the DNF dataset, then there is an explanation for the next two functions that define a week for the compliance dataset.
# The final line in this section compares the two, and they should be equal. See the comment after the line (diffdf::diffdf).

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
# 1. Use 'filter' to select rows where 'COMP_WEEK' is equal to 1 and 'COMP_YEAR' is equal to "my_year".
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
# if the two df’s are the same, no issues were found! This means that both df’s are showing the same Mon-Sun week pattern.

### join the dfs ----
dnfs_join_overr <-
  full_join(
    SEFHIER_dnfs_short_date__iso,
    compl_override_data__renamed__this_year,
    join_by(
      TRIP_DATE_YEAR == COMP_YEAR,
      VESSEL_OFFICIAL_NUMBER,
      TRIP_DATE_WEEK == COMP_WEEK
    ),
    relationship = "many-to-many"
  )
# We need the “many-to-many” relationship. There will be many DNFs that match a compliance week, because a DNF is submitted for every day of the week.
# to see the many-to-many relationship see find_duplicates_in_compl.R

# check the difference
# This list of vessels is not concerning, it’s just an extra step in providing a thorough analysis. It's possible that a vessel submitted a logbook for every week they were permitted, so that they remained compliant but never submitted a DNF.
in_compl_not_in_dnfs <-
  dnfs_join_overr |>
  filter(is.na(TRIP_ID)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_compl_not_in_dnfs)

## This list of vessels is not concerning, it’s just an extra step in providing a thorough analysis.
# This tells us that these vessels submitted a DNF for a week when they were not permitted. It can happen when a captain is unaware that his permit has expired, but reports anyway.
in_dnfs_not_in_compl <-
  dnfs_join_overr |>
  filter(is.na(IS_COMP)) |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

nrow(in_dnfs_not_in_compl)

# my_year
# View(in_dnfs_not_in_compl)

# Total # of records, vessels and trips, comparing the df before and after joining DNF and compliance data
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

### Remove rows with NA DNFs and entries in Compliance ----
# this just means that no DNFs were submitted for that compliance week, i.e. the vessel submitted a logbook instead
dnfs_join_overr__all_dnfs <-
  dnfs_join_overr |>
  filter(!is.na(TRIP_ID))

# dim(dnfs_join_overr)
# dim(dnfs_join_overr__all_dnfs)

# This shows the different scenarios we should account for in the case_when statement below. If the results differ from what is shown in the comments here, we will need to adapt the code.
dnfs_join_overr__all_dnfs |>
  select(IS_COMP,
         OVERRIDDEN) |>
  distinct()
#   IS_COMP OVERRIDDEN
# 1       1          0
# 2      NA         NA
# 3       0          1
# 4       0          0

# this is a check that we do not have any scenarios where IS_COMP = 1 and OVERRIDDEN = 1, NA means that we don’t, and that is what we want
dnfs_join_overr__all_dnfs |>
  filter(IS_COMP == 1 & OVERRIDDEN == 1) |>
  select(TRIP_ID) |>
  distinct()
# NA


### Add a compliant_after_override column ----
# This is needed so that we can easily filter out compliant or non-compliant vessels in the dataset, by adding an extra column that states yes or no regarding compliance. The NA represents one of two possible scenarios: 1) a DNF was submitted for a vessel that is missing from the compliance module but is in metrics tracking, or 2) a DNF was submitted for a week when the vessel was not permitted. It is not simple to determine which. Deciding what to do with these DNFs will depend on the individual analysis question, and so is not addressed here, but simply left as NA.
## NOTE: IF “Is_Overriden == 1 & is_Comp == 0, then the vessel should be considered compliant in any compliance analyses
tic("Add a compliant_after_override column")
dnfs_join_overr__compl <-
  dnfs_join_overr__all_dnfs |>
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

# Add a compliant_after_override column: 108.74 sec elapsed

# check distinct values in new ‘compliant_after_override’, ‘is_comp’ and ‘overridden’ columns
dnfs_join_overr__compl |>
  select(compliant_after_override,
         IS_COMP,
         OVERRIDDEN) |>
  distinct()
#   compliant_after_override IS_COMP OVERRIDDEN
#   <chr>                      <int>      <int>
# 1 yes                            1          0
# 2 NA                            NA         NA
# 3 yes                            0          1
# 4 no                             0          0
# 5 yes                            1          1

## Flag trips neg that were received > 30 days after the trip date, by using compliance data and time of submission ----

### Use trip end date to calculate the usable date 30 days later ----

### add a date 30 days later and set time to 23:59:59 ----

# Explanations:
# 1. Use 'mutate' to create a new column named 'USABLE_DATE_TIME' by adding 30 days to the 'TRIP_DATE' column.
# 2. Use the `hour<-` function from lubridate package to set the hour component of 'USABLE_DATE_TIME' to 23.
# 3. Use 'mutate' to update 'USABLE_DATE_TIME' with the hour modification.
# 4. Use the `minute<-` function from lubridate package to set the minute component of 'USABLE_DATE_TIME' to 59.
# 5. Use 'mutate' to update 'USABLE_DATE_TIME' with the minute modification.
# 6. Use the `second<-` function from lubridate package to set the second component of 'USABLE_DATE_TIME' to 59.
# 7. Use 'mutate' to update 'USABLE_DATE_TIME' with the second modification.
dnfs_join_overr__compl__usable <-
  dnfs_join_overr__compl |>
  mutate(USABLE_DATE_TIME =
           TRIP_DATE + days(30)) |>
  mutate(USABLE_DATE_TIME =
           `hour<-`(USABLE_DATE_TIME, 23)) |>
  mutate(USABLE_DATE_TIME =
           `minute<-`(USABLE_DATE_TIME, 59)) |>
  mutate(USABLE_DATE_TIME =
           `second<-`(USABLE_DATE_TIME, 59))

# this checks to see if the code above that creates the variable USABLE_DATE_TIME works, the format of the variable should be: (TRIP_DATE+ 30 days) 23:59:59
dnfs_join_overr__compl__usable |>
  select(TRIP_DATE, USABLE_DATE_TIME) |>
  head(1) |>
  glimpse()
# $ TRIP_DATE        <dttm> 2023-03-27
# $ USABLE_DATE_TIME <dttm> 2023-04-26 23:59:59

### Drop empty columns ----
# to use to drop empty columns, like select(where(not_all_na))
not_all_na <- function(x) any(!is.na(x))

dnfs_join_overr__compl__usable__not_empty <-
  dnfs_join_overr__compl__usable |>
  select(where(not_all_na))

# create a function to produce some stats for the next analysis step, assessing late submission
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

# subtract the usable date from the date of submission
# value is true if the dnf was submitted within 30 days, false if the dnf was not
# The logic should be that when the DE is less than the USABLE_DATE_TIME, the answer to the question MORE_THAN_30_DAYS_LATE should be No.

# Explanations for the function below:
# 1. Use 'mutate' to create a new column named 'MORE_THAN_30_DAYS_LATE'.
# 2. Use 'case_when' to conditionally assign values to the new column based on the comparison of 'DE' and 'USABLE_DATE_TIME'.
# 3. If 'DE' is less than or equal to 'USABLE_DATE_TIME', assign FALSE to 'MORE_THAN_30_DAYS_LATE'.
# 4. If the condition in step 3 is not met (i.e., 'DE' is greater than 'USABLE_DATE_TIME'), assign TRUE to 'MORE_THAN_30_DAYS_LATE'.
late_submission_filter <-
  function(dnf_df) {
    dnf_df__temp <-
      dnf_df |>
      mutate(MORE_THAN_30_DAYS_LATE =
               case_when(DE <= USABLE_DATE_TIME ~ FALSE,
                         .default = TRUE))

    late_submission_filter_stats(dnf_df__temp)

    return(dnf_df__temp)
  }

### Add a column with late submission ----
SEFHIER_processed_dnfs__late_subm <-
  late_submission_filter(dnfs_join_overr__compl__usable__not_empty)
# 2022
# rows: 369816
# columns: 26
# Unique vessels: 1991
# Unique trips: 369667
# ---
# Count late_submission (dnfs num)
# 272217
# ---
# Count late_submission (vessels num)
# 1926

# Add more columns from processed metrics tracking to obtain the Permit region ----
SEFHIER_processed_dnfs__late_subm__metrics <-
  left_join(SEFHIER_processed_dnfs__late_subm,
            processed_metrics_tracking)

# stats
my_stats(SEFHIER_processed_dnfs__late_subm__metrics)
# rows: 369816
# columns: 34
# Unique vessels: 1991
# Unique trips: 369667

# Export usable dnfs ----
# create 3 different dfs by
# a) compliance weeks;
# b) calendar dates;
# c) the whole year including "straddling" weeks

## a) compliance weeks ----

SEFHIER_processed_dnfs__compliance_weeks <-
  SEFHIER_processed_dnfs__late_subm__metrics |>
  dplyr::mutate(
    COMP_START_YEAR = lubridate::isoyear(COMP_WEEK_START_DT),
    COMP_END_YEAR = lubridate::isoyear(COMP_WEEK_END_DT)
  ) |>
  filter(COMP_START_YEAR == my_year &
           COMP_END_YEAR == my_year)

# check
# was:
min(SEFHIER_processed_dnfs__late_subm__metrics$TRIP_DATE)
# [1] "2022-12-26 EST"
# now:
min(SEFHIER_processed_dnfs__compliance_weeks$TRIP_DATE)
# [1] "2023-01-02 EST"
max(SEFHIER_processed_dnfs__compliance_weeks$TRIP_DATE)
# [1] "2023-12-31 EST"

# define file name
SEFHIER_processed_dnfs__compliance_weeks_file_name <-
  str_glue("SEFHIER_processed_dnfs__compliance_weeks_{my_year}.rds")

# write dataframe to file path location, using defined file name
write_rds(
  SEFHIER_processed_dnfs__compliance_weeks,
  file = file.path(output_file_path,
                   SEFHIER_processed_dnfs__compliance_weeks_file_name)
)

## b) calendar dates ----
my_calendar_date_beg <- curr_dates$my_calendar_date_beg
my_calendar_date_end <- curr_dates$my_calendar_date_end

SEFHIER_processed_dnfs__calendar_year <-
  SEFHIER_processed_dnfs__late_subm__metrics |>
  filter(between(
    TRIP_DATE,
    as.Date(my_calendar_date_beg, "%d-%b-%Y",
            tz = Sys.timezone()),
    as.Date(my_calendar_date_end, "%d-%b-%Y",
            tz = Sys.timezone())
  ))

# check
min(SEFHIER_processed_dnfs__calendar_year$TRIP_DATE)
# [1] "2023-01-01 EST"
max(SEFHIER_processed_dnfs__calendar_year$TRIP_DATE)
# [1] "2023-12-31 EST"

SEFHIER_processed_dnfs__calendar_year_file_name <-
  str_glue("SEFHIER_processed_dnfs_calendar_{my_year}.rds")

write_rds(
  SEFHIER_processed_dnfs__calendar_year,
  file = file.path(
    output_file_path,
    SEFHIER_processed_dnfs__calendar_year_file_name
  )
)

## c) the whole year including "straddling" weeks ----
# define file name
SEFHIER_processed_dnfs_file_name <-
  str_glue("SEFHIER_processed_dnfs_{my_year}.rds")

# write dataframe to file path location, using defined file name
write_rds(
  SEFHIER_processed_dnfs__late_subm__metrics,
  file = file.path(output_file_path,
                   SEFHIER_processed_dnfs_file_name)
)

