# processing_logbook_data

# This code processes logbook data from Oracle database,
# then cleans it up, so that we can use it in any logbook data analysis:
# (1) (a) pull all logbook and compliance/override data from Oracle database
#     (b) get Metrics Tracking from FHIER
#     (c) get SRHS list from Michelle
# (2) clean up logbook data set
#   (a) remove records from SRHS vessels
#   (b) remove records where start date/time is after end date/time
#   (c) remove records for trips lasting more than 10 days
# (3) remove all trips that were received > 30 days after trip end date, by using compliance data and time of submission
#   (a) remove all overridden data, because the submission date is unknown
# (4) Add permit region information (GOM, SA, or dual), using permit names

# We don't keep trips started in 2021 and ended in 2022.
# We only keep trips starting in 2022.

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
michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Logbook Processing (Do this before all Logbook Analyses)/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingLogbookData/"

annas_path <-
  r"(C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\processing_logbook_data/)"

# Change to use another path instead:
Path <- annas_path

Inputs <- "Inputs/"
Outputs <- "Outputs/"

# Set the date ranges for the logbook and compliance data you are pulling
my_date_beg <- '01-JAN-2022'
my_date_end <- '31-DEC-2022'

# Auxiliary methods ----

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
        dbDriver("Oracle"),  # Use the Oracle database driver.
        username = my_username,  # Use the retrieved username.
        password = keyring::key_get("SECPR", my_username),  # Retrieve the password from the keyring.
        dbname = "SECPR"  # Specify the name of the database as "SECPR."
    )

    # Return the established database connection.
    return(con)
}

# ---
# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg),
      sep = "\n")
}

# ---
# A function to print out stats.
# Usage: my_stat(Logbooks)

my_stat <- function(my_df, title_msg = NA) {
  if (is.na(title_msg))  {
    df_name = deparse(substitute(my_df))
    title_msg <- df_name
  }

  title_message_print(title_msg)

  str_glue("rows: {dim(my_df)[[1]]}
           columns: {dim(my_df)[[2]]}") |>
    print()

  uniq_vessel_num <-
    my_df |>
    select(VESSEL_OFFICIAL_NUMBER) |>
    distinct() |>
    dim()

  print(str_glue("Unique vessels: {uniq_vessel_num[[1]]}"))
}

#
# Explanation:
#
# 1. **Define Function with Optional Parameter:**
#    - `my_stat <- function(my_df, title_msg = NA) { ... }`: Define a function named 'my_stat' that takes a dataframe 'my_df' as input and an optional 'title_msg' parameter with a default value of NA.
#
# 2. **Check and Assign Default Title Message:**
#    - `if (is.na(title_msg))  { ... }`: Check if 'title_msg' is NA, and if so, assign the dataframe name as the default title message using 'deparse(substitute(my_df))'.
#
# 3. **Print Title Message:**
#    - `cat(title_msg, sep = "\n")`: Print the title message using 'cat', separating lines with a newline character.
#
# 4. **Print Rows and Columns Information:**
#    - `str_glue("rows: {dim(my_df)[[1]]} columns: {dim(my_df)[[2]]}") |>
#     print()`: Use 'str_glue' to interpolate the number of rows and columns in the dataframe and print the result.
#
# 5. **Extract Unique Vessel Numbers:**
#    - `uniq_vessel_num <- my_df |> select(VESSEL_OFFICIAL_NUMBER) |> distinct() |> dim()`: Use the pipe operator '|>' to extract unique vessel numbers by selecting the 'VESSEL_OFFICIAL_NUMBER' column, applying 'distinct' to get unique values, and then using 'dim' to count them.
#
# 6. **Print Count of Unique Vessels:**
#    - `print(str_glue("Unique vessels: {uniq_vessel_num[[1]]}"))`: Use 'str_glue' to interpolate and print the count of unique vessels extracted in the previous step.

# ---
# A function to use every time we want to read a ready file or query the database if no files exist.

# The read_rds_or_run_query function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run_query <- function(my_file_path,
                                  my_query,
                                  force_from_db = NULL) {

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

      # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result. I.e. download data from the Oracle database. Must be on VPN. Must have a connection (`con`) already established.

      my_result <- dbGetQuery(con, my_query)

      tictoc::toc()  # Stop timing the operation.

      # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
      # try is a wrapper to run an expression that might fail and allow the user's code to handle error-recovery.

      # 4. Print this message.
      function_message_print(c("Saving new data into a file here: ",
                       my_file_path))

      try(readr::write_rds(my_result,
                           my_file_path))
    }

    # Return the generated or read data.
    return(my_result)
}

#---
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
            "Compliance_raw_data_Year.rds")

# 2) Create a variable with a table name to call data from, define year.
# >= 2020 because of when the program started
compl_err_query <-
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '2020'"

# Check if the file path is correct, optional
# file.exists(compl_override_data_file_path)

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

compl_override_data <-
  read_rds_or_run_query(compl_override_data_file_path,
                        compl_err_query)

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data <-
  compl_override_data |>
  dplyr::rename(VESSEL_OFFICIAL_NUMBER =
                  "VESSEL_OFFICIAL_NBR",
                OVERRIDDEN = "IS_COMP_OVERRIDE")

# stat
my_stat(compl_override_data)
# rows: 458071
# columns: 19
# Unique vessels: 4393

# stat
min(compl_override_data$COMP_WEEK_START_DT)
# [1] "2021-01-04 EST"

# keep only year 2022, including the week 52 of the previous year
Override_data_this_year <-
  compl_override_data %>%
  filter(COMP_WEEK_END_DT >= as.Date(my_date_beg, "%d-%b-%Y") &
    COMP_WEEK_START_DT <= as.Date(my_date_end, "%d-%b-%Y"))

# check
min(Override_data_this_year$COMP_WEEK_START_DT)
# [1] "2021-12-27 EST"
min(Override_data_this_year$COMP_WEEK_END_DT)
# [1] "2022-01-02 EST"

# change data type of this column if needed
if (!class(compl_override_data$VESSEL_OFFICIAL_NUMBER) == "character") {
  compl_override_data$VESSEL_OFFICIAL_NUMBER <-
    as.character(compl_override_data$VESSEL_OFFICIAL_NUMBER)
}

## Import and prep the permit data ----
# use Metrics Tracking report from FHIER
# remove SRHS vessels from the list

# import the permit data
SEFHIER_metrics_tracking <- read.csv(
  paste(
    Path,
    Inputs,
    "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_2022.csv",
    sep = ""
  )
) # here I am using paste to combine the path name with the file, sep is used to say there are no breaks "" (or if breaks " ") in the paste/combining

# rename column headers
SEFHIER_metrics_tracking <-
  SEFHIER_metrics_tracking %>%
  rename(PERMIT_REGION = `Permit.Grouping.Region`,
         VESSEL_OFFICIAL_NUMBER = `Vessel.Official.Number`)

# import the list of SRHS vessels
# this is a single spreadsheet with all vessels listed, as opposed to the version where they are separated by region (bothregions_asSheets)
SRHS_vessels <-
  read_csv(paste(Path, Inputs, "2022SRHSvessels.csv", sep = ""))

# Rename and reformat column
SRHS_vessels <-
  rename(SRHS_vessels,
       VESSEL_OFFICIAL_NUMBER = "USCG #")

if (!class(SRHS_vessels$VESSEL_OFFICIAL_NUMBER) == "character") {
  SRHS_vessels$VESSEL_OFFICIAL_NUMBER <-
    as.character(SRHS_vessels$VESSEL_OFFICIAL_NUMBER)
}

# stat
my_stat(SEFHIER_metrics_tracking,
        title_msg = "SEFHIER_metrics_tracking")
# rows: 3598
# columns: 13
# Unique vessels: 3598

# Filter: remove SRHS_vessels from SEFHIER_metrics_tracking list
SEFHIER_permit_Info <-
  anti_join(SEFHIER_metrics_tracking, SRHS_vessels,
            by = 'VESSEL_OFFICIAL_NUMBER')

# stat
my_stat(SEFHIER_permit_Info, "Metrics tracking minus SRHS vsls")
# rows: 3469
# columns: 13
# Unique vessels: 3469

# remove the columns you don't need and keep only 2
SEFHIER_permit_Info <-
  SEFHIER_permit_Info |>
  select(VESSEL_OFFICIAL_NUMBER,
         PERMIT_REGION)

# stat, not needed for processing
my_stat(SEFHIER_permit_Info)
# rows: 3469
# columns: 2
# Unique vessels: 3469

## Import and prep the logbook data ####

# Import the logbook data from file or database
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) create a variable with file path to read or write the logbook file

logbooks_file_path <-
  paste(Path,
        Outputs,
        "SAFIS_TripsDownload_1.1.22-12.01.23.rds",
        sep = "")
# here I am using paste to combine the path name with the file, sep is used to say there are no breaks "" (or if breaks " ") in the paste/combining

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

# Rename column to be consistent with other dataframes
Logbooks <-
  rename(Logbooks,
         VESSEL_OFFICIAL_NUMBER =
           "VESSEL_OFFICIAL_NBR")

# stat
my_stat(Logbooks, "Logbooks from the db")
# rows: 484413
# columns: 149
# Unique vessels: 2218

# reformat trip start/end date
Logbooks <-
  Logbooks |>
  mutate(across(c(!where(is.Date) & ends_with("_DATE")),
                as.Date))

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

Logbooks <-
  Logbooks |>
  mutate(across(c(!where(is.numeric) & all_of(time_col_names)),
                as.numeric)) |>
  mutate(across(all_of(time_col_names),
         ~ sprintf("%04d", .x)))

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

### Filter out just 2022 logbook entries ----

# check
# min(Logbooks$TRIP_START_DATE)
# [1] "2022-01-01"
# max(Logbooks$TRIP_START_DATE)
# [1] "2023-12-01"

Logbooks <-
  Logbooks %>%
  filter(TRIP_START_DATE >= as.Date(my_date_beg, "%d-%b-%Y") &
           TRIP_START_DATE <= as.Date(my_date_end, "%d-%b-%Y"))

# stat, to compare with the end result
logbooks_stat_correct_dates_before_filtering <-
  c(dim(Logbooks),
    length(unique(Logbooks$VESSEL_OFFICIAL_NUMBER)))

my_stat(Logbooks, "Logbooks after filtering by dates")
# rows: 327773
# columns: 149
# Unique vessels: 1882

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

### Prepare data to determine what weeks were overridden, and exclude those logbooks ----

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
  Logbooks %>%
  mutate(COMP_WEEK = isoweek(TRIP_END_DATE), # puts it in week num
         TRIP_END_YEAR = isoyear(TRIP_END_DATE)) # adds a year

# to see the respective data in compl_override_data
# not needed for processing
# compl_override_data |>
#   select(COMP_YEAR,
#          COMP_WEEK_END_DT,
#          COMP_WEEK) |>
#   distinct() |>
#   arrange(COMP_WEEK_END_DT) |>
#   head(3)
#   COMP_YEAR COMP_WEEK_END_DT COMP_WEEK
# 1      2022       2022-01-09         1
# 2      2022       2022-01-16         2
# 3      2022       2022-01-23         3

## add override data to logbooks ----

my_stat(compl_override_data,
        "Compl/override data from the db")
# rows: 458071
# columns: 19
# Unique vessels: 4393

SEFHIER_logbooks_join_overr <-
  left_join(Logbooks,
            compl_override_data,
            join_by(TRIP_END_YEAR == COMP_YEAR,
                    VESSEL_OFFICIAL_NUMBER,
                    COMP_WEEK),
            relationship = "many-to-many"
            )

# stat
my_stat(SEFHIER_logbooks_join_overr)
# rows: 327818
# columns: 169
# Unique vessels: 1882

# Make lists of overridden or not vessels
# If a week for a vessel was overridden (compl_override_data), remove the trip reports from the corresponding week in the logbook data
# We have to remove logbooks for weeks that were overridden because we don't have a timestamp for when the logbook was submitted to the app, only when it was submitted to Oracle/SAFIS, and we can't differentiate that time laps.
# We can't differentiate between turning a logbook in on time in the app, and it taking two months to get it vs turning in a logbook two months late.
# E.g. user submitted Jan 1, 2022, but SEFHIER team found it missing in FHIER (and SAFIS) in March, 2022 (At permit renewal)... user submitted on time in app (VESL) but we may not get that report in SAFIS for months later (when its found as a "missing report" and then requeued for transmission)

SEFHIER_logbooks_overridden <-
  filter(SEFHIER_logbooks_join_overr, OVERRIDDEN == 1) #data frame of logbooks that were overridden

# stat
my_stat(SEFHIER_logbooks_overridden)
# rows: 10136
# columns: 169
# Unique vessels: 286

SEFHIER_logbooks_notoverridden <-
  filter(SEFHIER_logbooks_join_overr, OVERRIDDEN == 0) #data frame of logbooks that weren't overridden

# stat
my_stat(SEFHIER_logbooks_notoverridden)
# rows: 317029
# columns: 169
# Unique vessels: 1870

SEFHIER_logbooks_NA <-
  filter(SEFHIER_logbooks_join_overr, is.na(OVERRIDDEN)) #logbooks with an Overridden value of NA, because they were
# 1) submitted by a vessel that is missing from the Compliance report and therefore has no associated override data, or
# 2) submitted by a vessel during a period in which the permit was inactive, and the report was not required

# stat
my_stat(SEFHIER_logbooks_NA)
# rows: 653
# columns: 169
# Unique vessels: 10

## Add vessels missing from the Compliance report ----
# SEFHIER vessels missing from the Compliance report
SEFHIER_vessels_missing <-
  anti_join(SEFHIER_permit_Info,
            compl_override_data,
            by = 'VESSEL_OFFICIAL_NUMBER') |>
  select(VESSEL_OFFICIAL_NUMBER) |>
  distinct()

# stat
my_stat(SEFHIER_vessels_missing)
# Unique vessels: 25

# SEFHIER logbooks from vessels missing from the Compliance report
SEFHIER_vessels_missing_logbooks <-
  SEFHIER_logbooks_NA |>
  filter(VESSEL_OFFICIAL_NUMBER %in% SEFHIER_vessels_missing)

# add missing logbooks back to the not overridden data frame
SEFHIER_logbooks_notoverridden <-
  rbind(SEFHIER_logbooks_notoverridden,
        SEFHIER_vessels_missing_logbooks)

# remove missing logbooks from NA dataset, the NA dataset is now only those that were submitted when not needed
SEFHIER_logbooks_NA <-
    SEFHIER_logbooks_NA |>
  filter(!VESSEL_OFFICIAL_NUMBER %in% SEFHIER_vessels_missing)

my_stat(SEFHIER_logbooks_NA,
        "SEFHIER_logbooks_NA after remove missing logbooks")

# We have decided to throw out logbooks that were submitted when the permit was inactive, the logic
# being we shouldn't include logbooks that weren't required in the first place. Alternatively,
# deciding to keep in the NAs means we would be keeping reports that were submitted by a vessel
# during a period in which the permit was inactive, and the report was not required.
# rbind(SEFHIER_logbooks_notoverridden, SEFHIER_logbooks_NA) this is the alternative

# unique list of vessels that submitted logbooks, useful stat, not needed for processing (doesn't work, AS)??
# SEFHIER_logbooks_vessels <-
  # unique(rbind(SEFHIER_logbooks[, 1], SEFHIER_logbooks_overridden[, 1]))
# NumSEFHIER_logbooks_vessels <- nrow(SEFHIER_logbooks_vessels)

## Determine which logbooks were turned in within 30 days, making them usable for analyses ####

# use trip end date to calculate the usable date 30 days later
SEFHIER_logbooks_notoverridden <-
  SEFHIER_logbooks_notoverridden %>%
  mutate(USABLE_DATE =
           format(
             as.Date(SEFHIER_logbooks_notoverridden$TRIP_END_DATE, '%Y-%m-%d') + 30,
             format = "%Y-%m-%d"
           ))

# Append a time to the due date since the submission data has a date and time
add_time <- "23:59:59" # 24 hr clock

SEFHIER_logbooks_notoverridden$USABLE_DATE <-
  as.POSIXct(paste(as.Date(
    SEFHIER_logbooks_notoverridden$USABLE_DATE, '%Y-%m-%d'
  ),
  add_time),
  format = "%Y-%m-%d %H:%M:%S")

#format the submission date (TRIP_DE)
SEFHIER_logbooks_notoverridden <-
  SEFHIER_logbooks_notoverridden |>
  mutate(TRIP_DE = as.POSIXct(TRIP_DE, format = "%Y-%m-%d %H:%M:%S"))

# NumVessels_unusablelogbooks <-
  # length(unique(SEFHIER_logbooks_unusable$VESSEL_OFFICIAL_NUMBER)) #how many vessels had an unusable logbook?
# 1053
# 430

# Filtering logbook data ----

## Filter vessels: Keep only vessels in Metricks tracking ----
# Revise that section after deciding on the permit info source.
SEFHIER_logbooks_notoverridden__in_metr <-
  SEFHIER_logbooks_notoverridden |>
  filter(VESSEL_OFFICIAL_NUMBER %in% SEFHIER_permit_Info$VESSEL_OFFICIAL_NUMBER)

# stat
my_stat(SEFHIER_logbooks_notoverridden,
        "SEFHIER_logbooks_notoverridden before filtered by Metrics tracking and SRHS list")
# rows: 317029
# columns: 170
# Unique vessels: 1870

my_stat(SEFHIER_logbooks_notoverridden__in_metr)
# rows: 312293
# columns: 170
# Unique vessels: 1829

# thrown away in this step
SEFHIER_logbooks_notoverridden |>
  filter(!VESSEL_OFFICIAL_NUMBER %in% SEFHIER_permit_Info$VESSEL_OFFICIAL_NUMBER) |>
  my_stat("Thrown away by 'not in Metrics tracking'")
# rows: 4736
# columns: 170
# Unique vessels: 41

# NB. The rest is removing logbooks, not necessary vessels.

## Start date/time is after end date/time ----
# check logbook records for cases where start date/time is after end date/time, delete these records

# the Time Stamp Error is true if start date/time is greater than or equal to end date/time, false if not
SEFHIER_logbooks_notoverridden__in_metr['time_stamp_error'] <-
  ifelse(
    SEFHIER_logbooks_notoverridden__in_metr$STARTDATETIME >= SEFHIER_logbooks_notoverridden__in_metr$ENDDATETIME,
    "true",
    "false"
  )

### Filter: only keep the rows where there is no error between start & end date & time ----
SEFHIER_logbooks_notoverridden__in_metr__start_end_ok <-
  SEFHIER_logbooks_notoverridden__in_metr %>%
  filter(time_stamp_error == "false")

# stat
my_stat(SEFHIER_logbooks_notoverridden__in_metr__start_end_ok)
# rows: 310464
# columns: 171
# Unique vessels: 1825

thrown_by_time_stamp_error <-
  SEFHIER_logbooks_notoverridden__in_metr %>%
  filter(time_stamp_error == "true") |>
  select(TRIP_ID) |>
  distinct() |>
  nrow()

title_message_print("Thrown away by time_stamp_error (logbooks)")
print(thrown_by_time_stamp_error)
# rows: 1829
# trip ids: 551

## Delete logbooks for trips lasting more than 10 days ----

# The assumption is there is an error in either start or end date and time and the trip didn't really last that long.

SEFHIER_logbooks_notoverridden__in_metr__start_end_ok['TripLength'] <-
  as.numeric(
    difftime(
      SEFHIER_logbooks_notoverridden__in_metr__start_end_ok$ENDDATETIME,
      SEFHIER_logbooks_notoverridden__in_metr__start_end_ok$STARTDATETIME,
      units = "hours"
    )
  )

# Output trips with length > 240 into data frame
# LogbooksTooLong = Logbooks %>% filter(TripLength > 240) # useful stat, not needed for processing
# NumLogbooksTooLong = nrow(LogbooksTooLong) #useful stat, not needed for processing
# 74

### Filter: only keep trips with a length less than or equal to 10 days (240 hours) ----

SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok <-
  SEFHIER_logbooks_notoverridden__in_metr__start_end_ok %>%
  filter(TripLength <= 240)

# stat
my_stat(SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok)
# rows: 310353
# columns: 172
# Unique vessels: 1821

title_message_print("Thrown away by trip_more_10_days (logbooks num)")
trip_more_10_days <-
  SEFHIER_logbooks_notoverridden__in_metr__start_end_ok %>%
  filter(TripLength > 240) |>
  select(TRIP_ID) |>
  distinct() |>
  nrow()

print(trip_more_10_days)
# rows: 111
# trip_ids: 44

# subsets the data with no logbook entries, useful stat, not needed for processing
# That is mostly for compliance analysis
# VesselsNoLogbooks <-
#   subset(SEFHIER_logbooks, (SEFHIER_logbooks$TRIP_TYPE %in% c(NA)))

# nrow(VesselsNoLogbooks)
# 1636

## Remove all trips that were received > 30 days after trip end date, by using compliance data and time of submission ----

# subtract the usable date from the date of submission
# value is true if the logbook was submitted within 30 days, false if the logbook was not
SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok['USABLE'] <-
  ifelse(
    SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok$USABLE_DATE >= SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok$TRIP_DE,
    "true",
    "false"
  )

SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok1 <-
  SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok |>
  mutate(USABLE =
           ifelse(USABLE_DATE >= TRIP_DE, TRUE, FALSE))

# diffdf::diffdf(SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok,
#           SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok1)
# "Component “USABLE”: target is character, current is logical"
#?? the same result, which is easier to read?

### Filter: data frame of logbooks that were usable ----
SEFHIER_logbooks_usable <-
  SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok %>%
  filter(USABLE == "true")

# stat
my_stat(SEFHIER_logbooks_usable)
# rows: 271101
# columns: 173
# Unique vessels: 1628

title_message_print("Thrown away by late_submission (logbooks)")
late_submission <-
  SEFHIER_logbooks_notoverridden__in_metr__start_end_ok__trip_len_ok %>%
  filter(USABLE == "false") |>
  select(TRIP_ID) |>
  distinct() |>
  nrow()
print(late_submission)
# rows: 39252
# trip_ids: 16447

# check
min(SEFHIER_logbooks_usable$TRIP_START_DATE)
# [1] "2022-01-01"
max(SEFHIER_logbooks_usable$TRIP_START_DATE)
# [1] "2022-12-31"
min(SEFHIER_logbooks_usable$TRIP_END_DATE)
# [1] "2022-01-01"
max(SEFHIER_logbooks_usable$TRIP_END_DATE)
# [1] "2022-12-31"

# Separate permit regions to GOM only, SA only or dual using PERMIT_GROUP ----
# Data example:
# SEFHIER_logbooks_usable %>%
#   select(PERMIT_GROUP) |>
#   distinct() |>
#   tail(3)
# PERMIT_GROUP
# (CDW)CDW, (CHG)1615, (CHS)CHS, (SC)SC
# (CHG)1034, (RCG)982
# (CHG)589, (RCG)567

# Note, PERMIT_REGION column has only GOM or SA

# Auxiliary: how to find the column name
#
# grep("permit",
#      names(SEFHIER_logbooks_usable),
#      value = TRUE,
#      ignore.case = TRUE)

# Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group

# Check if 'permit_group_field_name' doesn't contain 'RCG', 'HRCG', 'CHG', or 'HCHG'; assign "sa_only" if true
# Check if 'permit_group_field_name' doesn't contain 'CDW', 'CHS', or 'SC'; assign "gom_only" if true
# For all other cases, assign "dual"

SEFHIER_logbooks_usable_p_regions <-
  SEFHIER_logbooks_usable %>%
  mutate(
    permit_sa_gom =
      dplyr::case_when(
        !grepl("RCG|HRCG|CHG|HCHG", PERMIT_GROUP) ~
          "sa_only",
        !grepl("CDW|CHS|SC", PERMIT_GROUP) ~ "gom_only",
        .default = "dual"
      )
  )

# Explanation:
#
# 1. **Create New Dataframe:**
#    - `SEFHIER_logbooks_usable_regions <- SEFHIER_logbooks_usable %>% ...`: Create a new dataframe 'SEFHIER_logbooks_usable_regions' based on the 'SEFHIER_logbooks_usable' dataframe.
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

# stat
my_stat(SEFHIER_logbooks_usable_p_regions)

title_message_print("Compare with Logbooks before filtering")

logbooks_before_filtering <-
  n_distinct(Logbooks$TRIP_ID)
# print(logbooks_before_filtering)
# [1] 94714

logbooks_after_filtering <-
  n_distinct(SEFHIER_logbooks_usable_p_regions$TRIP_ID)
# print(logbooks_after_filtering)
# [1] 73270

percent_of_removed_logbooks <-
  (logbooks_before_filtering - logbooks_after_filtering) * 100 / logbooks_before_filtering
# print(percent_of_removed_logbooks)
# [1] 22.64079

# removed_vessels
vessels_before_filtering <-
  n_distinct(Logbooks$VESSEL_OFFICIAL_NUMBER)
# print(vessels_before_filtering)
# [1] 1882

vessels_after_filtering <-
  n_distinct(SEFHIER_logbooks_usable_p_regions$VESSEL_OFFICIAL_NUMBER)
# print(vessels_after_filtering)
# 1628

removed_vessels <-
  vessels_before_filtering - vessels_after_filtering
# 254

percent_of_removed_vessels <-
  (vessels_before_filtering - vessels_after_filtering) * 100 / vessels_before_filtering
# 13.49628 %

cat(
  crayon::blue("percent_of_removed_logbooks"),
  str_glue("{round(percent_of_removed_logbooks)}%"),
  crayon::blue("removed_vessels"),
  removed_vessels,
  crayon::blue("percent_of_removed_vessels"),
  str_glue("{round(percent_of_removed_vessels)}%"),
  sep = "\n"
)

# Export usable logbooks ----
#write.csv(GOMlogbooksAHU_usable, "//ser-fs1/sf/LAPP-DM Documents\\Ostroff\\SEFHIER\\Rcode\\ProcessingLogbookData\\Outputs\\UsableLogbooks2022.csv", row.names=FALSE)
#write.xlsx(GOMlogbooksAHU_usable, 'UsableLogbooks2022.xlsx', sheetName="2022Logbooks", row.names=FALSE)

annas_file_path <-
  file.path(Path, "Outputs", "SEFHIER_usable_Logbooks.rds")

jennys_file_path <-
  paste(Path, Outputs, "SEFHIER_usable_Logbooks.rds",
        sep = "")

# Change to the correct path
output_file_path <-
  annas_file_path

write_rds(
  SEFHIER_logbooks_usable_p_regions,
  file = output_file_path
)
