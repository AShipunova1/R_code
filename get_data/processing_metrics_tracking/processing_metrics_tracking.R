# remove SRHS vessels from the Metrics tracking data

# Creates
# "Permitted_vessels_nonSRHS_{my_year}_plusfringedates.rds"

# Steps for Input files
# 1) set date range (use my_date_beg and my_date_end , see below) and download data from Metrics Tracking Detail Report in FHIER as is, no need to filter data or delete any columns
# We use this file to collect the vessel permit information that is included in the table, but the file itself is an output of tallies of types of submitted reports for each vessel for the date range selected, regardless of if the vessel was permitted in that date range.
# 3) manually to download the file
# 2) add year and name it: Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv
# 4) save file to the directory that has this R script, in an “input” sub-directory

# 5) download the SRHS list from Google Drive (comes from Ken Brennan/SRHS branch chief)
# 6) save as "{my_year}SRHSvessels.csv" to the directory that has this R script, in an “input” sub-directory

# Raw data in the Detail Report includes "fringe weeks”. Fringe weeks are defined as
# compliance week 52 of the previous year, and/or compliance week 1 of the next year. Calendar dates of my_year sometimes fall into these extra “fringe weeks” before and after my year, because 365 days do not neatly fit into 52 weeks. We want to include the entire range of compliance weeks that encompass the calendar year, which may include the weeks that are in both the previous and the next compliance year. We will filter the data set by the current year’s compliance and calendar dates in further scripts (DNF and Logbook).

# Compliance weeks are defined in FHIER, and based on ISO week (Mon-Sun) determination. # 'my_date_beg` is calculated as the beginning of the compliance week containing the calendar start date. `my_date_end` is calculated as the end of the compliance week containing the calendar end date.  E.g. The first week that is included in the Detail Report dataset is the week that includes Jan 1st. The last week that is included in the Detail Report dataset is the week that includes Dec 31st. This might include week 52 of the previous, or week 1 of the next year.

# Use get_the_dates(my_year).

# setup ----
library(tidyverse)

# set working and output directory - where do you keep the data and analysis folder on your computer?
michelles_path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Logbook related analyses/Logbook Processing (Do this before all Logbook Analyses)/"

jennys_path <-
  "//ser-fs1/sf/LAPP-DM Documents/Ostroff/SEFHIER/Rcode/ProcessingLogbookData/"
# r"(C:\Users\jenny.ostroff\Desktop\Backups\Rcode\ProcessingMetricsTracking)"

annas_path <-
  r"(C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\processing_logbook_data/)"

# Change to use another path instead:
# Path <- michelles_path
Path <- annas_path

# create these paths in your directory first
Inputs <- "Inputs/"
Outputs <- "Outputs/"

# Set the date ranges for the logbook and compliance data you are pulling
# this is the year to assign to the output file name
# my_year <- '2022'
my_year <- '2023'
# my_year <- '2024'

# ---
# Explanations:
# - This function, `get_the_dates`, generates a list of date strings and date objects based on a specified year and a start day for weeks.
# - It takes two parameters: `my_year`, which defaults to "2023", and `week_start_day`, which defaults to "Monday".
# - The function returns a list (`lst`) of the start and end dates for the calendar year, as well as the beg and end dates for compliance weeks to be included, based on week boundaries. lst is used to keep entries names.
# The beg and end dates include the "fringe" weeks if needed.
#
# 1. **Generating Calendar Dates**:
#     - The function first creates two strings representing the start and end dates of the calendar year based on the provided `my_year` parameter.
#     - `my_calendar_date_beg` is set to "01-JAN-{my_year}" and `my_calendar_date_end` is set to "31-DEC-{my_year}" using string interpolation (`str_glue`).
#
# 2. **Calculating Beg and End Date Boundaries**:
#     - The function calculates the compliance week start and end dates based on the provided `week_start_day` option.
#     - It uses `lubridate` functions to convert the calendar date strings to date objects (`dmy`) and then adjust them to the nearest week boundaries.
#     - `my_date_beg` is calculated as the beginning of the compliance week containing the calendar start date.
#     - `my_date_end` is calculated as the end of the compliance week containing the calendar end date.
#     - The `getOption` function is used to ensure the start of the week is set according to `week_start_day`.
#
# 3. **Creating the List of Dates**:
#     - The function combines the four calculated dates (`my_calendar_date_beg`, `my_calendar_date_end`, `my_date_beg`, and `my_date_end`) into a list using the `lst` function.
#
week_start_day = "Monday"

get_the_dates <-
  function(my_year = "2023",
           week_start_day = "Monday") {
    my_calendar_date_beg <- str_glue("01-JAN-{my_year}")
    my_calendar_date_end <- str_glue("31-DEC-{my_year}")
    my_date_beg <-
      dmy(my_calendar_date_beg) |>
      floor_date('weeks', week_start = getOption("lubridate.week.start", week_start_day))
    my_date_end <-
      dmy(my_calendar_date_end) |>
      ceiling_date('weeks',
                   week_start = getOption("lubridate.week.start", week_start_day)) - 1

    my_dates <- lst(
      my_calendar_date_beg,
      my_calendar_date_end,
      my_date_beg,
      my_date_end
    )

    return(my_dates)
  }

curr_dates <- get_the_dates(my_year)
my_date_beg <- curr_dates$my_date_beg
my_date_end <- curr_dates$my_date_end

# 'my_date_beg’ and 'my_date_end’ represent the maximum and minimum compliance weeks #that  my_year falls into. This may include week 52 of the previous year, and/or week 1 of the #next year, because 365 days do not neatly fit into 52 weeks. These values are the bookends #for the entire range of compliance weeks that encompass the current year.

# — This section produces a log output file that details the variables defined here
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

# Start the log ----
my_tee(date(),
       my_title = str_glue("Start metrics tracking processing for {my_year}"))

# import the permit data
SEFHIER_metrics_tracking_path <-
  file.path(
  Path,
  Inputs,
  paste0(
    "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_",
    my_year,
    ".csv"
  )
)

# read in metrics tracking data
SEFHIER_metrics_tracking <- read.csv(SEFHIER_metrics_tracking_path)

# rename column headers
SEFHIER_metrics_tracking <-
  SEFHIER_metrics_tracking |>
  rename(PERMIT_REGION = `Permit.Grouping.Region`,
         VESSEL_OFFICIAL_NUMBER = `Vessel.Official.Number`)

# import the list of SRHS vessels
# this is a single spreadsheet with all vessels listed, as opposed to the version where they are separated by region (bothregions_asSheets)
SRHS_vessels <-
  read_csv(file.path(Path, Inputs, paste0(my_year, "SRHSvessels.csv")))

# Rename and reformat column
SRHS_vessels <-
  rename(SRHS_vessels,
         VESSEL_OFFICIAL_NUMBER = "USCG #")

# if the class is not character, change it to character
if (!class(SRHS_vessels$VESSEL_OFFICIAL_NUMBER) == "character") {
  SRHS_vessels$VESSEL_OFFICIAL_NUMBER <-
    as.character(SRHS_vessels$VESSEL_OFFICIAL_NUMBER)
}

# stats
my_stats(SEFHIER_metrics_tracking,
         title_msg = "SEFHIER_metrics_tracking")

# Filter: remove SRHS_vessels from SEFHIER_metrics_tracking list
SEFHIER_permit_info <-
  anti_join(SEFHIER_metrics_tracking,
            SRHS_vessels,
            by = 'VESSEL_OFFICIAL_NUMBER')

# Add permit_region column ----
processed_metrics_permit_info <-
  SEFHIER_permit_info |>
  mutate(
    permit_sa_gom_dual =
      case_when(
        SA.Permits. == "Y" &
          GOM.Permits. == "N" ~ "sa_only",
        SA.Permits. == "N" &
          GOM.Permits. == "Y" ~ "gom_only",
        SA.Permits. == "Y" &
          GOM.Permits. == "Y" ~ "dual",
        .default = "unknown"
      )
  )

# check
# processed_metrics_permit_info |> filter(permit_sa_gom_dual == "unknown")
# 0

processed_metrics_permit_info |>
  count(permit_sa_gom_dual)
#   permit_sa_gom_dual    n
# 2022
# 1               dual  277
# 2           gom_only  980
# 3            sa_only 2212

# 2023
# 1               dual  251
# 2           gom_only  987
# 3            sa_only 2149

# stats
my_stats(processed_metrics_permit_info, "Metrics tracking minus SRHS vsls")

# see all column names
processed_metrics_permit_info |> names() |> cat(sep = ", ")

# remove the columns you don't need and rename the rest
processed_metrics_permit_info_short <-
  processed_metrics_permit_info |>
  select(-starts_with("Total")) |>
  rename_all(function(x) {
    gsub("\\.", "_", x) |>
      toupper()
  })



# change the format of the date for these two columns
processed_metrics_permit_info_short <-
  processed_metrics_permit_info_short |>
  mutate(EFFECTIVE_DATE =
           as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
         END_DATE =
           as.Date(END_DATE, "%m/%d/%Y")
  )

# filter data to only include permits with an effective and end date within the current year
# this step is necessary because the original data frame downloaded from FHIER includes some vessels that were not permitted in my_year, and we need to remove them
processed_metrics_permit_info_short_this_year <-
  processed_metrics_permit_info_short |>
  filter(
    EFFECTIVE_DATE <= as.Date(my_date_end, "%d-%b-%Y") &
      END_DATE >= as.Date(my_date_beg, "%d-%b-%Y")
  )

## Check vessels removed by dates ----
not_my_year_vessels <-
  setdiff(
    processed_metrics_permit_info_short$VESSEL_OFFICIAL_NUMBER,
    processed_metrics_permit_info_short_this_year$VESSEL_OFFICIAL_NUMBER
  )

 # this is a check to make sure that all the vessels removed from the dataframe did not submit any reports in my_year, which makes sense since they were not permitted during “my_year”
processed_metrics_permit_info |>
  filter(VESSEL_OFFICIAL_NUMBER %in% not_my_year_vessels) |>
  filter(VESSEL_OFFICIAL_NUMBER %in% not_my_year_vessels) |>
  select(all_of(starts_with("total"))) |>
  distinct() |>
  glimpse()
# 0 - OK, all removed vessels have no "total" information

# stats
my_stats(processed_metrics_permit_info_short)
my_stats(processed_metrics_permit_info_short_this_year)

# Save to a file ----
all_metrics_tracking_vessels_path <-
  file.path(Path,
            Outputs,
            str_glue("Permitted_vessels_nonSRHS_{my_year}_plusfringedates.rds"))

write_rds(processed_metrics_permit_info_short_this_year,
          all_metrics_tracking_vessels_path)

# test <- read_rds(all_metrics_tracking_vessels_path)
# View(test)

