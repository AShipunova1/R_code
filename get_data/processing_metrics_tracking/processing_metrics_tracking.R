# remove SRHS vessels from the Metrics tracking data

# Creates
# "SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds"

# Steps for Input files
# 1) set date range (use compliance dates, see below) and download data from Metrics Tracking Detail Report in FHIER as is, no need to filter data or delete any columns
# we use this file to collect the vessel permit information that is included in the table, but the file itself is an output of tallies of types of submitted reports for each vessel for the date range selected, regardless of if the vessel was permitted in that date range
# 2) name it: Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv
# 3) add year manually to downloaded file name
# 4) save file to the directory that has this R script, in an “input” sub-directory

# 5) download the SRHS list from Google Drive (comes from Ken Brennan/SRHS branch chief)
# 6) save as "{my_year}SRHSvessels.csv" to the directory that has this R script, in an “input” sub-directory
# Compliance dates include the "fringe" weeks, the weeks which are in both the previous and the next year (if any). Use get_the_dates(my_year).

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
# my_year <- '2023'
my_year <- '2024'

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

curr_dates <- get_the_dates(my_year)
my_compliance_date_beg <- curr_dates$my_compliance_date_beg
my_compliance_date_end <- curr_dates$my_compliance_date_end

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

file.exists(SEFHIER_metrics_tracking_path)

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

# 2024
#   permit_sa_gom_dual    n
# 1               dual  194
# 2           gom_only 1036
# 3            sa_only 1930

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

# filter data to only include permits with an effective and end date within the current compliance year
# this step is necessary because the original data frame downloaded from FHIER includes some vessels that were not permitted in my_year, and we need to remove them
processed_metrics_permit_info_short_this_year <-
  processed_metrics_permit_info_short |>
  filter(
    EFFECTIVE_DATE <= as.Date(my_compliance_date_end, "%d-%b-%Y") &
      END_DATE >= as.Date(my_compliance_date_beg, "%d-%b-%Y")
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
            str_glue("SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds"))

write_rds(processed_metrics_permit_info_short_this_year,
          all_metrics_tracking_vessels_path)

# test <- read_rds(all_metrics_tracking_vessels_path)
# View(test)

