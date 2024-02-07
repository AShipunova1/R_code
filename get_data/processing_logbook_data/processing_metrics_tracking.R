# remove SRHS vessels from the Metrics tracking data

# get Metrics Tracking from FHIER
# get SRHS list from Ken Brennan (SRHS branch chief)

# setup ----
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
my_year <- "2022"
my_date_beg <- '01-JAN-2022'
my_date_end <- '31-DEC-2022'

# my_year <- "2023"
# my_date_beg <- '01-JAN-2023'
# my_date_end <- '31-DEC-2023'

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

if (!class(SRHS_vessels$VESSEL_OFFICIAL_NUMBER) == "character") {
  SRHS_vessels$VESSEL_OFFICIAL_NUMBER <-
    as.character(SRHS_vessels$VESSEL_OFFICIAL_NUMBER)
}

# stats
my_stats(SEFHIER_metrics_tracking,
         title_msg = "SEFHIER_metrics_tracking")
# rows: 3598
# columns: 13
# Unique vessels: 3598

# Filter: remove SRHS_vessels from SEFHIER_metrics_tracking list
SEFHIER_permit_info <-
  anti_join(SEFHIER_metrics_tracking,
            SRHS_vessels,
            by = 'VESSEL_OFFICIAL_NUMBER')

# TODO:
# save all SEFHIER_permit_info vessels
# SEFHIER_permitted_vessels_nonSRHS_{year}

# stats
my_stats(SEFHIER_permit_info, "Metrics tracking minus SRHS vsls")
# rows: 3469
# columns: 13
# Unique vessels: 3469

# see all names
SEFHIER_permit_info |> names() |> cat(sep = ", ")

# remove the columns you don't need and rename the rest
SEFHIER_permit_info_short <-
  SEFHIER_permit_info |>
  select(-starts_with("Total")) |>
  rename_all(function(x) {
    gsub("\\.", "_", x) |>
      toupper()
  })

# stats
my_stats(SEFHIER_permit_info)
# rows: 3469
# columns: 8
# Unique vessels: 3469

SEFHIER_permit_info_short <-
  SEFHIER_permit_info_short |>
  mutate(EFFECTIVE_DATE =
           as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
         END_DATE =
           as.Date(END_DATE, "%m/%d/%Y")
  )

SEFHIER_permit_info_short_this_year <-
  SEFHIER_permit_info_short |>
  filter(
    EFFECTIVE_DATE <= as.Date(my_date_end, "%d-%b-%Y") &
      END_DATE >= as.Date(my_date_beg, "%d-%b-%Y")
  )

my_stats(SEFHIER_permit_info_short)
my_stats(SEFHIER_permit_info_short_this_year)
