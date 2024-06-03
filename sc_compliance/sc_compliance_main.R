# This script fulfills a monthly request from SCDNR for compliance data from FHIER.

# Essentially SC (Eric Hiltz) sends us a list of active SC/SEFHIER permitted vessels each month, which Anna has been using to update the FHIER flag ("SC permitted") in the FHIER website. Eric will still be sending us that same list of vessels each month but with added columns that now reflect whether the vessel is compliant or not.
# In addition to using the list to update FHIER, we will use this code to do two things:

# 1) Vessels that are non-compliant in SC and compliant in FHIER.
# Read in this file of SC/SEFHIER vessel IDs for the given month (e.g. March 2024) and then pull out all vessels marked as "1" (NON-COMPLIANT) - herein, "SC non-compliant vessels list". Then with that SC non-compliant vessel list, this code pulls all those vessels that are COMPLIANT in the FHIER compliance table and creates an output file that consists of a check that we will send back to Eric.
# Add logbooks and dnfs for FHIER compliant weeks for the SC non-compliant month.

# 2) Vessels that are compliant in SC and non-compliant in FHIER.
# Grab the compliant vessels (herein "SC compliant vessels list"), and then check FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

# So, in the output file we have: (sheet 1) the list of those SC NON-COMPLIANT vessels that are COMPLIANT in FHIER,
# with (on sheets 2-3) the list of all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month).
# (sheet 4) is the list of those SC COMPLIANT vessels that are NON-COMPLIANT in FHIER.


# Needed files (6):
#   1) "scdnrFedVessels_04012024.xlsx" (South Carolina compliance, instead of "04012024" there will be the date of the latest file)
          # this file comes from Eric Hilts at SCDNR
#   2) "Raw_Oracle_Downloaded_compliance_2021_plus.rds"
          # this file is downloaded from the Oracle db or read in below
#   3) "SEFHIER_processed_dnfs_{my_year}.rds"
          # this file comes from running the DNF processing code for a given year, it should be available to download on Google Drive
#   4) "SEFHIER_processed_Logbooks_{my_year}.rds"
          # this file comes from running the logbook processing code for a given year, it should be available to download on Google Drive
#   5) "Vessel_List_{my_year}.csv" (Southeast Region Headboat Survey, SRHS)
          # this file comes from Ken Brennan at the SRHS program, it should be available to download on Google Drive
#   6) “column_definitions.csv” (csv file of column definitions written by Michelle, used for the ReadMe tab of the output Excel spreadsheet, it should be available to download on Google Drive)

# set up ----
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions@development")
                         # ,
                         # force = T)
library(auxfunctions)

# assign dates to variables,
my_year <- "2024" # the year of the analysis
db_year_1 <- "2023" # set the range based on how many years of data you want to pull, ex. the year before the year of the analysis
db_year_2 <- "2024" # set the range based on how many years of data you want to pull, ex. the year after the year of the analysis

annas_path <- set_work_dir()

# set the current project directory name to the directory that has this R script in it
current_project_dir_name <- this.path::this.dir()

# set the current project base name to the name of the directory that has this R script in it
current_project_basename <-
  basename(current_project_dir_name)

# set the path to processed logbook data on Anna’s computer
annas_processed_data_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)"

# set the path to SC vessels data on Anna’s computer
annas_sc_mismatch_file_path <-
  file.path(annas_path$inputs,
            r"(sc_mismatches\2024_06)",
            "scdnrFedVessels_05312024.xlsx")

# check that the file exists
file.exists(annas_sc_mismatch_file_path)

# set the path to SRHS data on Anna’s computer
annas_srhs_2024_file_path <-
  file.path(annas_path$inputs,
            "SRHS_headboat_survey",
            stringr::str_glue("Vessel_List_{my_year}.csv"))

# check that the file exists
file.exists(annas_srhs_2024_file_path)

## Add correct paths for your environment in the next 5 lines ----

input_path <- file.path(annas_path$inputs, current_project_basename)
output_path <- file.path(annas_path$outputs, current_project_basename)
processed_data_path <- annas_processed_data_path
sc_file_path <- annas_sc_mismatch_file_path
srhs_2024_file_path <- annas_srhs_2024_file_path

