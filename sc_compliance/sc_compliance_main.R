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

## assign dates to variables ----
my_year <- "2024" # the year of the analysis
db_year_1 <- "2023" # set the range based on how many years of data you want to pull, ex. the year before the year of the analysis
db_year_2 <- "2024" # set the range based on how many years of data you want to pull, ex. the year after the year of the analysis

## save common column names ----
common_outpt_fields <-
  c("delinquent",
    "month_sc",
    "year_sc",
    "comp_week_start_dt",
    "comp_week_end_dt")

## set up paths ----
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

get_data_path <- file.path(current_project_dir_name,
                           paste0(current_project_basename, "_get_data.R"))

file.exists(get_data_path)

source(get_data_path)
# res in
# sc__fhier_compl__join_w_month
# sc__fhier_compl__join_w_month_last2

# Answering the questions ----

# save common column names
common_output_fields <-
  c("delinquent",
    "month_sc",
    "year_sc",
    "comp_week_start_dt",
    "comp_week_end_dt")

## 1. Non-compliant in SC and compliant in FHIER ----

# a)
non_compliant_vessels_in_sc_and_compl_in_fhier <-
  sc__fhier_compl__join_w_month |>
  dplyr::filter(delinquent_month == 1 &
           common_month_compliance == "compl")

non_compliant_vessels_in_sc_and_compl_in_fhier_last_2 <-
  sc__fhier_compl__join_w_month_last2 |>
  dplyr::filter(delinquent_month == 1 &
           common_month_compliance == "compl")

dim(non_compliant_vessels_in_sc_and_compl_in_fhier)
# [1] 0 24

# Get month and weeks when the vessels are marked as non-compliant in SC, but are compliant in FHIER
non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output <-
  non_compliant_vessels_in_sc_and_compl_in_fhier |>
  select(
    vessel_reg_uscg_,
    all_of(common_output_fields),
    compliant_after_override
  ) |>
  distinct() |>
  arrange(vessel_reg_uscg_, comp_week_start_dt)

dim(non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output)
# [1] 0  7

# b) list all the dates of DNFs and/or logbooks we have in FHIER by vessel.

## add logbooks info ----
# Logbook (list any dates for that month)
# Get all logbooks info for this vessels filtered by month

dim(non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output)
# 0

intersect(names(logbooks),
          names(non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output))
# [1] "comp_week_start_dt"       "comp_week_end_dt"         "compliant_after_override"

logbooks__sc_fhier <-
  logbooks |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    join_by(
      vessel_official_number == vessel_reg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    )
  )

dim(logbooks__sc_fhier)
# 0

# There is a good example of a trip that happens in Jan (1/30), in the week started in Jan and ended in Feb, hence it is marked as compliant in FHIER for February.
grep("start", names(logbooks__sc_fhier), value = T)

# subset columns of data to output
logbooks__sc_fhier_for_output <-
  logbooks__sc_fhier |>
  select(
    vessel_official_number,
    all_of(common_outpt_fields),
    trip_start_date,
    trip_end_date,
    # vendor_app_name,
    trip_de,
    trip_ue
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_start_date)

glimpse(logbooks__sc_fhier_for_output)
0

## add DNF info ----

# check names
intersect(names(dnfs),
          names(non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output))
# [1] "comp_week_start_dt"       "comp_week_end_dt"         "compliant_after_override"

# DNF (list week date range for any for that month)
dnfs__sc_fhier <-
  dnfs |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    join_by(
      vessel_official_number == vessel_reg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    ),
    suffix = c("__dnf", "__fhier")
  )

# check
dim(dnfs__sc_fhier)
# 0

# subset columns of data to output
dnfs__sc_fhier_for_output <-
  dnfs__sc_fhier |>
  select(
    vessel_official_number,
    all_of(common_outpt_fields),
    trip_date,
    compliant_after_override__fhier
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_date)

dim(dnfs__sc_fhier_for_output)
# 0

# 2. SC compliant and not compliant in FHIER ----

# 2) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 0 &
           common_month_compliance == "non_compl")

dim(compliant_vessels_in_sc_and_non_compl_fhier)
# [1] 1202   24

# "all_m_comp" field shows if any weeks of that month were compliant. We consider the whole month non-compliant if even one week was non-compliant. If SC considers the month compliant if at least one week was compliant that makes a big difference in the monthly compliance counts between SC and FHIER.

# subset columns of data to output
compliant_vessels_in_sc_and_non_compl_fhier__for_output <-
  compliant_vessels_in_sc_and_non_compl_fhier |>
  select(
    vessel_reg_uscg_,
    all_of(common_outpt_fields),
    compliant_after_override
  ) |>
  filter(compliant_after_override == "no") |>
  distinct() |>
  arrange(vessel_reg_uscg_, comp_week_start_dt)

dim(compliant_vessels_in_sc_and_non_compl_fhier__for_output)
# [1] 559   7

# Write results to xlsx ----
# (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or
# (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# 2. we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER

# 1. Create a wb with all output dfs ----

# Explanations:
# Create a list 'output_df_list' containing multiple data frames.
# This list is constructed using the 'lst' function from the tibble package,
# which combines several objects into a list. lst() also generates missing names automatically.

# 1. 'non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output': A data frame of non-compliant vessels in SC but compliant in FHIER.
# 2. 'logbooks__sc_fhier_for_output': A data frame of logbooks data from FHIER.
# 3. 'dnfs__sc_fhier_for_output': A data frame of DNFs data from FHIER.
# 4. 'compliant_vessels_in_sc_and_non_compl_fhier__for_output': A data frame of compliant vessels in SC and non-compliant FHIER.

output_df_list <-
  lst(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w__output,
    logbooks__sc_fhier_for_output,
    dnfs__sc_fhier_for_output,
    compliant_vessels_in_sc_and_non_compl_fhier__for_output
  )

# a simple list of sheet names
sheet_names <-
  list(
    "non_compl_sc__compl_fhier",
    "non_compl_sc__compl_fhier_lgb",
    "non_compl_sc__compl_fhier_dnf",
    "compl_sc__non_compl_fhier"
  )

# Make a copy with different names. output_df_list has dataframe names for column names and print_result_list has sheet names for column names.
print_result_list <-
  output_df_list

names(print_result_list) <- sheet_names

# Explanations:
# Build a workbook using a list of data frames ('print_result_list') and create tables in the workbook.

# 1. 'wb': An object representing the workbook to be created.
# 2. The function 'buildWorkbook()' is used to create a workbook from a list of data frames.
#    It takes the list of data frames ('print_result_list') as input and an option to convert them to tables ('asTable = TRUE').
#    The 'asTable = TRUE' option ensures that each data frame is added to the workbook as a formatted table.

wb <- openxlsx::buildWorkbook(print_result_list, asTable = TRUE)

