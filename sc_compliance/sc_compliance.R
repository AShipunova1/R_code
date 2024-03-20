# We have a request that is going to require some new coding, but it shouldn't be too difficult. Essentially SC (Eric Hiltz) sends us a list of active SC/SEFHIER permitted vessels each month, which Anna has been using to update the FHIER flag ("SC permitted"). Eric will still be sending us that same list of vessels each month but with added columns that now reflect whether the vessel is compliant or not (0 or 1, respectively). Note, we still need to use the list to update FHIER but we need to add a couple new items to the task.
#
# So, in addition to updating FHIER, we need to create code that can do two things:
#
# read in this file of SC/SEFHIER vessel IDs for the given month (e.g. March 2024) and then pull out all vessels marked as "1" (non-compliant) - herein, "SC non-compliant vessels list". Then with that SC non-compliant vessel list we need to join//find all those vessels that are in the FHIER compliance table and create an output file that consists of a check that we will send back to Eric. In the check/output file we need, (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.
# If you have time or would like to attempt writing this code let me know ASAP. Ideally, I would like to have this operational by the next time Eric sends his list (which is usually the beginning of every month). I can start writing the code this week, if neither of you has time.

# set up ----
my_year <- "2024"
my_month <- "2"
db_year_1 <- "2023"
db_year_2 <- "2024"


source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

# get data ----
## Import and prep compliance/override data ----

### Import compliance/override data ----
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs\Raw_Oracle_Downloaded_compliance_2021_plus.rds)")
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
  read_rds_or_run(compl_override_data_file_path,
                  my_function = compl_err_query)

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data__renamed <-
  compl_override_data |>
  dplyr::rename(vessel_official_number =
                  "VESSEL_OFFICIAL_NBR",
                overridden = "IS_COMP_OVERRIDE") |>
  clean_headers()

# change data type of this column if needed
if (!class(compl_override_data__renamed$vessel_official_number) == "character") {
  compl_override_data__renamed$vessel_official_number <-
    as.character(compl_override_data__renamed$vessel_official_number)
}

## Download Maintenance / SC Vessels Reporting via VESL from FHIER ----
# https://grunt.sefsc.noaa.gov/apex/f?p=162:386:5458401387184:::RP,386::&cs=3lR5MlDRVs7tWDLbTPOrYh-j00HYH4yeXtQKl8Dqltvjuxmt6sBAwnah0ltdU_dBPQRSNZ21KX_NR4YGfsjtJOA

csv_names_list = list(r"(sc_mismatches\2024_03\fhier_report_03_01_2024.csv)")

xsl_names_list = list(r"(sc_mismatches\2024_03\scdnrFedVessels_03012024.xlsx)")

SC_vessels_FHIERData_0 <-
  load_csv_names(my_paths$inputs, csv_names_list)[[1]]

SC_vessels_FHIERData <- clean_headers(SC_vessels_FHIERData_0)

# glimpse(SC_vessels_FHIERData)
str(SC_vessels_FHIERData)

# get enabled only
SC_vessels_FHIERData_enabled <-
  SC_vessels_FHIERData |>
  filter(tolower(enabled) == "yes")
# dim(SC_vessels_FHIERData_enabled)
# 199 8
# [1] 187   8
# 189
# 188
# SC_vessels_FHIERData_enabled %>% names()
#   filter(vessel_official_number   == "1225219")

# create new dataframe with just enabled vessel official # for analysis
FHIER_vessel_officialnumber <-
  data.frame(Official_number = tolower(SC_vessels_FHIERData_enabled$vessel_official_number))

dim(FHIER_vessel_officialnumber)
# 188

## read sc permitted data ----
SC_permittedVessels  <-
  load_xls_names(my_paths, xsl_names_list, 1)

glimpse(SC_permittedVessels)
# 200

# for test purposes add random 0 and 1, change to real compliance data from SC
SC_permittedVessels_compl <-
  SC_permittedVessels |>
  mutate(is_compl_sc =
           sample(
             0:1,
             size = nrow(SC_permittedVessels),
             replace = TRUE
           )) |>
  clean_headers()

# View(SC_permittedVessels_compl)

## get logbooks ----
logbooks_path <- file.path(
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)",
  str_glue("SEFHIER_processed_Logbooks_{my_year}.rds")
)

logbooks <-
  read_rds(logbooks_path) |>
  clean_headers()

# View(logbooks)

## get dnfs ----
dnfs_path <- file.path(
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)",
  str_glue("SEFHIER_processed_dnfs_{my_year}.rds")
)

dnfs <-
  read_rds(dnfs_path) |>
  clean_headers()

# View(dnfs)

# combine data ----
print_df_names(SC_permittedVessels_compl)
print_df_names(compl_override_data__renamed)

sc_fhier <-
  left_join(
    SC_permittedVessels_compl,
    compl_override_data__renamed,
    join_by(vessel_reg_uscg_ == vessel_official_number)
  )

# 1. the list of those SC non-compliant vessels that are also non-compliant in FHIER ----

non_compliant_vessels_in_sc_and_fhier <-
  sc_fhier |>
  filter(is_compl_sc == 0 &
           is_comp == 0) |>
  select(vessel_reg_uscg_, vessel_name) |>
  distinct()

# dim(non_compliant_vessels_in_sc_and_fhier)

# 2. non compliant in SC and compliant in FHIER ----
# 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
non_compliant_vessels_in_sc_and_compl_in_fhier <-
  sc_fhier |>
  filter(is_compl_sc == 0 &
           is_comp == 1)

# glimpse(non_compliant_vessels_in_sc_and_compl_in_fhier)

## add logbooks info ----
# Logbook (list any dates for that month)
logbooks__sc_fhier <-
  logbooks |>
  filter(vessel_official_number %in%
           non_compliant_vessels_in_sc_and_compl_in_fhier$vessel_reg_uscg_)

logbooks__sc_fhier_my_month <-
  logbooks__sc_fhier |>
  filter(month(trip_end_date) == my_month) |>
  select(vessel_official_number, trip_start_date, trip_end_date) |>
  distinct()

## add DNF info ----
# DNF (list week date range for any for that month)
dnfs__sc_fhier <-
  dnfs |>
  filter(vessel_official_number %in%
           non_compliant_vessels_in_sc_and_compl_in_fhier$vessel_reg_uscg_)

# Explanations:
# 1. Use 'filter' to keep only rows where the month component of 'trip_date' matches 'my_month'.
# 2. Use 'mutate' to create a new column 'week_start_date_mon' by rounding down 'trip_date' to the start of the week (Monday) using 'floor_date' function.
# 3. Use 'mutate' to create a new column 'week_end_date_mon' by rounding up 'trip_date' to the end of the week (Sunday) using 'ceiling_date' function.
# 4. Use 'select' to keep only the columns 'vessel_official_number', 'week_start_date_mon', and 'week_end_date_mon'.
# 5. Use 'distinct' to keep only unique rows based on the selected columns.
dnfs__sc_fhier_my_month <-
  dnfs__sc_fhier |>
  filter(month(trip_date) == my_month) |>
  mutate(
    week_start_date_mon =
      floor_date(trip_date, unit = 'week', week_start = 1),
    week_end_date_mon =
      ceiling_date(trip_date, unit = 'week', week_start = 1)
  ) |>
  select(vessel_official_number,
         week_start_date_mon,
         week_end_date_mon) |>
  distinct()

# glimpse(dnfs__sc_fhier_my_month)


# 3. SC compliant vessels list ----
# 3) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc_fhier |>
  filter(is_compl_sc == 1 &
           is_comp == 0)

compliant_vessels_in_sc_and_non_compl_fhier__weeks_only <-
  compliant_vessels_in_sc_and_fhier |>
  select(vessel_reg_uscg_, comp_week_start_dt, comp_week_end_dt) |>
  distinct()

# write results to xlsx ----
# (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or
# (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER



output_file_name <-
  file.path(curr_proj_output_path,
            "sc_compliance.xlsx")

result_list <-
  Hmisc::llist(non_compliant_vessels_in_sc_and_fhier,
       logbooks__sc_fhier_my_month,
       dnfs__sc_fhier_my_month,
       compliant_vessels_in_sc_and_non_compl_fhier__weeks_only
       )

View(result_list)

write.xlsx(
  non_compliant_vessels_in_sc_and_fhier,
  output_file_name,
  sheetName = "non_compl_in_both",
  col.names = TRUE,
  row.names = FALSE,
  append = TRUE,
  showNA = TRUE,
  password = NULL
)
