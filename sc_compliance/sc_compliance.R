# We have a request that is going to require some new coding, but it shouldn't be too difficult. Essentially SC (Eric Hiltz) sends us a list of active SC/SEFHIER permitted vessels each month, which Anna has been using to update the FHIER flag ("SC permitted"). Eric will still be sending us that same list of vessels each month but with added columns that now reflect whether the vessel is compliant or not (0 or 1, respectively). Note, we still need to use the list to update FHIER but we need to add a couple new items to the task.
#
# So, in addition to updating FHIER, we need to create code that can do two things:
#
# read in this file of SC/SEFHIER vessel IDs for the given month (e.g. March 2024) and then pull out all vessels marked as "1" (non-compliant) - herein, "SC non-compliant vessels list". Then with that SC non-compliant vessel list we need to join//find all those vessels that are in the FHIER compliance table and create an output file that consists of a check that we will send back to Eric. In the check/output file we need, (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.
# If you have time or would like to attempt writing this code let me know ASAP. Ideally, I would like to have this operational by the next time Eric sends his list (which is usually the beginning of every month). I can start writing the code this week, if neither of you has time.

# set up ----
db_year_1 <- "2023"
db_year_2 <- "2024"

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

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

# Download Maintenance / SC Vessels Reporting via VESL
# from FHIER
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

