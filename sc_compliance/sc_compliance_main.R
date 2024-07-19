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
          # this file comes from running the DNF processing code for a given year, it should be available to download on Google Drive or ran from the sourced code below
#   4) "SEFHIER_processed_Logbooks_{my_year}.rds"
          # this file comes from running the logbook processing code for a given year, it should be available to download on Google Drive  or ran from the sourced code below
#   5) "Vessel_List_{my_year}.csv" (Southeast Region Headboat Survey, SRHS)
          # this file comes from Ken Brennan at the SRHS program, it should be available to download on Google Drive
#   6) “column_definitions.csv” (csv file of column definitions written by Michelle, used for the ReadMe tab of the output Excel spreadsheet, it should be available to download on Google Drive)

# set up ----
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions")
library(auxfunctions)

## assign dates to variables ----
my_year <- "2024" # the year of the analysis
db_year_1 <- "2023" # set the range based on how many years of data you want to pull, ex. the year before the year of the analysis
db_year_2 <- "2024" # set the range based on how many years of data you want to pull, ex. the year after the year of the analysis

## save common column names ----
# Define a vector of common output field names used across different parts of the analysis
common_output_fields <-
  c("delinquent",
    "month_sc",
    "year_sc",
    "comp_week_start_dt",
    "comp_week_end_dt")

## set up paths ----
annas_path <- set_work_dir()

# Get the current directory path using the this.path package
current_project_dir_name <- this.path::this.dir()

# set the current project base name to the name of the directory that has this R script in it
current_project_basename <-
  basename(current_project_dir_name)

# set the path to processed logbook data on Anna’s computer
annas_processed_data_path <-
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)"

# Set the date from the SC file name (update this with each new file)
# this number is from the file provided by SC, e.g. "scdnrFedVessels_05312024.xlsx"
# Change it with every new file
sc_file_date <- "06282024"
sc_file_dir <- "2024_07"

annas_sc_mismatch_file_path <-
  file.path(annas_path$inputs,
            "sc_mismatches",
            sc_file_dir,
            stringr::str_glue("scdnrFedVessels_",
            sc_file_date,
            ".xlsx"))

# Verify that the SC mismatch file exists
file.exists(annas_sc_mismatch_file_path)

# set the path to SRHS data on Anna’s computer
annas_srhs_2024_file_path <-
  file.path(annas_path$inputs,
            "SRHS_headboat_survey",
            stringr::str_glue("Vessel_List_{my_year}.csv"))

# Verify that the SRHS file exists
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

# Source the get_data script to load and process the required data
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
    all_of(common_output_fields),
    trip_start_date,
    trip_end_date,
    # vendor_app_name,
    trip_de,
    trip_ue
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_start_date)

glimpse(logbooks__sc_fhier_for_output)
# 0

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
    all_of(common_output_fields),
    trip_date,
    compliant_after_override__fhier
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_date)

dim(dnfs__sc_fhier_for_output)
# 0

## 2. SC compliant and not compliant in FHIER ----

# 2) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 0 &
           common_month_compliance == "non_compl")

dim(compliant_vessels_in_sc_and_non_compl_fhier)
# [1] 1202   24

# "all_m_comp" field shows if any weeks of that month were compliant. We consider the whole month non-compliant if even one week was non-compliant. If SC considers the month compliant if at least one week was compliant that makes a big difference in the monthly compliance counts between SC and FHIER.

### 2a. Exclude non-compliant in both SC and FHIER ----
compliant_vessels_in_sc_and_non_compl_fhier__not_both_nc <-
  compliant_vessels_in_sc_and_non_compl_fhier |>
  dplyr::filter(!delinquent == 1 &
                  tolower(compliant_after_override) == "no")

dim(compliant_vessels_in_sc_and_non_compl_fhier__not_both_nc)
# [1] 458   24

### subset columns of data to output ----
compliant_vessels_in_sc_and_non_compl_fhier__for_output <-
  compliant_vessels_in_sc_and_non_compl_fhier |>
  select(
    vessel_reg_uscg_,
    all_of(common_output_fields),
    compliant_after_override
  ) |>
  filter(compliant_after_override == "no") |>
  distinct() |>
  arrange(vessel_reg_uscg_, comp_week_start_dt)

dim(compliant_vessels_in_sc_and_non_compl_fhier__for_output)
# [1] 559   7

## 3. List of vessels non compliant in both ----
sc__fhier_compl__join_w_month__non_compl_in_both <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 1 &
           common_month_compliance == "non_compl")

dim(sc__fhier_compl__join_w_month__non_compl_in_both)
# [1] 61 24

## 4. List of vessels compliant in both ----
sc__fhier_compl__join_w_month__compl_in_both <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 0 &
           common_month_compliance == "compl")

dim(sc__fhier_compl__join_w_month__compl_in_both)
# [1] 7759   24

# Write results to xlsx ----
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

# 2. create a readme sheet ----
## add today ----

# Explanations:
# Create a tibble with the current date in a specific format and set it as the "Read me" column.

# 1. 'top_of_read_me_text': A variable that stores a tibble containing the current date as its content.

# 2. The 'today()' function from the 'lubridate' package is used to get the current date.

# 3. The 'as_tibble_col()' function is used to create a tibble with a single column.
#    - The first argument is the value to be used for the tibble (in this case, the current date).
#    - The 'column_name' argument specifies the name of the single column ("Read me").
#    This results in a tibble with the current date stored under the "Read me" column.

top_of_read_me_text <-
  lubridate::today() |>
  tibble::as_tibble_col(column_name =  "Read me")

## sheet names with sheet descriptions ----
# save sheet_descriptions
sheet_descriptions <-
  c(
    "vessels that are non-compliant with SC but compliant with SEFHIER",
    "logbooks in FHIER from non-compliant SC vessels",
    "DNFs in FHIER from non-compliant SC vessels",
    "vessels that are compliant with SC but not with SEFHIER"
  )

# Explanations:
# Combine 'sheet_names' and 'sheet_descriptions' into a data frame.

# 1. 'sheet_names_with_df_names': A variable that stores a data frame containing sheet names and descriptions.

# 2. The 'cbind()' function combines the two vectors 'sheet_names' and 'sheet_descriptions' into a single data frame.
#    - This function combines the columns of the input vectors.
#    - Since 'sheet_names' and 'sheet_descriptions' are vectors, the resulting output is a two-column matrix.

# 3. The matrix is then converted to a data frame using the 'as.data.frame()' function.
#    - This conversion is necessary to work with the data structure more efficiently.
#    - The resulting data frame contains one column for sheet names and one column for sheet descriptions.

sheet_names_with_df_names <-
  cbind(sheet_names, sheet_descriptions) |>
  as.data.frame()

# rename columns in the new df
names(sheet_names_with_df_names) <-
  c("Sheet name", "Sheet Details")

# glimpse(sheet_names_with_df_names)

## column explanations for each tab ----
### colnames_for_each_df ----

# Use to create the list of colnames for each DF to be corrected in Excel
# Explanations:
# 1. 'colnames_for_each_df' is created by mapping over 'output_df_list' and 'sheet_names' simultaneously.
# 2. For each dataframe 'my_df' in 'output_df_list' and corresponding 'sheet_name':
#    a. Retrieve the column names of 'my_df' using the 'names' function.
#    b. Convert the column names into a tibble format with a single column named 'column_name' and assign the value of 'sheet_name' to each row.
#    c. Return the tibble containing the column names with associated sheet names.

get_each_df_colnames <-
  function(output_df_list, sheet_names) {

  colnames_for_each_df <-
    purrr::map2(output_df_list, sheet_names,
         \(my_df, sheet_name) {
           names(my_df) |>
             tibble::as_tibble_col(column_name = sheet_name) %>%
             return()
         })

  return(colnames_for_each_df)
}

# Uncomment and run if col names changed above
colnames_for_each_df <- get_each_df_colnames(output_df_list, sheet_names)
# 1 vessel_reg_uscg_
# 2 delinquent
# 3 month_sc
# 4 year_sc
# 5 comp_week_start_dt
# 6 comp_week_end_dt
# 7 compliant_after_override

# Use column definitions saved in a csv file

# define a path
column_definitions_path <-
    file.path(input_path,
            "column_definitions.csv")

# optional check
file.exists(column_definitions_path)

# read csv
column_definitions <-
  readr::read_csv(column_definitions_path)

# combine 3 dfs and convert to a type needed for output.
readme_text <-
  list(
    top_of_read_me_text,
    sheet_names_with_df_names,
    column_definitions
  )

# check class for each df, optional
purrr::map(readme_text, class)

## Add a new sheet ----
openxlsx::addWorksheet(wb, "Readme")

## Add a new style ----
bold.style <- openxlsx::createStyle(textDecoration = "Bold")

## Write each df from readme_text on to the same sheet ----

# Explanations:
# - This function, `write_readme_sheet`, writes the given `readme_text` data into a specified sheet of an Excel workbook.
# - `readme_text`: A list of data frames to be written into the workbook's specified sheet.
# - `workbook`: The Excel workbook object where data will be written. Defaults to the `wb` variable.
# - `sheet_name`: The name of the sheet where the data will be written. Defaults to "Readme".

  # 1. Initialize the `curr_row` variable to 1.
  #    This variable keeps track of the current row position in the sheet where data will be written.

  # 2. Loop through each element in `readme_text`:
  #    - `i` is the index of the current element.
  #    - `one_df` is the current dataframe.

  # 3. Check if `one_df` is indeed a data frame:
  #    - If not, convert it to a data frame using `as.data.frame()`.
  #    - This is done to ensure compatibility with the `writeData` function.

  # 4. Calculate the number of rows in `one_df` using `nrow()`.
  #    - This is stored in `one_df_size`.

  # 5. Use the `writeData` function to write `one_df` to the workbook:
  #    - `workbook` is the Excel workbook object where data will be written.
  #    - `sheet_name` is the name of the sheet where data will be written.
  #    - `one_df` is the data to be written.
  #    - `startRow` is set to `curr_row`, specifying the starting row in the sheet.
  #    - `headerStyle` is set to `bold.style` to apply bold styling to headers.

  # 6. Update `curr_row` to point to the next available row in the sheet:
  #    - Add the number of rows in `one_df` to `curr_row`.
  #    - Add 2 to account for spacing between each data frame.

write_readme_sheet <-
  function(readme_text, workbook = wb, sheet_name = "Readme") {
    curr_row <- 1
    for (i in seq_along(readme_text)) {
      # browser()
      one_df <- readme_text[[i]]

      if (!any(grepl("data.frame", class(one_df))))
      {
        one_df <- as.data.frame(one_df)
      }
      one_df_size <- nrow(one_df)

      openxlsx::writeData(workbook,
                sheet_name,
                one_df,
                startRow = curr_row,
                headerStyle = bold.style)
      curr_row <- curr_row + one_df_size + 2
    }
  }

# run the function
write_readme_sheet(readme_text,
                   workbook = wb,
                   sheet_name = "Readme")

# Optional. Not needed for processing.
# To see the wb.
openxlsx::openXL(wb)

## Move readme to the first position ----

# Explanations:
# 1. Get the current order of worksheets in the Excel workbook 'wb' using 'worksheetOrder' function and store it in 'old_order'.
# 2. Calculate the length of 'old_order' and store it in 'length_of_wb'.
# 3. Rearrange the order of worksheets in 'wb' by assigning a new order:
#    a. Start with the last worksheet by placing it at the first position using 'length_of_wb'.
#    b. Followed by the rest of the worksheets from the first to the second-to-last position using '1:(length_of_wb - 1)'.

# The readme sheet will be in the last position.

old_order <- openxlsx::worksheetOrder(wb)
old_order
# [1] 1 2 3 4 5

length_of_wb <- old_order |> length()
openxlsx::worksheetOrder(wb) <- c(length_of_wb, 1:(length_of_wb - 1))

# Optional. Not needed for processing.
# To see the new sheet order.
openxlsx::worksheetOrder(wb)
# [1] 5 1 2 3 4

# To see the wb
# openXL(wb)

# Write the Excel file ----

# define the path
output_file_name <-
  file.path(output_path,
            stringr::str_glue("sc_compliance_{sc_file_date}.xlsx"))

# write the workbook unto a file
openxlsx::saveWorkbook(wb, output_file_name, overwrite = T)

