# today()
# [1] "2024-04-15"
# compare this 4 files (permits) for 2023
# 1) compliance report downloaded from FHIER (= complaince module)
# 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types) -- don't know how to get permit info,
# 3) Metrics tracking from FHIER
# 4) permit info from the Oracle db
# 5) permit info from the PIMS
# "~\from_PIMS\Permits - 2024-01-25_0904.xlsx"
# get it from_PIMS
# Menu: permits
# Filter:
# Fishery = RCG - Gulf Charter/headboat For Reef Fish, CHG - Gulf Charter/headboat For Coastal Migratory Pelagic Fish, SC - South Atlantic Charter/headboat For Snapper-grouper, CHS - Atlantic Charter/headboat For Coastal Migratory Pelagics, HCHG - Historical Captain Gulf Charter/headboat For Coastal Migratory Pelagic Fish, HRCG - Historical Captain Gulf Charter/headboat For Reef Fish, CDW - Atlantic Charter/headboat For Dolphin/wahoo
#
# download
#
# skip first 5 lines in R)

# check transformed permits

# setup ----
source("~/R_code_github/useful_functions_module.r")

library(tidyverse)

# Determine the path of the executing script
library(this.path)

my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

my_year <- "2023"
my_date_beg <- str_glue('01-JAN-{my_year}')
my_date_end <- str_glue('31-DEC-{my_year}')

# get data ----
## 0) srhs vessels ----
srhs_vessels_file_path <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Inputs)",
            str_glue("{my_year}SRHSvessels.csv"))

file.exists(srhs_vessels_file_path)

srhs_vessels_my_year <-
  read_csv(srhs_vessels_file_path) |>
  clean_headers()

## 1) import the list of SRHS vessels ----
# this is a single spreadsheet with all vessels listed, as opposed to the version where they are separated by region (bothregions_asSheets)
# Use it to remove SRHS vessels from all inputs.
srhs_vessels_path <-
  file.path(
    my_paths$inputs,
    "processing_logbook_data",
    "inputs",
    str_glue("{my_year}srhsvessels.csv")
  )

file.exists(srhs_vessels_path)

srhs_vessels <-
  read_csv(srhs_vessels_path)

# rename and reformat column
srhs_vessels__renamed <-
  rename(srhs_vessels,
         vessel_official_number = "USCG #")

if (!class(srhs_vessels__renamed$vessel_official_number) == "character") {
  srhs_vessels__renamed$vessel_official_number <-
    as.character(srhs_vessels__renamed$vessel_official_number) |>
    str_trim()
}

## 2) compliance report downloaded from FHIER----
# (= complaince module)

compliance_file_path <-
  file.path(my_paths$inputs,
            r"(from_Fhier\FHIER Compliance\2024_04_09\FHIER_Compliance_2023__04_09_2024.csv)")

# file.exists(compliance_file_path)

compliance_from_fhier <-
  read_csv(compliance_file_path)

dim(compliance_from_fhier)
# [1] 149412     17

## 3) logbooks from the Oracle db all_logbooks ----
# (has 3 or 4 letters coded permit types)

# check_processed_logbooks

processed_logbooks_path <-
  file.path(
    my_paths$inputs,
    r"(processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2023_feb_7_2024.rds)"
  )

# file.exists(processed_logbooks_path)

processed_logbooks <-
  read_rds(processed_logbooks_path)

# has permits from metrics tracking

db_logbooks_query <-
  str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_start_date >= '{my_date_beg}'
  AND trip_start_date <= '{my_date_end}'
")

db_logbooks_file_name <-
  file.path(curr_proj_input_path,
                      str_glue("logbooks_{my_year}.rds"))

file.exists(db_logbooks_file_name)

db_logbooks_fun <-
  function(db_logbooks_query) {
    return(dbGetQuery(con,
                      db_logbooks_query))
  }

try(con <- connect_to_secpr())

get_db_logbooks <-
  function() {
    read_rds_or_run(db_logbooks_file_name,
                    db_logbooks_query,
                    db_logbooks_fun)
  }

db_logbooks <- get_db_logbooks()
# 2024-02-21 run for logbooks_2022.rds: 55.92 sec elapsed
# File: logbooks_2022.rds modified Wed Feb 21 15:35:26 2024

dim(db_logbooks)
# [1] 327987    149
# [1] 173521    149

## 4) Metrics tracking from FHIER, processed ----
# removed SRHS vessels and added permit_region column

metrics_report_dir_name <-
  r"(processing_logbook_data\Outputs)"

# was
# r"(Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source).csv)"

metrics_report_file_path <-
  file.path(
    my_paths$inputs,
    metrics_report_dir_name,
    str_glue("SEFHIER_permitted_vessels_nonSRHS_{my_year}.rds")
  )

file.exists(metrics_report_file_path)
# T

metrics_report <- read_rds(metrics_report_file_path)

dim(metrics_report)
# [1] 3606   13 (csv from FHIER)
# [1] 3443    9

## 5) permit table from the Oracle db ----
# end_date >= TO_DATE('{my_date_beg}', 'dd-mon-yy')
#     OR
dates_filter <-
  str_glue(
    " (expiration_date >= TO_DATE('{my_date_beg}', 'dd-mon-yy') )
  AND effective_date <= TO_DATE('{my_date_end}', 'dd-mon-yy')
"
  )

mv_sero_fh_permits_his_query_file_path <-
  file.path(my_paths$inputs,
            "get_db_data",
            str_glue("permit_info_{my_year}.rds"))

file.exists(mv_sero_fh_permits_his_query_file_path)
# T

mv_sero_fh_permits_his_query <-
  str_glue(
    "SELECT * FROM
srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE {dates_filter}
AND permit_group = 7
"
  )

mv_sero_fh_permits_his_query_fun <- function(mv_sero_fh_permits_his_query) {
  result <- dbGetQuery(con, mv_sero_fh_permits_his_query)
  return(result)
}

get_permit_info <-
  function() {
    read_rds_or_run(mv_sero_fh_permits_his_query_file_path,
                    mv_sero_fh_permits_his_query,
                    # force_from_db = TRUE
                    )
  }

permit_info_from_db <- get_permit_info()
# File: permit_info_2022.rds modified 2024-01-23 12:43:12.146822
# 2024-02-27 run for permit_info_2022.rds: 2.19 sec elapsed

nrow(permit_info_from_db)
# [1] 183855
# [1] 20777    2022 only
# 15807 group 7

# permit_info_from_db |> select(TOP) |>
#   distinct()
# 1  CDW
# 2  CHS
# 3   SC
# 4  CHG
# 5  RCG
# 6 HCHG
# 7 HRCG

# permit_info_from_db |>
#   filter(!TOP == PERMIT) |>
#   distinct() |>
#   View()
# Permit has ##

### check dates ----
# dates_filter <- " (end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
#     OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
#   AND effective_date <= TO_DATE('01-JAN-23', 'dd-mon-yy')
# "

min(permit_info_from_db$EXPIRATION_DATE)
# [1] "2007-01-31 EST"
# [1] "2007-02-28 EST"

# permit_info_from_db |>
#   filter(EXPIRATION_DATE == "2007-01-31 EST") |>
#   glimpse()
# $ VESSEL_ID            <chr> "514001"
# END_DATE == 2022-02-24 !!!

# TODO why min() doesn't work?
permit_info_from_db$END_DATE |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-19 EST"
# [1] "2021-01-21 EST"

max(permit_info_from_db$EFFECTIVE_DATE)
# [1] "2023-01-01 EST"
# [1] "2022-12-30 EST"

## 6) from_PIMS permit info ----
# "~\from_PIMS\Permits - 2024-01-25_0904.xlsx"

permit_file_path <-
  file.path(my_paths$inputs,
            "from_PIMS",
            "Permits - 2024-02-28_0930.xlsx")

to_skip <- 4
my_sheet <- "Sheet 1"

file.exists(permit_file_path)

permits_from_pims <-
  read.xlsx(permit_file_path,
            startRow = 4,
            detectDates = TRUE) |>
  clean_headers() |>
  remove_empty_cols()

# print_df_names(permits_from_pims)
# [1] "permit__, type, request_type, status, vessel_or_dealer, status_date, issue_date, effective_date, expiration_date, end_date, term_date"

correct_names_pims <-
  c(
  "permit__",
  "type",
  "request_type",
  "status",
  "vessel_or_dealer",
  "status_date",
  "issue_date",
  "effective_date",
  "expiration_date",
  "end_date",
  "term_date"
)

names(permits_from_pims) <- correct_names_pims

dim(permits_from_pims)
# [1] 23575    11
# [1] 53365    11

## 7) from_PIMS - permit applications for transfer ----
# "~\from_PIMS\Permit Applications - transfer - 2024-02-27_1555.xlsx"

transfer_applications_file_path <-
  file.path(my_paths$inputs,
            "from_PIMS",
            "pims_appliications",
            "Permit Applications - transfer - 2024-02-27_1555.xlsx")

to_skip <- 4
my_sheet <- "Sheet 1"


file.exists(transfer_applications_file_path)

transfer_applications_from_pims <-
  read.xlsx(permit_file_path,
            startRow = 4,
            detectDates = TRUE) |>
  clean_headers() |>
  remove_empty_cols()

names(transfer_applications_from_pims) <- correct_names_pims
dim(transfer_applications_from_pims)
# [1] 3214    9

correct_names_pims
## combine 4 dataframes ----
# "llist" is like list except that it preserves the names or labels of the component variables in the variables label attribute.
all_dfs_list <-
  Hmisc::llist(compliance_from_fhier,
    permits_from_pims,
    metrics_report,
    permit_info_from_db,
    transfer_applications_from_pims)

# View(all_dfs_list)

all_dfs_list_names <- names(all_dfs_list)

# aux functions ----
# Explanations:
# - The function `sep_chr_column` aims to transform a given column in a data frame by splitting its contents based on a specified delimiter and expanding the data frame to include a separate row for each unique value from the split column.
#
# 1. **Function Definition**:
#     - The function `sep_chr_column` takes three arguments:
#         - `my_df`: The input data frame that contains the column to be split.
#         - `col_name_to_sep`: The name of the column to be split, as a string.
#         - `split_chr`: The character used to split the column's values (default is a comma `,`).
#
# 2. **Data Transformation**:
#     - The function operates on the input data frame `my_df` and proceeds through several transformations:
#         - **`sep_s`**: A new column is added to the data frame (`my_df`) using the `mutate` function. The `str_split` function is applied to the specified column (`col_name_to_sep`) in each row, splitting the string into a list of separate values using the specified delimiter (`split_chr`).
#         - **`rowwise()`**: The data frame is processed row by row.
#         - **`sep_u`**: In each row, the unique values in `sep_s` are sorted and stored in a list format.
#         - **`ungroup()`**: Stops the row-wise operation after processing all rows.
#
# 3. **Data Unnesting**:
#     - The function `unnest_longer` is used to unpack the `sep_u` list in each row, creating separate rows for each unique value from the split column. The unpacked values are stored in a new column `permit_sep_u`.
#
# 4. **Return**:
#     - The function returns the transformed data frame (`my_df_w_split_col`), which contains separate rows for each unique value from the original column that was split.
#
# Overall, the function provides a convenient way to expand a data frame by splitting a specified column based on a delimiter, resulting in a new data frame with separate rows for each unique value from the split column.
sep_chr_column <-
  function(my_df,
           col_name_to_sep,
           split_chr = ",") {
    my_df_w_split_col <-
      my_df |>
      mutate(sep_s =
               str_split(!!sym(col_name_to_sep), split_chr)) |>
      rowwise() |>
      mutate(sep_u =
               list(sort(unique(sep_s)))) |>
      ungroup() |>
      unnest_longer(sep_u,
                    values_to = "permit_sep_u")

    return(my_df_w_split_col)
  }

# prepare data for comparison ----
## clean_headers ----
all_dfs_list1 <- map(all_dfs_list, clean_headers)

# map(all_dfs_list1, print_df_names)
# $compliance_from_fhier
# [1] "vessel_official_number, name, permitgroup, permit_groupexpiration, year, week, gom_permitteddeclarations__, captainreports__, negativereports__, complianceerrors__, compliant_, set_permits_on_hold_, overridden_, override_date, override_by, contactedwithin_48_hours_, submittedpower_down_"
#
# $permits_from_pims
# [1] "permit__, type, request_type, status, vessel_or_dealer, status_date, issue_date, effective_date, expiration_date, end_date, term_date"
#
# $metrics_report
# [1] "vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual"
#
# $permit_info_from_db
# [1] "vessel_id, entity_id, expiration_date, permit_group, top, permit, effective_date, end_date, initial_eff_date, grp_eff_date, last_expiration_date, tm_order, tm_top_order, prior_owner, new_owner, grp_prior_owner, application_id, permit_status, vessel_alt_num, min_period, max_period, top_name"
#

## add a column with the df name, for future joins ----
# Explanations:
# 1. Use the 'imap' function to iterate over each data frame in 'all_dfs_list1' along with its name.
# 2. For each data frame, use the 'add_column' function to add a new column with the name of the current data frame.
# 3. The '!!' is the unquote operator, used to evaluate 'curr_name' dynamically.
# 4. The result is a list of data frames with an additional column indicating the source data frame, stored in 'all_dfs_list2'.
all_dfs_list__name_col <-
  imap(all_dfs_list1,
       \(curr_df, curr_name) {
         curr_df |>
           add_column(!!curr_name := str_glue("{curr_name}__df_name"))
       })

# View(all_dfs_list__name_col)

## keep only vessel ids and permit columns ----

# Explanation:
# 1. The 'names_to_keep' function takes a data frame ('my_df') as input.
# 2. 'names(my_df)' retrieves the column names of the input data frame.
# 3. The 'grep' function is used to find column names containing specific substrings.
# 4. The substrings include "vessel", "permit", "exp", "effect", and "end_date".
# 5. 'value = TRUE' ensures that the actual column names are returned instead of indices.
# 6. 'ignore.case = TRUE' makes the search case-insensitive.
# 7. The result is a vector of selected column names based on the specified substrings.
names_to_keep <-
  function(my_df) {
    my_df_names <- names(my_df)
    grep("vessel|permit|date",
         my_df_names,
         value = TRUE,
         ignore.case = TRUE)
  }

### Apply the names_to_keep function to each dataframe in all_dfs_list__name_col ----
col_names_to_keep <- map(all_dfs_list__name_col, names_to_keep)

# View(col_names_to_keep)

# Explanation:
# 1. The 'imap' function iterates over each element (data frame) in the 'all_dfs_list__name_col' list along with its index.
# 2. For each data frame, the function inside 'imap' is executed.
# 3. The 'select' function is used to choose specific columns from the data frame 'x'.
# 4. The column names to be kept are specified by 'col_names_to_keep[[idx]]'.
# 5. The pipeline operator '|>' is used to pass the result to the next operation.
# 6. The 'select' function is again used to exclude columns containing the substring "trip" and any of others.
# 7. The result of this selective transformation is stored in the 'all_dfs_list2' list.

# View(col_names_to_keep)

all_dfs_list2 <-
  imap(all_dfs_list__name_col,
       function(x, idx)
       {
         select(x,
                col_names_to_keep[[idx]],
                any_of(c("top", all_dfs_list_names))) |>
           select(-contains("trip"),
                  -any_of(
                    c(
                      "gom_permitteddeclarations__",
                      "vessel_name",
                      "set_permits_on_hold_",
                      "override_date"
                    )
                  )) |>
           remove_empty_cols() |>
           distinct()
       }
  )

map(all_dfs_list2, print_df_names)

# Convert dates to Date format
# Explanations:
# 1. Use the 'map' function to iterate over each data frame in 'all_dfs_list2'.
# 2. For each data frame, use the 'mutate' function along with 'across'.
# 3. The 'across' function allows the application of a transformation to multiple columns.
# 4. Use the 'where' condition to select columns that are of character type.
# 5. Further filter the selected columns by those either ending with "_date" or starting with "permit_groupexpiration".
# 6. Apply 'lubridate::parse_date_time' to convert the selected character columns to the date-time format.
# 7. The 'orders' argument specifies the expected date formats, helping the parser identify the correct format.
# 8. The result is a list of data frames with parsed date columns, stored in 'all_dfs_list_dates'.

all_dfs_list_dates <-
  map(all_dfs_list2,
    \(current_df) {
      # browser()
      # names(current_df)
      current_df |>
        mutate(
          across(
            where(is.character) &
            (ends_with("_date") |
               starts_with("permit_groupexpiration")
             ),
            ~ lubridate::parse_date_time(.x, orders = c("mdY"))
          )
        )
    })

# View(all_dfs_list_dates)

# save the df
all_dfs_list3 <- all_dfs_list_dates

## individual df preparations ----

### compliance_from_fhier: split permit column ----

# Explanations:
# 1. **Column Transformation**:
#     - **`permitgroup_sep_0`**: The function `gsub` is used to remove parentheses and their contents from the `permitgroup` column and replaces them with a comma-separated list of groups.
#     - **`permitgroup_sep_1`**: The function `gsub` replaces consecutive commas (`,,`) with a single comma in the `permitgroup_sep_0` column.
#     - **`permitgroup_sep`**: The function `gsub` removes any trailing commas at the end of the string in `permitgroup_sep_1`.
#     - **`permitgroup_sep_s`**: The function `str_split` splits the cleaned `permitgroup_sep` strings into lists of separate groups based on commas.
#
# 2. **Data Processing**:
#     - The function operates `rowwise()`, processing each row individually:
#         - **`permitgroup_sep_u`**: In each row, the unique groups in `permitgroup_sep_s` are sorted and stored in a list format.
#         - The code comments out the `permitgroup_sep_u_str` line, which previously concatenated the list into a string.
#     - The data frame is then `ungroup()`-ed to stop the row-wise operation.
#
# 3. **Data Unnesting**:
#     - The function `unnest_longer` is used to unpack the `permitgroup_sep_u` list in each row, creating separate rows for each unique permit group.
#     - The unpacked values are stored in a new column `permit_sep_u`.
#
# 4. **Output**:
#     - The final data frame contains separate rows for each unique permit group per original row, allowing easier analysis and manipulation.

temp_compliance_from_fhier <-
  all_dfs_list_dates$compliance_from_fhier |>
  mutate(permitgroup_sep_0 =
           gsub("\\(([^)]+)\\)", "\\1,", permitgroup)) |>
  mutate(permitgroup_sep_1 =
           gsub(",,+", ",", permitgroup_sep_0)) |>
  mutate(permitgroup_sep =
           gsub(",$", "", permitgroup_sep_1)) |>
  mutate(permitgroup_sep_s =
           str_split(permitgroup_sep, ",")) |>
  rowwise() |>
  mutate(permitgroup_sep_u =
           list(sort(unique(permitgroup_sep_s)))) |>
  # mutate(permitgroup_sep_u_str =
  #          permitgroup_sep_u |>
  #          stringi::stri_paste(sep = ',', collapse = ',')
  #      ) |>
  ungroup() |>
  unnest_longer(permitgroup_sep_u,
                values_to = "permit_sep_u")

# View(temp_compliance_from_fhier)

temp_compliance_from_fhier__drop_permit_numbers <-
  temp_compliance_from_fhier |>
  filter(!grepl("\\d", permit_sep_u))

unique(temp_compliance_from_fhier__drop_permit_numbers$permit_sep_u)
# [1] "CDW"  "CHS"  "SC"   "CHG"  "RCG"  "HCHG" "HRCG"

all_dfs_list3$compliance_from_fhier <-
  temp_compliance_from_fhier__drop_permit_numbers |>
  select(
    vessel_official_number,
    permit_groupexpiration,
    permit_sep_u,
    compliance_from_fhier
  ) |>
  distinct()

# View(all_dfs_list3$compliance_from_fhier)

#### check diff permitgroup for the same vessel ----
short_compliance_from_fhier_to_test <-
  all_dfs_list_dates$compliance_from_fhier |>
  select(vessel_official_number,
         permit_groupexpiration,
         permitgroup) |>
  distinct()

dim(short_compliance_from_fhier_to_test)
# [1] 3692    3
# [1] 3614    3 2023

n_distinct(short_compliance_from_fhier_to_test$vessel_official_number)
# [1] 3687
# [1] 3613 2023
# Some vessels have > 1 permitgroup

dim(all_dfs_list3$compliance_from_fhier)
# [1] 9965    3

n_distinct(all_dfs_list3$compliance_from_fhier$vessel_official_number)
# 3687

unique(all_dfs_list3$compliance_from_fhier$permit_sep_u)
# [1] "CDW"  "CHS"  "SC"   "CHG"  "RCG"  "HCHG" "HRCG"

# Explanation:
# 1. The pipeline operator '|>' applies a sequence of operations to the 'short_compliance_from_fhier_to_test' data frame.
# 2. 'group_by(vessel_official_number)' groups the data frame by the 'vessel_official_number' column.
# 3. 'mutate(multiple_permitgroups = +(n_distinct(permitgroup) > 1))' adds a new column 'multiple_permitgroups'.
#    - 'n_distinct(permitgroup)' calculates the number of distinct permit groups for each vessel.
#    - '> 1' checks if there are more than 1 distinct permit groups.
#    - '+(...)' converts the logical result to 0 or 1.
# 4. 'ungroup()' removes the grouping to work with the entire data frame.
# 5. 'filter(multiple_permitgroups > 0)' filters rows where 'multiple_permitgroups' is greater than 0, keeping only vessels with multiple distinct permit groups.

get_multiple_entries_per_vessel <-
  function(my_df,
           vessel_id_col_name,
           to_check_col_name) {
    # browser()

    multiple_res <-
      my_df |>
      group_by(!!sym(vessel_id_col_name)) |>
      mutate(multiple_entries =
               +(n_distinct(!!sym(to_check_col_name)) > 1)) |>
      ungroup() |>
      filter(multiple_entries > 0) |>
      rename_with(~ str_glue("multiple_{to_check_col_name}s"),
                  multiple_entries) |>
      arrange(!!sym(vessel_id_col_name))

    return(multiple_res)
  }

short_compliance_from_fhier_multi_permitgroups <-
  get_multiple_entries_per_vessel(short_compliance_from_fhier_to_test,
                                  "vessel_official_number",
                                  "permitgroup")

nrow(short_compliance_from_fhier_multi_permitgroups)
# 10
# the same permits, diff format
# 0 2023

### metrics_report: split permit column ----
# Explanations:
# - This block of code modifies the `metrics_report` data frame in the `all_dfs_list_dates` list and assigns the resulting data frame back to `all_dfs_list3$metrics_report`.
# - The code uses a series of `mutate` transformations, `rowwise` processing, and `unnest_longer` to split a column in the data frame and expand it.
#
# 1. **Column Trimming**:
#     - The code starts by using the `mutate` function to create a new column, `permits_trim`.
#     - `permits_trim` is created by removing all spaces from the `permits` column using the `gsub` function.
#
# 2. **Splitting Column**:
#     - Another `mutate` step creates a new column `permits_sep_s`.
#     - This column is a list of split values from `permits_trim`, split by semicolons (`;`) using the `str_split` function.
#
# 3. **Row-wise Processing**:
#     - The data frame is processed row by row using the `rowwise` function.
#
# 4. **Creating Unique Sorted List**:
#     - In each row, the code calculates the `permits_sep_u` column.
#     - It contains a list of sorted, unique values from `permits_sep_s` for each row.
#
# 5. **Stop Row-wise Processing**:
#     - The function `ungroup()` is called to stop row-wise processing.
#
# 6. **Data Unnesting**:
#     - The function `unnest_longer` unpacks the `permits_sep_u` list column, creating a separate row for each unique value from the original `permits` column.
#     - The new values are stored in the `permit_sep_u` column.
#
# 7. **Selecting Columns**:
#     - The code removes the original columns `permits_trim`, `permits_sep_s`, and `permits` using the `select` function.
#
# 8. **Final Output**:
#     - The modified data frame is stored back in `all_dfs_list3$metrics_report`.
#
# Overall, the code efficiently processes the `metrics_report` data frame by splitting a specified column, expanding the data frame with separate rows for each unique value, and removing unnecessary columns.
all_dfs_list3$metrics_report <-
  all_dfs_list_dates$metrics_report |>
  mutate(permits_trim =
           gsub(" ", "", permits)) |>
  mutate(permits_sep_s =
           str_split(permits_trim, ";")) |>
  rowwise() |>
  mutate(permits_sep_u =
           list(sort(unique(permits_sep_s)))) |>
  ungroup() |>
  unnest_longer(permits_sep_u,
                values_to = "permit_sep_u") |>
  select(-c(permits_trim,
            permits_sep_s,
            permits))

# for future use
all_permits_in_metrics <-
  all_dfs_list3$metrics_report |>
  select(permit_sep_u) |>
  distinct()

# 1 CDW
# 2 CHS
# 3 SC
# 4 CHG
# 5 RCG
# 6 HCHG
# 7 HRCG

# all_dfs_list3$metrics_report |> glimpse()

### permit_info_from_db: unify vessel ids ----

# grep("vessel", names(all_dfs_list3$permit_info_from_db), value = T)
# [1] "vessel_id"      "vessel_alt_num"

nrow(all_dfs_list3$permit_info_from_db)
# 20730
# 15763

all_dfs_list3$permit_info_from_db <-
  all_dfs_list3$permit_info_from_db |>
  mutate(permit_info_from_db_vessel_id = vessel_id) |> # want to keep it to see if NA in the full join
  rename("vessel_official_number" = "vessel_id")

### permit_info_from_db groups to keep ----
# now in the query
# permit_info_from_db |>
#   select(TOP, PERMIT_GROUP) |>
#   distinct() |>
#   View()

# all_dfs_list3$permit_info_from_db |>
#   filter(tolower(top) %in% tolower(all_permits_in_metrics$permit_sep_u)) |>
#   select(permit_group) |> distinct()
#   permit_group
# 1            7

# all_dfs_list3$permit_info_from_db |>
# filter(permit_group == 6) |>
#   select(top) |>
#            distinct()
# GC
# not used

# permit_info_from_db |>
# filter(PERMIT_GROUP == 6) |>
#   select(starts_with("TOP")) |>
#            distinct()
#   TOP                   TOP_NAME
# 1  GC SOUTH ATLANTIC GOLDEN CRAB

# all_dfs_list3$permit_info_from_db <-
#   all_dfs_list3$permit_info_from_db |>
#   filter(permit_group == 7)

# nrow(all_dfs_list3$permit_info_from_db)
# 16073

# print_df_names(all_dfs_list3$permit_info_from_db)
permit_info_from_db__no_digit_perm <-
  all_dfs_list3$permit_info_from_db |>
  filter(!grepl("\\d", top))

unique(permit_info_from_db__no_digit_perm$top)
# [1] "CDW"  "CHS"  "SC"   "CHG"  "RCG"  "HCHG" "HRCG"

#### put permit_info_from_db back ----
all_dfs_list3$permit_info_from_db <-
  permit_info_from_db__no_digit_perm

# View(all_dfs_list3$permit_info_from_db)

### permits_from_pims get only new ----
# TODO: don't include if all the dates before 2021 or after 2023?
program_start_date <- lubridate::dmy("04-JAN-2021")

in_my_date_range <-
  rlang::quo(
      # end_date >= program_start_date |
        expiration_date >= program_start_date
  )

all_dfs_list3$permits_from_pims |>
glimpse()

# permits_from_pims_new <-
  all_dfs_list3$permits_from_pims |>
  filter(expiration_date >= program_start_date) |> dim()
  # filter(!!in_my_date_range)

# check
dim(permits_from_pims_new)
# [1] 8801   8
# [1] 20485  9

n_distinct(permits_from_pims_new$vessel_or_dealer)
# 3127
# 7178
# [1] 7102

# don't do that, too few vessels left
# in_my_date_range <-
#   rlang::quo(
#     (
#       end_date >=          lubridate::dmy(my_date_beg) |
#         expiration_date >= lubridate::dmy(my_date_beg)
#     ) &
#       effective_date <=    lubridate::dmy(my_date_end)
#   )
#
# permits_from_pims_my_year <-
#   all_dfs_list3$permits_from_pims |>
#   filter(!!in_my_date_range)
#
# dim(permits_from_pims_my_year)
# # [1] 1036    8

# check
# n_distinct(permits_from_pims_my_year$vessel_or_dealer)
# 324
# n_distinct(all_dfs_list2$permits_from_pims$vessel_or_dealer)
# 7417

# permits_from_pims_my_year |>
#   select(effective_date, end_date, expiration_date) |>
#   distinct() |>
#   arrange(effective_date) |>
#   glimpse()

### permits_from_pims split permit__ ----
# $ permit__         <chr> "CHG-981", "CHG-120", "RCG-114", "CHG-1417", "RCG-1359"…
# print_df_names(permits_from_pims_new)
permits_from_pims__permit_only <-
  permits_from_pims_new |>
  mutate(permit_clean =
           str_replace(permit_,
                       "-\\d+", ""),
         .before = permits_from_pims)

n_distinct(permits_from_pims__permit_only$vesselordealer)
# 3127
# [1] 7178
# [1] 7102

### permits_from_pims split vessel_or_dealer ----

permits_from_pims__permit_only__vessel_id <-
  permits_from_pims__permit_only |>
  separate(vesselordealer,
           c('vessel_official_number', 'dealer'),
           sep = " / ") |>
  mutate(across(c('vessel_official_number', 'dealer'),
                str_squish))

# permits_from_pims__permit_only[8636,] |> glimpse()
# Expected 2 pieces. Missing pieces filled with `NA` in 3 rows [8636, 8637, 8638].
# Expected 2 pieces. Missing pieces filled with `NA` in 778 rows [393, 396, 478, 479,
# 508, 514, 519, 546, 555, 766, 767, 768, 810, 811, 815, 828, 893, 910, 911, 912,
# ...].

n_distinct(permits_from_pims__permit_only__vessel_id$vessel_official_number)
# 3069
# [1] 7016
# [1] 6990

# View(permits_from_pims__permit_only__vessel_id)

### permits_from_pims fewer cols ----

permits_from_pims__permit_only__vessel_id_short <-
  permits_from_pims__permit_only__vessel_id |>
  select(-c(permit_, dealer)) |>
  distinct()

# glimpse(permits_from_pims__permit_only__vessel_id_short)

### permits_from_pims: exclude srhs vessels ----
permits_from_pims__permit_only__vessel_id_short__not_srhs <-
  permits_from_pims__permit_only__vessel_id_short |>
  filter(!vessel_official_number %in% srhs_vessels_my_year$uscg__)

n_distinct(permits_from_pims__permit_only__vessel_id_short$vessel_official_number)
# 6979

n_distinct(permits_from_pims__permit_only__vessel_id_short__not_srhs$vessel_official_number)
# 6865
# lost to SRHS 114

# this year only
in_my_date_range <-
  rlang::quo((
    expiration_date >= lubridate::dmy(my_date_beg) |
      end_date >= lubridate::dmy(my_date_beg)
  ) &
    effective_date <= lubridate::dmy(my_date_end)
  )

permits_from_pims_my_year <-
  permits_from_pims__permit_only__vessel_id_short__not_srhs |>
  filter(!!in_my_date_range)

dim(permits_from_pims_my_year)
# [1] 1500    9
# [1] 13007     9

# print_df_names(permits_from_pims_my_year)

# check
n_distinct(permits_from_pims_my_year$vessel_official_number)
# 734

n_distinct(metrics_report$VESSEL_OFFICIAL_NUMBER)
# 3443

permits_from_pims_my_year |>
  select(effective_date, end_date, expiration_date) |>
  distinct() |>
  arrange(effective_date) |>
  # arrange(desc(effective_date)) |>
  glimpse()
# effective_date  2020-02-01 -- 2022-12-30
# expiration_date 2022-01-31 -- 2024-05-31

### put permits_from_pims back ----
all_dfs_list3$permits_from_pims <-
  permits_from_pims__permit_only__vessel_id_short__not_srhs

unique(all_dfs_list3$permits_from_pims$permit_clean)
# [1] "CDW"  "SC"   "CHS"  "CHG"  "RCG"  "HRCG" "HCHG"
# ...
# [41] "GC"     "ALR
# TODO: why permits are not cleaned?

### transfer_applications_from_pims split vessel_or_dealer 1 ----

all_dfs_list3$transfer_applications_from_pims$vesselordealer |>
  head()

transfer_applications_from_pims__split1 <-
  all_dfs_list3$transfer_applications_from_pims |>
  separate(vesselordealer,
           c('vessel_official_numbers', 'dealer'),
           sep = "\\(") |>
  mutate(across(c('vessel_official_numbers', 'dealer'),
                str_squish)) |>
  mutate(dealer = str_replace(dealer, "\\)", ""))

n_distinct(transfer_applications_from_pims__split1$vessel_official_numbers)
# 2471

# View(transfer_applications_from_pims__split1)

#### check double vessel ids ----
transfer_applications_from_pims__split1 |>
  select(vessel_official_numbers) |>
  distinct() |>
  filter(grepl("/", vessel_official_numbers)) |>
  head(15)

### transfer_applications_from_pims split vessel_or_dealer 2 ----
transfer_applications_from_pims__split2 <-
  transfer_applications_from_pims__split1 |>
  separate(
    vessel_official_numbers,
    c('vessel_official_number', 'vessel_official_number2'),
    sep = " / "
  ) |>
  mutate(across(starts_with('vessel_official_number'),
                str_squish))
# for those not havin a "/":
# Expected 2 pieces. Missing pieces filled with `NA` in 3022 rows [1, 2, 3, 4, 5,
# 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

# transfer_applications_from_pims__split1[1,]

# View(transfer_applications_from_pims__split2)

### transfer_applications_from_pims__split2 fewer cols ----

transfer_applications_from_pims__split2_short <-
  transfer_applications_from_pims__split2 |>
  # select(-c(dealer, permitholder_name)) |>
  select(-c(dealer)) |>
  distinct()

# glimpse(transfer_applications_from_pims__split2_short)

### put transfer_applications_from_pims__split2_short back ----
all_dfs_list3$transfer_applications_from_pims <-
  transfer_applications_from_pims__split2_short

# TODO: how to deal with vessel_official_number 1 & 2 in here? Needed for further transformations

## remove SRHS vessels ----
# map(all_dfs_list3, print_df_names)
# $compliance_from_fhier
# [1] "vessel_official_number, permit_groupexpiration, permit_sep_u, compliance_from_fhier"
#
# $permits_from_pims
# [1] "vessel_official_number, status_date, issue_date, effective_date, expiration_date, end_date, term_date, permits_from_pims, permit_clean"
#
# $metrics_report
# [1] "vessel_official_number, effective_date, end_date, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual, metrics_report, permit_sep_u"
#
# $permit_info_from_db
# [1] "vessel_official_number, expiration_date, permit_group, permit, effective_date, end_date, initial_eff_date, grp_eff_date, last_expiration_date, permit_status, vessel_alt_num, permit_info_from_db, top, permit_info_from_db_vessel_id"

all_dfs_list_no_srhs <-
  all_dfs_list3 |>
  map(\(one_df) {
    # browser()
    one_df |>
      filter(!vessel_official_number %in% srhs_vessels__renamed$vessel_official_number)
  })

map(all_dfs_list3, dim)
# $compliance_from_fhier
# [1] 9965    4
#
# $permits_from_pims
# [1] 20483     9
#
# $metrics_report
# [1] 9171    9
#
# $permit_info_from_db
# [1] 15763    14
#
# $transfer_applications_from_pims
# [1] 3168    4

# map(all_dfs_list_no_srhs, dim)

# check permit_sep_u & permit_info_from_db ----
setdiff(all_permits_in_metrics$permit_sep_u,
        all_dfs_list_no_srhs$permit_info_from_db$top)
# should be 0 both ways, ok

all_dfs_list_no_srhs$permit_info_from_db$top |>
  unique()
# [1] "CDW"  "CHS"  "SC"   "CHG"  "RCG"  "HCHG" "HRCG"

min(all_dfs_list_no_srhs$permit_info_from_db$expiration_date)
# [1] "2007-02-28 EST"

all_dfs_list_no_srhs$permit_info_from_db$end_date |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-21 EST"

max(all_dfs_list_no_srhs$permit_info_from_db$effective_date)
# [1] "2022-12-30 EST"

# get pairs ----
file_name_combinations <-
  combn(all_dfs_list_names, 2)

# compare each pair ----

## aux functions for comparison ----

# Usage:
# add_groups_by_where(my_joined_df, file_name_combinations[,1])

add_groups_by_where <-
  function(my_df,
           df_name_cols_vector
           ) {
    df_name_col_1 <- df_name_cols_vector[[1]]
    df_name_col_2 <- df_name_cols_vector[[2]]

    # file_name_combinations[,1][[1]]
    my_df__vsl_perm__grps <-
      my_df |>
      mutate(
        where_is_vessel_permit =
          case_when(
            !is.na(!!sym(df_name_col_1)) &
              !is.na(!!sym(df_name_col_2)) ~
              "in_both",
            !is.na(!!sym(df_name_col_1)) &
              is.na(!!sym(df_name_col_2)) ~
              str_glue("in_{df_name_col_1}"),
            is.na(!!sym(df_name_col_1)) &
              !is.na(!!sym(df_name_col_2)) ~
              str_glue("in_{df_name_col_2}"),
            .default = "unknown"
          )
      )

    return(my_df__vsl_perm__grps)
  }


split_by_3_grps <-
  function(my_df) {
    my_df__list <-
      my_df |>
      split(as.factor(my_df$where_is_vessel_permit))

    return(my_df__list)
  }

vessel_ids_only_by_group <- function(my_df) {
  vessel_ids_by_group <-
    my_df |>
    map(\(curr_df) {
      curr_df |>
        select(vessel_official_number) |>
        distinct()
    })

  return(vessel_ids_by_group)
}

group_vsls_and_count <-
  function(my_df, curr_file_name_combination) {

    # browser()
    my_df__grps <-
      add_groups_by_where(my_df,
                          curr_file_name_combination)

    # to see group names
    # unique(my_df__grps$where_is_vessel_permit)

    my_df__grps_short_cnt <-
      my_df__grps |>
      select(vessel_official_number, where_is_vessel_permit) |>
      distinct() |>
      count(where_is_vessel_permit)

    title_message_print("Group counts")
    print(my_df__grps_short_cnt)
    # unlist(my_df__grps_short_cnt) |>
    #   pretty_print("Group counts")

    my_df__grps_short_cnt |>
      count(wt = n) |>
      unlist() |>
      pretty_print("Total cnt in groups")

    n_distinct(my_df__grps$vessel_official_number) |>
      pretty_print("Total vsl cnt")

    return(my_df__grps)
  }

vessel_in_more_than_1_grp <- function(my_names_lists) {
  # browser()
  names_combns <- combn(names(my_names_lists), 2) |>
    as.data.frame()

  # make_col names
  my_col_names <-
    names_combns |>
    map(\(x) {
      # browser()
      name1 <- x[[1]]
      name2 <- x[[2]]

      comb_name <-
        str_glue("inters_{name1}__{name2}")
    })

  names(names_combns) <- my_col_names

  names_combns |>
    map(\(x) {
      # browser()
      name1 <- x[[1]]
      name2 <- x[[2]]

      curr_intersection <-
        intersect(my_names_lists[[name1]]$vessel_official_number,
                  my_names_lists[[name2]]$vessel_official_number)

      return(curr_intersection)
  })
}

## combine all functions to find intersections ----

run_intersection_check <-
  function(my_df) {
    my_df__list <-
      split_by_3_grps(my_df)

    # vessels in > 1 group
    vessel_ids_by_group <-
      vessel_ids_only_by_group(my_df__list)

    curr_intersections <-
      vessel_in_more_than_1_grp(vessel_ids_by_group)

    return(curr_intersections)
  }

## [1] "compliance_from_fhier" "permits_from_pims" ----
file_name_combinations[,1]

# print_df_names(all_dfs_list_no_srhs$compliance_from_fhier)
# print_df_names(all_dfs_list_no_srhs$permits_from_pims)

n_distinct(all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number)
# 3687
# 3684 (no srhs)
n_distinct(all_dfs_list_no_srhs$permits_from_pims$vessel_official_number)
# 3069
# 2957 (no srhs)
# 6902

join_compliance_from_fhier__permits_from_pims__perm <-
  full_join(
    all_dfs_list_no_srhs$compliance_from_fhier,
    all_dfs_list_no_srhs$permits_from_pims,
    join_by(vessel_official_number)
  )
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 14828 of `y` matches multiple rows in `x`.
# TODO check, this is a result of having sep permits

# View(join_compliance_from_fhier__permits_from_pims__perm)

### vessel is in compliance_from_fhier, not in permits_from_pims ----

vessel_in_compl_not_in_pims_perm <-
  join_compliance_from_fhier__permits_from_pims__perm |>
  filter(is.na(permits_from_pims)) |>
  select(vessel_official_number) |>
  distinct()

num__vessel_in_compl_not_in_pims_perm <-
  nrow(vessel_in_compl_not_in_pims_perm)
# 1520
# [1] 1433

vessel_in_compl_not_in_pims_perm1 <-
  setdiff(
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number,
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number
  )
num__vessel_in_compl_not_in_pims_perm1 <-
  length(vessel_in_compl_not_in_pims_perm1)
# 1520
# 1433

# check: should be the same as above
num__vessel_in_compl_not_in_pims_perm == num__vessel_in_compl_not_in_pims_perm1
# T

vessel_in_permits_from_pims_not_in_compl <-
  setdiff(
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number,
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number
  )
length(vessel_in_permits_from_pims_not_in_compl)
# 905
# 793 (no srhs)
# 4651

glimpse(vessel_in_permits_from_pims_not_in_compl)

### compliance_from_fhier & permits_from_pims join by vessel and permit ----
join_compliance_from_fhier__permits_from_pims__vsl_perm <-
  full_join(
    all_dfs_list_no_srhs$compliance_from_fhier,
    all_dfs_list_no_srhs$permits_from_pims,
    join_by(vessel_official_number, permit_sep_u == permit_clean)
  )

# ℹ Row 334 of `x` matches multiple rows in `y`.
# ℹ Row 6383 of `y` matches multiple rows in `x`.

# View(join_compliance_from_fhier__permits_from_pims__vsl_perm)

### 3 groups, join_compliance_from_fhier__permits_from_pims__vsl_perm__grps ----
# 1) in both,
# 2) in compl only,
# 3) in pims only

join_compliance_from_fhier__permits_from_pims__vsl_perm__grps <-
  group_vsls_and_count(
    join_compliance_from_fhier__permits_from_pims__vsl_perm,
    file_name_combinations[, 1]
  )

unique(join_compliance_from_fhier__permits_from_pims__vsl_perm__grps$where_is_vessel_permit)
# [1] "in_both"                  "in_compliance_from_fhier"
# [3] "in_permits_from_pims"

join_compliance_from_fhier__permits_from_pims__vsl_perm__grps |>
  select(vessel_official_number, where_is_vessel_permit) |>
  distinct() |>
  count(where_is_vessel_permit) |>
  # head()
# 1 in_both                  2161
# 2 in_compliance_from_fhier 1653
# 3 in_permits_from_pims      930
# today
# 1 in_both                   2147
# 2 in_compliance_from_fhier  1692
# 3 in_permits_from_pims      5594

  count(wt = n)
# 4744, the same vessel in >1 group!
# 9433

n_distinct(join_compliance_from_fhier__permits_from_pims__vsl_perm__grps$vessel_official_number)
# 4592
# [1] 8335
# the same vessel in >1 group!

### check all 3 groups, join_compliance_from_fhier__permits_from_pims__vsl_perm__grps ----
# TODO: save vessel id and look for them in other dfs
vessels_only_in_compliance <-
  join_compliance_from_fhier__permits_from_pims__vsl_perm__grps |>
  filter(where_is_vessel_permit == "in_compliance_from_fhier") |>
  select(vessel_official_number) |>
  distinct()

### vessels in > 1 group, join_compliance_from_fhier__permits_from_pims__vsl_perm__grps ----
intersections_1 <-
  run_intersection_check(join_compliance_from_fhier__permits_from_pims__vsl_perm__grps)

# View(intersections_1)

map(intersections_1, length)
# $inters_in_both__in_compliance_from_fhier
# [1] 127
#
# $inters_in_both__in_permits_from_pims
# [1] 22
#
# $inters_in_compliance_from_fhier__in_permits_from_pims
# [1] 5

map(intersections_1, head(1))
# $inters_in_both__in_compliance_from_fhier

# $inters_in_both__in_permits_from_pims

# $inters_in_compliance_from_fhier__in_permits_from_pims

join_compliance_from_fhier__permits_from_pims__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()
# only 1 permit in pims

join_compliance_from_fhier__permits_from_pims__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()
# old SA in PIMS, ok

join_compliance_from_fhier__permits_from_pims__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()
# diff in compliance and in pims

## [2] "compliance_from_fhier" "metrics_report" ----
file_name_combinations[,2]

# print_df_names(all_dfs_list_no_srhs$compliance_from_fhier)
# print_df_names(all_dfs_list_no_srhs$metrics_report)

### join by vessel and permit, join_compliance_from_fhier__metrics_report__vsl_permit ----
join_compliance_from_fhier__metrics_report__vsl_permit <-
  full_join(
    all_dfs_list_no_srhs$compliance_from_fhier,
    all_dfs_list_no_srhs$metrics_report,
    join_by(vessel_official_number,
            permit_sep_u)
  )

vessel_in_compl_not_in_metrics <-
  setdiff(
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number,
    all_dfs_list_no_srhs$metrics_report$vessel_official_number
  )

length(vessel_in_compl_not_in_metrics)
# 255

vessel_in_metrics_not_in_compl <-
  setdiff(
    all_dfs_list_no_srhs$metrics_report$vessel_official_number,
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_metrics_not_in_compl)
# 11

# check
join_compliance_from_fhier__metrics_report__vsl_permit__grps <-
  add_groups_by_where(join_compliance_from_fhier__metrics_report__vsl_permit,
                      file_name_combinations[, 2])

glimpse(join_compliance_from_fhier__metrics_report__vsl_permit__grps)

join_compliance_from_fhier__metrics_report__vsl_permit__grps |>
  select(vessel_official_number, where_is_vessel_permit) |>
  distinct() |>
  count(where_is_vessel_permit) |>
  # head()
# 1 in_both                   3432
# 2 in_compliance_from_fhier   397
# 3 in_metrics_report           19
  count(wt = n)
# 3848

n_distinct(join_compliance_from_fhier__metrics_report__vsl_permit__grps$vessel_official_number)
# 3698, the same vessel in >1 group!

intersections_2 <-
  run_intersection_check(join_compliance_from_fhier__metrics_report__vsl_permit__grps)

map(intersections_2, length)
# $inters_in_both__in_compliance_from_fhier
# [1] 142
#
# $inters_in_both__in_metrics_report
# [1] 8
#
# $inters_in_compliance_from_fhier__in_metrics_report
# [1] 0

map(intersections_2, head(1))
# $inters_in_both__in_compliance_from_fhier

# $inters_in_both__in_metrics_report

join_compliance_from_fhier__metrics_report__vsl_permit |>
  filter(vessel_official_number == "") |>
  glimpse()
# missing in metrics_report__df_name

join_compliance_from_fhier__metrics_report__vsl_permit |>
  filter(vessel_official_number == "") |>
  glimpse()
#  "CHG", "RCG" are missing in compl

## [3] "compliance_from_fhier" "permit_info_from_db" ----
curr_file_name_combinations <-
  file_name_combinations[,3]

# print_df_names(all_dfs_list_no_srhs$compliance_from_fhier)
# print_df_names(all_dfs_list_no_srhs$permit_info_from_db)

join_compliance_from_fhier__permit_info_from_db__vsl_perm <-
  full_join(
    all_dfs_list_no_srhs$compliance_from_fhier,
    all_dfs_list_no_srhs$permit_info_from_db,
    join_by(vessel_official_number,
            permit_sep_u == top)
  )

vessel_in_compl_not_in_permit_info_from_db <-
  setdiff(
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number
  )

length(vessel_in_compl_not_in_permit_info_from_db)
# 3687
# 13 after +id
# 10 after 2022 and permit group 7
# 1143
# 13

vessel_in_compl_not_in_permit_info_from_db_alt <-
  setdiff(
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_compl_not_in_permit_info_from_db_alt)
# 171
# 169 after 2022 and permit group 7
# 1237

vessel_in_permit_info_from_db_not_in_compl <-
  setdiff(
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number,
    all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_compl)
# 0
# 10387 after +id
# 195 after 2022 and permit group 7
# 95
# [1] "2024-03-15"
# 74

### check join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps ----
join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps <-
  add_groups_by_where(
    join_compliance_from_fhier__permit_info_from_db__vsl_perm,
    curr_file_name_combinations
  )

# glimpse(join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps)

join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps |>
  select(vessel_official_number, where_is_vessel_permit) |>
  distinct() |>
  count(where_is_vessel_permit) |>
  # head()
# 1 in_both                   2544
# 2 in_compliance_from_fhier  1536
# 3 in_permit_info_from_db      96
  count(wt = n)
# 4176
# 3866

n_distinct(join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps$vessel_official_number)
# 3782, the same vessel in >1 group!
# 3758

intersections_3 <-
  run_intersection_check(join_compliance_from_fhier__permit_info_from_db__vsl_perm__grps)

map(intersections_3, length)
# $inters_in_both__in_compliance_from_fhier
# [1] 393
# 100

# $inters_in_both__in_permit_info_from_db
# [1] 1
# 8

map(intersections_3, head(1))
# $inters_in_both__in_compliance_from_fhier
# [1] "VA2668BK"
# SC5306EA
#
# $inters_in_both__in_permit_info_from_db
# [1] "1024989"
# MC7540US

join_compliance_from_fhier__permit_info_from_db__vsl_perm |>
# filter(vessel_official_number == "") |>
  # gom permits are missing in compl
  filter(vessel_official_number == "") |>
  glimpse()
# misssing in db

join_compliance_from_fhier__permit_info_from_db__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()
# GOM p. are missing from compl (TRANSFERRED)

## [4] "compliance_from_fhier"           "transfer_applications_from_pims" ----
curr_file_name_combinations <-
  file_name_combinations[,4]
# TODO

## [5] "permits_from_pims" "metrics_report" ----
curr_file_name_combinations <-
  file_name_combinations[,5]

# print_df_names(all_dfs_list_no_srhs$permits_from_pims)
# print_df_names(all_dfs_list_no_srhs$metrics_report)

join_permits_from_pims__metrics_report <-
  full_join(
    all_dfs_list_no_srhs$permits_from_pims,
    all_dfs_list_no_srhs$metrics_report,
    join_by(vessel_official_number)
  )
# after separating permits:
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 1891 of `y` matches multiple rows in `x`.
#

#### check Row 1 of `x` matches multiple rows in `y` ----

# join_permits_from_pims__metrics_report
all_dfs_list_no_srhs$metrics_report |>
  filter(
    vessel_official_number ==
      all_dfs_list_no_srhs$permits_from_pims[1, ]$vessel_official_number
  ) |>
  glimpse()
# multiple permit_sep_u, ok.

#### check Row 7304 of `y` matches multiple rows in `x`. ----

all_dfs_list_no_srhs$permits_from_pims |>
  filter(
    vessel_official_number ==
      all_dfs_list_no_srhs$metrics_report[1891, ]$vessel_official_number
  ) |>
  glimpse()
# multiple permit_sep_u, ok.

# $ effective_date         <dttm> 2007-03-24, 2007-03-24, 2008-04-23, 2011-07-01, 2011…
# $ expiration_date        <dttm> 2008-02-29, 2008-02-29, 2009-02-28, 2012-06-30, 2012…
# $ end_date               <dttm> 2023-04-27, 2023-04-27, 2023-04-27, 2023-04-27, 2023…

### vessel diff ----
vessel_in_permits_from_pims_not_in_metrics_report <-
  setdiff(
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number,
    all_dfs_list_no_srhs$metrics_report$vessel_official_number
  )

length(vessel_in_permits_from_pims_not_in_metrics_report)
# [1] 4706

vessel_in_metrics_report_not_in_permits_from_pims <-
  setdiff(
    all_dfs_list_no_srhs$metrics_report$vessel_official_number,
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number
  )

length(vessel_in_metrics_report_not_in_permits_from_pims)
# [1] 1284

### join by vessel and permit, join_permits_from_pims__metrics_report__vsl_perm ----
join_permits_from_pims__metrics_report__vsl_perm <-
  full_join(
    all_dfs_list_no_srhs$permits_from_pims,
    all_dfs_list_no_srhs$metrics_report,
    join_by(vessel_official_number,
            permit_clean == permit_sep_u),
    suffix = c("__permits_from_pims",
               "__metrics_report")
  )

### 3 grps, join_permits_from_pims__metrics_report__vsl_perm__grps ----
join_permits_from_pims__metrics_report__vsl_perm__grps <-
  group_vsls_and_count(
    join_permits_from_pims__metrics_report__vsl_perm,
    curr_file_name_combinations
  )
# Group counts
# # A tibble: 3 × 2
#   where_is_vessel_permit     n
#   <glue>                 <int>
# 1 in_both                 2071
# 2 in_metrics_report       1476
# 3 in_permits_from_pims    5655
# Total cnt in groups
# 9202
# ---
# Total vsl cnt
# 8166

# View(join_permits_from_pims__metrics_report__vsl_perm__grps)

### check vessels in pims, but not in metrics ----
join_permits_from_pims__metrics_report__vsl_perm__grps |>
  filter(vessel_official_number %in%
           vessel_in_permits_from_pims_not_in_metrics_report) |>
  glimpse()
# end_date__permits_from_pims       <dttm> 2021-03-31,

### vessels in > 1 group, join_permits_from_pims__metrics_report__vsl_perm__grps ----
intersections_4 <-
  run_intersection_check(join_permits_from_pims__metrics_report__vsl_perm__grps)

map(intersections_4, length)
# $inters_in_both__in_metrics_report
# [1] 81
#
# $inters_in_both__in_permits_from_pims
# [1] 66
#
# $inters_in_metrics_report__in_permits_from_pims
# [1] 15
# today
# $inters_in_both__in_metrics_report
# [1] 104
#
# $inters_in_both__in_permits_from_pims
# [1] 824
#
# $inters_in_metrics_report__in_permits_from_pims
# [1] 143

map(intersections_4, head(1))
# inters_in_both__in_metrics_report

# $inters_in_both__in_permits_from_pims

# $inters_in_metrics_report__in_permits_from_pims

# today
# $inters_in_both__in_metrics_report

# $inters_in_both__in_permits_from_pims

# $inters_in_metrics_report__in_permits_from_pims


join_permits_from_pims__metrics_report__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()
# ok, pims exp date < 2021, but end_date is not

join_permits_from_pims__metrics_report__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()

join_permits_from_pims__metrics_report__vsl_perm |>
  filter(vessel_official_number == "") |>
  glimpse()

## [5] "permits_from_pims" "permit_info_from_db" ----
curr_file_name_combinations <-
  file_name_combinations[, 5]

# print_df_names(all_dfs_list_no_srhs$permits_from_pims)
# print_df_names(all_dfs_list_no_srhs$permit_info_from_db)

# not needed, count the vessel difference directly, join by vessel an permit
# join_permits_from_pims__permit_info_from_db <-
#   full_join(
#     all_dfs_list_no_srhs$permits_from_pims,
#     all_dfs_list_no_srhs$permit_info_from_db,
#     join_by(vessel_official_number)
#   )
# #   Detected an unexpected many-to-many relationship between `x` and `y`.
#
# ### why multiple?
# # 1) x to y
# # ℹ Row 1 of `x` matches multiple rows in `y`.
# all_dfs_list_no_srhs$permit_info_from_db |>
#   filter(vessel_official_number ==
#     all_dfs_list_no_srhs$permits_from_pims[1,][["vessel_official_number"]] |
#       vessel_alt_num ==
#     all_dfs_list_no_srhs$permits_from_pims[1,][["vessel_official_number"]]) |>
#   glimpse()
# # multiple permits, OK
# # $ permit               <chr> "1066", "1013", "1066", "1013"
# # $ effective_date       <dttm> 2021-06-04, 2021-06-04, 2022-04-18, 2022-04-18
#
# # 2) y to x
# # ℹ Row 9426 of `y` matches multiple rows in `x`.
# all_dfs_list_no_srhs$permits_from_pims |>
#   filter(vessel_official_number ==
#     all_dfs_list_no_srhs$permit_info_from_db[9426,][["vessel_official_number"]] |
#       vessel_official_number ==
#     all_dfs_list_no_srhs$permit_info_from_db[9426,][["vessel_alt_num"]]) |>
#   glimpse()
#
# permits_from_pims_multi_notif_accsp_permit_id <-
#   get_multiple_entries_per_vessel(all_dfs_list_no_srhs$permits_from_pims,
#                                   "vessel_official_number",
#                                   "notif_accsp_permit_id")
#
# nrow(permits_from_pims_multi_notif_accsp_permit_id)
# # [1] 714

# uncomment to run
# permits_from_pims_multi_notif_accsp_permit_id |>
#   write_csv(
#     file.path(
#       curr_proj_output_path,
#       "permits_from_pims__multiple_notif_accsp_permit_id.csv"
#     )
#   )

vessel_in_permits_from_pims_not_in_permit_info_from_db <-
  setdiff(
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number
  )

length(vessel_in_permits_from_pims_not_in_permit_info_from_db)
# 1
# 2 after 2022 and sep permits

all_dfs_list_no_srhs$permit_info_from_db |>
  filter(vessel_official_number == "") |>
  nrow()
# [1] 18
# 3 after 2022 and sep permits

all_dfs_list_no_srhs$permits_from_pims |>
  filter(vessel_official_number == "" |
           vessel_official_number == "") |>
  glimpse()
# In permits_from_pims only. (PIMS "No items available", Official Number From USCG Certificate Of Documentation)
# In permit_info_from_db only.

vessel_in_permits_from_pims_not_in_permit_info_from_db_alt <-
  setdiff(
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_permits_from_pims_not_in_permit_info_from_db_alt)
# 92
# 94 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_permits_from_pims <-
  setdiff(
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number,
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_permits_from_pims)
# 12176
# 1985 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_permits_from_pims_alt <-
  setdiff(
    all_dfs_list_no_srhs$permit_info_from_db$vessel_alt_num,
    all_dfs_list_no_srhs$permits_from_pims$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_permits_from_pims_alt)
# 12247
# 2074 after 2022 and sep permits

### join by vessel and permit, permits_from_pims and permit_info_from_db ----
intersect(names(all_dfs_list_no_srhs$permits_from_pims),
          names(all_dfs_list_no_srhs$permit_info_from_db))
# [1] "vessel_official_number" "effective_date"         "expiration_date"
# [4] "end_date"

c(names(all_dfs_list_no_srhs$permits_from_pims),
  names(all_dfs_list_no_srhs$permit_info_from_db)) |>
  map(\(x) {
    grep("permit", x, value = T)
  })

all_dfs_list_no_srhs$permit_info_from_db |>
  filter(!top == permit) |>
  glimpse()
# 0, ok, all the same

glimpse(all_dfs_list_no_srhs$permits_from_pims)

join_permits_from_pims__permit_info_from_db__vsl_perm <-
  full_join(
    all_dfs_list_no_srhs$permits_from_pims,
    all_dfs_list_no_srhs$permit_info_from_db,
    join_by(vessel_official_number,
            permit_clean == top),
    suffix = c("__permits_from_pims",
               "__permit_info_from_db"),
    relationship = "many-to-many"
  )

#### check multiple, join_permits_from_pims__permit_info_from_db__vsl_perm ----
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 4016 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"`
#   to silence this warning.

in_x <-
  all_dfs_list_no_srhs$permits_from_pims[1, ]$vessel_official_number


all_dfs_list_no_srhs$permit_info_from_db |>
  filter(vessel_official_number == in_x) |>
  glimpse()

### 3 grps, join_permits_from_pims__permit_info_from_db__vsl_perm__grps ----
join_permits_from_pims__permit_info_from_db__vsl_perm__grps <-
  group_vsls_and_count(
    join_permits_from_pims__permit_info_from_db__vsl_perm,
    curr_file_name_combinations
  )
# Group counts
# # A tibble: 3 × 2
#   where_is_vessel_permit     n
#   <glue>                 <int>
# 1 in_both                 1560
# 2 in_permit_info_from_db  1098
# 3 in_permits_from_pims    1752
# Total cnt in groups
# 4410
# ---
# Total vsl cnt
# 4098
# ---

### vessels in > 1 group, join_permits_from_pims__permit_info_from_db__vsl_perm__grps ----
intersections_5 <-
  run_intersection_check(join_permits_from_pims__permit_info_from_db__vsl_perm__grps)

map(intersections_5, length)
# $inters_in_both__in_permit_info_from_db
# [1] 19
#
# $inters_in_both__in_permits_from_pims
# [1] 243
#
# $inters_in_permit_info_from_db__in_permits_from_pims
# [1] 53

map(intersections_5, head(1))
# $inters_in_both__in_permit_info_from_db

# $inters_in_both__in_permits_from_pims

# $inters_in_permit_info_from_db__in_permits_from_pims

# join_permits_from_pims__permit_info_from_db__vsl_perm__grps |>
#   filter(vessel_official_number == "") |>
#   View()
# duplicated entries?

# join_permits_from_pims__permit_info_from_db__vsl_perm__grps |>
#   filter(vessel_official_number == "") |>
#   View()
# ok, in pims 2024

# join_permits_from_pims__permit_info_from_db__vsl_perm__grps |>
#   filter(vessel_official_number == "") |>
#   View()
# missing in PIMS?

## [6] "metrics_report" "permit_info_from_db" ----
curr_file_name_combinations <-
  file_name_combinations[, 6]

# print_df_names(all_dfs_list_no_srhs$metrics_report)
# print_df_names(all_dfs_list_no_srhs$permit_info_from_db)

join_metrics_report__permit_info_from_db__vsl_perm <-
  full_join(
    all_dfs_list_no_srhs$metrics_report,
    all_dfs_list_no_srhs$permit_info_from_db,
    join_by(vessel_official_number,
            permit_sep_u == top),
    suffix = c("__metrics_report",
               "__permit_info_from_db")
  )

vessel_in_metrics_report_not_in_permit_info_from_db <-
  setdiff(
    all_dfs_list_no_srhs$metrics_report$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number
  )

length(vessel_in_metrics_report_not_in_permit_info_from_db)
# 5
# 27 after 2022 and sep permits
# 952
# 9

vessel_in_metrics_report_not_in_permit_info_from_db_alt <-
  setdiff(
    all_dfs_list_no_srhs$metrics_report$vessel_official_number,
    all_dfs_list_no_srhs$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_metrics_report_not_in_permit_info_from_db_alt)
# 161
# 185 after 2022 and sep permits
# 1046
# 152

vessel_in_permit_info_from_db_not_in_metrics_report <-
  setdiff(
    all_dfs_list_no_srhs$permit_info_from_db$vessel_official_number,
    all_dfs_list_no_srhs$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_metrics_report)
# 10460
# 293 after 2022 and sep permits
# 148
# 311

vessel_in_permit_info_from_db_not_in_metrics_report_alt <-
  setdiff(
    all_dfs_list_no_srhs$permit_info_from_db$vessel_alt_num,
    all_dfs_list_no_srhs$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_metrics_report_alt)
# 10596
# 448 after 2022 and sep permits
# 239
# 451

### 3 grps, join_metrics_report__permit_info_from_db__vsl_perm__grps ----
# where_is_vessel_permit
# View(join_metrics_report__permit_info_from_db__vsl_perm)
join_metrics_report__permit_info_from_db__vsl_perm__grps <-
  group_vsls_and_count(
    join_metrics_report__permit_info_from_db__vsl_perm,
    curr_file_name_combinations
  )
# Group counts
#   where_is_vessel_permit     n
#   <glue>                 <int>
# 1 in_both                 2484
# 2 in_metrics_report       1236
# 3 in_permit_info_from_db   156
# Total cnt in groups
# 3876
# ---
# Total vsl cnt
# 3591

### check vessels in metrics only ----
join_metrics_report__permit_info_from_db__vsl_perm__grps |>
  filter(
    vessel_official_number %in%
      vessel_in_metrics_report_not_in_permit_info_from_db
  ) |>
  glimpse()

### vessels in > 1 group, join_metrics_report__permit_info_from_db__vsl_perm__grps ----
intersections_6 <-
  run_intersection_check(join_metrics_report__permit_info_from_db__vsl_perm__grps)

map(intersections_6, length)
# $inters_in_both__in_metrics_report
# [1] 277
#
# $inters_in_both__in_permit_info_from_db
# [1] 1
#
# $inters_in_metrics_report__in_permit_info_from_db
# [1] 7

vessels_to_check <-
  map_df(intersections_6, head(1))
# $inters_in_both__in_metrics_report

# $inters_in_both__in_permit_info_from_db

# $inters_in_metrics_report__in_permit_info_from_db

vessels_to_check |>
  map(\(x) {
    # browser()
    join_metrics_report__permit_info_from_db__vsl_perm__grps |>
      filter(vessel_official_number == x) |>
      glimpse()
  })

