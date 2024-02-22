# compare this 4 files (permits) for 2022
# 1) compliance report downloaded from FHIER (= complaince module)
# 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types) -- don't know how to get permit info,
# 3) Metrics tracking from FHIER
# 4) permit info from the Oracle db
# 5) permit info from the PIMS
# "~\from PIMS\Permits - 2024-01-25_0904.xlsx"
# get it from PIMS
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

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

my_year <- "2022"
my_date_beg <- '01-JAN-2022'
my_date_end <- '31-DEC-2022'

# get data ----
## 1) compliance report downloaded from FHIER (= complaince module) ----

compliance_file_name <- "FHIER Compliance.csv"

compliance_file_path <-
  file.path(curr_proj_input_path,
            compliance_file_name)

compliance_from_fhier <-
  read_csv(compliance_file_path)

dim(compliance_from_fhier)
# [1] 148375     17

## 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types) ----
# check_processed_logbooks
processed_logbooks <- read_rds(r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2023.rds)")

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

## 3) Metrics tracking from FHIER, processed ----
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

## 4) permit table from the Oracle db ----
dates_filter <-
  str_glue(
    " (end_date >= TO_DATE('{my_date_beg}', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('{my_date_beg}', 'dd-mon-yy') )
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
                    mv_sero_fh_permits_his_query_fun
                    # force_from_db = TRUE
                    )
  }

permit_info_from_db <- get_permit_info()
# File: permit_info_2022.rds modified 2024-01-23 12:43:12.146822

nrow(permit_info_from_db)
# [1] 183855
# [1] 20777    2022 only

### check dates ----
# dates_filter <- " (end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
#     OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
#   AND effective_date <= TO_DATE('01-JAN-23', 'dd-mon-yy')
# "

min(permit_info_from_db$EXPIRATION_DATE)
# [1] "2007-01-31 EST"

permit_info_from_db |>
  filter(EXPIRATION_DATE == "2007-01-31 EST") |>
  glimpse()
# $ VESSEL_ID            <chr> "514001"
# END_DATE == 2022-02-24 !!!

# TODO why min() doesn't work?
permit_info_from_db$END_DATE |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-19 EST"

max(permit_info_from_db$EFFECTIVE_DATE)
# [1] "2023-01-01 EST"

## 5) permit info from the PIMS ----
# "~\from PIMS\Permits - 2024-01-25_0904.xlsx"

permit_file_path <-
  file.path(my_paths$inputs,
            "from PIMS",
            "Permits - 2024-01-25_0904.xlsx")
to_skip <- 4
my_sheet <- "Sheet 1"

file.exists(permit_file_path)

permits_from_pims <-
  read_xlsx(permit_file_path,
            sheet = my_sheet,
            skip = to_skip)

# glimpse(permits_from_pims_raw)

dim(permits_from_pims)
# [1] 23575    11

## combine 4 dataframes ----
# "llist" is like list except that it preserves the names or labels of the component variables in the variables label attribute.
all_4_dfs <-
  Hmisc::llist(compliance_from_fhier,
    permits_from_pims,
    metrics_report,
    permit_info_from_db)

# View(all_4_dfs)

all_4_df_names <- names(all_4_dfs)

# prepare data for comparison ----
## clean_headers ----
all_4_dfs1 <- map(all_4_dfs, clean_headers)
# View(all_4_dfs1)

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
    grep("vessel|permit|_date",
         my_df_names,
         value = TRUE,
         ignore.case = TRUE)
  }

### Apply the names_to_keep function to each dataframe in all_4_dfs1 ----
col_names_to_keep <- map(all_4_dfs1, names_to_keep)

# View(col_names_to_keep)

# Explanation:
# 1. The 'imap' function iterates over each element (data frame) in the 'all_4_dfs1' list along with its index.
# 2. For each data frame, the function inside 'imap' is executed.
# 3. The 'select' function is used to choose specific columns from the data frame 'x'.
# 4. The column names to be kept are specified by 'col_names_to_keep[[idx]]'.
# 5. The pipeline operator '|>' is used to pass the result to the next operation.
# 6. The 'select' function is again used to exclude columns containing the substring "trip" and any of others.
# 7. The result of this selective transformation is stored in the 'all_4_dfs2' list.

# View(col_names_to_keep)
all_4_dfs2 <-
  imap(all_4_dfs1,
       function(x, idx)
       {
         select(x,
                col_names_to_keep[[idx]],
                any_of(c("top"))) |>
           select(-contains("trip"),
                  -any_of(c(
                    "gom_permitteddeclarations__",
                    "vessel_name",
                    "set_permits_on_hold_",
                    "override_date"
                  ))) |>
           remove_empty_cols() |>
           distinct()
       }
  )

map(all_4_dfs2, print_df_names)

# Convert dates to Date format
# Explanations:
# 1. Use the 'map' function to iterate over each data frame in 'all_4_dfs2'.
# 2. For each data frame, use the 'mutate' function along with 'across'.
# 3. The 'across' function allows the application of a transformation to multiple columns.
# 4. Use the 'where' condition to select columns that are of character type.
# 5. Further filter the selected columns by those either ending with "_date" or starting with "permit_groupexpiration".
# 6. Apply 'lubridate::parse_date_time' to convert the selected character columns to the date-time format.
# 7. The 'orders' argument specifies the expected date formats, helping the parser identify the correct format.
# 8. The result is a list of data frames with parsed date columns, stored in 'all_4_dfs_dates'.

all_4_dfs_dates <-
  map(all_4_dfs2,
    \(current_df) {
      current_df |>
        mutate(
          across(
            where(is.character) &
            (ends_with("_date") |
               starts_with("permit_groupexpiration")),
            ~ lubridate::parse_date_time(.x, orders = c("mdY"))
          )
        )
    })

# View(all_4_dfs_dates)

# save the df
all_4_dfs3 <- all_4_dfs_dates

## individual df preparations ----

### compliance_from_fhier: split permit column ----

temp_compliance_from_fhier <-
  all_4_dfs_dates$compliance_from_fhier |>
  mutate(permitgroup_sep_0 =
           gsub("\\(([^)]+)\\)", "\\1,", permitgroup)) |>
  mutate(permitgroup_sep_1 =
           gsub(",,+", ",", permitgroup_sep_0)) |>
  mutate(permitgroup_sep =
           gsub(",$", "", permitgroup_sep_1)) |>
  # filter(vessel_official_number == 'FL6900MH') |> View()
  # !!! 3
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

all_4_dfs3$compliance_from_fhier <-
  temp_compliance_from_fhier |>
  select(
    vessel_official_number,
    permit_groupexpiration,
    permit_sep_u
  ) |>
  distinct()

# View(all_4_dfs3$compliance_from_fhier)

#### check diff permitgroup for the same vessel ----
short_compliance_from_fhier_to_test <-
  all_4_dfs_dates$compliance_from_fhier |>
  select(vessel_official_number,
         permit_groupexpiration,
         permitgroup) |>
  distinct()

dim(short_compliance_from_fhier_to_test)
# [1] 3692    3

n_distinct(short_compliance_from_fhier_to_test$vessel_official_number)
# [1] 3687
# Some vessels have > 1 permitgroup

dim(all_4_dfs3$compliance_from_fhier)
# [1] 13442     3

n_distinct(all_4_dfs3$compliance_from_fhier$vessel_official_number)
# 3687

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

### metrics_report: split permit column ----
all_4_dfs3$metrics_report <-
  all_4_dfs_dates$metrics_report |>
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
  all_4_dfs3$metrics_report |>
  select(permit_sep_u) |>
  distinct()

# 1 CDW
# 2 CHS
# 3 SC
# 4 CHG
# 5 RCG
# 6 HCHG
# 7 HRCG

### permit_info_from_db: unify vessel ids ----

# grep("vessel", names(all_4_dfs3$permit_info_from_db), value = T)
# [1] "vessel_id"      "vessel_alt_num"

nrow(all_4_dfs3$permit_info_from_db)
# 20730

all_4_dfs3$permit_info_from_db <-
  all_4_dfs3$permit_info_from_db |>
  mutate(permit_info_from_db_vessel_id = vessel_id) |> # want to keep it to see if NA in the full join
  rename("vessel_official_number" = "vessel_id")

### permit_info_from_db groups to keep ----
# permit_info_from_db |>
#   select(TOP, PERMIT_GROUP) |>
#   distinct() |>
#   View()

all_4_dfs3$permit_info_from_db |>
  filter(tolower(top) %in% tolower(all_permits_in_metrics$permit_sep_u)) |>
  select(permit_group) |> distinct()
#   permit_group
# 1            7

all_4_dfs3$permit_info_from_db |>
filter(permit_group == 6) |>
  select(top) |>
           distinct()
# GC
# not used

# permit_info_from_db |>
# filter(PERMIT_GROUP == 6) |>
#   select(starts_with("TOP")) |>
#            distinct()
#   TOP                   TOP_NAME
# 1  GC SOUTH ATLANTIC GOLDEN CRAB

all_4_dfs3$permit_info_from_db <-
  all_4_dfs3$permit_info_from_db |>
  filter(permit_group == 7)

nrow(all_4_dfs3$permit_info_from_db)
# 16073

# stopped presenting here
# check
setdiff(all_permits_in_metrics$permit_sep_u,
        all_4_dfs3$permit_info_from_db$top)
# 0 both ways, ok

min(all_4_dfs3$permit_info_from_db$expiration_date)
# [1] "2007-02-28 EST"

all_4_dfs3$permit_info_from_db$end_date |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-21 EST"

max(all_4_dfs3$permit_info_from_db$effective_date)
# [1] "2023-01-01 EST"

# get pairs ----
file_name_combinations <-
  combn(all_4_df_names, 2)

# compare each pair ----
## [1] "compliance_from_fhier" "db_logbooks" ----
file_name_combinations[,1]

print_df_names(all_4_dfs3$compliance_from_fhier)
print_df_names(all_4_dfs3$db_logbooks)

join_compliance_from_fhier__db_logbooks__perm <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$db_logbooks,
    join_by(vessel_official_number)
  )
# ℹ Row 27 of `x` matches multiple rows in `y`.
# ℹ Row 1735 of `y` matches multiple rows in `x`.
# TODO check, this is a result of having sep permits

# View(join_compliance_from_fhier__db_logbooks)

### vessel is in compliance_from_fhier, not in db_logbooks ----

vessel_in_compl_not_in_logb <-
  join_compliance_from_fhier__db_logbooks__perm |>
  filter(is.na(db_logbooks_vessel_id)) |>
  select(vessel_official_number) |>
  distinct()

nrow(vessel_in_compl_not_in_logb)
# 1803

vessel_in_compl_not_in_logb <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_number
  )
length(vessel_in_compl_not_in_logb)
# 1803

vessel_in_logb_not_in_compl <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )
length(vessel_in_logb_not_in_compl)
# 5

glimpse(vessel_in_logb_not_in_compl)
# chr [1:2] "1038780" "1292480"
# "1292480/NC0676EK"
# chr [1:5] "1311397" "1038780" "1316517" "1292480" "1301119"

# all_4_dfs3$db_logbooks |>
#   filter(vessel_official_nbr == "NC0676EK")
# 0

## [2] "compliance_from_fhier" "metrics_report" ----
file_name_combinations[,2]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$metrics_report)

join_compliance_from_fhier__metrics_report <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$metrics_report,
    join_by(vessel_official_number,
            permit_sep_u)
  )

vessel_in_compl_not_in_metrics <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_compl_not_in_metrics)
# 240

vessel_in_metrics_not_in_compl <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_metrics_not_in_compl)
# 159

## [3] "compliance_from_fhier" "permit_info_from_db" ----
file_name_combinations[,3]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$permit_info_from_db)

join_compliance_from_fhier__permit_info_from_db <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$permit_info_from_db,
    join_by(vessel_official_number,
            permit_sep_u == top)
  )
# View(join_compliance_from_fhier__permit_info_from_db)

# no many-to-many after adding permit_sep_u == top!
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 74370 of `y` matches multiple rows in `x`.

### why multiple? ----
# 1) x to y
# ℹ Row 1 of `x` matches multiple rows in `y`.
all_4_dfs3$permit_info_from_db |>
  filter(vessel_official_number ==
    all_4_dfs3$compliance_from_fhier[1,][["vessel_official_number"]] |
      vessel_alt_num ==
    all_4_dfs3$compliance_from_fhier[1,][["vessel_official_number"]]) |>
  glimpse()

all_4_dfs3$compliance_from_fhier |>
  filter(vessel_official_number == "579608") |>
  nrow()
# 0

all_4_dfs3$permit_info_from_db |>
  filter(vessel_official_number == "579608") |>
  nrow()
# 27 (multiple permits, ok)
# 0 after join by permits

# 2) y to x
# ℹ Row 74282 of `y` matches multiple rows in `x`.
all_4_dfs1$permit_info_from_db[74282,]

all_4_dfs3$permit_info_from_db |>
  filter(vessel_official_number ==
    all_4_dfs3$compliance_from_fhier[74282,][["vessel_official_number"]] |
      vessel_alt_num ==
    all_4_dfs3$compliance_from_fhier[74282,][["vessel_official_number"]]) |>
  nrow()
# 0

vessel_in_compl_not_in_permit_info_from_db <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_official_number
  )

length(vessel_in_compl_not_in_permit_info_from_db)
# 3687
# 13 after +id
# 10 after 2022 and permit group 7

vessel_in_compl_not_in_permit_info_from_db_alt <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_compl_not_in_permit_info_from_db_alt)
# 171
# 169 after 2022 and permit group 7

vessel_in_permit_info_from_db_not_in_compl <-
  setdiff(
    all_4_dfs3$permit_info_from_db$vessel_official_number,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_compl)
# 0
# 10387 after +id
# 195 after 2022 and permit group 7

## [4] "db_logbooks" "metrics_report" ----
file_name_combinations[,4]

# print_df_names(all_4_dfs3$db_logbooks)
# print_df_names(all_4_dfs3$metrics_report)

join_db_logbooks__metrics_report <-
  full_join(
    all_4_dfs3$db_logbooks,
    all_4_dfs3$metrics_report,
    join_by(vessel_official_number)
  )
# after separating permits:
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 2905 of `y` matches multiple rows in `x`.
# TODO: check

vessel_in_db_logbooks_not_in_metrics_report <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_db_logbooks_not_in_metrics_report)
# 43

vessel_in_metrics_report_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_number
  )

length(vessel_in_metrics_report_not_in_db_logbooks)
# 1760

## [5] "db_logbooks" "permit_info_from_db" ----
file_name_combinations[,5]

# print_df_names(all_4_dfs3$db_logbooks)
# print_df_names(all_4_dfs3$permit_info_from_db)

join_db_logbooks__permit_info_from_db <-
  full_join(
    all_4_dfs3$db_logbooks,
    all_4_dfs3$permit_info_from_db,
    join_by(vessel_official_number)
  )
#   Detected an unexpected many-to-many relationship between `x` and `y`.

### why multiple? ----
# 1) x to y
# ℹ Row 1 of `x` matches multiple rows in `y`.
all_4_dfs3$permit_info_from_db |>
  filter(vessel_official_number ==
    all_4_dfs3$db_logbooks[1,][["vessel_official_number"]] |
      vessel_alt_num ==
    all_4_dfs3$db_logbooks[1,][["vessel_official_number"]]) |>
  glimpse()
# multiple permits, OK
# $ permit               <chr> "1066", "1013", "1066", "1013"
# $ effective_date       <dttm> 2021-06-04, 2021-06-04, 2022-04-18, 2022-04-18

# 2) y to x
# ℹ Row 9426 of `y` matches multiple rows in `x`.
all_4_dfs3$db_logbooks |>
  filter(vessel_official_number ==
    all_4_dfs3$permit_info_from_db[9426,][["vessel_official_number"]] |
      vessel_official_number ==
    all_4_dfs3$permit_info_from_db[9426,][["vessel_alt_num"]]) |>
  glimpse()
# $ vessel_official_nbr      <chr> "FL6432SU", "FL6432SU"
# $ notif_accsp_permit_id    <dbl> 584995, NA

db_logbooks_multi_notif_accsp_permit_id <-
  get_multiple_entries_per_vessel(all_4_dfs3$db_logbooks,
                                  "vessel_official_number",
                                  "notif_accsp_permit_id")

nrow(db_logbooks_multi_notif_accsp_permit_id)
# [1] 714

# uncomment to run
# db_logbooks_multi_notif_accsp_permit_id |>
#   write_csv(
#     file.path(
#       curr_proj_output_path,
#       "db_logbooks__multiple_notif_accsp_permit_id.csv"
#     )
#   )

vessel_in_db_logbooks_not_in_permit_info_from_db <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_official_number
  )

length(vessel_in_db_logbooks_not_in_permit_info_from_db)
# 1
# 2 after 2022 and sep permits

# 1292480
# 1292480:NC0676EK........ SOUTHERN RUN - BENJAMIN AUGUSTUS MORRIS  (828) 4298076

all_4_dfs3$permit_info_from_db |>
  filter(vessel_official_number == "NC0676EK") |>
  nrow()
# [1] 18
# 3 after 2022 and sep permits

all_4_dfs3$db_logbooks |>
  filter(vessel_official_number == "1292480" |
           vessel_official_number == "NC0676EK") |>
  glimpse()
# In db_logbooks 1292480 only. (PIMS "No items available", Official Number From USCG Certificate Of Documentation)
# In permit_info_from_db NC0676EK only.

vessel_in_db_logbooks_not_in_permit_info_from_db_alt <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_db_logbooks_not_in_permit_info_from_db_alt)
# 92
# 94 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$permit_info_from_db$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_db_logbooks)
# 12176
# 1985 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_db_logbooks_alt <-
  setdiff(
    all_4_dfs3$permit_info_from_db$vessel_alt_num,
    all_4_dfs3$db_logbooks$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_db_logbooks_alt)
# 12247
# 2074 after 2022 and sep permits

## [6] "metrics_report" "permit_info_from_db" ----
file_name_combinations[,6]

# print_df_names(all_4_dfs3$metrics_report)
# print_df_names(all_4_dfs3$permit_info_from_db)

join_metrics_report__permit_info_from_db <-
  full_join(
    all_4_dfs3$metrics_report,
    all_4_dfs3$permit_info_from_db,
    join_by(vessel_official_number,
            permit_sep_u == top)
  )

vessel_in_metrics_report_not_in_permit_info_from_db <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_official_number
  )

length(vessel_in_metrics_report_not_in_permit_info_from_db)
# 5
# 27 after 2022 and sep permits

vessel_in_metrics_report_not_in_permit_info_from_db_alt <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info_from_db$vessel_alt_num
  )

length(vessel_in_metrics_report_not_in_permit_info_from_db_alt)
# 161
# 185 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_metrics_report <-
  setdiff(
    all_4_dfs3$permit_info_from_db$vessel_official_number,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_metrics_report)
# 10460
# 293 after 2022 and sep permits

vessel_in_permit_info_from_db_not_in_metrics_report_alt <-
  setdiff(
    all_4_dfs3$permit_info_from_db$vessel_alt_num,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_from_db_not_in_metrics_report_alt)
# 10596
# 448 after 2022 and sep permits


