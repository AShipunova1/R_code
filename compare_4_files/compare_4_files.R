# compare this 4 files (permits)
# 1) compliance report downloaded from FHIER (= complaince module)
# 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types)
# 3) Metrics tracking from FHIER
# 4) joined permit and vessel tables from the Oracle db
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

db_logbooks_query <-
  "SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_start_date >= '01-JAN-2022'
  AND trip_start_date <= '31-DEC-2022'
"

db_logbooks_file_name <-
  file.path(curr_proj_input_path,
                      "logbooks22.rds")

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
# 2024-01-19 run for logbooks22.rds: 147.9 sec elapsed
# File: logbooks22.rds modified 2024-01-19 17:14:31.881392

dim(db_logbooks)
# [1] 327869    149

## 3) Metrics tracking from FHIER ----
metrics_report_file_name <-
  r"(Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source).csv)"

metrics_report_file_path <-
  file.path(curr_proj_input_path,
            metrics_report_file_name)

file.exists(metrics_report_file_path)
# T

metrics_report <- read_csv(metrics_report_file_path)

dim(metrics_report)
# [1] 3606   13

## permit table from the Oracle db ----

# ## 4) joined permit and vessel tables from the Oracle db ----

# # Need both tables to get the vessel official numbers
# # get_vessels with permits 2021+ ----
#
# dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
#     OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
#   AND effective_date <= CURRENT_DATE
# "
# # Use that "dates_filter" in all parts of the union below.
#
# # The 3 part union is needed because while the permit table has only one vessel id, the vessel table has 3 different columns for that (sero_official_number, coast_guard_nbr and state_reg_nbr) and we want to join tables by all 3 in turn.
# # stringr::str_glue is a function that allows you to create strings with placeholders for variable values. It works by using curly braces {} to enclose variable names within a string.
# vessels_permits_query <-
#   stringr::str_glue("SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = sero_official_number )
# WHERE {dates_filter}
# UNION ALL
# SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = coast_guard_nbr )
# WHERE
#   {dates_filter}
# UNION ALL
# SELECT
#   *
# FROM
#        srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
#   JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
#   ON ( p.vessel_id = state_reg_nbr )
# WHERE
# {dates_filter}
# ")
#
# vessels_permits_file_path <-
#   file.path(my_paths$inputs, "get_db_data", "vessels_permits.rds")
#
# vessels_permits_fun <-
#   function(vessels_permits_query) {
#     return(dbGetQuery(con,
#                       vessels_permits_query))
#   }
#
# get_vessels_permits <-
#   function() {
#     read_rds_or_run(vessels_permits_file_path,
#                     vessels_permits_query,
#                     vessels_permits_fun) |>
#       vessels_permits_id_clean()
#   }
#
# vessels_permits <- get_vessels_permits()
# # File: vessels_permits.rds modified 2024-01-02 10:27:22.824263

dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
  AND effective_date <= CURRENT_DATE
"

mv_sero_fh_permits_his_query_file_path <-
  file.path(my_paths$inputs, "get_db_data", "permit_info.rds")

# file.exists(file_name_permits)
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
                    mv_sero_fh_permits_his_query_fun)
  }

permit_info <- get_permit_info()
# File: permit_info.rds modified 2023-10-19 09:34:47.729631

dim(permit_info)
# [1] 183855     22

## all 4 dataframes ----
# llist is like list except that it preserves the names or labels of the component variables in the variables label attribute.
all_4_dfs <-
  Hmisc::llist(compliance_from_fhier,
    db_logbooks,
    metrics_report,
    permit_info)

# str(all_4_dfs$db_logbooks)
all_4_df_names <- names(all_4_dfs)

# prepare data for comparison ----
## clean_headers ----
all_4_dfs1 <- map(all_4_dfs, clean_headers)
# View(all_4_dfs1)

## keep only vessel_id and permit columns ----

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
    grep("vessel|permit|exp|effect|end_date",
         my_df_names,
         value = TRUE,
         ignore.case = TRUE)
  }

### Apply the names_to_keep function to each dataframe in all_4_dfs1 ----
col_names_to_keep <- map(all_4_dfs1, names_to_keep)

# Explanation:
# 1. The 'imap' function iterates over each element (data frame) in the 'all_4_dfs1' list along with its index.
# 2. For each data frame, the function inside 'imap' is executed.
# 3. The 'select' function is used to choose specific columns from the data frame 'x'.
# 4. The column names to be kept are specified by 'col_names_to_keep[[idx]]'.
# 5. The pipeline operator '|>' is used to pass the result to the next operation.
# 6. The 'select' function is again used to exclude columns containing the substring "trip".
# 7. The result of this selective transformation is stored in the 'all_4_dfs2' list.
all_4_dfs2 <-
  imap(all_4_dfs1,
       function(x, idx)
       {
         select(x,
                col_names_to_keep[[idx]]) |>
           select(-contains("trip")) |>
           remove_empty_cols() |>
           distinct()
       }
  )

# View(all_4_dfs2)

# save the df
all_4_dfs3 <- all_4_dfs2

## individual df preparations ----

### rm an unused column ----
all_4_dfs3$compliance_from_fhier <-
  all_4_dfs2$compliance_from_fhier |>
  select(-gom_permitteddeclarations__) |>
  distinct()

### compliance_from_fhier: split permit column ----

all_4_dfs3$compliance_from_fhier <-
  all_4_dfs3$compliance_from_fhier |>
  mutate(permitgroup_sep_0 =
           gsub("\\(([^)]+)\\)", "\\1,", permitgroup)
  ) |>
    mutate(permitgroup_sep_1 =
           gsub(",,+", ",", permitgroup_sep_0)
  ) |>
    mutate(permitgroup_sep =
           gsub(",$", "", permitgroup_sep_1)
  ) |>
      # filter(vessel_official_number == 'FL6900MH') |> View()
  # !!! 3
  mutate(permitgroup_sep_s =
           str_split(permitgroup_sep, ",")
  ) |>
  rowwise() |>
  mutate(permitgroup_sep_u =
           list(sort(unique(permitgroup_sep_s)))
         ) |>
    # mutate(a = list(stringi::stri_remove_empty(permitgroup_sep_u))) |>
    # filter(vessel_official_number == 'FL6900MH') |> View()
  mutate(permitgroup_sep_u_str =
           permitgroup_sep_u |>
           stringi::stri_paste(sep = ',', collapse = ',')
       ) |>
  ungroup() |>
  separate_wider_delim(cols = permitgroup_sep_u_str,
                       delim = ",",
                       names_sep = "__",
                       too_few = "align_start"
                       )

# View(all_4_dfs3$compliance_from_fhier)

all_4_dfs3$compliance_from_fhier <-
  all_4_dfs3$compliance_from_fhier |>
  select(
    vessel_official_number,
    permit_groupexpiration,
    # permitgroup,
    contains("__")
  ) |>
  distinct()

# View(all_4_dfs3$compliance_from_fhier)

#### check diff permitgroup for the same vessel ----
short_compliance_from_fhier_to_test <-
  all_4_dfs2$compliance_from_fhier |>
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
# [1] 3691   19
n_distinct(all_4_dfs3$compliance_from_fhier$vessel_official_number)

# Explanation:
# 1. The pipeline operator '|>' applies a sequence of operations to the 'short_compliance_from_fhier_to_test' data frame.
# 2. 'group_by(vessel_official_number)' groups the data frame by the 'vessel_official_number' column.
# 3. 'mutate(multiple_permitgroups = +(n_distinct(permitgroup) > 1))' adds a new column 'multiple_permitgroups'.
#    - 'n_distinct(permitgroup)' calculates the number of distinct permit groups for each vessel.
#    - '> 1' checks if there are more than 1 distinct permit groups.
#    - '+(...)' converts the logical result to 0 or 1.
# 4. 'ungroup()' removes the grouping to work with the entire data frame.
# 5. 'filter(multiple_permitgroups > 0)' filters rows where 'multiple_permitgroups' is greater than 0, keeping only vessels with multiple distinct permit groups.
short_compliance_from_fhier_to_test_res <-
  short_compliance_from_fhier_to_test |>
  group_by(vessel_official_number) |>
  mutate(multiple_permitgroups = +(n_distinct(permitgroup) > 1)) %>%
  ungroup() |>
  filter(multiple_permitgroups > 0)

# short_compliance_from_fhier_to_test_res |>
# write_csv(
#     file.path(
#       curr_proj_output_path,
#       "compliance_from_fhier__multiple_permitgroups.csv"
#     )
#   )

### metrics_report: split permit column ----
all_4_dfs3$metrics_report <-
  all_4_dfs2$metrics_report |>
  mutate(permits_trim =
           gsub(" ", "", permits)
  ) |>
  separate_wider_delim(
    cols = permits_trim,
    delim = ";",
    names_sep = "__",
    too_few = "align_start",
    cols_remove = F
  )

# str(all_4_dfs3$metrics_report)

# get pairs ----
file_name_combinations <-
  combn(all_4_df_names, 2)

# compare each pair ----
## [1] "compliance_from_fhier" "db_logbooks" ----
file_name_combinations[,1]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$db_logbooks)

join_compliance_from_fhier__db_logbooks <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$db_logbooks,
    join_by(vessel_official_number == vessel_official_nbr)
  )

# View(join_compliance_from_fhier__db_logbooks)

### vessel is in compliance_from_fhier, not in db_logbooks ----

vessel_in_compl_not_in_logb <-
  join_compliance_from_fhier__db_logbooks |>
  filter(is.na(vessel_id)) |>
  select(vessel_official_number) |>
  distinct()

dim(vessel_in_compl_not_in_logb)
# 1803

vessel_in_compl_not_in_logb <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_nbr
  )
length(vessel_in_compl_not_in_logb)
# 1803

vessel_in_logb_not_in_compl <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_nbr,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )
length(vessel_in_logb_not_in_compl)
# 2

glimpse(vessel_in_compl_not_in_logb)
# chr [1:2] "1038780" "1292480"
# "1292480/NC0676EK"

# all_4_dfs3$db_logbooks |>
#   filter(vessel_official_nbr == "NC0676EK")
# 0
#

## [2] "compliance_from_fhier" "metrics_report" ----
file_name_combinations[,2]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$metrics_report)

join_compliance_from_fhier__metrics_report <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$metrics_report,
    join_by(vessel_official_number)
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
#


## [3] "compliance_from_fhier" "permit_info" ----
file_name_combinations[,3]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$permit_info)

join_compliance_from_fhier__permit_info <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$permit_info,
    join_by(vessel_official_number == vessel_id)
  )
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 74282 of `y` matches multiple rows in `x`.

vessel_in_compl_not_in_permit_info <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info$vessel_official_number
  )

length(vessel_in_compl_not_in_permit_info)
# 3687

vessel_in_compl_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_compl_not_in_permit_info_alt)
# 171

vessel_in_permit_info_not_in_compl <-
  setdiff(
    all_4_dfs3$permit_info$vessel_official_number,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_permit_info_not_in_compl)
# 0

## [4] "db_logbooks" "metrics_report" ----
file_name_combinations[,4]

# print_df_names(all_4_dfs3$db_logbooks)
# print_df_names(all_4_dfs3$metrics_report)

join_db_logbooks__metrics_report <-
  full_join(
    all_4_dfs3$db_logbooks,
    all_4_dfs3$metrics_report,
    join_by(vessel_official_nbr == vessel_official_number)
  )

vessel_in_db_logbooks_not_in_metrics_report <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_nbr,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_db_logbooks_not_in_metrics_report)
# 42

vessel_in_metrics_report_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_nbr
  )

length(vessel_in_metrics_report_not_in_db_logbooks)
# 1762

## [5] "db_logbooks" "permit_info" ----
file_name_combinations[,5]

# print_df_names(all_4_dfs3$db_logbooks)
# print_df_names(all_4_dfs3$permit_info)

join_db_logbooks__permit_info <-
  full_join(
    all_4_dfs3$db_logbooks,
    all_4_dfs3$permit_info,
    join_by(vessel_official_nbr == vessel_id)
  )
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 111110 of `y` matches multiple rows in `x`.

vessel_in_db_logbooks_not_in_permit_info <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_nbr,
    all_4_dfs3$permit_info$vessel_id
  )

length(vessel_in_db_logbooks_not_in_permit_info)
# 1

vessel_in_db_logbooks_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_nbr,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_db_logbooks_not_in_permit_info_alt)
# 92

vessel_in_permit_info_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$permit_info$vessel_id,
    all_4_dfs3$db_logbooks$vessel_official_nbr
  )

length(vessel_in_permit_info_not_in_db_logbooks)
# 12176


vessel_in_permit_info_not_in_db_logbooks_alt <-
  setdiff(
    all_4_dfs3$permit_info$vessel_alt_num,
    all_4_dfs3$db_logbooks$vessel_official_nbr
  )

length(vessel_in_permit_info_not_in_db_logbooks_alt)
# 12247

file_name_combinations[,6]


## [6] "metrics_report" "permit_info" ----
file_name_combinations[,6]

# print_df_names(all_4_dfs3$metrics_report)
# print_df_names(all_4_dfs3$permit_info)

join_metrics_report__permit_info <-
  full_join(
    all_4_dfs3$metrics_report,
    all_4_dfs3$permit_info,
    join_by(vessel_official_number == vessel_id)
  )

vessel_in_metrics_report_not_in_permit_info <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info$vessel_id
  )

length(vessel_in_metrics_report_not_in_permit_info)
# 5

vessel_in_metrics_report_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_metrics_report_not_in_permit_info_alt)
# 161

vessel_in_permit_info_not_in_metrics_report <-
  setdiff(
    all_4_dfs3$permit_info$vessel_id,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_not_in_metrics_report)
# 10460

vessel_in_permit_info_not_in_metrics_report_alt <-
  setdiff(
    all_4_dfs3$permit_info$vessel_alt_num,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_not_in_metrics_report_alt)
# 10596

