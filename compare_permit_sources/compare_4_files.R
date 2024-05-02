# compare this 4 files (permits)
# 1) compliance report downloaded from FHIER (= complaince module)
# 2) logbooks from the Oracle db all_logbooks... (has 3 or 4 letters coded permit types)
# 3) Metrics tracking from FHIER
# 4) permit info from the Oracle db
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

## 4) permit table from the Oracle db ----
dates_filter <- " (end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
  AND effective_date <= TO_DATE('01-JAN-23', 'dd-mon-yy')
"

mv_sero_fh_permits_his_query_file_path <-
  file.path(my_paths$inputs, "get_db_data", "permit_info_2022.rds")

# file.exists(mv_sero_fh_permits_his_query_file_path)
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

permit_info <- get_permit_info()
# File: permit_info_2022.rds modified 2024-01-23 12:43:12.146822
# File: permit_info_2022.rds modified 2024-02-27 09:55:09.126511

nrow(permit_info)
# [1] 183855
# [1] 20777    2022 only
# [1] 15807

### check dates ----
# dates_filter <- " (end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
#     OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
#   AND effective_date <= TO_DATE('01-JAN-23', 'dd-mon-yy')
# "

min(permit_info$EXPIRATION_DATE)
# [1] "2007-01-31 EST"
# [1] "2007-02-28 EST"

permit_info |>
  filter(EXPIRATION_DATE == "2007-02-28 EST") |>
  # $ VESSEL_ID            <chr> "FL3270FN"
  # $ END_DATE             <dttm> 2022-08-25 01:00:00
  # filter(EXPIRATION_DATE == "2007-01-31 EST") |>
# $ VESSEL_ID            <chr> "514001"
# END_DATE == 2022-02-24 !!!
  glimpse()

# TODO why min() doesn't work?
permit_info$END_DATE |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-21 EST"

max(permit_info$EFFECTIVE_DATE)
# [1] "2023-01-01 EST"
# [1] "2022-12-30 EST"

## all 4 dataframes ----
# llist is like list except that it preserves the names or labels of the component variables in the variables label attribute.
all_4_dfs <-
  Hmisc::llist(compliance_from_fhier,
    db_logbooks,
    metrics_report,
    permit_info)

# View(all_4_dfs)

# str(all_4_dfs$db_logbooks)

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
    grep("vessel|permit|exp|effect|end_date",
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
                    "vessel_name"
                  ))) |>
           remove_empty_cols() |>
           distinct()
       }
  )

# View(all_4_dfs2)

# save the df
all_4_dfs3 <- all_4_dfs2

## individual df preparations ----

### compliance_from_fhier: split permit column ----

temp_compliance_from_fhier <-
  all_4_dfs2$compliance_from_fhier |>
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

# short_compliance_from_fhier_multi_permitgroups |>
#   diffdf::diffdf(short_compliance_from_fhier_multi_permitgroups1)
# # No issues were found!

# short_compliance_from_fhier_multi_permitgroups1 <-
#   short_compliance_from_fhier_to_test |>
#   group_by(vessel_official_number) |>
#   mutate(multiple_permitgroups = +(n_distinct(permitgroup) > 1)) %>%
#   ungroup() |>
#   filter(multiple_permitgroups > 0)

# short_compliance_from_fhier_multi_permitgroups |>
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

# for the future use
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

### db_logbooks: unify vessel ids ----
# grep("vessel", names(all_4_dfs3$db_logbooks), value = T)
# [1] "vessel_id"           "vessel_official_nbr" "vessel_name"
# [4] "sero_vessel_permit"  "garfo_vessel_permit"

all_4_dfs3$db_logbooks <-
  all_4_dfs3$db_logbooks |>
  rename("vessel_official_number" = "vessel_official_nbr",
         "db_logbooks_vessel_id" = "vessel_id") # needed to see if NA in the full join

### permit_info: unify vessel ids ----

# grep("vessel", names(all_4_dfs3$permit_info), value = T)
# [1] "vessel_id"      "vessel_alt_num"

nrow(all_4_dfs3$permit_info)
# 20730

all_4_dfs3$permit_info <-
  all_4_dfs3$permit_info |>
  mutate(permit_info_vessel_id = vessel_id) |> # want to keep it to see if NA in the full join
  rename("vessel_official_number" = "vessel_id")

### permit_info groups to keep ----
# permit_info |>
#   select(TOP, PERMIT_GROUP) |>
#   distinct() |>
#   View()

all_4_dfs3$permit_info |>
  filter(tolower(top) %in% tolower(all_permits_in_metrics$permit_sep_u)) |>
  select(permit_group) |> distinct()
#   permit_group
# 1            7

all_4_dfs3$permit_info |>
filter(permit_group == 6) |>
  select(top) |>
           distinct()
# GC

# permit_info |>
# filter(PERMIT_GROUP == 6) |>
#   select(starts_with("TOP")) |>
#            distinct()
#   TOP                   TOP_NAME
# 1  GC SOUTH ATLANTIC GOLDEN CRAB

all_4_dfs3$permit_info <-
  all_4_dfs3$permit_info |>
  filter(permit_group == 7)

nrow(all_4_dfs3$permit_info)
# 16073

# stopped presenting here
# check
setdiff(all_permits_in_metrics$permit_sep_u,
        all_4_dfs3$permit_info$top)
# 0 both ways, ok

min(all_4_dfs3$permit_info$expiration_date)
# [1] "2007-02-28 EST"

all_4_dfs3$permit_info$end_date |>
  sort() |>
  unique() |>
  head(1)
# [1] "2021-01-21 EST"

max(all_4_dfs3$permit_info$effective_date)
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
# 2

glimpse(vessel_in_logb_not_in_compl)
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

## [3] "compliance_from_fhier" "permit_info" ----
file_name_combinations[,3]

# print_df_names(all_4_dfs3$compliance_from_fhier)
# print_df_names(all_4_dfs3$permit_info)

join_compliance_from_fhier__permit_info <-
  full_join(
    all_4_dfs3$compliance_from_fhier,
    all_4_dfs3$permit_info,
    join_by(vessel_official_number,
            permit_sep_u == top)
  )
# View(join_compliance_from_fhier__permit_info)

# no many-to-many after adding permit_sep_u == top!
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 74370 of `y` matches multiple rows in `x`.

### why multiple? ----
# 1) x to y
# ℹ Row 1 of `x` matches multiple rows in `y`.
all_4_dfs3$permit_info |>
  filter(vessel_official_number ==
    all_4_dfs3$compliance_from_fhier[1,][["vessel_official_number"]] |
      vessel_alt_num ==
    all_4_dfs3$compliance_from_fhier[1,][["vessel_official_number"]]) |>
  glimpse()

all_4_dfs3$compliance_from_fhier |>
  filter(vessel_official_number == "579608") |>
  nrow()
# 0

all_4_dfs3$permit_info |>
  filter(vessel_official_number == "579608") |>
  nrow()
# 27 (multiple permits, ok)
# 0 after join by permits

# 2) y to x
# ℹ Row 74282 of `y` matches multiple rows in `x`.
all_4_dfs1$permit_info[74282,]

all_4_dfs3$permit_info |>
  filter(vessel_official_number ==
    all_4_dfs3$compliance_from_fhier[74282,][["vessel_official_number"]] |
      vessel_alt_num ==
    all_4_dfs3$compliance_from_fhier[74282,][["vessel_official_number"]]) |>
  nrow()
# 0

vessel_in_compl_not_in_permit_info <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info$vessel_official_number
  )

length(vessel_in_compl_not_in_permit_info)
# 3687
# 13 after +id
# 10 after 2022 and permit group 7

vessel_in_compl_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$compliance_from_fhier$vessel_official_number,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_compl_not_in_permit_info_alt)
# 171
# 169 after 2022 and permit group 7

vessel_in_permit_info_not_in_compl <-
  setdiff(
    all_4_dfs3$permit_info$vessel_official_number,
    all_4_dfs3$compliance_from_fhier$vessel_official_number
  )

length(vessel_in_permit_info_not_in_compl)
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
# 42

vessel_in_metrics_report_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_number
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
    join_by(vessel_official_number)
  )
#   Detected an unexpected many-to-many relationship between `x` and `y`.

### why multiple? ----
# 1) x to y
# ℹ Row 1 of `x` matches multiple rows in `y`.
all_4_dfs3$permit_info |>
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
    all_4_dfs3$permit_info[9426,][["vessel_official_number"]] |
      vessel_official_number ==
    all_4_dfs3$permit_info[9426,][["vessel_alt_num"]]) |>
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

vessel_in_db_logbooks_not_in_permit_info <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$permit_info$vessel_official_number
  )

length(vessel_in_db_logbooks_not_in_permit_info)
# 1
# 2 after 2022 and sep permits

# 1292480
# 1292480:NC0676EK........ SOUTHERN RUN - BENJAMIN AUGUSTUS MORRIS  (828) 4298076

all_4_dfs3$permit_info |>
  filter(vessel_official_number == "NC0676EK") |>
  nrow()
# [1] 18
# 3 after 2022 and sep permits

all_4_dfs3$db_logbooks |>
  filter(vessel_official_number == "1292480" |
           vessel_official_number == "NC0676EK") |>
  glimpse()
# In db_logbooks 1292480 only. (PIMS "No items available", Official Number From USCG Certificate Of Documentation)
# In permit_info NC0676EK only.

vessel_in_db_logbooks_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$db_logbooks$vessel_official_number,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_db_logbooks_not_in_permit_info_alt)
# 92
# 91 after 2022 and sep permits

vessel_in_permit_info_not_in_db_logbooks <-
  setdiff(
    all_4_dfs3$permit_info$vessel_official_number,
    all_4_dfs3$db_logbooks$vessel_official_number
  )

length(vessel_in_permit_info_not_in_db_logbooks)
# 12176
# 1988 after 2022 and sep permits

vessel_in_permit_info_not_in_db_logbooks_alt <-
  setdiff(
    all_4_dfs3$permit_info$vessel_alt_num,
    all_4_dfs3$db_logbooks$vessel_official_number
  )

length(vessel_in_permit_info_not_in_db_logbooks_alt)
# 12247
# 2074 after 2022 and sep permits

## [6] "metrics_report" "permit_info" ----
file_name_combinations[,6]

# print_df_names(all_4_dfs3$metrics_report)
# print_df_names(all_4_dfs3$permit_info)

join_metrics_report__permit_info <-
  full_join(
    all_4_dfs3$metrics_report,
    all_4_dfs3$permit_info,
    join_by(vessel_official_number,
            permit_sep_u == top)
  )

vessel_in_metrics_report_not_in_permit_info <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info$vessel_official_number
  )

length(vessel_in_metrics_report_not_in_permit_info)
# 5
# 27 after 2022 and sep permits

vessel_in_metrics_report_not_in_permit_info_alt <-
  setdiff(
    all_4_dfs3$metrics_report$vessel_official_number,
    all_4_dfs3$permit_info$vessel_alt_num
  )

length(vessel_in_metrics_report_not_in_permit_info_alt)
# 161
# 185 after 2022 and sep permits

vessel_in_permit_info_not_in_metrics_report <-
  setdiff(
    all_4_dfs3$permit_info$vessel_official_number,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_not_in_metrics_report)
# 10460
# 293 after 2022 and sep permits

vessel_in_permit_info_not_in_metrics_report_alt <-
  setdiff(
    all_4_dfs3$permit_info$vessel_alt_num,
    all_4_dfs3$metrics_report$vessel_official_number
  )

length(vessel_in_permit_info_not_in_metrics_report_alt)
# 10596
# 448 after 2022 and sep permits

