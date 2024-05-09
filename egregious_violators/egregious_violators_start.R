# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired and were active for the same period as (1);
# 3) the grace period is 7 days back from today.
# 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.
# 5) not counting any correspondence (regardless of the type - email/call, voicemail or not) that includes "No contact made" in the text of the entry as a actual "direct" contact for any egregious vessel (May 6 2024)

# NB. Update (download) all input files every time before run.

# ----set up----

# Get common functions
# source("~/R_code_github/useful_functions_module.r")
# install.packages("~/R_code_github/auxfunctions_1.0.tar.gz",
#                  repos = NULL,
#                  type = "source")

# update.packages("~/R_code_github/auxfunctions_1.0.tar.gz")
library(auxfunctions)
library(ROracle)
# library(dplyr)
# library(stringr)
# library(lubridate)
# library(purrr)
# library(readr)
# library(ROracle)
# library(tictoc)
# library(tibble)
library(zoo)
library(diffdf)
# library(RColorBrewer)

my_paths <- set_work_dir()

current_project_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

current_project_name <- current_project_basename

all_inputs <- my_paths$inputs

my_year1 <- "2023"
my_beginning1 <- stringr::str_glue("{my_year1}-01-01")
my_end1 <- stringr::str_glue("{my_year1}-12-31")

my_year2 <- "2024"
my_beginning2 <- stringr::str_glue("{my_year2}-01-01")
my_end2 <- stringr::str_glue("{my_year2}-12-31")

data_file_date <- 
  lubridate::today()
  # lubridate::ymd("2024-02-21")
  
number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7
# 182

grace_period = 7 #days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period
# [1] "2023-10-04"

# check week and day of the period's start
# week("2023-10-04") 
# 40

# wday("2023-10-04",
#      label = T)
# Wed

# 30 days from today
permit_expired_check_date <- data_file_date + 30

last_week_start <- data_file_date - grace_period

# get_data ----

get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)

# compl_clean
# corresp_contact_cnts_clean0
# prev_result
# processed_metrics_tracking_permits
# fhier_addresses
# processed_pims_home_ports
# db_participants_address
# vessels_permits_participants

# ---- Preparing compliance info ----

## Permit Expiration ----
### ---- add permit_expired column ----
# Explanations:
# 1. Add a new column 'permit_expired' using 'mutate'.
# 2. Use 'case_when' to determine if 'permit_groupexpiration' is greater than permit_expired_check_date.
# 3. If true, set 'permit_expired' to "no", otherwise set it to "yes".

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is after permit_expired_check_date than "not expired"
  dplyr::mutate(permit_expired =
           dplyr::case_when(
             permit_groupexpiration > permit_expired_check_date ~ "no",
             .default = "yes"
           ))

# glimpse(compl_clean_w_permit_exp)

### get only not expired last 27 weeks of data minus grace period ----
compl_clean_w_permit_exp__not_exp <-
  compl_clean_w_permit_exp |>
  # the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_end < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

min(compl_clean_w_permit_exp__not_exp$permit_groupexpiration)
# [1] "2024-02-29 EST"

min(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2023-08-14"

max(compl_clean_w_permit_exp__not_exp$week_start)
# [1] "2024-01-29"

max(compl_clean_w_permit_exp__not_exp$week_end)
# [1] "2024-02-04"

## ---- add year_month column ----

compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp__not_exp |>
  dplyr::mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last check period
  dplyr::filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)

dim(compl_clean_w_permit_exp_last_half_year)

## ---- Have only SA and dual permits ----
# Use 'filter' to select rows where 'permitgroup' contains "CDW", "CHS", or "SC".
compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  dplyr::filter(grepl("CDW|CHS|SC", permitgroup))

# today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"
# [1] "2024-02-16"
# [1] "2024-04-09"

dim(compl_clean_w_permit_exp_last_half_year__sa)

## fewer columns ----
remove_columns <- c(
  "name",
  "gom_permitteddeclarations__",
  "captainreports__",
  "negativereports__",
  "complianceerrors__",
  "set_permits_on_hold_",
  "override_date",
  "override_by",
  "contactedwithin_48_hours_",
  "submittedpower_down_",
  "permit_expired"
)

# Explanations:
# 1. Use 'select' to remove columns specified in 'remove_columns'.
# 2. Use 'distinct' to keep only unique rows in the resulting data frame.
compl_clean_w_permit_exp_last_half_year__sa__short <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::select(-dplyr::any_of(remove_columns)) |> 
  dplyr::distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa__short)

## work with the whole period ----
## add compliant_after_overr ----

tictoc::tic("compl_overr")
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr <-
  compl_clean_w_permit_exp_last_half_year__sa__short |>
  add_compliant_after_override(overridden_col_name = "overridden_",
                               compliance_col_name = "compliant_")
tictoc::toc()
# compl_overr: 8.76 sec elapsed

# check
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |> 
  dplyr::select(compliant_, overridden_, compliant_after_override) |>
  dplyr::count(compliant_, overridden_, compliant_after_override)
#   compliant_ overridden_ compliant_after_override     n
#   <chr>      <chr>       <chr>                    <int>
# 1 NO         NO          no                       11258
# 2 NO         YES         yes                         70
# 3 YES        NO          yes                      29628

# check
compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$compliant_after_override |> 
  unique()
# [1] "yes" "no" 

dim(compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr)

# check
dplyr::n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number) ==
  dplyr::n_distinct(
    compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr$vessel_official_number
  )
# T

## get only non-compliant for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa__short__comp_after_overr |>
  # not compliant
  dplyr::filter(tolower(compliant_after_override) == "no")

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c)

## keep only vessels with info for all weeks in the period ----
all_weeks_num <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  dplyr::select(week) |>
  dplyr::distinct() |>
  nrow()

# Explanations:
# 1. Group the data frame by 'vessel_official_number'.
# 2. Filter the groups based on the condition that the number of distinct weeks is greater than or equal to 'all_weeks_num'.
# 3. Remove the grouping from the data frame.
# 4. Exclude the 'week' column from the resulting data frame, we don't need it anymore.

compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(dplyr::n_distinct(week) >= all_weeks_num) |> 
  dplyr::ungroup() |> 
  dplyr::select(-week)

compl_clean_w_permit_exp_last_half_year__sa_non_c |> dim()

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present)

## check the last report date ----
### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids)

### check these ids in the full compliance information ----
compl_clean_w_permit_exp_last_half_year__sa |>
  dplyr::filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present__vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # [1] 1938   22
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(
    tolower(compliant_) == "yes" &
      tolower(overridden_) == "yes" &
      # not the current month
      year_month < as.yearmon(data_file_date)
  ) |>
  nrow()
# 0 OK!

# Results: prepared Compliance is in compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present

# ---- Preparing Correspondence ----

## ---- remove 999999 ----
# Explanations:
# Create a new data frame 'corresp_contact_cnts_clean' by filtering 'corresp_contact_cnts_clean0' based on the condition.
# 1. Use 'filter' to select rows where 'vessel_official_number' does not start with "99999".
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  dplyr::filter(!grepl("^99999", vessel_official_number))

dplyr::n_distinct(corresp_contact_cnts_clean$vesselofficial_number)

# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

## new requirement 2024-02-26 ----
# It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

## new requirement 2024-05-06 ----
# Exclude any correspondence (regardless of the type - email/call, voicemail or not) that includes "No contact made" in the text of the entry as a actual "direct" contact for any egregious vessel.

# check
corresp_contact_cnts_clean |>
  dplyr::select(calltype, voicemail, contacttype) |>
  dplyr::distinct() |> head(10)

## Filters ----
# The functions below are creating filter conditions using quosures. Quosures are a part of tidy evaluation in R, allowing expressions to be captured without evaluation, which is useful for creating functions with flexible inputs.

we_called_filter <-
  dplyr::quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))

we_emailed_once_filter <-
  dplyr::quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outgoing"
  ))

# Explanations:
# 
# **Expression inside quo()**:
#    - `!grepl("No contact made", contactcomments, ignore.case = TRUE)`: This expression is a negation of the `grepl` function, which is used to search for a pattern ("No contact made") in the `contactcomments` column.
#    - `grepl()` returns `TRUE` for each element of `contactcomments` that contains the pattern, and `FALSE` otherwise.
#    - The `!` operator negates the result, so the filter condition will be `TRUE` for rows where "No contact made" is not found in the `contactcomments` column.
# 
# The `exclude_no_contact_made_filter` function effectively creates a filter condition that can be used to exclude rows where "No contact made" is found in the `contactcomments` column when applied to a dataset.
exclude_no_contact_made_filter <-
  dplyr::quo(!grepl("No contact made", 
            contactcomments, 
            ignore.case = TRUE))

# don't need a second contact
they_contacted_direct_filter <-
  dplyr::quo(
    any(
      tolower(calltype) == "incoming"
      )
  )

# corresp_filter <-
#   quo(!!they_contacted_direct_filter |
#         (
#           contact_freq > 1 &
#             (!!we_called_filter &
#                !!we_emailed_once_filter)
#         ))

# calltype voicemail contacttype

# two_attempts_filter <-
#   quo(contact_freq > 1 &
#         any(tolower(contacttype) == "call"))

### use the filters ----
corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  # select(calltype) |> distinct()
  dplyr::filter(tolower(calltype) == "incoming" |
           (
             contact_freq > 1 &
               (!!we_called_filter &
                  !!we_emailed_once_filter)
           )) |> 
  dplyr::filter(!!exclude_no_contact_made_filter)

dim(corresp_contact_cnts_clean)
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)

dplyr::n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)

## fix dates ----
# check
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1) |> str()
 # chr "02/15/2024 03:15PM"

# Explanations:
# Mutate new columns 'created_on_dttm' and 'contact_date_dttm' by parsing 'created_on' and 'contact_date' using lubridate package.
# The date-time formats considered are "mdY R".
# 1. Use the pipe operator to pass 'corresp_contact_cnts_clean_direct_cnt_2atmps' as the left-hand side of the next expression.
# 2. Use 'mutate' to create new columns with parsed date-time values.
# 3. Use 'lubridate::parse_date_time' to parse the date-time values using the specified formats.

corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  mutate(
    created_on_dttm =
      lubridate::parse_date_time(created_on,
                                 c("mdY R")),
    contact_date_dttm =
      lubridate::parse_date_time(contact_date,
                                 c("mdY R"))
  )

# check
str(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm)
# POSIXct[1:29089], format: "2024-02-15 15:15:00" 

# preprared Correspondence is in 
# corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates

# Join correspondence with compliance ----
# Explanations:
# Create a new dataframe 'compl_corr_to_investigation' by performing an inner join between
# 'correspondence' and 'compliance'.
# The join is performed on the column 'vessel_official_number'.
# Use 'multiple = "all"' and 'relationship = "many-to-many"' to handle multiple matches during the join.
# 1. Use the 'inner_join' function from the dplyr package to combine the two dataframes based on the specified columns.
# 2. Pass the column names and other parameters to the 'by', 'multiple', and 'relationship' arguments.

compl_corr_to_investigation <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_non_c__all_weeks_present,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation)

# check
n_distinct(compl_corr_to_investigation$vesselofficial_number)

# View(compl_corr_to_investigation)

## save number of vessels to investigate for checks ----
num_of_vsl_to_investigate <- 
  n_distinct(compl_corr_to_investigation$vesselofficial_number)

# Results: Compl & corresondence together are in
# compl_corr_to_investigation

# ---- output needed investigation ----
# 1. remove unused columns
# 2. create additional columns
# 3. mark vessels already in the know list (prev_result)
# 4. duals vs. sa_only

## 1. remove extra columns ----

# Explanations:
# Group the dataframe by the 'vessel_official_number' column and then apply the 'summarise_all' function.
# The 'summarise_all' function applies the specified function (in this case, 'concat_unique') to each column.

# Note: 'concat_unique' is not a standard R function, it is a custom function defined previously.

colnames(compl_corr_to_investigation) |> 
  cat(sep = '",\n"')

unused_fields <- c(
  "vesselofficial_number",
  "primary",
  # "contact_date",
  "follow_up",
  "log_group",
  "calltype",
  "voicemail",
  # "contacttype",
  "contact_reason",
  # "contactrecipientname",
  # "contactphone_number",
  # "contactemailaddress",
  "contactcomments",
  "srfhuser",
  "created_on",
  "follow_up_nbr",
  "srhs_vessel",
  # "vessel_official_number",
  "was_contacted",
  "contact_freq",
  "created_on_dttm",
  # "contact_date_dttm",
  # "name",
  # "permit_expired",
  # "permitgroup",
  # "permit_groupexpiration",
  "compliant_after_override")

# Explanations:
# 1. Exclude columns specified in 'unused_fields' from the data frame.
# 2. Group the data frame by 'vessel_official_number'.
# 3. Apply the custom function 'concat_unique' to all columns to concatenate unique non-missing values into a single string.
# 4. Remove the grouping from the data frame.

compl_corr_to_investigation_short <-
  compl_corr_to_investigation |>
  # compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id |>
  select(-any_of(unused_fields)) |>
  group_by(vessel_official_number) |>
  summarise_all(concat_unique) |>
  ungroup()

# print_df_names(compl_corr_to_investigation_short)

compl_corr_to_investigation_short |> glimpse()

dim(compl_corr_to_investigation_short)

## 2. create additional columns ----
### add list of contact dates and contact type in parentheses  -----

# put names into vars (needed, bc spaces and underscores placements vary from source to source)
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation_short, "contact", "date")[1]

contacttype_field_name <-
  find_col_name(compl_corr_to_investigation_short, "contact", "type")[1]

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation_short, ".*contact", "number.*")[1]

# Explanations:
# Define a function 'get_date_contacttype' that takes a dataframe 'compl_corr_to_investigation' as input.
# Perform several data manipulation steps to extract and organize relevant information.
# 1. Add a new column 'date__contacttype' by concatenating the values from 'contactdate_field_name' and 'contacttype'.
# 2. Select only the 'vessel_official_number' and 'date__contacttype' columns.
# 3. Arrange the dataframe by 'vessel_official_number' and 'date__contacttype'.
# 4. Keep distinct rows based on 'vessel_official_number' and 'date__contacttype'.
# 5. Group the dataframe by 'vessel_official_number'.
# 6. Summarize the data by creating a new column 'date__contacttypes' that concatenates all 'date__contacttype' values for each vessel separated by a comma.
# 7. Return the resulting dataframe.
get_date_contacttype <-
  function(my_df) {
    my_df |>
      # add a new column date__contacttype with contactdate and contacttype
      mutate(date__contacttype =
                      paste(!!sym(contactdate_field_name),
                            !!sym(contacttype_field_name))) |>
      # use 2 columns only
      select(vessel_official_number, date__contacttype) |>
      # sort
      arrange(vessel_official_number, date__contacttype) |>
      distinct() |>
      group_by(vessel_official_number) |>
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = 
                  paste(date__contacttype, collapse = ", ")) %>%
      return()
  }

# use the function
date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation_short)

dim(date__contacttype_per_id)

# glimpse(date__contacttype_per_id)

#### add the new column back ----
compl_corr_to_investigation__corr_date <-
  left_join(compl_corr_to_investigation_short,
            date__contacttype_per_id) |>
  # Joining with `by = join_by(vessel_official_number)`
  # this columns are not longer needed
  select(-all_of(c(
    contactdate_field_name,
    contacttype_field_name
  )))
  
# check
compl_corr_to_investigation__corr_date |> 
  glimpse()

### add pims home port info ----
# compl_corr_to_investigation_short_dup_marked__hailing_port <-
compl_corr_to_investigation__corr_date__hailing_port <- 
  left_join(
    compl_corr_to_investigation__corr_date,
    processed_pims_home_ports,
    join_by(vessel_official_number)
  ) |> 
  rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)

# stopped here with compl_corr_to_investigation__corr_date__hailing_port

### add prepared addresses ----

prep_addresses_path <-
  file.path(current_project_path,
            str_glue("{current_project_basename}_prep_addresses.R"))

file.exists(prep_addresses_path)

source(prep_addresses_path)

# result: compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr

## 3. mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

vessels_to_mark_ids <-
  prev_result |>
  select(vessel_official_number)

dim(vessels_to_mark_ids)

#### mark these vessels ----
# Explanations:
# Create a new column 'duplicate_w_last_time' in the dataframe 'compl_corr_to_investigation_short'.
# This column is marked with "duplicate" for rows where 'vessel_official_number' is present in the list of vessel IDs to mark as duplicates ('vessels_to_mark_ids').
# For all other rows, it is marked as "new".
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked)

### check ----
n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked$vessel_official_number)

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |>
  count(duplicate_w_last_time)
# 1 duplicate               108
# 2 new                      48

## 4. how many are duals? ----
# Explanations:
# Create a new dataframe 
# Use the 'mutate' function to add a new column 'permit_region' based on conditions.
# If 'permitgroup' contains any of the specified patterns ("RCG", "HRCG", "CHG", "HCHG"),
# set 'permit_region' to "dual". Otherwise, set 'permit_region' to "sa_only".
# If none of the conditions are met, set 'permit_region' to "other".
# The resulting dataframe includes the original columns from 'compl_corr_to_investigation_short_dup_marked'
# along with the newly added 'permit_region' column.

compl_corr_to_investigation_short_dup_marked__permit_region <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr__dup_marked |> 
  # compl_corr_to_investigation_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols |>
  mutate(permit_region =
           case_when(
             grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "dual",
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "other"
           ))

# Explanations:
# Use the 'select' function to extract the columns 'vessel_official_number' and 'permit_region'
# from the dataframe 'compl_corr_to_investigation_short_dup_marked__permit_region'.
# Use the 'distinct' function to keep only unique combinations of 'vessel_official_number' and 'permit_region'.
# Use the 'count' function to count the occurrences of each unique 'permit_region'.
# The resulting count provides the frequency of each 'permit_region'.
region_counts <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  select(vessel_official_number, permit_region) |>
  distinct() |>
  count(permit_region)

n_distinct(compl_corr_to_investigation_short_dup_marked__permit_region$vessel_official_number)

### dual permitted cnts ----

region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100

# Print out results ----
## add additional columns in front ----

additional_column_name1 <-
  str_glue(
    "Confirmed Egregious? (permits must still be active till {permit_expired_check_date}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call/other (voicemail counts) and at least 1 email)"
  )

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns <-
  compl_corr_to_investigation_short_dup_marked__permit_region |>
  add_column(
    !!(additional_column_name1) := NA,
    Notes = NA,
    .before = 2
  )

# print_df_names(compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

result_path <- 
  file.path(my_paths$outputs,
            current_project_basename,
            str_glue("egregious_violators_to_investigate_{today()}.csv"))

compl_corr_to_investigation_short_dup_marked__permit_region__add_columns |>
write_csv(result_path)

cat("Result:",
    "compl_corr_to_investigation_short_dup_marked__permit_region__add_columns",
    "and",
    str_glue("egregious_violators_to_investigate_{today()}.csv"),
    sep = "\n")

