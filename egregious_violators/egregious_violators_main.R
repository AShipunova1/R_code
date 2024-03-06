# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired as of today and were active for the same period as (1);
# 3) the grace period is 7 days back from today.
# 4) It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
library(diffdf)
# library(RColorBrewer)

# ----set up----
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
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2024"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

data_file_date <- lubridate::ymd("2024-02-21")
  # today()
# [1] "2024-02-16"

number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7
# 182

grace_period = 7 #days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period
# [1] "2023-08-11"

# 30 days from today
permit_expired_check_date <- data_file_date + 30

last_week_start <- data_file_date - grace_period
# [1] "2024-02-10"
  
# get_data ----

get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)

# ---- Preparing compliance info ----

## Permit Expiration ----
## ---- add permit_expired column ----
# Explanations:
# 1. Add a new column 'permit_expired' using 'mutate'.
# 2. Use 'case_when' to determine if 'permit_groupexpiration' is greater than today's date.
# 3. If true, set 'permit_expired' to "no", otherwise set it to "yes".

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is before permit_expired_check_date than "no"
  mutate(permit_expired =
           case_when(
             permit_groupexpiration > (permit_expired_check_date) ~ "no",
             .default = "yes"
           ))

# glimpse(compl_clean_w_permit_exp)

## ---- add year_month column ----

compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last check period
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)
# today()
# [1] "2023-08-10"
# [1] 235509     22
# [1] "2024-02-16"
# [1] 168740     21

dim(compl_clean_w_permit_exp_last_half_year)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d
# [1] 87826    23
# [1] 74169    23
# [1] 80413    22

## ---- Have only SA and dual permits ----
# Use 'filter' to select rows where 'permitgroup' contains "CDW", "CHS", or "SC".
compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  filter(grepl("CDW|CHS|SC", permitgroup))

today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"
# [1] "2024-02-16"

dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 55194    22

## get only not expired last 27 weeks of data minus grace period ----
compl_clean_w_permit_exp_last_half_year__sa__not_exp <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  # the last 27 week
  filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  filter(week_end < last_week_start) |>
  # not expired
  filter(tolower(permit_expired) == "no")

# View(compl_clean_w_permit_exp_last_half_year__sa__not_exp)
# print_df_names(compl_clean_w_permit_exp_last_half_year__sa__not_exp)

min(compl_clean_w_permit_exp_last_half_year__sa__not_exp$permit_groupexpiration)
# [1] "2024-02-29 EST"

min(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_start)
# [1] "2023-08-14"
max(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_start)
# [1] "2024-01-29"

max(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_end)
# [1] "2024-02-04"

## keep only vessels with info for all weeks in the period ----
all_weeks_num <- 
compl_clean_w_permit_exp_last_half_year__sa__not_exp |>
  # filter(vessel_official_number == "NC5586WD") |> View()
  select(week) |> 
  distinct() |> 
  nrow()

compl_clean_w_permit_exp_last_half_year__sa__not_exp__all_weeks_present <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp |>
  group_by(vessel_official_number) |>
  filter(n_distinct(week) >= all_weeks_num)

compl_clean_w_permit_exp_last_half_year__sa__not_exp |> dim()
# [1] 55194    22

# > compl_clean_w_permit_exp_last_half_year__sa__not_exp |> dim()
# [1] 44756    22
# dim(compl_clean_w_permit_exp_last_half_year__sa__not_exp__all_weeks_present)
# [1] 40275    22

# compl_clean_w_permit_exp_last_half_year__sa__not_exp__all_weeks_present |>
#   filter(vessel_official_number == "NC5586WD") |> dim()
# 0

## fewer columns ----
remove_columns <- c(
  "name",
  "week",
  "gom_permitteddeclarations__",
  "captainreports__",
  "negativereports__",
  "complianceerrors__",
  "set_permits_on_hold_",
  "overridden_",
  "override_date",
  "override_by",
  "contactedwithin_48_hours_",
  "submittedpower_down_",
  "permit_expired"
)

# Explanations:
# 1. Use 'select' to remove columns specified in 'remove_columns'.
# 2. Use 'distinct' to keep only unique rows in the resulting data frame.
compl_clean_w_permit_exp_last_half_year__sa__not_exp_short <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp__all_weeks_present |>
  select(-any_of(remove_columns)) |> 
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 55194    22
dim(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short)
# [1] 40275     9

## work with the whole period ----

# keep only 2 coulmns
compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates <- 
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short |>
  select(vessel_official_number, compliant_) |>
  distinct()

glimpse(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates)
# [1] 3668    2
# Rows: 2,481 (p active all time)

## add no_yes compliant ----
compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide <-
  get_compl_by(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates)

# an empty vector
cols_names <- c()

compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide |>
  compl__back_to_longer_format(cols_names) |>
  filter(stats::complete.cases(is_compl_or_both))
# back_to_long: 21.31 sec elapsed with 22 cols
# back_to_long: 0.87 sec elapsed with 6 cols

compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long$is_compl_or_both |> 
  unique()
# [1] "YES"    "NO"     "NO_YES"

dim(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long)
# [1] 2246    2
# [1] 1917    2
# [1] 1611    2

n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number)
# 2246

n_distinct(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long$vessel_official_number)
# [1] 1917
# [1] 1611    2

## get only all "compliant_ == "NO" for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long |>
  # not compliant
  filter(tolower(is_compl_or_both) == "no")

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp)
# [1] 10419    23
# [1] 9486   23
# [1] 9315   23
# [1] 7138   22
# [1] 141   2
# [1] 328   2
# [1] 228   2

## add back columns needed for the output ----
need_cols_names <- c(
  "vessel_official_number",
  "name",
  "permit_expired",
  "permitgroup",
  "permit_groupexpiration"
  # ,
  # "week_start"
)

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp)
# [1] 328   2
# [1] 228   2

# Explanations:
# Create a new data frame 'compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c' by combining information from 'compl_clean_w_permit_exp_last_half_year__sa' and 'compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp'.
# 1. Use 'select' to keep only the columns specified in 'need_cols_names' in 'compl_clean_w_permit_exp_last_half_year__sa'.
# 2. Use 'inner_join' to perform an inner join with 'compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp' based on the common column 'vessel_official_number'.
# 3. Use 'distinct' to keep only unique rows in the resulting data frame.

compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp) |>
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c)
# [1] 130   8
# 0
# 127
# 121
# [1] 328   6
# [1] 228   6

## check the last report date ----
### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c |>
  select(vessel_official_number) |>
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids)
# [1] 128   1
# [1] 328   1
# [1] 228   1

### check these ids in the full compliance information ----
compl_clean_w_permit_exp_last_half_year__sa |>
  filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # [1] 1938   22
  group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  # dim()
# [1] 11 22
  glimpse()

## get only the latest compliant weeks ----
# Explanations:
# Create a new data frame 'compliant_in_last_half_year' containing information about vessels that were compliant in the last half year.
# 1. Use 'filter' to select rows where 'vessel_official_number' is in 'compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids'.
# 2. Group the data by 'vessel_official_number'.
# 3. Use 'filter' to select rows where 'compliant_' is "yes", 'year_month' is before the current month, and 'week_num' is the maximum value for each group.
# 4. Calculate the 'latest_compl' as the maximum 'week_num'.
# 5. Use another 'filter' to select rows where 'week_num' is equal to 'latest_compl'.
# 6. Use 'ungroup' to remove grouping.
# 7. Select specific columns ('vessel_official_number', 'year_month', 'week', 'latest_compl').
# 8. Use 'distinct' to keep only unique rows.

compliant_in_last_half_year <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  mutate(latest_compl = max(week_num)) |>
  filter(week_num == latest_compl) |>
  ungroup() |>
  select(vessel_official_number,
                year_month,
                week,
                latest_compl) |>
  distinct()

glimpse(compliant_in_last_half_year)
# Rows: 9
# Columns: 3
# $ vessel_official_number <chr> "1332041", "1284153", "FL9738SR", "FL7361TJ", "FL…
# $ year_month             <yearmon> Aug 2023, Aug 2023, Aug 2023, Aug 2023, Aug 2…
# $ latest_compl           <int> 33, 33, 32, 32, 32, 32, 32, 32, 32

## Remove the vessels with the last compliant weeks ----

compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok <-
  compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c |>
  filter(!vessel_official_number %in% compliant_in_last_half_year$vessel_official_number)

dim(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok)
# [1] 319   6
# [1] 219   6

# ---- Preparing Correspondence ----

## ---- remove 999999 ----
# Explanations:
# Create a new data frame 'corresp_contact_cnts_clean' by filtering 'corresp_contact_cnts_clean0' based on the condition.
# 1. Use 'filter' to select rows where 'vessel_official_number' does not start with "99999".
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

n_distinct(corresp_contact_cnts_clean$vesselofficial_number)
# vesselofficial_number   3223
# vessel_official_number  3371
# vesselofficial_number 3434
# 4118

# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

## new requirement 2024-02-26 ----
# Michelle
# It needs to be that we called at least 1 time and emailed at least 1 time. Or they contacted us at least once.

corresp_contact_cnts_clean |>
  select(calltype, voicemail, contacttype) |>
  distinct() |> head(10)

we_called_filter <-
  quo(any(tolower(contacttype) == "call" &
        tolower(calltype) == "outgoing"))

we_emailed_once_filter <-
  quo(any(
    tolower(contacttype) %in% c("email", "other") &
      tolower(calltype) == "outcoming"
  ))

# don't need a second contact
they_contacted_direct_filter <-
  quo(
    any(
      tolower(calltype) == "incoming"
      )
  )

corresp_filter <-
  quo(!!they_contacted_direct_filter |
        (
          contact_freq > 1 &
            (!!we_called_filter &
               !!we_emailed_once_filter)
        ))

# calltype voicemail contacttype

# two_attempts_filter <-
#   quo(contact_freq > 1 &
#         any(tolower(contacttype) == "call"))

# use the filter
corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  filter(!!corresp_filter)

# dim(corresp_contact_cnts_clean)
# [1] 18629    23
# dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23
# today()
# [1] "2024-02-16"
dim(corresp_contact_cnts_clean)
# [1] 29587    20
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 29089    20
# [1] 29587    22

n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)
# vesselofficial_number 2968
# 3620
# 4118

## fix dates ----
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1)
# [1] "02/15/2024 03:15PM"

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

str(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm)
# POSIXct[1:29089], format: "2024-02-15 15:15:00" 

# Join correspondence with compliance ----
# Explanations:
# Create a new dataframe 'compl_corr_to_investigation1' by performing an inner join between
# 'corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates' and 'compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok'.
# The join is performed on the column 'vessel_official_number'.
# Use 'multiple = "all"' and 'relationship = "many-to-many"' to handle multiple matches during the join.
# 1. Use the 'inner_join' function from the dplyr package to combine the two dataframes based on the specified columns.
# 2. Pass the column names and other parameters to the 'by', 'multiple', and 'relationship' arguments.

compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation1)
# [1] 486  30
# [1] 522  30
# [1] 940  27
# [1] 2100   27
# [1] 1926   27

# check
n_distinct(compl_corr_to_investigation1$vesselofficial_number)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105
# 108
# 97
# vesselofficial_number 116
# 262
# 217

# View(compl_corr_to_investigation1)

## save number of vessels to investigate ----
num_of_vsl_to_investigate <- 
  n_distinct(compl_corr_to_investigation1$vesselofficial_number)
# 262
# 217

# ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) mark vessels already in the know list (prev_result)

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# # put names into vars
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "date")[1]
contacttype_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "type")[1]
 
# write.csv(compl_corr_to_investigation1,
#           file.path(
#             my_paths$outputs,
#             paste0(              "more_than_24_compl_corr_to_investigation1_22_23__",
#               today(),
#               ".csv"
#             )
#           ),
#           row.names = FALSE)
# # 435 distinct ids

# Explanations:
# Define a function 'get_date_contacttype' that takes a dataframe 'compl_corr_to_investigation1' as input.
# Perform several data manipulation steps to extract and organize relevant information.
# 1. Add a new column 'date__contacttype' by concatenating the values from 'contactdate_field_name' and 'contacttype'.
# 2. Select only the 'vessel_official_number' and 'date__contacttype' columns.
# 3. Arrange the dataframe by 'vessel_official_number' and 'date__contacttype'.
# 4. Keep distinct rows based on 'vessel_official_number' and 'date__contacttype'.
# 5. Group the dataframe by 'vessel_official_number'.
# 6. Summarize the data by creating a new column 'date__contacttypes' that concatenates all 'date__contacttype' values for each vessel separated by a comma.
# 7. Return the resulting dataframe.
get_date_contacttype <-
  function(compl_corr_to_investigation1) {
    compl_corr_to_investigation1 |>
      # add a new column date__contacttype with contactdate and contacttype
      mutate(date__contacttype =
                      paste(!!sym(contactdate_field_name),
                            contacttype)) |>
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
  get_date_contacttype(compl_corr_to_investigation1)

dim(date__contacttype_per_id)
# [1] 110    2
# 107
# 27: 177
# 188   2
# 105   2 (the new filter)
# 108
# 97
# [1] 116   2 (2 contact attempts)
# [1] 262   2
# 217

# glimpse(date__contacttype_per_id)

## add permit and address info ----
# print_df_names(vessels_permits_participants)
# print_df_names(db_participants_asddress)
# print_df_names(fhier_addr_short)
# intersect(names(db_participants_asddress),
#           names(fhier_addr_short))
# 0

# compare ----


## join db_participant and fhier info ----
db_fhier_addr <-
  full_join(
    fhier_addr_short,
    db_participants_asddress,
    join_by(vessel_official_number == official_number)
  )

dim(db_fhier_addr)
# [1] 55114    46

# here
# print_df_names(db_fhier_addr)
# [1] "vessel_official_number, permit_holder_names, physical_address_1, physical_address_2, physical_city, physical_county, physical_state, physical_zip_code, phone_number, primary_email, ser_id, uscg_documentation, state_registration, vchar_hull_id_number, vchar_vessel_name, is_primary, is_mail_rec, erv_entity_type, erv_entity_name, erb_entity_type, erb_entity_name"

# db_fhier_addr |>
#   filter(!paste0(erv_ph_area, erv_ph_number) == phone_number) |>
#   select(erv_ph_area, erv_ph_number, phone_number) |>
#   dim()
# [1] 2125    3

## compare by join ----
db_fhier_addr_1 <-
  db_fhier_addr |>
  mutate(erv_ph_area__erv_ph_number =
           paste0(erv_ph_area, erv_ph_number))

compare_colnames_list <-
  list(
    c("permit_holder_names",
      "erv_entity_name"),
    c("phone_number",
      "erv_ph_area__erv_ph_number"),
    c("primary_email",
      "erv_primary_email"),
    c("physical_address_1",
      "erv_physical_address1"),
    c("physical_address_2",
      "erv_physical_address2"),
    c("physical_city",
      "erv_physical_city"),
    c("physical_county",
      "erv_physical_county"),
    c("physical_state",
      "erv_physical_state"),
    c("physical_zip_code",
      "erv_physical_zip_code")
  )

compare_colnames_list__db_fhier_addr_1 <-
  compare_colnames_list |>
  map(\(curr_pair) {
    # browser()
    db_fhier_addr_1 |>
      group_by(vessel_official_number) |> 
      filter(!curr_pair[[1]] == curr_pair[[2]]) |>
      ungroup() |> 
      select(all_of(curr_pair)) |>
      distinct() %>%
      return()
  })

View(compare_colnames_list__db_fhier_addr_1)

### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants |> 
  select(P_VESSEL_ID) |> 
  distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1
# [1] 3203    1

vessels_not_in_vessel_permit_from_db <-
  setdiff(
    date__contacttype_per_id$vessel_official_number,
    vessels_permits_participants_v_ids$P_VESSEL_ID
  ) |>
  sort()

length(vessels_not_in_vessel_permit_from_db)
# 32
# 22

vessels_not_in_vessel_permit_from_db |>
  cat(sep = "\n")
# cat(sep = "', '")
  # length()
# 32
# (wrong license_nbr in full_participants
# or entity_id in permits,
# check manually)

vessels_from_pims_needed <-
  vessels_from_pims |>
  filter(official__ %in% vessels_not_in_vessel_permit_from_db)

vessels_from_pims_needed |> 
  arrange(official__) |> 
  dim()
# [1] 30  8
# [1] 22  8

vessels_from_pims_double |>
  filter(
    vessel_official_number1 %in% vessels_not_in_vessel_permit_from_db |
      vessel_official_number2 %in% vessels_not_in_vessel_permit_from_db
  ) |>
  arrange(vessel_official_number1, vessel_official_number2) |>
  dim()
# 3
# [1] 1 3

# to_remove <- c("", "UN", " UN", "UN ", ";, UN"
# NA, "NA")

# Explanations:
# Define a function 'clean_names_and_addresses' that takes a dataframe 'my_df' as input.
# Perform various cleaning operations on character columns to standardize names and addresses.
# 1. Remove leading and trailing whitespaces from all character columns.
# 2. Replace NA values with empty strings.
# 3. Replace instances of ', ;' with ';'.
# 4. Replace any whitespaces followed by ',' or ';' with ','.
# 5. Replace multiple consecutive commas with a single comma.
# 6. Replace multiple consecutive semicolons with a single semicolon.
# 7. Remove any leading commas or semicolons.
# 8. Remove trailing commas.
# 9. Remove occurrences of the word 'UN' surrounded by word boundaries after a comma or semicolon.
# 10. Remove occurrences of the word 'UN' surrounded by word boundaries.
# 11. Remove spaces around the word 'UN'.
# 12. Remove leading spaces after commas or semicolons.
# 13. Remove trailing spaces before commas.
# 14. Remove trailing commas.
# 15. Remove leading and trailing spaces again.
# Return the cleaned dataframe.

# Note: The 'str_replace_all' and 'str_squish' functions are from the 'stringr' package.
# The 'across', 'where', 'mutate', and 'replace_na' functions are from the 'dplyr' package.
# Example:
# Before:
#   my_df:
#     Name            Address
#     John Doe        , 123 Main St; Apt 45, City
#     Jane Smith      UN, 567 Park Ave
# After applying 'clean_names_and_addresses':
#   Result:
#     Name         Address
#     John Doe     123 Main St, Apt 45, City
#     Jane Smith   567 Park Ave

clean_names_and_addresses <- function(my_df) {
  
  my_df_cleaned <- 
    my_df |>
    mutate(
      across(where(is.character),
             ~ str_squish(.x)),
      across(where(is.character),
             ~ replace_na(.x, "")),
      across(where(is.character),
             ~ str_replace_all(.x, ", ;", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s+[,;]", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, ";,+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ";;+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ",,+", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;] *\\bUN\\b *", "")),
      across(where(is.character),
                          ~ str_replace_all(.x, "\\bUN\\b", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;] ", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;]$", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;]$", "")),
      across(where(is.character),
             ~ str_squish(.x))
    )
  
  return(my_df_cleaned)
}

# Use the function
vessels_permits_participants_space <-
  vessels_permits_participants |>
  clean_names_and_addresses()

dim(vessels_permits_participants_space)
# [1] 31942    38
# [1] 30511    38

# Explanations:
# Create a new dataframe 'vessels_permits_participants_short_u' by performing the following operations:
# 1. Group the dataframe 'vessels_permits_participants_space' by the column 'P_VESSEL_ID'.
# 2. For each group, create new columns 'sero_home_port', 'full_name', and 'full_address'.
# 3. 'sero_home_port': Concatenate unique values of 'SERO_HOME_PORT_CITY', 'SERO_HOME_PORT_COUNTY', and 'SERO_HOME_PORT_STATE'.
# 4. 'full_name': Concatenate unique values of 'FIRST_NAME', 'MIDDLE_NAME', 'LAST_NAME', and 'NAME_SUFFIX'.
# 5. 'full_address': Concatenate unique values of 'ADDRESS_1', 'ADDRESS_2', 'STATE', and 'POSTAL_CODE'.
# 6. Select the columns 'P_VESSEL_ID', 'sero_home_port', 'full_name', and 'full_address'.
# 7. Ungroup the dataframe and keep only distinct rows.

# Note: The 'str_glue' function is from the 'stringr' package.
# The 'group_by', 'mutate', 'select', 'ungroup', and 'distinct' functions are from the 'dplyr' package.

vessels_permits_participants_short_u <-
  vessels_permits_participants_space |>
  group_by(P_VESSEL_ID) |>
  mutate(
    sero_home_port = list(unique(
      str_glue(
        "{SERO_HOME_PORT_CITY}, {SERO_HOME_PORT_COUNTY}, {SERO_HOME_PORT_STATE}"
      )
    )),
    full_name = list(unique(
      str_glue("{FIRST_NAME} {MIDDLE_NAME} {LAST_NAME}, {NAME_SUFFIX}")
    )),
    full_address = list(unique(
      str_glue("{ADDRESS_1}, {ADDRESS_2}, {STATE}, {POSTAL_CODE}")
    ))
  ) |>
  select(P_VESSEL_ID,
                sero_home_port,
                full_name,
                full_address) |>
  ungroup() |>
  distinct()

dim(vessels_permits_participants_short_u)
# [1] 7858    4
# [1] 3302    4
# [1] 3203    4

# View(vessels_permits_participants_short_u)

# vessels_permits_participants_short_u |> 
#   # filter(lengths(full_name) > 0) %>%
#   # unnest(full_name) %>%
#   # unnest_wider(full_name, names_sep = "_") |> 
#   rowwise() |> 
#   mutate_if(is.list, ~paste(unlist(.), collapse = ', ')) %>% 
#   View()
 # cat()

# Explanations:
# Flatten the 'vessels_permits_participants_short_u' dataframe by converting list columns to character columns.
# 1. Apply 'rowwise()' to perform row-wise operations.
# 2. Use 'mutate_if' to apply a function to columns that meet a certain condition (is.list).
# 3. Inside the function, use 'paste' to concatenate the elements of each list with a separator '; '.
# 4. 'ungroup()' the dataframe after row-wise operations.

# Note: The 'rowwise', 'mutate_if', and 'ungroup' functions are from the 'dplyr' package.
vessels_permits_participants_short_u_flat <-
  vessels_permits_participants_short_u |>
  rowwise() |>
  mutate_if(is.list, ~ paste(unlist(.), collapse = '; ')) |> 
  ungroup()

n_distinct(vessels_permits_participants_short_u_flat$P_VESSEL_ID)
# P_VESSEL_ID 3302
# 3203

vessels_permits_participants |>
  filter(P_VESSEL_ID == "635942") |>
  select(ADDRESS_1,
         ADDRESS_2,
         STATE,
         POSTAL_CODE) |>
  distinct() |>
  dim()
# 7
# $ ADDRESS_1   <chr> "160 SNUFF MILL RD", "160 SNUFF MILL ROAD", "P O BOX 625", "P…
# $ ADDRESS_2   <chr> NA, NA, NA, NA, NA, NA, NA
# $ STATE       <chr> "RI", "RI", "MD", "VA", "SC", "MD", NA
# $ POSTAL_CODE <chr> "02874", "02874", "21041", "23183", "295822571", "21014", NA

# use the function definef earlier
vessels_permits_participants_short_u_flat_sp <-
  vessels_permits_participants_short_u_flat |>
  clean_names_and_addresses()

filter(vessels_permits_participants_short_u_flat_sp,
       P_VESSEL_ID == "1173297") |>
  glimpse()

## combine with info from PIMS when missing ----
# feser columns
vessels_from_pims_needed_short <-
  vessels_from_pims_needed |>
  select(official__,
         hailing_port,
         owner) |>
  distinct()

### add an empty column ----
# to make the same amount of column as from db
vessels_from_pims_needed_short[ncol(vessels_from_pims_needed_short) + 1] <-
  c("")

# make names the same
names(vessels_from_pims_needed_short) <- 
  names(vessels_permits_participants_short_u_flat_sp)

# Explanations:
# Combine two dataframes 'vessels_permits_participants_short_u_flat_sp' and 'vessels_from_pims_needed_short' row-wise using 'rbind'.
# This assumes that both dataframes have the same structure.
vessels_permits_participants_short_u_flat_sp_full <-
  rbind(vessels_permits_participants_short_u_flat_sp,
        vessels_from_pims_needed_short)

### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants_short_u_flat_sp_full |> 
  select(P_VESSEL_ID) |> 
  distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1
# 3203
# 3232    
# [1] 3224    1

# how many vessels are missing from the db report
setdiff(
  date__contacttype_per_id$vessel_official_number,
  vessels_permits_participants_v_ids$P_VESSEL_ID
) |> 
  # cat(sep = "', '")
  length()
# 3
# 1
# 1039937

# We don't need to check the reverse, there will be more vessels in the permit info we are not interested in

# combine vessels_permits and date__contacttype ----
vessels_permits_participants_date__contacttype_per_id <-
  left_join(
    date__contacttype_per_id,
    vessels_permits_participants_short_u_flat_sp,
    join_by(vessel_official_number == P_VESSEL_ID)
  )

# View(vessels_permits_participants_date__contacttype_per_id)
n_distinct(date__contacttype_per_id$vessel_official_number)
# 262
# 217

dim(vessels_permits_participants_date__contacttype_per_id)
# 117
# [1] 262   5
# 217

# compare vsl numbers
num_of_vsl_to_investigate == n_distinct(vessels_permits_participants_date__contacttype_per_id$vessel_official_number)
# T

# combine output ----
# join compliance, correspondence and vessel/permit/owner information
compl_corr_to_investigation1__w_addr <-
  left_join(
    compl_corr_to_investigation1,
    vessels_permits_participants_date__contacttype_per_id
  )

dim(compl_corr_to_investigation1__w_addr)
# [1] 2100   31
# [1] 1926   31

# compare vsl numbers
num_of_vsl_to_investigate == n_distinct(compl_corr_to_investigation1__w_addr$vessel_official_number)
# T
# 262

# compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
#   compl_corr_to_investigation1 |>
#   inner_join(vessels_permits_participants_date__contacttype_per_id,
#              by = "vessel_official_number")

# dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31
# 309
# 271
# [1] 522  31

## ---- 2) remove extra columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

# print_df_names(compl_corr_to_investigation1__w_addr)

# Explanations:
# Group the dataframe by the 'vessel_official_number' column and then apply the 'summarise_all' function.
# The 'summarise_all' function applies the specified function (in this case, 'concat_unique') to each column.

# Note: 'concat_unique' is not a standard R function, it is a custom function defined previously.

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1__w_addr |>
  # compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permit_groupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    sero_home_port,
    full_name,
    full_address,
    # "week_start",
    "date__contacttypes"
  ) |>
  group_by(vessel_official_number) |>
  summarise_all(concat_unique) |> 
  ungroup()

# compl_corr_to_investigation1_short |> glimpse()

dim(compl_corr_to_investigation1_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9
# 108
# [1] 262  12
# 217

## 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
# Download first
previous_egr_data_path <-
  file.path(
    my_paths$inputs,
    current_project_name,
    r"(egregious violators for investigation_2023-01-24_to_2023-08-01_OLEAction(green).xlsx)"
  )

file.exists(previous_egr_data_path)
# T

vessels_to_mark <-
  read_xlsx(previous_egr_data_path) |> 
  remove_empty_cols()

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  select(vessel_official_number)

dim(vessels_to_mark_ids)
# [1] 96  1

#### mark these vessels ----
# Explanations:
# Create a new column 'duplicate_w_last_time' in the dataframe 'compl_corr_to_investigation1_short'.
# This column is marked with "duplicate" for rows where 'vessel_official_number' is present in the list of vessel IDs to mark as duplicates ('vessels_to_mark_ids').
# For all other rows, it is marked as "new".
compl_corr_to_investigation1_short_dup_marked <-
  compl_corr_to_investigation1_short |>
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation1_short_dup_marked)
# [1] 177  11
# [1] 105  10
# 108
# 97
# [1] 110  10 2 atmpts
# [1] 116  10
# [1] 262  13
# [1] 217  13

### check ----
n_distinct(compl_corr_to_investigation1_short_dup_marked$vessel_official_number)
# 107
# 102
# 27: 164
# 177
# 105
# 108
# 97
# 110
# 116
# 2024-02-20
# 262
# 217

## add pims home port info ----
compl_corr_to_investigation1_short_dup_marked__hailing_port <-
  left_join(
    compl_corr_to_investigation1_short_dup_marked,
    processed_pims_home_ports,
    join_by(vessel_official_number)
  ) |> 
  rename("hailing_port_city" = city_fixed,
         "hailing_port_state" = state_fixed)

# compl_corr_to_investigation1_short_dup_marked__hailing_port |> 
#   select(sero_home_port, starts_with("hailing")) |> 
#   distinct() |> 
#   View()

# compl_corr_to_investigation1_short_dup_marked__hailing_port
# how many are duals? ----
# Explanations:
# Create a new dataframe 'compl_corr_to_investigation1_short_dup_marked__permit_region'.
# Use the 'mutate' function to add a new column 'permit_region' based on conditions.
# If 'permitgroup' contains any of the specified patterns ("RCG", "HRCG", "CHG", "HCHG"),
# set 'permit_region' to "dual". Otherwise, set 'permit_region' to "sa_only".
# If none of the conditions are met, set 'permit_region' to "other".
# The resulting dataframe includes the original columns from 'compl_corr_to_investigation1_short_dup_marked'
# along with the newly added 'permit_region' column.

compl_corr_to_investigation1_short_dup_marked__permit_region <-
  compl_corr_to_investigation1_short_dup_marked__hailing_port |> 
  # compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols |>
  mutate(permit_region =
           case_when(
             grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "dual",
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "other"
           ))

# Explanations:
# Use the 'select' function to extract the columns 'vessel_official_number' and 'permit_region'
# from the dataframe 'compl_corr_to_investigation1_short_dup_marked__permit_region'.
# Use the 'distinct' function to keep only unique combinations of 'vessel_official_number' and 'permit_region'.
# Use the 'count' function to count the occurrences of each unique 'permit_region'.
# The resulting count provides the frequency of each 'permit_region'.
region_counts <-
  compl_corr_to_investigation1_short_dup_marked__permit_region |>
  select(vessel_official_number, permit_region) |>
  distinct() |>
  count(permit_region)
# 1 dual             56
# 2 sa_only         206
#   permit_region     n
#   <chr>         <int>
# 1 dual             51
# 2 sa_only         166

n_distinct(compl_corr_to_investigation1_short_dup_marked__permit_region$vessel_official_number)
# 262
# 217

## dual permitted ----
# 56 / (206 + 56) * 100
# 21.37405

region_counts$n[[1]] / (region_counts$n[[2]] + region_counts$n[[1]]) * 100
# 51 / (166 + 51) * 100
# 23.5023%

# add info from FHIER to the results ----

fix_addresses_path <-
  file.path(current_project_path,
            str_glue("{current_project_basename}_fix_addresses.R"))

file.exists(fix_addresses_path)

source(fix_addresses_path)

# View(compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols)

# Print out results ----

result_path <- 
  file.path(my_paths$outputs,
            current_project_basename,
            str_glue("egregious_violators_to_investigate_{today()}.csv"))

write_csv(compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols,
          result_path)

