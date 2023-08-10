# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
current_project_name <- "egregious_violators"
current_project_path <-
  file.path(my_paths$git_r, current_project_name)

source(file.path(current_project_path, "get_data.R"))

## check ----
check_new_vessels <-
  function(my_df) {
    list_to_check <-
      c("FL1848EY",
        "FL4232JY",
        "1246468",
        "FL7549EJ")
    my_df |>
      filter(vessel_official_number %in% list_to_check) |>
      select(vessel_official_number) |>
      distinct() |>
      dim() %>%
      return()
  }


# ---- Preparing compliance info ----

## ---- add permit_expired column ----
check_new_vessels(compl_clean)
# 4

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is more than a month from data_file_date than "no"
  mutate(permit_expired = case_when(permitgroupexpiration > (data_file_date + 30) ~ "no",
                                    .default = "yes"))

## ---- add year_month column ----

number_of_weeks_for_non_compliancy = 27
days_in_27_weeks <- number_of_weeks_for_non_compliancy*7

half_year_ago <-
  data_file_date - days_in_27_weeks

compl_clean_w_permit_exp_last_27w <-
  compl_clean_w_permit_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last 28 weeks
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)
# today()
# [1] "2023-08-01"
# [1] 235509     22

check_new_vessels(compl_clean_w_permit_exp)
# 4

# 185538
# [1] 217772     22
dim(compl_clean_w_permit_exp_last_27w)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d
# [1] 87826    23

check_new_vessels(compl_clean_w_permit_exp_last_27w)
# 4
## ---- Have only SA permits, exclude those with Gulf permits ----

compl_clean_sa <-
  compl_clean_w_permit_exp_last_27w |>
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

today()
# [1] "2023-08-01"
# [1] "2023-07-10"
## Not "compliant_" ----
compl_clean_sa_non_compl <-
  compl_clean_sa |>
  filter(compliant_ == 'NO')

check_new_vessels(compl_clean_sa_non_compl)
# 4

dim(compl_clean_sa_non_compl)
# [1] 18205    23
# [1] 11473    23
# [1] 10597    23
# [1] 12484    23
# [1] 15549    23
# [1] 13992    23
# [1] 14204    23

compl_clean_sa_non_compl |>
  count_uniq_by_column() |> head(1)
# vesselofficialnumber 1785
# today()
# "2023-06-23"
# vessel_official_number 1573
# [1] "2023-07-10"
# vessel_official_number 1403
# vessel_official_number 1369
# [1] "2023-08-01"
# vessel_official_number 1370

## filter for egregious ----
### check if there is no "compliant_ == YES" since half_year_ago ----

last_week_start <- data_file_date - 6

compl_clean_sa |> check_new_vessels()
# 4

compl_clean_sa_non_c_not_exp <-
  compl_clean_sa |>
  # not compliant
  filter(tolower(compliant_) == "no") |>
  # in the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_start < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

    # filter(vessel_official_number %in% list_to_check) |>
    # select(vessel_official_number) |>
    # distinct() |>
    # head()
    # check_new_vessels()
# 1 FL7549EJ
# 2 FL4232JY
# 3 FL1848EY

# dim(compl_clean_sa_non_c_not_exp)
# [1] 10419    23
# [1] 9486   23
# [1] 9315   23

compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

compl_clean_sa_all_weeks_non_c_short <-
  compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  dplyr::arrange(dplyr::desc(compl_weeks_amnt), vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # all weeks were non compliant
  filter(compl_weeks_amnt == total_weeks) |>
  # all weeks are not compliant
  filter(total_weeks >= (number_of_weeks_for_non_compliancy - 3))

compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  # dim()
  # [1] 9486    5
  # [1] 9315    5
  dplyr::arrange(dplyr::desc(compl_weeks_amnt), vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # dim()
  # [1] 1046    4
  # all weeks were non compliant
  filter(compl_weeks_amnt == total_weeks) |>
    filter(vessel_official_number %in% list_to_check) |>
    glimpse()
    # FL1848EY total_weeks == 22
    # select(vessel_official_number) |>
    # distinct() |>
    # head()
# 1 FL4232JY
# 2 FL7549EJ
# 3 FL1848EY

### add back columns needed for the output ----
need_cols_names <- c(
  "vessel_official_number",
  "name",
  "permit_expired",
  "permitgroup",
  "permitgroupexpiration"
  # ,
  # "week_start"
)
compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

compl_clean_sa_all_weeks_non_c_short |>
    filter(vessel_official_number %in% list_to_check) |>
    select(vessel_official_number) |>
    distinct() |>
    head()
# 1 FL4232JY
# 2 FL7549EJ

  # check_new_vessels()
# 2

# dim(compl_clean_sa_non_c_not_exp)
compl_clean_sa_all_weeks_non_c <-
  compl_clean_sa_non_c_not_exp |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_sa_all_weeks_non_c_short) |>
# Joining with `by = join_by(vessel_official_number)`
  distinct()

dim(compl_clean_sa_all_weeks_non_c)
# [1] 130   8
# 0
# 127
# 121

## check the last report date ----
compl_clean_sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_sa_all_weeks_non_c_short |>
  select(vessel_official_number) |>
  distinct()

# compl_clean_sa_non_c_not_exp |> 
dim(compl_clean_sa_all_weeks_non_c_short_vesl_ids)
# [1] 121   1

compl_clean_sa |>
  filter(
    vessel_official_number %in% compl_clean_sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # View()
  group_by(vessel_official_number) |> 
  filter(tolower(compliant_) == "yes") |>
  mutate(latest_compl = max(week_num)) |> 
  View()

# ## check the last output manually ----
# manual_no <- c("1133962",
# "1158893",
# "1202614",
# "FL6540TD",
# "NJ3548GR",
# "1254648",
# "943683",
# "1077813",
# "1243529",
# "1266505",
# "1064222",
# "FL9628RY",
# "FL7060HK",
# "978217",
# "508385",
# "FL2034JA",
# "594390",
# "1321667",
# "591881",
# "1233418",
# "606326",
# "FL1348GL",
# "1254999",
# "FL3557RC",
# "FL1267MZ",
# "FL6220RN",
# "FL6900MH",
# "FL5207CA",
# "SC6965DN",
# "FL4959PK",
# "FL7207LT",
# "FL7703LT",
# "1279625",
# "565041")
#
# # compl_clean_sa_all_weeks_non_c |>
# #   filter(vessel_official_number %in%
# #            manual_no) |>
# #   View()
#
# # compl_clean_sa |>
# #   filter(vessel_official_number %in%
# #            manual_no) |>
# #   View()
#
## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean) |>
  head(1)
# vesselofficial_number   3223
# vessel_official_number  3371
# vesselofficial_number 3434

# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## ---- direct_contact ----
## ---- 1) all are voicemails ----
get_all_voicemails_id <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean |>
    group_by(vessel_official_number) |>
    # add a new logical column all_vm with a TRUE if all entries for voicemail column for this vessel are yeses
    reframe(all_vm = all(tolower(voicemail) == "yes")) |>
    # keep a row only if all_vm == TRUE
    filter(all_vm) |>
    # keep only one column
    select("vessel_official_number") |>
    dplyr::distinct() %>%
    return()
}

all_vm_ids <- get_all_voicemails_id(corresp_contact_cnts_clean)
dim(all_vm_ids)
# 284
# 27: 222
# 134
# 120

# field_name into a var
contactcomments_field_name <-
  sym(find_col_name(corresp_contact_cnts_clean, ".*contact", "comments.*")[1])

add_a_direct_contact_column <-
  function(corresp_contact_cnts_clean) {
    corresp_contact_cnts_clean |>
      # create a new column "direct_contact" with a "yes" or "no"
      # search comments for indicators that there was no direct contact
      mutate(
        direct_contact = case_when(
          grepl("no answer", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
          grepl("wrong number", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
          grepl("not in service", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
          grepl(
            "number.+is incorrect",
            !!contactcomments_field_name,
            ignore.case = TRUE
          ) ~ "no",
          grepl(
            "the incorrect number",
            !!contactcomments_field_name,
            ignore.case = TRUE
          ) ~ "no",
          grepl(
            "incorrect phone number",
            !!contactcomments_field_name,
            ignore.case = TRUE
          ) ~ "no",
          grepl(
            "call could not be completed as dialed",
            !!contactcomments_field_name,
            ignore.case = TRUE
          ) ~ "no",
          vessel_official_number %in% all_vm_ids$vessel_official_number ~ "no",
          .default = "yes"
        )
      ) %>%
      return()

    # filter(direct_contact == "no") |>
    # select(vesselofficialnumber) |>
    # dplyr::distinct() |>
    # str()
    # 'data.frame':	1126 obs. of  1 variable
    # with new filters
    # 'data.frame':	1138 obs. of  1 variable
  }

corresp_contact_cnts_clean_direct_cnt <-
  add_a_direct_contact_column(corresp_contact_cnts_clean)
dim(corresp_contact_cnts_clean_direct_cnt)
# [1] 18629    23

## ---- Add a filter: If there was 1 call or 2 emails (out and in, bc they got the email, we shared the information and received a confirmation) with a direct communication. ----
# to investigation (to NEIS)

test_new_egr1 <-
  corresp_contact_cnts_clean_direct_cnt |>
  check_new_vessels()
# 4
test_new_egr1[1] == 4
# T

## new requirement ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

two_attempts_filter <-
  quo(contact_freq > 1 &
        any(tolower(contacttype) == "call"))

# emails_filter <-
#   quo(contact_freq > 1 &
#         ((tolower(contacttype) == "email") |
#            (tolower(contacttype) == "other")))


corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean_direct_cnt |>
  filter(!!two_attempts_filter)

test_new_egr2 <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()

test_new_egr2[1] == 4
# T

dim(corresp_contact_cnts_clean_direct_cnt)
# [1] 18629    23
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

data_overview(corresp_contact_cnts_clean_direct_cnt_2atmps) |>
  head(1)
# vesselofficial_number 2968
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23


## ---- 1) 1 call with a direct communication ----
# get_calls_with_direct_communication <-
#   function(corresp_contact_cnts_clean_direct_cnt) {
#     # save the long filter
#     # more than one call
#     answered_1_plus_filter <- quo(
#       contact_freq > 1 &
#         tolower(contacttype) == "call" &
#         direct_contact == "yes" &
#         tolower(voicemail) ==  "no"
#     )
#     # use the filter
#     corresp_contact_cnts_clean_direct_cnt |>
#       filter(!!answered_1_plus_filter) %>%
#       return()
#   }
#
# calls_with_direct_communication <-
#   get_calls_with_direct_communication(corresp_contact_cnts_clean_direct_cnt)
# # TODO: out of 4 contacts for 1305207 only one is in the "contactdate_field_name
#
# dim(calls_with_direct_communication)
# # [1] 12584    23
# # 27: [1] 10062    23
# # [1] 10475    23
# # [1] 10594    23
#
# # str(calls_with_direct_communication)
#
## ---- 2) in and out emails ----
# get_both_in_n_out_emails <- function(corresp_contact_cnts_clean) {
#   # save a filter: more than 1 email
#   emails_filter <- quo(contact_freq > 1 &
#                          ((tolower(contacttype) == "email") |
#                             (tolower(contacttype) == "other")))
#
#   # use emails_filter for incoming
#   incoming_2_plus_emails <-
#     corresp_contact_cnts_clean |>
#     filter(!!emails_filter &
#              tolower(calltype) == "incoming") |>
#     select(vessel_official_number) |>
#     dplyr::distinct()
#   # 259
#
#   # use emails_filter for outgoing
#   outgoing_2_plus_emails <-
#     corresp_contact_cnts_clean |>
#     filter(!!emails_filter &
#              tolower(calltype) == "outgoing") |>
#     select(vessel_official_number) |>
#     dplyr::distinct()
#   # |>
#   #   glimpse()
#   # 624
#   # 712
#
#   # get ids wihch are in both in and out lists
#   both_in_n_out_2_plus_email_ids <-
#     intersect(incoming_2_plus_emails, outgoing_2_plus_emails)
#   # 148
#   # 173
#
#   # keep correspondence information only for those
#   corresp_contact_cnts_clean_direct_cnt |>
#     filter(
#       vessel_official_number %in% both_in_n_out_2_plus_email_ids$vessel_official_number
#     ) %>%
#     return()
# }
#
# both_in_n_out_2_plus_emails <-
#   get_both_in_n_out_emails(corresp_contact_cnts_clean)
#
# # check
# data_overview(corresp_contact_cnts_clean) |> head(1)
# # vesselofficialnumber  3450
# # 3571
# # 27: vesselofficial_number 3223
# # vesselofficial_number 3371
# # vesselofficial_number 3434
#
# data_overview(both_in_n_out_2_plus_emails)  |> head(1)
# # vesselofficialnumber  147
# # 173
# # 27: 246
# # 461
# # 549
#
# # group_by_arr <- c("vessel_official_number", "calltype")
# # count_by_column_arr(both_in_n_out_2_plus_emails, group_by_arr) |> glimpse()
#
# to_investigation_to_NEIS <-
#   rbind(both_in_n_out_2_plus_emails,
#         calls_with_direct_communication)
#
## ---- look at the to_investigation_to_NEIS ----
# data_overview(to_investigation_to_NEIS) |> head(1)
# vesselofficialnumber  3070
# 3170
# 27: vesselofficial_number 2843
# 3034
# 2811

# names(to_investigation_to_NEIS)
# dim(to_investigation_to_NEIS)
# [1] 14401    23
# 27: [1] 12192    23
# [1] 14463    23
# [1] 15556    23

# str(to_investigation_to_NEIS)
# View(to_investigation_to_NEIS)
# count dplyr::distinct values in each column
# apply(to_investigation_to_NEIS, 2, function(x) length(dplyr::distinct(x))) |>
#   as.data.frame() |> head(1)
# vesselofficialnumber  3070
# 3170

## ---- Combine compliance information with filtered correspondence info by vesselofficialnumber ----

# compl_corr_to_investigation <-
#   inner_join(
#     to_investigation_to_NEIS,
#     compl_clean_sa_all_weeks_non_c,
#     by = c("vessel_official_number"),
#     multiple = "all",
#     relationship = "many-to-many"
#   )
corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()
# 4

compl_clean_sa_all_weeks_non_c |>
  check_new_vessels()
# 2


compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps,
    compl_clean_sa_all_weeks_non_c,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

# check
# to_investigation_to_NEIS[15,] FL3262PM
# compl_clean_sa_all_weeks_non_c |>
#   filter(vessel_official_number == "FL3262PM") |>
#   dim()

# dim(compl_corr_to_investigation)
# [1] 16081    44
# 27: 11151
# 18093    45
# [1] 264  26
# [1] 264  30
# 309

dim(compl_corr_to_investigation1)
# [1] 486  30
# [1] 522  30

# str(compl_corr_to_investigation)
# compl_corr_to_investigation |>
#   head() |>
#   select(grep("official", names(compl_corr_to_investigation), value = T))

## check
count_uniq_by_column(compl_corr_to_investigation1) |>
  head(1)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105
# 108
# 97
# vesselofficial_number 116

count_uniq_by_column(compl_corr_to_investigation1) |>
  head(1)
# vesselofficial_number 116

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# put names into vars
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "date")[1]
contacttype_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "type")[1]

write.csv(compl_corr_to_investigation1,
          file.path(
            my_paths$outputs,
            paste0(              "more_than_24_compl_corr_to_investigation1_22_23__",
              today(),
              ".csv"
            )
          ),
          row.names = FALSE)
# 435 dplyr::distinct ids

get_date_contacttype <- function(compl_corr_to_investigation1) {
  compl_corr_to_investigation1 |>
    # add a new column date__contacttype with contactdate and contacttype
    mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) |>
    # use 2 columns only
    select(vessel_official_number, date__contacttype) |>
    # [1] 49903     2
    # sort
    arrange(vessel_official_number, date__contacttype) |>
    dplyr::distinct() |>
    group_by(vessel_official_number) |>
    # [1] 1125    2
    # for each vessel id combine all date__contacttypes separated by comma in one cell
    summarise(date__contacttypes = paste(date__contacttype, collapse = ", ")) %>%
    # [1] 435   2
    return()
}

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

compl_corr_to_investigation1 |>
  check_new_vessels()
# 2

date__contacttype_per_id |>
  check_new_vessels()
# 2

## ---- combine output ----
compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation1 |>
  inner_join(date__contacttype_per_id,
             by = "vessel_official_number")

dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31
# 309
# 271
# [1] 522  31

## ---- 2) remove duplicated columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permitgroupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    # "week_start",
    "date__contacttypes"
  ) |>
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number")

dim(compl_corr_to_investigation1_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9
# 108
# str(compl_corr_to_investigation1_short)
# 97
# 116   9

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
# Download first
previous_egr_data_path <-
  file.path(
    my_paths$outputs, current_project_name,
    r"(from_web\egregious violators for investigation - 2023-01-24_to_2023-08-01.csv)"
  )

file.exists(previous_egr_data_path)
# T
vessels_to_mark <-
  read_csv(previous_egr_data_path)

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  # filter(tolower(`Contacted 2x?`) == 'yes') |>
  select(vessel_official_number)

# mark these vessels
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

#### remove some? ----
# vessels_to_remove <-
#   read.csv(file.path(my_paths$inputs, r"(egr_violators\vessels_to_remove_04_05_2023.csv)"))
# names(vessels_to_remove) = "vessel_official_number"
# dim(vessels_to_remove)
# 58

# compl_corr_to_investigation1_short2 <-
#   compl_corr_to_investigation1_short_dup_marked |>
#   filter(!(
#     vessel_official_number %in%
#       vessels_to_remove$vessel_official_number
#   ))
# 164

dim(compl_corr_to_investigation1_short_dup_marked)
# 102
# 27: 164
# 177
# 31
# 108

#### check ----
  # no applicable method for 'distinct' applied to an object of class "character"

length(unique(compl_corr_to_investigation1_short_dup_marked$vessel_official_number))
# 107
# 102
# 27: 164
# 177
# 105
# 108
# 97
# 110
# 116

data_overview(compl_corr_to_investigation1_short_dup_marked) |> head(1)
# vessel_official_number
# 177
# 105
# 108
# 110
# 116

## add comments from the compliance crew (if already exist) ----
# results_with_comments_path <-
#   file.path(
#     my_paths$outputs,
#     current_project_name,
#     r"(from_web\egregious violators for investigation - 06-26-2023.csv)"
#   )

# file.exists(results_with_comments_path)
# T

# results_with_comments <-
#   readr::read_csv(results_with_comments_path,
#                   col_types = cols(.default = 'c'))
#
# dim(results_with_comments)
# 134 13

# all.equal(results_with_comments,
#           compl_corr_to_investigation1_short_output)
# F

# setdiff(results_with_comments$vessel_official_number,
#         compl_corr_to_investigation1_short_dup_marked$vessel_official_number) |>
#   length()
# 68
# 35 (new filter)
# 67
# 71
# in_the_new_res_only <-
#   setdiff(
#     compl_corr_to_investigation1_short_dup_marked$vessel_official_number,
#     results_with_comments$vessel_official_number
#   )
# |> cat()
# 1266718 602091 FL0435LD FL6279PH FL7282LE FL8725DA
# length(in_the_new_res_only)
# 47
# 6
# 1061382 1069364 1168496 1209015 1224219 1259129 1266718 1296710 1308401 1318467 1331794 523112 602091 678141 970286 996263 FL0435LD FL2447TL FL2453TE FL3159TK FL3697PB FL3979EA FL4801NV FL6279PH FL6680JK FL6954LD FL7772SV FL8090RU FL8666CH FL8725DA FL9131RJ FL9446TH FL9793RU FL9914GX GA8847NJ MD9128BD MS8535ZG NC2851DH NC4246DP NC9819DF VA1460CJ
# 34

### join comments

# by = join_by(
#   vessel_official_number,
#   name,
#   permit_expired,
#   permitgroup,
#   permitgroupexpiration,
#   contactrecipientname,
#   contactphone_number,
#   contactemailaddress,
#   date__contacttypes
# )

# compl_corr_to_investigation1_short_output_w_comments <-
#   left_join(compl_corr_to_investigation1_short_dup_marked,
#             results_with_comments,
#             by,
#             # Override the default suffixes, c(".x", ".y") in not merged cols
#     suffix = c(".my_output",
#                ".commented_output")
#             )
# Joining with `by = join_by(vessel_official_number, name, permit_expired,
# permitgroup, permitgroupexpiration, contactrecipientname,
# contactphone_number, contactemailaddress, week_start, date__contacttypes)`

# dim(compl_corr_to_investigation1_short_output_w_comments)
# 38
# 280
# 0
# [1] 105  14
# 108
# 97
# [1] 110  14

#### check no comments ----
# no_comments_vsls <-
#   compl_corr_to_investigation1_short_output_w_comments |>
#   filter(is.na(
#     `Confirmed Egregious? (missing past 6 months, 2 contacts with at least 1 call)`
#   ))
# # |>
# View(no_comments_vsls)
# Rows: 53
# Columns: 14

# in_the_new_res_only_df <-
#   as.data.frame(in_the_new_res_only)
# names(in_the_new_res_only_df) <- "vessel_official_number"

# no_comments_vsls_ids <-
#   no_comments_vsls |>
#   select(vessel_official_number)
# dim(no_comments_vsls_ids)
# 62

# no_comments_vsls_ids |>
#   filter(vessel_official_number == '1305207') |> dim()
# 1
# compl_corr_to_investigation1_short_output_w_comments |>
#   filter(vessel_official_number == '1305207') |> dim()
# [1]  1 21

# setdiff(no_comments_vsls_ids$vessel_official_number, in_the_new_res_only_df) |>
#   length()
# 1305207
# 62

# setdiff(in_the_new_res_only_df, no_comments_vsls_ids$vessel_official_number)
# 0

# output ----
result_file_path <- file.path(
  my_paths$outputs,
  current_project_name,
  paste0(
    "egregious_violators_for_investigation_from_",
    half_year_ago,
    "_to_",
    data_file_date,
    ".csv"
  ))
# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\egregious_violators_for_investigation_from_2023-01-24_to_2023-08-01.csv"

readr::write_csv(
  compl_corr_to_investigation1_short_dup_marked,
    # compl_corr_to_investigation1_short_output_w_comments,
  result_file_path,
  na = "")

compl_corr_to_investigation1_short_dup_marked |>
  check_new_vessels()
# 2
# FL4232JY
# FL7549EJ

## ---- who needs an email ----
# source(file.path(current_project_path, "need_an_email.R"))

# ## ---- no correspondence ----
# source(
#   file.path(
#     current_project_path,
#     "not_compliant_51_plus_weeks_and_no_correspondence.R"
#   )
# )
#
# ## ---- correspondence, no compliance information ----
# no_compl_info <-
#   setdiff(
#     corresp_contact_cnts_clean$vessel_official_number,
#     compl_clean$vessel_official_number
#   )
# length(no_compl_info)
# # 398
# # 136
# # Not in compliance info!
#
# # grep("1131132", compl_clean$vessel_official_number)
# # 0