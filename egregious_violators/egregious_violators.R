# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
current_project_path <-
  file.path(my_paths$git_r, "egregious_violators")

source(file.path(current_project_path, "get_data.R"))

# ---- Preparing compliance info ----

## ---- add permit_expired column ----
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

# dim(compl_clean_w_permit_exp)
# 185538     
# [1] 217772     22
# dim(compl_clean_w_permit_exp_last_27w)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d

## ---- Have only SA permits, exclude those with Gulf permits ----
compl_clean_sa <- 
  compl_clean_w_permit_exp_last_27w |>
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

# today()
# [1] "2023-07-10"
## Not "compliant_" ----
# compl_clean_sa_non_compl <-
  # compl_clean_sa |>
  # filter(compliant_ == 'NO')

# dim(compl_clean_sa_non_compl)
# [1] 18205    23
# [1] 11473    23
# [1] 10597    23
# [1] 12484    23
# [1] 15549    23
# [1] 13992    23

# compl_clean_sa_non_compl |>
  # count_uniq_by_column() |> head(1)
# vesselofficialnumber 1785
# today()
# "2023-06-23"
# vessel_official_number 1573
# [1] "2023-07-10"
# vessel_official_number 1403
# vessel_official_number 1369

## filter for egregious ----
### check if there is no "compliant_ == YES" since half_year_ago ----

last_week_start <- data_file_date - 6

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

dim(compl_clean_sa_non_c_not_exp)
# [1] 10419    23

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
  # permitted for the whole period (disregard the last week)
  filter(total_weeks == (number_of_weeks_for_non_compliancy - 1))

dim(compl_clean_sa_all_weeks_non_c_short)
# 130

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

compl_clean_sa_all_weeks_non_c <-
  compl_clean_sa_non_c_not_exp |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_sa_all_weeks_non_c_short) |> 
# Joining with `by = join_by(vessel_official_number)`
  distinct()

# dim(compl_clean_sa_all_weeks_non_c_long)
# [1] 130   8

### check the last output manually ----
manual_no <- c("1133962",
"1158893",
"1202614",
"FL6540TD",
"NJ3548GR",
"1254648",
"943683",
"1077813",
"1243529",
"1266505",
"1064222",
"FL9628RY",
"FL7060HK",
"978217",
"508385",
"FL2034JA",
"594390",
"1321667",
"591881",
"1233418",
"606326",
"FL1348GL",
"1254999",
"FL3557RC",
"FL1267MZ",
"FL6220RN",
"FL6900MH",
"FL5207CA",
"SC6965DN",
"FL4959PK",
"FL7207LT",
"FL7703LT",
"1279625",
"565041")

# compl_clean_sa_all_weeks_non_c |>
#   filter(vessel_official_number %in%
#            manual_no) |>
#   View()

# compl_clean_sa |>
#   filter(vessel_official_number %in%
#            manual_no) |>
#   View()

## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean)
# vesselofficial_number   3223
# vessel_official_number  3371

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
str(all_vm_ids)
# 284
# 27: 222
# 134

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
# glimpse(corresp_contact_cnts_clean_direct_cnt)

## ---- Add a filter: If there was 1 call or 2 emails (out and in, bc they got the email, we shared the information and received a confirmation) with a direct communication. ----
# to investigation (to NEIS)

## ---- 1) 1 call with a direct communication ----
get_calls_with_direct_communication <-
  function(corresp_contact_cnts_clean_direct_cnt) {
    # save the long filter
    # more than one call
    answered_1_plus_filter <- quo(
      contact_freq > 0 &
        tolower(contacttype) == "call" &
        direct_contact == "yes" &
        tolower(voicemail) ==  "no"
    )
    # use the filter
    corresp_contact_cnts_clean_direct_cnt |>
      filter(!!answered_1_plus_filter) %>%
      return()
  }

calls_with_direct_communication <-
  get_calls_with_direct_communication(corresp_contact_cnts_clean_direct_cnt)
dim(calls_with_direct_communication)
# [1] 12584    23
# 27: [1] 10062    23
# [1] 10475    23

# str(calls_with_direct_communication)

## ---- 2) in and out emails ----
get_both_in_n_out_emails <- function(corresp_contact_cnts_clean) {
  # save a filter: more than 1 email
  emails_filter <- quo(contact_freq > 1 &
                         ((tolower(contacttype) == "email") |
                            (tolower(contacttype) == "other")))
  
  # use emails_filter for incoming
  incoming_2_plus_emails <-
    corresp_contact_cnts_clean |>
    filter(!!emails_filter &
             tolower(calltype) == "incoming") |>
    select(vessel_official_number) |>
    dplyr::distinct()
  # 259
  
  # use emails_filter for outgoing
  outgoing_2_plus_emails <-
    corresp_contact_cnts_clean |>
    filter(!!emails_filter &
             tolower(calltype) == "outgoing") |>
    select(vessel_official_number) |>
    dplyr::distinct()
  # |>
  #   glimpse()
  # 624
  # 712
  
  # get ids wihch are in both in and out lists
  both_in_n_out_2_plus_email_ids <-
    intersect(incoming_2_plus_emails, outgoing_2_plus_emails)
  # 148
  # 173
  
  # keep correspondence information only for those
  corresp_contact_cnts_clean_direct_cnt |>
    filter(
      vessel_official_number %in% both_in_n_out_2_plus_email_ids$vessel_official_number
    ) %>%
    return()
}

both_in_n_out_2_plus_emails <-
  get_both_in_n_out_emails(corresp_contact_cnts_clean)

# check
data_overview(corresp_contact_cnts_clean) |> head(1)
# vesselofficialnumber  3450
# 3571
# 27: vesselofficial_number 3223
# vesselofficial_number 3371

data_overview(both_in_n_out_2_plus_emails)  |> head(1)
# vesselofficialnumber  147
# 173
# 27: 246
# 461
# group_by_arr <- c("vessel_official_number", "calltype")
# count_by_column_arr(both_in_n_out_2_plus_emails, group_by_arr) |> glimpse()

to_investigation_to_NEIS <-
  rbind(both_in_n_out_2_plus_emails,
        calls_with_direct_communication)

# ---- look at the to_investigation_to_NEIS ----
data_overview(to_investigation_to_NEIS) |> head(1)
# vesselofficialnumber  3070
# 3170
# 27: vesselofficial_number 2843
# 3034
# names(to_investigation_to_NEIS)
dim(to_investigation_to_NEIS)
# [1] 14401    23
# 27: [1] 12192    23
# [1] 14463    23

# str(to_investigation_to_NEIS)
# View(to_investigation_to_NEIS)
# count dplyr::distinct values in each column
# apply(to_investigation_to_NEIS, 2, function(x) length(dplyr::distinct(x))) |>
#   as.data.frame() |> head(1)
# vesselofficialnumber  3070
# 3170

## ---- Combine compliance information with filtered correspondence info by vesselofficialnumber ----

compl_corr_to_investigation <-
  inner_join(
    to_investigation_to_NEIS,
    compl_clean_sa_all_weeks_non_c,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

# check
# to_investigation_to_NEIS[15,] FL3262PM
compl_clean_sa_all_weeks_non_c |>
  filter(vessel_official_number == "FL3262PM") |> 
  View()

dim(compl_corr_to_investigation)
# [1] 16081    44
# 27: 11151
# 18093    45
# [1] 264  26
# [1] 264  30

# str(compl_corr_to_investigation)
# compl_corr_to_investigation |>
#   head() |>
#   select(grep("official", names(compl_corr_to_investigation), value = T))

## check
count_uniq_by_column(compl_corr_to_investigation) |> head(1)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# put names into vars
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation, "contact", "date")[1]
contacttype_field_name <-
  find_col_name(compl_corr_to_investigation, "contact", "type")[1]

# write.csv(compl_corr_to_investigation, file.path(my_paths$outputs, "more_than_27_compl_corr_to_investigation_22_23__03_27_2023.csv"), row.names = FALSE)
# 435 dplyr::distinct ids

get_date_contacttype <- function(compl_corr_to_investigation) {
  compl_corr_to_investigation |>
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
  get_date_contacttype(compl_corr_to_investigation)
dim(date__contacttype_per_id)
# [1] 110    2
# 107
# 27: 177
# 188   2
# 105   2 (the new filter)

## ---- combine output ----
compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation |>
  inner_join(date__contacttype_per_id,
             by = "vessel_official_number")

dim(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31

## ---- 2) remove duplicated columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation, ".*contact", "number.*")[1]

# names(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)
compl_corr_to_investigation_short <-
  compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id |>
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

dim(compl_corr_to_investigation_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9

# str(compl_corr_to_investigation_short)

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases. 

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
previous_egr_data_path <-
  file.path(
    my_paths$outputs,
    r"(egregious_violators\from_web\egregious violators for investigation - 04-05-2023 27 weeks.csv)"
  )

# file.exists(previous_egr_data_path)
# T
vessels_to_mark <-
  read_csv(previous_egr_data_path)

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  filter(tolower(`Contacted 2x?`) == 'yes') |>
  select(vessel_official_number)

# mark these vessels
compl_corr_to_investigation_short_dup_marked <-
  compl_corr_to_investigation_short |>
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation_short_dup_marked)
# [1] 177  11
# [1] 105  10

#### remove some? ----
vessels_to_remove <-
  read.csv(file.path(my_paths$inputs, r"(egr_violators\vessels_to_remove_04_05_2023.csv)"))
names(vessels_to_remove) = "vessel_official_number"
dim(vessels_to_remove)
# 58

compl_corr_to_investigation_short2 <-
  compl_corr_to_investigation_short_dup_marked |>
  filter(!(
    vessel_official_number %in%
      vessels_to_remove$vessel_official_number
  ))
# 164

dim(compl_corr_to_investigation_short_dup_marked)
# 102
# 27: 164
# 177
# 31

# dim(compl_corr_to_investigation_short2)
# 164

#### check ----
  # no applicable method for 'distinct' applied to an object of class "character"

length(unique(compl_corr_to_investigation_short_dup_marked$vessel_official_number))
# 107
# 102
# 27: 164
# 177
# 105

data_overview(compl_corr_to_investigation_short_dup_marked) |> head(1)
# vessel_official_number
# 177
# 105

# compl_corr_to_investigation_short_output <-
# compl_corr_to_investigation_short2 |>
#   filter(permit_expired == "no")

# dim(compl_corr_to_investigation_short_output)
# [1] 146  10
# 134
# [1] 151  11
# [1] 146  11 (with removed from Apr)

View(compl_corr_to_investigation_short_dup_marked)

## add comments from the compliance crew (if already exist) ----
results_with_comments_path <-
  file.path(
    my_paths$outputs,
    r"(egregious_violators\from_web\egregious violators for investigation - 06-26-2023.csv)"
  )

file.exists(results_with_comments_path)
# T

results_with_comments <-
  readr::read_csv(results_with_comments_path,
                  col_types = cols(.default = 'c'))

View(results_with_comments)

# all.equal(results_with_comments,
#           compl_corr_to_investigation_short_output)
# F

setdiff(results_with_comments$vessel_official_number,
        compl_corr_to_investigation_short_dup_marked$vessel_official_number) |> 
  length()
# 68
# 35 (new filter)

setdiff(compl_corr_to_investigation_short_dup_marked$vessel_official_number,
        results_with_comments$vessel_official_number) |> cat()
# 1266718 602091 FL0435LD FL6279PH FL7282LE FL8725DA

  length()
# 6

### join comments

by = join_by(vessel_official_number, name, permit_expired,
permitgroup, permitgroupexpiration, contactrecipientname,
contactphone_number, contactemailaddress, week_start)

compl_corr_to_investigation_short_output_w_comments <-
  full_join(results_with_comments,
             compl_corr_to_investigation_short_output,
             by)
# Joining with `by = join_by(vessel_official_number, name, permit_expired,
# permitgroup, permitgroupexpiration, contactrecipientname,
# contactphone_number, contactemailaddress, week_start, date__contacttypes)`

View(compl_corr_to_investigation_short_output_w_comments)
# 38
# 280
# 0
# output ----
write.csv(compl_corr_to_investigation_short_output, file.path(my_paths$outputs, "egregious_violators_for_investigation_27_plus_weeks_06_22_2023.csv"), row.names = FALSE)

## ---- who needs an email ----
source(file.path(current_project_path, "need_an_email.R"))

## ---- no correspondence ----
source(
  file.path(
    current_project_path,
    "not_compliant_51_plus_weeks_and_no_correspondence.R"
  )
)

## ---- correspondence, no compliance information ----
no_compl_info <-
  setdiff(
    corresp_contact_cnts_clean$vessel_official_number,
    compl_clean$vessel_official_number
  )
length(no_compl_info)
# 398
# 136
# Not in compliance info!

# grep("1131132", compl_clean$vessel_official_number)
# 0