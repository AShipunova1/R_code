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

## ---- Preparing compliance info ----

## ---- add permit_expired column ----
compl_clean_w_permit_exp <-
  compl_clean %>%
  # if permit group expiration is more than a month from data_file_date than "no"
  mutate(permit_expired = case_when(permitgroupexpiration > (data_file_date + 30) ~ "no",
                                    .default = "yes"))

## ---- add year_month column ----
# half_year_ago <- 
#   floor_date(data_file_date, "month") - months(6)

days_in_27_weeks <- 27*7
half_year_ago <- 
  data_file_date - days_in_27_weeks

compl_clean_w_permit_exp_last_27w <-
  compl_clean_w_permit_exp %>%
  mutate(year_month = as.yearmon(week_start)) %>%
  # keep entries for the last 28 weeks
  filter(year_month >= as.yearmon(half_year_ago))

# dim(compl_clean_w_permit_exp)
# 185538     
# [1] 217772     22
dim(compl_clean_w_permit_exp_last_27w)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d

## ---- Have only SA permits, exclude those with Gulf permits ----
compl_clean_sa <- 
  compl_clean_w_permit_exp_last_27w %>%
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

## ---- filter for egregious here, use all compliance data ----

# # put col names to variables
# gom_declarations_field_name <-
#   find_col_name(compl_clean_sa, ".*gom", "declarations.*")[1]
# captainreports_field_name <-
#   find_col_name(compl_clean_sa, ".*captain", "reports.*")[1]
# negativereports_field_name <-
#   find_col_name(compl_clean_sa, ".*negative", "reports.*")[1]
# complianceerrors_field_name <-
#   find_col_name(compl_clean_sa, ".*compliance", "errors.*")[1]

# # test: Get all entries with no reports and more than 1 compliance error ----
# # create a filter
# filter_egregious <- quo(
#   # use variables with column names we saved in the previous step
#   !!sym(gom_declarations_field_name) == 0 &
#     !!sym(captainreports_field_name) == 0 &
#     !!sym(negativereports_field_name) == 0 &
#     !!sym(complianceerrors_field_name) > 0
# )
# 36965

# use the filter 
# today()
# [1] "2023-07-10"
# Look at "compliant_" only
compl_clean_sa_non_compl <-
  compl_clean_sa %>%
  # filter(!!filter_egregious)
  filter(compliant_ == 'NO')

dim(compl_clean_sa_non_compl)
# [1] 18205    23
# [1] 11473    23
# [1] 10597    23
# [1] 12484    23
# [1] 15549    23
# [1] 13992    23

compl_clean_sa_non_compl %>%
  count_uniq_by_column() %>% head(1)
# vesselofficialnumber 1785
# today()
# "2023-06-23"
# vessel_official_number 1573
# [1] "2023-07-10"
# vessel_official_number 1403
# vessel_official_number 1369

## ----- get only those with n+ weeks of non compliance -----
# number_of_weeks_for_non_compliancy = 51
number_of_weeks_for_non_compliancy = 27
get_num_of_non_compliant_weeks <-
  function(compl_clean_sa_non_compl) {
    # browser()
    compl_clean_sa_non_compl %>%
      # use only 2 columns
      select(vessel_official_number, week) %>%
      # sort
      arrange(vessel_official_number, week) %>%
      unique() %>%
      # add a column with counts
      count(vessel_official_number) %>%
      # save an intermediate result for checking
      {. ->> temp_n_compl_cnts } %>% 
      # keep only with count > number_of_weeks_for_non_compliancy
      filter(n >= number_of_weeks_for_non_compliancy) %>%
      return()
  }

# View(compl_clean_sa_non_compl)

id_n_plus_weeks <-
  get_num_of_non_compliant_weeks(compl_clean_sa_non_compl)

temp_n_compl_cnts %>%
  arrange(desc(n)) %>%
  View()

# check '1066100', marked as a yes, egr.
temp_n_compl_cnts %>% 
  filter(vessel_official_number == '1066100') %>% 
  View()

compl_clean_sa %>%
  filter(vessel_official_number == '1066100') %>%
  View()

compl_clean %>%
  filter(vessel_official_number == '1066100') %>%
  View()

# Why weeks 13 and 14 in my data are compliant?

# all 27
data_overview(id_n_plus_weeks)
# vessel_official_number 241

# 'data.frame':	156 obs. of  2 variables
# vesselofficialnumber: ...
# n                   : int  58 55
# # "2023-06-23" Rows: 226
# today()
# [1] "2023-07-10"
# Rows: 151
# Columns: 2
# $ vessel_official_number <chr> "1000164", "1020529", "1030892", "1064222", …
# $ n                      <int> 28, 28, 27, 28, 28, 28, 28, 28, 28, 28, 28, …
# Rows: 241

compl_clean_sa %>%
  filter(vessel_official_number %in% id_n_plus_weeks$vessel_official_number) %>%
  filter(compliant_ == "YES") %>%
  select(vessel_official_number, year_month, week_num) %>%
  distinct() %>%
  data_overview()
# week_num   13, 14 ?
# vessel_official_number 241
# year_month               2
# week_num                 2

# All weeks in the last 6 m are "non-compliance" ----
compl_clean_sa_all_weeks_cnt <- 
  compl_clean_sa %>% 
  select(vessel_official_number, week, compliant_) %>% 
  add_count(vessel_official_number,
            name = "total_weeks") %>%
  add_count(vessel_official_number, compliant_,
            name = "compl_weeks_amnt")

compl_clean_sa_all_weeks_cnt %>%
  select(-week) %>% 
  dplyr::distinct() %>%
# [1] 3095    4
  filter(compl_weeks_amnt >= (number_of_weeks_for_non_compliancy - 1)) %>%
# 418
# -1 514   
  filter(compliant_ == 'NO') %>%
# 0
  arrange(desc(compl_weeks_amnt), vessel_official_number) %>% 
  View()

# ---- Get compliance information for only vessels which have more than n "NO REPORT". ----
compl_w_non_compliant_weeks <-
  compl_clean_sa_non_compl %>%
  filter(vessel_official_number %in% id_n_plus_weeks$vessel_official_number)
dim(compl_w_non_compliant_weeks)
# [1] 8941   21
# [1] 7943   23
# [1] 5867   23

## ---- Check vesselofficialnumbers for "all weeks are non-compliant" ----
compliant_field_name <-
  as.name(find_col_name(compl_clean_sa, ".*comp", "liant.*")[1])

get_all_weeks_not_compliance_id <- function(compl_clean_sa) {
  compl_clean_sa %>%
    group_by(vessel_official_number) %>%
    reframe(all_weeks_non_compl = all(tolower(!!sym(
      compliant_field_name
    )) == "no")) %>%
    # leave only those with all weeks are non compliant
    filter(all_weeks_non_compl) %>%
    select(vessel_official_number) %>%
    unique() %>%
    return()
}
all_weeks_not_compliance_id <-
  get_all_weeks_not_compliance_id(compl_clean_sa)
# str(all_weeks_not_compliance_id)
# 343
# 355
# 201 
# 169
# all weeks are non compliant
intersect(
  id_n_plus_weeks$vessel_official_number,
  all_weeks_not_compliance_id$vessel_official_number
) %>% str()
# 19
# 27: 185
# 42
# 22

# n+ weeks are not compliant, but some other weeks are compliant
setdiff(
  id_n_plus_weeks$vessel_official_number,
  all_weeks_not_compliance_id$vessel_official_number
) %>% str()
# 137
# 27: 392
# 184
# 166
# all weeks are not compliant, but there are fewer than n weeks for 2022-2023
setdiff(
  all_weeks_not_compliance_id$vessel_official_number,
  id_n_plus_weeks$vessel_official_number
) %>%
  {
    . ->> fewer_n_all_non_compl22_23_ids
  } %>% # save into a var
  str()
# 324
# 27: 170
# 159

group_by_arr <-
  c("vessel_official_number",
    as.character(compliant_field_name))

# compl_clean_sa %>%
#   filter(vessel_official_number %in%
#            fewer_n_all_non_compl22_23_ids) %>%
#   select(vessel_official_number, !!compliant_field_name, week) %>%
#   count_by_column_arr(group_by_arr) %>%
#   {
#     . ->> fewer_n_all_non_compl22_23
#   } %>% # save into a var
#   head()

# str(fewer_n_all_non_compl22_23)
# [1] 324   3
# 27: gropd_df [170 × 3] (S3: grouped_df/tbl_df/tbl/data.frame)
# gropd_df [159 × 3] (S3: grouped_df/tbl_df/tbl/data.frame)


# write.csv(fewer_52_all_non_compl22_23, file.path(my_paths$outputs, "fewer_52_all_non_compl22_23.csv"), row.names = FALSE)

## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 %>%
  filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean)
# vesselofficial_number   3223
# vessel_official_number  3371

## ---- direct_contact ----
## ---- 1) all are voicemails ----
get_all_voicemails_id <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    group_by(vessel_official_number) %>%
    # add a new logical column all_vm with a TRUE if all entries for voicemail column for this vessel are yeses
    reframe(all_vm = all(tolower(voicemail) == "yes")) %>%
    # keep a row only if all_vm == TRUE
    filter(all_vm) %>%
    # keep only one column
    select("vessel_official_number") %>%
    unique() %>%
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
    corresp_contact_cnts_clean %>%
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
    
    # filter(direct_contact == "no") %>%
    # select(vesselofficialnumber) %>%
    # unique() %>%
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
    corresp_contact_cnts_clean_direct_cnt %>%
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
    corresp_contact_cnts_clean %>%
    filter(!!emails_filter &
             tolower(calltype) == "incoming") %>%
    select(vessel_official_number) %>%
    unique()
  # 259
  
  # use emails_filter for outgoing
  outgoing_2_plus_emails <-
    corresp_contact_cnts_clean %>%
    filter(!!emails_filter &
             tolower(calltype) == "outgoing") %>%
    select(vessel_official_number) %>%
    unique()
  # %>%
  #   glimpse()
  # 624
  # 712
  
  # get ids wihch are in both in and out lists
  both_in_n_out_2_plus_email_ids <-
    intersect(incoming_2_plus_emails, outgoing_2_plus_emails)
  # 148
  # 173
  
  # keep correspondence information only for those
  corresp_contact_cnts_clean_direct_cnt %>%
    filter(
      vessel_official_number %in% both_in_n_out_2_plus_email_ids$vessel_official_number
    ) %>%
    return()
}

both_in_n_out_2_plus_emails <-
  get_both_in_n_out_emails(corresp_contact_cnts_clean)

# check
data_overview(corresp_contact_cnts_clean) %>% head(1)
# vesselofficialnumber  3450
# 3571
# 27: vesselofficial_number 3223
# vesselofficial_number 3371

data_overview(both_in_n_out_2_plus_emails)  %>% head(1)
# vesselofficialnumber  147
# 173
# 27: 246
# 461
# group_by_arr <- c("vessel_official_number", "calltype")
# count_by_column_arr(both_in_n_out_2_plus_emails, group_by_arr) %>% glimpse()

to_investigation_to_NEIS <-
  rbind(both_in_n_out_2_plus_emails,
        calls_with_direct_communication)

# ---- look at the to_investigation_to_NEIS ----
data_overview(to_investigation_to_NEIS) %>% head(1)
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
# count unique values in each column
# apply(to_investigation_to_NEIS, 2, function(x) length(unique(x))) %>%
#   as.data.frame() %>% head(1)
# vesselofficialnumber  3070
# 3170

## ---- Combine compliance information with filtered correspondence info by vesselofficialnumber ----


inner_join(
    to_investigation_to_NEIS, 
    compl_w_non_compliant_weeks,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  ) ->
  compl_corr_to_investigation

# to_investigation_to_NEIS[15,] FL3262PM
compl_w_non_compliant_weeks %>%
  filter(vessel_official_number == "FL3262PM") %>% 
  View()

dim(compl_corr_to_investigation)
# [1] 16081    44
# 27: 11151
# 18093    45

# str(compl_corr_to_investigation)
# compl_corr_to_investigation %>%
#   head() %>%
#   select(grep("official", names(compl_corr_to_investigation), value = T))

## check
count_uniq_by_column(compl_clean_sa_non_compl) %>% head(1)
# 1785
# 27: 1969
# vessel_official_number 1573

count_uniq_by_column(compl_corr_to_investigation) %>% head(1)
# 110
# 107
# 27: 177
# vesselofficial_number 188

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# put nammes int vars
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation, "contact", "date")[1]
contacttype_field_name <-
  find_col_name(compl_corr_to_investigation, "contact", "type")[1]

# write.csv(compl_corr_to_investigation, file.path(my_paths$outputs, "more_than_27_compl_corr_to_investigation_22_23__03_27_2023.csv"), row.names = FALSE)
# 435 unique ids

get_date_contacttype <- function(compl_corr_to_investigation) {
  compl_corr_to_investigation %>%
    # add a new column date__contacttype with contactdate and contacttype
    mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) %>%
    # use 2 columns only
    select(vessel_official_number, date__contacttype) %>%
    # [1] 49903     2
    # sort
    arrange(vessel_official_number, date__contacttype) %>%
    unique() %>%
    group_by(vessel_official_number) %>%
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

## ---- combine output ----
compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation %>%
  inner_join(date__contacttype_per_id,
             by = "vessel_official_number")

# str(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)

## ---- 2) remove duplicated columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation, ".*contact", "number.*")[1]

# names(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)
compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id %>%
  select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permitgroupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    "week_start",
    "date__contacttypes",
    "permit_expired"
  ) %>%
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number") ->
  compl_corr_to_investigation_short

dim(compl_corr_to_investigation_short)
# [1] 107   9
# 27: [1] 177  10

# str(compl_corr_to_investigation_short)

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases. 

# vessels_to_remove <-
#   read.csv(file.path(my_paths$inputs, r"(egr_violators\vessels_to_remove_04_05_2023.csv)"))
# names(vessels_to_remove) = "vessel_official_number"
# str(vessels_to_remove)
# 58

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
vessels_to_remove <-
  read_csv(previous_egr_data_path)

# data_overview(vessels_to_remove)

vessels_to_remove_ids <-
  vessels_to_remove %>%
  filter(tolower(`Contacted 2x?`) == 'yes') %>%
  select(vessel_official_number)

# mark these vessels
compl_corr_to_investigation_short1 <-
  compl_corr_to_investigation_short %>%
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_remove$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )
# filter(!(
  #   vessel_official_number %in%
  #     vessels_to_remove$vessel_official_number
  # ))
dim(compl_corr_to_investigation_short1)
# 102
# 27: 164
# 177
# 31
## check
length(unique(compl_corr_to_investigation_short1$vessel_official_number))
# 107
# 102
# 27: 164
# 177

data_overview(compl_corr_to_investigation_short1) %>% head(1)
# vessel_official_number
# 177

compl_corr_to_investigation_short_output <-
compl_corr_to_investigation_short1 %>%
  filter(permit_expired == "no")

dim(compl_corr_to_investigation_short_output)
# [1] 146  10
# 134
# [1] 151  11

View(compl_corr_to_investigation_short_output)

# test compl_corr_to_investigation_short_output non compliant weeks number ----
week_start_1 <-
  compl_corr_to_investigation_short_output %>%
  select(week_start) %>%
  # count commas
  mutate(week_amount = str_count(week_start, ",") + 1) %>%
  filter(week_amount < 27)
  
dim(week_start_1)[[1]] == 0
# TRUE

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