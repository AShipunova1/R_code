# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

# library(VennDiagram)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
curren_project_path <- file.path(my_paths$git_r, "egregious_violators")

source(file.path(curren_project_path, "get_data.R"))

# identical(corresp_contact_cnts_clean0$vesselofficial_number, corresp_contact_cnts_clean0$vessel_official_number)
# T
## ---- Preparing compliance info ----

## ---- Have only SA permits, exclude those with Gulf permits ----
compl_clean_sa <- compl_clean %>%
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

## ---- filter for egregious here, use all compliance data ----

# put col names to variables
gom_declarations_field_name <- find_col_name(compl_clean_sa, ".*gom", "declarations.*")[1]
captainreports_field_name <- find_col_name(compl_clean_sa, ".*captain", "reports.*")[1]
negativereports_field_name <- find_col_name(compl_clean_sa, ".*negative", "reports.*")[1]
complianceerrors_field_name <- find_col_name(compl_clean_sa, ".*compliance", "errors.*")[1]

# Get all entries with no reports and more than 1 compliance error
filter_egregious <- quo(!!sym(gom_declarations_field_name) == 0 &
                          !!sym(captainreports_field_name) == 0 &
                          !!sym(negativereports_field_name) == 0 &
                          !!sym(complianceerrors_field_name) > 0
                        )
# 36965    
# test
# filter_egregious1 <- quo(gom_permitteddeclarations__ == 0 &
                          # captainreports__ == 0 &
                          # negativereports__ == 0 &
                          # complianceerrors__ > 0
# )
# 36965    
compl_clean_sa_non_compl <-
  compl_clean_sa %>%
    filter(!!filter_egregious) 

# compl_clean_sa_non_compl %>%
#   count_uniq_by_column() %>% head(1)
# vesselofficialnumber 1785

## ----- get only those with 51+ weeks of non compliance -----
get_num_of_non_compliant_weeks <- function(compl_clean_sa_non_compl){
  compl_clean_sa_non_compl %>%
    select(vessel_official_number, week) %>%
    arrange(vessel_official_number, week) %>%
    unique() %>%
    # add a column with counts
    count(vessel_official_number) %>% 
    # keep only with count > 51
    filter(n > 51) %>%
    return()
}
id_52_plus_weeks <- get_num_of_non_compliant_weeks(compl_clean_sa_non_compl)
# glimpse(id_52_plus_weeks)
# 'data.frame':	156 obs. of  2 variables
# vesselofficialnumber: ...
# n                   : int  58 55

# ---- Get compliance information for only vessels which have more than 52 "NO REPORT". ----
compl_w_non_compliant_weeks <- 
  compl_clean_sa_non_compl %>%
  filter(vessel_official_number %in% id_52_plus_weeks$vessel_official_number)
# dim(compl_w_non_compliant_weeks)
# [1] 8941   21

## ---- Check vesselofficialnumbers for "all weeks are non-compliant" ----
compliant_field_name <- as.name(find_col_name(compl_clean_sa, ".*comp", "liant.*")[1])

get_all_weeks_not_compliance_id <- function(compl_clean_sa) {
  compl_clean_sa %>% 
    group_by(vessel_official_number) %>% 
    reframe(all_weeks_non_compl = all(tolower(!!sym(compliant_field_name)) == "no")) %>% 
    # leave only those with all weeks are non compliant
    filter(all_weeks_non_compl) %>% 
    select(vessel_official_number) %>%
    unique() %>% 
    return()
}
all_weeks_not_compliance_id <- get_all_weeks_not_compliance_id(compl_clean_sa)
# str(all_weeks_not_compliance_id)
# 343

# all 52+ weeks are non compliant
intersect(id_52_plus_weeks$vessel_official_number, all_weeks_not_compliance_id$vessel_official_number) %>% str()
# 19

# 52+ weeks are not compliant, but some other weeks are compliant
setdiff(id_52_plus_weeks$vessel_official_number, all_weeks_not_compliance_id$vessel_official_number) %>% str()
# 137

# all weeks are not compliant, but there are fewer than 52 weeks for 2022-2023
setdiff(all_weeks_not_compliance_id$vessel_official_number, id_52_plus_weeks$vessel_official_number) %>%
{ . ->> fewer_52_all_non_compl22_23_ids} %>% # save into a var 
  str()
# 324

group_by_arr <- c("vessel_official_number", as.character(compliant_field_name))

compl_clean_sa %>%
    filter(vessel_official_number %in%
             fewer_52_all_non_compl22_23_ids) %>%
  select(vessel_official_number, !!compliant_field_name, week) %>%
  count_by_column_arr(group_by_arr) %>%
  { . ->> fewer_52_all_non_compl22_23} %>% # save into a var 
  head()

# dim(fewer_52_all_non_compl22_23)
# [1] 324   3

# write.csv(fewer_52_all_non_compl22_23, file.path(my_paths$outputs, "fewer_52_all_non_compl22_23.csv"), row.names = FALSE)

## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 %>%
    filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean)

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
# str(all_vm_ids)
# 284

# field_name into a var
contactcomments_field_name <- sym(find_col_name(corresp_contact_cnts_clean, ".*contact", "comments.*")[1])

add_a_direct_contact_column <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    # create a new column "direct_contact" with a "yes" or "no"
    # search comments for indicators that there was no direct contact
    mutate(direct_contact = case_when(grepl("no answer", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("wrong number", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("not in service", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("number.+is incorrect", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("the incorrect number", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("incorrect phone number", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
                                    grepl("call could not be completed as dialed", !!contactcomments_field_name, ignore.case = TRUE) ~ "no",
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
corresp_contact_cnts_clean_direct_cnt <- add_a_direct_contact_column(corresp_contact_cnts_clean)
# glimpse(corresp_contact_cnts_clean_direct_cnt)

## ---- Add a filter: If there was 1 call or 2 emails (out and in, bc they got the email, we shared the information and received a confirmation) with a direct communication. ----
# to investigation (to NEIS)

## ---- 1) 1 call with a direct communication ----
get_calls_with_direct_communication <- function(corresp_contact_cnts_clean_direct_cnt){
  # save the long filter
  # more than one call
  answered_1_plus_filter <- quo(contact_freq > 0 &
                                  tolower(contacttype) == "call" &
                                  direct_contact == "yes" &
                                  tolower(voicemail) ==  "no")
  # use the filter
  corresp_contact_cnts_clean_direct_cnt %>%
    filter(!!answered_1_plus_filter) %>%
    return()
}
calls_with_direct_communication <- get_calls_with_direct_communication(corresp_contact_cnts_clean_direct_cnt)
dim(calls_with_direct_communication)
# [1] 12584    23
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
  both_in_n_out_2_plus_email_ids <- intersect(incoming_2_plus_emails, outgoing_2_plus_emails)
  # 148
  # 173
  
  # keep correspondence information only for those
  corresp_contact_cnts_clean_direct_cnt %>%
    filter(vessel_official_number %in% both_in_n_out_2_plus_email_ids$vessel_official_number) %>%
    return()
}

both_in_n_out_2_plus_emails <- get_both_in_n_out_emails(corresp_contact_cnts_clean)

# check
# data_overview(corresp_contact_cnts_clean) %>% head(1)
# vesselofficialnumber  3450
# 3571
# data_overview(both_in_n_out_2_plus_emails)  %>% head(1)
# vesselofficialnumber  147
# 173

# group_by_arr <- c("vessel_official_number", "calltype")
# count_by_column_arr(both_in_n_out_2_plus_emails, group_by_arr) %>% glimpse()

to_investigation_to_NEIS <- rbind(both_in_n_out_2_plus_emails, calls_with_direct_communication)

# ---- look at the to_investigation_to_NEIS ----
# data_overview(to_investigation_to_NEIS) %>% head(1)
# vesselofficialnumber  3070
# 3170

# dim(to_investigation_to_NEIS)
# [1] 14401    23
# str(to_investigation_to_NEIS)
# View(to_investigation_to_NEIS)
# count unique values in each column
# apply(to_investigation_to_NEIS, 2, function(x) length(unique(x))) %>%
#   as.data.frame() %>% head(1)
# vesselofficialnumber  3070
# 3170

## ---- Combine compliance information with filtered correspondence info by vesselofficialnumber ----

compl_w_non_compliant_weeks %>%
  inner_join(to_investigation_to_NEIS,
             by = c("vessel_official_number"),
             multiple = "all") ->
  compl_corr_to_investigation

# dim(compl_corr_to_investigation)
# [1] 16081    44


## check
# count_uniq_by_column(compl_clean_sa_non_compl) %>% head(1)
# 1785
# count_uniq_by_column(compl_corr_to_investigation) %>% head(1)
# 110
# 107

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# put nammes int vars
contactdate_field_name <- find_col_name(compl_corr_to_investigation, "contact", "date")[1]
contacttype_field_name <- find_col_name(compl_corr_to_investigation, "contact", "type")[1]

get_date_contacttype <- function(compl_corr_to_investigation) {
  compl_corr_to_investigation %>%
    # add a new column date__contacttype with contactdate and contacttype
    mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) %>%
    # use 2 columns only
    select(vessel_official_number, date__contacttype) %>%
    # sort
    arrange(vessel_official_number, date__contacttype) %>%
    unique() %>%
    group_by(vessel_official_number) %>%
    # for each vessel id combine all date__contacttypes separated by comma in one cell
    summarise(date__contacttypes = paste(date__contacttype, collapse=", ")) %>% 
    return()
}

date__contacttype_per_id <- get_date_contacttype(compl_corr_to_investigation)
# dim(date__contacttype_per_id)
# [1] 110    2
# 107

## ---- combine output ----
compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation %>%
  inner_join(date__contacttype_per_id,
             by = c(as.character(vessel_id_field_name)))
  
# str(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)

## ---- 2) remove duplicated columns ----

contactphonenumber_field_name <- find_col_name(compl_corr_to_investigation, ".*contact", "number.*")[1]

compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id %>%
  select(vessel_id_field_name,
         "name",
         "permitgroup",
         "permitgroupexpiration",
         "contactrecipientname",
         contactphonenumber_field_name,
         "contactemailaddress", 
         "week_start",
         "date__contacttypes") %>%
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values(c(as.character(vessel_id_field_name))) ->
  compl_corr_to_investigation_short

# dim(compl_corr_to_investigation_short)
# [1] 107   9

## ---- 3) remove vessels already in the know list ----
vessels_to_remove <- read.csv(file.path(my_paths$inputs, "vessels_to_remove.csv"))
names(vessels_to_remove) = as.character(vessel_id_field_name)

# remove these vessels
compl_corr_to_investigation_short1 <-
  compl_corr_to_investigation_short %>%
  filter(!(vessel_official_number %in%
             vessels_to_remove$vessel_official_number
           )
         )
# dim(compl_corr_to_investigation_short1)
# 102

## check
# length(unique(compl_corr_to_investigation_short$vessel_official_number))
# 107
# 102

# data_overview(compl_corr_to_investigation_short1)
# write.csv(compl_corr_to_investigation_short1, file.path(my_paths$outputs, "egregious_violators_for_investigation.csv"), row.names = FALSE)

## ---- who needs an email ----
source(file.path(curren_project_path, "need_an_email.R"))

## ---- no correspondence ----
source(file.path(curren_project_path, "not_compliant_51_plus_weeks_and_no_correspondence.R"))
