# download the compliance details and filter out those South Atlantic vessels that have never reported, and then check that short list against # of calls/emails (need at least 2, and if never spoken too/responded then they'd need a certified letter from OLE)
# From Leeanne:
# You can download that report from the FHIER compliance report. Within that report you can refine the search parameters like the 2022 year and for "Has Error" at the top, select "No report". The "no report" error will limit the report to Atlantic/South Atlantic vessels that are non-compliant for no reports. You will have to specifically filter out the vessels that are egregious.
# An egregious violator, in the past, is considered as a vessel that has not reported at all (either all 52 weeks out of the year or since permit issue if they were issued new permits throughout the year) but has been contacted (called/emailed) at least twice since the program began Jan 4, 2021. 
# *) pull all, filter "no report" in R instead of "has error"
# TODO: compare with the given
# Workflow:

# get compliance report for 2022-2023
# get correspondence report for all years
# upload to R
# add correspondence counts
# only SA permits, exclude those with Gulf permits
# Gulf of Mexico (Gulf) federal for-hire permits: 
#   Charter/Headboat for Reef fish permit (RCG)
#   Historical Captain Charter/Headboat for Reef fish permit (HRCG)
#   Charter/Headboat for Coastal Migratory Pelagic fish permit (CHG) 
#   Historical Captain Charter/Headboat for Coastal Migratory Pelagic fish (HCHG) permit 
# South Atlantic/Atlantic (South Atlantic) federal for-hire permits: 
#   South Atlantic Charter/Headboat for Coastal Migratory Pelagic fish (CHS) permit
#   Atlantic Charter/Headboat for Dolphin/wahoo (CDW) permit
#   South Atlantic Charter/Headboat for Snapper-grouper fish (SC) permit
# keep only specific entries (not a voicemail) (no filter by a reason) bc they should already know about the program from any kind of communication
# 12 months of compliance errors (no reports) for one vessel (or since permit issue if they were issued new permits throughout the year)
# # GOM PermittedDeclarations	# CaptainReports	# NegativeReports	# ComplianceErrors
# # 0	0	0	1
# contacttype: other == email for now

# Filters:
# 1) SA permits only
#  & > 51 week with no reports
#  & (a direct contact call | 
#     emails in and out)

## Output:
# For the egregious output files, we'll need the following columns:
# Vessel name
# vessel ID
# Permit type(s) list
# Permit expirations, in order of types, as list
# Owner name
# owner address
# owner phone #
# list of non-compliant weeks
# list of contact dates and contact type in parentheses 

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(VennDiagram)
library(RColorBrewer)

# ----set up----
# add my additional folder names
my_paths <- set_work_dir()
add_path_corresp <- "Correspondence"
add_path_compl <- "FHIER Compliance"

get_compl_and_corresp_data <- function() {
  csv_names_list = list(file.path(add_path_corresp,  "Correspondence21_23.csv"), 
                        file.path(add_path_compl, "FHIER_Compliance_22.csv"),
                        file.path(add_path_compl, "FHIER_Compliance_23.csv"))
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
  
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
  
  # specific correspondence manipulations
  corresp_arr <- csvs_clean1[[1]]
  # add a new column with a "yes" if there is a contactdate (and a "no" if not),
  # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
  # change classes from char to POSIXct
  corresp_arr_contact_cnts %>% 
    change_to_dates("createdon", "%m/%d/%Y %H:%M") %>%
    change_to_dates("contactdate", "%m/%d/%Y %I:%M %p") ->
    corresp_arr_contact_cnts_clean
    
  # specific compliance manipulations
  compl_arr <- list(csvs_clean1[[2]], csvs_clean1[[3]])
  # combine 2 separate dataframes for 2022 and 2023 into one
  compl <- compl_arr
  # if it is one df already, do nothing
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }
  
  compl %>% 
    # split week column (52: 12/26/2022 - 01/01/2023) into 3 columns with proper classes, week_num (week order number), week_start and week_end
    clean_weeks() %>%
    # change dates classes from char to POSIXct 
    change_to_dates("permitgroupexpiration", "%m/%d/%Y") ->
    compl_clean
  
  return(list(compl_clean, corresp_arr_contact_cnts_clean))
}

## ---- get csv data into variables ----
temp_var <- get_compl_and_corresp_data()
compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

# write.csv(filter(corresp_contact_cnts_clean0, vesselofficialnumber == "132038"), file.path(my_paths$outputs, "132038_info.csv"), row.names = FALSE)

## ---- Preparing compliance info ----
## ---- only SA permits, exclude those with Gulf permits ----
# data_overview(compl_clean)
# names(compl_clean)

## ---- remove GOM permits ----
compl_clean_sa <- compl_clean %>%
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

## ---- filter for egr here, use all compliance data ----
filter_egregious <- quo(xgompermitteddeclarations == 0 &
                          xcaptainreports == 0 &
                          xnegativereports == 0 &
                          xcomplianceerrors > 0
                        )
compl_clean_sa_non_compl <-
  compl_clean_sa %>%
    filter(!!filter_egregious) 

# compl_clean_sa_non_compl %>%
  # count_uniq_by_column()
# vesselofficialnumber      1878

## ----- get only those with 51+ weeks of non compliance -----
get_num_of_non_compliant_weeks <- function(compl_clean_sa_non_compl){
  compl_clean_sa_non_compl %>%
    select("vesselofficialnumber", "week") %>%
    arrange("vesselofficialnumber", "week") %>%
    unique() %>%
    count(vesselofficialnumber) %>% 
    filter(n > 51) %>%
    return()
}
id_52_plus_weeks <- get_num_of_non_compliant_weeks(compl_clean_sa_non_compl)
# glimpse(id_52_plus_weeks)
# 'data.frame':	154 obs. of  2 variables
# vesselofficialnumber: ...
# n                   : int  58 55

# cleaned data to combine with the correspondence info
compl_w_non_compliant_weeks <- 
  compl_clean_sa_non_compl %>%
  filter(vesselofficialnumber %in% id_52_plus_weeks$vesselofficialnumber)

# data_overview(compl_w_non_compliant_weeks)

get_all_weeks_not_compliance_id <- function(corr_df) {
  corr_df %>% 
    group_by(vesselofficialnumber) %>%
    reframe(all_weeks_non_compl = all(tolower(compliant) == "no")) %>% 
    filter(all_weeks_non_compl) %>% 
    select(vesselofficialnumber) %>%
    unique() %>%
    return()
}
all_weeks_not_compliance_id <- get_all_weeks_not_compliance_id(compl_clean_sa_non_compl)
str(all_weeks_not_compliance_id)
# 586

intersect(id_52_plus_weeks$vesselofficialnumber, all_weeks_not_compliance_id$vesselofficialnumber) %>% str()
# 23

setdiff(id_52_plus_weeks$vesselofficialnumber, all_weeks_not_compliance_id$vesselofficialnumber)

setdiff(all_weeks_not_compliance_id$vesselofficialnumber, id_52_plus_weeks$vesselofficialnumber)

## ---- Preparing Correspondence ----
## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 %>%
    filter(!grepl("^99999", vesselofficialnumber))

# data_overview(corresp_contact_cnts_clean)

## ---- direct_contact ----
## ---- 1) all are voicemails ----
get_all_voicemails_id <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    group_by(vesselofficialnumber) %>%
    reframe(all_vm = all(tolower(voicemail) == "yes")) %>%
    filter(all_vm) %>% 
    select(vesselofficialnumber) %>%
    unique() %>%
    return()
}

all_vm_ids <- get_all_voicemails_id(corresp_contact_cnts_clean)
str(all_vm_ids)
# 277

add_a_direct_contact_column <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    # search comments for indicators that there was no direct contact
    mutate(direct_contact = case_when(grepl("no answer", contactcomments, ignore.case = TRUE) ~ "no",
                                    grepl("wrong number", contactcomments, ignore.case = TRUE) ~ "no",
                                    grepl("number.*not in service", contactcomments, ignore.case = TRUE) ~ "no",
                                    vesselofficialnumber %in% all_vm_ids$vesselofficialnumber ~ "no",
                                    .default = "yes"
                                    )
         ) %>% 
    return()
  
  # filter(direct_contact == "no") %>%
  # select(vesselofficialnumber) %>% 
  # unique() %>%
  # str()
  # 836 
}
corresp_contact_cnts_clean_direct_cnt <- add_a_direct_contact_column(corresp_contact_cnts_clean)
glimpse(corresp_contact_cnts_clean_direct_cnt)

## ---- Add a filter: If there was 1 call or 2 emails (out and in, bc they got the email, we shared the information and received a confirmation) with a direct communication. ----
# to investigation (to NEIS)

## ---- 1) 1 call with a direct communication ----
get_calls_with_direct_communication <- function(corresp_contact_cnts_clean_direct_cnt){
  # save the long filter
  answered_1_plus_filter <- quo(contact_freq > 0 &
                                  tolower(contacttype) == "call" &
                                  direct_contact == "yes" &
                                  tolower(voicemail) ==  "no")
  # use the filter
  corresp_contact_cnts_clean_direct_cnt %>%
    filter(!!answered_1_plus_filter) %>%
    return()
    # # save to a varibale
    # { . ->> answered_call_1_plus } %>% 
    # glimpse()
}
calls_with_direct_communication <- get_calls_with_direct_communication(corresp_contact_cnts_clean_direct_cnt)
# dim(calls_with_direct_communication)
# str(calls_with_direct_communication)

## ---- 2) in and out emails ----
get_both_in_n_out_emails <- function(corresp_contact_cnts_clean) {
  emails_filter <- quo(contact_freq > 1 &
                         ((tolower(contacttype) == "email") | 
                            (tolower(contacttype) == "other")))
  
  corresp_contact_cnts_clean %>%
    filter(!!emails_filter &
             tolower(calltype) == "incoming") %>% 
    select(vesselofficialnumber) %>%
    unique() -> incoming_2_plus_emails
  # %>%
  #   glimpse()
  # 232  
  
  corresp_contact_cnts_clean %>%
    filter(!!emails_filter &
             tolower(calltype) == "outgoing") %>% 
    select(vesselofficialnumber) %>%
    unique() -> outgoing_2_plus_emails
    # glimpse()
  # 624
  
  both_in_n_out_2_plus_email_ids <- intersect(incoming_2_plus_emails, outgoing_2_plus_emails)
  # 148
  
  corresp_contact_cnts_clean_direct_cnt %>%
    filter(vesselofficialnumber %in% both_in_n_out_2_plus_email_ids$vesselofficialnumber) %>%
    return()
}

both_in_n_out_2_plus_emails <- get_both_in_n_out_emails(corresp_contact_cnts_clean)

# check
# data_overview(corresp_contact_cnts_clean) %>% head()
# vesselofficialnumber  3450
# data_overview(both_in_n_out_2_plus_emails)  %>% head()
# vesselofficialnumber  147

# group_by_arr <- c("vesselofficialnumber", "calltype")
# count_by_column_arr(both_in_n_out_2_plus_emails, group_by_arr) %>% glimpse()

to_investigation_to_NEIS <- rbind(both_in_n_out_2_plus_emails, calls_with_direct_communication)

# ---- look at the to_investigation_to_NEIS ----
# data_overview(to_investigation_to_NEIS)
# vesselofficialnumber  3070

# dim(to_investigation_to_NEIS)
# str(to_investigation_to_NEIS)
# View(to_investigation_to_NEIS)
# apply(to_investigation_to_NEIS, 2, function(x) length(unique(x))) %>% as.data.frame()
# vesselofficialnumber  3070

## ---- Combine compliance information with filtered correspondence info ----
compl_w_non_compliant_weeks %>%
  inner_join(to_investigation_to_NEIS,
             by = c("vesselofficialnumber"),
             multiple = "all") ->
  compl_corr_to_investigation

## check
# count_uniq_by_column(compl_clean_sa_non_compl) %>% head()
# count_uniq_by_column(compl_corr_to_investigation) %>% head()
# 110

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

get_date_contacttype <- function(compl_corr_to_investigation) {
compl_corr_to_investigation %>%
  mutate(date__contacttype = paste(contactdate, contacttype, sep = " ")) %>% 
  select(vesselofficialnumber, date__contacttype) %>%
  arrange(vesselofficialnumber, date__contacttype) %>%
  unique() %>%
  group_by(vesselofficialnumber) %>% 
  summarise(date__contacttypes = paste(date__contacttype, collapse=", ")) %>%
    return()
}

date__contacttype_per_id <- get_date_contacttype(compl_corr_to_investigation)
str(date__contacttype_per_id)
# [1] 110    2

## ---- combine output ----
compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation %>%
  inner_join(date__contacttype_per_id,
             by = c("vesselofficialnumber"))
  
# str(compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id)

## ---- 2) remove duplicated columns ----

# names(compl_corr_to_investigation)

compl_corr_to_investigation_w_non_compliant_weeks_n_date__contacttype_per_id %>%
  select("vesselofficialnumber",
         "name",
         "permitgroup",
         "permitgroupexpiration",
         "contactrecipientname",
         "contactphonenumber",
         "contactemailaddress", 
         "week_start",
         "date__contacttypes") %>% 
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values(c("vesselofficialnumber")) ->
  compl_corr_to_investigation_short

dim(compl_corr_to_investigation_short)

# names_out_arr <- c("vesselofficialnumber",
#                    "name",
#                    "permitgroup",
#                    "permitgroupexpiration",
#                    "contactrecipientname",
#                    "contactphonenumber",
#                    "contactemailaddress",
#                    "list_of_non_compliant_weeks",
#                    "list_of_contact_dates_and_contact_type"
# )

vessels_to_remove <- c("639564", 
"659046", 
"923218", 
"924651", 
"946409", 
"FL5569FG", 
"FL8090RU", 
"FL9024NH", 
"FL9259SW", 
"FL9683MD", 
"MD9128BD", 
"NJ3548GR")

compl_corr_to_investigation_short1_ids <- setdiff(compl_corr_to_investigation_short$vesselofficialnumber, vessels_to_remove)

str(compl_corr_to_investigation_short1_ids)

compl_corr_to_investigation_short1 <- 
  compl_corr_to_investigation_short %>%
  filter(vesselofficialnumber %in% compl_corr_to_investigation_short1_ids)

# data_overview(compl_corr_to_investigation_short1)
write.csv(compl_corr_to_investigation_short1, file.path(my_paths$outputs, "egregious_violators_for_investigation.csv1"), row.names = FALSE)

## ---- draft ----

omars_names <- c("1052994", 
                 "1069364", 
                 "1098501", 
                 "1114138", 
                 "517614", 
                 "529768", 
                 "581401", 
                 "605367", 
                 "639564", 
                 "929266", 
                 "946409", 
                 "964983", 
                 "971427", 
                 "FL1431JU", 
                 "FL1767LJ", 
                 "FL1845GB", 
                 "FL2014PB", 
                 "FL3253NR", 
                 "FL3589PP", 
                 "FL4589RZ", 
                 "FL5036PH", 
                 "FL5554DL", 
                 "FL6444MJ", 
                 "FL6516KH", 
                 "FL7037LR", 
                 "FL7217FP", 
                 "FL8090RU", 
                 "FL8777RF", 
                 "FL9584PX", 
                 "FL9728HE", 
                 "NC0410DD", 
                 "NC0913EE", 
                 "NC7015DR")

compl_corr_to_investigation_short_ids2 <- setdiff(tolower(compl_corr_to_investigation_short1$vesselofficialnumber), tolower(omars_names))

str(compl_corr_to_investigation_short_ids2)
# 102

intersect(tolower(compl_corr_to_investigation_short1$vesselofficialnumber), tolower(omars_names))

in_given <- intersect(spreadsheet_ids, compl_corr_to_investigation_short$vesselofficialnumber)
# %>% str()
# 48

in_given_only <- setdiff(tolower(spreadsheet_ids), tolower(compl_corr_to_investigation_short$vesselofficialnumber))
str(in_given_only)
# 281

in_results_only <- setdiff(tolower(compl_corr_to_investigation_short$vesselofficialnumber), tolower(spreadsheet_ids))
str(in_results_only)
# 62

## ---- who needs an email ----
# at least 2 correspondences & no direct contact
# keep only 2 or more correspondence with no direct contact, check manually?
compliance_clean <- compl_w_non_compliant_weeks
glimpse(compliance_clean)
corresp_clean <- corresp_contact_cnts_clean_direct_cnt
glimpse(corresp_clean)

get_2_plus_contacts <- function(corresp_clean) {
  corresp_clean %>%
    filter(contact_freq > 1) %>%
    return()
}
corr_2_plus_contact <- get_2_plus_contacts(corresp_clean)


# TODO
# called twice w no direct communications &
# sent an email w. no answer &
# those with no contact information

# ## ---- 1) all are voicemails ----
# get_all_voicemails_id <- function(corr_2_plus_contact) {
#   corr_2_plus_contact %>%
#     group_by(vesselofficialnumber) %>%
#     reframe(all_vm = all(tolower(voicemail) == "yes")) %>%
#     filter(all_vm) %>% 
#     select(vesselofficialnumber) %>%
#     unique() %>%
#     return()
# }
# 
# all_vm_ids <- get_all_voicemails_id(corr_2_plus_contact)
# # str(all_vm_ids)
# # 86

get_all_not_direct_contact_id <- function(corr_2_plus_contact) {
  corr_2_plus_contact %>%
    group_by(vesselofficialnumber) %>%
    reframe(all_dc = all(tolower(direct_contact) == "no")) %>%
    filter(all_dc) %>% 
    select(vesselofficialnumber) %>%
    unique() %>%
    return()
}
all_not_direct_contact_id <- get_all_not_direct_contact_id(corr_2_plus_contact)
str(all_not_direct_contact_id)
# $ vesselofficialnumber: chr [1:4] "945896" "991667" "FL6531ND" "NC-9578 WS"

# filter for "email's needed":
email_s_needed <- corr_2_plus_contact %>%
  filter(direct_contact == "no" |
           is.na(contactphonenumber) |
           contactphonenumber == "" |
           vesselofficialnumber %in% all_vm_ids$vesselofficialnumber
  ) 
# vesselofficialnumber  940

# all no direct contact
email_s_needed_short <- corr_2_plus_contact %>%
  filter(is.na(contactphonenumber) |
           contactphonenumber == "" |
           vesselofficialnumber %in% all_vm_ids$vesselofficialnumber |
           vesselofficialnumber %in% all_not_direct_contact_id$vesselofficialnumber
         
  ) 
# %>%
# data_overview(email_s_needed_short)
# vesselofficialnumber 170
  
# email_s_needed_to_csv <- combine_rows_based_on_multiple_columns_and_keep_all_unique_values(email_s_needed, c("vesselofficialnumber"))

# sorted:
email_s_needed_to_csv_short_sorted <-
combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values(email_s_needed_short, c("vesselofficialnumber"))

str(email_s_needed_to_csv_short_sorted)
## ---- output to csv ----
# this script results

write.csv(email_s_needed_to_csv_short_sorted, file.path(my_paths$outputs, "email_s_needed_to_csv_short_sorted.csv"), row.names = FALSE)

# TODO:
# done add all vm to "not direct contact"
# -- for email needed --
# all contacts outgoing


## ---- Get more info ----

## ---- no calls ----
count_by_column_arr(corr_w_cnts_2_plus_contact_out_compl_only, group_by_arr) %>%
  filter(contacttype != "Call") %>%
  { . ->> corr_w_cnts_2_plus_contact_out_compl_only__not_calls_only} %>% # save into a var 
  glimpse()

## What if not "calls"?
# ungroup(corr_w_cnts_2_plus_contact_out_compl_only__not_calls_only) %>%
#   select(contacttype) %>%
#   unique() 
# 1 Other      
# 2 Email

# dim(corr_w_cnts_2_plus_contact_out_compl_only__not_calls_only)
## [1] 228   5



