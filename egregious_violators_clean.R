# download the compliance details and filter out those South Atlantic vessels that have never reported, and then check that short list against # of calls/emails (need at least 2, and if never spoken too/responded then they'd need a certified letter from OLE)
# From Leeanne:
# You can download that report from the FHIER compliance report. Within that report you can refine the search parameters like the 2022 year and for "Has Error" at the top, select "No report". The "no report" error will limit the report to Atlantic/South Atlantic vessels that are non-compliant for no reports. You will have to specifically filter out the vessels that are egregious.
# An egregious violator, in the past, is considered as a vessel that has not reported at all (either all 52 weeks out of the year or since permit issue if they were issued new permits throughout the year) but has been contacted (called/emailed) at least twice since the program began Jan 4, 2021. 
# *) pull all, filter "no report" in R instead of "has error"
# Workflow:
# get compliance report with "no report"
# get correspondence report
# upload to R
# add correspondence counts
# keep only specific entries (not a voicemail) (no filter by a reason) bc they should already know about the program from any kind of communication
# 12 months of compliance errors (no reports) for one vessel (or since permit issue if they were issued new permits throughout the year)
# # GOM PermittedDeclarations	# CaptainReports	# NegativeReports	# ComplianceErrors
# # 0	0	0	1
# *) remove outgoing filter
# choose the ones with 2+
# (who needs a certify letter or an investigation)
# group by id and sort by contact date within each group
# get the first 2 entries for each id
# contacttype: other == email for now
# ! only SA permits, exclude those with Gulf permits
# Gulf of Mexico (Gulf) federal for-hire permits: 
#   Charter/Headboat for Reef fish permit (RCG)
#   Historical Captain Charter/Headboat for Reef fish permit (HRCG)
#   Charter/Headboat for Coastal Migratory Pelagic fish permit (CHG) 
#   Historical Captain Charter/Headboat for Coastal Migratory Pelagic fish (HCHG) permit 
# South Atlantic/Atlantic (South Atlantic) federal for-hire permits: 
#   South Atlantic Charter/Headboat for Coastal Migratory Pelagic fish (CHS) permit
#   Atlantic Charter/Headboat for Dolphin/wahoo (CDW) permit
#   South Atlantic Charter/Headboat for Snapper-grouper fish (SC) permit
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
add_path_egr <- "egr_violators"
add_path_corresp <- "Correspondence"
add_path_compl <- "FHIER Compliance"

get_compl_and_corresp_data <- function() {
  csv_names_list = list(file.path(add_path_corresp,  "Correspondence21_23.csv"), 
                        file.path(add_path_egr, "egr2022.csv"),
                        file.path(add_path_egr, "egr2023.csv"))
  # read all csv files
  csv_contents_egr <- load_csv_names(my_paths, csv_names_list)
  
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents_egr)
  
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
corresp_contact_cnts_clean <- temp_var[[2]]

# don't use
# keep only specific entries (outgoing, reason == compliance contacts)
filter_only <- function(corresp_contact_cnts_clean){
  corresp_contact_cnts_clean %>%
    filter(tolower(calltype) == "outgoing" &
             tolower(contactreason) == "compliance"
           # &
             # (tolower(voicemail) != "yes")
    )
}
# corr_w_cnts_contact_out_compl_only <- filter_only(corresp_contact_cnts_clean)
# View(corr_w_cnts_contact_out_compl_only)

add_a_direct_contact_column <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    # search comments for indicators that there was no direct contact
    mutate(direct_contact = case_when(grepl("no answer", contactcomments, ignore.case = TRUE) ~ "no",
                                    grepl("wrong number", contactcomments, ignore.case = TRUE) ~ "no",
                                    grepl("number.*not in service", contactcomments, ignore.case = TRUE) ~ "no",
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
# save the long filter
answered_1_plus_filter <- quo(contact_freq > 0 &
                                tolower(contacttype) == "call" &
                                is.na(direct_contact) &
                                tolower(voicemail) ==  "no")
# use the filter
corresp_contact_cnts_clean_direct_cnt %>%
  filter(!!answered_1_plus_filter) %>%
  # save to a varibale
  { . ->> answered_call_1_plus } %>% 
  glimpse()

## ---- 2) in and out emails ----
# corresp_contact_cnts_clean_direct_c %>%
#   select(calltype) %>% unique()

get_one_calltype_only_ids <- function(my_df, calltypes = c("incoming")) {
  subset(my_df, tolower(calltype) %in% calltypes) %>%
    select(vesselofficialnumber) %>% 
    unique() %>%
    return()
}

## ---- get 2 plus emails ----
# get_not_calls_only <- function(my_df) {
  # save a filter
  emails_filter <- quo(contact_freq > 1 &
                         (tolower(contacttype) == "email") | 
                         (tolower(contacttype) == "other")
  )

  # corresp_contact_cnts_clean_direct_cnt %>%
    # filter(!!emails_filter) -> 
    # orresp_contact_cnts_clean_direct_cnt_emails
  # %>%
    # return()
# }

# get_emails_both_in_n_out(corresp_contact_cnts_clean_direct_cnt) {
  # emails <- get_not_calls_only(corresp_contact_cnts_clean_direct_cnt)
  # glimpse(emails)
  # 1,250
  
get_both_in_n_out_ids <- function(my_df = corresp_contact_cnts_clean_direct_cnt) {
  df_out_ids <- get_one_calltype_only_ids(my_df, "outgoing")
  df_in_ids <- get_one_calltype_only_ids(my_df)
}

both_in_n_out_ids <- get_both_in_n_out_ids(corresp_contact_cnts_clean_direct_cnt)

filter_direct_communication <- function(corresp_contact_cnts_clean_direct_c){
  # create filters
  # 1)
  answered_1_plus_filter <- quo(contact_freq > 0 &
                                tolower(contacttype) == "call" &
                                is.na(direct_contact) &
                                tolower(voicemail) ==  "no")
  emails_filter <- quo(contact_freq > 1 &
                                (tolower(contacttype) == "email") | 
                                  (tolower(contacttype) == "other")
                                )
  answered_emails_filter <- quo()
  
    # check if there are both out and in
    # &
    # calltype 
    # 

  
  
  # 2) 
  corresp_contact_cnts_clean_direct_c %>%
    filter(!!answered_1_plus_filter) %>%
    { . ->> answered_call_1_plus } %>% glimpse
  
  # answered_call_1_plus %>% filter(is.na(direct_contact)) %>% dim() 
  # 13034    
           
             
     # ) %>% return()
}
had_direct_contact <- filter_direct_communication(corresp_contact_cnts_clean)
had_direct_contact %>% dim()
# # Rows: 19,307
# [1] 19116    19

count_by_column_list(had_direct_contact, c("vesselofficialnumber", "contacttype", "calltype", "contact_freq")) %>% str()
  

# who needs an email
# at least 2 correspondences & no direct contact
# keep only 2 or more correspondence with no direct contact, check manually?
get_2_plus_contacts <- function(corr_w_cnts_contact_out_compl_only) {
  corr_w_cnts_contact_out_compl_only %>%
    filter(contact_freq > 1) %>%
    return()
}
corr_w_cnts_2_plus_contact_out_compl_only <- get_2_plus_contacts(corresp_contact_cnts_clean)
# View(corr_w_cnts_2_plus_contact_out_compl_only)

# TODO
# who needs a certify letter
# called twice w no direct communications &
# sent an email w. no answer &
# those with no contact information

## ---- get the first 2 contacts ----
# get ids and the first two dates for each, 
# and then add all the information for them

# use the filter to
# get unique ids (vessel official numbers) for correspondence 
#   with at least 2 contacts, 
#   having not all voicemails,
# TODO remove  outgoing, 
# TODO remove reason == compliance contacts
corr__2_plus_contacts__not_all_voicemails_ids <- function(corr_w_cnts_2_plus_contact_out_compl_only) {
  corr_w_cnts_2_plus_contact_out_compl_only %>% 
    group_by(vesselofficialnumber) %>%
    reframe(all_vm = all(voicemail == "YES")) %>% 
    filter(!all_vm) %>% 
    select(vesselofficialnumber) %>%
    unique() %>%
    return()
}
corr_not_all_vm_ids <- corr__2_plus_contacts__not_all_voicemails_ids(corr_w_cnts_2_plus_contact_out_compl_only)

get_first_2_out_contactdates <- function(corr_w_cnts_2_plus_contact_out_compl_only) {
  corr_w_cnts_2_plus_contact_out_compl_only %>%
    # vesselofficialnumber is in the ids from the previous step
    filter(vesselofficialnumber %in% corr_not_all_vm_ids$vesselofficialnumber) %>%
    # keep only 2 columns
    select(vesselofficialnumber, contactdate) %>%
    group_by(vesselofficialnumber) %>%
    # sort
    arrange(vesselofficialnumber, contactdate) %>%
    # get first 2
    slice(1:2) %>% 
    ungroup() %>% 
    return()
}

get_2_first_dates_w_info <- function(corr_w_cnts_2_plus_contact_out_compl_only) {
  # get_first_2_out_contactdates filter
  first2_dates_filter <- get_first_2_out_contactdates(corr_w_cnts_2_plus_contact_out_compl_only)
  # tibble [2,646 × 2] (S3: tbl_df/tbl/data.frame)
  # $ vesselofficialnumber: chr [1:2646]
  # $ contactdate         : POSIXct[1:2646]
  
  # keep only entries for this filter
  corr_w_cnts_2_plus_contact_out_compl_only %>%
    filter(paste0(vesselofficialnumber, contactdate) %in%
             paste0(first2_dates_filter$vesselofficialnumber, first2_dates_filter$contactdate)) %>% 
    return()
}
corr_w_cnts_2_plus_contact_first_2_dates <- get_2_first_dates_w_info (corr_w_cnts_2_plus_contact_out_compl_only)
# str(corr_w_cnts_2_plus_contact_first_2_dates)
# 'data.frame':	2646 obs. of  19 variables
  
## ---- Compare vessels from the existing spreadsheet with the results of this script ----

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\SEFHIER vessels--non-reporting after contact - SA no reports.csv"

# corr_w_cnts_2_plus_contact_first_2_dates_ids <-
#   corr_w_cnts_2_plus_contact_first_2_dates %>%
#   select(vesselofficialnumber) %>% unique() %>% str()
# 'data.frame':	1748 obs. of  1 variable:
#   $ vesselofficialnumber: chr 

# download https://docs.google.com/spreadsheets/d/1xwk4RfqROs2ybmjcO3UnwyqSBRcSwDiGr4qKZ4JtLbs/edit#gid=0
# and upload into R
upload_data_from_the_egr_spreadsheet <- function(add_path_egr, my_paths) {
  csv_names_list = list(file.path(add_path_egr, "sa_egr_given.csv"))
  given_egr_csv_arr <- load_csv_names(my_paths, csv_names_list)
  given_egr_csv <- clean_headers(given_egr_csv_arr[[1]])
  
  return(given_egr_csv)
}
given_egr_csv <- upload_data_from_the_egr_spreadsheet(add_path_egr, my_paths)

# change classes to POSIXct
date_format_use_names_as_variable <- function(given_egr_csv,   date_col_names = c("x1stcontact", "x2ndcontact")) {
  given_egr_csv %>%
    mutate_at(date_col_names, as.POSIXct, format = "%m/%d/%Y") %>%
    return()
}
given_egr_csv_clean <- date_format_use_names_as_variable(given_egr_csv)
# names(given_egr_csv_clean)[1:3]
# [1]   "senttoole" "vesselid"              "vesselname"

prepare_given_ids_for_comparison <- function(given_egr_csv_clean) {
  # rename "vesselid" to "vesselofficialnumber"
    given_egr_csv_vessels <- 
    given_egr_csv_clean %>%
    rename(vesselofficialnumber = vesselid) 

  # remove the wrong rows from the downloaded info
    wrong_ids <- c("", "Anything after 9/17/2021 goes below the black line", "Vessel ID")
  given_egr_csv_vessels %<>%
    filter(! vesselofficialnumber %in% wrong_ids)
  
  return(given_egr_csv_vessels)
}
given_egr_csv_vessels <- prepare_given_ids_for_comparison(given_egr_csv_clean)
# names(given_egr_csv_vessels)[1:3]
# [1] "senttoole"            "vesselofficialnumber" "vesselname"          

## ---- check the difference between ids which are only in the spreadsheet, or in the script results ----
script_result_ids <- unique(corr_w_cnts_2_plus_contact_out_compl_only$vesselofficialnumber)
spreadsheet_ids <- unique(given_egr_csv_vessels$vesselofficialnumber)

script_only_ids <- setdiff(script_result_ids, spreadsheet_ids)
# str(script_only_ids)
# 2536
# see for example egregious_violators/Correspondence for 977129.csv"
intersect(script_result_ids, spreadsheet_ids) %>% str() 
# 299

## ---- check ids which are only in the spreadsheet, but not in the script results ----
spreadsheet_only_ids <- setdiff(spreadsheet_ids, script_result_ids)
str(spreadsheet_only_ids)
## 30
# write.csv(spreadsheet_only_ids, file.path(my_paths$outputs, "spreadsheet_only_ids.csv"), row.names = FALSE)

# str(spreadsheet_only_ids)
get_correspondence_info_about__spreadsheet_only_ids <- function(spreadsheet_only_ids, corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    filter(vesselofficialnumber %in% spreadsheet_only_ids) %>% 
    return()
}
correspondence_info_about__spreadsheet_only_ids <- get_correspondence_info_about__spreadsheet_only_ids(spreadsheet_only_ids, corresp_contact_cnts_clean)
# str(correspondence_info_about__spreadsheet_only_ids)
# 'data.frame':	18 obs. of  19 variables

# write.csv(correspondence_info_about__spreadsheet_only_ids, file.path(my_paths$outputs, "correspondence_info_about__spreadsheet_only_ids.csv"), row.names = FALSE)

dim(correspondence_info_about__spreadsheet_only_ids)
# only 18, why not 30?
# get ids only
have_info <- correspondence_info_about__spreadsheet_only_ids %>% select(vesselofficialnumber)
# str(have_info)

# get ids for wich there is no info in the correspondence csv
# and look them up in PHIER one by one, see "egregious_violators/spreadsheet_only_ids_info.xlsx"
setdiff(spreadsheet_only_ids, have_info$vesselofficialnumber) %>%
  write.csv(file.path(my_paths$outputs, "spreadsheet_only_ids_no_info.csv"), row.names = FALSE)
  
## ----Venn diagram----
# add custom colors
myCol <- brewer.pal(8, "Pastel2")[c(1, 2)]
# use vesselofficialnumber from both sets
venn_plot_cat <- list(script_result_ids, spreadsheet_ids)

# use this function to write the diagram directly to a file
venn_to_file <- function(venn_plot_cat, my_paths, myCol) {
  venn.diagram(venn_plot_cat
               , category.names = c("Script result", "From the manual spreadsheet")
               , filename = file.path(my_paths$outputs, "venn2.png")
               , output = TRUE
               , fill = myCol)
}
# uncomment to use
# venn_to_file(venn_plot_cat, my_paths, myCol)

# show the diagram in RStudio
venn_to_show_in_r <- function(venn_plot_cat) {
  # get amount of vessels in each set
  venn_plot_cat1 <- sapply(venn_plot_cat, length)
  venn_plot_cat_w_intersect <- list(venn_plot_cat1[[1]],
                                    venn_plot_cat1[[2]],
                                    length(intersect(venn_plot_cat[[1]], venn_plot_cat[[2]])))

  # clean the plot panel
  grid.newpage();
  
  # save the plot
  venn.plot <- draw.pairwise.venn(venn_plot_cat_w_intersect[[1]],
                                  venn_plot_cat_w_intersect[[2]],
                                  venn_plot_cat_w_intersect[[3]],
                                  c("Script result", "G. spreadsheet"),
                                  fill = c("#AFEEEE", "#FFE4B5"),
                                  # put category names in good places
                                  ext.pos = (180)
                                  , cat.pos = c(-45, 15)
                                  , cat.dist = rep(0.035, 2)
  );
  # show the plot
  grid.draw(venn.plot);
}
# clean the plot panel again just in case
grid.newpage();

# run and show the Venn diagram comparing this script result with the existing spreadsheet, to see how many are in common. If they are only in the script result it is because the spreadsheet is made manually when people run across the problem and haven't caught them yet.
# Those only in the spreadsheet but not in the script result are from 2021 or are already taken care of.
venn_to_show_in_r(venn_plot_cat)

##---- output to csv ----
# this script results so far
# str(corr_w_cnts_2_plus_contact_first_2_dates)
# 'data.frame':	2646 obs. of  19 variables:
  
write.csv(corr_w_cnts_2_plus_contact_first_2_dates, file.path(my_paths$outputs, "info_for_2_first_dates_corr_only.csv"), row.names = FALSE)

## ---- Get more info ----

## ---- 1) just 1 call ----
group_by_list = c("vesselofficialnumber", "voicemail", "contacttype", "contact_freq")

# see what there
# count_by_column_list(corr_w_cnts_contact_out_compl_only, group_by_list) %>% glimpse()

# get vessel ids with exactly one call (no voicemail)
count_by_column_list(corr_w_cnts_contact_out_compl_only, group_by_list) %>%
# corr_w_cnts_contact_out_compl_only %>%
  filter(contact_freq == 1 & contacttype == "Call") %>%
  # save into a var
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__1_call } %>% 
  glimpse()
## Rows: 162
  
## ---- 2) 2 calls no emails ----
count_by_column_list(corr_w_cnts_2_plus_contact_out_compl_only, group_by_list) %>%
  filter(contact_freq > 1 &
           contacttype == "Call") %>%
  { . ->> corr_w_cnts_2_plus_contact_out_compl_only__not_voicemail__calls_only__2_plus_call} %>% # save into a var
  dim()
## [1] 1680    5

## ----3) no calls ----
count_by_column_list(corr_w_cnts_2_plus_contact_out_compl_only, group_by_list) %>%
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

## ---- 4) voicemails only ----
count_by_column_list(corr_w_cnts_2_plus_contact_out_compl_only, c("vesselofficialnumber", "voicemail", "contact_freq")) %>%
  filter(tolower(voicemail) == "yes") %>%
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__voicemails} %>% # save into a var
  # glimpse()
  # Rows: 2,032
  # percent voicemails of all contacts
  mutate(percent_vm = scales::label_percent()(my_freq / contact_freq)) %>%
  head()


# TODO
# filter out "no answer" & "wrong number"
# ?? Should not be in the egregious violators if there was no 2 direct communications?

grep("no answer", corr_w_cnts_2_plus_contact_out_compl_only$contactcomments, value = T) %>% glimpse()
# 163
# head()
# need an email

grep("wrong number", corr_w_cnts_2_plus_contact_out_compl_only$contactcomments, value = T) %>% glimpse()
# 14

grep("number.*not in service", corr_w_cnts_2_plus_contact_out_compl_only$contactcomments, value = T) %>% glimpse()
# 33


