# download the compliance details and filter out those South Atlantic vessels that have never reported, and then check that short list against # of calls/emails (need at least 2, and if never spoken too/responded then they'd need a certified letter from OLE)

source("~/GitHub/R_code/start_module.r")
source("~/GitHub/R_code/useful_functions_module.r")

library(VennDiagram)
library(RColorBrewer)

# ----set up----
add_path_egr <- "egr_violators"
add_path_corresp <- "Correspondence"
add_path_compl <- "FHIER Compliance"

# Use before combining with compliant info
add_count_contacts <- function(all_data_df_cleen) {
  all_data_df_cleen %>%
    mutate(was_contacted = if_else(is.na(contactdate), "no", "yes")) %>% 
    add_count(vesselofficialnumber, was_contacted, name = "contact_freq") ->
    egr_w_cnts
  return(egr_w_cnts)
}

get_compl_and_corresp_data <- function() {
  my_paths <- set_work_dir()
  csv_names_list = list(file.path(add_path_corresp,  "Correspondence21_23.csv"), 
                        file.path(add_path_egr, "egr2022.csv"),
                        file.path(add_path_egr, "egr2023.csv"))
  csv_contents_egr <- load_csv_names(my_paths, csv_names_list)
  csvs_clean1 <- clean_all_csvs(csv_contents_egr)
  
  corresp_arr <- csvs_clean1[[1]]
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
  corresp_arr_contact_cnts %>% 
    change_to_dates("createdon", "%m/%d/%Y %H:%M") %>%
    change_to_dates("contactdate", "%m/%d/%Y %I:%M %p") ->
    corresp_arr_contact_cnts_clean
    
  compl_arr <- list(csvs_clean1[[2]], csvs_clean1[[3]])
  compl <- compl_arr
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }
  
  compl %>% 
    cleen_weeks() %>%
    change_to_dates("permitgroupexpiration", "%m/%d/%Y") ->
    compl_clean
  
  # to do at the end
  # all_data_df <- join_all_csvs(corresp_arr_contact_cnts, compl_arr)
  # all_data_df_cleen <- change_classes(all_data_df)
  return(list(my_paths, compl_clean, corresp_arr_contact_cnts_clean))
}

temp_var <- get_compl_and_corresp_data()
my_paths <- temp_var[[1]]
compl_clean <- temp_var[[2]]
corresp_contact_cnts_clean <- temp_var[[3]]

get_2_plus_contacts <- function(corresp_contact_cnts_clean) {
  corresp_contact_cnts_clean %>%
    filter(contact_freq > 1) %>%
    return()
}
corr_w_cnts_2_plus_contact <- get_2_plus_contacts(corresp_contact_cnts_clean)

corr__2_plus_contacts__not_all_voicemails_ids <- function(corr_w_cnts_2_plus_contact) {
  corr_w_cnts_2_plus_contact %>% 
    group_by(vesselofficialnumber) %>%
    reframe(all_vm = all(voicemail == "YES")) %>% 
    filter(!all_vm) %>% 
    select(vesselofficialnumber) %>%
    unique() ->
    not_all_vm_ids
  
  return(not_all_vm_ids)
}
corr_not_all_vm_ids <- corr__2_plus_contacts__not_all_voicemails_ids(corr_w_cnts_2_plus_contact)

str(corr_w_cnts_2_plus_contact)

get_first_2_out_contactdates <- function(corr_w_cnts_2_plus_contact) {
  corr_w_cnts_2_plus_contact %>%
    filter(vesselofficialnumber %in% corr_not_all_vm_ids$vesselofficialnumber &
      tolower(calltype) == "outgoing") %>%
    select(vesselofficialnumber, contactdate) %>%
    group_by(vesselofficialnumber) %>%
    arrange(vesselofficialnumber, contactdate) %>%
    slice(1:2) %>% 
    ungroup() %>% return()
}

first_2_out_contactdates <- get_first_2_out_contactdates(corr_w_cnts_2_plus_contact)

str(first_2_out_contactdates)

get_2_first_dates_w_info <- function(corr_w_cnts_2_plus_contact) {
  first2_dates_filter <- get_first_2_out_contactdates(corr_w_cnts_2_plus_contact)
  corr_w_cnts_2_plus_contact %>%
    filter(paste0(vesselofficialnumber, contactdate) %in%
             paste0(first2_dates_filter$vesselofficialnumber, first2_dates_filter$contactdate)) %>% 
    return()
}

corr_w_cnts_2_plus_contact_first_2_dates <- get_2_first_dates_w_info(corr_w_cnts_2_plus_contact)
dim(corr_w_cnts_2_plus_contact_first_2_dates)

#---- old part ----

## ----Venn diagram----

myCol <- brewer.pal(8, "Pastel2")[c(1, 2)]
venn_plot_cat0 <- list(egr__not_all_vm__outgoing__no_reports__reason_compl_ids$vesselofficialnumber, given_egr_csv_vessels$vesselofficialnumber)

venn_to_file <- function(venn_plot_cat0, my_paths, myCol) {
  # to a file
  venn.diagram(venn_plot_cat0
               , category.names = c("Script result", "From the manual spreadsheet")
               , filename = file.path(my_paths$outputs, "venn2.png")
               , output = T
               , fill = myCol)
}
# venn_to_file(venn_plot_cat0, my_paths, myCol)

venn_to_show_in_r <- function(venn_plot_cat0) {
  venn_plot_cat1 <- sapply(venn_plot_cat0, length)
  venn_plot_cat_w_intersect <- list(venn_plot_cat1[[1]],
                                    venn_plot_cat1[[2]],
                                    length(intersect(venn_plot_cat0[[1]], venn_plot_cat0[[2]])))
  
  grid.newpage();
  
  venn.plot <- draw.pairwise.venn(venn_plot_cat_w_intersect[[1]],
                                  venn_plot_cat_w_intersect[[2]],
                                  venn_plot_cat_w_intersect[[3]],
                                  c("Script result", "G. spreadsheet"),
                                  fill = c("#AFEEEE", "#FFE4B5"),
                                  ext.pos = (180),
                                  cat.pos = c(-45, 15)
                                  , cat.dist = rep(0.035, 2)
                                  # , cat.dist = rep(0.025, 2)
  );
  grid.newpage();
  grid.draw(venn.plot);
}

venn_to_show_in_r(venn_plot_cat0)


add_other_info <- function(egr_w_cnts_2_plus_contact, no_reports_not_all_vm_ids) {
  egr_w_cnts_2_plus_contact %>% 
      filter((vesselofficialnumber %in% no_reports_not_all_vm_ids$vesselofficialnumber) & 
               contactreason == "Compliance" &
               calltype == "Outgoing"
      ) %>%
    select(vesselofficialnumber, contactdate, followup, loggroup, calltype, voicemail, contacttype, contactreason, contactrecipientname, contactphonenumber, contactemailaddress, contactcomments, srfhuser, createdon, followupnbr, contact_freq) ->
    egr__not_all_vm__outgoing__no_reports__reason_compl
  
  return(egr__not_all_vm__outgoing__no_reports__reason_compl)
}

egr__not_all_vm__outgoing__no_reports__reason_compl <- add_other_info(egr_w_cnts_2_plus_contact, no_reports_not_all_vm_ids)
  

# TODO
# from all egr with 2 contacts: get ids for
# 1) just 1 call
# 2) 2 calls no emails
# 3) not calls
# 4) voicemails only

options(dplyr.summarise.inform = F)

count_by_column_list <- function(my_df, group_by_list) {
  my_df %>%
    arrange(vesselofficialnumber, contactdate) %>%
    group_by_at(group_by_list) %>%
    # summarise(my_freq = n()) %>%
    return()
}

## ---- 1) just 1 call ----
group_by_list = c("vesselofficialnumber", "voicemail", "contacttype")

# count_by_column_list(egr__not_all_vm__outgoing__no_reports__reason_compl, group_by_list) %>% glimpse()

count_by_column_list(egr__not_all_vm__outgoing__no_reports__reason_compl, group_by_list) %>% 
  filter(contact_freq == 1 & contacttype == "Call") %>%
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__not_voicemail__1_call } %>% # save into a var
  glimpse()
# 0 - filteresd out before
  
## ---- 2) 2 calls no emails ----
count_by_column_list(egr__not_all_vm__outgoing__no_reports__reason_compl, group_by_list) %>% 
  filter(contact_freq > 1 &
           contacttype == "Call") %>%
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__not_voicemail__calls_only__2_plus_call} %>% # save into a var
  glimpse()

## ----3) no calls ----
count_by_column_list(egr__not_all_vm__outgoing__no_reports__reason_compl, group_by_list) %>% 
  filter(contacttype != "Call") %>%
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__not_calls_only__2_plus_call} %>% # save into a var
  glimpse()

dim(egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__not_calls_only__2_plus_call)

## ---- 4) voicemails----
count_by_column_list(egr__not_all_vm__outgoing__no_reports__reason_compl, c("vesselofficialnumber", "voicemail")) %>% 
  filter(tolower(voicemail) == "yes") %>%
  { . ->> egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__voicemails} %>% # save into a var
  glimpse()

egr_ids__outgoing__not_all_voicemails__no_reports__2_plus_contacts__voicemails %>% dim()

## ----get 2 first contacts----
names_short_list1 <- c("vesselofficialnumber", "compliant", "contactdate", "calltype", "voicemail", "contacttype", "contactreason", "contactrecipientname", "contactphonenumber", "contactemailaddress", "contactcomments", "contact_freq")

first_2_out_contactdates <- function(egr_w_cnts_2_plus_contact) {
  egr_w_cnts_2_plus_contact %>%
    filter(tolower(calltype) == "outgoing") %>% 
    select(vesselofficialnumber, contactdate) %>%
    group_by(vesselofficialnumber) %>%
    arrange(vesselofficialnumber, contactdate) %>%
    slice(1:2) %>% 
    ungroup() %>% return()
}

first2_dates <- first_2_out_contactdates(egr_w_cnts_2_plus_contact)

test_first_2 <- function(first2_dates, egr_w_cnts_2_plus_contact) {
  str(first2_dates)
  first2_dates %>% select(vesselofficialnumber) %>% unique() %>% str()
  str(egr_w_cnts_2_plus_contact)
  egr_w_cnts_2_plus_contact %>% select(vesselofficialnumber) %>% unique() %>% str()
  
}
# test_first_2(first2_dates, egr_w_cnts_2_plus_contact)

get_all_info_for_2_first_dates <- function(egr_w_cnts_2_plus_contact, first2_dates) {
  egr_w_cnts_2_plus_contact %>%
    filter(paste0(vesselofficialnumber, contactdate) %in% 
             paste0(first2_dates$vesselofficialnumber, first2_dates$contactdate)) %>%
  return()
}

test_output1 <- function(egr_w_cnts_2_plus_contact, first2_dates) {
  all_info_for_2_first_dates <- get_all_info_for_2_first_dates(egr_w_cnts_2_plus_contact, first2_dates)
  dim(all_info_for_2_first_dates)
  
  group_by_list1 <- c("vesselofficialnumber", "voicemail")
  egr_w_cnts_2_plus_contact %>%
    count_by_column_list(group_by_list1) %>% glimpse()

  all_info_for_2_first_dates2 <- get_all_info_for_2_first_dates(egr_w_cnts_2_plus_contact, first2_dates)
  glimpse(all_info_for_2_first_dates2)

  # write.csv(all_info_for_2_first_dates, file.path(my_paths$outputs, "info_for_2_first_dates.csv"), row.names = FALSE)
}

## ---- compare with the existing spreadsheet ----
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\SEFHIER vessels--non-reporting after contact - SA no reports.csv"

egr__not_all_vm__outgoing__no_reports__reason_compl_ids <-
  egr__not_all_vm__outgoing__no_reports__reason_compl %>%
  select(vesselofficialnumber) %>% unique()

upload_data_from_the_egr_spreadsheet <- function(add_path_egr, my_paths) {
  csv_names_list = list(file.path(add_path_egr, "sa_egr_given.csv"))
  given_egr_csv_arr <- load_csv_names(my_paths, csv_names_list)
  given_egr_csv <- clean_headers(given_egr_csv_arr[[1]])
  
  return(given_egr_csv)
}

given_egr_csv <- upload_data_from_the_egr_spreadsheet(add_path_egr, my_paths)

date_format_use_names_as_variable <- function(given_egr_csv,   date_col_names = c("x1stcontact", "x2ndcontact")) {
  
  given_egr_csv %>%
    mutate_at(date_col_names, as.POSIXct, format = "%m/%d/%Y") ->
    given_egr_csv_clean
  
  return(given_egr_csv_clean)
}

given_egr_csv_clean <- date_format_use_names_as_variable(given_egr_csv)
# names(given_egr_csv_clean)[1:4]
prepare_given_ids_for_comparison <- function(given_egr_csv_clean) {
  given_egr_csv_vessels <- 
    given_egr_csv_clean %>%
    rename(vesselofficialnumber = vesselid) 
  # names(given_egr_csv_vessels)[1:4]
  
  # remove wrong row
  given_egr_csv_vessels %<>%
    filter(vesselofficialnumber != "Vessel ID")
  
  return(given_egr_csv_vessels)
}
given_egr_csv_vessels <- prepare_given_ids_for_comparison(given_egr_csv_clean)

## ----Venn diagram----

myCol <- brewer.pal(8, "Pastel2")[c(1, 2)]
venn_plot_cat0 <- list(egr__not_all_vm__outgoing__no_reports__reason_compl_ids$vesselofficialnumber, given_egr_csv_vessels$vesselofficialnumber)

venn_to_file <- function(venn_plot_cat0, my_paths, myCol) {
  # to a file
  venn.diagram(venn_plot_cat0
               , category.names = c("Script result", "From the manual spreadsheet")
               , filename = file.path(my_paths$outputs, "venn2.png")
               , output = T
               , fill = myCol)
}
# venn_to_file(venn_plot_cat0, my_paths, myCol)

venn_to_show_in_r <- function(venn_plot_cat0) {
  venn_plot_cat1 <- sapply(venn_plot_cat0, length)
  venn_plot_cat_w_intersect <- list(venn_plot_cat1[[1]],
                                    venn_plot_cat1[[2]],
                                    length(intersect(venn_plot_cat0[[1]], venn_plot_cat0[[2]])))
  
  grid.newpage();
  
  venn.plot <- draw.pairwise.venn(venn_plot_cat_w_intersect[[1]],
                                  venn_plot_cat_w_intersect[[2]],
                                  venn_plot_cat_w_intersect[[3]],
                                  c("Script result", "G. spreadsheet"),
                                  fill = c("#AFEEEE", "#FFE4B5"),
                                  ext.pos = (180),
                                  cat.pos = c(-45, 15)
                                  , cat.dist = rep(0.035, 2)
                                  # , cat.dist = rep(0.025, 2)
  );
  grid.newpage();
  grid.draw(venn.plot);
}

venn_to_show_in_r(venn_plot_cat0)

# ## ----prepare_data_for_clean_first_two_contacts ----
get_not_vm_outgoing_ids <- function(egr_w_cnts_2_plus_contact) {
  egr_w_cnts_2_plus_contact %>%
      filter(calltype == "Outgoing") %>%
      group_by(vesselofficialnumber) %>%
      reframe(
        no_reports = (xcaptainreports == 0 & xnegativereports == 0),
        all_vm = all(voicemail == "YES")) %>%
      filter(no_reports & !all_vm) %>%
      select(vesselofficialnumber) %>%
      unique() ->
      no_reports_not_all_vm_ids

    return(no_reports_not_all_vm_ids)
}

not_vm_outgoing_ids <- get_not_vm_outgoing_ids(egr_w_cnts_2_plus_contact)

# str(not_vm_outgoing_ids)

filter_egr_w_cnts <- function(egr_w_cnts_2_plus_contact) {
  egr_w_cnts_2_plus_contact %>%
    filter(tolower(calltype) == tolower("Outgoing") &
             tolower(contactreason) == tolower("Compliance") &
             tolower(voicemail) == tolower("no")) %>%
    filter(vesselofficialnumber %in% not_vm_outgoing_ids$vesselofficialnumber) %>%
    return()
}

test_data1 <- filter_egr_w_cnts(egr_w_cnts_2_plus_contact)
# str(test_data1)
# 
# # write.csv(all_info_for_2_first_dates, file.path(my_paths$outputs, "info_for_2_first_dates.csv"), row.names = FALSE)
# 
get_2_first_dates_w_info <- function(test_data1) {
  first2_dates_filter <- first_2_out_contactdates(test_data1)
  test_data1 %>%
    filter(paste0(vesselofficialnumber, contactdate) %in%
             paste0(first2_dates_filter$vesselofficialnumber, first2_dates_filter$contactdate)) %>%
    unique() %>%
    return()
}
res_output <- get_2_first_dates_w_info(test_data1)

test_res_output <- function(res_output, test_data1) {
  t1 <- res_output %>% select(vesselofficialnumber) %>% unique() %>% str()
  t2 <- first_2_out_contactdates(test_data1) %>% select(vesselofficialnumber) %>% unique() %>% str()
  identical(t1, t2) %>% print()
  # TRUE
}

write.csv(res_output, file.path(my_paths$outputs, "info_for_2_first_dates.csv"), row.names = FALSE)

# TODO
# filter out "no answer"

#----Redo with no week info----
no_week_names <- c("vesselofficialnumber", "name", "permitgroup", "permitgroupexpiration", "xgompermitteddeclarations", "xcaptainreports", "xnegativereports", "xcomplianceerrors", "compliant", "setpermitsonhold", "overridden", "overridedate", "overrideby", "contactedwithin48hours", "submittedpowerdown", "primary", "contactdate", "followup", "loggroup", "calltype", "voicemail", "contacttype", "contactreason", "contactrecipientname", "contactphonenumber", "contactemailaddress", "contactcomments", "srfhuser", "createdon", "followupnbr", "srhsvessel", "contact_freq")

filter_egr_w_cnts_no_week <- function(egr_w_cnts_2_plus_contact) {
  
  egr_w_cnts_2_plus_contact %>% 
    select(all_of(no_week_names)) %>%
    filter(tolower(calltype) == tolower("Outgoing") &
             tolower(contactreason) == tolower("Compliance") &
             tolower(voicemail) == tolower("no")) %>%
    filter(vesselofficialnumber %in% not_vm_outgoing_ids$vesselofficialnumber) %>%
    return()
}

egr_w_cnts_no_week <- filter_egr_w_cnts_no_week(egr_w_cnts_2_plus_contact)

res_output2 <- get_2_first_dates_w_info(egr_w_cnts_no_week)

write.csv(res_output2, file.path(my_paths$outputs, "info_for_2_first_dates_no_week.csv"), row.names = FALSE)

