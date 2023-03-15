## ---- who needs an email ----
# at least 2 correspondences & no direct contact
# keep only 2 or more correspondence with no direct contact, check manually?

# rename for short
compliance_clean <- compl_w_non_compliant_weeks
# glimpse(compliance_clean)

# correspondence with contact frequency and direct_contact column
corresp_clean <- corresp_contact_cnts_clean_direct_cnt
# glimpse(corresp_clean)

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
# TODO?
# -- for email needed --
# all contacts outgoing

get_all_not_direct_contact_id <- function(corr_2_plus_contact) {
  corr_2_plus_contact %>%
    group_by(vesselofficialnumber) %>%
    # add a new column all_dc with TRUE if all are not direct contacts
    reframe(all_dc = all(tolower(direct_contact) == "no")) %>%
    # keep these only
    filter(all_dc) %>% 
    select(vesselofficialnumber) %>%
    unique() %>%
    return()
}
all_not_direct_contact_id <- get_all_not_direct_contact_id(corr_2_plus_contact)
# str(all_not_direct_contact_id)
# 93

## ---- filter for "email's needed" ----
# no direct contact or no phone number or all are voicemails
#TODO: and exclude if they had an email and if they had at least one phone call!
email_s_needed_short <- corr_2_plus_contact %>%
  filter(is.na(contactphonenumber) |
           contactphonenumber == "" |
           vesselofficialnumber %in% all_vm_ids$vesselofficialnumber |
           vesselofficialnumber %in% all_not_direct_contact_id$vesselofficialnumber
  ) 

data_overview(email_s_needed_short) %>% head(1)
# vesselofficialnumber 176

# sorted:
email_s_needed_to_csv_short_sorted <-
  combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values(email_s_needed_short, c("vesselofficialnumber"))

# str(email_s_needed_to_csv_short_sorted)

## ---- output to csv ----
# this script results

# write.csv(email_s_needed_to_csv_short_sorted, file.path(my_paths$outputs, "email_s_needed_to_csv_short_sorted.csv"), row.names = FALSE)

