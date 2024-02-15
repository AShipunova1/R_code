## ---- who needs an email ----
# at least 2 correspondences & no direct contact
# keep only 2 or more correspondence with no direct contact, check manually?

# compliance_clean <- compl_w_non_compliant_weeks
# dplyr::glimpse(compliance_clean)

# correspondence with contact frequency and direct_contact column
corresp_clean <- corresp_contact_cnts_clean_direct_cnt
# dplyr::glimpse(corresp_clean)

get_2_plus_contacts <- function(corresp_clean) {
  corresp_clean %>%
    filter(contact_freq > 1) %>%
    return()
}
corr_2_plus_contact <- get_2_plus_contacts(corresp_clean)

# called twice w no direct communications &
# sent an email w. no answer &
# those with no contact information
#
# TODO
# separate permit expired
# all contacts outgoing

get_all_not_direct_contact_id <- function(corr_2_plus_contact) {
  corr_2_plus_contact %>%
    dplyr::group_by(vessel_official_number) %>%
    # add a new column all_dc with TRUE if all are not direct contacts
    reframe(all_dc = all(tolower(direct_contact) == "no")) %>%
    # keep these only
    filter(all_dc) %>%
    dplyr::select(vessel_official_number) %>%
    unique() %>%
    return()
}
all_not_direct_contact_id <-
  get_all_not_direct_contact_id(corr_2_plus_contact)
# str(all_not_direct_contact_id)
# 93
# 6

## ---- filter for "email's needed" ----
# no direct contact or no phone number or all are voicemails
#TODO: and exclude if they had an email and if they had at least one phone call!
email_s_needed_short <- corr_2_plus_contact %>%
  filter(
    is.na(!!contactphonenumber_field_name) |
      !!contactphonenumber_field_name == "" |
      vessel_official_number %in%
      all_vm_ids$vessel_official_number |
      vessel_official_number %in%
      all_not_direct_contact_id$vessel_official_number
  )

data_overview(email_s_needed_short) %>% head(1)
# vesselofficialnumber 176
# 93

# sorted:
email_s_needed_to_csv_short_sorted <-
  combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values(email_s_needed_short, "vessel_official_number")

names(email_s_needed_to_csv_short_sorted)

## ---- separate expired permits ----
# str(email_s_needed_to_csv_short_sorted)

compl_clean_w_permit_exp <-
  compl_clean %>%
  dplyr::mutate(permit_expired = dplyr::case_when(permitgroupexpiration > Sys.Date() ~ "no",
                                    .default = "yes")) %>%
  dplyr::select(vessel_official_number,
         permit_expired,
         permitgroup,
         permitgroupexpiration) %>%
  unique()
# dim(compl_clean_w_permit_exp)
# [1] 3757    4

# dim(compliance_clean_w_permit_exp)
# [1] 8941    4
# unique()
# [1] 157   4

email_s_needed_to_csv_short_sorted_w_permit_info <-
  email_s_needed_to_csv_short_sorted %>%
  left_join(compl_clean_w_permit_exp,
            by = "vessel_official_number",
            multiple = "all")

# str(email_s_needed_to_csv_short_sorted_w_permit_info)
# inner_join
# tibble [80 × 26] (S3: tbl_df/tbl/data.frame)
# left_join (14 are not in compliance)
# tibble [94 × 26] (S3: tbl_df/tbl/data.frame)
# View(email_s_needed_to_csv_short_sorted_w_permit_info)

intersect(
  email_s_needed_to_csv_short_sorted_w_permit_info$vessel_official_number,
  compl_clean_w_permit_exp$vessel_official_number
) %>% length()
# 79

intersect(
  email_s_needed_to_csv_short_sorted$vessel_official_number,
  compl_clean_w_permit_exp$vessel_official_number
)
# 79

setdiff(
  email_s_needed_to_csv_short_sorted$vessel_official_number,
  compl_clean_w_permit_exp$vessel_official_number
)
# 14
# Not in compliance info!

# grep("FL9599SN", compl_clean$vessel_official_number)

# View(email_s_needed_to_csv_short_sorted_w_permit_info)
# TODO: grep, setdiff and delete not "vessel_official_number"
email_s_needed_to_csv_short_sorted_w_permit_info1 <-
  email_s_needed_to_csv_short_sorted_w_permit_info %>%
  dplyr::select(-vesselofficial_number)

names(email_s_needed_to_csv_short_sorted_w_permit_info1)
glimpse(email_s_needed_to_csv_short_sorted_w_permit_info1)

email_s_needed_to_csv_not_expired <-
email_s_needed_to_csv_short_sorted_w_permit_info1 %>%
  filter(!permit_expired == "yes")
## ---- output to csv ----
# this script results

# write.csv(email_s_needed_to_csv_short_sorted_w_permit_info1, file.path(my_paths$outputs, "email_s_needed_to_csv_short_sorted_w_permit_info1.csv"), row.names = FALSE)


# write.csv(email_s_needed_to_csv_not_expired, file.path(my_paths$outputs, "email_s_needed_to_csv_not_expired.csv"), row.names = FALSE)
