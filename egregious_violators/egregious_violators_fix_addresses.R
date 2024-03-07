# Manually check missing addresses
is_empty <- c(NA, "NA", "", "UN", "N/A")

# From FHIER ----

## fewer fields ----
# fhier_addresses are from get_data (For-hire Primary Physical Address List)
fhier_addr_short <-
  fhier_addresses |>
  dplyr::select(
    vessel_official_number,
    permit_holder_names,
    physical_address_1,
    physical_address_2,
    physical_city,
    physical_county,
    physical_state,
    physical_zip_code,
    phone_number,
    primary_email
  )

fhier_addr_short_clean <-
  fhier_addr_short |>
  clean_names_and_addresses() |> 
  distinct()

# nrow(fhier_addr_short_clean)

# don't combine address
# fhier_addr_short__comb_addr <- 
#   fhier_addr_short |> 
#   clean_names_and_addresses() |> 
#   mutate(
#     fhier_address =
#       str_glue(
#         "
#         {physical_address_1}, {physical_address_2}, {physical_city}, {physical_county}, {physical_state}, {physical_zip_code}
#       "
#       )
#   ) |>
#   dplyr::select(
#     -c(
#       physical_address_1,
#       physical_address_2,
#       physical_city,
#       physical_county,
#       physical_state,
#       physical_zip_code
#     )
#   ) |>
#   clean_names_and_addresses() |> 
#   distinct()

# dim(fhier_addr_short__comb_addr)
# [1] 2390    5

# dim(fhier_addr_short_clean)

## 1. add addresses from FHIER ----
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr <-
  left_join(compl_corr_to_investigation__corr_date__hailing_port,
            fhier_addr_short_clean)
# Joining with `by = join_by(vessel_official_number)`

# View(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)

# check if no name, phone or email
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  filter(
    is.na(contactrecipientname) |
      is.na(contactphone_number) |
      is.na(contactemailaddress)
  ) |> nrow()
0

### combine fhier info from correspondence and the physical addr.

coacross <- function(...) {
  coalesce(!!!across(...))
}

col_pairs__correspond__physical__list <-
  list(
    c("contactrecipientname", "permit_holder_names"),
    c("contactphone_number", "phone_number"),
    c("contactemailaddress", "primary_email")
  )

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__comb <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  mutate(fhier_owner_name = coacross(col_pairs__correspond__physical__list[[1]]),
         fhier_phone = coacross(col_pairs__correspond__physical__list[[2]]),
         fhier_email = coacross(col_pairs__correspond__physical__list[[3]]))

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__comb |> 
  filter(is.na(contactphone_number)) |> View()

# print_df_names(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__comb)

print_df_names(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
### vessels with no addresses ----

# print_df_names(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
no_name_vsl_ids <- 
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |> 
  filter(physical_address_1 %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

nrow(no_name_vsl_ids)
# 109

### add FHIER names to previous results ----

# Explanations:
# 1. Use 'fhier_addr_short__comb_addr' as a base DataFrame.
# 2. Filter rows where 'vessel_official_number' is in 'no_name_vsl_ids' and 'permit_holder_names' is not empty.
# 3. Select 'vessel_official_number' and 'permit_holder_names'.
# 4. Remove duplicate rows.
# 5. Perform a right join with 'compl_corr_to_investigation1_short_dup_marked__permit_region' using 'vessel_official_number'.
# 6. The result is stored in 'compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names'.

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names <-
  fhier_addr_short__comb_addr |>
  filter(vessel_official_number %in% no_name_vsl_ids$vessel_official_number) |>
  filter(!permit_holder_names %in% is_empty) |>
  select(vessel_official_number, permit_holder_names) |>
  distinct() |>
  right_join(
    compl_corr_to_investigation1_short_dup_marked__permit_region,
    join_by(vessel_official_number)
  )

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names |> 
  nrow()
# 262

## 2) add addresses ----
no_addr_vsl_ids <- 
  compl_corr_to_investigation1_short_dup_marked__permit_region |>
  filter(full_address %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

nrow(no_addr_vsl_ids)
# 183

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr <-
  fhier_addr_short__comb_addr |>
  # filter(vessel_official_number %in% no_addr_vsl_ids$vessel_official_number) |>
  filter(!fhier_address %in% is_empty) |>
  # nrow()
  # 99
  select(vessel_official_number, fhier_address) |>
  distinct() |>
  right_join(
    compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names,
    join_by(vessel_official_number)
  )

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr |> 
  dim()
# [1] 262  16

#### move fileds ----
a1_cell_text <-
  str_glue(
    "Confirmed Egregious? (permits must still be active as of {last_week_start}, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call (voicemail counts) and at least 1 email)"
  )

# Explanations:
# 1. Use the 'relocate' function to rearrange columns in 'compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr'.
# 2. Move 'permit_holder_names' column after 'contactrecipientname'.
# 3. Move 'full_name' column after 'permit_holder_names'.
# 4. Move 'fhier_address' column after 'full_address'.
# 5. Move 'hailing_port_city' column after 'sero_home_port'.
# 6. Move 'hailing_port_state' column after 'hailing_port_city'.
# 7. Use 'add_column' to add an empty column with the name in 'a1_cell_text' before 'vessel_official_number'.
# 8. Use 'add_column' to add a 'Notes' column before 'vessel_official_number'.
# 9. '!!sym(a1_cell_text)' is used for dynamic column naming.
# 10. The result is stored in 'compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols'.
compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols <-
  compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr |>
  relocate(permit_holder_names, .after = contactrecipientname) |>
  relocate(full_name, .after = permit_holder_names) |>
  relocate(fhier_address, .after = full_address) |>
  relocate(hailing_port_city, .after = sero_home_port) |>
  relocate(hailing_port_state, .after = hailing_port_city) |>
  add_column(
    !!sym(a1_cell_text) := "", 
    .before = "vessel_official_number"
  ) |>
  add_column(Notes = "", .before = "vessel_official_number")

# View(compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols)

