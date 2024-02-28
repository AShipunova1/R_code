# Manually check missing addresses
is_empty <- c(NA, "NA", "", "UN")

# From FHIER ----

## fewer fields ----
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

fhier_addr_short__comb_addr <- 
  fhier_addr_short |> 
  clean_names_and_addresses() |> 
  mutate(
    fhier_address =
      str_glue(
        "
        {physical_address_1}, {physical_address_2}, {physical_city}, {physical_county}, {physical_state}, {physical_zip_code}
      "
      )
  ) |>
  dplyr::select(
    -c(
      physical_address_1,
      physical_address_2,
      physical_city,
      physical_county,
      physical_state,
      physical_zip_code
    )
  ) |>
  clean_names_and_addresses() |> 
  distinct()

# dim(fhier_addr_short__comb_addr)
# [1] 2390    5

## 1) add names ----
# View(fhier_addresses)

no_name_vsl_ids <- 
  compl_corr_to_investigation1_short_dup_marked__permit_region |> 
  filter(full_name %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

nrow(no_name_vsl_ids)
# 109

### add FHIER names to previous results ----
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

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols <-
  compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr |>
  relocate(permit_holder_names, .after = contactrecipientname) |>
  relocate(full_name, .after = permit_holder_names) |>
  relocate(fhier_address, .after = full_address) |>
  add_column(
    !!sym(a1_cell_text) := "", 
    .before = "vessel_official_number"
  ) |>
  add_column(Notes = "", .before = "vessel_official_number")

# View(compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols)

