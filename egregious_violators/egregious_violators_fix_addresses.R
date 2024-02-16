## Manually check missing addresses ----
### From FHIER ----

# fewer fields
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
  # mutate(across(where(is.character),
  #             ~ str_replace(., ",$", ""))) |>
  mutate(across(where(is.character),
                ~ replace_na(., ""))) |>
  mutate(across(where(is.character),
                ~ str_squish(.))) |>
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
  mutate(
    across(fhier_address,
           ~ str_trim(.x)),
    across(fhier_address,
           ~ str_replace_all(.x, "\\s+,", ",")),
    across(fhier_address,
           ~ str_replace_all(.x, ",,+", ",")),
    across(fhier_address,
           ~ str_replace_all(.x, ",$", "")),
    across(fhier_address,
           ~ str_replace_all(.x, "^,", ""))
  ) |>
  distinct()

dim(fhier_addr_short__comb_addr)
# [1] 3266    5

# join with the previous results from the db

fhier_addr_short__comb_addr_needed <-
  fhier_addr_short__comb_addr |>
  filter(vessel_official_number %in% no_addr_vessel_ids$P_VESSEL_ID)

# View(fhier_addr_short__comb_addr_needed)
# intersect(names(vessels_permits_participants_short_u_flat_sp_full),
#           names(fhier_addr_short__comb_addr_needed))
# 0

vessels_permits_participants_short_u_flat_sp_full__more_addr <- 
  left_join(
    vessels_permits_participants_short_u_flat_sp_full,
    fhier_addr_short__comb_addr_needed,
    join_by(P_VESSEL_ID == vessel_official_number)
  )

View(vessels_permits_participants_short_u_flat_sp_full__more_addr)
# [1] 3212    8
# "UN"

### check if the address or name missing from the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  filter((is.na(full_name) &
            !is.na(permit_holder_names)) |
           is.na(full_address) &
           !is.na(fhier_address))

dim(new_addr)
# 0

### check if the address or name is a "UN" in the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  filter((full_name == "UN" &
            !is.na(permit_holder_names)) |
           full_address == "UN" &
           !is.na(fhier_address))

dim(addr_name_in_fhier)
# [1] 19 17

addr_name_in_not_fhier <-
  fhier_addr__compl_corr |>
  filter(((!is.na(full_name) | !full_name == "UN") &
            is.na(permit_holder_names)) |
           (!is.na(full_address) | !full_address == "UN") &
           is.na(fhier_address))

dim(addr_name_in_not_fhier)
# 39
