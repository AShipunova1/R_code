# Manually check missing addresses ----
is_empty <- c(NA, "NA", "", "UN")

## From FHIER ----

### 1) add names ----
# View(fhier_addresses)

no_name_vsl_ids <- 
  compl_corr_to_investigation1_short_dup_marked__permit_region |> 
  filter(full_name %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

nrow(no_name_vsl_ids)
# 109

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names <- 
  fhier_addresses |>
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

# 2) add addresses ----

# fewer fields ----
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

dim(fhier_addr_short__comb_addr)
# [1] 3266    5

# join with the previous results from the db

fhier_addr_short__comb_addr_needed <-
  fhier_addr_short__comb_addr |>
  filter(vessel_official_number %in% no_addr_vessel_ids$P_VESSEL_ID)

dim(fhier_addr_short__comb_addr_needed)
# 1218

# intersect(names(vessels_permits_participants_short_u_flat_sp_full),
#           names(fhier_addr_short__comb_addr_needed))
# 0

vessels_permits_participants_short_u_flat_sp_full__more_addr <- 
  left_join(
    vessels_permits_participants_short_u_flat_sp_full,
    fhier_addr_short__comb_addr_needed,
    join_by(P_VESSEL_ID == vessel_official_number)
  )

# dim(vessels_permits_participants_short_u_flat_sp_full__more_addr)
# [1] 3212    8

### check if the address or name missing from the db is in FHIER ----
addr_name_in_fhier <-
  vessels_permits_participants_short_u_flat_sp_full__more_addr |>
  filter((is.na(full_name) &
            !is.na(permit_holder_names)) |
           is.na(full_address) &
           !is.na(fhier_address))

glimpse(addr_name_in_fhier)
# 1
# vessels_permits_participants_short_u_flat_sp_full__more_addr |> 
#   filter(P_VESSEL_ID == "FL1431JU")
# FL1431JU    KEY WEST, FL   KODY GRIFFIN MICHAEL ""           KODY GRIFFIN MICHAEL
# 2 FL1431JU    MARATHON, FL   NA                   

### check if the address or name is a "UN" in the db is in FHIER ----
addr_name_in_fhier_un <-
  vessels_permits_participants_short_u_flat_sp_full__more_addr |>
  filter((grepl("\\bUN\\b", full_name) &
            !is.na(permit_holder_names)) |
           grepl("\\bUN\\b", full_address) &
           !is.na(fhier_address))

# dim(addr_name_in_fhier)
# [1] 19 17
# [1] 351   8
# 0 after using clean_names_and_addresses()

# addr_name_in_not_fhier <-
#   fhier_addr__compl_corr |>
#   filter(((!is.na(full_name) | !full_name == "UN") &
#             is.na(permit_holder_names)) |
#            (!is.na(full_address) | !full_address == "UN") &
#            is.na(fhier_address))
# 
# dim(addr_name_in_not_fhier)
# # 39
