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
# 0
# We want a result of 0, if the value is non-zero, investigate.


## vessels with no addresses ----

# print_df_names(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
no_addr_vsl_ids <- 
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |> 
  filter(physical_address_1 %in% is_empty) |> 
  select(vessel_official_number) |> 
  distinct()

n_distinct(no_addr_vsl_ids$vessel_official_number)
# 109
# 71
# [1] "656222"   "FL0450JN"

# From PIMS ----
