# get the long name from the second column
one_name <- colnames(prev_result[2])

# get the vessel ids for these with a "yes" in it
vessels_ids_from_prev_yes <-
  prev_result |> 
  filter(tolower(!!sym(one_name)) == "yes") |> 
  select(vessel_official_number)

# get the vessel ids for these with a "no" in it
vessels_ids_from_prev_no <-
  prev_result |> 
  filter(tolower(!!sym(one_name)) == "no") |> 
  select(vessel_official_number)

dim(vessels_ids_from_prev_yes)

### mark these vessels ----

compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
  filter(vessel_official_number %in%
           vessels_ids_from_prev_no$vessel_official_number) |>
  View()

vessels_ids_from_prev_yes
