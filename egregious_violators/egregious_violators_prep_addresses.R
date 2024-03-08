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
# 0

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

# From Oracle db ----
db_participants_address__needed <-
  db_participants_address |>
  filter(official_number %in% no_addr_vsl_ids$vessel_official_number) |> 
  distinct()

dim(db_participants_address__needed)
# [1] 139  37

n_distinct(db_participants_address__needed$official_number)
# 71

## keep fewer columns ----
col_part_names <-
  c(
    "entity_name",
    "primary_email",
    "ph_is_primary",
    "ph_area",
    "ph_number",
    "entity_name",
    "physical_city",
    "physical_county",
    "physical_state",
    "physical_zip_code",
    "mailing_address1",
    "mailing_address2",
    "mailing_city",
    "mailing_county",
    "mailing_country",
    "mailing_state",
    "mailing_zip_code"
  )

db_participants_address__needed_short <-
  db_participants_address__needed |>
  select(
    official_number,
    all_of(ends_with("entity_name")),
    all_of(ends_with("primary_email")),
    all_of(ends_with("is_primary")),
    all_of(ends_with("ph_area")),
    all_of(ends_with("ph_number")),
    all_of(ends_with("entity_name")),
    all_of(ends_with("physical_city")),
    all_of(ends_with("physical_county")),
    all_of(ends_with("physical_state")),
    all_of(ends_with("physical_zip_code")),
    all_of(ends_with("mailing_address1")),
    all_of(ends_with("mailing_address2")),
    all_of(ends_with("mailing_city")),
    all_of(ends_with("mailing_county")),
    all_of(ends_with("mailing_country")),
    all_of(ends_with("mailing_state")),
    all_of(ends_with("mailing_zip_code"))
  ) |>
  distinct()

# ser_id, official_number, uscg_documentation, state_registration, vchar_hull_id_number, vchar_vessel_name, is_primary, is_mail_rec, erv_ser_id, erv_entity_type, erv_entity_name, erv_ph_is_primary, erv_ph_area, erv_ph_number, erv_primary_email, erv_physical_address1, erv_physical_address2, erv_physical_city, erv_physical_county, erv_physical_state, erv_physical_zip_code, erv_mailing_address1, erv_mailing_address2, erv_mailing_city, erv_mailing_county, erv_mailing_country, erv_mailing_state, erv_mailing_zip_code, association_start_dt, relationship, erb_ser_id, erb_entity_type, erb_entity_name, erb_ph_is_primary, erb_ph_area, erb_ph_number, erb_primary_email

nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
# 199
n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr$vessel_official_number)
# 199
# one vessel per row

# have to combine rows
dim(db_participants_address__needed_short)
# 106
n_distinct(db_participants_address__needed_short$official_number)
# 71

# compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |> 
#   print_df_names()

# db_participants_address__needed_short |> 
  # filter(!erv_ph_number == erb_ph_number)
# 0
# erv_ph_area	erb_ph_area
# erv_ph_number	erb_ph_number

db_participants_address__needed_short__phone0 <- 
  db_participants_address__needed_short |> 
  mutate(erv_phone = paste0(erv_ph_area, erv_ph_number),
         erb_phone = paste0(erb_ph_area, erb_ph_number))

# db_participants_address__needed_short__phone1 <-
#   db_participants_address__needed_short__phone0 |>
#   group_by(official_number) |>
#   summarise(across(ends_with("_phone"), ~list(sort(unique(.x))))) |> 
#   ungroup() |> 
#   as.data.frame()

## make erv and erb combinations ----

list_sort_uniq <- function(my_lists) {
  # browser()
  list(sort(unique(str_trim(my_lists)))) |> 
    flatten() %>%
    return()
}

# print_df_names(db_participants_address__needed_short__phone0)

db_participants_address__needed_short__phone2 <-
  db_participants_address__needed_short__phone0 |>
  group_by(official_number) |>
  mutate(db_phone = pmap(across(ends_with("_phone")),
                         ~ list_sort_uniq(.))) |>
  ungroup()

# View(db_participants_address__needed_short__phone2)
# c("3364239470", 
#   "3365282062", 
#   "3367239470")

  # mutate(db_phone = paste(sort(unique(
  #   erv_phone, erb_phone
  # )),
  # collapse = ", "))
#   filter(grepl("&", active_or_expired)) |>
  

# View(db_participants_address__needed_short__phone1)


# rr2 <- 
  # col_part_names |>
#      map_df(~ df %>%
#            transmute(!! str_c(.x, '3') :=  !! rlang::sym(str_c(.x, '1'))  + 
#          !! rlang::sym(str_c(.x, 2)))) %>%
#      bind_cols(df, .)
# list_cbind

col_pre %>%
     map_dfc(~ df %>%
           transmute(!! str_c(.x, '3') :=  !! rlang::sym(str_c(.x, '1'))  + 
         !! rlang::sym(str_c(.x, 2)))) %>%
     bind_cols(df, .)


df <- db_participants_address__needed_short__phone0
res3 <-   
col_part_names |>
     map_dfc(~ df %>%
      # group_by(official_number) |>
      mutate(pmap(across(ends_with(.x)),
                                                    ~ list_sort_uniq(.)),
             .keep = "none",
             )) |>
      # ungroup()) |>
         #                  transmute(!! str_c(.x, '3') :=  !! rlang::sym(str_c(.x, '1'))  + 
         # !! rlang::sym(str_c(.x, 2)))) %>%
     bind_cols(df, .)

new_df <- db_participants_address__needed_short__phone0 |>
  select(official_number)

tic("map all pairs")
rr2 <- 
  col_part_names |>
  map_df(\(curr_col_part) {
    # browser()
    
    new_col_name <- str_glue("db_{curr_col_part}")
    cat(new_col_name, sep = "\n")
    res <-
      db_participants_address__needed_short__phone0 |>
      group_by(official_number) |>
      mutate(!!new_col_name := pmap(across(ends_with(curr_col_part)),
                                                    ~ list_sort_uniq(.)),
             .keep = "none",
             ) |>
      ungroup() 
    new_df <- left_join(new_df, res)
        
    # return(new_df)
    
  }) 
toc()
# map all pairs: 19.57 sec elapsed
# map all pairs: 24.42 sec elapsed

View(rr2)
  # group_by(official_number) |>
  # mutate(db_phone = pmap(across(ends_with("_phone")),
  #                        ~ list_sort_uniq(.))) |>
  # ungroup()
