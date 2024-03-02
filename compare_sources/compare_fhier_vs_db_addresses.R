# compare erv and erb addresses in db ----

# print_df_names(db_participants_asddress)
# ser_id, official_number, uscg_documentation, state_registration, vchar_hull_id_number, vchar_vessel_name, is_primary, is_mail_rec, erv_ser_id, erv_entity_type, erv_entity_name, erv_ph_is_primary, erv_ph_area, erv_ph_number, erv_primary_email, erv_physical_address1, erv_physical_address2, erv_physical_city, erv_physical_county, erv_physical_state, erv_physical_zip_code, erv_mailing_address1, erv_mailing_address2, erv_mailing_city, erv_mailing_county, erv_mailing_country, erv_mailing_state, erv_mailing_zip_code, association_start_dt, relationship, erb_ser_id, erb_entity_type, erb_entity_name, erb_ph_is_primary, erb_ph_area, erb_ph_number, erb_primary_email

# official_number
erv_entity_type	
erv_entity_name	
erv_ph_area	
erv_ph_number	
erv_primary_email	
erv_physical_address1	
erv_physical_address2	
erv_physical_city	
erv_physical_county	
erv_physical_state	
erv_physical_zip_code

#  erv_entity_type	 erb_entity_type
#  erv_entity_name	 erb_entity_name
#  erv_ph_area	 erb_ph_area
#  erv_ph_number	 erb_ph_number
#  erv_primary_email	 erb_primary_email
#  erv_physical_address1	 erv_mailing_address1
#  erv_physical_address2	 erv_mailing_address2
#  erv_physical_city	 erv_mailing_city
#  erv_physical_county	 erv_mailing_county
#  erv_physical_state	 erv_mailing_state
#  erv_physical_zip_code	 erv_mailing_zip_code


# compare addresses from fhier and db ----
# get_data from from egregious violators
names(fhier_addr_short) |> 
  cat(sep = '", "')

col_to <-
  c(
    "vessel_official_number",
    "permit_holder_names",
    "physical_address_1",
    "physical_address_2",
    "physical_city",
    "physical_county",
    "physical_state",
    "physical_zip_code",
    "primary_email",
    "phone_number"
  )

# print_df_names(db_participants_asddress)
col_from <- c(
  "official_number",
  "erv_entity_name",
  "erv_physical_address1",
  "erv_physical_address2",
  "erv_physical_city",
  "erv_physical_county",
  "erv_physical_state",
  "erv_physical_zip_code",
  "erv_primary_email",
  "erv_full_ph_number"
)

dim(db_participants_asddress)
# [1] 55113    37

db_participants_asddress_short_0 <-
  db_participants_asddress |>
  select(any_of(col_from),
         "erv_ph_area",
         "erv_ph_number") |>
  distinct()

dim(db_participants_asddress_short_0)
# [1] 30287    11

db_participants_asddress_short_1 <-
  db_participants_asddress_short_0 |>
  mutate(erv_full_ph_number = paste0(erv_ph_area,
                                     erv_ph_number)) |>
  select(-c(erv_ph_area,
            erv_ph_number)) |> 
  distinct()

db_participants_asddress_short_2 <-
  db_participants_asddress_short_1 |>
  rename_with( ~ col_to, .cols = all_of(col_from))


db_participants_asddress_short <-
  db_participants_asddress_short_2 |>
  filter(vessel_official_number %in% fhier_addr_short$vessel_official_number) |> 
  mutate(across(where(is.character), str_squish)) |> 
  arrange(vessel_official_number)

# View(db_participants_asddress_short)
# View(fhier_addr_short)

fhier_addr_short_arr <- 
  fhier_addr_short |>
  mutate(across(where(is.character), str_squish)) |> 
  arrange(vessel_official_number)


# diffdf::diffdf(fhier_addr_short_arr, db_participants_asddress_short)
library(arsenal)

address_compare <-
  summary(
    comparedf(fhier_addr_short_arr,
              db_participants_asddress_short,
              by = "vessel_official_number"),
    max.print.diffs.per.var = NA,
    max.print.diffs	= NA,
    tol.char = "both"
  )

# all.equal(address_compare,
#                address_compare2)

aa <-
  address_compare$diffs.table |>
  filter(vessel_official_number == "1020822") |>
  select(values.x, values.y)

setdiff(aa[[1]], aa[[2]])
# 0
setdiff(aa[[2]], aa[[1]])
# [1] "JUDY LYNN HELMEY"
# 
# > setdiff(aa[[2]], aa[[1]])
# [[1]]
# [1] "JUDY LYNN HELMEY "

# View(address_compare)
# write_csv(address_compare$diffs.table,
# r"(compare\address_compare__fhier_vs_db.csv)")
