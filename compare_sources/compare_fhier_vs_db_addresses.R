# ----set up----

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(arsenal)

my_paths <- set_work_dir()

current_project_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

current_project_name <- current_project_basename

all_inputs <- my_paths$inputs

my_year <- "2022"
my_beginning1 <- str_glue("{my_year}-01-01")
my_end1 <- str_glue("{my_year}-12-31")

data_file_date <- today()

fhier_fields <-
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
db_fields <- c(
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

# get_data ----
# Physical Address List from FHIER ----
# REPORTS / For-hire Primary Physical Address List

fhier_addresses_path <-
  file.path(
    my_paths$inputs,
    r"(from PIMS\address\For-hire Primary Physical Address List_02_21_2024.csv)"
  )

# file.exists(fhier_addresses_path)

fhier_addresses <-
  read_csv(fhier_addresses_path,
           # read all as characters
           col_types = cols(.default = 'c'),
           name_repair = fix_names)

# View(fhier_addresses)

# get info from the db ----
# get addresses ----
db_participants_asddress_query <-
  "select * from
SRH.MV_SERO_VESSEL_ENTITY@Secapxdv_Dblk.sfsc.noaa.gov
"

db_participants_asddress_file_path <-
  file.path(my_paths$inputs,
            r"(from_db\safis\mv_sero_vessel_entity.rds)")

# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- connect_to_secpr())
}

db_participants_asddress_fun <-
  function(db_participants_asddress) {
    # browser()
    return(dbGetQuery(con,
                      db_participants_asddress))
  }

db_participants_asddress <-
  read_rds_or_run(
    db_participants_asddress_file_path,
    db_participants_asddress_query,
    db_participants_asddress_fun
    # force_from_db = "yes"
  ) |>
  remove_empty_cols() |>
  clean_headers()

dim(db_participants_asddress)
# [1] 55113    41
# [1] 55113    37 remove_empty_cols
# [1] 55145    37

# 2024-03-04 run for mv_sero_vessel_entity.rds: 40.65 sec elapsed

# aux functions ----
subdf_prep <-
  function(my_df,
           field_names_from,
           field_names_to,
           sort_by = "vessel_official_number") {
    my_df_renamed <-
      my_df |>
      rename_with( ~ field_names_to,
                   .cols = all_of(field_names_from))

    my_df_renamed_cleaned_sorted <-
      my_df_renamed |>
      mutate(across(where(is.character), str_squish)) |>
      arrange(sort_by)

    return(my_df_renamed_cleaned_sorted)
  }

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

## actual compare fhier and erv ----

# diffdf::diffdf(fhier_addr_short_arr, db_participants_asddress_short)

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

res_fhier__db_erv <-
  address_compare$diffs.table |>
  filter(vessel_official_number == "1020822") |>
  select(values.x, values.y)

# setdiff(aa[[1]], aa[[2]])
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

# compare erv and erb addresses in db ----
## prepare sub dfs to have the same col names ----
# print_df_names(db_participants_asddress)
# ser_id, official_number, uscg_documentation, state_registration, vchar_hull_id_number, vchar_vessel_name, is_primary, is_mail_rec, erv_ser_id, erv_entity_type, erv_entity_name, erv_ph_is_primary, erv_ph_area, erv_ph_number, erv_primary_email, erv_physical_address1, erv_physical_address2, erv_physical_city, erv_physical_county, erv_physical_state, erv_physical_zip_code, erv_mailing_address1, erv_mailing_address2, erv_mailing_city, erv_mailing_county, erv_mailing_country, erv_mailing_state, erv_mailing_zip_code, association_start_dt, relationship, erb_ser_id, erb_entity_type, erb_entity_name, erb_ph_is_primary, erb_ph_area, erb_ph_number, erb_primary_email

# official_number
erv_fields <-
  c(
    "official_number",
    "erv_entity_type",
    "erv_entity_name",
    "erv_ph_area",
    "erv_ph_number",
    "erv_primary_email",
    "erv_physical_address1",
    "erv_physical_address2",
    "erv_physical_city",
    "erv_physical_county",
    "erv_physical_state",
    "erv_physical_zip_code"
  )

second_field_set <-
  c(
    "official_number",
    "erb_entity_type",
    "erb_entity_name",
    "erb_ph_area",
    "erb_ph_number",
    "erb_primary_email",
    "erv_mailing_address1",
    "erv_mailing_address2",
    "erv_mailing_city",
    "erv_mailing_county",
    "erv_mailing_state",
    "erv_mailing_zip_code"
  )

db_participants_asddress_erv <-
  db_participants_asddress |>
  select(any_of(erv_fields)) |>
  distinct()

db_participants_asddress_2 <-
  db_participants_asddress |>
  select(any_of(second_field_set)) |>
  distinct()

dim(db_participants_asddress_erv)
# [1] 30287    12
dim(db_participants_asddress_2)
# [1] 35462    12

db_participants_asddress_erv_cleaned <- 
  subdf_prep(db_participants_asddress_erv,
             erv_fields,
             erv_fields,
             sort_by = "official_number")

db_participants_asddress_2_cleaned <- 
  subdf_prep(db_participants_asddress_2,
             second_field_set,
             erv_fields,
             sort_by = "official_number")

  # db_participants_asddress_2 |>
  # rename_with(~ erv_fields,
  #             .cols = all_of(second_field_set))

# all.equal(db_participants_asddress_2_renamed,
#           db_participants_asddress_2_renamed__1)
# F (trimmed)

identical(names(db_participants_asddress_erv),
          names(db_participants_asddress_2_renamed))
# T

## compare w arsenal ----

address_compare__in_db <-
  summary(
    comparedf(
      db_participants_asddress_erv_cleaned,
      db_participants_asddress_2_cleaned,
      by = "official_number"
    ),
    max.print.diffs.per.var = NA,
    max.print.diffs	= NA,
    tol.char = "both"
  )

address_compare__in_db$diffs.table |>
  filter(!grepl("entity_type", var.x)) |>
  write_csv(r"(compare\address_compare__db_erv_vs_db_other.csv)")


# entity_type - buisness vs. individual