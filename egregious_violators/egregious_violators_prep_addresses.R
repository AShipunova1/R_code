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
  auxfunctions::clean_names_and_addresses() |> 
  dplyr::distinct()

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
#   select(
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
  dplyr::filter(physical_address_1 %in% is_empty) |> 
  dplyr::select(vessel_official_number) |> 
  dplyr::distinct()

dplyr::n_distinct(no_addr_vsl_ids$vessel_official_number)
# 109
# 71

# From Oracle db ----
db_participants_address__needed <-
  db_participants_address |>
  dplyr::filter(official_number %in% no_addr_vsl_ids$vessel_official_number) |>
  dplyr::distinct()

dim(db_participants_address__needed)
# [1] 139  37

dplyr::n_distinct(db_participants_address__needed$official_number)
# 71

## keep fewer columns ----
db_participants_address__needed_short <-
  db_participants_address__needed |>
  dplyr::select(
    official_number,
    all_of(ends_with("entity_name")),
    all_of(ends_with("primary_email")),
    # all_of(ends_with("is_primary")),
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
  dplyr::distinct()

nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
# 199
dplyr::n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr$vessel_official_number)
# 199
# one vessel per row, OK

# have to combine rows
dim(db_participants_address__needed_short)
# 106
dplyr::n_distinct(db_participants_address__needed_short$official_number)
# 71

## combine area and phone numbers ----
db_participants_address__needed_short__phone0 <- 
  db_participants_address__needed_short |> 
  dplyr::mutate(erv_phone = paste0(erv_ph_area, erv_ph_number),
         erb_phone = paste0(erb_ph_area, erb_ph_number))

## make erv and erb combinations ----
col_part_names <-
  c(
    "entity_name",
    "primary_email",
    # "ph_is_primary",
    # "ph_area",
    # "ph_number",
    "physical_city",
    "physical_county",
    "physical_state",
    "physical_zip_code",
    "mailing_address1",
    "mailing_address2",
    "mailing_city",
    "mailing_county",
    # "mailing_country",
    "mailing_state",
    "mailing_zip_code"
  )

# Explanation:

# 1. **Mapping Over Column Parts:**
#    - `col_part_names |> purrr::map(\(curr_col_part) { ... })`: It iterates over each element in `col_part_names` using the `map` function from the purrr package. For each column part (`curr_col_part`), it executes the code inside the curly braces `{ ... }`.
# 
# 2. **Generating New Column Names:**
#    - `new_col_name <- stringr::str_glue("db_{curr_col_part}")`: It creates a new column name by combining the prefix "db_" with the current column part (`curr_col_part`) using `str_glue` from the stringr package.
# 
# 3. **Grouping and Mutating Data:**
#    - `db_participants_address__needed_short__phone0 |> dplyr::group_by(official_number) |> ...`: It groups the dataframe `db_participants_address__needed_short__phone0` by the column `official_number` using `group_by` from dplyr. Then, it proceeds with further data manipulation operations.
# 
# 4. **Applying Purrr::pmap Function:**
#    - `purrr::pmap(dplyr::across(dplyr::ends_with(curr_col_part)), ...)`: It applies the `pmap` function from the purrr package to iterate over columns that end with the current column part (`curr_col_part`). Within the `pmap` call, a custom function (`auxfunctions::list_sort_uniq`) is applied to each corresponding set of columns.
# 
# 5. **Ungrouping and Selecting Columns:**
#    - `... |> dplyr::ungroup() |> dplyr::select(-official_number)`: After the mutation step, it ungroups the dataframe and removes the `official_number` column using `ungroup()` and `select()` functions from dplyr, respectively.
# 
# 6. **Binding Columns Together:**
#    - `dplyr::bind_cols(db_participants_address__needed_short__phone0, .)`: Finally, it binds the original dataframe `db_participants_address__needed_short__phone0` with the transformed columns obtained from the mapping operation using `bind_cols` from dplyr.
# 
# This code dynamically generates new columns in the dataframe based on the provided column parts, applies a custom function to each set of corresponding columns, and then binds the resulting columns back to the original dataframe.

tictoc::tic("map all pairs")
db_participants_address__needed_short__erv_erb_combined3 <-
  col_part_names |>
  purrr::map(\(curr_col_part)  {
    new_col_name <- stringr::str_glue("db_{curr_col_part}")
    # cat(new_col_name, sep = "\n")
    
    db_participants_address__needed_short__phone0 |>
      dplyr::group_by(official_number) |>
      dplyr::mutate(!!new_col_name := 
                      purrr::pmap(dplyr::across(dplyr::ends_with(curr_col_part)),
                                    ~ auxfunctions::list_sort_uniq(.)),
             .keep = "none" ) |>
      dplyr::ungroup() |>
      dplyr::select(-official_number)
    
  }) %>%
  dplyr::bind_cols(db_participants_address__needed_short__phone0, .)
tictoc::toc()
# map all pairs: 14.31 sec elapsed

### shorten ----
db_participants_address__needed_short__erv_erb_combined_short <-
  db_participants_address__needed_short__erv_erb_combined3 |>
  dplyr::select(official_number, 
                tidyselect::all_of(tidyselect::starts_with("db_"))) |>
  dplyr::distinct()

dim(db_participants_address__needed_short__erv_erb_combined_short)
# 94 17

dplyr::n_distinct(db_participants_address__needed_short__erv_erb_combined_short$official_number)
# 71

db_participants_address__needed_short__erv_erb_combined_short |> 
  dplyr::filter(official_number == "1235397") |>
  dplyr::glimpse()
# $ db_entity_name       <list> ["DC SERVICE AND MAINTENANCE"], ["DAVID A RUBINO"]
# $ db_physical_city     <list> ["SOUTH ISLANDIA"], ["ISLANDIA"]

## combine similar fields ----

# Explanations:
# 1. Iterate over each participant column using 'col_part_names'.
#    - 'map' applies the provided function to each element of the list.
# 2. Define the old and new column names based on the current participant column.
#    - 'str_glue' is used for string interpolation to create column names.
# 3. Group the DataFrame by 'official_number' using 'group_by'.
# 4. For each group, create a new column with unique sorted values for the current participant.
#    - 'list_sort_uniq' ensures unique values and sorts them.
# 5. Ungroup the DataFrame and remove the 'official_number' column.
#    - 'ungroup' removes grouping structure.
#    - 'select' is used to exclude the 'official_number' column and keep only the new column.
# 6. Bind the resulting columns to 'db_participants_address__needed_short__erv_erb_combined_short'.
#    - 'bind_cols' combines columns horizontally.
# 7. Select only the 'official_number' and columns ending with '_u'.
# 8. Keep only distinct rows in the final DataFrame using 'distinct'.
# 9. The resulting DataFrame is stored in 'db_participants_address__needed_short__erv_erb_combined_short__u'.

db_participants_address__needed_short__erv_erb_combined_short__u_temp <-
  col_part_names |>
  purrr::map(\(curr_col_part)  {
    # browser()
    old_col_name <- stringr::str_glue("db_{curr_col_part}")
    new_col_name <- stringr::str_glue("db_{curr_col_part}_u")
    cat(new_col_name, sep = "\n")
    
    db_participants_address__needed_short__erv_erb_combined_short |>
      dplyr::group_by(official_number) |>
      dplyr::mutate(!!new_col_name := list(paste(sort(unique(stringr::str_trim(purrr::list_flatten(!!sym(old_col_name))))))),
             .keep = "none" ) |>
      dplyr::ungroup() |>
      dplyr::select(-official_number)
  })

# glimpse(db_participants_address__needed_short__erv_erb_combined_short)

# Explanation:
# 
# This code creates a new dataframe by combining columns with suffix "_u" from two existing dataframes (`db_participants_address__needed_short__erv_erb_combined_short` and `db_participants_address__needed_short__erv_erb_combined_short__u_temp`). Here's a detailed breakdown:
# 
# 1. **Binding Columns Together:**
#    - `dplyr::bind_cols(...)`: It binds columns from two dataframes together. The columns from `db_participants_address__needed_short__erv_erb_combined_short` and `db_participants_address__needed_short__erv_erb_combined_short__u_temp` are combined horizontally.
# 
# 2. **Selecting Columns:**
#    - `dplyr::select(official_number, dplyr::all_of(dplyr::ends_with("_u")))`: After binding columns, it selects the `official_number` column along with all columns that end with "_u" using `select` from dplyr. 
#    
# 3. **Removing Duplicate Rows:**
#    - `dplyr::distinct()`: It removes duplicate rows from the dataframe to ensure each row is unique.
# 
# This code essentially creates a new dataframe containing selected columns from two existing dataframes and ensures that there are no duplicate rows in the resulting dataframe.

db_participants_address__needed_short__erv_erb_combined_short__u <-
  dplyr::bind_cols(
    db_participants_address__needed_short__erv_erb_combined_short,
    db_participants_address__needed_short__erv_erb_combined_short__u_temp
  ) |>
  dplyr::select(official_number, dplyr::all_of(dplyr::ends_with("_u"))) |> 
  dplyr::distinct()

# db_participants_address__needed_short__erv_erb_combined_short__u |>
#   filter(official_number == "1235397") |>
#   glimpse()

### convert to characters ----
db_participants_address__needed_short__erv_erb_combined_short__u_no_c <-
  db_participants_address__needed_short__erv_erb_combined_short__u |>
  dplyr::rowwise() |>
  dplyr::mutate_if(is.list, ~ paste(unlist(.), collapse = '; ')) |>
  dplyr::ungroup()

# db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
#   filter(official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state_u     <chr> "NY"
# $ db_mailing_zip_code_u  <chr> "..; ..-.."

## rename fields ----
db_participants_address__needed_short__erv_erb_combined_short__u_ok <-
  db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
  dplyr::rename_with( ~ stringr::str_replace(.x, pattern = "_u$", replacement = ""))

# Join fhier and Oracle db addresses ----
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  left_join(
    db_participants_address__needed_short__erv_erb_combined_short__u_ok,
    join_by(vessel_official_number == official_number)
  )

# compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
#   filter(vessel_official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state       <chr> "NY"
# $ db_mailing_zip_code    <chr> "11749; 11749-5010"

cat("Result: ",
    "compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr",
    sep = "\n")

