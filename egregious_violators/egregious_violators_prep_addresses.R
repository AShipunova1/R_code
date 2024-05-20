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
col_names_to_keep <-
  c(
    "official_number",
    "entity_name",
    "primary_email",
    # "is_primary",
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

my_cols_ends <- paste0(col_names_to_keep, 
                  '$', 
                  collapse = '|')

db_participants_address__needed_short1 <-
  db_participants_address__needed |>
  dplyr::select(tidyselect::matches(my_cols_ends)) |>
  dplyr::distinct() |>
  dplyr::arrange(official_number)

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
  dplyr::distinct() |> 
  dplyr::arrange(official_number)

diffdf::diffdf(db_participants_address__needed_short,
               db_participants_address__needed_short1)

nrow(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr)
# 199
dplyr::n_distinct(compl_corr_to_investigation__corr_date__hailing_port__fhier_addr$vessel_official_number)
# 199
# one vessel per row, OK

# have to combine rows
dim(db_participants_address__needed_short)
# 106
# check
dplyr::n_distinct(db_participants_address__needed_short$official_number)
# 71

## combine area and phone numbers ----

# Explanation:

# This code creates a new dataframe `db_participants_address__needed_short__phone0` by modifying the `db_participants_address__needed_short` dataframe. It adds two new columns (`erv_phone` and `erb_phone`) that concatenate existing columns.
# 
# 1. **Starting with the Original Dataframe:**
#    - `db_participants_address__needed_short |>`: Begins with the `db_participants_address__needed_short` dataframe and pipes it into the next function.
# 
# 2. **Adding New Columns:**
#    - `dplyr::mutate(...)`: The `mutate` function from the `dplyr` package is used to add or modify columns in the dataframe.
#      - `erv_phone = paste0(erv_ph_area, erv_ph_number)`: Creates a new column `erv_phone` by concatenating the `erv_ph_area` and `erv_ph_number` columns using `paste0`, which combines strings without any separator.
#      - `erb_phone = paste0(erb_ph_area, erb_ph_number)`: Similarly, creates a new column `erb_phone` by concatenating the `erb_ph_area` and `erb_ph_number` columns.
# 
# The result is a new dataframe `db_participants_address__needed_short__phone0` that contains all the original columns from `db_participants_address__needed_short` plus two new columns (`erv_phone` and `erb_phone`) that contain concatenated phone numbers.

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

# Explanation:

# This code processes the `db_participants_address__needed_short__erv_erb_combined3` dataframe to create a new dataframe named `db_participants_address__needed_short__erv_erb_combined_short` by selecting specific columns and ensuring the rows are distinct.
# 
# 1. **Dataframe Selection and Transformation:**
#    - `db_participants_address__needed_short__erv_erb_combined3 |>`: Starts with the input dataframe `db_participants_address__needed_short__erv_erb_combined3` and pipes it into the subsequent functions.
# 
# 2. **Selecting Specific Columns:**
#    - `dplyr::select(official_number, tidyselect::all_of(tidyselect::starts_with("db_")))`: Uses `dplyr::select` to retain only the `official_number` column and any columns whose names start with "db_".
#      - `tidyselect::all_of(tidyselect::starts_with("db_"))`: Uses the `tidyselect` package to identify all column names that start with "db_". The `all_of` function ensures that the selected columns exist within the dataframe.
# 
# 3. **Ensuring Unique Rows:**
#    - `dplyr::distinct()`: Ensures that the resulting dataframe contains only unique rows, removing any duplicate rows based on the selected columns.
# 
# The result is a new dataframe `db_participants_address__needed_short__erv_erb_combined_short` that contains only the `official_number` column and columns starting with "db_", with all duplicate rows removed.
db_participants_address__needed_short__erv_erb_combined_short <-
  db_participants_address__needed_short__erv_erb_combined3 |>
  dplyr::select(official_number, 
                tidyselect::all_of(tidyselect::starts_with("db_"))) |>
  dplyr::distinct()

dim(db_participants_address__needed_short__erv_erb_combined_short)
# 94 17

dplyr::n_distinct(db_participants_address__needed_short__erv_erb_combined_short$official_number)
# 71

# check
# db_participants_address__needed_short__erv_erb_combined_short |> 
#   dplyr::filter(official_number == "1235397") |>
#   dplyr::glimpse()
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

# check
# db_participants_address__needed_short__erv_erb_combined_short__u |>
#   filter(official_number == "1235397") |>
#   glimpse()

### convert to characters ----
# Explanation:
# 
# This code modifies the dataframe `db_participants_address__needed_short__erv_erb_combined_short__u` by concatenating the elements of list-type columns into a single string separated by semicolons. Here's a detailed explanation:
# 
# 1. **Row-wise Operation:**
#    - `dplyr::rowwise()`: It sets the dataframe to be processed row-wise, meaning each operation will be applied independently to each row.
# 
# 2. **Mutating List-type Columns:**
#    - `dplyr::mutate_if(is.list, ~ paste(unlist(.), collapse = '; '))`: This line applies a mutation to each column of the dataframe that is of list type. 
#      - `is.list`: Checks if a column is of list type.
#      - `paste(unlist(.), collapse = '; ')`: For each list-type column, it converts the list elements into a single string by unlisting them and concatenating them together with a semicolon as the separator.
# 
# 3. **Ungrouping:**
#    - `dplyr::ungroup()`: It removes the grouping previously applied to the dataframe, returning it to its original state.
# 
# This code effectively transforms list-type columns in the dataframe into character vectors, concatenating their elements into a single string with semicolons as separators.

db_participants_address__needed_short__erv_erb_combined_short__u_no_c <-
  db_participants_address__needed_short__erv_erb_combined_short__u |>
  dplyr::rowwise() |>
  dplyr::mutate_if(is.list, ~ paste(unlist(.), collapse = '; ')) |>
  dplyr::ungroup()

# check
# db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
#   filter(official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state_u     <chr> "NY"
# $ db_mailing_city_u      <chr> "ISLANDIA; SOUTH ISLANDIA"

## rename fields ----

# Explanation:
# 
# This code renames the columns in the dataframe `db_participants_address__needed_short__erv_erb_combined_short__u_no_c` by removing the suffix "_u" from their names. Here's a breakdown:
# 
# 1. **Renaming Columns:**
#    - `dplyr::rename_with( ~ stringr::str_replace(.x, pattern = "_u$", replacement = ""))`: It renames the columns of the dataframe using a function provided by `rename_with`.
#      - `~`: It indicates the start of an anonymous function.
#      - `stringr::str_replace(.x, pattern = "_u$", replacement = "")`: For each column name (`x`), it applies the `str_replace` function from the `stringr` package to replace the pattern "_u" at the end of the column name with an empty string, effectively removing it.
# 
# This code removes the "_u" suffix from the column names in the dataframe.

db_participants_address__needed_short__erv_erb_combined_short__u_ok <-
  db_participants_address__needed_short__erv_erb_combined_short__u_no_c |>
  dplyr::rename_with(~ stringr::str_replace(.x, pattern = "_u$", 
                                            replacement = ""))

# Join fhier and Oracle db addresses ----
compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr <-
  compl_corr_to_investigation__corr_date__hailing_port__fhier_addr |>
  dplyr::left_join(
    db_participants_address__needed_short__erv_erb_combined_short__u_ok,
    dplyr::join_by(vessel_official_number == official_number)
  )

# check
# compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr |>
#   filter(vessel_official_number == "1235397") |>
#   glimpse()
# $ db_mailing_state       <chr> "NY"
# $ db_mailing_city        <chr> "ISLANDIA; SOUTH ISLANDIA"

cat("Result: ",
    "compl_corr_to_investigation__corr_date__hailing_port__fhier_addr__db_addr",
    sep = "\n")

