source("~/R_code_github/useful_functions_module.r")

# get csvs ----

base_path <- getwd()

correctd_path <- 
  file.path(base_path,
            r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation__2023-01-24_to_2023-08-01_c.csv)")
file.exists(correctd_path)
corrected_csv <-
  readr::read_csv(correctd_path,
                  name_repair = fix_names,
                  col_types = cols(.default = 'c'),
                  skip = 1
                  ) |> 
  distinct()

# "R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv"

aug_9_res_path <- 
  file.path(base_path,
            r"(../../R_files_local\my_outputs\egregious_violators\egregious_violators_for_investigation_from_2023-02-07_to_2023-08-15_addr.csv)")

file.exists(aug_9_res_path)
# T

aug_9_csv <-
  readr::read_csv(aug_9_res_path,
                  name_repair = fix_names,
                  col_types = cols(.default = 'c'))

# FHIER info from Reports \ SERO Permits
""


fhier_info_path <- 
  file.path(base_path,
            r"(../../\R_files_local\my_inputs\from_Fhier\SERO Permits.csv)")

file.exists(fhier_info_path)
# T

fhier_info <-
  readr::read_csv(fhier_info_path,
                  name_repair = fix_names,
                  col_types = cols(.default = 'c'))

# dim(fhier_info)
# [1] 138557     19

# shorten ----
fields_to_use <-
  c(
    "vessel_official_number",
    "name",
    "contactrecipientname",
    "contactphone_number",
    "contactemailaddress",
    "sero_home_port",
    "state",
    "division",
    "full_name",
    "full_address"
  )


corrected_csv1 <-
  corrected_csv |>
  select(all_of(fields_to_use)) |> 
  distinct()

short_dfs <-
  list(aug_9_csv,
       corrected_csv) |>
  map( ~ .x |>
         select(any_of(fields_to_use)) |>
         distinct())

map(short_dfs,
       dim)
# [1] 97 10
# 
# [[2]]
# [1] 117   8

# compare files ----

csvs_joined <-
  full_join(
    short_dfs[[1]],
    short_dfs[[2]],
    join_by(vessel_official_number,
            name),
    # Override the default suffixes, c(".x", ".y") in not merged cols
    suffix = c(".aug", ".cor")
  )

csvs_joined_diff <-
  csvs_joined |>
  filter(
    !sero_home_port.aug == sero_home_port.cor |
      !full_name.aug == full_name.cor |
      !full_address.aug == full_address.cor
  )

dim(csvs_joined_diff)
# [1] 57 16

View(csvs_joined_diff)

# compare fhier_info with corrected
# print_df_names(fhier_info)
corrected_n_fhier <-
  left_join(short_dfs[[2]],
            fhier_info,
            join_by(vessel_official_number == vessel_id))

dim(corrected_n_fhier)  
# [1] 1659   26
