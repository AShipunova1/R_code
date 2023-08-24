source("~/R_code_github/useful_functions_module.r")

# get csvs ----

base_path <- getwd()

correctd_path <- 
  file.path(base_path,
            r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation__2023-01-24_to_2023-08-01_c.csv)")
file.exists(correctd_path)
corrected_csv <-
  readr::read_csv(correctd_path,
                  col_types = cols(.default = 'c'),
                  skip = 1
                  )

# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv"

aug_9_res_path <- 
  file.path(base_path,
            r"(../../R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv)")

file.exists(aug_9_res_path)
# T

aug_9_csv <- readr::read_csv(aug_9_res_path,
                             col_types = cols(.default = 'c'))

# find and rm empty cols ----
empty_cols_corrected_csv <-
  corrected_csv |>
  map_df(function(x) {
    if (length(unique(x)) == 1) {
      return(unique(x))
    }
  }) |> 
  names()
empty_cols_corrected_csv
# 8
# NA

corrected_csv1 <-
  corrected_csv |>
  select(-all_of(empty_cols_corrected_csv)) |> 
  distinct()

dim(corrected_csv1)
# 97 19
dim(aug_9_csv)
# [1] 111  17

# compare files ----
print_df_names(corrected_csv1)

# data_overview(aug_9_csv)

join_files <-
  full_join(
    aug_9_csv,
    corrected_csv1,
    join_by(vessel_official_number),
    # Override the default suffixes, c(".x", ".y") in not merged cols
    suffix = c(".aug", ".cor")
  )
