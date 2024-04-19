# check srhs vessels for compliance vs. calendar year
# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

srhs_22_23_filepath <-
  file.path(my_paths$inputs,
            r"(SRHS_headboat_survey\srhs_compare_years.xlsx)")

# file.exists(srhs_22_23_filepath)

srhs_22_23_raw <-
  read.xlsx(srhs_22_23_filepath) |> clean_headers()
# print_df_names(srhs_22_23_raw)

srhs_22_23_diff <-
  srhs_22_23_raw |>
  filter(diff == TRUE) |>
  select(-diff)

srhs_22_23_diff_as_char <-
  srhs_22_23_diff |>
  mutate(across(everything(), as.character))

# read processed results ----
# SEFHIER_permitted_vessels_nonSRHS_2023

## get all results ----
processed_data_dir <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs)")

# dir.exists(processed_data_dir)

file_names_all <-
  list.files(processed_data_dir,
             "*\\.rds", full.names = T)

# \SEFHIER_permitted_vessels_nonSRHS_2023.rds

file_names_to_read_not_calendar <-
  grep(
    "calendar",
    file_names_all,
    invert = T,
    ignore.case = T,
    value = T
  )


# Compare with 2022 result of metrics tracking processing ----

processed_metrics_permit_info_short_2022 <-
  processed_metrics_permit_info_short_this_year

srhs_22_23_diff_as_char__2022 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2023 %in% processed_metrics_permit_info_short_2022$VESSEL_OFFICIAL_NUMBER)
# 4

# processed_metrics_permit_info_short_this_year |> print_df_names()

# the same for 2023 ----

processed_metrics_permit_info_short_2023 <-
  processed_metrics_permit_info_short_this_year

srhs_22_23_diff_as_char__2023 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2022 %in% processed_metrics_permit_info_short_2023$VESSEL_OFFICIAL_NUMBER)
# 2

# Check if in processed logbooks or dnfs ----
## get all results ----
processed_data_dir <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs)")

# \SEFHIER_permitted_vessels_nonSRHS_2023.rds

# dir.exists(processed_data_dir)

file_names_all <-
  list.files(processed_data_dir,
             "process.*rds", full.names = T)

file_names_to_read_not_calendar <-
  grep(
    "calendar",
    file_names_all,
    invert = T,
    ignore.case = T,
    value = T
  )

file_names_to_read_dnf <-
  grep(
    "dnf",
    file_names_to_read_not_calendar,
    ignore.case = T,
    value = T
  )

file_names_to_read_logbooks_0 <-
  grep("logbook",
       file_names_to_read_not_calendar,
       ignore.case = T,
       value = T)
# incl. "C:/Users/anna.shipunova/Documents/R_files_local/my_inputs/processing_logbook_data\\Outputs/SEFHIER_processed_dnfs_2022.rds"

file_names_to_read_logbooks <-
  grep(
    "dnf",
    file_names_to_read_logbooks_0,
    invert = T,
    ignore.case = T,
    value = T
  )

# check if vessels in questions are in the results
# 2022 ---

one_year <- "2022"
file_name_list <- file_names_to_read_logbooks

read_one_year_rds <-
  function(one_year, file_name_list) {
    file_name_to_read <-
      grep(one_year, file_name_list, value = T)

    this_year_result <-
      read_rds(file_name_to_read)

    return(this_year_result)
  }

### 2022 ----
processed_logbooks_2022 <-
  read_one_year_rds("2022", file_names_to_read_logbooks)

# glimpse(processed_logbooks_2022)
