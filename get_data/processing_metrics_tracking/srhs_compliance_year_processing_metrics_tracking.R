# check srhs vessels for compliance vs. calendar year
# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# read srhs file comparison, created manually ----
# TODO: automate
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

# prepare processed file names ----
## get all result file names ----
processed_data_dir <-
  file.path(my_paths$inputs,
            r"(processing_logbook_data\Outputs)")

# dir.exists(processed_data_dir)

file_names_all <-
  list.files(processed_data_dir,
             "*\\.rds", full.names = T)

file_names_to_read_ok <-
  grep(
    "calendar|\\.lnk",
    file_names_all,
    invert = T,
    ignore.case = T,
    value = T
  )

## metriks tracking file names ----

file_names_to_read_metrics <-
  grep(
    "SEFHIER_permitted_vessels_nonSRHS_",
    file_names_to_read_ok,
    ignore.case = T,
    value = T
  )

## dnf file names ----
file_names_to_read_dnf <-
  grep(
    "SEFHIER_processed_dnfs_",
    file_names_to_read_ok,
    ignore.case = T,
    value = T
  )

## logbook file names ----
file_names_to_read_logbooks <-
  grep("SEFHIER_processed_logbook",
       file_names_to_read_ok,
       ignore.case = T,
       value = T)

# read processed results ----
years <- c("2022", "2023", "2024")

all_file_names <-
  lst(file_names_to_read_dnf,
    file_names_to_read_logbooks,
    file_names_to_read_metrics)

all_files <-
  all_file_names |>
  imap(\(curr_names, i)
       {
         current_name_res <- map(curr_names, read_rds)

         names(current_name_res) <- years
         return(current_name_res)
  })

# Compare with 2022 result of metrics tracking processing ----

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

# check if vessels in questions are in processed logbooks or dnfs ----
