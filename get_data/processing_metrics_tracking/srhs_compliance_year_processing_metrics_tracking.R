# check srhs vessels for compliance vs. calendar year
# run after processing_metrics_tracking.R
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

# Compare with 2022 result of metrics tracking processing

processed_metrics_permit_info_short_2022 <-
  processed_metrics_permit_info_short_this_year

srhs_22_23_diff_as_char__2022 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2023 %in% processed_metrics_permit_info_short_2022$VESSEL_OFFICIAL_NUMBER)
# 4

# processed_metrics_permit_info_short_this_year |> print_df_names()

# the same for 2023

processed_metrics_permit_info_short_2023 <-
  processed_metrics_permit_info_short_this_year

srhs_22_23_diff_as_char__2023 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2022 %in% processed_metrics_permit_info_short_2023$VESSEL_OFFICIAL_NUMBER)
# 2

# Check if in processed logbooks or dnfs
