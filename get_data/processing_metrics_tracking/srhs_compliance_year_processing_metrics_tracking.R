# check srhs vessels for compliance vs. calendar year

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

srhs_22_23_diff
