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

# rename
names(all_files) <-
  gsub("file_names_to_read_",
       "processed_results_",
       names(all_files))

# Compare with 2022 result of metrics tracking processing ----

srhs_22_23_diff_as_char__2022 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2023 %in% all_files$processed_results_metrics$`2022`$VESSEL_OFFICIAL_NUMBER)
# 4

# the same for 2023 ----

srhs_22_23_diff_as_char__2023 <-
  srhs_22_23_diff_as_char |>
  filter(uscg_2022 %in% all_files$processed_results_metrics$`2023`$VESSEL_OFFICIAL_NUMBER)
# 2

# check if vessels in questions are in processed logbooks or dnfs ----

## srhs 2022 in 2023 ----
srhs_2022_in_2023 <-
  all_files |>
  map(\(one_df) {
    one_df |>
      map(\(one_year) {
        one_year |>
          filter(VESSEL_OFFICIAL_NUMBER %in%
                   srhs_22_23_diff_as_char__2023$uscg_2022)
      })
  })
# $processed_results_logbooks$`2023`
# [1] 62

## srhs 2023 in 2022 ----
srhs_2023_in_2022 <-
  all_files |>
  map(\(one_df) {
    one_df |>
      map(\(one_year) {
        one_year |>
          filter(VESSEL_OFFICIAL_NUMBER %in%
                   srhs_22_23_diff_as_char__2022$uscg_2023)
      })
  })
# $processed_results_dnf$`2022`
# [1] 372
# $processed_results_logbooks$`2022`
# [1] 388
#
# $processed_results_logbooks$`2023`
# [1] 5
#

# check each ----
# View(srhs_2022_in_2023)
srhs_2022_in_2023$processed_results_logbooks$`2023`$VESSEL_OFFICIAL_NUMBER |> unique()
# [1] "1197401"

srhs_2022_in_2023$processed_results_logbooks$`2023` |>
  select(VESSEL_OFFICIAL_NUMBER, TRIP_START_DATE, TRIP_END_DATE) |>
  distinct() |>
  mutate(TRIP_START_year = year(TRIP_START_DATE)) |>
  select(TRIP_START_year) |>
  distinct()

# 2023 only, ok

# TODO: the same for 2022

srhs_2023_in_2022$processed_results_logbooks$`2022`$VESSEL_OFFICIAL_NUMBER |> unique()
# [1] "1038780"

srhs_2023_in_2022$processed_results_logbooks$`2022` |>
  select(VESSEL_OFFICIAL_NUMBER, TRIP_START_DATE, TRIP_END_DATE) |>
  distinct() |>
  mutate(TRIP_START_year = year(TRIP_START_DATE)) |>
  select(TRIP_START_year) |>
  distinct()
# 2022

srhs_2023_in_2022$processed_results_logbooks$`2022` |>
  select(VESSEL_OFFICIAL_NUMBER, TRIP_START_DATE, TRIP_END_DATE) |>
  distinct() |>
  mutate(TRIP_END_year = year(TRIP_END_DATE)) |>
  select(TRIP_END_year) |>
  distinct()
# 2022

# all trips in 2022, ok

# srhs_2023_in_2022$processed_results_logbooks$`2022` |>
#   filter(VESSEL_OFFICIAL_NUMBER  == "1038780") |>
#   View()

srhs_22_23_raw |>
  filter(uscg_2023 == "1038780") |>
  glimpse()
# ok

## dnfs 23 in 22 ----

srhs_2023_in_2022_processed_results_dnf_2022 <-
  srhs_2023_in_2022$processed_results_dnf$`2022` |>
  select(VESSEL_OFFICIAL_NUMBER, TRIP_DATE) |>
  distinct() |>
  mutate(TRIP_year = year(TRIP_DATE))

srhs_2023_in_2022_processed_results_dnf_2022 |>
  count(TRIP_year)
#   TRIP_year     n
#       <dbl> <int>
# 1      2021     5
# 2      2022   366
# 3      2023     1


srhs_2023_in_2022_processed_results_dnf_2022 |>
  filter(TRIP_year == "2023") |> View()
# $ VESSEL_OFFICIAL_NUMBER <chr> "644342"
# $ TRIP_DATE              <dttm> 2023-01-01
# srhs_22_23_diff_as_char__2022 |> filter(uscg_2023 == "644342")
#   uscg_2022 uscg_2023
# 1      <NA>    644342
# ok
#
