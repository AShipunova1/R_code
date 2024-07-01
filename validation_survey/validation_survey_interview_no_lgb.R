#' how many interviews with no logbooks?
#' 

glimpse(lgb_join_i1__t_diff_short_has_no_trip)


lgb_join_i1__t_diff_short_has_no_trip_not_empty <-
  lgb_join_i1__t_diff_short_has_no_trip |>
  auxfunctions::remove_empty_cols()

dim(lgb_join_i1__t_diff_short_has_no_trip_not_empty)
# [1] 833   4

lgb_join_i1__t_diff_short_has_no_trip_not_empty |> 
  head() |> 
  glimpse()

survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts |>
  head() |>
  glimpse()

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts <-
  left_join(
    lgb_join_i1__t_diff_short_has_no_trip_not_empty,
    survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts,
    join_by(id_code, VESSEL_OFFICIAL_NBR == vsl_num)
  )

# View(lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts)
# [1] 833  9

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts <- 
lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts |> 
  add_count(st_2, name = "no_lgb_int_by_state") |> 
  add_count(fips, name = "no_lgb_int_by_st_county")

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts |> 
  select(st_2, no_lgb_int_by_state) |> 
  distinct() |> 
  arrange(st_2)
# 1 00                     28
# 2 01                     37
# 3 12                    440
# 4 22                    245
# 5 28                     28
# 6 48                     55

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc <-
  lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts |>
  mutate(
    percent_no_lgb_cnty = no_lgb_int_by_st_county * 100 / total_int_by_st_county       ,
    percent_no_lgb_st = no_lgb_int_by_state * 100 / total_int_by_state
  ) |> 
  mutate(percent_cnty_round = round(percent_no_lgb_cnty, 0),
         percent_st_round = round(percent_no_lgb_st, 0)) |> 
  mutate(percent_cnty_label = paste0(percent_cnty_round, "%"),
         percent_st_label = paste0(percent_st_round, "%"))

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc |> 
  head() |> 
  glimpse()

