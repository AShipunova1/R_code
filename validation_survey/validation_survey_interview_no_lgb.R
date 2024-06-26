library(fuzzyjoin)

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

View(lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts)
# [1] 833  9

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts <- 
lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts |> 
  # select(id_code, VESSEL_OFFICIAL_NBR, st_2, cnty_3, fips) |> 
  add_count(st_2, name = "no_lgb_int_by_state") |> 
  add_count(fips, name = "no_lgb_int_by_st_county")
#   st_2      n
#   <chr> <int>
# 1 00       35
# 2 01      252
# 3 12     1030
# 4 22      332
# 5 28      108
# 6 48       78

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc <-
  lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts |>
  mutate(
    percent_no_lgb_cnty = no_lgb_int_by_st_county * 100 / total_int_by_st_county       ,
    percent_no_lgb_st = no_lgb_int_by_state * 100 / total_int_by_state
  ) |> 
  mutate(percent_cnty_round = round(percent_no_lgb_cnty, 0),
         percent_st_round = round(percent_no_lgb_st, 0))

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc |> 
  head() |> 
  glimpse()

int_no_lgb_by_state_to_plot_w_labels <- 
  make_state_labels_w_cnts(lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc,
           "percent_st_round")
   

# int_no_lgb_by_state_to_plot_w_labels

lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc |> 
plot_counties(county_cnt_col_name = "percent_cnty_round") 
