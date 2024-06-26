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

dim(lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts)
# [1] 833  9

lgb_join_i1__t_diff_short_has_no_trip_not_empty
  select(id_code, vsl_num, st_2, cnty_3, fips) |> 
  count(st_2)
#   st_2      n
#   <chr> <int>
# 1 00       35
# 2 01      252
# 3 12     1030
# 4 22      332
# 5 28      108
# 6 48       78

count_interviews <-
  function(my_df,
           cnt_field = "st_2") {
    
