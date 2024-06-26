library(ggplot2)

# prep survey data for plotting ----
survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty |> 
  select(id_code, vsl_num, st_2, cnty_3, fips) |> 
  # group_by()
  count(st_2)
#   st_2      n
#   <chr> <int>
# 1 00       35
# 2 01      252
# 3 12     1030
# 4 22      332
# 5 28      108
# 6 48       78

survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty |> 
  select(id_code, vsl_num, st_2, cnty_3, fips) |> 
  add_count()


