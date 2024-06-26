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

count_interviews <-
  function(my_df,
           cnt_field = "st_2") {
    
    my_df |>
      dplyr::group_by(fips) |>
      dplyr::mutate(total_int_by_st_county = n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!sym(cnt_field)) |>
      dplyr::add_count(!!sym(cnt_field), 
                       name = "total_int_by_state") |>
      dplyr::ungroup()
  }

survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts <-
  survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty |>
  select(id_code, vsl_num, st_2, cnty_3, fips) |>
  count_interviews()

survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts |> 
  select(st_2, total_int_by_state) |> 
  distinct() |> 
  arrange(st_2)
  
#   <chr>              <int>
# 1 00                    35
# 2 01                   252
# 3 12                  1030
# 4 22                   332
# 5 28                   108
# 6 48                    78




### prep state info for plotting ----


# prep state info for plotting ----
plot_states <- usmap::plot_usmap(include = c(gulf_states, "FL")) 

plot_cnties_only <-
  plot_usmap(regions = "counties",
             include = c(florida_gulf_counties))

selected_states_df <- usmap::us_map(include = c(gulf_states, "FL"))

#' Get centroids for state labels
centroid_labels <- usmapdata::centroid_labels("states")

#' Join centroids to data

#' rename centroid_labels to the same names as in survey data
old_names <- names(centroid_labels)

centroid_labels <-
  centroid_labels |>
  dplyr::rename("st_2" = "fips")

make_state_labels <-
  function(my_df,
           state_field_name = "st_2",
           number_to_show_field_name) {
    temp_df <-
      dplyr::inner_join(my_df,
                        centroid_labels,
                        dplyr::join_by(!!state_field_name == st_2))
    
    temp_df |>
      dplyr::select(!!state_field_name,
                    abbr,
                    full,
                    !!number_to_show_field_name,
                    geom) |>
      dplyr::distinct() |>
      dplyr::mutate(label_st_cnt =
                      paste(abbr, !!sym(number_to_show_field_name))) |>
      dplyr::arrange(full)
  }


