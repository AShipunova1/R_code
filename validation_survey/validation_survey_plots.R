library(ggplot2)

# prep survey data for plotting ----
survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts |> 
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

make_state_labels_w_cnts <-
  function(my_df,
           number_to_show_field_name,
           state_field_name = "st_2"
           ) {
    
    temp_df <-
      my_df |> 
      mutate(across(everything(), as.character))

    temp_df <-
      dplyr::inner_join(temp_df,
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

interviews_by_state_to_plot_w_labels <- 
  make_state_labels_w_cnts(survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts,
           "total_int_by_state")
   
# plot interviews ----

plot_counties <- 
  function(my_df, county_cnt_col_name = "total_int_by_st_county") {
    # rename a column, because plot_usmap doesn't allow dynamic naming
    my_df <- 
      rename(my_df,
             "cnt_by_county" = !!county_cnt_col_name)
    
  usmap::plot_usmap(
    regions = "counties",
    include = c(gulf_states, "FL"),
    data = my_df,
    values = "cnt_by_county",
    color = "lightgrey"
  ) +
    ggplot2::scale_fill_gradient(
      name = "",
      high = "blue",
      low = "yellow",
      na.value = "transparent"
    ) +
    ggplot2::theme(legend.position = "right",
                   legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

plot_counties_res <-
  plot_counties(survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts)

#' check
no_state_lgb_num <- 
  survey_data_l_2022_i1_w_dates_clean_vsl__st_restored_by_v_cnty__cnts |>
  dplyr::filter(st_2 == "00") |> 
  dplyr::select(total_int_by_state) |> 
  dplyr::distinct()
#' 35

#' add state labels,
#' state boundaries,
#' and title

add_state_labels <- function(my_df_w_labels) {
  ggplot2::geom_sf_text(data = my_df_w_labels, 
                        ggplot2::aes(geometry = geom, 
                                     label = label_st_cnt))
}

add_st_boundaries <-
  function(variables) {
    ggplot2::geom_sf(data = selected_states_df,
                     color = "green",
                     fill = NA)
  }

plot_counties_res_with_labels <-
  plot_counties_res +
  add_state_labels(interviews_by_state_to_plot_w_labels) +
  add_st_boundaries() +
  ggplot2::labs(
    title = "Number of interviews by state/county",
    caption = stringr::str_glue(
      "{no_state_lgb_num} interviews have no state info and are not included."
    )
  )

## interview count plot show ----
# #| column: screen
#| out-width: 100%

plot_counties_res_with_labels
