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

# Interview no logbooks plot ----

plot_states <- usmap::plot_usmap(include = c(gulf_states, "FL")) 

plot_cnties_only <-
  plot_usmap(regions = "counties",
             include = c(florida_gulf_counties))

### prep state info for plotting ----
selected_states_df <- usmap::us_map(include = c(gulf_states, "FL"))

#' Get centroids
centroid_labels <- usmapdata::centroid_labels("states")

#' Join centroids to data
old_names <- names(centroid_labels)

names(centroid_labels) <- c("st_2", "abbr", "full", "geom")

make_state_labels <- function(my_df, state_field_name = "st_2") {
  temp_df <- 
  dplyr::inner_join(my_df, 
                   centroid_labels, 
                   dplyr::join_by(!!state_field_name == st_2))
  
  temp_df |> 
    dplyr::select(!!state_field_name,
                  abbr,
                  full,
                  total_int_no_lgb_by_state,
                  geom) |>
    dplyr::distinct() |>
    dplyr::mutate(label_st_cnt = 
                    paste(abbr, total_int_no_lgb_by_state)) |> 
    dplyr::arrange(full)
}

# my_dfs_to_plot <- Hmisc::llist(
#   intv_w_no_lgb_join_by_day_vsl_cnt,
#   intv_w_no_lgb_join_by_day_vsl_restored_cnt,
#   intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt,
#   intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips_cnt
# )
# 
# fields_to_join_by <- 
#   list("st_2", "restored_st", "st_2", "st_2")
# 
# my_dfs_to_plot_w_labels <-
#   purrr::map2(my_dfs_to_plot,
#               fields_to_join_by,
#               make_state_labels)

### interviews w no lgb plot ----

plot_counties <- function(my_df) {
  usmap::plot_usmap(
    regions = "counties",
    include = c(gulf_states, "FL"),
    data = my_df,
    values = "num_int_no_lgb_by_fips",
    color = "lightgrey"
  ) +
    ggplot2::scale_fill_gradient(
      # name = "Interviews w/o lgbks",
      name = "",
      high = "blue",
      low = "yellow",
      na.value = "transparent"
      # ,
      # guide = guide_legend()
    ) +
    ggplot2::theme(legend.position = "right",
                   # legend.position = c(.95, .95),
                   # legend.justification = c("right", "top"),
                   # legend.box.just = "right",
                   legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
          # ,
          # legend.spacing.x = unit(0, 'cm'))
  # +
    # guides(fill = guide_legend(label.position = "bottom"))
}

plot_cnties_4 <-
  my_dfs_to_plot |>
  purrr::map(plot_counties)

#' check
no_state_interview_no_lgb_num <- 
  intv_w_no_lgb_join_by_day_vsl_cnt |>
  dplyr::filter(st_2 == "00") |> 
  dplyr::select(total_int_no_lgb_by_state) |> 
  dplyr::distinct()
#' 192

intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt__no_st <-
  intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt |> 
  dplyr::filter(st_2 == "00") |> 
  dplyr::select(total_int_no_lgb_by_state) |> 
  dplyr::distinct()
#' 189

add_state_labels <-
  function(usmap_plot, labels_by_state = state_labels_short) {
    usmap_plot +
      ggplot2::geom_sf_text(data = labels_by_state, 
                            ggplot2::aes(geometry = geom, label = label_st_cnt)) +
      ggplot2::geom_sf(data = selected_states_df,
              color = "green",
              fill = NA)
  }

#' add_state_labels

all_4_plots <- 
  purrr::map2(plot_cnties_4,
              my_dfs_to_plot_w_labels,
              add_state_labels)

all_4_plots_together <-
  gridExtra::grid.arrange(
    grobs = all_4_plots,
    top =
      ggpubr::text_grob(
        "Interview with no logbooks, \n (1) Raw, (1) With restored states, (2) With removed by captain names, (2) With restored states",
        rot = 0,
        vjust = 1
      ),
    ncol = 1,
    heights = c(5, 10, 5, 10)
  )

# plot_restored_all <- 
#   add_state_labels(plot_cnties_restored, state_labels_restored) +
#   ggplot2::labs(title = "Number of interviews without logbooks by state/county")
# 
# plot_cnties_state_lbls <-
#   add_state_labels(plot_cnties, state_labels_short) +
#   ggplot2::labs(
#     title = "Number of interviews without logbooks by state/county",
#     caption = stringr::str_glue(
#       "Number of interviews without logbooks with no state info is {no_state_interview_no_lgb_num$total_int_no_lgb_by_state}."
#     )
#   )

#### interview wo lgb plot show ----
# #| column: screen
#| out-width: 100%

purrr::map2(
  all_4_plots,
  names(all_4_plots),
  \(p, p_name) {
    p +
      ggplot2::labs(title = "Number of interviews without logbooks by state/county",
                    caption = p_name)
  })
    
int_no_lgb_by_state_to_plot_w_labels <- 
  make_state_labels_w_cnts(lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc,
           "percent_st_label")
   

no_state_lgb_num <- 
lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts |> 
  filter(st_2 == '00') |> 
  select(no_lgb_int_by_state) |> 
  distinct()


plot_counties_res_with_labels <-
  lgb_join_i1__t_diff_short_has_no_trip_not_empty__with_total_cnts__with_no_lgb_cnts_perc |> 
  plot_counties(county_cnt_col_name = "percent_cnty_round")  +
  add_state_labels(int_no_lgb_by_state_to_plot_w_labels) +
  add_st_boundaries() +
  ggplot2::labs(
    title = "Percent of interviews without logbooks by state/county",
    caption = stringr::str_glue(
      "{no_state_lgb_num$no_lgb_int_by_state} interviews have no state info and are not included."
    )
  )

plot_counties_res_with_labels
