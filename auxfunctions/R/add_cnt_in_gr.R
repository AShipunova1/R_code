add_cnt_in_gr <-
function(my_df,
           group_by_col,
           cnt_col_name = "total_vsl_m_by_year_perm") {
    my_df %>%
      # group by per month and permit
      dplyr::group_by_at(group_by_col) %>%
      # cnt distinct vessels in each group
      dplyr::mutate({
        {
          cnt_col_name
        }
      } :=
        dplyr::n_distinct(vessel_official_number)) %>%
      dplyr::ungroup() %>%
      return()
  }
