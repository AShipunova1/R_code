add_cnt_in_gr <-
function(my_df,
           group_by_col,
           cnt_col_name = "total_vsl_m_by_year_perm") {
    my_df %>%
      # group by per month and permit
      group_by_at(group_by_col) %>%
      # cnt distinct vessels in each group
      mutate({
        {
          cnt_col_name
        }
      } :=
        n_distinct(vessel_official_number)) %>%
      ungroup() %>%
      return()
  }
