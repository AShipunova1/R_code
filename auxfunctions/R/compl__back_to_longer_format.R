compl__back_to_longer_format <-
function(my_df,
           cols_names) {
    my_df %>%
      # turn back to a longer format, vessel ids in one column
      tidyr::pivot_longer(
        # all other columns are vessel ids, use them as names
        cols = !any_of(cols_names),
        values_to = "is_compl_or_both",
        names_to = "vessel_official_number"
      ) %>%
      return()
  }
