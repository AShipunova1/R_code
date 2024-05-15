get_compl_by <-
function(my_df,
           group_by_for_compl =
             vars(-c("vessel_official_number", "compliant_", "overridden_")),
           names_from_list = c("vessel_official_number")) {
    # browser()
    my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = tidyselect::all_of(names_from_list),
      values_from = c("compliant_", "overridden_"),
        # compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(unique(sort(.x)), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}
