add_compliant_after_override <-
function(my_compl_df,
           overridden_col_name = "overridden",
           compliance_col_name = "is_comp") {
  # browser()
  res <-
    my_compl_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      compliant_after_override =
        dplyr::case_when(
          !!rlang::sym(compliance_col_name) %in% c(0, "NO") &
            !!rlang::sym(overridden_col_name) %in% c(0, "NO")  ~ "no",
          !!rlang::sym(compliance_col_name) %in% c(1, "YES") ~ "yes",
          !!rlang::sym(overridden_col_name) %in% c(1, "YES") ~ "yes",
          is.na(!!rlang::sym(compliance_col_name)) ~ NA,
          .default = toString(!!rlang::sym(compliance_col_name))
        )
    ) |>
    ungroup()

  return(res)
}
