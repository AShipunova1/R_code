add_compliant_after_override <-
function(my_compl_df,
           overridden_col_name = "overridden",
           compliance_col_name = "is_comp") {
  # browser()
  res <-
    my_compl_df |>
    rowwise() |>
    mutate(
      compliant_after_override =
        case_when(
          !!sym(compliance_col_name) %in% c(0, "NO") &
            !!sym(overridden_col_name) %in% c(0, "NO")  ~ "no",
          !!sym(compliance_col_name) %in% c(1, "YES") ~ "yes",
          !!sym(overridden_col_name) %in% c(1, "YES") ~ "yes",
          is.na(!!sym(compliance_col_name)) ~ NA,
          .default = toString(!!sym(compliance_col_name))
        )
    ) |>
    ungroup()

  return(res)
}
