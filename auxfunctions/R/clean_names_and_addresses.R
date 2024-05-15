clean_names_and_addresses <-
  function(my_df) {
    my_df_cleaned <-
      my_df |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ stringr::str_squish(.x)),
        dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, "")),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, ", ;", ";")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "\\s+[,;]", ",")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, ";,+", ";")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, ";;+", ";")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, ",,+", ",")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "[,;] *\\bUN\\b *", "")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "\\bUN\\b", "")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "^[,;] ", "")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "^[,;]$", "")
        ),
        dplyr::across(
          dplyr::where(is.character),
          ~ stringr::str_replace_all(.x, "[,;]$", "")
        ),
        dplyr::across(dplyr::where(is.character), 
                      ~ stringr::str_squish(.x))
      )

  return(my_df_cleaned)
}
