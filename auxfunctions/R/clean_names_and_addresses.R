clean_names_and_addresses <-
function(my_df) {

  my_df_cleaned <-
    my_df |>
    mutate(
      across(where(is.character),
             ~ str_squish(.x)),
      across(where(is.character),
             ~ replace_na(.x, "")),
      across(where(is.character),
             ~ str_replace_all(.x, ", ;", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s+[,;]", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, ";,+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ";;+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ",,+", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;] *\\bUN\\b *", "")),
      across(where(is.character),
                          ~ str_replace_all(.x, "\\bUN\\b", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;] ", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;]$", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;]$", "")),
      across(where(is.character),
             ~ str_squish(.x))
    )

  return(my_df_cleaned)
}
