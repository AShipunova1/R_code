get_df_name_as_text <-
function(my_df) {
    df_name = deparse(substitute(my_df))
    return(df_name)
  }
