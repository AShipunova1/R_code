remove_0_cols <-
function(my_df) {
  not_all_0 <- function(x)
  {
    any(!x == 0)
  }

  my_df |>
    select(where(not_all_0)) %>%
    return()
}
