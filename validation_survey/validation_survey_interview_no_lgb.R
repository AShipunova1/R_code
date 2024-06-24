library(fuzzyjoin)

#' how many interviews with no logbooks?
#' 

lgb_join_i1__t_diff_short_has_no_trip_not_empty <-
  lgb_join_i1__t_diff_short_has_no_trip |>
  auxfunctions::remove_empty_cols()

dim(lgb_join_i1__t_diff_short_has_no_trip_not_empty)
# 833

lgb_join_i1__t_diff_short_has_no_trip_not_empty |> 
  head() |> 
  glimpse()

