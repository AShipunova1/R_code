library(fuzzyjoin)

#' how many interviews with no logbooks?
#' 

lgb_join_i1__t_diff_short_has_no_trip_not_empty <-
  lgb_join_i1__t_diff_short_has_no_trip |>
  auxfunctions::remove_empty_cols()

dim(lgb_join_i1__t_diff_short_has_no_trip_not_empty)
# 833

# Prep data for interviews w no logbooks ----

## prep lgb info ----
db_logbooks_2022_vsl_t_end <-
  db_logbooks_2022_vsl_t_end_all |>
  dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_END_DATE, TRIP_ID) |>
  dplyr::distinct()

