# add lat/lon ----
# Uses 'tidygeocoder::geocode' to obtain latitude and longitude for the given city and state columns.

get_lat_lon_no_county <-
  function(my_df,
           city_col_name = "city",
           state_col_name = "state") {
    result_coord <-
      my_df |>
      tidygeocoder::geocode(city = "city",
                            state = "state",
                            return_input = TRUE)
    return(result_coord)
  }
