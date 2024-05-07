my_to_sf <-
function(my_df, my_crs = sf::st_crs(sa_shp)) {
  my_df %>%
    sf::st_as_sf(
      # Specify the field names to use as coordinates
      coords = c("longitude", "latitude"),
      # Use the provided CRS (Coordinate Reference System), default to sa_shp's CRS
      crs = my_crs,
      # Keep the LATITUDE and LONGITUDE columns in the resulting sf object
      remove = FALSE
    ) %>%
    return()
}
